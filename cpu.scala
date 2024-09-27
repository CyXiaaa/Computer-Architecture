// This file is where all of the CPU components are assembled into the whole CPU

package dinocpu.pipelined

import chisel3._
import chisel3.util._
import dinocpu._
import dinocpu.components._

/**
 * The main CPU definition that hooks up all of the other components.
 *
 * For more information, see section 4.6 of Patterson and Hennessy
 * This follows figure 4.49
 */
class PipelinedCPU(implicit val conf: CPUConfig) extends BaseCPU {
  // Everything in the register between IF and ID stages
  class IFIDBundle extends Bundle {
    val instruction = UInt(32.W)
    val pc          = UInt(64.W)
  }

  // Control signals used in EX stage
  class EXControl extends Bundle {
    val aluop             = UInt(3.W)
    val op1_src           = UInt(1.W)
    val op2_src           = UInt(2.W)
    val controltransferop = UInt(2.W)
  }

  // Control signals used in MEM stage
  class MControl extends Bundle {
    val memop             = UInt(2.W)

  }

  // Control signals used in WB stage
  class WBControl extends Bundle {
    val writeback_valid   = UInt(1.W)
    val writeback_src     = UInt(2.W)
  }

  // Data of the the register between ID and EX stages
  class IDEXBundle extends Bundle {
    val pc          = UInt(64.W)
    val sextImm     = UInt(64.W)
    val readdata1   = UInt(64.W)
    val readdata2   = UInt(64.W)
    val instruction = UInt(32.W)  // or should it carry the write back reg only?
  }

  // Control block of the IDEX register (Each register controls its own signals )
  class IDEXControl extends Bundle {
    val ex_ctrl  = new EXControl
    val mem_ctrl = new MControl
    val wb_ctrl  = new WBControl
  }

  // Everything in the register between EX and MEM stages
  class EXMEMBundle extends Bundle {
    val nextpc      = UInt(64.W)
    val taken       = Bool() 
    val result      = UInt(64.W)
    val operand2    = UInt(64.W)
    val sextImm     = UInt(64.W)
    val instruction = UInt(32.W)  // or should it carry the write back reg only?

  }

  // Control block of the EXMEM register
  class EXMEMControl extends Bundle {
    val mem_ctrl  = new MControl
    val wb_ctrl   = new WBControl
  }

  // Everything in the register between MEM and WB stages
  class MEMWBBundle extends Bundle {
    val readdata    = UInt(64.W)
    val result      = UInt(64.W)
    val sextImm     = UInt(64.W)
    val instruction = UInt(32.W)  // or should it carry the write back reg only?
  }

  // Control block of the MEMWB register
  class MEMWBControl extends Bundle {
    val wb_ctrl = new WBControl
  }

  // All of the structures required
  val pc              = RegInit(0.U(64.W))
  val control         = Module(new Control())
  val registers       = Module(new RegisterFile())
  val aluControl      = Module(new ALUControl())
  val alu             = Module(new ALU())
  val immGen          = Module(new ImmediateGenerator())
  val controlTransfer = Module(new ControlTransferUnit())
  val pcPlusFour      = Module(new Adder())
  val forwarding      = Module(new ForwardingUnit())  //pipelined only
  val hazard          = Module(new HazardUnit())      //pipelined only
  val (cycleCount, _) = Counter(true.B, 1 << 30)

  // The four pipeline registers
  val if_id       = Module(new StageReg(new IFIDBundle))

  val id_ex       = Module(new StageReg(new IDEXBundle))  // handles data in ID/EX block 
  val id_ex_ctrl  = Module(new StageReg(new IDEXControl)) // handles control signals in ID/EX block

  val ex_mem      = Module(new StageReg(new EXMEMBundle))
  val ex_mem_ctrl = Module(new StageReg(new EXMEMControl))

  val mem_wb      = Module(new StageReg(new MEMWBBundle))

  // To make the interface of the mem_wb_ctrl register consistent with the other control
  // registers, we create an anonymous Bundle
  val mem_wb_ctrl = Module(new StageReg(new MEMWBControl))

  // Remove when connected
  // control.io          := DontCare
  // registers.io        := DontCare
  // aluControl.io       := DontCare
  // alu.io              := DontCare
  // immGen.io           := DontCare
  // controlTransfer.io  := DontCare
  //pcPlusFour.io       := DontCare
  // forwarding.io       := DontCare
  //hazard.io           := DontCare

   io.dmem := DontCare
  dontTouch(pc)

  // id_ex.io       := DontCare
  // id_ex_ctrl.io  := DontCare
  // ex_mem.io      := DontCare
  // ex_mem_ctrl.io := DontCare
  // mem_wb.io      := DontCare
  // mem_wb_ctrl.io := DontCare

  // From memory back to fetch. Since we don't decide whether to take a branch or not until the memory stage.
  val next_pc = Wire(UInt(64.W))
  next_pc := DontCare // Remove when connected

  /////////////////////////////////////////////////////////////////////////////
  // FETCH STAGE
  /////////////////////////////////////////////////////////////////////////////

  // Update the PC:
  pcPlusFour.io.inputx := pc
  pcPlusFour.io.inputy := 4.U
  // (Part I) Choose between PC+4 and nextpc from the ControlTransferUnit to update PC
  when (hazard.io.pcstall === true.B) {                                
    pc := pc                                        
  } .otherwise {                                                               
    when(hazard.io.pcfromtaken === true.B){
      pc := ex_mem.io.data.nextpc
    }
    .otherwise{
      pc := pcPlusFour.io.result
    }                                       
  } 
  
          
  // (Part III) Only update PC when pcstall is false

  // Send the PC to the instruction memory port to get the instruction
  io.imem.address := pc
  io.imem.valid   := true.B

  // Fill the IF/ID register
  when ((pc % 8.U) === 4.U) {
    if_id.io.in.instruction := io.imem.instruction(63, 32)
  } .otherwise {
    if_id.io.in.instruction := io.imem.instruction(31, 0)
  }
  if_id.io.in.pc := pc

  // Update during Part III when implementing branches/jump
  when(hazard.io.if_id_stall){
    if_id.io.valid := false.B
  }
  .otherwise{
    if_id.io.valid := true.B
  }

  if_id.io.flush := hazard.io.if_id_flush
  when(if_id.io.flush === true.B){
    if_id.io.in.instruction := 0.U
  }


  /////////////////////////////////////////////////////////////////////////////
  // ID STAGE
  /////////////////////////////////////////////////////////////////////////////
  
  // Send opcode to control
  control.io.opcode := if_id.io.data.instruction(6, 0) 


  // Grab rs1 and rs2 from the instruction in this stage
  val rs1 = if_id.io.data.instruction(19, 15)                                                
  val rs2 = if_id.io.data.instruction(24, 20) 

  // (Part III and/or Part IV) Send inputs from this stage to the hazard detection unit
  hazard.io.rs1 := rs1
  hazard.io.rs2 := rs2
  // Send rs1 and rs2 to the register file
  registers.io.readreg1 := rs1
  registers.io.readreg2 := rs2 

  // Send the instruction to the immediate generator
  immGen.io.instruction := if_id.io.data.instruction

  // Sending signals from this stage to EX stage
  //  - Fill in the ID_EX register
    id_ex.io.in.pc          := if_id.io.data.pc
    id_ex.io.in.sextImm     := immGen.io.sextImm
    id_ex.io.in.readdata1   := registers.io.readdata1
    id_ex.io.in.readdata2   := registers.io.readdata2
    id_ex.io.in.instruction := if_id.io.data.instruction
  //  - Set the execution control singals
  id_ex_ctrl.io.in.ex_ctrl.aluop    := control.io.aluop
  id_ex_ctrl.io.in.ex_ctrl.op1_src  := control.io.op1_src
  id_ex_ctrl.io.in.ex_ctrl.op2_src  := control.io.op2_src
  id_ex_ctrl.io.in.ex_ctrl.controltransferop := control.io.controltransferop
  //  - Set the memory control singals
  id_ex_ctrl.io.in.mem_ctrl.memop   := control.io.memop
  
  //  - Set the writeback control signals
  id_ex_ctrl.io.in.wb_ctrl.writeback_valid   := control.io.writeback_valid
  id_ex_ctrl.io.in.wb_ctrl.writeback_src     := control.io.writeback_src
  

  // (Part III and/or Part IV) Set the control signals on the ID_EX pipeline register
  id_ex.io.valid := true.B
  id_ex.io.flush := hazard.io.id_ex_flush
  when(id_ex.io.flush === true.B){
    id_ex.io.in.pc := 0.U
    id_ex.io.in.sextImm := 0.U
    id_ex.io.in.readdata1 := 0.U
    id_ex.io.in.readdata2 := 0.U
    id_ex.io.in.instruction := 0.U
  }
  id_ex_ctrl.io.valid := true.B
  id_ex_ctrl.io.flush := hazard.io.id_ex_flush
    when(id_ex_ctrl.io.flush === true.B){
  id_ex_ctrl.io.in.ex_ctrl.aluop    := 0.U
  id_ex_ctrl.io.in.ex_ctrl.op1_src  := 0.U
  id_ex_ctrl.io.in.ex_ctrl.op2_src  := 0.U
  id_ex_ctrl.io.in.ex_ctrl.controltransferop := 0.U
  id_ex_ctrl.io.in.mem_ctrl.memop   := 0.U
  id_ex_ctrl.io.in.wb_ctrl.writeback_valid   := 0.U
  id_ex_ctrl.io.in.wb_ctrl.writeback_src     := 0.U
  
  }


  /////////////////////////////////////////////////////////////////////////////
  // EX STAGE
  /////////////////////////////////////////////////////////////////////////////

  // (Skip for Part I) Set the inputs to the hazard detection unit from this stage
  hazard.io.idex_rd := id_ex.io.data.instruction(11,7)
  hazard.io.idex_memread := false.B
  when(id_ex_ctrl.io.data.mem_ctrl.memop === 1.U){
    hazard.io.idex_memread := true.B
  }
  // (Skip for Part I) Set the inputs to the forwarding unit from this stage
  forwarding.io.rs1 := id_ex.io.data.instruction(19, 15)
  forwarding.io.rs2 := id_ex.io.data.instruction(24, 20)

  

  // Connect the ALU Control wires
  aluControl.io.aluop   := id_ex_ctrl.io.data.ex_ctrl.aluop
  aluControl.io.funct7  := id_ex.io.data.instruction(31, 25)  
  aluControl.io.funct3  := id_ex.io.data.instruction(14, 12)
  // Connect the ControlTransferUnit control wire
  controlTransfer.io.controltransferop := id_ex_ctrl.io.data.ex_ctrl.controltransferop
  

  // (Skip for Part I) Insert the mux for Selecting data to forward from the MEM stage to the EX stage
  //                   (Can send either alu result or immediate from MEM stage to EX stage)

  // (Skip for Part I) Insert the forward operand1 mux

  // (Skip for Part I) Insert the forward operand2 mux

  // Operand1 mux 
  val op1_mux = Wire (UInt(64.W))
  val x = Wire(UInt(64.W))
  val writedata_mux = Wire(UInt(64.W))
  op1_mux :=0.U
  when(forwarding.io.forwardA === 0.U){
    op1_mux := id_ex.io.data.readdata1
  }
  .elsewhen(forwarding.io.forwardA === 1.U){
    op1_mux := x
  }
  .otherwise{
    op1_mux := writedata_mux
  }
  

  // Operand2 mux
  val op2_mux = Wire (UInt(64.W))
  op2_mux := 0.U
  when(forwarding.io.forwardB === 0.U){
    op2_mux := id_ex.io.data.readdata2
  }
  .elsewhen(forwarding.io.forwardB === 1.U){
    op2_mux := x 
  }
  .otherwise{
    op2_mux := writedata_mux
  }

  // Set the ALU operation
  alu.io.operation  := aluControl.io.operation
  // Connect the ALU data wires
   // alu operand1 mux
  when(id_ex_ctrl.io.data.ex_ctrl.op1_src === 0.U){
    alu.io.operand1 := op1_mux
  }
  .otherwise{
    alu.io.operand1 := id_ex.io.data.pc 
  }

  // alu operand2 mux
  when(id_ex_ctrl.io.data.ex_ctrl.op2_src === 0.U){
      alu.io.operand2 := op2_mux
  }
  .elsewhen(id_ex_ctrl.io.data.ex_ctrl.op2_src  === 1.U){
    alu.io.operand2 := 4.U
  }
  .otherwise{
    alu.io.operand2 := id_ex.io.data.sextImm
    
  }

  // Connect the ControlTransfer data wires
  controlTransfer.io.pc                := id_ex.io.data.pc
  controlTransfer.io.funct3            := id_ex.io.data.instruction(14, 12)
  controlTransfer.io.operand1          := op1_mux
  controlTransfer.io.operand2          := op2_mux
  controlTransfer.io.imm               := id_ex.io.data.sextImm 

  // Sending signals from this stage to MEM stage
  //  - Fill in the EX_MEM register
  ex_mem.io.in.nextpc       := controlTransfer.io.nextpc
  ex_mem.io.in.taken        := controlTransfer.io.taken
  ex_mem.io.in.result       := alu.io.result
  ex_mem.io.in.operand2     := op2_mux
  ex_mem.io.in.sextImm      := id_ex.io.data.sextImm
  ex_mem.io.in.instruction  := id_ex.io.data.instruction
  //  - Set the memory control singals
  ex_mem_ctrl.io.in.mem_ctrl.memop  := id_ex_ctrl.io.data.mem_ctrl.memop
  //  - Set the writeback control signals
  ex_mem_ctrl.io.in.wb_ctrl.writeback_valid   := id_ex_ctrl.io.data.wb_ctrl.writeback_valid
  ex_mem_ctrl.io.in.wb_ctrl.writeback_src     := id_ex_ctrl.io.data.wb_ctrl.writeback_src
//printf(p"ex_mem_ctrl.io.in.wb_ctrl.writeback_src : 0x${Hexadecimal(ex_mem_ctrl.io.in.wb_ctrl.writeback_src )}\n")
  // (Part III and/or Part IV) Set the control signals on the EX_MEM pipeline register
  ex_mem.io.valid      := true.B
  ex_mem.io.flush      := hazard.io.ex_mem_flush
  when(ex_mem.io.flush)
  {
    ex_mem.io.in.nextpc       := 0.U
    ex_mem.io.in.taken        := 0.U
    ex_mem.io.in.result       := 0.U
    ex_mem.io.in.operand2     := 0.U
    ex_mem.io.in.sextImm      := 0.U
    ex_mem.io.in.instruction  := 0.U
  }

  ex_mem_ctrl.io.valid := true.B
  ex_mem_ctrl.io.flush := hazard.io.ex_mem_flush
  when(ex_mem_ctrl.io.flush)
  {
    ex_mem_ctrl.io.in.mem_ctrl.memop  := 0.U
    //  - Set the writeback control signals
    ex_mem_ctrl.io.in.wb_ctrl.writeback_valid   := 0.U
    ex_mem_ctrl.io.in.wb_ctrl.writeback_src     := 0.U
  }

  /////////////////////////////////////////////////////////////////////////////
  // MEM STAGE
  /////////////////////////////////////////////////////////////////////////////

  // Set data memory IO
  io.dmem.address := ex_mem.io.data.result

  io.dmem.memread  := false.B
  io.dmem.memwrite := false.B
  io.dmem.valid    := false.B
   when(ex_mem_ctrl.io.data.mem_ctrl.memop === 1.U){ //load
    io.dmem.memread   := true.B
    io.dmem.valid     := true.B
  }
  .elsewhen(ex_mem_ctrl.io.data.mem_ctrl.memop === 2.U){ // write
    io.dmem.memwrite := true.B
    io.dmem.valid    := true.B
  }
  io.dmem.maskmode   := ex_mem.io.data.instruction(13,12)
  io.dmem.writedata := ex_mem.io.data.operand2 
    when(ex_mem.io.data.instruction(14) === 0.U){
    io.dmem.sext := true.B
  }
  .otherwise
  {
    io.dmem.sext := false.B
  }

  // Send next_pc back to the Fetch stage

  // (Skip for Part I) Send input signals to the hazard detection unit
  hazard.io.exmem_taken := ex_mem.io.data.taken
  // (Skip for Part I) Send input signals to the forwarding unit
  forwarding.io.exmemrd  := ex_mem.io.data.instruction(11,7)
  forwarding.io.exmemrw  := ex_mem_ctrl.io.data.wb_ctrl.writeback_valid
  
  x := 0.U
  when(ex_mem_ctrl.io.data.wb_ctrl.writeback_src === 0.U)
  { // forwarding the ALU result
      x := ex_mem.io.data.result
  }
  .elsewhen(ex_mem_ctrl.io.data.wb_ctrl.writeback_src === 1.U)
  {
      x := ex_mem.io.data.sextImm
  }
  

  // Sending signals from this stage to the WB stage
  //  - Fill in the MEM_WB register
  mem_wb.io.in.result      := ex_mem.io.data.result
  mem_wb.io.in.sextImm     := ex_mem.io.data.sextImm
  mem_wb.io.in.readdata    := io.dmem.readdata
  mem_wb.io.in.instruction := ex_mem.io.data.instruction 
  //  - Set the writeback control signals
  mem_wb_ctrl.io.in.wb_ctrl.writeback_valid := ex_mem_ctrl.io.data.wb_ctrl.writeback_valid
  mem_wb_ctrl.io.in.wb_ctrl.writeback_src   := ex_mem_ctrl.io.data.wb_ctrl.writeback_src
  // Set the control signals on the MEM_WB pipeline register
  mem_wb.io.valid      := true.B
  mem_wb.io.flush      := false.B
  mem_wb_ctrl.io.valid := true.B
  mem_wb_ctrl.io.flush := false.B


  /////////////////////////////////////////////////////////////////////////////
  // WB STAGE
  /////////////////////////////////////////////////////////////////////////////

  // Set the register to be written to
  registers.io.writereg := mem_wb.io.data.instruction(11,7)
  // Set the writeback data mux
  
  writedata_mux := 0.U
  when(mem_wb_ctrl.io.data.wb_ctrl.writeback_src === 0.U){
    writedata_mux := mem_wb.io.data.result 
  }
  .elsewhen(mem_wb_ctrl.io.data.wb_ctrl.writeback_src === 1.U){
    writedata_mux := mem_wb.io.data.sextImm
  }
  .elsewhen(mem_wb_ctrl.io.data.wb_ctrl.writeback_src === 2.U){
    writedata_mux := mem_wb.io.data.readdata
  }

  // Write the data to the register file
  registers.io.writedata := writedata_mux
  when(registers.io.writereg === 0.U){
    registers.io.wen := false.B
  }
  .otherwise{
      registers.io.wen := mem_wb_ctrl.io.data.wb_ctrl.writeback_valid
  }

  // (Skip for Part I) Set the input signals for the forwarding unit
  forwarding.io.memwbrd  := mem_wb.io.data.instruction(11,7)
  forwarding.io.memwbrw  := mem_wb_ctrl.io.data.wb_ctrl.writeback_valid
}

/*
 * Object to make it easier to print information about the CPU
 */
object PipelinedCPUInfo {
  def getModules(): List[String] = {
    List(
      "imem",
      "dmem",
      "control",
      //"branchCtrl",
      "registers",
      "aluControl",
      "alu",
      "immGen",
      "pcPlusFour",
      //"branchAdd",
      "controlTransfer",
      "forwarding",
      "hazard",
    )
  }
  def getPipelineRegs(): List[String] = {
    List(
      "if_id",
      "id_ex",
      "id_ex_ctrl",
      "ex_mem",
      "ex_mem_ctrl",
      "mem_wb",
      "mem_wb_ctrl"
    )
  }
}