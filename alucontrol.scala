// This file contains ALU control logic.

package dinocpu.components

import chisel3._
import chisel3.util._

/**
 * The ALU control unit
 *
 * Input:  aluop        Specifying the type of instruction using ALU
 *                          . 0 for none of the below
 *                          . 1 for 64-bit R-type
 *                          . 2 for 64-bit I-type
 *                          . 3 for 32-bit R-type
 *                          . 4 for 32-bit I-type
 *                          . 5 for non-arithmetic instruction types that uses ALU (auipc/jal/jarl/Load/Store)
 * Input:  funct7       The most significant bits of the instruction.
 * Input:  funct3       The middle three bits of the instruction (12-14).
 *
 * Output: operation    What we want the ALU to do.
 *
 * For more information, see Section 4.4 and A.5 of Patterson and Hennessy.
 * This is loosely based on figure 4.12
 */
class ALUControl extends Module {
  val io = IO(new Bundle {
    val aluop     = Input(UInt(3.W))
    val funct7    = Input(UInt(7.W))
    val funct3    = Input(UInt(3.W))

    val operation = Output(UInt(5.W))
  })

  io.operation := "b11111".U // Invalid

  // Your code goes here
  when(io.aluop === 1.U)
  { 
     when(io.funct7 === "b0000000".U) // classify funct7
          {
            when(io.funct3 === "b000".U) {io.operation := "b00001".U} // ADD
            .elsewhen(io.funct3 === "b001".U){io.operation := "b10010".U} // SLL 
            .elsewhen(io.funct3 === "b010".U){io.operation := "b10110".U} // SLT 
            .elsewhen(io.funct3 === "b011".U){io.operation := "b10111".U} // SLTU
            .elsewhen(io.funct3 === "b100".U){io.operation := "b01111".U} // XOR
            .elsewhen(io.funct3 === "b101".U){io.operation := "b10100".U} // SRL
            .elsewhen(io.funct3 === "b110".U){io.operation := "b01110".U} // OR
            .elsewhen(io.funct3 === "b111".U){io.operation := "b01101".U} // AND
          }
      .elsewhen(io.funct7 === "b0000001".U)
          {
            when(io.funct3 === "b000".U){io.operation := "b00110".U} // MUL
            .elsewhen(io.funct3 === "b001".U){io.operation := "b00111".U} // MULH
            .elsewhen(io.funct3 === "b010".U){io.operation := "b11000".U} // MULHSU
            .elsewhen(io.funct3 === "b011".U){io.operation := "b01000".U} // MULHU
            .elsewhen(io.funct3 === "b100".U){io.operation := "b01011".U} // DIV
            .elsewhen(io.funct3 === "b101".U){io.operation := "b01010".U} // DIVU
            .elsewhen(io.funct3 === "b110".U){io.operation := "b11100".U} // REM
            .elsewhen(io.funct3 === "b111".U){io.operation := "b11011".U} // REMU
            }
          
       .elsewhen(io.funct7 === "b0100000".U){

            when(io.funct3 === "b000".U){io.operation := "b00100".U} //SUB
            .elsewhen(io.funct3 === "b101".U){io.operation := "b10000".U} //SRA
          }

  }
switch(io.aluop){
  is(3.U){ //32R type
    switch(io.funct7){
      is("b0000000".U){
        switch(io.funct3){
          is("b000".U){io.operation := "b00000".U }
          is("b001".U){io.operation := "b10011".U }
          is("b101".U){io.operation := "b10101".U }
        }
      }
      is("b0100000".U){
        switch(io.funct3){
          is("b000".U){io.operation := "b00010".U }
          is("b101".U){io.operation := "b10001".U }
        }
      }
      is("b0000001".U){
        switch(io.funct3){
          is("b000".U){io.operation := "b00101".U }
          is("b100".U){io.operation := "b01001".U }
          is("b101".U){io.operation := "b01100".U }
          is("b110".U){io.operation := "b11010".U }
          is("b111".U){io.operation := "b11001".U }
        }
      }
    }
  }
}

// 64-bit I-Type
when(io.aluop === 2.U){
    when(io.funct3 === "b000".U) {io.operation := "b00001".U} // ADDI
    .elsewhen(io.funct3 === "b111".U){io.operation := "b01101".U} // ANDI
    .elsewhen(io.funct3 === "b110".U){io.operation := "b01110".U} // ORI
    .elsewhen(io.funct3 === "b100".U){io.operation := "b01111".U} // XORI
    .elsewhen(io.funct3 === "b010".U){io.operation := "b10110".U} // SLTI
    .elsewhen(io.funct3 === "b011".U){io.operation := "b10111".U} // SLTIU
    .elsewhen(io.funct3 === "b101".U){
      when(io.funct7 === "b0100000".U){io.operation := "b10000".U} //SRAI
      .otherwise {io.operation := "b10100".U} // SRLI
      
    }
    .elsewhen(io.funct3 === "b001".U)
    {
       {io.operation := "b10010".U} // SLLI
    }
  }


// 32-bit I-type
when(io.aluop === 4.U){
  when(io.funct3 === "b101".U){
     when(io.funct7 === "b0000000".U){io.operation := "b10101".U} // SRLIW
     .elsewhen(io.funct7 === "b0100000".U){io.operation := "b10001".U} //SRAIW
  }
  .elsewhen(io.funct3 === "b001".U){
    when(io.funct7 === "b0000000".U) {io.operation := "b10011".U} // SLLIW
  }
  .otherwise{
  when(io.funct3 === "b000".U) {io.operation := "b00000".U} // ADDIW
  }
}

when(io.aluop === 5.U){
  io.operation := "b00001".U
}
}