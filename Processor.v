module Processor (clk);
input clk;
wire [31:0] InstrFetch, Source1, Source2, Result, Data, DataReg;
wire RW, S;
wire [15:0] AddBus, i;
wire [4:0] shift_ror;
wire [3:0] SourceSel1, SourceSel2, dest_reg, OP_CODE, COND;
wire [7:0] PC;
	
    RAM ram (RW, Data, AddBus, InstrFetch);
   instruction_decoder inst (InstrFetch, i, shift_ror, SourceSel1,SourceSel2,dest_reg,S,OP_CODE,COND);
MemoryControlAll memcont(Source1, Source2, Result, Data, RW, PC, AddBus, OP_CODE, DataReg);
PCInstructionAccess pc (clk, PC);
	
register_all register (dest_reg, SourceSel1, SourceSel2, Source1, Source2, DataReg);
	
ALU alu (OP_CODE, Result, Source1, Source2, COND, S, shift_ror, i); 	


		
endmodule

module RAM (ReadWrite, Data, Address, InstructionFetch);
	input ReadWrite;
	input [15:0] Address;
	output reg [31:0] InstructionFetch;
	inout [31:0] Data;
	reg [31:0] Mem[0:(1<<16)-1];
	assign Data = (ReadWrite) ? Mem[Address] : 32'bz; //out
	always @(*)
	begin
	if (!ReadWrite) 
		Mem[Address] = Data; //Write in
	else if (Address <= 16'b0000000100000000)
		InstructionFetch = Mem[Address];
	end
endmodule

module PCInstructionAccess (clk, PC);
	input clk;
	output reg [7:0] PC;
	assign PC = 0;
	always @ (posedge clk)
	begin
		PC = PC+1;
	end
endmodule


module instruction_decoder (instruction,i,shift_ror,src_reg1,src_reg2,dest_reg,Sbit,OPcode,cond);
input [31:0] instruction;
output reg [15:0] i;
output reg[4:0] shift_ror;
output reg [3:0] src_reg1, src_reg2, dest_reg;
output reg Sbit;
output reg [3:0] OPcode,cond;

assign unused = instruction[2:0];
assign i = instruction[18:3];
assign shift_ror = instruction[10:6];
assign src_reg1 = instruction[14:11];
assign src_reg2 = instruction[18:15];
assign dest_reg = instruction[22:19];
assign Sbit = instruction[23];
assign OPcode = instruction[27:24];
assign cond = instruction[31:28];

endmodule

// Register module that includes decoder, register bank, and MUXs
module register_all(destination, SourceSel1, SourceSel2, Source1, Source2, DataReg);
input [3:0] destination, SourceSel1, SourceSel2;
input [31:0] DataReg;
output [31:0] Source1, Source2;
wire [15:0] enable;
wire [31:0] R0, R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R11, R12, R13, R14, R15;
 
four_to_sixteen_decoder DEC(destination, enable);	// call decoder
registerbank RegBank (enable, R0, R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R11, R12, R13, R14, R15, DataReg);	// call register bank
thirtytwo_bit_sixteen_to_one_MUX MUX1(SourceSel1, Source1, R0, R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R11, R12, R13, R14, R15); // call MUX
thirtytwo_bit_sixteen_to_one_MUX MUX2(SourceSel2, Source2, R0, R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R11, R12, R13, R14, R15); // call MUX
 
endmodule

// decoder module
module four_to_sixteen_decoder(destination, enable);
input [3:0] destination;
output reg [15:0] enable;
always@*
        	begin
                    	case(destination)
                                	4'b0000: enable = 16'b0000000000000001; //0
                                	4'b0001: enable = 16'b0000000000000010; //1
                                	4'b0010: enable = 16'b0000000000000100; //2
                                	4'b0011: enable = 16'b0000000000001000; //3
                                	4'b0100: enable = 16'b0000000000010000; //4
                                	4'b0101: enable = 16'b0000000000100000; //5
                                	4'b0110: enable = 16'b0000000001000000; //6
                                	4'b0111: enable = 16'b0000000010000000; //7
                                	4'b1000: enable = 16'b0000000100000000; //8
                                	4'b1001: enable = 16'b0000001000000000; //9
                                	4'b1010: enable = 16'b0000010000000000; //10
                                	4'b1011: enable = 16'b0000100000000000; //11
                                	4'b1100: enable = 16'b0001000000000000; //12
                                	4'b1101: enable = 16'b0010000000000000; //13
                                	4'b1110: enable = 16'b0100000000000000; //14
                                	4'b1111: enable = 16'b1000000000000000; //15
                                	default: enable = 16'b0000000000000000; //default case
                    	endcase
        	end
endmodule
 
// register bank module
module registerbank (enable, R0, R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R11, R12, R13, R14, R15, DataReg);
input [15:0] enable;
input [31:0] DataReg;
output reg [31:0] R0, R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R11, R12, R13, R14, R15;

assign R0 = 0;
assign R1 = 0;
assign R2 = 0;
assign R3 = 0;
assign R4 = 0;
assign R5 = 0;
assign R6 = 0;
assign R7 = 0;
assign R8 = 0;
assign R9 = 0;
assign R10 = 0;
assign R11 = 0;
assign R12 = 0;
assign R13 = 0;
assign R14 = 0;
assign R15 = 0;

always@(*)
        	begin
                    	case(enable)
                                	16'b0000000000000001: R0= DataReg; //0
                                	16'b0000000000000010: R1 = DataReg; //1
                                	16'b0000000000000100: R2 =DataReg; //2
                                	16'b0000000000001000: R3 = DataReg; //3
                                	16'b0000000000010000: R4 = DataReg; //4
                                	16'b0000000000100000: R5 = DataReg; //5
                                	16'b0000000001000000: R6 = DataReg; //6
                                	16'b0000000010000000: R7 = DataReg; //7
                                	16'b0000000100000000: R8 = DataReg; //8
                                	16'b0000001000000000: R9 = DataReg; //9
                                	16'b0000010000000000: R10= DataReg; //10
                                	16'b0000100000000000: R11 = DataReg; //11
                                	16'b0001000000000000: R12 = DataReg; //12
                                	16'b0010000000000000: R13 = DataReg; //13
                                	16'b0100000000000000: R14 = DataReg; //14
                                	16'b1000000000000000: R15 = DataReg; //15
                                	default: R0 = 'bz; //default case Comment: I dont think we need this
                    	endcase
        	end
endmodule
 
// MUX module
module thirtytwo_bit_sixteen_to_one_MUX(SourceSel, Source, R0, R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R11, R12, R13, R14, R15);
input [3:0] SourceSel;
input [31:0]  R0, R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R11, R12, R13, R14, R15;
output reg [31:0] Source;
always@*
begin
                    	case(SourceSel)
                                	4'd0: Source = R0;
                                	4'd1: Source = R1;
                                	4'd2: Source = R2;
                                	4'd3: Source = R3;
                                	4'd4: Source = R4;
                                	4'd5: Source = R5;
                                	4'd6: Source = R6;
                                	4'd7: Source = R7;
                                	4'd8: Source = R8;
                                	4'd9: Source = R9;
                                	4'd10: Source = R10;
                                	4'd11: Source = R11;
                                	4'd12: Source = R12;
                                	4'd13: Source = R13;
                                	4'd14: Source = R14;
                                	4'd15: Source = R15;
                                	default: Source = 32'bz; //default case
                    	endcase
        	end
endmodule     

module ALU (OP_CODE, R3, R1, R2, COND, S, n, i); 
    //input CLK;
    input S;
	input [3:0] OP_CODE, COND;
	input [31:0] R1, R2;
	input [4:0] n;
	input [15:0] i;
	//inout [3:0] FLAGS_Reg;
	output reg [31:0] R3;
	wire [3:0] FLAGS;
	wire [31:0] DataOut;
	wire RESULT, Carry;
	reg [3:0] FLAGS_Reg;

    CONDITION cndcheck (R1, R2, COND, FLAGS_Reg, RESULT);
    OPCODE opcheck (OP_CODE, R1, R2, DataOut, Carry, n, i);
    SET_FLAGS flags (R1, R2, DataOut, Carry, FLAGS, S, OP_CODE);
	//always @ (posedge CLK) 
	always @ *
	begin
		if (RESULT)
		begin
			R3 = DataOut;	
			if (S) FLAGS_Reg = FLAGS;
			else if (OP_CODE == 4'b1011) FLAGS_Reg = FLAGS;
		end
		else R3 = 32'bz0000000000000000000000000000000;
	end
endmodule


module SET_FLAGS (R1, R2, R3, carryIn, FLAGS, S, OP_CODE);
    input [31:0] R1, R2, R3;
    input [3:0] OP_CODE;
    input carryIn, S;
    output reg [3:0] FLAGS;        //N = 3, Z=2, C=1, V=0
	always @ *
	begin
		if (S) begin
		FLAGS[3] = R3[31];
		FLAGS[2] = ~(|R3);
		FLAGS[1] = carryIn;
		FLAGS[0] = (!R1[31]&&!R2[31]&&R3[31])||(R1[31]&&R2[31]&&!R3[31]);
		end
		else if(OP_CODE == 4'b1011)
		begin
			FLAGS = (R1 == R2) ? 4'b0100 : ((R1 < R2) ? 4'b1000 : 4'b0010) ;
		end
		//FLAGS = (R1 == R2) ? 4'b0100 : ((R1 < R2) ? 4'b1000 : 4'b0010));
		
	end
endmodule

module CONDITION (R1, R2, COND, FLAGS, RESULT);
	input [3:0] FLAGS, COND;
	input [31:0] R1, R2;
	output reg RESULT;
	always @*
	begin
		case (COND)
			4'b0001 : RESULT = FLAGS[2] ? 1 : 0;
			4'b0010 : RESULT = ((FLAGS[2]==0) && (FLAGS[3] == FLAGS[0])) ? 1 : 0;
			4'b0011 : RESULT = (FLAGS[3] != FLAGS[0]) ? 1 : 0;
			4'b0100 : RESULT = (FLAGS[3] == FLAGS[0]) ? 1 : 0;
			4'b0101 : RESULT = ((FLAGS[2] == 1) || (FLAGS[3] != FLAGS[0])) ? 1 : 0;
			4'b0110 : RESULT = ((FLAGS[1] == 1) && (FLAGS[2] == 0)) ? 1 : 0;
			4'b0111 : RESULT = (FLAGS[1] == 0) ? 1 : 0;
			4'b1000 : RESULT = FLAGS[1] ? 1 : 0;
			4'b0000 : RESULT = 1; 
			default : RESULT = 1'bx;
		endcase
	end
endmodule

module OPCODE (OP_CODE, R1, R2, R3, Carry, n, i);
input [3:0] OP_CODE;
input [31:0] R1, R2;
input [4:0] n;
input [15:0] i;
output reg Carry;
output reg [31:0] R3;
wire [31:0] Add_Out, Sub_Out, Mul_Out, Or_Out, And_Out, Xor_Out, Lsr_Out, Lsl_Out, Ror_Out, DataOut;
wire Carry_Add, Carry_Sub, Carry_Mul, Carry_And, Carry_Or, Carry_Xor, Carry_Lsr, Carry_Lsl;
wire [31:0] Init_Out, Copy_Out, Adr_Out;

    ADDER add (R1, R2, Add_Out, Carry_Add);
    SUBTRACTOR subtract (R1, R2, Sub_Out, Carry_Sub);
    MULTIPLIER multiply (R1, R2, Mul_Out, Carry_Mul);
    AND and1 (R1, R2, And_Out, Carry_And);
    OR or1 (R1, R2, Or_Out, Carry_Or);
    XOR xor1 (R1, R2, Xor_Out, Carry_Xor);	
    LogicalShiftLeft lsl (Lsl_Out, R2, n, Carry_Lsl);
    RotateRight ror (Ror_Out, R2, n);
    LogicalShiftRight lsr(Lsr_Out, R2, n, Carry_Lsr);
    Address address(Adr_Out, i);
    INIT init(Init_Out, i);
    COPY copy(Copy_Out, R1);
    
		always@ *
		begin 

			case(OP_CODE)
				4'b0000 :  
					begin 
						R3 = {Add_Out[31:0]}; 
						Carry = Carry_Add;
					end
				4'b0001 :  
					begin 
						R3 = {Sub_Out[31:0]}; 
						Carry = Carry_Sub;
					end 
				4'b0010 :  
					begin
						R3 = {Mul_Out[31:0]}; 
						Carry = Carry_Mul;
					end 
				4'b0011 :  
					begin 
						R3 = {Or_Out[31:0]}; 
						Carry = Carry_Or;
					end 
				4'b0100 :  begin 
						R3 = {And_Out[31:0]}; 
						Carry = Carry_And;
					end 
				4'b0101 :  begin 
						R3 = {Xor_Out[31:0]}; 
						Carry = Carry_Xor;
					end 
				4'b0110 :  R3 = Init_Out;
				4'b0111 :  R3 = Copy_Out;
				4'b1000 :  begin
				        R3 = {Lsr_Out[31:0]};
				        Carry = Carry_Lsr;
				        end
				4'b1001 :  begin
				        R3 = {Lsl_Out[31:0]};
				        Carry = Carry_Lsl;
				        end
				4'b1010 :  R3 = {Ror_Out[31:0]};
				4'b1011 :  R3 = 'bz;
				4'b1100 :  R3 = Adr_Out;
				4'b1101 :  R3 = 'bz; //Changed from DataReg since its not defined
				4'b1110 :  R3 = 'bz;//Changed from DataReg since its not defined
				default :  R3 = R3; 
			endcase
		end
endmodule

module ADDER(R1, R2, Add_Out, Carry_Add);
	input [31:0] R1, R2;
	output Carry_Add;
	output [31:0] Add_Out;

    assign {Carry_Add, Add_Out} = R1+R2;

endmodule

module SUBTRACTOR(R1, R2, Sub_Out, Carry_Sub);
	input [31:0] R1, R2;
	output Carry_Sub;
	output [31:0] Sub_Out;

	assign {Carry_Sub, Sub_Out} = R1 - R2;

endmodule

module MULTIPLIER(R1, R2, Mul_Out, Carry_Mul);
	input [31:0] R1, R2;
	output Carry_Mul;
	output [31:0] Mul_Out;

	assign {Carry_Mul, Mul_Out} = R1*R2;

endmodule

module OR(R1, R2, Or_Out, Carry_Or);
	input [31:0] R1, R2;
	output Carry_Or;
	output [31:0] Or_Out;

	assign Or_Out = R1 | R2;
	assign Carry_Or = 0;

endmodule

module AND(R1, R2, And_Out, Carry_And);
	input [31:0] R1, R2;
	output Carry_And;
	output [31:0] And_Out;

	assign And_Out = R1 & R2;
	assign Carry_And=0;

endmodule

module XOR(R1, R2, Xor_Out, Carry_Xor);
	input [31:0] R1, R2;
	output Carry_Xor;
	output [31:0] Xor_Out;

	assign Xor_Out = R1 & R2;
	assign Carry_Xor=0;

endmodule

module INIT(Init_Out, i);
input [15:0] i;
output [31:0] Init_Out;

assign Init_Out = i;

endmodule

module COPY(Copy_Out, R1);
output[31:0] Copy_Out;
input [31:0] R1;

assign Copy_Out=R1;

endmodule 

module LogicalShiftRight (Lsr_Out, R2, n, Carry_Lsr);
input[31:0] R2;
input[4:0] n;
output [31:0] Lsr_Out;
output Carry_Lsr;

assign Lsr_Out = (R2 >> n);
assign Carry_Lsr = 0;
endmodule

module LogicalShiftLeft (Lsl_Out, R2, n, Carry_Lsl);
input[31:0] R2;
input[4:0] n;
output [31:0] Lsl_Out;
output Carry_Lsl;

assign Lsl_Out = (R2 << n);
assign Carry_Lsl = R2[32 - n];

endmodule

module RotateRight(Ror_Out, R2, n);
input[31:0] R2;
input[4:0] n;
output reg[31:0] Ror_Out;
integer i;

always @(*) begin
	Ror_Out = R2;
	for (i = 0; i <= n; i = i +1) begin
		Ror_Out = Ror_Out >> 1;

		if(R2[i] == 1'b1) begin
			Ror_Out[31] = 1'b1;
		end else begin
			Ror_Out[31] = 1'b0;
		end

	end

end

endmodule

module Address(Adr_Out, i);
input [15:0] i;
output [31:0] Adr_Out;

assign Adr_Out = i;

endmodule


// Mem_Control and MUXs
module MemoryControlAll (Source1, Source2, Result, DataBus, RW, PC, AdrBus, OpCode, DataReg);
input [3:0] OpCode;
input [31:0] Source1, Source2, Result; 
inout [31:0] DataBus;
input [7:0] PC;
output RW;
output [15:0] AdrBus;
output [31:0] DataReg;
wire [15:0] Adr;
wire Sel_Adr, Sel_LDR;
wire [31:0] LDRData;
mem_control MC (Source1, Source2, OpCode, DataBus, Adr, LDRData, RW, Sel_LDR, Sel_Adr);

AddBusMUX Add_MUX (Adr, AdrBus, PC, Sel_Adr);
LDR_MUX ldrMUX (DataReg, Result, LDRData, Sel_LDR);

endmodule

// Memory Control
module mem_control (Source1, Source2, OpCode, DataBus, Adr, LDRData, RW, Sel_LDR, Sel_Adr);
input [3:0] OpCode;
input [31:0] Source1, Source2; 
output reg RW, Sel_LDR, Sel_Adr;
output reg [31:0] LDRData;
output reg [15:0] Adr;
inout [31:0] DataBus;

assign DataBus = (OpCode == 4'b1110 ) ? Source2 : 'bz; //out (If opcode = store, the Data bus is going out and equals Source2)
assign Adr = Source1;
assign DataBus = Source2;
assign Sel_Adr = 0;
assign Sel_LDR = 0;
assign RW = 1;
always@*
begin
case (OpCode)
4'b1101: 
begin
LDRData = DataBus;
Sel_Adr = 1;
RW = 1;
Sel_LDR = 1; end
4'b1110: 
begin
//DataBus = Source2; dont need this since it is already above
Sel_Adr = 1; 
RW = 0;
end
default: 
	begin
		LDRData = 32'bz;
		Sel_Adr = 0;
		Sel_LDR = 0;
	end

endcase
end
endmodule 


// AddBusMUX
module AddBusMUX (Adr, AdrBus, PC, Sel_Adr);
input [15:0] Adr;
input [7:0]PC;
input Sel_Adr;
output reg [15:0] AdrBus;

always@*
begin
		if(Sel_Adr)
			AdrBus = Adr;
else
AdrBus = PC;
end
endmodule 


// LDR MUX
module LDR_MUX (DataReg, Result, LDRData, Sel_LDR);
input [31:0] Result, LDRData;
input Sel_LDR;
output reg [31:0] DataReg;

always@*
begin
	if(Sel_LDR)
		DataReg = LDRData;
else
DataReg = Result;
end
endmodule 
