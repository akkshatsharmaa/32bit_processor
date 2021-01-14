module tb_reg_decoder;
reg [3:0] destination;
reg [31:0] DataReg;
wire [15:0] enable;
wire [31:0] R0, R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R11, R12, R13, R14, R15;

_4_to_16_decoder decoder(destination, enable);
registerbank rb(enable,R0, R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R11, R12, R13, R14, R15, DataReg);
initial
begin
#10 DataReg = 32'bz;
#5 destination = 4'd0; DataReg = 32'b00000110001010000000000001100000;
#5 destination = 4'd9; DataReg = 32'b00000110000010000000000000100000;
end

initial
begin
$monitor($time, "R0=%b , R1=%b ,R2=%b ,R3=%b ,R4=%b ,R5=%b ,R6=%b ,R7=%b ,R8=%b ,R9=%b ,R10=%b ,R11=%b ,R12=%b ,R13=%b ,R14=%b ,R15=%b", R0, R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R11, R12, R13, R14, R15);
end
endmodule