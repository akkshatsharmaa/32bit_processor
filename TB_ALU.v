
module TB_ALUMain;
reg [31:0] R1, R2;
reg S;
reg [3:0] OP_CODE, COND, Flags_In;
reg [4:0] n;
reg [15:0] i;
wire [31:0] Output;
wire [3:0] FLAGS;

assign FLAGS = (COND) ? Flags_In : 'bz;

initial
begin
$display($time, " TestMy Design");
R1 = 0; R2 = 0; S = 0; OP_CODE = 4'b0110; COND = 0; Flags_In = 0; i = 4'b1100; n = 1;
#5 i = 3'b100; n = 0; 
#5 i = 4'b1000; n = 1;
#5 i = 3'b110; n = 0;
#5 OP_CODE = 0; R1 = 4; R2 = 6; i = 0; n = 0;
#5 OP_CODE = 4'b1001;  R1 = 0; R2 = 6; n = 2; i = 5'b10000;
#5 OP_CODE = 4'b0010; R1 = 4; R2 = 6; n = 0; i = 0;
#5 OP_CODE = 4'b0001; S = 1; R1 = 24; R2 = 24; 
#5 OP_CODE = 4'b1011; S = 0; R1 = 24; R2 = 10; 
#5 OP_CODE = 4'b0110; COND = 1; R1 = 0; R2 = 0; i = 7'b01111111;
#5 OP_CODE = 4'b0110; COND = 1; R1 = 0; R2 = 1; i = 7'b01111111;
#5 OP_CODE = 4'b0000; COND = 1; R1 = 24; R2 = 24; i = 0;
#5 OP_CODE = 4'b0000; COND = 4'b0010; R1 = 26; R2 = 24; i = 0;
#5 OP_CODE = 4'b0000; COND = 4'b0011; R1 = 26; R2 = 24; i = 0;

end

initial
begin
$monitor($time, " OP_Code=%b, R1=%d, R2=%d, Output=%d, Output=%b, Flags=%b, Condition=%b, S=%d, n=%d, i=%d", OP_CODE, R1, R2, Output, Output, FLAGS, COND, S, n, i);
end

ALU alumain(OP_CODE, Output, R1, R2, FLAGS, COND, S, n, i);

endmodule