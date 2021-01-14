//This test bench loads a file into the ram reads from the ram, writes to the ram and outputs the end ram result
module TB_RAM;
reg ReadWrite;
reg [15:0] Address;
reg [31:0] Data_in;
wire [31:0] InstructionFetch, Data;

assign Data = (!ReadWrite) ? Data_in : 'bz; 

initial
begin
$display($time, " TestMy Design");
$readmemb("data_b.txt", ram.Mem);

ReadWrite = 1; Address = 16'b0000000000000001;  
#10 ReadWrite = 1; Address = 16'b0000000000100001; 
#10 ReadWrite = 0; Address = 16'b0000000000010001; Data_in = 3409;
#10 ReadWrite = 1; Address = 16'b0000000000000001;
#10 ReadWrite = 1; Address = 16'b0000000000010001;

$writememb("output_data_b.txt", ram.Mem);
end

initial
begin
$monitor($time, " instruction fetch=%d, data=%d, Address=%d, Read/Write=%b", InstructionFetch, Data, Address, ReadWrite );
end

RAM ram(ReadWrite, Data, Address, InstructionFetch);

endmodule