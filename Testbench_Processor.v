module test_initiatemodule;
	reg Clock;
 
Processor proc(Clock);

initial
begin
$readmemb("data_b.txt", proc.ram.Mem);
Clock = 0;
#150 $writememb("out_data_b.txt", proc.ram.Mem);	
#155 $finish;

end

always #2 Clock=~Clock;

initial
begin
$monitor($time, "R0=%d, R1=%d, R2=%d, R3=%d, R4=%d, R5=%d, R6=%d, R7=%d, R8=%d, R9=%d, R10=%d, R11=%d, R12=%d R13=%d, R14=%d, R15=%d", proc.register.RegBank.R0, proc.register.RegBank.R1, proc.register.RegBank.R2, proc.register.RegBank.R3, proc.register.RegBank.R4, proc.register.RegBank.R5, proc.register.RegBank.R6, proc.register.RegBank.R7, proc.register.RegBank.R8, proc.register.RegBank.R9, proc.register.RegBank.R10, proc.register.RegBank.R11, proc.register.RegBank.R12, proc.register.RegBank.R13, proc.register.RegBank.R14, proc.register.RegBank.R15);
end
endmodule
