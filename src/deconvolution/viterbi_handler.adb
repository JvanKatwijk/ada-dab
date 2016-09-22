with Text_IO;		use Text_IO;
with Interfaces;	use Interfaces;
with Ada. Exceptions;	use Ada. Exceptions;

package body Viterbi_Handler is
	procedure Initialize (Object: in out Viterbi_Processor) is
	begin
	   Object. handler	:= create_viterbi (Object. wordlength);
	   if Object. handler = Null_Address then
	      put_line ("disaster");
	      Object. isOK	:= false;
	   else
	      Object. isOK	:= true;
	   end if;
	   Object. Symbols	:= new uintArray (0 .. 4 * (Object. wordlength + (K - 1)) - 1);
	end Initialize;

	procedure Finalize	(Object: in out Viterbi_Processor) is
	begin
	   if Object. isOK then
--	delete_viterbi (Object. handler);
	      Free_uintArray (Object. Symbols);
	   end if;
	end Finalize;

	function Getbit	(v: uint8_t; o: Integer) return uint8_t is
	   mask	: uint8_t	:= Shift_Left (1, 7 - o);
	begin
	   return (if (v and mask) /= 0 then 1 else 0);
	end Getbit;

	procedure Deconvolve (Object: in out Viterbi_Processor;
	                      Input:  shortArray;
	                      Output: out byteArray) is
	   Temp_Value	: Integer;
	   subtype outBuffer is
	           byteArray (0 .. (Object. wordlength + (K - 1)) / 8 - 1);
	   data	: outBuffer;
	begin
	   if not Object. isOK then
	      return;
	   end if;
	   Init_Viterbi (Object. handler, 0);
	   for I in 0 .. (Object. wordLength + (K - 1)) * rate - 1 loop
	      Temp_Value	:= Integer (-input (Integer (I))) + 127;
	      if Temp_Value < 0 then
	         Temp_Value := 0;
	      elsif Temp_Value > 255 then
	         Temp_Value := 255;
	      end if;
	      Object. symbols (I) := uint32_t (Temp_Value);
	   end loop;
	   Update_Viterbi_Blk (Object. Handler,
	                       Object. Symbols. all' Address, 
	                       Object. Wordlength + (K - 1));
	   Chainback_Viterbi (Object. Handler,
	                      Data' Address,
	                      Object. Wordlength, 0);
	   for I in 0 .. uint16_t (Object. Wordlength) - 1 loop
	      output (Integer (I)) :=
	                Getbit (data (Integer (Shift_Right (i, 3))),
	                                         Integer (i and 8#07#));
	   end loop;
	end Deconvolve;
end viterbi_handler;

