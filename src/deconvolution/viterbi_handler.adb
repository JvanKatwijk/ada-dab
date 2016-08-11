with Text_IO; use Text_IO;
with Interfaces; use Interfaces;
with Ada. Exceptions; use Ada. Exceptions;
package body viterbi_handler is
procedure Initialize (Object: in out viterbiProcessor) is
begin
	Object. handler	:= create_viterbi (Object. wordlength);
	if Object. handler = Null_Address
	then
	   put_line ("disaster");
	   Object. isOK	:= false;
	else
	   Object. isOK	:= true;
	end if;
	Object. symbols	:= new uintArray (0 .. 4 * (Object. wordlength + (K - 1)) - 1);
end Initialize;

procedure Finalize	(Object: in out viterbiProcessor) is
begin
	if Object. isOK
	then
	--   delete_viterbi (Object. handler);
	   Free_uintArray (Object. symbols);
	end if;
end Finalize;

function	getBit	(v	: uint8_t; o: Integer) return uint8_t is
mask	: uint8_t	:= Shift_Left (1, 7 - o);
begin
	return (if (v and mask) /= 0 then 1 else 0);
end getBit;

procedure	deconvolve (Object	: in out viterbiProcessor;
	                    input	: shortArray;
	                    output	: out byteArray) is
temp	: Integer;
subtype outBuffer is byteArray (0 .. (Object. wordlength + (K - 1)) / 8 - 1);
data	: outBuffer;
begin
	if not Object. isOK
	then
	   return;
	end if;
	init_viterbi (Object. handler, 0);
	for i in 0 .. (Object. wordLength + (K - 1)) * rate - 1 loop
	   temp	:= Integer (-input (Integer (i))) + 127;
	   if temp < 0
	   then
	      temp := 0;
	   elsif temp > 255
	   then
	      temp := 255;
	   end if;
	   Object. symbols (i) := uint32_t (temp);
	end loop;
	update_viterbi_blk (Object. handler,
	                    Object. symbols. all' Address, 
	                    Object. wordlength + (K - 1));
	chainback_viterbi (Object. handler,
	                   data' Address,
	                   Object. wordlength, 0);
	for i in 0 .. uint16_t (Object. wordlength) - 1 loop
	   output (Integer (i)) := getBit (data (Integer (Shift_Right (i, 3))),
	                                         Integer (i and 8#07#));
	end loop;
	exception
	   when others => put (" foutj toch ergens hier\n");
	   raise;
end deconvolve;
end viterbi_handler;

