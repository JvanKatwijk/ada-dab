--
--    Copyright (C) 2016
--    Jan van Katwijk (J.vanKatwijk@gmail.com)
--    Lazy Chair Programming
--
--    This file is part of the SDR-J (JSDR).
--    SDR-J is free software; you can redistribute it and/or modify
--    it under the terms of the GNU General Public License as published by
--    the Free Software Foundation; either version 2 of the License, or
--    (at your option) any later version.
--
--    SDR-J is distributed in the hope that it will be useful,
--    but WITHOUT ANY WARRANTY; without even the implied warranty of
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--    GNU General Public License for more details.
--
--    You should have received a copy of the GNU General Public License
--    along with SDR-J; if not, write to the Free Software
--    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
--
with Text_IO; use Text_IO;
package body Viterbi_Handler is
	use Interfaces;
	procedure Initialize (Object: in out Viterbi_Processor) is
	begin
	   Object. handler	:=
	            create_viterbi (Interfaces. C. int (Object. wordlength));
	   if Object. handler = Null_Address then
	      put_line ("disaster");
	      Object. isOK	:= false;
	   else
	      Object. isOK	:= true;
	      Object. Symbols	:= new c_intArray (0 .. 4 * (Object. wordlength + (K - 1)) - 1);
	   end if;
	end Initialize;

	procedure Finalize	(Object: in out Viterbi_Processor) is
	begin
	   if Object. isOK then
	      delete_viterbi  (Object. handler);
	      Free_c_intArray (Object. Symbols);
	   end if;
	end Finalize;

	maskVector : constant byteArray (0 .. 7) :=
	                         (1, 2, 4, 8, 16, 32, 64, 128);

	function Getbit	(v : uint8_t; o: Integer) return uint8_t is
	   mask : uint8_t    := maskVector (7 - o);
	begin
	   return (if (v and mask) /= 0 then 1 else 0);
	end Getbit;

	procedure Deconvolve (Object : in out Viterbi_Processor;
	                      Input  : shortArray;
	                      Output : out byteArray) is
	   Temp_Value	: int16_t;
	   subtype outBuffer is
	           byteArray (0 .. (Object. wordlength + (K - 1)) / 8 - 1);
	   Data	: outBuffer;
	   subtype dataVectype is c_intArray (0 .. 4 * (Object. wordlength + (K - 1)) - 1);
	   procedure Update_Viterbi_Blk (Handle    : system. Address;
	                                 Symbols   : dataVectype;
	                                 nbits     : Interfaces. C. int);
	   pragma Import (C, Update_Viterbi_Blk, "update_viterbi_blk_GENERIC");
	   procedure Chainback_Viterbi (Handle     : system. Address;
	                                Output     : out outBuffer;
	                                Wordlength : Interfaces. C. int;
                                        EndState   : Interfaces. C. int);
	   pragma Import (C, Chainback_Viterbi, "chainback_viterbi");

	begin
	   if not Object. isOK then
	      return;
	   end if;

	   Init_Viterbi (Object. handler, 0);
	   
	   for I in  Object. Symbols' Range loop
	      Temp_Value	:= -Input (Input' First + I) + 127;
	      if Temp_Value < 0 then
	         Temp_Value := 0;
	      elsif Temp_Value > 255 then
	         Temp_Value := 255;
	      end if;
	      Object. Symbols (I) := Interfaces. C. int (Temp_Value);
	   end loop;
--
--	The real work is done by C functions
	   Update_Viterbi_Blk (Object. Handler,
	                       Object. Symbols. all, 
	                       Interfaces. C. int (Object. Wordlength + (K - 1)));
	   Chainback_Viterbi (Object. Handler,
	                      Data,
	                      Interfaces. C. int (Object. Wordlength), 0);

	   for I in  0 ..  uint16_t (Object. Wordlength) - 1 loop
	      Output (Output' First + Integer (I)) :=
	                Getbit (Data (Integer (Shift_Right (I, 3))),
	                                         Integer (I and 8#07#));
	   end loop;
	end Deconvolve;
end viterbi_handler;

