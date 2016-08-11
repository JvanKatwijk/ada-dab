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
with dab_handler;
with protection_handler;
with uep_handler;	use uep_handler;
with eep_handler;	use eep_handler;
with header;	use header;
with Text_IO;	use Text_IO;
with Ada. Unchecked_Deallocation;
with Ada. Exceptions; use Ada. Exceptions;
package body dab_handler is
interleaveDelays	: constant  shortArray (0 .. 16 - 1) := 
	(15, 7, 11, 3, 13, 5, 9, 1, 14, 6, 10, 2, 12, 4, 8, 0);
task body dabProcessor is
        procedure Free_uepProcessor is new Ada. Unchecked_DeAllocation (
	   Object => uepProcessor, Name => uepProcessor_P);
	procedure Free_eepProcessor is new Ada. Unchecked_DeAllocation (
	   Object => eepProcessor, Name => eepProcessor_P);
	outV			: byteArray (0 ..  Integer (bitRate * 24 - 1));
	interleaveData		: shortBlock (0 .. fragmentSize - 1, 0 .. 15);
	countforInterleaver	: short_Integer;
	the_protectionProcessor	: protection_handler. protectionProcessor_P;
	the_uepProcessor	: uep_handler. uepProcessor_P;
	the_eepProcessor	: eep_handler. eepProcessor_P;
	audioProcessor		: mp4_handler. mp4Processor_P;
	tempBuffer		: shortArray (0 .. fragmentSize - 1);
begin
	interleaveData		:= (others => (others => 0));
	countforInterleaver	:= 0;

--	if uepFlag = 0
--	then
--	   the_uepProcessor := new uepProcessor (bitRate, protLevel);
--	else
--	   the_eepProcessor := new eepProcessor (bitRate, protLevel);
--	end if;
	if uepFlag = 0
	then
	   the_protectionProcessor := new uepProcessor (bitRate, protLevel);
	else
	   the_protectionProcessor := new eepProcessor (bitRate, protLevel);
	end if;
	audioProcessor	:= new mp4Processor (bitRate);

	loop
	   select
	      accept	stop;
	      if uepFlag = 0
	      then
	         Free_uepProcessor	(the_uepProcessor);
	      else
	         Free_eepProcessor	(the_eepProcessor);
	      end if;
	      exit;
	   or 
	      accept Process (Data	: shortArray) do
	         tempBuffer	:= Data;
	      end Process;
-- first interleaving, we do it in-line
	      for i in Integer range 0 .. fragmentSize - 1 loop
	         declare
	            index	: Integer := Integer (i mod 16);
	         begin
	            interleaveData (i, interleaveDelays (index)) :=
	                                                tempBuffer (i);
	            tempBuffer (i) := interleaveData (i, 0);
	            for j in short_Integer Range 1 ..
	                            interleaveDelays (index) loop
	               interleaveData (i, j - 1) := interleaveData (i, j);
	            end loop;
	         end;
	      end loop;

	      if countforInterleaver < 15 
	      then
	         countforInterleaver := countforInterleaver + 1;
	      else
	         the_protectionProcessor. deconvolve (tempBuffer, outV);
--
--	the in-line energy dispersal
	         declare
	            shiftRegister	: byteArray (0 .. 8) :=  (others => 1);
	         begin
	            for i in outV' Range loop
	               declare
	                  b : uint8_t := shiftRegister (8) xor shiftRegister (4);
	               begin
	                  for j in reverse 1 .. 8 loop
	                     shiftRegister (j) := shiftRegister (j - 1);
	                  end loop;
	                  shiftRegister (0)	:= b;
	                  outV (i)		:= outV (i) xor b;
	               end;
	            end loop;
	         end;
	         audioProcessor. addtoFrame (outV, 24 * bitRate);
	      end if;
	      end select;
	end loop;
end dabProcessor;
end dab_handler;
