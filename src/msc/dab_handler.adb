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
with protection_handler;
with header;		use header;
with uep_handler;	use uep_handler;
with eep_handler;	use eep_handler;
with Text_IO;		use Text_IO;
with Ada. Unchecked_Deallocation;
with Ada. Exceptions; use Ada. Exceptions;
with audio_handler;
with mp4_handler;
with mp2_handler;

package body dab_handler is
	interleaveDelays: constant  shortArray (0 .. 16 - 1) := 
	          (15, 7, 11, 3, 13, 5, 9, 1, 14, 6, 10, 2, 12, 4, 8, 0);
	task body dabProcessor is
           procedure Free_uepProcessor is new Ada. Unchecked_DeAllocation (
	      Object => uepProcessor, Name  => uepProcessor_P);
           procedure Free_eepProcessor is new Ada. Unchecked_DeAllocation (
	      Object => eepProcessor, Name => eepProcessor_P);
	   procedure Free_mp2Processor is new Ada. Unchecked_Deallocation (
	      Object => mp2_handler. mp2Processor,
	      Name   => mp2_handler. mp2Processor_P);
	   procedure Free_mp4Processor is new Ada. Unchecked_Deallocation (
	      Object  => mp4_handler. mp4Processor,
	      Name    => mp4_handler. mp4Processor_P);
	   outV:       byteArray (0 ..  Integer (bitRate * 24 - 1));

	   InterleaveData: shortBlock (0 .. fragmentSize - 1, 0 .. 15);
	   countforInterleaver: short_Integer;

	   The_ProtectionProcessor: protection_handler. protectionProcessor_P;

	   tempBuffer:         shortArray (0 .. fragmentSize - 1);
	   The_AudioProcessor: audio_handler. Audio_Processor_P;
	begin
	   InterleaveData       := (others => (others => 0));
	   countforInterleaver  := 0;

	   if uepFlag = 0 then
	      The_ProtectionProcessor :=
	                         new uepProcessor (bitRate, protLevel);
	   else
	      The_ProtectionProcessor :=
	                         new eepProcessor (bitRate, protLevel);
	   end if;

	   if dabModus = DAB then
	      The_AudioProcessor :=
	                       new mp2_handler. mp2Processor (bitRate, audio);
	   else
	      The_AudioProcessor :=
	                       new mp4_handler. mp4Processor (bitRate, audio);
	   end if;
--
--	It looks strange to me that we can use a single pointer to these
--	two processors and that then deallocation should be as clumsy
--	as below
	   loop
	      select
	         accept stop;
	            if uepFlag = 0 then
	               Free_uepProcessor (uepProcessor_P (The_ProtectionProcessor));
	            else
	               Free_eepProcessor (eepProcessor_P (The_ProtectionProcessor));
	            end if;

	            if dabModus = DAB then
	               Free_mp2Processor (mp2_handler.
	                                  mp2Processor_P (The_AudioProcessor));
	            else
	               Free_mp4Processor (mp4_handler.
	                                  mp4Processor_P (The_AudioProcessor));
	            end if;
	            exit;
	         or 
	            accept Process (Data: shortArray) do
	               tempBuffer   := Data;
	            end Process;
-- first interleaving, we do it in-line
	            for I in Integer range 0 .. fragmentSize - 1 loop
	               declare
	                  Index   : Integer := Integer (I mod 16);
	               begin
	                  interleaveData (I, interleaveDelays (Index)) :=
	                                                tempBuffer (I);
	                  tempBuffer (I) := interleaveData (I, 0);
	                  for J in short_Integer Range 1 ..
	                               interleaveDelays (index) loop
	                     interleaveData (I, J - 1) := interleaveData (I, J);
	                  end loop;
	               end;
	            end loop;

	            if countforInterleaver < 15 then
	               countforInterleaver := countforInterleaver + 1;
	            else
	               The_ProtectionProcessor. deconvolve (tempBuffer, outV);
--
--	the in-line energy dispersal
	               declare
	                  shiftRegister: byteArray (0 .. 8) :=  (others => 1);
	               begin
	                  for I in outV' Range loop
	                     declare
	                        B: uint8_t := shiftRegister (8) xor
	                                                shiftRegister (4);
	                     begin
	                        for J in reverse 1 .. 8 loop
	                           shiftRegister (J) := shiftRegister (J - 1);
	                        end loop;
	                        shiftRegister (0)   := b;
	                        outV (I)            := outV (I) xor b;
	                     end;
	                  end loop;
	               end;
	               The_AudioProcessor. Add_to_Frame (outV, 24 * bitRate);
	            end if;
	      end select;
	   end loop;
	end dabProcessor;
end dab_handler;
