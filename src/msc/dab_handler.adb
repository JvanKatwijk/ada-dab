--
--    Copyright (C) 2016
--    Jan van Katwijk (J.vanKatwijk@gmail.com)
--    Lazy Chair Computing
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
	interleaveMap : constant shortArray (0 .. 16 - 1) :=
	          (0,  8,  4, 12,  2, 10, 6, 14, 1, 9, 5, 13, 3, 11, 7, 15);

	procedure Free_uepProcessor is new Ada. Unchecked_DeAllocation (
	    Object => uepProcessor, Name  => uepProcessor_P
	);
	procedure Free_eepProcessor is new Ada. Unchecked_DeAllocation (
	   Object => eepProcessor, Name => eepProcessor_P
	);
	procedure Free_mp2Processor is new Ada. Unchecked_Deallocation (
	   Object => mp2_handler. mp2Processor,
	   Name   => mp2_handler. mp2Processor_P
	);
	procedure Free_mp4Processor is new Ada. Unchecked_Deallocation (
	   Object  => mp4_handler. mp4Processor,
	   Name    => mp4_handler. mp4Processor_P
	);
--
	procedure initialize (Object : in out dabProcessor) is
	begin
	   Object. interleaveData         := (others => (others => 0));
	   Object. countforInterleaver    := 0;
	   Object. interleaverIndex       := 0;
	   if Object. uepFlag = 0 then
	      Object. The_ProtectionProcessor :=
	                         new uepProcessor (Object. bitRate,
	                                           Object. protLevel);
	   else
	      Object. The_ProtectionProcessor :=
	                         new eepProcessor (Object. bitRate,
	                                           Object. protLevel);
	   end if;

	   if Object. dabModus = DAB then
	      Object. The_AudioProcessor :=
	                       new mp2_handler. mp2Processor (Object.bitRate,
	                                                      Object. audio);
	   else
	      Object. The_AudioProcessor :=
	                       new mp4_handler. mp4Processor (Object. bitRate,
	                                                      Object. audio);
	   end if;
	end;

	procedure Finalize (Object : in out dabProcessor) is
	begin
	   if Object. uepFlag = 0 then
	      Free_uepProcessor (
	               uepProcessor_P (Object. The_ProtectionProcessor));
	   else
	      Free_eepProcessor (
	               eepProcessor_P (Object. The_ProtectionProcessor));
	   end if;

	   if Object. dabModus = DAB then
	      Free_mp2Processor (mp2_handler.
	                         mp2Processor_P (Object. The_AudioProcessor));
	   else
	      Free_mp4Processor (mp4_handler.
	                         mp4Processor_P (Object. The_AudioProcessor));
	   end if;
	end;

	procedure Process (Object : in out dabProcessor;
	                                   data : shortArray) is
	   tempX  : shortArray (0 .. Object. fragmentSize - 1);
	   outV   : byteArray  (0 .. Integer (Object. bitRate * 24 - 1));
	begin
-- first interleaving, we do it in-line
	   for I in Integer range 0 .. Object. fragmentSize - 1 loop
	      declare
	         Index          : Integer := Integer (I mod 16);
	         currentRow     : int16_t :=
	                 int16_t ((Object. interleaverIndex +
	                                    interleaveMap (Index)) mod 16);
	      begin
	         tempX (I) := Object. Interleavedata (currentRow, I);
	         Object. interleaveData (Object. InterleaverIndex, I) :=
	                                                data (data' First + I);
              end;
	   end loop;

	   Object. interleaverIndex :=
	                  int16_t ((Object. interleaverIndex + 1) mod 16);

--	just wait until the interleaver is "filled"
	   if Object. countforInterleaver < 15 then
	      Object. countforInterleaver :=
	                              Object. countforInterleaver + 1;
	      return;
	   end if;
--
--	tempX now contains the output of the interleaver, apply 
--	deconvolution and energy dispersal

	   begin
	      Object. The_ProtectionProcessor. deconvolve (tempX, outV);
	   exception
	      when Others => put_line ("in prot handler");
	      raise;
	   end;
--	the in-line energy dispersal
	   declare
	      shiftRegister: byteArray (0 .. 8) :=  (others => 1);
	   begin
	      for I in outV' Range loop
	         declare
	            B: uint8_t := shiftRegister (8) xor shiftRegister (4);
	         begin
	            for J in reverse 1 .. 8 loop
	               shiftRegister (J) := shiftRegister (J - 1);
	            end loop;
	            shiftRegister (0)   := b;
	            outV (I)            := outV (I) xor b;
	         end;
	      end loop;
	   end;
--
--	and the result will be processed as audio
	   Object. The_AudioProcessor. Add_to_Frame (outV,
	                                                24 * Object. bitRate);
	end Process;
end dab_handler;
