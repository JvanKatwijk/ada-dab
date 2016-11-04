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
--	Driver program for processing the MSC.
--	Operations (apart from selecting
--	the local frame in the MSC vector)
--	1. deinterleaving
--	2. deconvolution (including depuncturing)
--	3. energy dispersal
--	4. in case of DAB: creating MP2 packets
--	5. in case of DAB+: assembling superframes and creating MP4 packets
--	the mscProcessor is merely a dispatcher.
--	It is implemented as an Ada task, to allow access from different
--	sources (i.e. a request for a new channel can be simultaneosly
--	with the processing of incoming dab data.

--	Note CIF counts from 0 .. 3
with header;		use header;
with Text_IO;		use Text_IO;
with Ada. Exceptions;	use Ada. Exceptions;
with Ada. Unchecked_Deallocation;
with dab_handler;	use dab_handler;
with audiopackage;
package body msc_handler is
	Audio_Handler	: audiopackage. audioSink_P;
	task msc_processor is
	   entry reset;
	   entry stop;
	   entry process_mscBlock (Fbits : shortArray; Blkno : Integer);
	   entry set_audioData    (Data  : audioData);
	end msc_processor;

	procedure reset is
	begin
	    msc_processor. reset;
	end reset;

	procedure set_audioData    (Data : audioData) is
	begin
	   msc_processor. set_AudioData (Data);
	end set_audioData;

	procedure stop is
	begin
	   msc_processor. stop;
	end;

	function cifs (the_Mode : dabMode) return Integer is
	   Blocks_per_CIF : Integer;
	begin
	   case the_Mode is
	      when Mode_1	=> Blocks_per_CIF := 18;
	      when Mode_2	=> Blocks_per_CIF := 72;
	      when Mode_4	=> Blocks_per_CIF := 36;
	      when others	=> Blocks_per_CIF := 18; -- should not happen
	   end case;
	   return Blocks_per_CIF;
	end;

	procedure process_mscBlock	(Fbits	: shortArray;
	                                 Blkno	: Integer) is
	begin
	   msc_processor. process_mscBlock (Fbits, Blkno);
	end process_mscBlock;
	   
	procedure Free_dabProcessor is new Ada. Unchecked_DeAllocation (
	   Object	=> dab_handler. dabProcessor,
	   Name		=> dab_handler. dabProcessor_P);

	procedure Free_audioProcessor is new Ada. Unchecked_DeAllocation (
	   Object	=> audiopackage. audioSink,
	   Name		=> audiopackage. audioSink_P);
--
	task body msc_processor is
	   CUSize          : constant Integer := 4 * 16;
	   Cif_Vector      : shortArray (0 .. 55296 - 1);
	   Cif_Count       : Integer          := 0;
	   The_Data        : audioData;
	   Dabmodus        : dataMode;
	   Current_Block   : Integer          := 0;
	   Work_To_Be_Done : Boolean          := False;
	   Start_Address   : short_Integer;
	   Length          : short_Integer;
	   The_Dabprocessor: Dab_Handler. Dabprocessor_P := null;
	   endMSC          : exception;
	   Bits_per_Block  : Integer	:= 2 * K (the_Mode);
	   Blocks_per_CIF  : Integer	:= cifs (the_Mode);
	begin
--	The main loop
	   loop
	      select
	         accept reset;
	         Work_To_Be_Done   := false;
	         Current_Block     := 0;
	         Cif_Count         := 0;
	      or
	         accept stop;
	         if The_Dabprocessor /= null
	         then
	            The_Dabprocessor. stop;
	            while not The_Dabprocessor' terminated loop
	               delay 0.01;
	            end loop;
	            put_line ("old dab handler now stopped");
	            Free_dabProcessor (the_dabProcessor);
	         end if;
	         raise endMSC;
	      or 
	         accept Set_AudioData (Data: audioData) do
	            The_Data         := Data;
	         end Set_AudioData;
	         Work_To_Be_Done     := true;
	         Start_Address       := The_Data. startAddr;
	         Length              := The_Data. Length;
	         if The_Data. ASCTy = 8#077# then
	            Dabmodus         := DAB_PLUS;
	         else
	            Dabmodus         := DAB;
	         end if;

	         if The_Dabprocessor /= null then
	            The_Dabprocessor. stop;
	            while not The_Dabprocessor' terminated loop
	               delay 0.01;
	            end loop;
	            put_line ("old dab handler now stopped");
	            Free_dabProcessor (the_dabProcessor);
	         end if;
	         The_Dabprocessor	:= new Dab_Handler. Dabprocessor
	                                            (Dabmodus,
	                                    Integer (The_Data. Length) * CUSize,
	                                             The_Data. bitRate,
	                                             The_Data. uepFlag,
	                                             The_Data. protLevel,
	                                             Audio_Handler);

	      or
	         accept Process_mscBlock (fbits	: shortArray;
	                                  blkno	: Integer) do
	            if Work_To_Be_done then
	               Current_Block := (blkno - 5) mod Blocks_per_CIF;
	               Cif_Vector (Current_Block * Bits_per_Block .. 
	                          (Current_Block  + 1) * Bits_per_Block - 1) :=
	                                        fbits;
	            else
	               return;
	            end if;
	         end process_mscBlock;
	         if Current_Block >= Blocks_per_CIF - 1 then  --  a full CIF
	            Cif_Count	:= (Cif_Count + 1) mod 4;
	            The_DabProcessor.
	                   Process (Cif_Vector (Integer (Start_Address) * CUSize ..
	                                 Integer (Start_Address + Length) * CUSize - 1));
	         end if;
	      end select;
	   end loop;
	exception
	   when endMSC	=> put_line ("msc_processor normally terminated");
	   when Error: others		=> Put ("Exception in mscHandler: ");
	                                   Put_Line (Exception_Name (Error));
	   
	end msc_processor;
	res	: Boolean;
begin

	Audio_Handler       := new  audiopackage.
	                              audioSink (48000,
	                                         32768,
	                                         audiopackage. HIGH_LATENCY);
	Audio_Handler. selectDefaultDevice (res);
	if res then
	   put_line ("setting default device succeeded");
	end if;
	Audio_Handler. portAudio_start (res);
	if res then
	   put_line ("starting device succeeded");
	end if;
end msc_handler;


