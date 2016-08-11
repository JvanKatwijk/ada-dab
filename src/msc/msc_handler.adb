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
with msc_handler;
with header;	use header;
with Text_IO;	use Text_IO;
with Ada. Exceptions; use Ada. Exceptions;
with Ada. Unchecked_Deallocation;
with dab_handler; use dab_handler;
package body msc_handler is
	mode		: dabMode	:= Mode_1;	-- default
	bitsperBlock	: Integer	:= 2 * 1536;	-- default
	blocksperCIF	: Integer	:= 18;		-- default;

	task msc_processor is
	   entry set_Mode	(mode	: dabMode);
	   entry reset;
	   entry stop;
	   entry process_mscBlock (fbits: shortArray; blkno : Integer);
	   entry set_audioData	  (d	: audioData);
	end msc_processor;

	procedure set_Mode	(mode: dabMode) is
	begin
	   msc_processor. set_Mode (mode);
	end set_Mode;
	
	procedure reset is
	begin
	    msc_processor. reset;
	end reset;

	procedure set_audioData	(d	: audioData) is
	begin
	   msc_processor. set_AudioData (d);
	end set_audioData;

	procedure stop is
	begin
	   msc_processor. stop;
	end;

	procedure process_mscBlock	(fbits	: shortArray;
	                                 blkno	: Integer) is
	begin
	   msc_processor. process_mscBlock (fbits, blkno);
	end process_mscBlock;
	   
	procedure Free_dabProcessor is new Ada. Unchecked_DeAllocation (
	   Object	=> dab_handler. dabProcessor,
	   Name		=> dab_handler. dabProcessor_P);
	task body msc_processor is
	   CUSize	: constant Integer := 4 * 16;
	   cifVector	: shortArray (0 .. 55296 - 1);
	   cifCount	: Integer	:= 0;
	   endMSC	: exception;
	   theData	: audioData;
	   dabModus	: dataMode;
	   currentblk	: Integer	:= 0;
	   work_to_be_done	: Boolean	:= False;
	   startAddr	: short_Integer;
	   Length	: short_Integer;
	   the_dabProcessor	: dab_handler. dabProcessor_P	:= null;
	begin
	   accept set_Mode (mode : dabMode) do
	      bitsperBlock	:= 2 * K (mode);
	      case mode is
	         when Mode_1	=> blocksperCIF := 18;
	         when Mode_2	=> blocksperCIF := 72;
	         when Mode_4	=> blocksperCIF := 36;
	         when others	=> blocksperCIF	:= 18; -- should not happen
	      end case;
	   end set_Mode;
	   loop
	      select
	         accept reset;
	         work_to_be_done	:= false;
	         currentblk		:= 0;
	         cifCount		:= 0;
	      or
	         accept stop;
	         if the_dabProcessor /= null
	         then
	            the_dabProcessor. stop;
	            while not the_dabProcessor' terminated loop
	               delay 0.01;
	            end loop;
	            put_line ("old dab handler now stopped");
	            Free_dabProcessor (the_dabProcessor);
	         end if;
	         raise endMSC;
	      or 
	         accept set_audioData (d: audioData) do
	            theData	:= d;
	         end set_audioData;
	         work_to_be_done	:= true;
	         startAddr		:= theData. startAddr;
	         Length			:= theData. Length;
	         if  theData. ASCTy = 8#077#
	         then
	            dabModus	:= DAB_PLUS;
	         else
	            dabModus	:= DAB;
	         end if;
	         if the_dabProcessor /= null
	         then
	            the_dabProcessor. stop;
	            while not the_dabProcessor' terminated loop
	               delay 0.01;
	            end loop;
	            put_line ("old dab handler now stopped");
	            Free_dabProcessor (the_dabProcessor);
	         end if;
	         the_dabProcessor	:= new dab_handler. dabProcessor
	                                            (dabModus,
	                                    Integer (theData. Length) * CUSize,
	                                             theData. bitRate,
	                                             theData. uepFlag,
	                                             theData. protLevel);

	      or
	         accept process_mscBlock (fbits	: shortArray;
	                                  blkno	: Integer) do
	            if work_to_be_done
	            then
	               currentblk:= (blkno - 5) mod blocksperCIF;
	               cifVector (currentblk * bitsperBlock .. 
	                            (currentblk  + 1) * bitsperBlock - 1) :=
	                                        fbits;
	            else
	               return;
	            end if;
	         end process_mscBlock;
	         if currentblk >= blocksperCIF - 1
	         then	-- we have a full CIF
	            cifCount	:= (cifCount + 1) mod 4;
	            the_dabProcessor.
	                   process (cifVector (Integer (startAddr) * CUSize ..
	                                 Integer (startAddr + Length) * CUSize - 1));
	         end if;
	      end select;
	   end loop;
	exception
	   when endMSC	=> put_line ("msc_processor normally terminated");
	   when Error: others		=> Put ("Exception in mscHandler: ");
	                                   Put_Line (Exception_Name (Error));
	   
	end msc_processor;
end msc_handler;


