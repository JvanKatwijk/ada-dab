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
with reed_solomon; 
with faad_decoder;
with firecode_checker;	use firecode_checker;
with Text_IO;		use Text_IO;
with Interfaces. C;	use Interfaces. C;
with Ada. Exceptions;	use Ada. Exceptions;
with simple_messages;	use simple_messages;
with pad_handler;	use pad_handler;

package body mp4_handler is
	package the_rsDecoder is new reed_solomon (8, 8#0435#, 0, 1, 10);
	use the_rsDecoder;
	type uintArray	is Array (uint16_t Range 0 .. 20) of uint16_t;

	procedure Initialize (Object: in out mp4Processor) is
	   bitRate	: short_Integer renames Object. bitRate;
	   RSDims	: Integer renames Object. RSDims;
	begin
	   Object. Superframe_size   := Integer (110 * (bitRate / 8));
	   Object. RSDims            := Integer (bitRate / 8);
	   Object. RSin_Data         := new byteArray (0 .. RSDims * 120 - 1);
	   Object. RSout_Data        := new byteArray (0 .. RSDims * 110 - 1);
	   Object. Block_FillIndex   := 0;
	   Object. Blocks_InBuffer   := 0;
	   Object. Framecount        := 0;
	   Object. Frameerrors       := 0;
--
	   Object. au_count          := 0;
	   Object. au_errors         := 0;
	   faad_decoder. reset;		
	end Initialize;

	procedure Finalize (Object: in out mp4Processor) is
	begin
	   Free_byteArray (Object. RSin_Data);
	   Free_byteArray (Object. RSout_Data);
	end Finalize;
--
--	The outsize world calls the "Add_to_Frame" function,
--	that function does what the name suggests
	procedure Add_to_Frame (Object   : in out mp4Processor;
	                        Data     : byteArray;
	                        Nbits    : short_Integer) is
	   temp    :  Byte       := 0;
           nbytes  :  Integer    := Integer (nbits / 8);
	   fcVector:  firecode_Checker. checkVector;
	   result  :  Boolean;
	   Block_FillIndex:   Integer renames Object. Block_FillIndex;
	begin
--
--	pack the bits, resulting from the deconvolution
--	into bytes
	   for i in 0 .. nbytes - 1 loop
	      temp    := 0;
	      for j in 0 .. 8 - 1 loop
	         temp := Shift_Left (temp, 1) or (Data (8 * i + j) and 8#01#);
	      end loop;
	      Object. RSin_Data (Block_FillIndex * nbytes + i) := temp;
	   end loop;
--
	   Object. Blocks_InBuffer := Object. Blocks_InBuffer + 1;
	   Object. Block_FillIndex := (Object. Block_FillIndex + 1) mod 5;
--
--	we take the last five blocks to look at
	   if Object. Blocks_InBuffer < 5 then
	      return;
	   end if;

--	we could show the successrate here
	   Object. Framecount := Object. Framecount + 1;
	   if  Object. Framecount >= 25 then
	      Object. Framecount	:= 0;
	      simple_messages. message_queue.
	                       Put ((MSC_RESULTS, 4 * (25 - Object. Frameerrors)));
--	   show 4 * (25 - frameErrors)
	      Object. Frameerrors	:= 0;
	   end if;

--	Now for real: first check the firecode
--	copy the first bytes into a vector fcVector
	   
	   fcVector := Object. RSin_Data (Block_FillIndex * nbytes ..
	                 Block_FillIndex * nbytes + fcVector' Length - 1);
	   if not firecode_Checker. check (fcVector) then
--	we were wrong, a virtual shift to left in block sizes
	      Object. Blocks_InBuffer  := 4;
	      Object. FrameErrors      := Object. FrameErrors + 1;
	      return;
	   end if;	

--	firecode seems OK, so we can (try to) process
--	the superframe
	   processSuperFrame (Object,
	                      Object. RSin_Data. all,
	                      short_Integer (Block_FillIndex * nbytes),
	                      result);

	   if not result then	-- we will try it again, next time
	      Object. Blocks_InBuffer   := 4;
	      Object. Frameerrors       := Object. Frameerrors + 1;
	      return;
	   end if;

--	we are done, set up for the next superframe
	   Object. Blocks_InBuffer	:= 0;
	end Add_to_Frame;
--
--	pretty tough function, we have a superframe and need to
--	extract the audio frames
	procedure processSuperframe (Object     : in out mp4Processor;
	                             frameBytes : byteArray;
	                             base       : short_Integer;
	                             result     : out Boolean) is
	   rsIn           : byteArray (0 .. 120 - 1);
	   rsOut          : byteArray (0 .. 110 - 1);
	   au_start       : uintArray;
	   num_aus        : uint16_t;
	   dacRate        : uint8_t;
	   sbrFlag        : uint8_t;
	   aacChannelMode : uint8_t;
	   psFlag         : uint8_t;
	   mpegSurround   : uint16_t;
	   RSout_Data     : byteArray renames Object. RSout_Data. all;
	   bitRate        : short_Integer renames Object. bitRate;
	   RSDims         : Integer renames Object. RSDims;
	   Samples_Out    : Integer;
	   Errors_in_RS   : short_Integer;
	begin
	   result	:= False;	-- always a good start
--	apply reed-solomon error repair
--	OK, what we now have is a vector with RSDims * 120 uint8_t's
--	in the superframe, containing parity bytes for error repair
--	take into account the interleaving that is applied.

	   for J in 0 .. RSDims - 1 loop
	      for K in rsIn' Range loop
	         rsIn (K) := frameBytes ((Integer (base) + J +
	                                 K * RSDims) mod (RSDims * 120));
	      end loop;

	      the_rsDecoder. decode_rs (rsIn, 135, rsOut, Errors_in_RS);
--	Errors_in_RS >= 0 means no errors or repaired errors
--	Errors_in_RS < 0 means errors beyond repair

	      if Errors_in_RS > 0 then
	         Object. nErrors	:= Object. nErrors + Errors_in_RS;
	      elsif Errors_in_RS < 0 then
	         result	:= false;
	         return;
	      end if;

	      for K in 0 .. 110 - 1 loop
	         Object. RSout_Data (J + K * RSDims) := rsOut (K);
	      end loop;
	   end loop;
--
--      OK, the result is RSDims * 110 * 8 bits
--      bits 0 .. 15 is firecode
--      bit 16 is unused

	   dacRate    := Shift_Right (RSout_Data (2), 6) and 8#01#; -- bit 17
	   sbrFlag    := Shift_Right (RSout_Data (2), 5) and 8#01#; -- bit 18
	   aacChannelMode := Shift_Right (RSout_Data (2), 4) and 8#01#; -- bit 19
           psFlag       := Shift_Right (RSout_Data (2), 3) and 8#01#;  -- bit 20
           mpegSurround	:= uint16_t (RSout_Data (2) and 8#07#); -- bits 21 .. 23

	   case 2 * dacRate + sbrFlag is
	      when 0	=> 
                 num_aus	:= 4;
                 au_start (0)	:= 8;
                 au_start (1) 	:= uint16_t (RSout_Data (3)) * 16 + 
	                           uint16_t (Shift_Right (RSout_Data (4), 4));
                 au_start (2)	:= uint16_t (
	                             (RSout_Data (4)) and 16#0f#) * 256 +
                                      uint16_t (RSout_Data (5));
                 au_start (3)	:= uint16_t (RSout_Data (6)) * 16 +
                                   uint16_t (Shift_Right (RSout_Data (7),  4));
                 au_start (4)	:= uint16_t (110 *  (bitRate / 8));

	      when 1	=>
	         num_aus	:= 2;
                 au_start (0)	:= 5;
                 au_start (1)	:= uint16_t (RSout_Data (3)) * 16 +
                                   uint16_t (Shift_Right (RSout_Data (4), 4));
                 au_start (2) 	:= uint16_t (110 * (bitRate / 8));

	      when 2	=>
	         num_aus	:= 6;
	         au_start (0) 	:= 11;
	         au_start (1)	:= uint16_t (RSout_Data (3)) * 16 +
                                   uint16_t (Shift_Right (RSout_Data (4), 4));
	         au_start (2) 	:= uint16_t ((RSout_Data (4)) and 16#0f#) * 256 +
	                           uint16_t (RSout_Data (5));
	         au_start (3) 	:= uint16_t (RSout_Data (6)) * 16 +
                                   uint16_t (Shift_Right (RSout_Data (7), 4));
	         au_start (4)	:= uint16_t (
	                               (RSout_Data (7)) and 16#0f#) * 256 +
	                                uint16_t (RSout_Data (8));
	         au_start (5)	:= uint16_t (RSout_Data (9)) * 16 +
                                   uint16_t (Shift_Right (RSout_Data (10), 4));
	         au_start (6) 	:= uint16_t (110 *  (bitRate / 8));
--
	      when 3	=>
	         num_aus 	:= 3;
	         au_start (0)	:= 6;
	         au_start (1)	:= uint16_t (RSout_Data (3)) * 16 +
                                   uint16_t (Shift_Right (RSout_Data (4), 4));
	         au_start (2) 	:= (uint16_t (RSout_Data (4)) and 16#0f#) * 256 +
	                           uint16_t (RSout_Data (5));
	         au_start (3) 	:= uint16_t (110 * (bitRate / 8));
	
	      when others =>	-- cannot happen
	         null;
	   end case;
--
--	extract the AU's and prepare a buffer, with sufficient
--	additional length for look ahead of the conversion program

	   for i in 0 ..  num_aus - 1 loop
	      declare
	         aac_frame_length  : uint16_t;
	         theAU             : ByteArray (0 .. 1920) := (others => 0);
	      begin
	         Object. au_count   := Object. au_count + 1;
--
--	first a sanity check
	         if au_start (i + 1) < au_start (i) then
	            for j in 0 .. num_aus - 1 loop
	               put (uint16_t' Image (au_start (i)));
	            end loop;
	            put_line (" should not happen");
	            result		:= false;
	            return;
	         end if;

	         aac_frame_length  := au_start (i + 1) - au_start (i) - 2;
	         if aac_frame_length > 2 * 960 then
	            put_line ("cannot happen");
	            result	:= false;
	            return;
	         end if;
--
--	Now first the crc check. If the data passes the test,
--	we copy the appropriate part of the data into the
--	AU vector for further processing
	         if dabPlus_crc (RSout_Data, au_start (i),
	                                     aac_frame_length) then
	            theAU (0 .. Integer (aac_frame_length - 1)) :=
	                          RSout_Data (Integer (au_start (i)) ..
	                          Integer (au_start (i)) +
	                               Integer (aac_frame_length) - 1);

	             if (Shift_Right (theAU (0), 5) and 07) = 4 then
	                pad_handler. processPAD (theAU);
	             end if;

--	we add a few zero bytes to allow look ahead of the decoder
	            for J in aac_frame_length .. aac_frame_length + 10 loop
	               theAU (Integer (J)) := 0;
	            end loop;

	            faad_decoder. mp42pcm (short_Integer (dacRate),
	                                   short_Integer (sbrFlag),
	                                   short_Integer (mpegSurround),
	                                   short_Integer (aacChannelMode),
	                                   theAU,
	                                   aac_frame_length,
	                                   Samples_Out,
	                                   Object. pcmHandler);
	         else
	            Object. au_errors	:= Object. au_errors + 1;
	         end if;
	      end;
	   end loop;

	   result  := true;
	exception
	   when Error: others	=> Put ("Exception in mp4 processor: ");
	                           Put (Exception_Name (Error));
	                           Put_Line ("frame ignored");
	end processSuperframe;

	procedure reset (Object : in out mp4Processor) is
	begin
	   faad_decoder. reset;
	end reset;

	function dabPlus_crc (Data_Vector : byteArray;
	                      Start_Byte  : uint16_t;
	                      Length      : uint16_t) return Boolean is
	   Accumulator:  uint16_t	:= 16#FFFF#;
	   Genpoly:      uint16_t	:= 16#1021#;
	   crc:          uint16_t;
	   Data:         uint16_t;
	   V1:           uint16_t;
	   V2:           uint16_t;
	begin

           for I in 0 .. Length - 1 loop
	      Data   := Shift_Left (
	                 uint16_t (Data_Vector (Integer (Start_Byte + i))), 8);
              for J in Reverse 1 .. 8 loop
	         if ((data xor accumulator) and 16#8000#) /= 0 then
                    Accumulator	:=
	                    (Shift_Left (Accumulator, 1) xor Genpoly) and
	                                                   16#FFFF#;
                 else
                    Accumulator	:= Shift_Left (Accumulator, 1) and 16#FFFF#;
	         end if;

                 Data	:= Shift_Left (Data, 1) and 16#FFFF#;
	      end loop;
	   end loop;
--
--	ok, now check with the crc that is contained
--	in the au
	   V1   := uint16_t (Data_Vector (Integer (Start_Byte + Length)));
	   V2   := uint16_t (Data_Vector (Integer (Start_Byte + Length + 1)));
	   crc  := not ((Shift_Left (V1, 8) or V2) and 16#FFFF#);
           return (crc xor Accumulator) = 0;
	end dabPlus_crc;
end  mp4_handler;
