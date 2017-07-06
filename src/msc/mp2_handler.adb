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
with Interfaces;	use Interfaces;
with Text_IO;		use Text_IO;
with audiopackage;	use audiopackage;

package body mp2_handler is

	KJMP2_SAMPLES_PER_FRAME : constant Integer	:= 1152;
	outBuffer:  shortArray (0 .. 2 * KJMP2_SAMPLES_PER_FRAME - 1);
	sample_Buf: complexArray (0 .. KJMP2_SAMPLES_PER_FRAME - 1);

	procedure Initialize (Object : in out mp2Processor) is
	begin
	   kjmp2_init (Object. context' Address);
	   Object. MP2framesize     := 24 * Integer (Object. bitrate);
	   Object. MP2Header_OK     := 0;
	   Object. MP2HeaderCount   := 0;
	   Object. MP2bitCount      := 0;
	   Object. baudRate         := 48000;	-- default
	   Object. MP2frame         := new byteArray (0 .. 24 * Integer (Object. bitRate));
	end Initialize;

	procedure Finalize (Object : in out mp2processor) is
	begin
	   Free_byteArray (Object. MP2frame);
	end Finalize;
--
--	just a local
	procedure Add_Bit_to_MP2   (Data  : in out byteArray;
	                            Bit   : uint8_t;
	                            Bitp  : Integer) is
	   byte:      uint8_t   := Data (bitp / 8);
	   bitnr:     Integer   := 7 - Integer (uint32_t (bitp) and 8#07#);
	   newbyte:   uint8_t   := Shift_Left (uint8_t (01), bitnr);
	begin
	   if Bit = 0 then
	      byte := byte and (not newbyte);
	   else
	      byte := byte or newbyte;
	   end if;
	   Data (bitp / 8)	:= byte;
	end Add_Bit_to_MP2;
--
--	Note: Data, input, is in bits, while the result is
--	in bytes
	procedure Add_to_Frame (Object  : in out mp2Processor;
	                        Data    : byteArray;
	                        Nbits   : int16_t) is
	   lf:     Integer := 
	           (if Object. baudRate = 48000 then Object. MP2framesize
	                                      else 2 * Object. MP2framesize);
	   count:  Integer     := 0;
	begin
	   for i in 0 .. Integer (nbits) - 1 loop
	      if Object. MP2Header_OK = 2 then
	         Add_bit_to_MP2 (Object. MP2frame. all,
	                         Data (i),
	                         Object. MP2bitCount);
	         Object. MP2bitCount   := Object. MP2bitCount + 1;
	         if Object. MP2bitCount >= lf then
	            count := kjmp2_decode_frame (Object. context' Address,
	                                         Object. MP2frame. all' Address,
	                                         outBuffer' Address);
	      
	            if count <= 0 then
	               return;	-- something wrong
	            end if;
	            for i in 0 .. KJMP2_SAMPLES_PER_FRAME - 1 loop
	               sample_Buf (i) :=
	                         (Float (outBuffer (2 * i    )) / 32768.0,
	                          Float (outBuffer (2 * i + 1)) / 32768.0);
	            end loop;
	            Object. pcmHandler. putSamples (sample_Buf,
	                                         uint64_t (Object. baudRate));
	            Object. MP2Header_OK     := 0;
	            Object. MP2HeaderCount   := 0;
	            Object. MP2bitCount      := 0;
	         end if;
	      elsif Object. MP2Header_OK = 0 then	-- no sync yet
	         if Data (i) = 01 then		-- all bits should be a "1"
	            Object. MP2HeaderCount  := Object. MP2HeaderCount + 1;
	            if Object. MP2HeaderCount = 12 then -- we have 12 '1' bits in a row
	               Object. MP2bitCount := 0;
	               for j in 0 .. 12 - 1 loop
	                  Add_Bit_to_MP2 (Object. MP2frame. all,
	                                           1, Object. MP2bitCount);
	                  Object. MP2bitCount   := Object. MP2bitCount + 1;
	               end loop;
	               Object. MP2Header_OK  := 1;	-- next state
	            end if;
	         else
	             Object. MP2HeaderCount   := 0;
	         end if;
	      elsif Object. MP2Header_OK = 1 then	
	         Add_Bit_to_MP2 (Object. MP2frame. all,
	                         Data (i),
	                         Object. MP2bitCount);
	         Object. MP2bitCount	:= Object. MP2bitCount + 1;
	         if Object. MP2bitCount = 24 then   -- relevant part header
	            Object. baudRate := 
	                 kjmp2_get_sample_rate (Object. MP2frame. all' Address);

	            if Object. baudRate /= 48000
	                        and then Object. baudRate /= 24000 then
	               Object. MP2Header_OK     := 0;
	               Object. MP2HeaderCount   := 0;
	               Object. MP2bitCount      := 0;
	               return;		-- failure
	            end if;
	            Object. MP2Header_OK  := 2;
	         end if;
	      end if;
	   end loop;
	end Add_to_Frame;
end mp2_handler;

