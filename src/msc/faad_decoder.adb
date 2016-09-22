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
with System. Address_To_Access_Conversions;
with audiopackage; 
with Ada. Exceptions; use  Ada. Exceptions;
with Text_IO;	      use Text_IO;

package body faad_decoder is
	amount	: Integer	:= 0;
	function get_aac_channel_configuration (m_mpeg_surround_config: short_Integer;
	                                        aacChannelMode		: short_Integer) 
	                                       return short_Integer is
	begin
	   case m_mpeg_surround_config is
	      when 0	=>	-- no surround
	         return (if aacChannelMode = 0 then 1 else 2);
	      when 1	=>	-- 5.1
	         return 6;
	      when 2	=>	-- 7.1
	         return 7;
	      when others	=>
	         return -1;
	   end case;
	end get_aac_channel_configuration;

	procedure reset is
	begin
	   aacInitialized	:= false;
	end reset;

	procedure mp42pcm (dacRate	: short_Integer;
	                   sbrFlag	: short_Integer;
	                   mpegSurround	: short_Integer;
	                   aacChannelMode	: short_Integer;
	                   buffer	: byteArray;
	                   bufferLength	: uint16_t;
	                   samples_out	: out Integer;
	                   pcmHandler	: audiopackage. audioSink_P) is
	   samples:       Integer;
	   channels:      Interfaces. C. unsigned_char;
	   sample_rate:   Interfaces. C. long;
	   outBuffer:     System. Address;
	begin
--	AudioSpecificConfig structure (the only way to
--	select 960 transform here!)
--	00010 = AudioObjectType 2 (AAC LC)
--	xxxx  = (core) sample rate index
--	xxxx  = (core) channel config
--	100   = GASpecificConfig with 960 transform
--
--	SBR: implicit signaling sufficient - libfaad2
--	automatically assumes SBR on sample rates <= 24 kHz
--	=> explicit signaling works, too, but is not necessary here
--
--	PS:  implicit signaling sufficient - libfaad2
--	therefore always uses stereo output
--	(if PS support was enabled) => explicit signaling not possible,
--	as libfaad2 does not support AudioObjectType 29 (PS)
--
	   if not aacInitialized then
	      declare
	         core_sr_index	: uint16_t;
	         core_ch_config	: short_Integer;
	         asc:             asc_vector;
	         pragma Convention (C, asc);
	         init_result		: Interfaces. C. long;
	      begin
-- 24/48/16/32 kHz
	         if dacRate /= 0 then
	            if sbrFlag /= 0 then
	               core_sr_index	:= 6;
	            else
	               core_sr_index	:= 3;
	            end if;
	         elsif sbrFlag /= 0 then
	            core_sr_index	:= 8;
	         else
	            core_sr_index	:= 5;
	         end if;

	         core_ch_config	:= get_aac_channel_configuration (mpegSurround,
	                                                          aacChannelMode);
	         if core_ch_config < 0 then
	            put_line ("unrecognized mpeg surround config");
	            samples_out	:= -1;
	            return;
	         end if;
	         asc (0)  := Interfaces. C. unsigned_char (
	                          (Shift_left (2#00010#, 3) or 
	                           Shift_Right (core_sr_index, 1)) and 16#FF#);
	         asc (1)  := Interfaces. C. unsigned_char (
	                          (Shift_Left (core_sr_index and 01, 7) or
	                     Shift_Left (uint16_t (core_ch_config), 3) or 2#100#) and 16#FF#);
--
--	Note that NeAACDecInit2 has two "out parameters", we just
--	pass their addresses
	         init_result	:= NeAACDecInit2 (aacHandle,
	                                          asc,
	                                          2,
	                                          sample_rate' Address,
	                                          channels' Address);
	         if init_result /= 0 then
	            put_line ("Error initializing the faad decoder library");
	            NeAACDecClose (aacHandle);
	            samples_out	:= -1;
	            return;
	         end if;

	         aacInitialized	:= true;
	      end;
	   end if;

---	When here we are properly initialized
	   outBuffer	:=  NeAACDecDecode (aacHandle,
	                                    hInfo' Address,
	                                    buffer' Address,
	                                    Interfaces. C. long (bufferLength));
	   sample_rate	:= hInfo. samplerate;
	   samples	:= Integer (hInfo. samples);
	   if sample_rate = 24000 or else
	                sample_rate = 48000 or else
	                    sample_rate /=  Interfaces. C. long (baudRate) then
	      baudRate	:=  Integer (sample_rate);
	   end if;
	   channels	:=  hInfo. channels;
	   if hInfo. error /= 0 then
	      put ("warning, faad error");
	      put_line (Integer' Image (Integer (hInfo. error)));
	      samples	:= 0;
	      return;
	   end if;
--
--	The aacDecoder returns a C array, however, we do no know
--	on beforehand the index constraints. We solve that by
--	creating a converter of the returned array address to
--	a decent Ada pointer to a C array
	   declare
	      type C_shortArray is array (integer range <>) of
	                       Interfaces. C. short;
	      subtype localOutput is C_shortArray (0 .. samples - 1);
	      package arrayConverter is
	            new System. Address_to_Access_Conversions (localOutput);
	      theBuffer	: arrayConverter. Object_Pointer :=
	                            arrayConverter. To_Pointer (outBuffer);
	   begin
	      if channels = 2 then
	         declare
--	Note: outBuf is an array of complexes, to be send to the 
--	soundcard handler
	            outBuf	: complexArray (0 .. samples / 2 - 1);
	         begin
	            for i in 0 .. samples / 2 - 1 loop
	               outBuf (i) :=
	                      (Float (theBuffer. all (2 * i)) / 32768.0,
	                       Float (theBuffer. all (2 * i + 1)) / 32768.0);
	            end loop;
	            pcmhandler. putSamples (outBuf, uint64_t (sample_rate));
	         end;
	      elsif channels = 1 then
	         declare
--	Note: outBuf is an array of complex
	            outBuf	: complexArray (0 .. samples - 1);
	         begin
	            for i in 0 .. samples - 1 loop
	               outBuf (i) := (Float (theBuffer. all (i)) / 32768.0,
	                           0.0);
	            end loop;
	            pcmHandler. putSamples (outBuf, uint64_t (sample_rate));
	         end;
	      else
	         null;	-- for ever
	      end if;
	   exception
	      when Error: others   => Put ("Exception in faad: ");
	                              Put_Line (Exception_Name (Error));
	   end;
	             
	   samples_out	:= samples / 2;
	end mp42pcm;

begin
	baudRate	:= 48000;		-- default
	NeAACDecGetCapabilities;
	aacHandle	:= NeAACDecOpen;
	NeAACDecGetCurrentConfiguration (aacHandle);
	aacInitialized	:= False;

end faad_decoder;
