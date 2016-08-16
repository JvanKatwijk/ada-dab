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

with header; use header;
with audio_handler;
with System; use System;
with audio_handler;
package mp2_handler is
   type mp2Processor  is new audio_handler. audioProcessor with private;
   type mp2Processor_P is access all mp2Processor;

   procedure addtoFrame (Object	: in out mp2Processor;
	                 V	: byteArray;
	                 nbits	: short_Integer);
private
   type kjmp2_context_t is
   record
	id	: Integer;
	V	: intArray (0 .. 2 * 1024 - 1);
	Voffs	: Integer;
   end record;
   pragma Convention (C, kjmp2_context_t);
   type mp2Processor  is new audio_handler. audioProcessor with 
	record
	   context		: kjmp2_context_t;
	   MP2framesize		: Integer;
	   MP2Header_OK		: Integer;
	   MP2HeaderCount	: Integer;
	   MP2bitCount		: Integer;
	   baudRate		: Integer;
	   MP2frame		: byteArray_P;
	end record;
procedure Initialize	(Object : in out mp2Processor);
procedure Finalize	(Object : in out mp2Processor);

--	kjmp2_init: This function must be called once to initialize each kjmp2
--	decoder instance.
procedure kjmp2_init	(mp2	: System. Address);
pragma Import (C, kjmp2_init, "kjmp2_init");

--	kjmp2_get_sample_rate: Returns the sample rate of a MP2 stream.
--	frame: Points to at least the first three bytes of a frame from the
--	stream.
--	return value: The sample rate of the stream in Hz,
--	or zero if the stream isn't valid.
function  kjmp2_get_sample_rate (frame	: System. Address) return Integer;
pragma Import (C, kjmp2_get_sample_rate, "kjmp2_get_sample_rate");

--	kjmp2_decode_frame: Decode one frame of audio.
--	mp2: A pointer to a context record that has been initialized with
--	kjmp2_init before.
--	frame: A pointer to the frame to decode. It *must* be a complete frame,
--	because no error checking is done!
--	pcm: A pointer to the output PCM data.
--	kjmp2_decode_frame() will always return 1152
--	(=KJMP2_SAMPLES_PER_FRAME) interleaved stereo samples
--      in a native-endian 16-bit signed format. Even for mono streams,
--      stereo output will be produced.
--	return value: The number of bytes in the current frame.
--	In a valid stream, frame + kjmp2_decode_frame(..., frame, ...)
--	will point to the next frame,
--	if frames are consecutive in memory.
--	Note: pcm may be NULL. In this case, kjmp2_decode_frame()
--	will return the size of the frame without actually decoding it.
function kjmp2_decode_frame (mp2	: System. Address;
	                     frame	: System. Address;
	                     pcm	: System. Address) return Integer;
pragma Import (C, kjmp2_decode_frame, "kjmp2_decode_frame");
end mp2_handler;
