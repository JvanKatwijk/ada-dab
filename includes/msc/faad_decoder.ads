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
--
--	Thin binding to the faad library
--
with System;
with header; use header;
with Interfaces; use Interfaces;
with Interfaces.C;
with audiopackage;
package faad_decoder is
procedure mp42pcm (dacRate	: short_Integer;
	           sbrFlag	: short_Integer;
	           mpegSurround	: short_Integer;
	           aacChannelMode	: short_Integer;
	           buffer	: byteArray;
	           bufferLength	: uint16_t;
	           samples_out	: out Integer;
	           pcmHandler	: audiopackage. audioSink_P);
procedure	reset;
private
type channelPositions	is Array (0 .. 63) of uint8_t;
pragma Convention (C, channelPositions);

type NeAACDecFrameInfo is record
	bytesconsumed		: uint64_t;
	samples			: uint64_t;
	channels		: uint8_t;
	error			: uint8_t;
	samplerate		: uint64_t;
--	SBR: 0: off, 1: on; upsample, 2: on; downsampled, 3: off; upsampled
	sbr			: uint8_t;
--	MPEG-4 ObjectType */
	object_type		: uint8_t;
--	AAC header type; MP4 will be signalled as RAW also 
	header_type		: uint8_t;
--	multichannel configuration
	num_front_channels	: uint8_t;
	num_side_channels	: uint8_t;
	num_back_channels	: uint8_t;
	num_lfe_channels	: uint8_t;
	channel_position	: channelPositions;
--	PS: 0: off, 1: on 
	ps			: uint8_t;
end record;
pragma Convention (C, NeAACDecFrameInfo);

aacInitialized	:	Boolean	:= False;
aacHandle	:	System. Address;
baudRate	:	Integer;

function	NeAACDecOpen	return System. Address;
pragma	Import (C, NeAACDecOpen, "NeAACDecOpen");

procedure	NeAACDecClose (handle : System. Address);
pragma Import (C, NeAACDecClose, "NeAACDecClose");

function	NeAACDecInit2 (handle	: System. Address;
	                       asc	: System. Address;
	                       sz	: Integer;
	                       sr	: System. Address;
	                       ch	: System. Address) return long_Integer;
pragma Import (C, NeAACDecInit2, "NeAACDecInit2");

function	NeAACDecDecode (handle	: System. Address;
	                        hInfo_p	: System. Address;
	                        buffer_p: System. Address;
	                        Length	: Integer) return System. Address;
pragma Import (C, NeAACDecDecode, "NeAACDecDecode");

procedure	NeAACDecGetCapabilities;
pragma Import  (C, NeAACDecGetCapabilities, "NeAACDecGetCapabilities");

procedure	NeAACDecGetCurrentConfiguration (Handle: System. Address);
pragma	Import (C, NeAACDecGetCurrentConfiguration,
	           "NeAACDecGetCurrentConfiguration");

subtype myError is Interfaces. C. char_array (0 .. 12);
function	faacDecGetErrorMessage (error: uint8_t)
	                return myError;
pragma Import (C, faacDecGetErrorMessage, "NeAACDecGetErrorMessage");
hInfo		: NeAACDecFrameInfo;
end faad_decoder;

