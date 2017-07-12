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
with audiopackage;
with header;		use header;
with Interfaces;	use Interfaces;
with Interfaces.C;	use Interfaces. C;

package faad_decoder is
	procedure mp42pcm (dacRate       : uint8_t;
	                   sbrFlag       : uint8_t;
	                   mpegSurround  : uint8_t;
	                   aacChannelMode: uint8_t;
	                   buffer        : byteArray;
	                   bufferLength  : uint16_t;
	                   samples_out   : out Integer;
	                   pcmHandler    : audiopackage. audioSink_P);
	procedure	reset;
private
	type channelPositions	is Array (0 .. 63) of uint8_t;
	pragma Convention (C, channelPositions);

	type NeAACDecFrameInfo is
	   record
	      bytesconsumed     : Interfaces. C. long;
	      samples           : Interfaces. C. long;
	      channels          : Interfaces. C. unsigned_char;
	      error             : Interfaces. C. unsigned_char;
	      samplerate        : Interfaces. C. long;
--	SBR: 0: off, 1: on; upsample, 2: on; downsampled, 3: off; upsampled
	      sbr               : Interfaces. C. unsigned_char;
--	MPEG-4 ObjectType */
	      object_type       : Interfaces. C. unsigned_char;
--	AAC header type; MP4 will be signalled as RAW also 
	      header_type       : Interfaces. C. unsigned_char;
--	multichannel configuration
	      num_front_channels: Interfaces. C. unsigned_char;
	      num_side_channels : Interfaces. C. unsigned_char;
	      num_back_channels : Interfaces. C. unsigned_char;
	      num_lfe_channels  : Interfaces. C. unsigned_char;
	      channel_position  : channelPositions;
--	PS: 0: off, 1: on 
	      ps                : Interfaces. C. unsigned_char;
	   end record;
	pragma Convention (C, NeAACDecFrameInfo);

	aacInitialized   :  Boolean	:= False;
	aacHandle        :  System. Address;
	baudRate         :  Integer;

	function NeAACDecOpen	return System. Address;
	pragma	Import (C, NeAACDecOpen, "NeAACDecOpen");

	procedure NeAACDecClose (handle:  System. Address);
	pragma Import (C, NeAACDecClose, "NeAACDecClose");
--
	type C_vector is array (Integer range <>) of
	                        Interfaces. C. unsigned_char;
	subtype asc_vector is C_vector (0 .. 1);
	function NeAACDecInit2 (handle  : System. Address;
	                        asc     : asc_vector;
	                        sz      : Interfaces. C. int;
	                        sr      : out Interfaces. C. long; --System. Address;
	                        ch      : out Interfaces. C. unsigned_char)
	                                 return Interfaces. C. long;
	pragma Import (C, NeAACDecInit2, "NeAACDecInit2");

	function NeAACDecDecode (handle   : System. Address;
	                         hInfo_p  : System. Address;
	                         buffer_p : System. Address;
	                         Length   : Interfaces.C. long)
	                                      return System. Address;
	pragma Import (C, NeAACDecDecode, "NeAACDecDecode");

	procedure NeAACDecGetCapabilities;
	pragma Import  (C, NeAACDecGetCapabilities, "NeAACDecGetCapabilities");

	procedure NeAACDecGetCurrentConfiguration (Handle: System. Address);
	pragma	Import (C, NeAACDecGetCurrentConfiguration,
	                           "NeAACDecGetCurrentConfiguration");

	subtype myError is Interfaces. C. char_array (0 .. 12);
	function faacDecGetErrorMessage (error: uint8_t) return myError;
	pragma Import (C, faacDecGetErrorMessage, "NeAACDecGetErrorMessage");
	hInfo:   NeAACDecFrameInfo;
end faad_decoder;

