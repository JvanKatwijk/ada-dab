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
with header; use header;
with system;
with ringbuffer;
with portaudioada; use portaudioada;
with fir_filters; use fir_filters;
with Ada. Finalization; use Ada. Finalization;
--	a first attempt to interface - using the portaudioada package -
--	to create an interface to the soundcard (just for output, and
--	in this version just to defaultoutput
package audiopackage is
use complexTypes;
	package pa_ringBuffer is new ringbuffer (complex);
	use pa_ringBuffer;
	subtype audioData is pa_ringBuffer. buffer_data;
	type outputLatency is (LOW_LATENCY, HIGH_LATENCY);
	type audioSink (cardRate: integer;
	                bufSize	: integer;
	                latency : outputLatency)
	                     is new Ada. Finalization. Controlled with private;
	type audioSink_P is access all audioSink;
	procedure portAudio_start	(s: in out audioSink;
	                                 result: out boolean);
	procedure portAudio_stop	(s: in out audioSink);
	procedure putSamples		(s	: in out audioSink;
	                                 data	: buffer_data;
	                                 sampleRate : uint64_t);	
	procedure putSamples_16		(s	: in out audioSink;
	                                 data	: buffer_data);
	procedure putSamples_24		(s	: in out audioSink;
	                                 data	: buffer_data);
	procedure putSamples_32		(s	: in out audioSink;
	                                 data	: buffer_data);
	procedure putSamples_48		(s	: in out audioSink;
	                                 data	: buffer_data);
	procedure selectDefaultDevice	(s: in out audioSink; res: out boolean);
	procedure selectDevice	(s: in out audioSink;
	                         res: out boolean; deviceIndex: PaDeviceIndex);
private
	function paCallback (input	:	System. Address;
	                     output	:	System. Address;
	                     frameCount  :	IC.unsigned_long;
	                     timeInfo	:	access PaStreamCallbackTimeInfo;
	                     statusFlags :	PaStreamCallbackFlags;
	                     userData	:	System. Address)
	                            return PaStreamCallbackResult;
	pragma Convention (C, paCallback);
	type audioSink (cardRate: Integer;
	                bufSize : Integer;
	                latency	: outputLatency) is new Ada. Finalization. Controlled with
	record
	   is_initialized	: Boolean	:= False;
	   hasError		: Boolean	:= False;
	   is_running		: Boolean	:= False;
	   numofDevices		: Integer;
	   buffer		: pa_ringBuffer. ringbuffer_data (4 * 32768);
	   callbackReturn	: PaStreamCallbackResult := paContinue;
	   ostream		: access PaStream 	:= new PaStream;
	   outputParameters	: aliased PaStreamParameters;
	end record;
procedure Initialize	(s: in out audioSink);
procedure Finalize	(s: in out audioSink);
end audiopackage;

