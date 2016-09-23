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

with system;
with Ada. Finalization;
with Ada. Numerics. Generic_Complex_Types;
with ringbuffer;
with portaudioada;	use portaudioada;
with header;
with fir_filters;	use fir_filters;
with Interfaces. C;	use interfaces. C;
--	a first attempt to interface - using the portaudioada package -
--	to create an interface to the soundcard (just for output, and
--	in this version just to defaultoutput
package audiopackage is
	package portaudioData is
	      new Ada. Numerics. Generic_Complex_Types (Interfaces. C. C_float);
	package pa_ringBuffer is new ringbuffer (portaudioData. complex);
	subtype dataComplex is header. complexTypes. complex;
	use pa_ringBuffer;
	subtype audioData is pa_ringBuffer. buffer_data;
	type outputLatency is (LOW_LATENCY, HIGH_LATENCY);
	type audioSink (cardRate: integer;
	                bufSize	: integer;
	                latency : outputLatency)
	                     is new Ada. Finalization. Controlled with private;
	type audioSink_P is access all audioSink;
	procedure portAudio_start	(Object:  in out audioSink;
	                                 result:  out boolean);
	procedure portAudio_stop	(Object:  in out audioSink);
	procedure putSamples		(Object:  in out audioSink;
	                                 data:    header. complexArray;
	                                 sampleRate: header. uint64_t);	
	procedure putSamples_16		(Object:  in out audioSink;
	                                 data:    header. complexArray);
	procedure putSamples_24		(Object:  in out audioSink;
	                                 data:    header. complexArray);
	procedure putSamples_32		(Object:  in out audioSink;
	                                 data:    header. complexArray);
	procedure putSamples_48		(Object:  in out audioSink;
	                                 data:    header. complexArray);
	procedure selectDefaultDevice	(Object:  in out audioSink;
	                                 res:     out boolean);
	procedure selectDevice          (Object:  in out audioSink;
	                                 res:     out boolean;
	                                 Device_Index: PaDeviceIndex);
private
	function paCallback		(input:      System. Address;
	                                 output:     System. Address;
	                                 Frame_Count: IC.unsigned_long;
	                                 TimeInfo:   access PaStreamCallbackTimeInfo;
	                                 statusFlags: PaStreamCallbackFlags;
	                                 userData:   System. Address)
	                                      return PaStreamCallbackResult;
	pragma Convention (C, paCallback);
	type audioSink (cardRate: Integer;
	                bufSize : Integer;
	                latency	: outputLatency) is
	   new Ada. Finalization. Controlled with
	   record
	      Is_Initialized:        Boolean       := False;
	      Has_Error:             Boolean       := False;
	      Is_Running:            Boolean       := False;
	      Numof_Devices:         Integer;
	      Buffer:                pa_ringBuffer. ringbuffer_data (4 * 32768);
	      Callback_Returnvalue:  PaStreamCallbackResult := paContinue;
	      Ostream:               access PaStream        := new PaStream;
	      Output_Parameters:     aliased PaStreamParameters;
	   end record;
	procedure Initialize	(Object: in out audioSink);
	procedure Finalize	(Object: in out audioSink);
end audiopackage;

