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
with audiopackage; 
with System; use System;
with System. Address_To_Access_Conversions;
with Interfaces. C;
with Text_IO; use Text_IO;
package body audiopackage is
package IC renames Interfaces.C;
f_16	: fir_filters. lowPass_filter (5, 16000, 48000);
f_24	: fir_filters. lowPass_filter (5, 24000, 48000);
f_32	: fir_filters. lowPass_filter (5, 32000, 96000);
--	In the callback we have to transform the
--	System. Address values to pointers to the right type
--	So, we dig deep and end up with address_to_access_conversions
--	It all seems a little messy to me, but it seems to work
--
--	Note that we enter with complexes, where one complex number
--	happily matches one "frame"
function paCallback (input 	:	System. Address;
	             output	:	System. Address;
	             frameCount	:	IC.unsigned_long;
	             timeInfo	:	access PaStreamCallbackTimeInfo;
	             statusFlags :	PaStreamCallbackFlags;
	             userData 	:	System. Address)
	                        return PaStreamCallbackResult is
subtype localBufferType is
	         pa_ringBuffer. buffer_data (0 .. integer (frameCount) - 1);
package environmentConverter is
	         new System. Address_To_Access_Conversions (audiosink);
package arrayConverter is
	         new System. Address_To_Access_Conversions (localBufferType);
-- 
-- my_environment is the record of type audiosink, passed on through the
-- Pa_OpenStream function
my_environment : environmentConverter. Object_Pointer :=
	                        environmentConverter. To_Pointer (userData);
--
--	mybuffer is actually a C array, provided for by the underlying
--	portaudio library
mybuffer	: arrayConverter. Object_Pointer :=
	                        arrayConverter. To_Pointer (output);
amount		: Integer;
begin
	if my_environment. callbackReturn /= paContinue
	then
	   return my_environment. callbackReturn;
	end if;

	pa_ringBuffer.
	       getDataFromBuffer (my_environment. buffer, mybuffer. all, amount);
	if amount <  Integer (frameCount)
	then
	   mybuffer. all (amount .. mybuffer. all' Last) :=
	                                      (Others =>  (0.0, 0.0));
	end if;
	return my_environment. callbackReturn;
end paCallBack;

procedure Initialize (s: in out audiosink) is
error: PaError;
begin
	if s. is_initialized or else s. hasError
	then
	   return;	-- i.e. do not try it again
	end if;
	error	:= Pa_Initialize;
	if error /= paNoError
	then
	   s. hasError	:= true;
	   return;
	end if;

	s. numofDevices		:= Pa_GetDeviceCount;
	s. is_initialized	:= true;
	s. is_running		:= false;
end Initialize;

procedure Finalize	(s: in out audiosink) is
error: PaError;
begin
	if s. hasError or else
	             not s. is_initialized 
	then
	   return;
	end if;

	if s. is_running	-- stop the device first
	then
	   s. callbackReturn := paAbort;
	   error := Pa_AbortStream (s. ostream. all);
	   while Pa_IsStreamStopped (s. ostream. all) /= 1 loop
	      Pa_Sleep (1);
	   end loop;
	end if;

	error := Pa_CloseStream (s. ostream. all);
	error := Pa_Terminate;
	s. is_initialized := false;
end Finalize;

procedure selectDefaultDevice	(s: in out audiosink; res : out boolean) is
deviceIndex	: PaDeviceIndex := Pa_GetDefaultOutputDevice;
begin
	selectDevice (s, res, deviceIndex);
end selectDefaultDevice;

procedure selectDevice (s: in out audiosink;
	                res: out boolean; deviceIndex: PaDeviceIndex) is
error			: PaError;
devicebufferSize	: integer;
selectedLatency		: aliased PaTime;
begin
	if s. hasError or else deviceIndex = paNoDevice
	then
	   res	:= false;
	   return;
	end if;

	if s. is_running 	-- stop first before selecting
	then
	   s. callbackReturn	:= paAbort;
	   error		:= Pa_AbortStream (s. ostream. all);
	   while Pa_IsStreamStopped (s. ostream. all) = 0 loop
	      Pa_Sleep (1);
	   end loop;
	   s. is_running := false;
	end if;

	error		:= Pa_CloseStream (s. ostream. all);
	s. outputParameters. device		:= deviceIndex;
	s. outputParameters. channelCount	:= 2;
	s. outputParameters. sampleFormat	:= paFloat32;
	if s. latency = LOW_LATENCY
	then
	   selectedLatency :=
	           Pa_GetDeviceInfo (deviceIndex). defaultLowOutputLatency;
	else	-- latency = HIGH_LATENCY
	   selectedLatency :=
	           Pa_GetDeviceInfo (deviceIndex). defaultHighOutputLatency;
	end if;
	
	deviceBufferSize :=
	 2 * integer (float (selectedLatency) * float (s. cardRate));
	put ("selected bufferSize ");
	put (deviceBufferSize' image); New_Line (1);

	error	:= Pa_OpenStream (s. ostream,
	                          null,
	                          s. outputParameters' access,
	                          Long_Float (48000),
	                          Interfaces.C.unsigned_long (deviceBufferSize),
	                          0,
	                          paCallback' access,
	                          s' Address);
	if error /= paNoError
	then
	   res	:= false;
	   return;
	end if;
	
	res	:= true;
end selectDevice;

procedure  portAudio_start	(s: in out audiosink; result: out boolean) is
error	: PaError;
begin
--	default:
	result	:= false;

	if s. hasError
	then
	   return;
	end if;

	if s. is_running
	then
	   result := true;
	   return;
	end if;

--	It took a while, but is seems we can start
	s. callbackReturn := paContinue;
	error := Pa_StartStream (s. ostream. all);
	if error = paNoError
	then
	   s. is_running	:= true;
	   result := true;
	end if;
end portAudio_start;

procedure portAudio_stop	(s: in out audioSink) is
error	: PaError;
size	: integer;
begin
	if not s. is_initialized or else not s. is_running
	then
	   return;
	end if;

	s. callbackReturn	:= paAbort;
	error			:= Pa_StopStream (s. ostream. all);
	while Pa_IsStreamStopped (s. ostream. all) /= 1 loop
	   Pa_Sleep (1);	
	end loop;

	size	:= pa_ringbuffer. GetRingBufferReadAvailable (s. buffer);
end portAudio_stop;

procedure putSamples	(s		: in out audiosink;
	                 data		: buffer_data; 
	                 sampleRate	: uint64_t) is
available	: integer := pa_ringbuffer. GetRingBufferWriteAvailable (
	                             s. buffer);
begin
	case sampleRate is
	   when 16000	=> putSamples_16 (s, data);
	   when 24000	=> putSamples_24 (s, data);
	   when 32000	=> putSamples_32 (s, data);
	   when 48000	=> putSamples_48 (s, data);
	   when Others	=> null;		--just ignore the stuff
	end case;
end putSamples;
--
--	scaling from 16000 -> 48000 is easy, just add
--	zero samples and filter
procedure putSamples_16	(s		: in out audiosink;
	                 data		: buffer_data) is
buffer	: buffer_data (0 .. 3 * data' Length - 1);
begin	
	for i in data' Range loop
	   declare
	      index : Integer := Integer (i - data' first);
	   begin
	      buffer (3 * index)	:= f_16. Pass (data (index));
	      buffer (3 * index + 1)	:= f_16. Pass ((0.0, 0.0));
	      buffer (3 * index + 2)	:= f_16. Pass ((0.0, 0.0));
	   end;
	end loop;
	pa_ringBuffer. putDataIntoBuffer (s. buffer, buffer);
end putSamples_16;
--
--	mapping from 24000 -> 48000 is simple, just
--	add a zero sample to each input sample
procedure putSamples_24	(s		: in out audiosink;
	                 data		: buffer_data) is
buffer	: buffer_data (0 .. 2 * data' Length - 1);
begin
	for i in data' Range loop
	   declare
	      index : Integer := Integer (i - data' First);
	   begin
	      buffer (2 * index)	:= f_24. Pass (data (i));
	      buffer (2 * index + 1)	:= f_24. Pass ((0.0, 0.0));
	   end;
	end loop;
	pa_ringBuffer. putDataIntoBuffer (s. buffer, buffer);
end putSamples_24;
--
--
--	Converting the rate from 32000 -> 48000 is in 2 steps,
--	step 1 is upconverting to 96000,
--	step 2 is downconverting by a factor of 2
procedure putSamples_32	(s		: in out audiosink;
	                 data		: buffer_data) is
buffer_1	: buffer_data (0 .. 3 * data' Length - 1);
buffer_2	: buffer_data (0 .. buffer_1' Length / 2 - 1);
begin
	for i in data' Range loop
	   declare
	      index : Integer := Integer (i - data' first);
	   begin
	      buffer_1 (3 * index)	:= f_32. Pass (data (i));
	      buffer_1 (3 * index + 1)	:= f_32. Pass ((0.0, 0.0));
	      buffer_1 (3 * index + 2)	:= f_32. Pass ((0.0, 0.0));
	   end;
	end loop;

	for i in buffer_2' Range loop	-- we know it is 0 .. X
	   buffer_2 (i)		:= buffer_1 (2 * i);
	end loop;
	pa_ringBuffer. putDataIntoBuffer (s. buffer, buffer_2);
end putSamples_32;
--
--	This one is easy, just pass on the data
procedure putSamples_48	(s		: in out audiosink;
	                 data		: buffer_data) is
begin
	pa_ringBuffer. putDataIntoBuffer (s. buffer, data);
end putSamples_48;
--
end audiopackage;

