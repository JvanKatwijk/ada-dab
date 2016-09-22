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
with System; use System;
with System. Address_To_Access_Conversions;
with Text_IO; use Text_IO;

package body audiopackage is
	use header;
--
--	the output samplerate will be 48000 in all cases, so
--	in those cases that the PCM rate differs, we need
--	to convert the rate. that is why we use the filters
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
	function paCallback (Input:       System. Address;
	                     Output:      System. Address;
	                     frameCount:  Interfaces. C. unsigned_long;
	                     timeInfo:    access PaStreamCallbackTimeInfo;
	                     statusFlags: PaStreamCallbackFlags;
	                     userData:    System. Address)
	                        return PaStreamCallbackResult is
	   subtype localBufferType is
	         pa_ringBuffer. buffer_data (0 .. integer (frameCount) - 1);
	   package environmentConverter is
	         new System. Address_To_Access_Conversions (audiosink);
	   package arrayConverter is
	         new System. Address_To_Access_Conversions (localBufferType);
-- 
--	my_environment is the record of type audiosink, passed on through the
--	Pa_OpenStream function
	   My_Environment: environmentConverter. Object_Pointer :=
	                        environmentConverter. To_Pointer (userData);
--
--	mybuffer is actually a C array, provided for by the underlying
--	portaudio library, here named as My_Buffer
	   My_Buffer: arrayConverter. Object_Pointer :=
	                        arrayConverter. To_Pointer (output);
	   amount:   Integer;
	begin
	   if My_Environment. Callback_Returnvalue /= paContinue then
	      return My_Environment. Callback_Returnvalue;
	   end if;

	   pa_ringBuffer.
	       getDataFromBuffer (My_Environment. buffer,
	                          My_Buffer. all, amount);
	   if amount <  Integer (frameCount) then
	      My_Buffer. all (amount .. My_Buffer. all' Last) :=
	                                      (Others =>  (0.0, 0.0));
	   end if;
	   return My_Environment. Callback_Returnvalue;
	end paCallBack;

	procedure Initialize (s: in out audiosink) is
	   error: PaError;
	begin
	   if s. Is_Initialized or else s. Has_Error then
	      return;	-- i.e. do not try it again
	   end if;
	   error	:= Pa_Initialize;
	   if error /= paNoError then
	      s. Has_Error	:= true;
	      return;
	   end if;

	   s. Numof_Devices	:= Pa_GetDeviceCount;
	   s. Is_Initialized	:= true;
	   s. Is_Running	:= false;
	end Initialize;

	procedure Finalize	(s: in out audiosink) is
	   error: PaError;
	begin
	   if s. Has_Error or else
	             not s. Is_Initialized then
	      return;
	   end if;

	   if s. Is_Running then-- stop the device first
	      s. Callback_Returnvalue := paAbort;
	      error := Pa_AbortStream (s. ostream. all);
	      while Pa_IsStreamStopped (s. ostream. all) /= 1 loop
	         Pa_Sleep (1);
	      end loop;
	   end if;

	   error := Pa_CloseStream (s. ostream. all);
	   error := Pa_Terminate;
	   s. Is_Initialized := false;
	end Finalize;

	procedure selectDefaultDevice	(s:    in out audiosink;
	                                 res:  out boolean) is
	   Device_index	: PaDeviceIndex := Pa_GetDefaultOutputDevice;
	begin
	   selectDevice (s, res, Device_Index);
	end selectDefaultDevice;

	procedure selectDevice (s:   in out audiosink;
	                        res: out boolean;
	                        Device_Index: PaDeviceIndex) is
	   error:            PaError;
	   Device_Buffersize: integer;
	   Selected_Latency:  aliased PaTime;
	begin
	   if s. Has_Error or else Device_Index = paNoDevice then
	      res	:= false;
	      return;
	   end if;

	   if s. Is_Running then 	-- stop first before selecting
	      s. Callback_Returnvalue	:= paAbort;
	      error		:= Pa_AbortStream (s. ostream. all);
	      while Pa_IsStreamStopped (s. ostream. all) = 0 loop
	         Pa_Sleep (1);
	      end loop;
	      s. Is_Running := false;
	   end if;

	   error	:= Pa_CloseStream (s. ostream. all);
	   s. Output_Parameters. device		:= Device_Index;
	   s. Output_Parameters. channelCount	:= 2;
	   s. Output_Parameters. sampleFormat	:= paFloat32;
	   if s. Latency = LOW_LATENCY then
	      Selected_Latency :=
	           Pa_GetDeviceInfo (Device_Index). defaultLowOutputLatency;
	   else     -- latency = HIGH_LATENCY
	      Selected_Latency :=
	           Pa_GetDeviceInfo (Device_Index). defaultHighOutputLatency;
	   end if;
	
	   Device_Buffersize :=
	        2 * integer (float (Selected_Latency) * float (s. cardRate));
	   put ("selected bufferSize ");
	   put_line (Integer' Image (Device_Buffersize));

	   error   := Pa_OpenStream (s. ostream,
	                             null,
	                             s. Output_Parameters' access,
	                             Long_Float (48000),
	                             Interfaces.C.unsigned_long (Device_BufferSize),
	                             0,
	                             paCallback' access,
	                             s' Address);
	   if error /= paNoError then
	      res	:= false;
	      return;
	   end if;
	
	   res	:= true;
	end selectDevice;

	procedure  portAudio_start (s: in out audiosink;
	                            result: out boolean) is
	   error	: PaError;
	begin
--	default:
	   result	:= false;

	   if s. Has_Error then
	      return;
	   end if;

	   if s. Is_Running then
	      result := true;
	      return;
	   end if;

--	It took a while, but is seems we can start
	   s. Callback_Returnvalue := paContinue;
	   error := Pa_StartStream (s. ostream. all);
	   if error = paNoError then
	      s. Is_Running	:= true;
	      result := true;
	   end if;
	end portAudio_start;

	procedure portAudio_stop (s: in out audioSink) is
	   error:    PaError;
	   Size:     integer;
	begin
	   if not s. Is_Initialized or else not s. Is_Running then
	      return;
	   end if;

	   s. Callback_Returnvalue	:= paAbort;
	   error		:= Pa_StopStream (s. ostream. all);
	   while Pa_IsStreamStopped (s. ostream. all) /= 1 loop
	      Pa_Sleep (1);	
	   end loop;

	   size	:= pa_ringbuffer. GetRingBufferReadAvailable (s. buffer);
	end portAudio_stop;

	procedure putSamples	(s:           in out audiosink;
	                         data:        complexArray; 
	                         sampleRate:  uint64_t) is
	   available	: integer :=
	                 pa_ringbuffer.
	                        GetRingBufferWriteAvailable ( s. buffer);
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
	procedure putSamples_16	(s:    in out audiosink;
	                         data: complexArray) is
	   buffer:   buffer_data (0 .. 3 * data' Length - 1);
	begin	
	   for I in data' Range loop
	      declare
	         index : Integer := Integer (I - data' first);
	         Temp_Value : dataComplex;
	      begin
	         Temp_Value          := f_16. Pass (data (index));
	         buffer (3 * index)	:=
	                       (Interfaces. C. C_float (Temp_Value. Re),
	                        Interfaces. C. C_float (Temp_Value. Im));
	         Temp_Value          := f_16. Pass ((0.0, 0.0));
	         buffer (3 * index + 1)	:=
	                       (Interfaces. C. C_float (Temp_Value. Re),
	                        Interfaces. C. C_float (Temp_Value. Im));
	         Temp_Value          := f_16. Pass ((0.0, 0.0));
	         buffer (3 * index + 2)	:= 
	                       (Interfaces. C. C_float (Temp_Value. Re),
	                        Interfaces. C. C_float (Temp_Value. Im));
	      end;
	   end loop;
	   pa_ringBuffer. putDataIntoBuffer (s. buffer, buffer);
	end putSamples_16;
--
--	mapping from 24000 -> 48000 is simple, just
--	add a zero sample to each input sample
	procedure putSamples_24	(s:       in out audiosink;
	                         data:    complexArray) is
	   buffer	: buffer_data (0 .. 2 * data' Length - 1);
	begin
	   for i in data' Range loop
	      declare
	         index : Integer := Integer (i - data' First);
	         Temp_Value : dataComplex;
	      begin
	         Temp_Value    := f_24. Pass (data (index));
	         buffer (2 * index)	:= 
	                      (Interfaces. C. C_float (Temp_Value. Re),
	                       Interfaces. C. C_float (Temp_Value. Im));
	         Temp_Value    := f_24. Pass ((0.0, 0.0));
	         buffer (2 * index + 1)	:=
	                      (Interfaces. C. C_float (Temp_Value. Re),
	                       Interfaces. C. C_float (Temp_Value. Im));
	      end;
	   end loop;
	   pa_ringBuffer. putDataIntoBuffer (s. buffer, buffer);
	end putSamples_24;
--
--
--	Converting the rate from 32000 -> 48000 is in 2 steps,
--	step 1 is upconverting to 96000,
--	step 2 is downconverting by a factor of 2
	procedure putSamples_32	(s:    in out audiosink;
	                         data: complexArray) is
	   buffer_1: complexArray (0 .. 3 * data' Length - 1);
	   buffer_2: buffer_data (0 .. buffer_1' Length / 2 - 1);
	begin
	   for i in data' Range loop
	      declare
	         index : Integer := Integer (i - data' first);
	      begin
	         buffer_1 (3 * index)        := f_32. Pass (data (i));
	         buffer_1 (3 * index + 1)    := f_32. Pass ((0.0, 0.0));
	         buffer_1 (3 * index + 2)    := f_32. Pass ((0.0, 0.0));
	      end;
	   end loop;

	   for i in buffer_2' Range loop	-- we know it is 0 .. X
	      buffer_2 (i)	:=
	                    (Interfaces. C. C_float (buffer_1 (2 * i). Re),
	                     Interfaces. C. C_float (buffer_1 (2 * i). Im));
	   end loop;
	   pa_ringBuffer. putDataIntoBuffer (s. buffer, buffer_2);
	end putSamples_32;
--
--	This one is easy, just pass on the data
	procedure putSamples_48	(s:       in out audiosink;
	                         data:    complexArray) is
	   buffer	: buffer_data (data' Range);
	begin
	   for I in data' Range loop
	      buffer (I) := 
	                  (Interfaces. C. C_float (data (I). Re),
	                   Interfaces. C. C_float (data (I). Im));
	   end loop;
	   pa_ringBuffer. putDataIntoBuffer (s. buffer, buffer);
	end putSamples_48;
--
end audiopackage;

