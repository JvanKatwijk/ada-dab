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
with System. Address_to_Access_Conversions;
with Ada. Unchecked_Deallocation;
with glib; use glib;
with Text_IO; use Text_IO;
package body rtlsdr_wrapper is
	use Interfaces. C;
	subtype localBufferType is
	         rtlsdr_buffer. buffer_data (0 .. READLEN_DEFAULT - 1);
	package bufferConverter is
	         new System. Address_To_Access_Conversions (localBufferType);
	package arrayConverter is
	         new System. Address_To_Access_Conversions (localBufferType);

--	set up
--
--	This callback will be called 2048000 / 8192 (i.e. app 250)
--	times a second and is kept simple: just put the data into
--	the buffer
	package environmentConverter is
              new System. Address_to_Access_Conversions (rtlsdr_device);

	procedure rtlsdr_Callback (buffer   : System. Address;
	                           size	    : Interfaces. C. int;
	                           userData : System. Address) is
--      mybuffer is actually a C array, provided for by the underlying
--      osmocom library
	   my_buffer   : arrayConverter. Object_Pointer :=
                                arrayConverter. To_Pointer (buffer);
	   localEnv    :  environmentConverter. Object_Pointer :=
                              environmentConverter. To_Pointer (userData);
	begin
	   if Integer (size) /= READLEN_DEFAULT then
	      return;
	   end if;
--
--	Put data in the ringbuffer and go on
	   localEnv. The_Buffer. putDataIntoBuffer (my_buffer. all);
	end rtlsdr_Callback;

	task body rtlsdr_reader is
	   the_Context : environmentConverter. Object_Pointer;
	   raw_context : System. Address;
	begin
	   accept start (context : System. Address) do
	      the_Context :=  environmentConverter. To_Pointer (context);
	      raw_context := context;
	   end;
	   rtlsdr_read_async_ada (the_Context. device. all,		-- 
	                          rtlsdr_Callback' access,
	                          raw_context,
	                          0,
	                          Interfaces. C. int (READLEN_DEFAULT));
	end rtlsdr_reader;

	procedure Set_VFOFrequency (Object       : in out rtlsdr_device;
	                            New_Frequency: Natural) is
	begin
	   Object. Last_Frequency   := New_Frequency;
	   rtlsdr_set_center_freq_ada (Object. device. all,
	                               Interfaces. C. int (New_Frequency));
	   put ("Frequency set to ");
	   put_line (Integer' Image (New_Frequency));
	end Set_VFOFrequency;

	function Get_VFOFrequency (Object :  rtlsdr_device) return Integer is
	begin
	    return  Integer (rtlsdr_get_center_freq_ada (Object. device. all));
	end Get_VFOFrequency;

	procedure Restart_Reader (Object   : in out rtlsdr_device;
	                          Success  : out Boolean) is
	   res : Interfaces. C. int;
	begin
	   if Object. workerHandle /= null then     -- running already
	      Success    := true;
	      return;
	   end if;

	   Object. The_Buffer. FlushRingBuffer;
	   res   := rtlsdr_reset_buffer_ada (Object. device. all);
	   if res < 0 then
	      Success	:= false;
	      return;
	   end if;

	   rtlsdr_set_center_freq_ada (Object. device. all,
	                               Interfaces. C. int (Object. Last_Frequency));
	   Object. workerHandle := new rtlsdr_reader;
	   Object. workerHandle. start (Object' Address);
	   Success := true;
	end Restart_Reader;

	procedure Stop_Reader (Object : in out rtlsdr_device) is
	begin
	   if Object. workerHandle = null then
	      return;
	   end if;

	   rtlsdr_cancel_async_ada (Object. device. all);
	   delay 1.0;
	   if Object. workerHandle. all 'Terminated then
	      put ("already terminated ");
	   end if;
	   while not Object. workerHandle. all' Terminated loop
	      put ("waiting for termination"); New_Line (1);
	      delay 0.5;
	   end loop;

	   Free_Handler (Object. workerHandle);
	   Object. workerHandle := null;
	end Stop_Reader;

	procedure Set_Gain (Object     : in out rtlsdr_device;
	                    New_Gain   : Natural) is
	  index : Natural := Natural (Integer (New_Gain) *
	                       Integer (Object. gainsCount) / 100);
	begin
	   rtlsdr_set_tuner_gain_ada (Object. device. all,
	                              Object. gains (index));
	   Object. theGain   := New_Gain;
	end Set_Gain;

--	Note that we get uint8_t's in and send complex(float)'s out
	procedure Get_Samples (Object  : in out rtlsdr_device;
	                       Out_V   : out complexArray;
	                       amount  : out Natural) is
	   tempBuffer: rtlsdr_buffer. buffer_data (0 .. 2 * Out_V' length - 1);
	   res   : Positive;
	begin
	   Object. The_Buffer. getDataFromBuffer (tempBuffer, res);
	   res := res / 2;
--
--	Now we are talking pairs
	   for I in 0 .. res - 1 loop
	      Out_V (Out_V' First + i) :=
	               (float (Integer (tempBuffer (2 * I)) - 128) / 128.0,
	                float (Integer (tempBuffer (2 * I + 1)) - 128) / 128.0);
	   end loop;
	   amount := res;
	exception
	   when others => put_line ("Hier gaat het mis");
	end Get_Samples;

	function Available_Samples (Object : rtlsdr_device) return Natural is
	begin
	   return  Object. The_Buffer. GetRingBufferReadAvailable / 2;
	end Available_Samples;

	function Valid_Device (Object : rtlsdr_device) return Boolean is
	begin
	   return  Object. valid;
	end valid_Device;

	procedure Initialize (Object : in out rtlsdr_device) is
	begin
	   Object. device		:= new rtlsdr_dev_t; 
	   Object. inputRate		:= 2048000;
	   Object. Last_Frequency	:= kHz (94700);
	   Object. workerHandle		:= null;
	   Object. gains		:= null;
	   Object. gainsCount		:= 0;
	   Object. theGain		:= 0;
	   Object. vfoOffset		:= 0;
	   Object. valid		:= False;
--
--	here we really start
--
	   Object. deviceCount	:= rtlsdr_get_device_count_ada;
	   if Object. deviceCount = 0 then		-- it won't work
	      put (" could not find a device"); New_Line (1);
	      return;
	   end if;

	   Object. deviceIndex	:= 0;	-- default;
	   Object. res		:= rtlsdr_open_ada (Object. device, 
	                                    Interfaces. C. int (Object. deviceIndex));
	   if Object. res < 0 then
	      put_line ( "could not open the device");
	   end if;

	   Object. valid     := true;
	   rtlsdr_set_sample_rate_ada (Object. device. all,
	                               Interfaces. C. int (Object. inputRate));
	   Object. res       :=
	                      rtlsdr_get_sample_rate_ada (Object. device. all);
	   put (" samplerate set to ");
	   put_line (Integer' Image (Integer (Object. res))); 

	   declare
	      function rtlsdr_get_tuner_gains_count (device : rtlsdr_dev_t;
	                                             dummy  : Integer)
	                                              return Interfaces. C. int;
	      pragma Import (C, rtlsdr_get_tuner_gains_count,
	                                       "rtlsdr_get_tuner_gains");
	   begin
	      Object. gainsCount   :=
	            rtlsdr_get_tuner_gains_count (Object. device. all, 0);
	      put (Integer' Image (Integer (Object. gainsCount)));
	      put_line (" gain values supported");
	      declare
	         subtype g is gainsArray (0 ..
	                           Integer (Object. gainsCount) - 1);
	         function rtlsdr_get_tuner_gains_ada (device : rtlsdr_dev_t;
	                                              gains  : out g)
	                                             return Interfaces. C. int;
	         pragma Import (C, rtlsdr_get_tuner_gains_ada,
	                                  "rtlsdr_get_tuner_gains");
	         dummy : interfaces. C. int;
	      begin
	         Object. gains     := new g;
	         dummy	:=
	               rtlsdr_get_tuner_gains_ada (Object. device. all,
	                                           Object. gains. all);
	      end;
--	will be overruled:
	      rtlsdr_set_tuner_gain_mode_ada (Object. device. all, 0);
	      Object. theGain	:=  50;
	      rtlsdr_set_tuner_gain_ada      (Object. device. all,
	                                      Object. gains (Integer (Object. gainsCount) / 2));
	      for i in Object. gains' Range loop
	         declare 
	            tmp : float	:= float (Object. gains (i)) / 10.0;
	         begin
	            put (float' Image (tmp));
	         end;
	      end loop;
	      New_Line (1);
	   end; 
	end;
	procedure Finalize (Object : in out rtlsdr_device) is
	begin
	   Stop_Reader (Object);
	   if Object. gains /= null then
	      Free_gainsArray (Object. gains);
	   end if;
	end;
end rtlsdr_wrapper;

