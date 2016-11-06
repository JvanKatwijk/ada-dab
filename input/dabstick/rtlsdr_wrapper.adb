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
	procedure Free_Handler is new Ada. Unchecked_Deallocation (
	         Object	=> rtlsdr_reader, Name	=> rtlsdr_reader_P);

	deviceCount	: Interfaces. C. int;
	deviceIndex	: Integer;
	res		: Interfaces. C. int;
--	set up
--
--	This callback will be called 2048000 / 8192 (i.e. app 250)
--	times a second and is kept simple: just put the data into
--	the buffer
	procedure rtlsdr_Callback (buffer   : System. Address;
	                           size	    : Interfaces. C. int;
	                           userData : System. Address) is
--      mybuffer is actually a C array, provided for by the underlying
--      osmocom library
	   my_buffer   : arrayConverter. Object_Pointer :=
                                arrayConverter. To_Pointer (buffer);
	begin
	   if Integer (size) /= READLEN_DEFAULT then
	      return;
	   end if;
--
--	Put data in the ringbuffer and go on
	   theBuffer. putDataIntoBuffer (my_buffer. all);
	end rtlsdr_Callback;

	task body rtlsdr_reader is
	begin
	   rtlsdr_read_async_ada (device. all,				-- 
	                          rtlsdr_Callback' access,
	                          null_Address,		-- user Data
	                          0,
	                          Interfaces. C. int (READLEN_DEFAULT));
	end rtlsdr_reader;

	procedure Set_VFOFrequency (frequency : Positive) is
	begin
	   lastFrequency   := frequency;
	   rtlsdr_set_center_freq_ada (device. all,
	                               Interfaces. C. int (frequency));
	end Set_VFOFrequency;

	function Get_VFOFrequency return Integer is
	begin
	    return  Integer (rtlsdr_get_center_freq_ada (device. all));
	end Get_VFOFrequency;

	procedure Restart_Reader (result : out Boolean) is
	   res : Interfaces. C. int;
	begin
	   if workerHandle /= null then     -- running already
	      result    := true;
	      return;
	   end if;

	   theBuffer. FlushRingBuffer;
	   res   := rtlsdr_reset_buffer_ada (device. all);
	   if res < 0 then
	      result	:= false;
	      return;
	   end if;

	   rtlsdr_set_center_freq_ada (device. all,
	                                 Interfaces. C. int (lastFrequency));
	   workerHandle := new rtlsdr_reader;
	   result := true;
	end Restart_Reader;

	procedure Stop_Reader is
	begin
	   if workerHandle = null then
	      return;
	   end if;

	   rtlsdr_cancel_async_ada (device. all);
	   delay 1.0;
	   if workerHandle. all 'Terminated then
	      put ("already terminated ");
	   else
	      abort workerhandle. all;
	   end if;
	   while not workerHandle. all' Terminated loop
	      put ("waiting for termination"); New_Line (1);
	      delay 0.5;
	   end loop;

	   Free_Handler (workerHandle);
	   workerHandle := null;
	end Stop_Reader;

	procedure Set_Gain (gain : Natural) is
	begin
	   rtlsdr_set_tuner_gain_ada (device. all, Interfaces. C. int (gain));
	   theGain   := gain;
	end Set_Gain;

--	Note that we get uint8_t's in and send complex(float)'s out
	procedure Get_Samples (outV : out complexArray; amount : out Integer) is
	   tempBuffer: rtlsdr_buffer. buffer_data (0 .. 2 * outV' length - 1);
	   res   : Positive;
	begin
	   theBuffer. getDataFromBuffer (tempBuffer, res);
	   res := res / 2;
--
--	Now we are talking pairs
	   for I in 0 .. res - 1 loop
	      outV (i) := (float (Integer (tempBuffer (2 * i)) - 128) / 128.0,
	                   float (Integer (tempBuffer (2 * i + 1)) - 128) / 128.0);
	   end loop;
	   amount := res;
	end Get_Samples;

	function Available_Samples return Integer is
	begin
	   return theBuffer. GetRingBufferReadAvailable / 2;
	end Available_Samples;

	function Valid_Device return Boolean is
	begin
	   return valid;
	end valid_Device;

	procedure Setup_GainTable (gainSelector : Gtk_Combo_Box_Text) is
	begin
	   for I in 0 .. Integer (gainsCount) - 1 loop
	      gainSelector. Insert_Text (Gint (I),
	                                 Integer' Image (Integer (gains (i))));
	   end loop;
	end Setup_GainTable;
begin
	device		:= new rtlsdr_dev_t; 
	inputRate	:= 2048000;
	lastFrequency	:= kHz (94700);
	workerHandle	:= null;
	gains		:= null;
	gainsCount	:= 0;
	theGain		:= 0;
	vfoOffset	:= 0;
	valid		:= False;
--
--	here we really start
--
	deviceCount	:= rtlsdr_get_device_count_ada;
	if deviceCount = 0
	then		-- it won't work
	   put (" could not find a device"); New_Line (1);
	   goto Error;
	end if;

	deviceIndex	:= 0;	-- default;
	res		:= rtlsdr_open_ada (device, 
	                                    Interfaces. C. int (deviceIndex));
	if res < 0
	then
	   put_line ( "could not open the device");
	end if;

	valid     := true;
	rtlsdr_set_sample_rate_ada (device. all, Interfaces. C. int (inputRate));
	res         := rtlsdr_get_sample_rate_ada (device. all);
	put (" samplerate set to "); put_line (Integer' Image (Integer (res))); 


	declare
	   function rtlsdr_get_tuner_gains_ada (device : rtlsdr_dev_t;
	                                        dummy  : Integer)
	                                             return Interfaces. C. int;
	   pragma Import (C, rtlsdr_get_tuner_gains_ada, "rtlsdr_get_tuner_gains");
	begin
	   gainsCount   := rtlsdr_get_tuner_gains_ada (device. all, 0);
	   put (Integer' Image (Integer (gainsCount)));
	   put_line (" gain values supported");
	   declare
	      subtype g is gainsArray (0 .. Integer (gainsCount) - 1);
	      procedure rtlsdr_get_tuner_gains_ada (device : rtlsdr_dev_t;
	                                            gains  : out g);
	      pragma Import (C, rtlsdr_get_tuner_gains_ada,
	                                  "rtlsdr_get_tuner_gains");
	   begin
	      gains	:= new g;
	      rtlsdr_get_tuner_gains_ada (device. all, gains. all);
	   end;
--	will be overruled:
	   rtlsdr_set_tuner_gain_mode_ada (device. all, 1);
	   theGain	:=  Integer (gainsCount) / 2;
	   rtlsdr_set_tuner_gain_ada      (device. all, gains (theGain));
	   for i in gains' Range loop
	      declare 
	         tmp : float	:= float (gains (i)) / 10.0;
	      begin
	         put (float' Image (tmp));
	      end;
	   end loop;
	end; 
	New_Line (1);
<<Error>>
	null;
end rtlsdr_wrapper;

