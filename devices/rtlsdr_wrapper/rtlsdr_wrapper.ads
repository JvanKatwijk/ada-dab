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
with ringbuffer;
with header;	        use header;
with device_handler;    use device_handler;
with Ada. Finalization; use Ada. Finalization;
with Ada. Unchecked_Deallocation;
with Interfaces. C;
package rtlsdr_wrapper is
	use header. complexTypes;
        type rtlsdr_device is new device with private;
        type rtlsdr_device_p is access all rtlsdr_device;

	subtype rtlsdr_dev_t	is System. Address;
	type rtlsdr_dev_P	is access all rtlsdr_dev_t;

	overriding
        procedure Restart_Reader   (Object       : in out rtlsdr_device;
                                    Success      : out Boolean);
        procedure Stop_Reader      (Object       : in out rtlsdr_device);
        procedure Set_VFOFrequency (Object       : in out rtlsdr_device;
                                    New_Frequency: Natural);
        procedure Set_Gain         (Object       : in out rtlsdr_device;
                                    New_Gain     : Natural);
        procedure Get_Samples      (Object       : in out rtlsdr_device;
                                    Out_V        : out complexArray;
                                    Amount       : out Natural);
        function Available_Samples (Object       : rtlsdr_device)
	                                                  return Natural;
        function Valid_Device      (Object       : rtlsdr_device)
	                                                  return Boolean;

private
	procedure Initialize (Object: in out rtlsdr_device);
        procedure Finalize   (Object: in out rtlsdr_device);

        package rtlsdr_Buffer is new ringBuffer (byte);
        use rtlsdr_Buffer;

	READLEN_DEFAULT	: constant	:= 2 * 8192;
--
--	The "worker task" is created dynamically and
--	needs the address of the "context", passing it on to
--	the callback function
	task type rtlsdr_reader is
	   entry start (context : System. Address);
	end;
	type rtlsdr_reader_P is access all rtlsdr_reader;

	procedure Free_Handler is new Ada. Unchecked_Deallocation (
                 Object => rtlsdr_reader, Name  => rtlsdr_reader_P);

	type gainsArray is Array (Positive Range <>) of
                                                 Interfaces. C. int;
        type gainsArray_P is access all gainsArray;
        procedure Free_gainsArray is new Ada. Unchecked_Deallocation (
                  Object => gainsArray, Name => gainsArray_P);

	type rtlsdr_device is new device with
	record
	   The_Buffer      : rtlsdr_Buffer. ringBuffer_data (16 * 32768);
	   device          : rtlsdr_dev_P;
	   inputRate	   : Natural;
	   deviceCount	   : Interfaces. C. int;
	   deviceIndex	   : Interfaces. C. int;
	   Running         : Boolean;
	   Last_Frequency  : Natural;
	   workerHandle    : rtlsdr_reader_P; 
	   res	           : Interfaces. C. int;
	   gains           : gainsArray_P;
	   gainsCount      : Interfaces. C. int;
	   theGain         : Integer;
	   vfoOffset       : Integer;
	   valid           : Boolean;
	end record;

	type rtlsdr_CallbackType is access
	                procedure (buffer   : System. Address;
	                           size     : Interfaces. C. int;
	                           userData : System. Address);
	pragma Convention (C, rtlsdr_CallbackType);

	function rtlsdr_open_ada (device_p     : rtlsdr_dev_P;
	                          deviceNumber : Interfaces. C. int)
	                                           return Interfaces. C. int;
	pragma Import (C, rtlsdr_open_ada,  "rtlsdr_open");

	procedure rtlsdr_close_ada (device    : rtlsdr_dev_t);
	pragma Import (C, rtlsdr_close_ada,  "rtlsdr_close");

	procedure rtlsdr_set_center_freq_ada (device    : rtlsdr_dev_t;
	                                      frequency : Interfaces. C. int);
	pragma Import (C, rtlsdr_set_center_freq_ada, "rtlsdr_set_center_freq");

	function rtlsdr_get_center_freq_ada (device : rtlsdr_dev_t)
	                                            return Interfaces. C. int;
	pragma Import (C, rtlsdr_get_center_freq_ada, "rtlsdr_get_center_freq");

	procedure rtlsdr_set_tuner_gain_mode_ada (device: rtlsdr_dev_t;
	                                          gainMode: Interfaces. C. int);
	pragma Import (C, rtlsdr_set_tuner_gain_mode_ada,
	                                         "rtlsdr_set_tuner_gain_mode");

	procedure rtlsdr_set_tuner_gain_ada (device : rtlsdr_dev_t;
	                                           gain : Interfaces. C. int);
	pragma Import (C, rtlsdr_set_tuner_gain_ada, "rtlsdr_set_tuner_gain");

	function rtlsdr_get_tuner_gain_ada (device : rtlsdr_dev_t)
	                                             return Interfaces. C. int;
	pragma Import (C, rtlsdr_get_tuner_gain_ada, "rtlsdr_get_tuner_gain");

	procedure rtlsdr_set_sample_rate_ada (device : rtlsdr_dev_t;
	                                             rate: Interfaces. C. int);
	pragma Import (C, rtlsdr_set_sample_rate_ada, "rtlsdr_set_sample_rate");

	function rtlsdr_get_sample_rate_ada (device : rtlsdr_dev_t)
	                                              return Interfaces. C. int;
	pragma Import (C, rtlsdr_get_sample_rate_ada,
	                                     "rtlsdr_get_sample_rate");

	function rtlsdr_reset_buffer_ada (device: rtlsdr_dev_t)
	                                              return Interfaces. C. int;
	pragma Import (C, rtlsdr_reset_buffer_ada, "rtlsdr_reset_buffer");

	procedure rtlsdr_read_async_ada (device      : rtlsdr_dev_t;
                                         theCallback : rtlsdr_CallbackType;
                                         userData    : System. Address;
	                                 bufNum      : Interfaces. C. int;
	                                 bufLen      : Interfaces. C. int);
	pragma Import (C, rtlsdr_read_async_ada, "rtlsdr_read_async");

	procedure rtlsdr_cancel_async_ada (device: rtlsdr_dev_t);
	pragma Import (C, rtlsdr_cancel_async_ada, "rtlsdr_cancel_async");

	function rtlsdr_get_device_count_ada return Interfaces. C. int;
	pragma Import (C, rtlsdr_get_device_count_ada,
	                                         "rtlsdr_get_device_count");

	function rtlsdr_set_freq_correction_ada (device : rtlsdr_dev_t;
	                                         amount : Interfaces. C. int)
	                                             return Interfaces. C. int;
	pragma Import (C, rtlsdr_set_freq_correction_ada,
	                                         "rtlsdr_set_freq_correction");

	procedure rtlsdr_Callback (buffer   : System. Address;
	                           size     : Interfaces. C. int;
	                           userData : System. Address);
	pragma Convention (C, rtlsdr_Callback);
end rtlsdr_wrapper;

