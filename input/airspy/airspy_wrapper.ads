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
with device_handler;		use device_handler;
with System;			use System;
with header;			use header;
with Interfaces;		use Interfaces;
with Interfaces.C;		use Interfaces. C;
with ringbuffer;

package Airspy_Wrapper is
	use header. complexTypes;
	type airspy_device is new device with private;
	type airspy_device_p is access all airspy_device;

	overriding
	procedure Restart_Reader   (Object       : in out airspy_device;
	                            Success      : out Boolean);
        procedure Stop_Reader      (Object       : in out airspy_device);
        procedure Set_VFOFrequency (Object       : in out airspy_device;
	                            New_Frequency: Natural);
        procedure Set_Gain         (Object       : in out airspy_device;
	                            New_Gain     : Natural);
        procedure Get_Samples      (Object       : in out airspy_device;
	                            Out_V        : out complexArray;
                                    Amount       : out Natural);
        function Available_Samples (Object       : airspy_device) return Natural;
        function Valid_Device      (Object       : airspy_device) return Boolean;
private
	procedure Initialize (Object: in out airspy_device);
	procedure Finalize   (Object: in out airspy_device);

	use header. complexTypes;
	package Airspy_Buffer is new ringBuffer (complex);
	use Airspy_Buffer;

--
--	airspy stuff
	type Airspy_Error is (
           AIRSPY_SUCCESS,
           AIRSPY_TRUE,
           AIRSPY_ERROR_OTHER 
	);

	for Airspy_Error use (
           AIRSPY_SUCCESS 	=> 0,
           AIRSPY_TRUE		=> 1,
	   AIRSPY_ERROR_OTHER	=> 2
	);

	type Type_Of_Sample is (
	   AIRSPY_SAMPLE_FLOAT32_IQ,
           AIRSPY_SAMPLE_FLOAT32_REAL,
           AIRSPY_SAMPLE_INT16_IQ,
           AIRSPY_SAMPLE_INT16_REAL,	-- 1 * 16bit int per sample 
           AIRSPY_SAMPLE_UINT16_REAL,	-- 1 * 16bit unsigned int per sample 
           AIRSPY_SAMPLE_END		-- Number of supported sample types 
	);

	for Type_Of_Sample use (
	   AIRSPY_SAMPLE_FLOAT32_IQ	=> 0, -- 2 * 32bit float per sample 
           AIRSPY_SAMPLE_FLOAT32_REAL	=> 1, -- 1 * 32bit float per sample 
           AIRSPY_SAMPLE_INT16_IQ	=> 2, -- 2 * 16bit int per sample 
           AIRSPY_SAMPLE_INT16_REAL	=> 3, -- 1 * 16bit int per sample 
           AIRSPY_SAMPLE_UINT16_REAL 	=> 4, -- 1 * 16bit unsigned int per sample */
           AIRSPY_SAMPLE_END 		=> 5  -- Number of supported sample types 
	);

	type Airspy_Transfer is
	record
	   Device          : System. Address;
	   Ctx             : System. Address;
	   Samples         : System. Address;
	   Sample_Count    : Interfaces. C. int;
	   Dropped_Samples :  Interfaces. C. unsigned_long;
	   Airspy_Sample_Type	: Type_of_Sample;
	end record;
	pragma Convention (C, Airspy_Transfer);

	type Airspy_Transfer_P	is access all Airspy_Transfer;

	function Airspy_Init return  Airspy_Error;
	pragma Import (C, Airspy_Init, "airspy_init");

	function Airspy_Open	(Device_P : access system. Address)
	                                           return Airspy_Error;
	pragma Import (C, Airspy_Open, "airspy_open");

	procedure Airspy_Set_Sensitivity (Device   : system. Address;
	                                  gain : Interfaces. C. int);
	pragma Import (C, Airspy_Set_Sensitivity, "airspy_set_sensitivity_gain");

	function Airspy_Is_Streaming (Device : system. Address)
	                                         return Interfaces.C.int;
	pragma Import (C, Airspy_Is_Streaming, "airspy_is_streaming");

	function Airspy_Set_Sample_Type (Device          :  system. Address;
	                                 The_Sample_Type : Type_Of_Sample)
	                                          return Airspy_Error;
	pragma Import (C, Airspy_Set_Sample_Type, "airspy_set_sample_type");
--
	function Airspy_Set_Samplerate	(Device      : system. Address;
	                                 Sample_Rate : Integer)
	                                          return Airspy_Error;
	pragma Import (C, Airspy_Set_Samplerate, "airspy_set_samplerate");

	function Airspy_Set_Frequency (Device       : system. Address;
	                               NewFrequency : Integer)
	                                          return Airspy_Error;
	pragma Import (C, Airspy_Set_Frequency, "airspy_set_freq");

	type Airspy_Callback_Type is access
	          function (Transfer : Airspy_Transfer_P)
	                          return Interfaces.C. int;
	pragma Convention (C, Airspy_Callback_Type);

	function  Airspy_Start_Rx	(device    : system. Address;
                                         cb        : Airspy_Callback_Type;
	                                 Userdata  : system. Address)
	                                               return Airspy_Error;
	pragma Import (C, Airspy_Start_Rx, "airspy_start_rx");

	procedure  Airspy_Stop_Rx	(Device : system. Address);
	pragma Import (C, Airspy_Stop_Rx, "airspy_stop_rx");

	function Airspy_Callback (transfer: Airspy_Transfer_P)
	                        return Interfaces. C. int;
	pragma Convention (C, Airspy_Callback);
--

	type airspy_device is new device with
	record
	   The_Buffer    : Airspy_Buffer. ringBuffer_data (16 * 32768);
	   Input_Rate    : Integer;
--	buffer for transfer to data into the ringbuffer
	   Local_Buffer  : airspy_buffer. buffer_data (0 .. 2048000 / 500 - 1);

--	we read in 500 buffers per second, and we have to convert them
--	to the DAB input rate of 2048000 samples per second
--	Conversion is by simple linear interpolation. Since we do not know
--	the inputrate of the device (yet),
--	we cannot yet decide on the size of the
--	conversionbuffer.
	   Conversion_Buffer : complexArray_P;
--
--	The mapping tables are known though
	   Conversion_Index  : Integer	:= 0;
	   Maptable_Int      : FloatArray	(0 .. 2048000 / 500 - 1);
	   Maptable_Float    : FloatArray	(0 .. 2048000 / 500 - 1);

	   Result            : Airspy_Error;
--	abstract away from the hardware
	   Device_P          : access System. Address := new System. Address;
--
	   Running           : Boolean;
	   Device_Is_Valid   : Boolean;
	   Sensitivity	     : Natural;
	   Last_Frequency    : Integer	:= 225000000;	-- dummy
	end record;
end Airspy_Wrapper;

