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
with Gtk.Combo_Box_Text;	use Gtk. Combo_Box_Text;
with System;			use System;
with header;			use header;
with Interfaces;		use Interfaces;
with Interfaces.C;		use Interfaces. C;
with ringbuffer;

package Airspy_Wrapper is
	procedure Setup_Gaintable  (Gain_Selector: Gtk_Combo_Box_Text);
	procedure Set_VFOFrequency (Frequency:	Integer);
	procedure Restart_Reader   (Valid:	out Boolean);
	procedure Stop_Reader;
	procedure Set_Gain	   (New_Gain:	Integer);
	procedure Get_Samples      (Out_V:	out complexArray;
	                            amount:	out Integer);
	function Available_Samples  return Integer;
	function Valid_Device       return Boolean;
private
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
	   Device:           System. Address;
	   Ctx:	             System. Address;
	   Samples:          System. Address;
	   Sample_Count:     Interfaces. C. int;
	   Dropped_Samples:  Interfaces. C. unsigned_long;
	   Airspy_Sample_Type	: Type_of_Sample;
	end record;
	pragma Convention (C, Airspy_Transfer);

	type Airspy_Transfer_P	is access all Airspy_Transfer;

	function Airspy_Init return  Airspy_Error;
	pragma Import (C, Airspy_Init, "airspy_init");

	function Airspy_Open	(Device_P: access system. Address)
	                                           return Airspy_Error;
	pragma Import (C, Airspy_Open, "airspy_open");

	procedure Airspy_Set_Vga_Gain	(Device	: system. Address;
	                                 Vga_Gain : Interfaces. C. int);
	pragma Import (C, Airspy_Set_Vga_Gain, "airspy_set_vga_gain");

	procedure Airspy_Set_Mixer_Gain	(Device:	system. Address;
	                                 Mixer_Gain:   Interfaces. C. int);
	pragma Import (C, Airspy_Set_Mixer_Gain, "airspy_set_mixer_gain");

	procedure Airspy_Set_Lna_Gain	(Device: system. Address;
	                                 Lna_Gain:  Interfaces. C. int);
	pragma Import (C, Airspy_Set_Lna_Gain, "airspy_set_lna_gain");

	procedure Airspy_Set_Sensitivity (device:	system. Address;
	                                  Sensitivity:	Interfaces. C. int);
	pragma Import (C, Airspy_Set_Sensitivity, "airspy_set_sensitivity_gain");

	function Airspy_Is_Streaming (Device : system. Address)
	                                         return Interfaces.C.int;
	pragma Import (C, Airspy_Is_Streaming, "airspy_is_streaming");

	function Airspy_Set_Sample_Type (device:          system. Address;
	                                 The_Sample_Type: Type_Of_Sample)
	                                          return Airspy_Error;
	pragma Import (C, Airspy_Set_Sample_Type, "airspy_set_sample_type");
--
--	just for this one
	type Int_Array is Array (Integer Range <>) of Interfaces. C. int;
	subtype Rate_Buffer is Int_Array (0 .. 20);
	procedure Airspy_Get_Samplerates (Device:	system. Address;
	                                  Out_Buffer:	out Rate_Buffer;
	                                  Count:	Interfaces. C. int);
	pragma Import (C, Airspy_Get_Samplerates, "airspy_get_samplerates");

	function Airspy_Set_Samplerate	(Device:	system. Address;
	                                 Sample_Rate:	Integer)
	                                          return Airspy_Error;
	pragma Import (C, Airspy_Set_Samplerate, "airspy_set_samplerate");

	function Airspy_Set_Frequency (Device:	system. Address;
	                               NewFrequency: Integer)
	                                          return Airspy_Error;
	pragma Import (C, Airspy_Set_Frequency, "airspy_set_freq");

	type Airspy_Callback_Type is access
	          function (Transfer : Airspy_Transfer_P)
	                          return Interfaces.C. int;
	pragma Convention (C, Airspy_Callback_Type);

	function  Airspy_Start_Rx	(device:	system. Address;
                                         cb:		Airspy_Callback_Type;
	                                 Userdata:	system. Address)
	                                               return Airspy_Error;
	pragma Import (C, Airspy_Start_Rx, "airspy_start_rx");

	procedure  Airspy_Stop_Rx	(Device: system. Address);
	pragma Import (C, Airspy_Stop_Rx, "airspy_stop_rx");

	function Airspy_Callback (transfer: Airspy_Transfer_P)
	                        return Interfaces. C. int;
	pragma Convention (C, Airspy_Callback);
--
end Airspy_Wrapper;

