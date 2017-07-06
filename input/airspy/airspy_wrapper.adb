--
--    Copyright (C) 2016
--    Jan van Katwijk (J.vanKatwijk@gmail.com)
--    Lazy Chair Computing
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
--	Wrapper around the airspy library.
--	The airspy delivers samples at a selectable rate.
--	We select the rate closest to 2048000 and downsample
--	The rates of the airspy are obtained by "asking" the device
--	and it shows that they are different between airspy and airspy mini
with System. Address_To_Access_Conversions;
with Text_IO; use Text_IO;

package body Airspy_Wrapper is
	procedure Set_VFOFrequency	(Object : in out airspy_device;
	                                 New_Frequency: Natural) is
	   Result	: Airspy_Error;
	begin
	   Result	:= Airspy_Set_Frequency (Object. Device_P. all,
	                                         New_Frequency);
	   if Result /= AIRSPY_SUCCESS then
	      put_line ("Airspy_set_freq failed");
	   else
	      Object. Last_Frequency	:= New_Frequency;
	   end if;
	end Set_VFOFrequency;

	procedure Restart_Reader (Object  : in out airspy_device;
	                          Success : out Boolean) is
	   Handler_Returns : Airspy_Error;
	begin
	   Success      := false;	-- default, unless ....
	   if not Object. Device_Is_Valid then
	      return;
	   end if;
	   if Object. Running then
	      Success     := true;
	      return;
	   end if;

	   Object. The_Buffer. FlushRingBuffer;
	   Handler_Returns   :=
	               Airspy_Set_Sample_Type (Object. device_P. all,
	                                       AIRSPY_SAMPLE_FLOAT32_IQ);
	   if Handler_Returns /= AIRSPY_SUCCESS then
	      put_line ("airspy_set_sample_type failed");
	      return;
	   end if;
--
	   Handler_Returns :=
	               Airspy_set_samplerate (Object. device_P. all,
	                                      Object. Input_Rate);
	   if Handler_Returns /= AIRSPY_SUCCESS then
	      put_line ("airspy_set_samplerate failed");
	      return;
	   end if;
--
--	we assume the next three settings work fine
	   Airspy_Set_Sensitivity  (Object. device_P. all,
	                            Interfaces. C. int (Object. Sensitivity));
--
--	context for the callback function
	   Handler_Returns     := Airspy_Start_Rx (Object. device_P. all,
                                                   Airspy_Callback' Access, 
	                                           Object' Address);
	   if Handler_Returns /= AIRSPY_SUCCESS then
	      put_Line ("airspy_start_rx failed");
	      return;
	   end if;
	   Object. Running      := true;
	   Success      := true;
	end Restart_Reader;

	procedure Stop_Reader (Object : in out airspy_device) is
	begin
	   if not Object. Running then
	      return;
	   end if;
	   Airspy_Stop_Rx (Object. device_P. all);
	   Object. Running	:= false;
	end Stop_Reader;
--
	package environmentConverter is
	      new System. Address_to_Access_Conversions (airspy_device);
	function Airspy_Callback (Transfer: Airspy_Transfer_P)
	                                            return Interfaces.C.int is
	   localEnv : environmentConverter. Object_Pointer :=
	                      environmentConverter. To_Pointer (Transfer. Ctx);
	begin
	   if Transfer = null then
	      return 0;
	   end if;
--	we do the rate conversion "in-line"
	   declare
	      type Airspy_Data is array (Integer range <>) of 
	                                        Interfaces. C. C_float;
	      pragma Convention (C, Airspy_Data);
	      subtype C_Buffer is Airspy_Data (0 .. 
	                          Integer (Transfer. Sample_Count) * 2 - 1);
	      package arrayConverter is
	         new System. Address_To_Access_Conversions (C_buffer);
	      Databuffer_P    : arrayConverter. Object_Pointer :=
                                arrayConverter. To_Pointer (transfer. samples);
--	"Data_Buffer" is now the Ada name for the incoming data
	   begin
	      for I in 0 .. Integer (transfer. Sample_Count) - 1 loop
	         localEnv. Conversion_Buffer (localEnv. Conversion_Index) :=
	                                 (Float (Databuffer_P (2 * i)),
	                                  Float (Databuffer_P (2 * i + 1)));
	         localEnv. Conversion_Index :=
	                                localEnv. Conversion_Index + 1;
	         if localEnv. Conversion_Index >
	                             localEnv. Input_Rate / 500 then
--	            do for i.e. 2048000 / 500
	            for J in localEnv. Local_Buffer' Range loop
	               declare
	                  Inp_Base    : Float	:= localEnv. Maptable_int (J);
	                  Inp_Ratio   : Float	:= localEnv. Maptable_float (J);
	                  Index       : Integer	:= Integer (Inp_Base);
	               begin
	                  localEnv. Local_Buffer (J). Re :=
	                      localEnv. Conversion_Buffer (Index + 1). Re * Inp_Ratio +
	                      localEnv. Conversion_Buffer (Index). Re *
	                                    (1.0 -  Inp_Ratio);
	                  localEnv. Local_Buffer (J). Im	:=
	                      localEnv. Conversion_Buffer (Index + 1). Im * Inp_Ratio +
	                      localEnv. Conversion_Buffer (Index). Im *
	                                    (1.0 -  Inp_Ratio);
	               end;
	            end loop;

	            localEnv. The_Buffer.
	                   putDataIntoBuffer (localEnv. Local_Buffer);
--
--	shift the sample at the end to the beginning, it is needed
--	as the starting sample for the next time
                    localEnv. Conversion_Buffer (0) :=
	                           localEnv. Conversion_Buffer (localEnv. Input_Rate / 500);
                    localEnv. Conversion_Index  := 1;
	         end if;
	      end loop;
	   end;
	   return 0;
	end Airspy_Callback;
--
--	we use the gain setting for setting the sensitivity
	procedure Set_Gain (Object   : in out airspy_device;
	                    New_Gain : Natural) is
	begin
	   Object. Sensitivity := New_Gain * 21 / 100;
	   Airspy_Set_Sensitivity (Object. device_P. all, 
	                           Interfaces. C. int (Object. Sensitivity));
	                          
	end Set_Gain;

	procedure Get_Samples	(Object  : in out airspy_device;
	                         Out_V   : out complexArray;
	                         Amount  : out Natural) is
	begin
	   Object. The_Buffer. getDataFromBuffer (buffer_data (Out_V), amount);
	end Get_Samples;
--
--
	function Available_Samples (Object : airspy_device) return Natural is
	begin
	   return  Object. The_Buffer. GetRingBufferReadAvailable;
	end Available_Samples;
--
--
	function Valid_Device  (Object : airspy_device) return Boolean is
	begin
	   return Object. Device_Is_Valid;
	end Valid_Device;

--	The C- function "airspy_get_samplerates" is overloaded.
--	when the third parameter is zero, it returns the amount
--	of different samplerates the device supports,
--	when the third parameter is the amount, it returns a list
--	of supported rates.
--
--	We create different procedures for the different behaviour
--	Note that we need to know the amount to create a subtype of
--	a C array to catch the different rates.
	function Fetchrates (Object : in out airspy_device) return Natural is
	   type C_Array is array (Interfaces. C. int range <>)
	                                             of Interfaces. C. int;
	   Rates  :	Interfaces. C. int;
	   Input_Rate : Natural := 10000000;
	   procedure Airspy_Amount_of_Rates (Device      : system. Address;
	                                     Amount_Rates: out Interfaces. C. int;
	                                     Zero        : Interfaces.C.int);
	   pragma Import (C, Airspy_Amount_of_Rates,
	                                     "airspy_get_samplerates");
	begin
	   Airspy_Amount_of_Rates (Object. Device_P. all, Rates, 0);
--	This call returns the number of supported samplerates
	   declare
	      subtype Ratebuffer is C_Array (0 .. Rates - 1);
	      procedure Airspy_Get_Samplerates (Device     : system. Address;
	                                        Out_Buffer : out Ratebuffer;
	                                        Count      : Interfaces. C. int);
	      pragma Import (C, Airspy_Get_Samplerates,
	                                    "airspy_get_samplerates");
	      The_Ratebuffer : Ratebuffer;
	   begin
--	read the list of supported rates
	      Airspy_Get_Samplerates (Object. device_P. all,
	                              The_Ratebuffer, Rates);

--	and look for the one closest to and larger than 2048000
	      for I in 0 .. Rates - 1 loop
	         if The_Ratebuffer (i) > 2048000 and then
	            The_Ratebuffer (i) < Interfaces. C. int (Input_Rate) then
	            Input_Rate :=  integer (The_Ratebuffer (i));
	         end if;
	      end loop;
	   end;
	   return Input_Rate;
	end;

	procedure Finalize   (Object: in out airspy_device) is
	begin
	   null;
	end Finalize;

	procedure Initialize (Object: in out airspy_device) is
	   Result : Airspy_Error;
	begin
	   Object. Sensitivity     := 11;

	   Object. Running         := False;
	   Object. Device_Is_Valid := False;
	   Result                  := Airspy_Init;	-- this is s call
	   if Result /= AIRSPY_SUCCESS then
	      put_line ("airspy_init failed");
	      return;
	   end if;
	   Result                  := Airspy_Open (Object. device_P);
	   if Result /= AIRSPY_SUCCESS then
	      put_line ("airspy_open failed");
	      return;
	   end if;
--
--	we look for an inputrate that is as close as possible to
--	the required 2048000 samples/second
--	We know that all airspy's support
	   Object. Input_Rate     := FetchRates (Object);
--
--	the Maptable and ConvTable are used to convert the inputRate
--	into a 2048000 rate
--	The setup is such that we get input every 2 millisecond input

	   for I in Object. Maptable_int' Range loop
	      declare 
	         Integral_Part	: Float := Float' Floor (
	                   Float (I) * Float (Input_Rate / 500) / 4096.0);
	      begin
	         Object. Maptable_int (I)          := Integral_Part;
	         Object. Maptable_float (I)	:= 
	                   Float (I) * Float (Input_Rate / 500) / 4096.0 -
	                                            Object. Maptable_int (I);
	      end;
	   end loop;

	   Object. Conversion_Buffer  :=
	                    new complexArray (0 .. Object. Input_Rate / 500);
	   Object. Device_Is_Valid    := true;
	   Object. Running            := false;
	end Initialize;

end Airspy_Wrapper;

