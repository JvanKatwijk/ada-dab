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
--
--	Wrapper around the airspy library.
--	The airspy delivers samples at a selectable rate.
--	We select the rate closest to 2048000 and downsample
--	The rates of the airspy are obtained by "asking" the device
--	and it shows that they are different between airspy and airspy mini
with Gtk.Main;
with Glib;
with System. Address_To_Access_Conversions;
with Text_IO; use Text_IO;

package body Airspy_Wrapper is
	use header. complexTypes;
	package Airspy_Buffer is new ringBuffer (complex);
	use Airspy_Buffer;

	The_Buffer        : Airspy_Buffer. ringBuffer_data (16 * 32768);
	Input_Rate        : Integer	:= 2500000;	-- default
--	buffer for transfer to data into the ringbuffer
	Local_Buffer      : airspy_buffer. buffer_data (0 .. 2048000 / 500 - 1);

--	we read 500 buffers per second, and we have to convert them
--	to the DAB input rate of 2048000 samples per second
--	Conversion is by simple linear interpolation. Since we do no know
--	the inputrate (yet), we cannot yet decide on the size of the
--	conversionbuffer.
	Conversion_Buffer : complexArray_P;
	Conversion_Index  : Integer	:= 0;
	Maptable_Int      : FloatArray	(0 .. 2048000 / 500 - 1);
	Maptable_Float    : FloatArray	(0 .. 2048000 / 500 - 1);
--
	Result            : Airspy_Error;
--
--	abstract away from the hardware
	Device_P          : access System. Address := new System. Address;
--
	Running           : Boolean;
	Device_Is_Valid   : Boolean;
	Vga_Gain          : Integer;
	Mixer_Gain        : Integer;
	Lna_Gain          : Integer;
	Last_Frequency    : Integer	:= 225000000;	-- dummy

	procedure Set_VFOFrequency	(New_Frequency: Integer) is
	   Result	: Airspy_Error;
	begin
	   Result	:= Airspy_Set_Frequency (Device_P. all, New_Frequency);
	   if Result /= AIRSPY_SUCCESS then
	      put_line ("Airspy_set_freq failed");
	   else
	      Last_Frequency	:= New_Frequency;
	   end if;
	end Set_VFOFrequency;

	procedure Restart_Reader (Success : out Boolean) is
	   Handler_Returns : Airspy_Error;
	begin
	   Success      := false;	-- default, unless ....
	   if not Device_Is_Valid then
	      return;
	   end if;
	   if Running then
	      Success     := true;
	      return;
	   end if;
	   The_Buffer. FlushRingBuffer;
	   Handler_Returns   := Airspy_Set_Sample_Type (device_P. all,
	                                              AIRSPY_SAMPLE_FLOAT32_IQ);
	   if Handler_Returns /= AIRSPY_SUCCESS then
	      put_line ("airspy_set_sample_type failed");
	      return;
	   end if;
--
	   Handler_Returns := airspy_set_samplerate (device_P. all, Input_Rate);
	   if Handler_Returns /= AIRSPY_SUCCESS then
	      put_line ("airspy_set_samplerate failed");
	      return;
	   end if;
--
--	we assume the next three settings work fine
	   Airspy_Set_Vga_Gain 	   (device_P. all,
	                            Interfaces. C. int (Vga_Gain));
	   Airspy_Set_Mixer_Gain   (device_P. all,
	                            Interfaces. C. int (Mixer_Gain));
	   Airspy_Set_Lna_Gain     (device_P. all,
	                            Interfaces. C. int (Lna_Gain));
--
--	since the context is "global" data in the package, we do not need
--	a context for the callback function
	   Handler_Returns     := Airspy_Start_Rx (device_P. all,
                                                   Airspy_Callback' Access, 
	                                           Null_Address);
	   if Handler_Returns /= AIRSPY_SUCCESS then
	      put_Line ("airspy_start_rx failed");
	      return;
	   end if;
	   Running      := true;
	   Success      := true;
	end Restart_Reader;

	procedure Stop_Reader is
	begin
	   if not Running then
	      return;
	   end if;
--	   put ("we stoppen");
	   Airspy_Stop_Rx (device_P. all);
	   Running	:= false;
	end Stop_Reader;
--
--
	function Airspy_Callback (Transfer: Airspy_Transfer_P)
	                                            return Interfaces.C.int is
	begin
	   if Transfer = null then
	      return 0;
	   end if;
--	we do the rate conversion "in-line"
	   declare
	      type Airspy_Data is array (Integer range <>) of 
	                                        Interfaces.C.C_float;
	      pragma Convention (C, Airspy_Data);
	      subtype C_Buffer is Airspy_Data (0 .. 
	                          Integer (Transfer. Sample_Count) * 2 - 1);
	      package arrayConverter is
	         new System. Address_To_Access_Conversions (C_buffer);
	      Databuffer_P    : arrayConverter. Object_Pointer :=
                                arrayConverter. To_Pointer (transfer. samples);
--	my_buffer is now the Ada name for the incoming data
	   begin
	      for i in 0 .. Integer (transfer. Sample_Count) - 1 loop
	         Conversion_Buffer (Conversion_Index) :=
	                      (Float (Databuffer_P (2 * i)),
	                       Float (Databuffer_P (2 * i + 1)));
	         Conversion_Index := Conversion_Index + 1;
	         if Conversion_Index > Input_Rate / 500 then
	            for J in Local_Buffer' Range loop	-- i.e. 2048000 / 500
	               declare
	                  Inp_Base    : Float	:= Maptable_int (j);
	                  Inp_Ratio   : Float	:= Maptable_float (j);
	                  Index       : Integer	:= Integer (Inp_Base);
	               begin
	                  Local_Buffer (j). Re	:=
	                      Conversion_Buffer (Index + 1). Re * Inp_Ratio +
	                      Conversion_Buffer (Index). Re *
	                                    (1.0 -  Inp_Ratio);
	                  Local_Buffer (j). Im	:=
	                      Conversion_Buffer (Index + 1). Im * Inp_Ratio +
	                      Conversion_Buffer (Index). Im *
	                                    (1.0 -  Inp_Ratio);
	               end;
	            end loop;

	            The_Buffer. putDataIntoBuffer (Local_Buffer);
--
--	shift the sample at the end to the beginning, it is needed
--	as the starting sample for the next time
                    Conversion_Buffer (0) := Conversion_Buffer (inputRate / 500);
                    Conversion_Index  := 1;
	         end if;
	      end loop;
	   end;
	   return 0;
	end Airspy_Callback;
--
--	We know that we support 15 gain values
	procedure Setup_Gaintable  (Gain_Selector: Gtk_Combo_Box_Text) is
	begin
	   for I in 0 .. 15 loop
	      Gain_Selector. Insert_text (Glib. Gint (I), Integer' Image (I));
	   end loop;
	end Setup_GainTable;
--
--	we use the gain setting for setting the sensitivity
	procedure Set_Gain (New_Gain : Integer) is
	begin
	   Mixer_Gain	:= New_Gain;
	   Vga_Gain	:= (if New_Gain >= 5 then Mixer_Gain - 5 else Mixer_Gain);
	   Lna_Gain	:= Vga_Gain;
	   Airspy_Set_Vga_Gain	 (device_P. all,
	                          Interfaces. C. int (Vga_Gain));
	   Airspy_Set_Mixer_Gain (device_P. all,
	                          Interfaces. C. int (Mixer_Gain));
	   Airspy_Set_Lna_Gain   (device_P. all,
	                          Interfaces. C. int (Lna_Gain));
	end Set_Gain;

	procedure Get_Samples	(Out_V   : out complexArray;
	                         Amount  : out Integer) is
	begin
	   The_Buffer. getDataFromBuffer (buffer_data (Out_V), amount);
	end Get_Samples;
--
--
	function Available_Samples return Integer is
	begin
	   return The_Buffer. GetRingBufferReadAvailable;
	end Available_Samples;
--
--
	function Valid_Device return Boolean is
	begin
	   return Device_Is_Valid;
	end Valid_Device;
--
begin
	Vga_Gain        := 11;		-- reasonable defaults
	Mixer_Gain      := 15;
	Lna_Gain        := 11;

	Running         := False;
	Device_Is_Valid := False;
	Result          := Airspy_Init;		-- this is s call
	if result /= AIRSPY_SUCCESS then
	   put_line ("airspy_init failed");
	   goto L_end;
	end if;
	Result	:= Airspy_Open (device_P);
	if Result /= AIRSPY_SUCCESS then
	   put_line ("airspy_open failed");
	   goto l_end;
	end if;
--
--	we look for n inputrate that is as close as possible to
--	the required 2048000 samples/second
--	We know that all airspy's support
	Input_Rate     := 10000000;
--
--	The C- function "airspy_get_samplerates" is overloaded.
--	when the third parameter is zero, it returns the amount
--	of different samplerates the device supports,
--	when the third parameter is the amount, it returns a list
--	of supported rates.
--
--	We create different procedures for the different behaviour
--	Note that we need to know the amount to create a subtype of
--	a C array to catch the different rates.
--	
	declare
	   type C_Array is array (Interfaces. C. int range <>)
	                                             of Interfaces. C. int;
	   Different_Rates  :	Interfaces. C. int;
	   procedure Airspy_Amount_of_Rates (Device      : system. Address;
	                                     Amount_Rates: out Interfaces. C. int;
	                                     Zero        : Interfaces.C.int);
	   pragma Import (C, Airspy_Amount_of_Rates,
	                                     "airspy_get_samplerates");
	begin

	   Airspy_Amount_of_Rates (Device_P. all, Different_Rates, 0);
--	This call returns the number of supported samplerates
	   declare
	      subtype Ratebuffer is C_Array (0 .. Different_Rates - 1);
	      procedure Airspy_Get_Samplerates (Device     : system. Address;
	                                        Out_Buffer : out Ratebuffer;
	                                        Count      : Interfaces. C. int);
	      pragma Import (C, Airspy_Get_Samplerates,
	                                    "airspy_get_samplerates");
	      The_Ratebuffer : Ratebuffer;
	   begin
--	read the list of supported rates
	      Airspy_Get_Samplerates (device_P. all,
	                           The_Ratebuffer, Different_Rates);

--	and look for the one closest to and larger than 2048000
	      for I in 0 .. Different_Rates - 1 loop
	         if The_Ratebuffer (i) > 2048000 and then
	            The_Ratebuffer (i) < Interfaces. C. int (Input_Rate) then
	            Input_Rate :=  integer (The_Ratebuffer (i));
	         end if;
	      end loop;
	   end;
	end;
--
--	the Maptable and ConvTable are used to convert the inputRate
--	into a 2048000 rate
--	The setup is such that we get input every 2 millisecond input

	for I in Maptable_int' Range loop
	   declare 
	      Integral_Part	: Float := Float' Floor (
	                Float (I) * Float (Input_Rate / 500) / 4096.0);
	   begin
	      Maptable_int (I)          := Integral_Part;
	      Maptable_float (I)	:= 
	                Float (I) * Float (Input_Rate / 500) / 4096.0 -
	                                            Maptable_int (I);
	   end;
	end loop;

	Conversion_Buffer  := new complexArray (0 .. Input_Rate / 500);
	Device_Is_Valid    := true;
	Running            := false;
<<l_end>>
	 null;
end Airspy_Wrapper;

