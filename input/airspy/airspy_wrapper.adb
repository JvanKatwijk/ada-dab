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
with airspy_wrapper;
with System. Address_To_Access_Conversions;
with Text_IO; use Text_IO;
package body airspy_wrapper is
package airspy_buffer is new ringBuffer (header. complexTypes. complex);
use header. complexTypes;
use airspy_buffer;
inputRate		: Integer	:= 2500000;	-- default
theBuffer		: airspy_buffer. ringBuffer_data (16 * 32768);
-- buffer for transfer to data into the ringbuffer
localBuffer		: airspy_buffer. buffer_data (0 .. 2048000 / 500 - 1);
--
conversionBuffer 	: complexArray_P;
conversionIndex	:	Integer	:= 0;
---
---
mapTable_int 		: FloatArray	(0 .. 2048000 / 500 - 1);
mapTable_float 		: FloatArray	(0 .. 2048000 / 500 - 1);
--
--
result	: airspyError;
--
--	abstract away from the hardware
device_P	: access System. Address	:= new System. Address;
--
--
running		: Boolean;
Valid		: Boolean;
vgaGain		: Integer;
mixerGain	: Integer;
lnaGain		: Integer;
lastFrequency	: Integer	:= 225000000;	-- dummy

procedure setVFOFrequency	(frequency	: Integer) is
result	: airspyError;
begin
	result	:= airspy_set_freq (device_P. all, frequency);
	if result /= AIRSPY_SUCCESS
	then
	   put_line ("airspy_set_freq failed");
	else
	   lastFrequency	:= frequency;
	end if;
end setVFOFrequency;

procedure restartReader (res	: out Boolean) is
result	: airspyError;
begin
	res		:= false;	-- default, unless ....
	if not valid
	then
	   return;
	end if;
	if running
	then
	   res		:= true;
	   return;
	end if;
	theBuffer. FlushRingBuffer;
	result		:= airspy_set_sample_type (device_P. all,
	                                              AIRSPY_SAMPLE_FLOAT32_IQ);
	if result /= AIRSPY_SUCCESS
	then
	   put_line ("airspy_set_sample_type failed");
	   return;
	end if;
--
--	we know that "1" stands for 2.5Msamples/second
	result	:= airspy_set_samplerate (device_P. all, 1);
	if result /= AIRSPY_SUCCESS
	then
	   put_line ("airspy_set_samplerate failed");
	   return;
	end if;
--
--	we assume the next three settings work fine
	airspy_set_vga_gain		(device_P. all, vgaGain);
	airspy_set_mixer_gain		(device_P. all, mixerGain);
	airspy_set_lna_gain		(device_P. all, lnaGain);
--
--	since the context is "global" data in the package, we do not need
--	a context
	result 	:= airspy_start_rx		(device_P. all,
                                                 airspy_Callback' Access, 
	                                         null_Address);
	if result /= AIRSPY_SUCCESS
	then
	   put_Line ("airspy_start_rx failed");
	   return;
	end if;
	running	:= true;
	res	:= true;
end restartReader;

procedure stopReader is
begin
	if not running
	then
	   return;
	end if;
	put ("we stoppen");
	airspy_stop_rx (device_P. all);
	running		:= false;
end stopReader;
--
--
function airspy_Callback (transfer : airspy_transfer_P) return Integer is
begin
	if transfer = null
	then
	   return 0;
	end if;
--	we do the rate conversion "in-line"
	declare
	   subtype C_buffer is floatArray (0 .. transfer. sampleCount * 2 - 1);
	   package arrayConverter is
	         new System. Address_To_Access_Conversions (C_buffer);
	   my_buffer        : arrayConverter. Object_Pointer :=
                                arrayConverter. To_Pointer (transfer. samples);
--	my_buffer is now the ada name for the incoming data
	begin
	   for i in 0 .. transfer. sampleCount - 1 loop
	      conversionBuffer (conversionIndex) :=
	                      (my_buffer (2 * i), my_buffer (2 * i + 1));
	      conversionIndex := conversionIndex + 1;
	      if conversionIndex > inputRate / 500
	      then
	         for j in localBuffer' Range loop	-- i.e. 2048000 / 500
	            declare
	               inpBase	: Float		:= mapTable_int (j);
	               inpRatio	: Float		:= mapTable_float (j);
	               index	: Integer	:= Integer (inpBase);
	            begin
	               localBuffer (j). Re	:=
	                      conversionBuffer (index + 1). Re *  inpRatio +
	                      conversionBuffer (index). Re *
	                                    (1.0 -  inpRatio);
	               localBuffer (j). Im	:=
	                      conversionBuffer (index + 1). Im *  inpRatio +
	                      conversionBuffer (index). Im *
	                                    (1.0 -  inpRatio);
	            end;
	         end loop;
	         theBuffer. putDataIntoBuffer (localBuffer);
--
--	shift the sample at the end to the beginning, it is needed
--	as the starting sample for the next time
                 conversionBuffer (0)	:= conversionBuffer (inputRate / 500);
                 conversionIndex 	:= 1;
	      end if;
	   end loop;
	end;
	return 0;
end airspy_Callback;
	
procedure setupGainTable  (gainSelector: Gtk_Combo_Box_Text) is
begin
	for i in 0 .. 15 loop
	   gainSelector. Insert_text (Glib. Gint (i), Integer' Image (i));
	end loop;
end setupGainTable;
--
--	we use the gain setting for setting the sensitivity
procedure setExternalGain (gain : Integer) is
begin
	mixerGain	:= gain;
	vgaGain		:= (if gain >= 5 then mixerGain - 5 else mixerGain);
	lnaGain		:= (if gain >= 5 then mixerGain - 5 else mixerGain);
	airspy_set_vga_gain		(device_P. all, vgaGain);
	airspy_set_mixer_gain		(device_P. all, mixerGain);
	airspy_set_lna_gain		(device_P. all, lnaGain);
--	airspy_set_sensitivity (device_P. all, gain);
end setExternalGain;

procedure getSamples	(outV : out complexArray; amount : out Integer) is
begin
	theBuffer. getDataFromBuffer (buffer_data (outV), amount);
end getSamples;
--
--
function Samples           return Integer is
begin
	return theBuffer. GetRingBufferReadAvailable;
end Samples;
--
--
function isValid	      return Boolean is
begin
	return Valid;
end isValid;
--
begin
	vgaGain		:= 11;
	mixerGain	:= 15;
	lnaGain		:= 11;
	running		:= False;
	Valid		:= False;
	put_line ("Klaar voor init");
	result		:= airspy_init;
	if result /= AIRSPY_SUCCESS
	then
	   put_line ("airspy_init failed");
	   goto l_end;
	end if;
	put_line ("hier");
	result	:= airspy_open (device_P);
	if result /= AIRSPY_SUCCESS
	then
	   put_line ("airspy_open failed");
	   goto l_end;
	end if;

	inputRate	:= 10000000;
	declare
	   rateCount	: Integer	:= 0;
	   the_rateBuffer	: rateBuffer;
	begin
	   airspy_get_samplerates (device_P. all, the_rateBuffer, 0);
	   rateCount	:= the_rateBuffer (0);
	   airspy_get_samplerates (device_P. all, the_rateBuffer, rateCount);

	   for i in 0 .. rateCount - 1 loop
	      if the_rateBuffer (i) > 2048000 and then
	         the_rateBuffer (i) < inputRate
	      then
	         inputRate := the_rateBuffer (i);
	      end if;
	   end loop;
	end;
--
--	the maptable and convTable are used to convert the inputRate
--	into a 2048000 rate
--	It is known that we take every 2 millisecond input

	for i in mapTable_int' Range loop
	   declare 
	      intPart	: Float := Float' Floor (
	                Float (i) * Float (inputRate / 500) / 4096.0);
	   begin
	      mapTable_int (i)		:= intPart;
	      mapTable_float (i)	:= 
	                Float (i) * Float (inputRate / 500) / 4096.0 -
	                                         Float (mapTable_int (i));
	   end;
	end loop;

	conversionBuffer	:= new complexArray (0 .. inputRate / 500);
	valid	:= true;
	running	:= false;
<<l_end>>
	 null;
end airspy_wrapper;

