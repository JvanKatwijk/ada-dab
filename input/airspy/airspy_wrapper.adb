
with Gtk.Main;
with Glib;
with airspy_wrapper;
with System. Address_To_Access_Conversions;
with Text_IO; use Text_IO;
package body airspy_wrapper is
package airspy_buffer is new ringBuffer (header. complexTypes. complex);
use header. complexTypes;
use airspy_buffer;
size_2500	: constant Integer 	:= 5000;
size_2048	: constant Integer	:= 4096;

theBuffer		: airspy_buffer. ringBuffer_data (16 * 32768);
-- buffer for transfer to data into the ringbuffer
localBuffer		: airspy_buffer. buffer_data (0 .. size_2048 - 1);
--
--	we have to map 2500000 s/s -> 2048000 s/s, so we take
--	segments of 5000 input samples to 4096 output samples
--	one more than converted
conversionBuffer 	: complexArray (0 .. size_2500);
conversionIndex	:	Integer	:= 0;
---
---
mapTable_int 		: FloatArray	(0 .. size_2048 - 1);
mapTable_float 		: FloatArray	(0 .. size_2048 - 1);
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
valuesRead	: Integer;
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
	      if conversionIndex > size_2500
	      then
	         for j in localBuffer' Range loop	-- i.e. size_2048
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
                 conversionBuffer (0)	:= conversionBuffer (size_2500);
                 conversionIndex 	:= 1;
	      end if;
	   end loop;
	end;
	return 0;
end airspy_Callback;
	
procedure setupGainTable  (gainSelector: Gtk_Combo_Box_Text) is
begin
	for i in 0 .. 15 loop
	   gainSelector. Insert_text (Glib. Gint (i), i' Image);
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
--
--	the maptable and convTable are used to convert the 2500000 rate
--	into a 2048000 rate

	for i in 0 .. size_2048 - 1 loop
	   declare 
	      intPart	: Float := Float' Floor (
	                Float (i) * Float (size_2500) / Float (4096.0));
	   begin
	      mapTable_int (i)		:= intPart;
	      mapTable_float (i)	:= 
	                Float (i) * Float (size_2500) / Float (4096.0) -
	                                         Float (mapTable_int (i));
	   end;
	end loop;

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
	valid	:= true;
	running	:= false;
<<l_end>>
	 null;
end airspy_wrapper;

