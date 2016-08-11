with System. Address_to_Access_Conversions;
with Ada. Unchecked_Deallocation;
with glib; use glib;
with Text_IO; use Text_IO;
package body rtlsdr_wrapper is
subtype localBufferType is
	         rtlsdr_buffer. buffer_data (0 .. READLEN_DEFAULT - 1);
package bufferConverter is
	         new System. Address_To_Access_Conversions (localBufferType);
package arrayConverter is
	         new System. Address_To_Access_Conversions (localBufferType);
procedure Free_Handler is new Ada. Unchecked_Deallocation (
	Object	=> rtlsdr_reader, Name	=> rtlsdr_reader_P);

deviceCount	: Integer;
deviceIndex	: Integer;
r		: Integer;
--	set up
--
--
--	This callback will be called 2048000 / 8192 (i.e. app 250)
--	times a second
procedure rtlsdr_Callback (buffer	: System. Address;
	                   size		: Integer;
	                   userData	: System. Address) is
-- 
--      mybuffer is actually a C array, provided for by the underlying
--      osmocom library
my_buffer        : arrayConverter. Object_Pointer :=
                                arrayConverter. To_Pointer (buffer);
begin
	if size /= READLEN_DEFAULT
	then
	   return;
	end if;
	theBuffer. putDataIntoBuffer (my_buffer. all);
end rtlsdr_Callback;

task body rtlsdr_reader is
begin
	rtlsdr_read_async_ada (device. all,				-- 
	                       rtlsdr_Callback' access,
	                       null_Address,		-- user Data
	                       0,
	                       READLEN_DEFAULT);
	put ("end of task reached");
end rtlsdr_reader;

procedure setVFOFrequency (frequency: Integer) is
begin
	lastFrequency	:= frequency;
	rtlsdr_set_center_freq_ada (device. all,
	                            frequency + vfoOffset);
end setVFOFrequency;

function getVFOFrequency return Integer is
begin
	return rtlsdr_get_center_freq_ada (device. all) - vfoOffset;
end getVFOFrequency;

procedure restartReader (result: out Boolean) is
r:	Integer;
begin
	if workerHandle /= null	-- running already
	then
	   result	:= true;
	   return;
	end if;

	theBuffer. FlushRingBuffer;
	r	:= rtlsdr_reset_buffer_ada (device. all);
	if r < 0
	then
	   result	:= false;
	   return;
	end if;

	rtlsdr_set_center_freq_ada (device. all, lastFrequency + vfoOffset);
	workerHandle := new rtlsdr_reader;
	result := true;
end restartReader;

procedure stopReader is
begin
	if workerHandle = null
	then
	   return;
	end if;

	rtlsdr_cancel_async_ada (device. all);
	delay 1.0;
	if workerHandle. all 'Terminated
	then
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
end stopReader;

procedure setExternalGain (gain : Integer) is
begin
	rtlsdr_set_tuner_gain_ada (device. all, gain);
	theGain	:= gain;
end setExternalGain;
--
--	Note that we get uint8_t's in and send complex(float)'s out
procedure getSamples (outV : out complexArray; amount : out Integer) is
tempBuffer: rtlsdr_buffer. buffer_data (0 .. 2 * outV' length - 1);
res	: Integer;
begin
	theBuffer. getDataFromBuffer (tempBuffer, res);
	res := res / 2;
--
--	Now we are talking pairs
	for i in 0 .. res - 1 loop
	   outV (i) := (float (Integer (tempBuffer (2 * i)) - 128) / 128.0,
	                float (Integer (tempBuffer (2 * i + 1)) - 128) / 128.0);
	end loop;
	amount	:= res;
end getSamples;

function Samples return Integer is
begin
	return theBuffer. GetRingBufferReadAvailable / 2;
end Samples;

function isValid return Boolean is
begin
	return valid;
end isValid;

procedure setupGainTable (gainSelector : Gtk_Combo_Box_Text) is
begin
	for i in 0 .. gainsCount - 1 loop
	   gainSelector. Insert_Text (Gint (i),
	                         Integer' Image (gains (i)));
	end loop;
end;

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
	r		:= rtlsdr_open_ada (device, deviceIndex);
	if r < 0
	then
	   put ( "could not open the device"); New_Line (1);
	end if;

	valid		:= true;
	rtlsdr_set_sample_rate_ada (device. all, inputRate);
	r		:= rtlsdr_get_sample_rate_ada (device. all);
	put (" samplerate set to "); put (r' image); New_Line (1);

	gainsCount	:= rtlsdr_get_tuner_gains_ada (device. all, 0);
	put (gainsCount' image); put (" gain values supported");
	New_Line (1);
	gains		:= new intArray (0 .. gainsCount - 1);
	rtlsdr_get_tuner_gains_ada (device. all, gains. all' Address);
--
--	will be overruled:
	rtlsdr_set_tuner_gain_mode_ada (device. all, 1);
	theGain		:= gainsCount / 2;
	rtlsdr_set_tuner_gain_ada      (device. all, gains (theGain));
	for i in 0 .. gainsCount - 1 loop
	   declare 
	      tmp : float	:= float (gains (i)) / 10.0;
	   begin
	      put (tmp' image);
	   end;
	end loop;
	New_Line (1);
<<Error>>
	null;
end rtlsdr_wrapper;

