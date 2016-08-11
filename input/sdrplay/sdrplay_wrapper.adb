with sdrplay_wrapper;
with Text_IO; use Text_IO;
package body sdrplay_wrapper is
mir_sdr_BW_1_536	: constant Integer	:= 1536;
mir_sdr_IF_Zero		: constant Integer	:= 0;
running			: Boolean	:= False;
currentGain		: Integer	:= 40;
vfoFrequency		: Integer	:= 0;

type KindofRequest is (FREQ_CHANGE, GAIN_CHANGE, STOP_CHANGE);
type request is record
	command	: KindofRequest;
	value	: Integer;
end record;
--
--	In order to - dynamically - change settings as gain or frequency
--	we use a simple buffer to store the requests.
type Holder is array (Integer Range <>) of request;
protected type data (Size: Integer) is
	entry Put (Item: in request);
	entry Get (Item: out request);
	function amount return Integer;
	procedure clean;
	private
	   Values:	Holder (1 .. Size);
	   Next_In:	Integer	:= 1;
	   Next_Out:	Integer := 1;
	   Count:	Natural := 0;
end data;
protected body data is
	entry Put (Item : in request) when Count < Size is
	begin
	   Values (Next_In) := Item;
	   Next_In 	:= (Next_In mod Size) + 1;
	   Count	:= Count + 1;
	end Put;

	entry Get (Item: out request) when Count > 0 is
	begin
	   Item	:= Values (Next_Out);
	   Next_Out	:= (Next_Out mod Size) + 1;
	   Count	:= Count - 1;
	end Get;

	function amount return Integer is
	begin
	   return Count;
	end;

	procedure clean is
	begin
	   Next_In		:= 1;
	   Next_Out		:= 1;
	   Count		:= 0;
	end clean;
end data;
changeRequests	: data (10);
--
--	the "worker" will capture the samples and make them
--	into nice complex numbers.
--	minor changes for the frequency and settings of the
--	gain are done within the task. Changes in frequency
--	that require a bandswitch involve a larger operation:
--	i.e. killing the task, and restarting it with the new
--	frequency in the new band.
task body sdrplayWorker is
	deviceRate	: Long_Float	:= 2048000.0;
	bandWidth	: Integer	:= mir_sdr_BW_1_536;
	sps		: Integer;
	err		: Integer;
	mHz_1		: Long_Float	:= 1000000.0;
	HardwareError	: exception;
begin
	changeRequests. clean;
--	Note: the "API check" has been done by the owner of this thread
	err	:= mir_sdr_Init (40,
	                         deviceRate  / mHz_1,
	                         long_Float (frequency) / mHz_1,
	                         mir_sdr_BW_1_536,
	                         mir_sdr_IF_Zero,
	                         sps' Address);
	if err /= 0
	then
	   put ("probleem init"); put_line (err' Image);
	   raise HardwareError;
	else
	  put ("New worker will start with"); put (sps' Image); put_line ("samples");
	end if;
	mir_sdr_SetDcMode (4, 1);
	mir_sdr_SetDcTrackTime (63);
	mir_sdr_SetSyncUpdatePeriod (Integer (deviceRate) / 2);
        mir_sdr_SetSyncUpdateSampleNum (sps);
        mir_sdr_SetParam (102, 1);        -- DC corr
        mir_sdr_SetParam (105, 0);        -- IQ corr
	declare
	   xi	: shortArray (0 .. sps - 1);
	   xq	: shortArray (0 .. sps - 1);
	   fs	: Integer;
	   grc	: Integer;
	   rfc	: Integer;
	   fsc	: Integer;
	   workBuffer	: inputBuffer. buffer_data (0 .. sps - 1);
	begin
	   while running loop		-- running is global here
	   err :=  mir_sdr_ReadPacket (xi' Address,
	                               xq' Address,
	                               fs' Address,
	                               grc' Address,
	                               rfc' Address,
	                               fsc' Address);
	   if err /= 0
	   then
	      put ("error with reading"); put_line (err' Image);
	   end if;
--	currently, we are not interested in the results other than the
--	actual data
	      for i in 0 .. sps - 1 loop
	         workBuffer (i)		:=
	                (Float (xi (i)) / 2048.0,  Float (xq (i)) / 2048.0);
	      end loop;
	      sdrplayBuffer. putDataIntoBuffer (workBuffer);
	      declare
	         theRequest	: request;
	      begin
	         while changeRequests. amount > 0 loop
	            changeRequests. Get (theRequest);
	            case theRequest. Command is
	               when GAIN_CHANGE	=> 
	                  mir_sdr_SetGr (theRequest. Value, 1, 0);
	                  put_line ("Gain request honoured");
	               when FREQ_CHANGE	=>
	                  mir_sdr_SetRf (Long_Float (theRequest. Value), 1, 0);
	                  put ("Frequency set to ");
	                  put_line (Integer' Image (theRequest. value));
	               when Others	=> -- should not happen
	                  running	:= false;
	            end case;
	         end loop;
	      exception
	         when Others => put_line ("It is happening here");
	      end;
	   end loop;
	   mir_sdr_UnInit;
	end;
	exception
	   when Others	=> put_line ("sdrplayWorker Terminated");
end sdrplayWorker;

--	For the sdrplay we use
function	bankFor_sdr (freq	: Integer) return Integer is
begin
	if freq < 12 * MHz (1)
	then
	   return 1;
	elsif freq < 30 * MHz (1)
	then
	   return 2;
	elsif freq < 60 * MHz (1)
	then
	   return 3;
	elsif freq < 120 * MHz (1)
	then
	   return 4;
	elsif freq < 250 * MHz (1)
	then
	   return 5;
	elsif freq < 420 * MHz (1)
	then
	   return 6;
	elsif freq < 1000 * MHz (1)
	then
	   return 7;
	elsif freq < 2000 * MHz (1)
	then
	   return 8;
	else
	   return -1;
	end if;
end bankFor_sdr;

procedure	setVFOFrequency	(newFrequency : Integer)  is
res	: Boolean;
begin
	put ("request for frequency "); put_line (newFrequency' Image);
	if bankFor_sdr (newFrequency) = -1
	then
	   return;
	end if;

	if theWorker = NULL
	then
	   vfoFrequency	:= newFrequency;
	   return;
	end if;

	if bankFor_sdr (newFrequency) /= bankFor_sdr (vfoFrequency)
	then
	   stopReader;
	   vfoFrequency	:= newFrequency;
	   restartReader (res);
	else		-- just a local change
	   changeRequests. Put ((FREQ_CHANGE, newFrequency));
	   vfoFrequency	:= newFrequency;
	end if;
end setVFOFrequency;


procedure	setExternalGain	(newGain : Integer) is
begin
	if newGain < 0 or else newGain > 102
	then
	   return;
	end if;

	if theWorker /= null
	then
	   changeRequests. Put ((GAIN_CHANGE, newGain));
	   currentGain 	:= newGain;
	end if;
end setExternalGain;


procedure	restartReader	(res : out Boolean) is
begin
	if theWorker /= NULL
	then
	   res	:= true;
	   return;
	end if;

	sdrplayBuffer. FlushRingBuffer;
	running		:= true;
	theWorker 	:= new sdrplayWorker (vfoFrequency);
	setExternalGain (currentGain);
	res		:= true;
end restartReader;

procedure	stopReader is
begin
	if theWorker = null
	then
	   return;		-- do not bother
	end if;

	running		:= false;
	while not theWorker.all' Terminated loop
	   delay 0.01;
	end loop;
	Free_theWorker (theWorker);
	theWorker	:= null;
end stopReader;

procedure	getSamples (outV : out complexArray; amount : out Integer) is
begin
	sdrplayBuffer. getDataFromBuffer (inputBuffer. buffer_data (outV),
	                                  amount);
end getSamples;

function 	Samples return Integer is
begin
	return sdrplayBuffer. GetRingBufferReadAvailable;
end Samples;

procedure setupGainTable  (gainSelector: Gtk_Combo_Box_Text) is
begin
	for i in 1 .. 102 loop
	   gainSelector. Insert_text (Glib. Gint (i), i' Image);
	end loop;
end setupGainTable;

function	isValid return Boolean is
begin
	return True;
end isValid;
end sdrplay_wrapper;
