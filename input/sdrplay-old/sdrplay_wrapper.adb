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
with Text_IO; use Text_IO;
package body sdrplay_wrapper is
	mir_sdr_BW_1_536  : constant Interfaces. C. int   := 1536;
	mir_sdr_IF_Zero   : constant Interfaces. C. int   := 0;
	Running           : Boolean                       := False;
	Current_Gain      : Integer                       := 40;
	VFO_Frequency     : Integer                       := 227000000;

	type KindofRequest is (FREQ_CHANGE, GAIN_CHANGE, STOP_CHANGE);
	type request is
	   record
	      command:   KindofRequest;
	      value:     Interfaces. C. int;
	   end record;
--
--	In order to - dynamically - change settings as gain or frequency
--	we use a simple buffer to store the requests.
	type Holder is array (Integer Range <>) of request;
	protected type data (Size: Integer) is
	   entry Put (Item  : request);
	   entry Get (Item  : out request);
	   function amount return Integer;
	   procedure clean;
	private
	   Values   : Holder (1 .. Size);
	   Next_In  : Integer   := 1;
	   Next_Out : Integer   := 1;
	   Count    : Natural   := 0;
	end data;

	protected body data is
	   entry Put (Item : in request) when Count < Size is
	   begin
	      Values (Next_In) := Item;
	      Next_In          := (Next_In mod Size) + 1;
	      Count            := Count + 1;
	   end Put;

	   entry Get (Item: out request) when Count > 0 is
	   begin
	      Item             := Values (Next_Out);
	      Next_Out         := (Next_Out mod Size) + 1;
	      Count            := Count - 1;
	   end Get;

	   function amount return Integer is
	   begin
	      return Count;
	   end;

	   procedure clean is
	   begin
	      Next_In          := 1;
	      Next_Out         := 1;
	      Count            := 0;
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
	   deviceRate    : Interfaces. C. double  := 2048000.0;
	   bandWidth     : Interfaces. C. int     := mir_sdr_BW_1_536;
	   sps           : Interfaces. C. int;
	   err           : Interfaces. C. int;
	   mHz_1         : Interfaces. C. double  := 1000000.0;
	   HardwareError : exception;
	begin
	   changeRequests. clean;
--	Note: the "API check" has been done by the owner of this thread
	   err   := mir_sdr_Init (Interfaces. C. int (Gain),
	                          DeviceRate  / mHz_1,
	                          Interfaces. C. double (frequency) / mHz_1,
	                          mir_sdr_BW_1_536,
	                          mir_sdr_IF_Zero,
	                          sps);
	   if err /= 0 then
	      put ("probleem init"); put_line (Integer' Image (Integer (err)));
	      raise HardwareError;
	   end if;
	   mir_sdr_SetDcMode (4, 1);
	   mir_sdr_SetDcTrackTime (63);
	   mir_sdr_SetSyncUpdatePeriod (Interfaces. C. int (deviceRate) / 2);
           mir_sdr_SetSyncUpdateSampleNum (Interfaces. C. int (sps));
           mir_sdr_SetParam (102, 1);        -- DC corr
           mir_sdr_SetParam (105, 0);        -- IQ corr
	   declare
--	It is only after having processed the Init that we can build
--	the proper subtype for the buffers, a subtype we need in the
--	specification of the ReadPacket function
	      type Sdrplay_Buffer is
                 array (Integer range 0 .. Integer (sps) - 1) of
	                                          Interfaces. C. short;
	      xi:     Sdrplay_Buffer;
	      xq:     Sdrplay_Buffer;
	      function mir_sdr_readPacket (xi        : out Sdrplay_Buffer;
	                                   xq        : out Sdrplay_Buffer;
	                                   fsn       : out Interfaces. C. int;
	                                   grChanged : out Interfaces. C. int;
	                                   rfChanged : out Interfaces. C. int;
	                                   fsChanged : out Interfaces. C. int)
	                                        return Interfaces. C. int;
	      pragma Import (C, mir_sdr_readPacket, "mir_sdr_ReadPacket");
	      fs         : Interfaces. C. int;
	      grc        : Interfaces. C. int;
	      rfc        : Interfaces. C. int;
	      fsc        : Interfaces. C. int;
	      workBuffer : inputBuffer. buffer_data (0 .. Integer (sps) - 1);
	      counter	: Integer	:= 0;
	   begin
	      while Running loop           -- running is global here
	         err :=  mir_sdr_ReadPacket (xi,
	                                     xq,
	                                     fs,
	                                     grc,
	                                     rfc,
	                                     fsc);
	         if err /= 0 then
	            put ("error with reading");
	            put_line (Integer' Image (Integer (err)));
	         else
--	currently, we are not interested in the results other than the
--	actual data
	            for I in 0 .. Integer (sps) - 1 loop
	               workBuffer (i) :=
	                         (Float (xi (I)) / 2048.0,
	                          Float (xq (I)) / 2048.0);
	            end loop;
	            sdrplayBuffer. putDataIntoBuffer (workBuffer);
	         end if;
	         declare
	            The_Request: request;
	         begin
	            while changeRequests. amount > 0 loop
	               changeRequests. Get (The_Request);
	               case The_Request. Command is
	                  when GAIN_CHANGE	=> 
	                     mir_sdr_SetGr (The_Request. Value, 1, 0);
	                  when FREQ_CHANGE	=>
	                     mir_sdr_SetRf (
	                            Interfaces. C. double (The_Request. Value),
                                            1, 0);
	                  when Others	=> -- should not happen
	                     put_line ("Illegal command caught");
	                     Running	:= false;
	               end case;
	            end loop;
	         end;
	         Counter	:= Counter + 1;
	         if Counter *  Integer (sps) > 2048000 then
--	            put_line ("weer een tel");
	            Counter := 0;
	         end if;
	      end loop;
	      mir_sdr_UnInit;
	   end;
	   exception
	      when Others	=> put_line ("sdrplayWorker Terminated");
	end sdrplayWorker;

--	For the sdrplay we use
	function bankFor_sdr (freq: Integer) return Integer is
	begin
	   if freq < 12 * MHz (1) then
	      return 1;
	   elsif freq < 30 * MHz (1) then
	      return 2;
	   elsif freq < 60 * MHz (1) then
	      return 3;
	   elsif freq < 120 * MHz (1) then
	      return 4;
	   elsif freq < 250 * MHz (1) then
	      return 5;
	   elsif freq < 420 * MHz (1) then
	      return 6;
	   elsif freq < 1000 * MHz (1) then
	      return 7;
	   elsif freq < 2000 * MHz (1) then
	      return 8;
	   else
	      return -1;
	   end if;
	end bankFor_sdr;

	procedure Set_VFOFrequency (New_Frequency : Integer) is
	   res:   Boolean;
	begin
	   if bankFor_sdr (New_Frequency) = -1 then   -- illegal
	      return;
	   end if;

	   if Our_Worker = NULL then         -- wait 
	      VFO_Frequency := New_Frequency;
	      return;
	   end if;

	   if bankFor_sdr (New_Frequency) /= bankFor_sdr (VFO_Frequency) then
	      Stop_Reader;
	      VFO_Frequency   := New_Frequency;
	      Restart_Reader (res);
	   else		-- just a local change
	      changeRequests. Put ((FREQ_CHANGE,
	                                 Interfaces. C. int (New_Frequency)));
	      VFO_Frequency	:= New_Frequency;
	   end if;
	end Set_VFOFrequency;

	procedure Set_Gain (New_Gain : Integer) is
	begin
	   if New_Gain < 0 or else New_Gain >= 102 then
	      return;
	   end if;

	   if Our_Worker /= null then
	      changeRequests. Put ((GAIN_CHANGE,
	                                   Interfaces. C. int (New_Gain)));
	      Current_Gain  := New_Gain;
	   end if;
	end Set_Gain;

	procedure Restart_Reader (Success: out Boolean) is
	begin
	   if Our_Worker /= NULL then
	      Success      := true;
	      return;
	   end if;

	   sdrplayBuffer. FlushRingBuffer;
	   Running     := true;
	   
	   Our_Worker   := new sdrplayWorker (VFO_Frequency, Current_Gain);
	   Success         := true;
	end Restart_Reader;

	procedure Stop_Reader is
	begin
	   if Our_Worker = null then
	      return;	     -- do not bother
	   end if;

	   Running     := false;
	   while not Our_Worker.all' Terminated loop
	      delay 0.01;
	   end loop;
	   Free_Our_Worker (Our_Worker);
	   Our_Worker   := null;
	end Stop_Reader;

	procedure Get_Samples (Out_V  : out complexArray;
	                       Amount : out Integer) is
	begin
	   sdrplayBuffer.
	          getDataFromBuffer (inputBuffer. buffer_data (Out_V), amount);
	end Get_Samples;

	function Available_Samples return Integer is
	begin
	   return sdrplayBuffer. GetRingBufferReadAvailable;
	end Available_Samples;

	procedure Setup_Gaintable  (gainSelector : Gtk_Combo_Box_Text) is
	begin
	   for i in 1 .. 102 loop
	      gainSelector. Insert_text (Glib. Gint (i), Integer' Image (i));
	   end loop;
	end Setup_GainTable;

	function Valid_Device return Boolean is
	begin
	   return True;
	end Valid_Device;
end sdrplay_wrapper;
