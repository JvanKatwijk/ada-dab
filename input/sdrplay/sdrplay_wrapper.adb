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
with System. Address_To_Access_Conversions;

package body sdrplay_wrapper is
	mir_sdr_BW_1_536  : constant Interfaces. C. int    := 1536;
	mir_sdr_IF_Zero   : constant Interfaces. C. int    := 0;
	bandWidth         : constant Interfaces. C. int    := mir_sdr_BW_1_536;
	mHz_1             : constant Interfaces. C. double := 1000000.0;
	Running           : Boolean                        := False;
	Current_Gain      : Integer                        := 40;
	VFO_Frequency     : Integer                        := 227000000;
	err               : Interfaces. C. int;
	HardwareError     : exception;
	Library_Version   : Interfaces. C. c_float;
--
--	We prefer a callback not to reference anything not local
--	or accessible through the parameters. So, we pass
--	the ringbuffer as "userData" parameter.
	procedure Sdrplay_Callback (xi             : access Interfaces.C. short;
	                            xq             : access Interfaces.C. short;
	                            firstSampleNum : Interfaces. C. int;
	                            grChanged      : Interfaces. C. int;
	                            rfChanged      : Interfaces. C. int;
	                            fsChanged      : Interfaces. C. int;
	                            numSamples     : Interfaces. C. unsigned;
	                            reset          : Interfaces. C. unsigned;
	                            userData       : system. Address) is
--      xi_buffer and xq_buffer are actually  C arrays,
--	provided for by the underlying sdrplay library
--	We convert them to Ada-like arrays by an "arrayConverter"
	   type shortArray is Array (0 .. Integer (numSamples) - 1) of short_Integer;
	   package arrayConverter is
	         new System. Address_To_Access_Conversions (shortArray);
	   package bufferConverter is
	         new System. Address_To_Access_Conversions (localBuffer);
	   xi_buffer      : arrayConverter. Object_Pointer :=
                                arrayConverter. To_Pointer (xi. all' Address);
	   xq_buffer      : arrayConverter. Object_Pointer :=
                                arrayConverter. To_Pointer (xq. all' Address);
	   output_Buffer  : bufferConverter. Object_Pointer :=
	                        bufferConverter. To_Pointer (userData);
	   collect_Buffer : inputBuffer. buffer_Data (0 .. Integer (numSamples) - 1);
	begin
	   for I in collect_Buffer' Range loop
	      collect_Buffer (I) := (Float (xi_buffer (I)) / 2048.0,
	                             Float (xq_buffer (I)) / 2048.0);
	   end loop;
	   output_Buffer. putDataIntoBuffer (collect_Buffer);
	end Sdrplay_Callback;
--
--	The gain change callback is not used here
	procedure  Sdrplay_Gain_Callback (gRdB      : Interfaces. C. unsigned;
	                                  lnsGRdB   : Interfaces. C. unsigned;
	                                  userData  : system. Address) is
	begin
	   null;
	end;
--
--
	procedure Restart_Reader (Success : out Boolean) is
	   sps         : Interfaces. C. int;
	   gRdBSystem  : Interfaces. C. int;
	   agcMode     : Interfaces. C. int := 0;
	begin
	   if Running then
	      Success  := true;
	      return;
	   end if;

	   Success     := false;	-- just a default
	   sdrplayBuffer. FlushRingBuffer;
	   
--	Note: the "API check" has been done by the owner of this thread
	   err   := mir_sdr_StreamInit (Interfaces. C. int (Current_Gain),
	                                Interfaces. C. double (inputRate)  / mHz_1,
	                                Interfaces. C. double (VFO_Frequency) / mHz_1,
	                                mir_sdr_BW_1_536,
	                                mir_sdr_IF_Zero,
	                                0,
	                                gRdBSystem,
	                                agcMode,
	                                sps,
	                                Sdrplay_Callback' Access,
	                                Sdrplay_Gain_Callback' Access,
	                                sdrplayBuffer' Address);
	   if err /= 0 then
	      put ("probleem init"); put_line (Integer' Image (Integer (err)));
	      raise HardwareError;
	   end if;
	   err := mir_sdr_SetDcMode (4, 1);
	   err := mir_sdr_SetDcTrackTime (63);
	   err := mir_sdr_SetSyncUpdatePeriod (Interfaces. C. int (inputRate) / 4);
           err := mir_sdr_SetSyncUpdateSampleNum (Interfaces. C. int (sps));
           err := mir_sdr_DCoffsetIQimbalanceControl (0, 1);
	   Running          := true;
	   Success          := true;
	end Restart_Reader;

--	The SDRplay requires that a changing a frequency to
--	one in a different band requires Uninit - Init.
--	The bands are defined here as a local function
	function Frequency_Banks (Frequency: Integer) return Integer is
	begin
	   if Frequency < 12 * MHz (1) then
	      return 1;
	   elsif Frequency < 30 * MHz (1) then
	      return 2;
	   elsif Frequency < 60 * MHz (1) then
	      return 3;
	   elsif Frequency < 120 * MHz (1) then
	      return 4;
	   elsif Frequency < 250 * MHz (1) then
	      return 5;
	   elsif Frequency < 420 * MHz (1) then
	      return 6;
	   elsif Frequency < 1000 * MHz (1) then
	      return 7;
	   elsif Frequency < 2000 * MHz (1) then
	      return 8;
	   else
	      return -1;
	   end if;
	end Frequency_Banks;

	procedure Set_VFOFrequency (New_Frequency : Integer) is
	   res:   Boolean;
	begin
	   if Frequency_Banks (New_Frequency) = -1 then   -- illegal
	      return;
	   end if;
--
	   if Frequency_Banks (New_Frequency) /=
	              Frequency_Banks (VFO_Frequency) then
	      Stop_Reader;
	      VFO_Frequency   := New_Frequency;
	      Restart_Reader (res);
	   else
 	      err := mir_sdr_SetRf (double (New_Frequency), 1, 0);
	      VFO_Frequency	:= New_Frequency;
	   end if;
	end Set_VFOFrequency;

	procedure Set_Gain (New_Gain : Integer) is
	begin
	   if New_Gain < 0 or else New_Gain >= 102 then
	      return;
	   end if;
	   Current_Gain := New_Gain;
	   err := mir_sdr_SetGr (Interfaces. C. int (Current_Gain), 1, 0);
	end Set_Gain;

	procedure Stop_Reader is
	begin
	   if not Running then
	      return;	     -- do not bother
	   end if;
	   mir_sdr_StreamUninit;
	   Running     := false;
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
begin
	err	:= mir_sdr_ApiVersion (Library_Version);
	if err /= 0 then
	   put_line ("Error in querying library");
	   raise  HardwareError;
	else
	   put ("Library version ");
	   put_line (Float' Image (Float (Library_Version)));
	end if;
end sdrplay_wrapper;
