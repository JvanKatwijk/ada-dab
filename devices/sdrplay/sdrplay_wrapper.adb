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
	HardwareError     : exception;
	inputRate         : constant Integer               := 2048000;
	mir_sdr_BW_1_536  : constant Interfaces. C. int    := 1536;
	mir_sdr_IF_Zero   : constant Interfaces. C. int    := 0;
	bandWidth         : constant Interfaces. C. int    := 
	                                                   mir_sdr_BW_1_536;
	mHz_1             : constant Interfaces. C. double := 1000000.0;

--
	package environmentConverter is
              new System. Address_to_Access_Conversions (sdrplay_device);

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
	   type shortArray is Array (0 .. Integer (numSamples) - 1) of int16_t;
	   package arrayConverter is
	         new System. Address_To_Access_Conversions (shortArray);
	   xi_buffer      : arrayConverter. Object_Pointer :=
                                arrayConverter. To_Pointer (xi. all' Address);
	   xq_buffer      : arrayConverter. Object_Pointer :=
                                arrayConverter. To_Pointer (xq. all' Address);
	   localEnv       : environmentConverter. Object_Pointer :=
	                        environmentConverter. To_Pointer (userData);
	   collect_Buffer : sdrplay_Buffer. buffer_data (0 .. Integer (numSamples) - 1);
	begin
	   for I in collect_Buffer' Range loop
	      collect_Buffer (I) := (Float (xi_buffer (I)) / 2048.0,
	                             Float (xq_buffer (I)) / 2048.0);
	   end loop;
	   localEnv. The_Buffer. putDataIntoBuffer (collect_Buffer);
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
	procedure Restart_Reader (Object : in out sdrplay_Device;
	                          Success : out Boolean) is
	   sps         : Interfaces. C. int;
	   gRdBSystem  : Interfaces. C. int := 0;
	   agcMode     : Interfaces. C. int := 0;
	   err         : Interfaces. C. int;
	   localGain   : Interfaces. C. int := 20;
	begin
	   if Object. Running then
	      Success  := true;
	      return;
	   end if;

	   Success     := false;	-- just a default
	   Object. The_Buffer. FlushRingBuffer;
	   
--	Note: the "API check" has been done by the owner of this thread
	   err   := mir_sdr_StreamInit (Interfaces. C. int (localGain),
	                                Interfaces. C. double (inputRate)  / mHz_1,
	                                Interfaces. C. double (Object. VFO_Frequency) / mHz_1,
	                                mir_sdr_BW_1_536,
	                                mir_sdr_IF_Zero,
	                                0,
	                                gRdBSystem,
	                                agcMode,
	                                sps,
	                                Sdrplay_Callback' Access,
	                                Sdrplay_Gain_Callback' Access,
	                                Object' Address);
	   if err /= 0 then
	      put ("probleem init"); put_line (Integer' Image (Integer (err)));
	      raise HardwareError;
	   end if;
	   err := mir_sdr_SetDcMode (4, 1);
	   err := mir_sdr_SetDcTrackTime (63);
	   err := mir_sdr_SetSyncUpdatePeriod (Interfaces. C. int (inputRate) / 4);
           err := mir_sdr_SetSyncUpdateSampleNum (Interfaces. C. int (sps));
           err := mir_sdr_DCoffsetIQimbalanceControl (0, 1);
	   Object. Running  := true;
	   Success  := true;
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

	procedure Set_VFOFrequency (Object        : in out sdrplay_Device;
	                            New_Frequency : Natural) is
	   res : Boolean;
	   err : Interfaces. C. int;
	begin
	   if Frequency_Banks (New_Frequency) = -1 then   -- illegal
	      return;
	   end if;
--
	   if Frequency_Banks (New_Frequency) /=
	              Frequency_Banks (Object. VFO_Frequency) then
	      Stop_Reader (Object);
	      Object. VFO_Frequency   := New_Frequency;
	      Restart_Reader (Object, res);
	   else
 	      err              := mir_sdr_SetRf (double (New_Frequency), 1, 0);
	      Object. VFO_Frequency	:= New_Frequency;
	   end if;
	end Set_VFOFrequency;

	procedure Set_Gain (Object : in out sdrplay_device;
	                    New_Gain : Natural) is
	   err : Interfaces. C. int;
	begin
	   if New_Gain < 0 or else New_Gain >= 102 then
	      return;
	   end if;
	   Object. Current_Gain := New_Gain;
	   err := mir_sdr_SetGr (Interfaces. C. int (Object. Current_Gain), 1, 0);
	end Set_Gain;

	procedure Stop_Reader (Object : in out sdrplay_device) is
	begin
	   if not Object. Running then
	      return;	     -- do not bother
	   end if;
	   mir_sdr_StreamUninit;
	   Object. Running     := false;
	end Stop_Reader;

	procedure Get_Samples (Object : in out sdrplay_Device;
	                       Out_V  : out complexArray;
	                       Amount : out Natural) is
	begin
	   Object. The_Buffer.
	          getDataFromBuffer (sdrplay_Buffer. buffer_data (Out_V), amount);
	end Get_Samples;

	function Available_Samples (Object : sdrplay_Device) return Natural is
	begin
	   return Object. The_Buffer. GetRingBufferReadAvailable;
	end Available_Samples;

	function Valid_Device (Object : sdrplay_Device) return Boolean is
	begin
	   return Object. isValid;
	end Valid_Device;

	subtype C_string is char_array (0 .. 10);
	type stringAccess is access C_string;
	type mir_sdr_DeviceT is record
	   SerNo    : stringAccess;
	   DevNm    : stringAccess;
	   hwVer    : uint8_t;
	   devAvail : uint8_t;
	end record;
	Pragma Convention (C, mir_sdr_DeviceT);
	type xxx is array (Natural Range <>) of mir_sdr_DeviceT;
	subtype devDescriptors is xxx (0 .. 6);
	procedure Initialize (Object : in out sdrplay_Device) is
	   err : Interfaces. C. int;
	   devDesc : devDescriptors;
	   procedure mir_sdr_GetDevices (devDesc : out devDescriptors;
	                                 numofDevs : out Integer;
	                                 amount    : integer);
	   Pragma Import (C, mir_sdr_GetDevices, "mir_sdr_GetDevices");
	   procedure mir_sdr_SetDeviceIdx (deviceIndex : Integer);
	   Pragma Import (C, mir_sdr_SetDeviceIdx, "mir_sdr_SetDeviceIdx");
	   procedure mir_sdr_DebugEnable;
	   Pragma Import (C, mir_sdr_DebugEnable, "mir_sdr_DebugEnable");
	   procedure mir_sdr_SetGr (gRdB : integer;
	                            absol : integer; syncUpdate : integer);
	   pragma Import (C, mir_sdr_SetGr, "mir_sdr_SetGr");
	   numofDevs : Integer;
	begin
	   Object. isValid := false;
	   err	:= mir_sdr_ApiVersion (Object. Library_Version);
	   if err /= 0 then
	      put_line ("Error in querying library");
	   elsif Object. Library_Version < 2.05 then
	      put ("Library version too old");
	   else
	      put ("Library version ");
	      put_line (Float' Image (Float (Object. Library_Version)));
	   end if;

	   mir_sdr_DebugEnable;
	   mir_sdr_GetDevices (devDesc, numofDevs, 4);
	   put ("Gevonden "); put (Integer' Image (numofDevs));
	   put_line (" devices");
	   declare
	      Serno : String := To_Ada (devDesc (0). Serno. all, true);
	   begin
	      put_line (Serno);
	   end;
	   put_line (uint8_t' Image (devDesc (0). devAvail));
	   mir_sdr_SetDeviceIdx (0);
	   mir_sdr_SetGr (20, 1, 1);
	   Object. isValid := true;
	   Object. VFO_Frequency := 200000000;
	end;

	procedure Finalize (Object : in out sdrplay_Device) is
	   procedure mir_sdr_ReleaseDeviceIdx (deviceIndex : Integer);
	   Pragma Import (C, mir_sdr_ReleaseDeviceIdx,
	                            "mir_sdr_ReleaseDeviceIdx");
	begin
	   mir_sdr_ReleaseDeviceIdx (0);
	end;
end sdrplay_wrapper;
