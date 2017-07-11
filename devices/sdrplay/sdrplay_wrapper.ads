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
with device_handler;            use device_handler;
with System;                    use System;
with header;                    use header;
with Interfaces;                use Interfaces;
with Interfaces.C;              use Interfaces. C;
with ringbuffer;
with ringbuffer;
with Ada. Unchecked_Deallocation;

package sdrplay_wrapper is
	use header. complexTypes;
	type sdrplay_device is new device with private;
	type sdrplay_device_p is access all sdrplay_device;

	overriding
	procedure Restart_Reader   (Object       : in out sdrplay_device;
	                            Success      : out Boolean);
        procedure Stop_Reader      (Object       : in out sdrplay_device);
        procedure Set_VFOFrequency (Object       : in out sdrplay_device;
	                            New_Frequency: Natural);
        procedure Set_Gain         (Object       : in out sdrplay_device;
	                            New_Gain     : Natural);
        procedure Get_Samples      (Object       : in out sdrplay_device;
	                            Out_V        : out complexArray;
                                    Amount       : out Natural);
        function Available_Samples (Object       : sdrplay_device)
	                                                   return Natural;
        function Valid_Device      (Object       : sdrplay_device)
	                                                   return Boolean;
private
	procedure Initialize (Object: in out sdrplay_device);
	procedure Finalize   (Object: in out sdrplay_device);

	package sdrplay_Buffer is new ringBuffer (complex);
	use sdrplay_Buffer;
	type sdrplay_device is new device with 
	record
	   The_Buffer        : sdrplay_Buffer. ringBuffer_data (16 * 32768);
	   Running           : Boolean                        := False;
	   Current_Gain      : Integer                        := 40;
	   VFO_Frequency     : Integer                        := 227000000;
	   err               : Interfaces. C. int;
	   Library_Version   : Interfaces. C. c_float;
	   isValid	     : Boolean;
	end record;

	function mir_sdr_ApiVersion (version : out Interfaces. C. c_float)
	                                          return Interfaces. C. int;
	pragma Import (C, mir_sdr_ApiVersion, "mir_sdr_ApiVersion");
--
--	xi and xq are access values, pointing to C arrays
	type Sdrplay_Callback_Type is access
	          procedure (xi             : access Interfaces. C. short;
	                     xq             : access Interfaces. C. short;
	                     firstSampleNum : Interfaces. C. int;
	                     grChanged      : Interfaces. C. int;
	                     rfChanged      : Interfaces. C. int;
	                     fsChanged      : Interfaces. C. int;
	                     numSamples     : Interfaces. C. unsigned;
	                     reset          : Interfaces. C. unsigned;
	                     userData       : system. Address);
	pragma Convention (C, Sdrplay_Callback_Type);

	type Sdrplay_Gain_Callback_Type is access
	           procedure (gRdB      : Interfaces. C. unsigned;
	                      lnsGRdB   : Interfaces. C. unsigned;
	                      userData  : system. Address);
	pragma Convention (C, Sdrplay_Gain_Callback_Type);

--	functions to be used:
	function mir_sdr_StreamInit (gain             : out Interfaces. C. int;
	                             inputRate        : Interfaces. C. double;
	                             vfoFreq          : Interfaces. C. double;
	                             bandWidth        : Interfaces. C. int;
	                             ifType           : Interfaces. C. int;
	                             lnaEnable        : Interfaces. C. int;
                                     gRdb             : out Interfaces. C. int;
	                             agcMode          : Interfaces. C. int;
	                             samplesperPacket : out Interfaces. C. int;
	                             myStreamCallback : Sdrplay_Callback_Type;
	                             myGainCallback   : Sdrplay_Gain_Callback_Type;
	                             userData         : system. Address)
	                                          return Interfaces. C. int;
	pragma Import (C, mir_sdr_StreamInit, "mir_sdr_StreamInit");

	procedure mir_sdr_StreamUnInit;
	pragma Import (C, mir_sdr_StreamUnInit, "mir_sdr_StreamUninit");
--
	function mir_sdr_SetDcMode (A  : Interfaces. C. int;
	                            B  : Interfaces. C. int)
	                                  return Interfaces. C. int;
	pragma Import (C, mir_sdr_SetDcMode, "mir_sdr_SetDcMode");

	function  mir_sdr_SetDcTrackTime (A : Interfaces. C. int)
	                                  return Interfaces. C. int;
	pragma Import (C, mir_sdr_SetDcTrackTime, "mir_sdr_SetDcTrackTime");

	function mir_sdr_SetFs	(dfsHz : Interfaces. C. double;
	                         A     : Interfaces. C. int;
	                         sync  : Interfaces. C. int)
	                                  return Interfaces. C. int;
	pragma Import (C, mir_sdr_SetFs, "mir_sdr_SetFs");

	function mir_sdr_SetRf (drfHz : Interfaces. C. double;
	                        A     : Interfaces. C. int;
	                        sync  : Interfaces. C. int)
	                                  return Interfaces. C. int;
	pragma Import (C, mir_sdr_SetRf, "mir_sdr_SetRf");

	function mir_sdr_SetGr	 (gRdb : Interfaces. C. int;
	                          A    : Interfaces. C. int;
	                          sync : Interfaces. C. int)
	                                   return Interfaces. C. int;
	pragma Import (C, mir_sdr_SetGr, "mir_sdr_SetGr");

	function mir_sdr_SetSyncUpdateSampleNum (S : Interfaces. C. int)
	                                   return Interfaces. C. int;
	pragma Import (C, mir_sdr_SetSyncUpdateSampleNum,
	                            "mir_sdr_SetSyncUpdateSampleNum");

	function mir_sdr_SetSyncUpdatePeriod (P : Interfaces. C. int)
	                                   return Interfaces. C. int;
	pragma Import (C, mir_sdr_SetSyncUpdatePeriod,
	                            "mir_sdr_SetSyncUpdatePeriod");

	function mir_sdr_AgcControl (enable     : Interfaces. C. unsigned;
	                             setPoint   : Interfaces. C. int;
	                             knee_dBfs  : Interfaces. C. int;
	                             decay_ms   : Interfaces. C. unsigned;
	                             hang_ms    : Interfaces. C. unsigned;
	                             syncUpdate : Interfaces. C. int;
	                             lnsEnable  : Interfaces. C. int)
	                                 return Interfaces. C. int;
	pragma Import (C, mir_sdr_AgcControl, "mir_sdr_AgcControl");

	function mir_sdr_DCoffsetIQimbalanceControl (
	                                       DCEnable : Interfaces. C. int;
	                                       IQEnable : Interfaces. C. int)
	                                           return Interfaces. C. int;
	pragma Import (C, mir_sdr_DCoffsetIQimbalanceControl,
	                         "mir_sdr_DCoffsetIQimbalanceControl");
	

	procedure Sdrplay_Callback (xi             : access Interfaces.C. short;
	                            xq             : access Interfaces.C. short;
	                            firstSampleNum : Interfaces. C. int;
	                            grChanged      : Interfaces. C. int;
	                            rfChanged      : Interfaces. C. int;
	                            fsChanged      : Interfaces. C. int;
	                            numSamples     : Interfaces. C. unsigned;
	                            reset          : Interfaces. C. unsigned;
	                            userData       : system. Address);
	pragma Convention (C, Sdrplay_Callback);

	procedure  Sdrplay_Gain_Callback (gRdB      : Interfaces. C. unsigned;
	                                  lnsGRdB   : Interfaces. C. unsigned;
	                                  userData  : system. Address);
	pragma Convention (C, Sdrplay_Gain_Callback);
end sdrplay_wrapper;
