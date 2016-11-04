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
with header;	use header;
Generic
	The_Mode           : Dabmode;
	with procedure Fetch_Samples (X : out complexArray; y : out Integer);
	with function Available_Samples return Integer;
	with procedure process_mscBlock (FBits : shortArray; Blkn : Integer);
	with procedure process_ficBlock (FBits : shortArray; Blkn : Integer);
	with function Sync_Reached return Boolean;
package Ofdm_Handler is
	procedure start;
	procedure Reset;
	procedure Stop;
	function Is_Stopped return Boolean;
private
	task Ofdm_Worker is
	   entry start;
	end;
	Exit_Ofdmprocessing :	exception;
	procedure Get_Samples (Out_V      : out complexArray;
	                       Phase_Ind  : Integer);

	Tu                   : Natural	:= header. T_u (The_Mode);
	Tg                   : Natural  := header. T_g (The_Mode);
	Ts                   : Natural  := header. T_s (The_Mode);
	Tnull                : Natural  := header. T_null (The_Mode);
	Carriers             : Natural  := header. K (The_Mode);
	Carrier_Diff         : Natural  := header. Carrier_Diff (The_Mode);
	L_Mode               : Integer  := header. L (The_Mode);
	Samplecounter        : Natural  := 0;
	Current_Strength     : Float    := 0.0;
	Running              : Boolean  := false;
	Buffer_Content       : Integer  := 0;
	Current_Phase        : Integer  := 0;
	Signal_Level         : Float    := 0.0;
	Fine_Corrector       : Integer  := 0;
	Coarse_Corrector     : Integer  := 0;
	Correction_Flag      : Boolean  := true;
--	Ofdm_fft             : fft_handler. FFT_Processor (FORWARD, The_Mode);
--
--	It took a while before I detected that just declaring an array
--	with the size of the OscillatorTable would cause a storage error
	OscillatorTable      : complexArray_P :=
	                                new complexArray (0 .. inputRate - 1);
end ofdm_handler;

