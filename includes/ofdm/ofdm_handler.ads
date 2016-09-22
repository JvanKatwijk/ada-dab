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
with Ada. Finalization;
with Ada. Unchecked_Deallocation;
with phase_handler;
with fft_handler;
with freq_interleaver;

package Ofdm_Handler is
	type Ofdm_Processor (mode:		 Dabmode;
	                     Fetch_Samples:	 Get_Samples_Access;
	                     Available_Samples:	 Available_Samples_Access) is
	                       new Ada. Finalization. Controlled with private;
	type Ofdm_Processor_P is access all Ofdm_Processor;
	procedure start (Object: in out ofdm_Processor; env : ofdm_Processor_P);
	procedure Reset (Object: in out ofdm_Processor);
	procedure Stop  (Object: in out ofdm_Processor);
	function Is_Stopped (Object: Ofdm_Processor) return Boolean;
private
	task type Ofdm_Worker (Object: Ofdm_Processor_P);
	type Ofdm_Worker_P is access all Ofdm_Worker;
	Exit_Ofdmprocessing:	exception;
	procedure Get_Samples (Object:	in out Ofdm_Processor;
	                       Out_V:	out complexArray;
	                       Phase:	Integer);
	type Ofdm_Processor (Mode:	dabMode;
	                     Fetch_Samples:	Get_Samples_Access;
	                     Available_Samples:	Available_Samples_Access) is
	            new Ada. Finalization. Controlled with
	   record
	      Tu:			Integer;
	      Tg:			Integer;
	      Ts:			Integer;
	      Tnull:		Integer;
	      Carriers:		Integer;
	      Carrier_Diff:	Integer;
	      L_Mode:		Integer;
	      Samplecounter:	Integer;
	      Current_Strength:	Float;
	      Running:		Boolean;
	      Buffer_Content:	Integer;
	      Current_Phase:	Integer;
	      Signal_Level:	Float;
	      Fine_Corrector:	Integer;
	      Coarse_Corrector:	Integer;
	      Correction_Flag:	Boolean;
	      Token_Length:	Integer;
	      The_Processor:	Ofdm_Worker_P;
	      My_Phasesynchronizer: phase_handler. Phase_Synchronizer_P;
	      Ofdm_fft:		fft_handler. FFT_Processor_P;
	      My_Mapper:		freq_interleaver. interleaver_P;
	      OscillatorTable:	complexArray (0 .. inputRate - 1);
	   end record;
	procedure Initialize	(Object : in out Ofdm_Processor);
	procedure Finalize	(Object : in out Ofdm_Processor);
end ofdm_handler;

