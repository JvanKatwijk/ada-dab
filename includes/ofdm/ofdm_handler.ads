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
with header; use header;
with Ada. Finalization;
with Ada. Unchecked_Deallocation;
with phase_handler;
with fft_handler;
with freq_interleaver;
package ofdm_handler is
	type ofdmProcessor (mode		: dabMode;
	                    fetchSamples	: getSamples_access;
	                    Samples_amount	: Samples_access) is new
	                    Ada. Finalization. Controlled with private;
	type ofdmProcessor_P is access all ofdmProcessor;
	procedure start (Object: in out ofdmProcessor; env : ofdmProcessor_P);
	procedure reset (Object: in out ofdmProcessor);
	procedure stop  (Object: in out ofdmProcessor);
	function is_stopped (Object: ofdmProcessor) return Boolean;
private
	task type ofdmWorker (Object: ofdmProcessor_P);
	type ofdmWorker_P is access all ofdmWorker;
	exit_ofdmProcessing	: exception;
	procedure getSamples (Object	: in out ofdmProcessor;
	                      outV	: out complexArray;
	                      phase	: Integer);
	type ofdmProcessor (mode		: dabMode;
	                    fetchSamples	: getSamples_access;
	                    Samples_amount	: Samples_access) is new
	                    Ada. Finalization. Controlled with record
	   Tu			: Integer;
	   Tg			: Integer;
	   Ts			: Integer;
	   Tnull		: Integer;
	   carriers		: Integer;
	   carrierdiff		: Integer;
	   L_mode		: Integer;
	   sampleCounter	: Integer;
           currentStrength	: Float;
	   running		: Boolean;
	   bufferContent	: Integer;
	   currentPhase		: Integer;
	   sLevel		: Float;
	   fineCorrector	: Integer;
	   coarseCorrector	: Integer;
	   f2Correction		: Boolean;
	   tokenLength		: Integer;
	   theProcessor		: ofdmWorker_P;
	   my_phaseSynchronizer	: phase_handler. phaseSynchronizer_P;
	   ofdm_fft		: fft_handler. fft_P;
	   myMapper		: freq_interleaver. interleaver_P;
	   oscillatorTable	: complexArray (0 .. inputRate - 1);
	end record;
	procedure Initialize	(Object : in out ofdmProcessor);
	procedure Finalize	(Object : in out ofdmProcessor);
end ofdm_handler;

