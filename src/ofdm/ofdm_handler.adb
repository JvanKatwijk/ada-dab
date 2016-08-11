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
with ofdm_handler;
with Text_IO; use Text_IO;
with Ada. Exceptions; use Ada. Exceptions;
with Ada. Unchecked_Deallocation;
with header; use header;
with simple_messages; use simple_messages;
with fic_handler;
with msc_handler;
with fft_handler;
--
--
package body ofdm_handler is
use header. complexTypes;
	function "abs" (Right: header. ComplexTypes. complex)
	                                        return Float renames
                                     header. complexTypes. "abs";
	function arg (Right: header. ComplexTypes. complex)
	                                        return Float renames
                                     header. complexTypes. Argument;
	function conj (val: complexTypes. complex)
	                                        return complexTypes. complex
	                             renames complexTypes. Conjugate;
--
	procedure Initialize (Object : in out ofdmProcessor) is
	begin
	   Object. Tu		:= header. T_u (Object. mode);
	   Object. Tg		:= header. T_g (Object. mode);
	   Object. Ts		:= header. T_s (Object. mode);
	   Object. Tnull	:= header. T_null (Object. mode);
	   Object. carriers	:= header. K  (Object. mode);
	   Object. carrierdiff	:= header. carrierDiff (Object. mode);
	   Object. L_mode	:= header. L (Object. mode);
	   Object. sampleCounter	:= 0;
	   Object.currentStrength	:= 0.0;

	   Object.running	:= false;
	   Object.bufferContent	:= 0;	
	   Object.currentPhase	:= 0;
	   Object.sLevel	:= 0.0;
	   Object.fineCorrector	:= 0;
	   Object.coarseCorrector	:= 0;
	   Object.f2Correction	:= true;
	   Object.tokenLength	:= 0;
	   Object.myMapper	:= new freq_interleaver.
	                                      interleaver (Object. mode);
--
--	will be set/ controlled dynamically
	   Object. theProcessor	:= null;
	   Object. my_phaseSynchronizer	:=
	                       new phase_handler. phaseSynchronizer (Object. mode);
	   Object. ofdm_fft	:= new fft_handler. fft (FORWARD, Object. Tu);
	   for i in 0 .. inputRate - 1 loop
	      Object. oscillatorTable (i) :=
	               (Math. cos (float (i) * 2.0 * M_PI / float (inputRate)),
	                Math. sin (float (i) * 2.0 * M_PI / float (inputRate)));
	   end loop;
	put_line ("Initialization complete");
	end Initialize;

	procedure Finalize (Object : in out ofdmProcessor) is
	procedure Free_fft is new Ada. Unchecked_Deallocation (
	   Object => fft_handler. fft, name => fft_handler. fft_P);
	procedure Free_interleaver is new Ada. Unchecked_Deallocation (
	   Object => freq_interleaver. interleaver,
	   Name	  => freq_interleaver. interleaver_P);
	procedure Free_phaseSynchronizer is new Ada. Unchecked_Deallocation (
	   Object	=> phase_handler. phaseSynchronizer,
	   Name		=> phase_handler. phaseSynchronizer_P);
	begin
	   Free_fft (Object. ofdm_fft);
	   Free_interleaver (Object. myMapper);
	   Free_phaseSynchronizer (Object. my_phaseSynchronizer);
	end Finalize;

	procedure getSamples (Object	: in out ofdmProcessor;
	                      outV	: out complexArray;
	                      phase	: Integer) is
	amount:	Integer	:= outV' length;
	begin
	if not Object. running
	then
	   raise exit_ofdmProcessing;
	end if;

	if amount > Object. bufferContent
	then
	   Object. bufferContent := Object. Samples_amount. all;
	   while Object. running and then
	                       Object. bufferContent < amount loop
	      delay 0.01;
	      Object. bufferContent := Object. Samples_amount .all;
	   end loop;

	   if not Object. running
	   then
	      raise exit_ofdmProcessing;
	   end if;
	end if;
--	we have samples, let's get them
	Object. fetchSamples (outV, amount);
--	myDevice. getSamples (outV, amount);
	Object. bufferContent :=  Object. bufferContent - amount;
--	first: adjust frequency. We need Hz accuracy
	for i in outV' Range loop
	   Object. currentPhase		:=
	                  (Object. currentPhase - phase) mod inputRate;
	   outV (i)		:= outV (i) *
	                           Object. oscillatorTable (Object.currentPhase);
	   Object. sLevel	:= 0.00001 * abs outV (i) +
	                           (1.0 - 0.00001) * Object. sLevel;
	end loop;
--
--	currently, tokenlength is not used
	Object. tokenLength	:= Object. tokenLength + amount;
	Object. sampleCounter	:= Object. sampleCounter + amount;
	if Object. sampleCounter >=  inputRate
	then
	   simple_messages. message_queue. Put ((FINE_CORRECTOR_SIGNAL,
	                                         Object. fineCorrector));
	   simple_messages. message_queue. Put ((COARSE_CORRECTOR_SIGNAL,
	                                         Object. coarseCorrector));
	   Object. sampleCounter:= 0;
--	   put ("sLevel = "); put (float' Image (Object. sLevel));
--	   put (" strength ");
--	   put_line (float' Image (Object. currentStrength / 50.0));
	end if; 
end getSamples;
--
--
--	we might consider a hard reset
procedure reset (Object : in out ofdmProcessor)	is
begin
	Object. fineCorrector	:= 0;
	Object. coarseCorrector	:= 0;
	Object. f2Correction	:= true;
end reset;
--
--
procedure stop (Object : in out ofdmProcessor) is
procedure Free_worker is new Ada. Unchecked_Deallocation (
	Object	=> ofdmWorker,
	Name	=> ofdmWorker_P);
begin
	if Object. running
	then
	   Object. running := false;
	   put_line ("we zijn aan het aborten");
	   while not Object. theProcessor' Terminated loop
	      delay 0.01;
	   end loop;
	   Free_worker (Object. theProcessor);
	   Object. theProcessor	:= null;
	end if;
end stop;

function is_stopped (Object : ofdmProcessor) return Boolean is
begin
	if not  Object. running
	then
	   return true;
	end if;
	return false;
end is_stopped;
	
procedure start (Object : in out ofdmProcessor; env : ofdmProcessor_P) is
begin
	if Object. running
	then
	   return;
	end if;
	Object. theProcessor	:= new ofdmWorker (env);
	Object. running		:= true;
end start;

--	the task is the main driver, it reads samples and does the
--	ofdm handling. It calls upon the fic handler and the msc handler
--
task body ofdmWorker is
Tu	: Integer renames	Object. Tu;
Tg	: Integer renames	Object. Tg;
Ts	: Integer renames	Object. Ts;
Tnull	: Integer renames	Object. Tnull;
carriers	: Integer	renames Object. carriers;
carrierDiff	: Integer	renames Object. carrierDiff;
startIndex	: Integer;
FreqCorr	: header. ComplexTypes. complex;
counter		: Integer;
syncBufferIndex	: Integer	:= 0;
envBuffer	: floatArray	(0 .. 32767);
previous_1	: Integer	:= 1000;
previous_2	: Integer	:=  999;
--
--	some shorthands
subtype Tu_sizedBuffer	is complexArray (0 .. Tu - 1);
subtype Ts_sizedBuffer	is complexArray (0 .. Ts - 1);
subtype Tnull_sizedBuffer is complexArray (0 .. Tnull - 1);
subtype	ibitVector 	is shortArray (0 .. 2 * carriers - 1);
--
--	some vectors
referenceVector	: Tu_sizedBuffer;
ofdmBuffer	: Ts_sizedBuffer;
null_Buffer	: Tnull_sizedBuffer;
ibits		: ibitVector;
--
--	procedures local to the task body are
--	function Estimate
--	procedure processToken
--
--	compute the estimated coarse frequency offset
function computeOffset (block_0_Buffer: Tu_sizedBuffer) return Integer is
searchRange	: constant Integer	:= 36;
resVector	: Tu_sizedBuffer renames block_0_Buffer;
Mmin		: Float := 1000.0;
OMmin		: Float := 1000.0;
index		: Integer	:= Object. Tu;
begin

--	we look at a special pattern consisting
--	of zeros in the row of args between successive carriers.
	for i in Tu - searchRange / 2 .. Tu + searchRange / 2 loop
	   declare
              a1: Float := 
	              abs (abs  (arg (resVector ((i + 1) mod Tu) *
                            conj (resVector ((i + 2) Mod Tu))) / M_PI) - 1.0);
	      a2: Float :=
	              abs arg (resVector ((i + 1) mod Tu) *
	      	            conj (resVector ((i + 3) Mod Tu)));
	      a3: Float :=
	              abs arg (resVector ((i + 3) mod Tu) *
	      	            conj (resVector ((i + 4) mod Tu)));
	      a4: Float :=
	              abs arg (resVector ((i + 4) mod Tu) *
	      	            conj (resVector ((i + 5) mod Tu)));
	      a5: Float :=
	              abs arg (resVector ((i + 5) mod Tu) *
	      	                    conj (resVector ((i + 6) mod Tu)));
	      b1: Float :=
	              abs (abs (arg (resVector ((i + 16 + 1) mod Tu) *
	      	                    conj (resVector ((i + 16 + 3) mod Tu))) / M_PI) - 1.0);
	      b2: Float :=
	              abs (arg (resVector ((i + 16 + 3) mod Tu) *
	      	                    conj (resVector ((i + 16 + 4) mod Tu))));
	      b3: Float :=
	              abs (arg (resVector ((i + 16 + 4) mod Tu) *
	      	                    conj (resVector ((i + 16 + 5) mod Tu))));
	      b4: Float :=
	              abs (arg (resVector ((i + 16 + 5) mod Tu) *
	      	                    conj (resVector ((i + 16 + 6) mod Tu))));
	      sum: Float := a1 + a2 + a3 + a4 + a5 + b1 + b2 + b3 + b4;
	   begin
	      if sum < Mmin 
	      then
	         OMmin	:= Mmin;
	         Mmin	:= sum;
	         index	:= i;
	      end if;
	   end;
	end loop;
	return index - Object. Tu;
end computeOffset;
--
--	All ofdm tokens, apart from "block 0" will be processed
--	by this procedure
--	Note that we specify "Buffer" as "in out" since we
--	pass it on to the fft processor
procedure processToken	 (Buffer	: in out Tu_sizedBuffer; 
	                  ibits		: out ibitVector;
	                  blkno		: Integer) is
workVector : Tu_sizedBuffer	renames Buffer;
begin

	Object. ofdm_fft. do_FFT (workVector' Address);
--
--	Note that "mapIn" maps to -carriers / 2 .. carriers / 2
--	we did not set the fft output to low .. high
	for i in 0 .. carriers - 1 loop
	   declare
	      index 	: Integer := Object. myMapper. mapIn (i);
	      r1	: complexTypes. complex;
	   begin
	      if index < 0
	      then
	          index :=  index + Tu;
	      end if;
--
--	this is the data value, we keep the "old" value as reference
--	value for the next block
	      r1 := workVector (index) *
	                conj (referenceVector (index));
	      referenceVector (index) := workVector  (index);
--	Recall:  with this viterbi decoder
--	we have 127 = max pos, -127 = max neg, so we scale
	      ibits (i)	   :=
	             short_Integer (r1. Re / abs r1 * 127.0);

	      ibits (carriers + i) :=
	             short_Integer (r1. Im / abs r1 * 127.0);
	   end;
	end loop;
end processToken;
begin
--
--	here we start the task body
	Object. running		:= true;
	Object. fineCorrector	:= 0;
	Object. sLevel		:= 0.0;
	Object. f2Correction	:= true;

--	Initing, to get a decent value for sLevel
	for i in 0 .. 20 loop
	   declare
	      dummy_buf : Ts_sizedBuffer;
	   begin
	      Object. getSamples (dummy_buf, 0);
	   end;
	end loop;
--
--	This is then the main loop, implemented using goto's
--	when we are really out of sync, we will be here
<<notSynced>>
	syncBufferIndex		:= 0;
	Object. currentStrength		:= 0.0;
	for i in 0 .. 50 loop
	   declare
	      sample : complexArray (0 .. 0);
	   begin
	      Object. getSamples (sample, 0);
	      envBuffer (syncBufferIndex)	:= abs sample (0);
	      Object. currentStrength		:=
	                        Object. currentStrength + abs sample (0);
	      syncBufferIndex		:= syncBufferIndex + 1;
	   end;
	end loop;

--	We now have initial values for currentStrength (i.e. the sum
--	over the last 50 samples) and sLevel, the long term average.

<<SyncOnNull>>
--	here we start looking for the null level, i.e. a dip
	counter		:= 0;

	while Object. currentStrength / 50.0 > 0.40 *  Object. sLevel loop
	   declare
	      sample : complexArray (0 .. 0);
	   begin
	      Object. getSamples (sample,
	                          Object. coarseCorrector +
	                                   Object. fineCorrector);
	      envBuffer (syncBufferIndex) := abs sample (0);
	      Object. currentStrength	:=  Object. currentStrength +
	                                    abs sample (0) -
	                                    envBuffer (syncBufferIndex - 50);
	      syncBufferIndex	:= syncBufferIndex + 1;
	      counter		:= counter + 1;
	      if counter > 3 * Ts	-- hopeless
	      then
	         goto notSynced;
	      end if;
	   end;
	end loop;
--
--	It seems we just passed the start of a null period,
--	now start looking for the end of the null period.
<<SyncOnEndNull>>

	counter	:= 0;
	while Object. currentStrength / 50.0 < 0.75 * Object. sLevel loop
	   declare
	      sample	: complexArray (0 .. 0);
	   begin
	      Object. getSamples (sample,
	                          Object. coarseCorrector +
	                                       Object. fineCorrector);
	      envBuffer (syncBufferIndex) := abs sample (0);
--	update 
	      Object. currentStrength :=  Object. currentStrength +
	                                  envBuffer (syncBufferIndex) -
	                                  envBuffer (syncBufferIndex - 50);
	      syncBufferIndex := syncBufferIndex + 1;
	      counter	 := counter + 1;
	      if counter >  Tnull -- hopeless
	      then
	         goto notSynced;
	      end if;
	   end;
	end loop;

--	The end of the null is period identified, it ends probably about 40 
--	or 50 samples earlier
<<SyncOnPhase>>

--	We now have to find the exact first sample of the non-null period.
--	We use a correlation that will find the first sample after the
--	cyclic prefix.
--	When in "sync", i.e. pretty sure that we know were we are,
--	we skip the "dip" identification and come here right away.
--
--	read in Tu samples and look for the startIndex
	Object. getSamples (referenceVector,
	                       Object. coarseCorrector + Object. fineCorrector);
	startIndex :=  Object. my_phaseSynchronizer.
	                              findIndex (referenceVector, 3);
	if startIndex < 0		-- no sync, try again
	then
	   goto notSynced;
	end if;

--	we have the start,  move the segment to the
--	beginning of the referenceVector and
--	read in the samples from "block 0"
	referenceVector (0 .. Tu - startIndex - 1) :=
	                referenceVector (startIndex .. Tu - 1);
	if startIndex > 0
	then
	   Object. getSamples (referenceVector (Tu - startIndex .. Tu - 1),
	                       Object. coarseCorrector +
	                                     Object. fineCorrector);
	end if;

--	we  now have the contents of block 0 and
--	set the reference vector
	Object. ofdm_fft. do_FFT (referenceVector' Address);

--	here we look only at computing a coarse offset when needed
--	first check 
	Object. f2Correction	:= not fic_handler. syncReached;
	if Object. f2Correction
	then
	   declare
	      correction : Integer := computeOffset (referenceVector);
	   begin
--
	      if correction = 0
	               and then previous_1 = 0
	               and then previous_1 = previous_2
	      then
	         Object.f2Correction	:= false;
	      elsif correction /= 100
	      then
	         Object. coarseCorrector :=  Object. coarseCorrector +
	                                     correction *
	                                       Object. carrierDiff;
	         if abs Object. coarseCorrector > kHz (35)
	         then
	            Object. coarseCorrector	:= 0;
	         end if;
	         previous_2		:= previous_1;
	         previous_1		:= correction;
	      end if;
	   end;
	end if;
--
--	Here we really start processing the data
	FreqCorr	:= (0.0, 0.0);
--
--	blocks 2, 3 and 4 contain the FIC data
	for symbolCount in 2 .. 4 loop
	   Object. getSamples (ofdmBuffer,
	                    Object. coarseCorrector + Object. fineCorrector);
	   for i in 0 .. Tg - 1 loop
	      FreqCorr := FreqCorr +
	                       conj (ofdmBuffer (i)) * ofdmBuffer (Tu + i);
	   end loop;
	   
	   processToken (ofdmBuffer (Tg .. Ts - 1), ibits, symbolCount);
	   fic_handler.  process_ficBlock (ibits, symbolCount);
	end loop;
--
--	the others the MSC data
	for symbolCount in 5 .. Object. L_mode loop
	   Object. getSamples (ofdmBuffer,
	                     Object. coarseCorrector + Object. fineCorrector);
	   for i in 0 .. Tg - 1 loop
	      FreqCorr := FreqCorr +
	                     conj (ofdmBuffer (i)) * ofdmBuffer (Tu + i);
	   end loop;
	   processToken (ofdmBuffer (Tg .. Ts - 1), ibits, symbolCount);
	   msc_handler.  process_mscBlock (ibits, symbolCount);
	end loop;

<<NewOffset>>
--	we integrate the newly found frequency error with the
--	existing frequency error.
	Object. fineCorrector :=  Object. fineCorrector +
	                       Integer (0.1 * arg (FreqCorr) / M_PI *
	                          float (Object. carrierDiff) / 2.0);

--	OK,  here we are at the end of the frame
--	we assume everything went well and skip T_null samples

	Object. getSamples (null_Buffer, 
	                     Object. coarseCorrector + Object. fineCorrector);
	syncBufferIndex	:= 0;

--	Here we just check the validity of the fineCorrector

	if Object. fineCorrector >  Object. carrierDiff / 2
        then
	   Object. coarseCorrector := 
	                    Object. coarseCorrector +  Object. carrierDiff;
	   Object. fineCorrector   :=
	                    Object. fineCorrector - Object. carrierDiff;
	elsif Object. fineCorrector < - Object. carrierDiff / 2
        then
	   Object. coarseCorrector :=
	                    Object. coarseCorrector - Object. carrierDiff;
	   Object. fineCorrector   :=
	                    Object. fineCorrector + Object. carrierDiff;
	end if;
<<ReadyForNewFrame>>
	goto SyncOnPhase;

	exception
	   when exit_ofdmProcessing	=> put ("normal termination"); New_Line (1);
	   when Error: others		=> Put ("Exception: ");
	                           Put_Line (Exception_Name (Error));
end ofdmWorker;

end ofdm_handler;

