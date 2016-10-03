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
with Text_IO;		use Text_IO;
with Ada. Exceptions;	use Ada. Exceptions;
with Ada. Unchecked_Deallocation;
with header;		use header;
with simple_messages;	use simple_messages;
with fic_handler;
with msc_handler;
with fft_handler;
--
package body Ofdm_Handler is
	use header. ComplexTypes;
	function "abs" (Right: header. ComplexTypes. complex)
	                                        return Float renames
                                     header. ComplexTypes. "abs";
	function arg (Right: header. ComplexTypes. complex)
	                                        return Float renames
                                     header. complexTypes. Argument;
	function conj (val: complexTypes. complex)
	                                        return complexTypes. complex
	                             renames complexTypes. Conjugate;
--
	procedure Initialize (Object : in out Ofdm_Processor) is
	begin
	   Object. Tu                   := header. T_u (Object. Mode);
	   Object. Tg                   := header. T_g (Object. Mode);
	   Object. Ts                   := header. T_s (Object. Mode);
	   Object. Tnull                := header. T_null (Object. Mode);
	   Object. Carriers             := header. K  (Object. Mode);
	   Object. Carrier_Diff         := header. Carrier_Diff (Object. Mode);
	   Object. L_mode	        := header. L (Object. Mode);
	   Object. Samplecounter	:= 0;
	   Object. Current_Strength	:= 0.0;

	   Object. Running              := false;
	   Object. Buffer_Content       := 0;	
	   Object. Current_Phase        := 0;
	   Object. Signal_Level	        := 0.0;
	   Object. Fine_Corrector       := 0;
	   Object. Coarse_Corrector     := 0;
	   Object. Correction_Flag      := true;
	   Object. Token_Length         := 0;
--
--	will be set/ controlled dynamically
	   Object. The_Processor        := null;
--
	   for i in Object. OscillatorTable' Range loop
	      Object. OscillatorTable (i) :=
	              (Math. cos (float (i) * 2.0 * M_PI / float (Input_Rate)),
	               Math. sin (float (i) * 2.0 * M_PI / float (Input_Rate)));
	   end loop;
--	   put_line ("Initialization complete");
	end Initialize;

	procedure Finalize (Object : in out Ofdm_Processor) is
	begin
	   null;
	end Finalize;
--
--	Someone (external to this function) will eventually
--	set "Running" to false, indicating that the task, which
--	will call Get_Samples, need to terminate
	procedure Get_Samples (Object     : in out Ofdm_Processor;
	                       Out_V      : out complexArray;
	                       Phase_Ind  : Integer) is
	   Amount:   Integer   := Out_V' length;
	begin

	   if not Object. Running then
	      raise Exit_ofdmProcessing;
	   end if;

	   if Amount > Object. Buffer_Content then
	      Object. Buffer_Content := Object. Available_Samples. all;
	      while Object. Running and then
	                       Object. Buffer_Content < Amount loop
	         delay 0.01;
	         Object. Buffer_Content := Object. Available_Samples. all;
	      end loop;

	      if not Object. Running then
	         raise Exit_OfdmProcessing;
	      end if;
	   end if;

--	we have samples, let's get them
	   Object. Fetch_Samples (Out_V, Amount);
	   Object. Buffer_Content :=  Object. Buffer_Content - Amount;

--	first: adjust frequency. We need Hz accuracy
	   for I in Out_V' Range loop
	      Object. Current_Phase :=
	                  (Object. Current_Phase - Phase_Ind) mod Input_Rate;
	      Out_V (I)             := Out_V (I) *
	                        Object. OscillatorTable (Object. Current_Phase);
	      Object. Signal_Level  := 0.00001 * abs Out_V (I) +
	                                (1.0 - 0.00001) * Object. Signal_Level;
	   end loop;
--
--	currently, tokenlength is not used
--	   Object. Token_Length         := Object. Token_Length + Amount;
--
--	Once a second (i.e. after INPUT_RATE samples), we
--	show some data
	   Object. Samplecounter    := Object. Samplecounter + Amount;
	   if Object. Samplecounter >=  Input_Rate then
	      simple_messages. message_queue. Put ((FINE_CORRECTOR_SIGNAL,
	                                            Object. Fine_Corrector));
	      simple_messages. message_queue. Put ((COARSE_CORRECTOR_SIGNAL,
	                                            Object. Coarse_Corrector));
	      Object. Samplecounter := 0;
	   end if; 

	exception
	      when Error: others   => Put ("Exception in getsamples: ");
	                              Put_Line (Exception_Name (Error));
	                              raise;
	end Get_Samples;
--
--
--	we might consider a hard reset
	procedure reset (Object : in out Ofdm_Processor)	is
	begin
	   Object. Fine_Corrector	:= 0;
	   Object. Coarse_Corrector	:= 0;
	   Object. Correction_Flag	:= true;
	end reset;
--
--
	procedure stop (Object : in out Ofdm_Processor) is
	   procedure Free_Worker is
	          new Ada. Unchecked_Deallocation (
	                          Object => Ofdm_Worker,
	                          Name   => Ofdm_Worker_P);
	begin

	   if Object. Running then
	      Object. Running := false;
--	      put_line ("we zijn aan het aborten");

	      while not Object. The_Processor' Terminated loop
	         delay 0.01;
	      end loop;
	      Free_Worker (Object. The_Processor);
	      Object. The_Processor	:= null;
	   end if;
	end stop;

	function Is_Stopped (Object : Ofdm_Processor) return Boolean is
	begin
	   return not  Object. Running;
	end is_stopped;
	
	procedure Start (Object : in out Ofdm_Processor;
	                 Env    : Ofdm_Processor_P) is
	begin

	   if Object. Running
	   then
	      return;
	   end if;

	   Object. The_Processor   := new Ofdm_Worker (Env);
	   Object. Running         := true;
	end start;

--	the task is the main driver, it reads samples and does the
--	ofdm handling. It calls upon the fic handler and the msc handler
--
	task body Ofdm_Worker is
	   Tu               : Natural renames Object. Tu;
	   Tg               : Natural renames Object. Tg;
	   Ts               : Natural renames Object. Ts;
	   Tnull            : Natural renames Object. Tnull;
	   Carriers         : Natural renames Object. Carriers;
	   Carrier_Diff	    : Natural renames Object. Carrier_Diff;
	   Start_Index      : Integer;
	   Phase_Error      : complex;
	   Counter          : Integer;
	   Syncbuffer_Index : Integer := 0;
	   Previous_1       : Integer := 1000;
	   Previous_2       : Integer :=  999;

	   Env_Buffer	: floatArray	(0 .. 32767);
	   --
	   --	some shorthands
	   subtype Tu_Sized_Buffer      is complexArray (0 .. Tu - 1);
	   subtype Ts_Sized_Buffer      is complexArray (0 .. Ts - 1);
	   subtype Tnull_Sized_Buffer   is complexArray (0 .. Tnull - 1);
	   subtype Ibit_Vector          is shortArray (0 .. 2 * Carriers - 1);
	   --
	   --	some vectors
	   Reference_Vector  : Tu_Sized_Buffer;
	   Ofdm_Buffer       : Ts_Sized_Buffer;
	   Null_Buffer       : Tnull_Sized_Buffer;
	   Ibits             : Ibit_Vector;
	   --
	   --	procedures local to the task body are
	   --	a. function Compute_Offset
	   --	b. procedure ProcessToken
	   --
	   --	compute the estimated coarse frequency offset
	   function Compute_Offset (Block_0_Buffer: Tu_Sized_Buffer)
	                                                  return Integer is
	      Search_Range   : constant Integer	:= 36;
	      Res_Vector     : Tu_Sized_Buffer renames Block_0_Buffer;
	      M_min          : Float            := 1000.0;
	      Index          : Integer          := Object. Tu;
	   begin
--	we look at a special pattern consisting
--	of zeros in the row of args between successive carriers.
	      for I in Tu - Search_Range / 2 .. Tu + Search_Range / 2 loop
	         declare
                    A1: Float := 
	                    abs (abs  (arg (Res_Vector ((I + 1) mod Tu) *
                            conj (Res_Vector ((I + 2) Mod Tu))) / M_PI) - 1.0);
	            A2: Float :=
	                    abs arg (Res_Vector ((I + 1) mod Tu) *
	            	            conj (Res_Vector ((I + 3) Mod Tu)));
	            A3: Float :=
	                    abs arg (Res_Vector ((I + 3) mod Tu) *
	            	            conj (Res_Vector ((I + 4) mod Tu)));
	            A4: Float :=
	                    abs arg (Res_Vector ((I + 4) mod Tu) *
	            	            conj (Res_Vector ((I + 5) mod Tu)));
	            A5: Float :=
	                    abs arg (Res_Vector ((I + 5) mod Tu) *
	            	            conj (Res_Vector ((I + 6) mod Tu)));
	            B1: Float :=
	                    abs (abs (arg (Res_Vector ((I + 16 + 1) mod Tu) *
	            	            conj (Res_Vector ((I + 16 + 3) mod Tu))) / M_PI) - 1.0);
	            B2: Float :=
	                    abs (arg (Res_Vector ((I + 16 + 3) mod Tu) *
	            	             conj (Res_Vector ((I + 16 + 4) mod Tu))));
	            B3: Float :=
	                    abs (arg (Res_Vector ((I + 16 + 4) mod Tu) *
	            	             conj (Res_Vector ((I + 16 + 5) mod Tu))));
	            B4: Float :=
	                    abs (arg (Res_Vector ((I + 16 + 5) mod Tu) *
	            	             conj (Res_Vector ((I + 16 + 6) mod Tu))));
	            Sum: Float := A1 + A2 + A3 + A4 + A5 + B1 + B2 + B3 + B4;
	         begin
	            if Sum < M_min then
	               M_min	:= Sum;
	               Index	:= I;
	            end if;
	         end;
	      end loop;
	      return Index - Object. Tu;
	   end Compute_Offset;
--
--	All ofdm tokens, apart from "block 0" will be processed
--	by this procedure
--
--	Note that we specify "Buffer" as "in out" since we
--	pass it on to the fft processor
	   procedure Process_Token (Buffer  : in out Tu_Sized_Buffer; 
	                            Ibits   : out Ibit_Vector;
	                            Blkno   : Integer) is
	      Work_Vector : Tu_Sized_Buffer renames Buffer;
	   begin
	      Object. Ofdm_fft. do_FFT (Work_Vector);
--
--	Note that "mapIn" maps to -carriers / 2 .. carriers / 2
--	we did not set the fft output to low .. high
	      for I in 0 .. Carriers - 1 loop
	         declare
	            Index   : Integer := Object. My_Mapper. Map_In (i);
	            R1      : complexTypes. complex;
	         begin
	            if Index < 0 then
	               Index := Index + Tu;
	            end if;
--
--	this is the data value, we keep the "old" value as reference
--	value for the next block
	            R1 := Work_Vector (Index) *
	                         conj (Reference_Vector (Index));
	            Reference_Vector (Index) := Work_Vector (Index);
--	Recall:  with this viterbi decoder
--	we have 127 = max pos, -127 = max neg, so we scale
	            Ibits (I)	   :=
	                           short_Integer (R1. Re / abs R1 * 127.0);
	            Ibits (Carriers + i) :=
	                           short_Integer (R1. Im / abs R1 * 127.0);
	         end;
	      end loop;
	   end Process_Token;
	begin
--
--	here we start the task body
	   Object. Running           := true;
	   Object. Fine_Corrector    := 0;
	   Object. Signal_Level      := 0.0;
	   Object. Correction_Flag   := true;

--	Initing, to get a decent value for Signal_Level. The samples are just
--	read, 
	   for I in 0 .. 20 loop
	      declare
	         Dummy_Buf : Ts_Sized_Buffer;
	      begin
	         Object. Get_Samples (Dummy_Buf, 0);
	      end;
	   end loop;
--
--	This is then the main loop, implemented using goto's
--	when we are really out of sync, we will be here
<<notSynced>>
	   Syncbuffer_Index          := 0;
	   Object. Current_Strength  := 0.0;

	   for I in 0 .. 50 loop
	      declare
	         Sample : complexArray (0 .. 0);
	      begin
	         Object. Get_Samples (Sample, 0);
	         Env_Buffer (Syncbuffer_Index)  := abs Sample (0);
	         Object. Current_Strength       :=
	                           Object. Current_Strength + abs Sample (0);
	         Syncbuffer_Index               := Syncbuffer_Index + 1;
	      end;
	   end loop;

--	We now have initial values for currentStrength (i.e. the sum
--	over the last 50 samples) and sLevel, the long term average.

<<SyncOnNull>>
--	here we start looking for the null level, i.e. a dip
	   Counter		:= 0;

	   while Object. Current_Strength / 50.0 >
	                              0.40 * Object. Signal_Level loop
	      declare
	         Sample: complexArray (0 .. 0);
	      begin
	         Object. Get_Samples (sample,
	                              Object. Coarse_Corrector +
	                                      Object. Fine_Corrector);
	         Env_Buffer (Syncbuffer_Index) := abs Sample (0);
	         Object. Current_Strength      :=
	                           Object. Current_Strength + abs Sample (0) -
	                                   Env_Buffer (Syncbuffer_Index - 50);
	         Syncbuffer_Index              := Syncbuffer_Index + 1;
	         Counter                       := Counter + 1;
	         if Counter > 3 * Ts then	-- hopeless
	            goto notSynced;
	         end if;
	      end;
	   end loop;
--
--	It seems we just successfully passed the start of a null period,
--	now start looking for the end of the null period.
<<SyncOnEndNull>>

	   counter   := 0;
	   while Object. Current_Strength / 50.0 <
	                                0.75 * Object. Signal_Level loop
	      declare
	         Sample	: complexArray (0 .. 0);
	      begin
	         Object. Get_Samples (Sample,
	                              Object. Coarse_Corrector +
	                                          Object. Fine_Corrector);
	         Env_Buffer (Syncbuffer_Index) := abs Sample (0);
--	update 
	         Object. Current_Strength      :=
	             Object. Current_Strength + Env_Buffer (Syncbuffer_Index) -
	                                 Env_Buffer (Syncbuffer_Index - 50);
	         Syncbuffer_Index              := Syncbuffer_Index + 1;
	         Counter                       := Counter + 1;
	         if Counter >  Tnull then	-- hopeless 
	            goto notSynced;
	         end if;
	      end;
	   end loop;

--	The end of the null period is identified, it ends probably about 40 
--	or 50 samples earlier
<<SyncOnPhase>>

--	We now have to find the exact first sample of the non-null period.
--	We use a correlation that will find the first sample after the
--	cyclic prefix.
--	When in "sync", i.e. pretty sure that we know were we are,
--	we skip the "dip" identification and come here right away.
--
--	Read in Tu samples and look for the startIndex
	   Object. Get_Samples (Reference_Vector,
	                        Object. Coarse_Corrector +
	                               Object. Fine_Corrector);
	   Start_Index   :=  Object. My_Phasesynchronizer.
	                                 Find_Index (Reference_Vector, 3);
	   if Start_Index < 0 then		-- no sync, try again
	      goto notSynced;
	   end if;

--	we have the start,  move the segment to the
--	beginning of the referenceVector and
--	read in the samples from "block 0"
	   Reference_Vector (0 .. Tu - Start_Index - 1) :=
	                   Reference_Vector (Start_Index .. Tu - 1);
	   if Start_Index > 0 then
	      Object. Get_Samples (Reference_Vector (Tu - Start_Index .. Tu - 1),
	                           Object. Coarse_Corrector +
	                                        Object. Fine_Corrector);
	   end if;

--	we  now have the contents of block 0 and
--	set the reference vector for the frame
	   Object. Ofdm_fft. do_FFT (Reference_Vector);

--	here we look only at computing a coarse offset when needed
--	first check 
	   if not Object. Correction_Flag then
	      Object. Correction_Flag	:= not fic_handler. Sync_Reached;
	   end if;
	   if Object. Correction_Flag then
	      declare
	         Correction_Value : Integer :=
	                              Compute_Offset (Reference_Vector);
	      begin
	         if Correction_Value = 0
	                  and then Previous_1 = 0
	                  and then Previous_1 = Previous_2 then
	            Object. Correction_Flag	:= false;
	         elsif Correction_Value /= 100 then
	            Object. Coarse_Corrector := Object. Coarse_Corrector +
	                                        Correction_Value *
	                                        Object. Carrier_Diff;
	            if abs Object. Coarse_Corrector > kHz (35) then
	               Object. Coarse_Corrector	:= 0;
	            end if;
	            Previous_2		:= Previous_1;
	            Previous_1		:= Correction_Value;
	         end if;
	      end;
	   end if;
--
--	Here we really start processing the data
	   Phase_Error   := (0.0, 0.0);
--
--	blocks 2, 3 and 4 contain the FIC data
	   for Symbolcount in 2 .. 4 loop
	      Object. Get_Samples (Ofdm_Buffer,
	                       Object. Coarse_Corrector + Object. Fine_Corrector);
	      for I in 0 .. Tg - 1 loop
	         Phase_Error := Phase_Error +
	                          conj (Ofdm_Buffer (I)) * Ofdm_Buffer (Tu + I);
	      end loop;
	      
	      Process_Token (Ofdm_Buffer (Tg .. Ts - 1), Ibits, Symbolcount);
	      fic_handler.  process_ficBlock (Ibits, Symbolcount);
	   end loop;
--
--	the others the MSC data
	   for Symbolcount in 5 .. Object. L_mode loop
	      Object. Get_Samples (Ofdm_Buffer,
	                           Object. Coarse_Corrector +
	                                        Object. fine_Corrector);
	      for I in 0 .. Tg - 1 loop
	         Phase_Error := Phase_Error +
	                        conj (Ofdm_Buffer (I)) * Ofdm_Buffer (Tu + I);
	      end loop;
	      Process_Token (Ofdm_Buffer (Tg .. Ts - 1), Ibits, Symbolcount);
	      msc_handler.  process_mscBlock (Ibits, Symbolcount);
	   end loop;

<<NewOffset>>
--	we integrate the newly found frequency error with the
--	existing frequency offset
	   Object. Fine_Corrector :=  Object. Fine_Corrector +
	                              integer (0.1 * arg (Phase_Error) / M_PI *
	                                     float (Object. Carrier_diff) / 2.0);

--	OK,  here we are at the end of the frame
--	we assume everything went well and skip T_null samples

	   Object. Get_Samples (Null_Buffer, 
	                        Object. Coarse_Corrector + Object. Fine_Corrector);
	   Syncbuffer_Index	:= 0;

--	Here we just check the validity of the fineCorrector

	   if Object. Fine_Corrector >  Object. Carrier_diff / 2 then
	      Object. Coarse_Corrector := 
	                       Object. Coarse_Corrector +  Object. Carrier_Diff;
	      Object. Fine_Corrector   :=
	                       Object. Fine_Corrector - Object. Carrier_Diff;
	   elsif Object. Fine_Corrector < - Object. Carrier_Diff / 2 then
	      Object. Coarse_Corrector :=
	                       Object. Coarse_Corrector - Object. Carrier_Diff;
	      Object. Fine_Corrector   :=
	                       Object. Fine_Corrector + Object. Carrier_Diff;
	   end if;
<<ReadyForNewFrame>>
	   goto SyncOnPhase;

	exception
	   when exit_ofdmProcessing	=> put ("normal termination"); New_Line (1);
	   when Error: others		=> Put ("Exception in ofdmProcessor: ");
	                                   Put_Line (Exception_Name (Error));
	end Ofdm_Worker;

end Ofdm_Handler;

