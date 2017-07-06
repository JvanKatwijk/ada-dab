	separate (ofdm_handler)
	task body Ofdm_Worker is
	   Start_Index      : Integer;
	   Phase_Error      : complex;
	   Counter          : Integer;
	   Syncbuffer_Index : Integer := 0;
	   Previous_1       : Integer := 1000;
	   Previous_2       : Integer :=  999;
	   Env_Buffer	    : floatArray (0 .. 16384);
	begin
	   accept start;
	   put_line ("ofdm_worker is gestart");
	   Fine_Corrector    := 0;
	   Signal_Level      := 0.0;
	   Correction_Flag   := true;

--	Initing, to get a decent value for Signal_Level. The samples are just
--	read, 
	   for I in 0 .. 20 loop
	      declare
	         Dummy_Buf : Ts_Sized_Buffer;
	      begin
	         Get_Samples (Dummy_Buf, 0);
	      end;
	   end loop;
--
--	This is then the main loop, implemented using goto's.
--	When we are really out of sync, we will be here
<<notSynced>>
	   Syncbuffer_Index  := 0;
	   Current_Strength  := 0.0;

	   for I in 0 .. 50 loop
	      declare
	         Sample : complexArray (0 .. 0);
	      begin
	         Get_Samples (Sample, 0);
	         Env_Buffer (Syncbuffer_Index)  := abs Sample (0);
	         Current_Strength       :=
	                           Current_Strength + abs Sample (0);
	         Syncbuffer_Index               := Syncbuffer_Index + 1;
	      end;
	   end loop;

--	We now have initial values for currentStrength (i.e. the sum
--	over the last 50 samples) and sLevel, the long term average.

<<SyncOnNull>>
--	here we start looking for the null level, i.e. a dip
	   Counter		:= 0;

	   while Current_Strength / 50.0 >
	                              0.40 * Signal_Level loop
	      declare
	         Sample: complexArray (0 .. 0);
	      begin
	         Get_Samples (sample, Coarse_Corrector + Fine_Corrector);
	         Env_Buffer (Syncbuffer_Index) := abs Sample (0);
	         Current_Strength      :=
	                           Current_Strength + abs Sample (0) -
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
--	This "end" should be there within T_null samples,
--	otherwise, just give up and start all over again
<<SyncOnEndNull>>

	   Counter   := 0;
	   while Current_Strength / 50.0 < 0.75 * Signal_Level loop
	      declare
	         Sample	: complexArray (0 .. 0);
	      begin
	         Get_Samples (Sample, Coarse_Corrector + Fine_Corrector);
	         Env_Buffer (Syncbuffer_Index) := abs Sample (0);
--	update 
	         Current_Strength      :=
	             Current_Strength + Env_Buffer (Syncbuffer_Index) -
	                                 Env_Buffer (Syncbuffer_Index - 50);
	         Syncbuffer_Index              := Syncbuffer_Index + 1;
	         Counter                       := Counter + 1;
	         if Counter >  Tnull then	-- hopeless 
	            goto notSynced;
	         end if;
	      end;
	   end loop;

--	The end of the null period is identified, it probably ended about 40 
--	or 50 samples earlier
<<SyncOnPhase>>

--	We now have to find the exact first sample of the non-null period.
--	We use a correlation that will find the first sample after the
--	cyclic prefix.
--	When in "sync", i.e. pretty sure that we know were we are,
--	we skip the "dip" identification and come here right away.
--
--	Read in Tu samples and look for the startIndex
	   Get_Samples (Reference_Vector, Coarse_Corrector + Fine_Corrector);
	   Start_Index   :=  my_Phasehandler.
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
	      Get_Samples (Reference_Vector (Tu - Start_Index .. Tu - 1),
	                           Coarse_Corrector + Fine_Corrector);
	   end if;

--	we  now have the contents of block 0 and
--	set the reference vector for the frame
	   my_fft. do_FFT (Reference_Vector);

--	here we look only at computing a coarse offset when needed
--	first check 
	   if not Correction_Flag then
	      Correction_Flag	:= not Sync_Reached;
	   end if;
	   if Correction_Flag then
	      declare
	         Correction_Value : Integer :=
	                              Compute_Offset (Reference_Vector);
	      begin
	         if Correction_Value = 0
	                  and then Previous_1 = 0
	                  and then Previous_1 = Previous_2 then
	            Correction_Flag	:= false;
	         elsif Correction_Value /= 100 then
	            Coarse_Corrector := Coarse_Corrector +
	                                        Correction_Value *
	                                        Carrier_Diff;
	            if abs Coarse_Corrector > kHz (35) then
	               Coarse_Corrector	:= 0;
	            end if;
	            Previous_2		:= Previous_1;
	            Previous_1		:= Correction_Value;
	         end if;
	      end;
	   end if;
--
--	Here we really start processing the data
	   Phase_Error   := (0.0, 0.0);
	   Ofdm_Decoder. Block_0 (Reference_Vector);

	   for Symbolcount in 2 .. L_Mode loop
	      Get_Samples (Ofdm_Buffer, Coarse_Corrector + Fine_Corrector);
	      for I in 0 .. Tg - 1 loop
	         Phase_Error := Phase_Error +
	                        conj (Ofdm_Buffer (I)) * Ofdm_Buffer (Tu + I);
	      end loop;
	      Ofdm_Decoder. Put (Symbolcount, Ofdm_Buffer (0 .. Tu - 1));
	   end loop;
--
<<NewOffset>>
--	we integrate the newly found frequency error with the
--	existing frequency offset
	   Fine_Corrector :=  Fine_Corrector +
	                              integer (0.1 * arg (Phase_Error) / M_PI *
	                                     float (Carrier_diff) / 2.0);

--	OK,  here we are at the end of the frame
--	we assume everything went well and we just skip T_null samples
	   Get_Samples (Null_Buffer, Coarse_Corrector + Fine_Corrector);
	   Syncbuffer_Index	:= 0;

--	Here we just check the validity of the fineCorrector

	   if Fine_Corrector > Carrier_diff / 2 then
	      Coarse_Corrector := Coarse_Corrector +  Carrier_Diff;
	      Fine_Corrector   := Fine_Corrector - Carrier_Diff;
	   elsif Fine_Corrector < - Carrier_Diff / 2 then
	      Coarse_Corrector := Coarse_Corrector - Carrier_Diff;
	      Fine_Corrector   := Fine_Corrector + Carrier_Diff;
	   end if;
<<ReadyForNewFrame>>
	   delay 0.02;
	   goto SyncOnPhase;

	exception
	   when exit_ofdmProcessing	=> put ("normal termination"); New_Line (1);
	   when Error: others		=> Put ("Exception in ofdmProcessor: ");
	                                   Put_Line (Exception_Name (Error));
	end Ofdm_Worker;
