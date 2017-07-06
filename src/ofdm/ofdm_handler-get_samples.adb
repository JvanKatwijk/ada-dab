	separate (ofdm_handler)
	procedure Get_Samples (Out_V      : out complexArray;
	                       Phase_Ind  : Integer) is
	   Amount : Integer   := Out_V' length;
	begin
	   if not Running then
	      raise Exit_ofdmProcessing;
	   end if;

	   if Amount > Buffer_Content then
	      Buffer_Content := Available_Samples;
	      while Running and then
	                       Buffer_Content < Amount loop
	         delay 0.01;
	         Buffer_Content := Available_Samples;
	      end loop;

	      if not Running then
	         raise Exit_OfdmProcessing;
	      end if;
	   end if;

--	we have samples, let's get them
	   Fetch_Samples (Out_V, Amount);
	   Buffer_Content :=  Buffer_Content - Amount;

--	first: adjust frequency. We need Hz accuracy
	   for I in Out_V' Range loop
	      Current_Phase :=
	                  (Current_Phase - Phase_Ind) mod Input_Rate;
	      Out_V (I)             := Out_V (I) *
	                        OscillatorTable (Current_Phase);
	      Signal_Level  := 0.00001 * abs Out_V (I) +
	                                (1.0 - 0.00001) * Signal_Level;
	   end loop;
--
--	Once a second (i.e. after INPUT_RATE samples), we
--	show some data
	   Samplecounter    := Samplecounter + Amount;
	   if Samplecounter >=  Input_Rate then
	      simple_messages. message_queue. Put ((FINE_CORRECTOR_SIGNAL,
	                                            Fine_Corrector));
	      simple_messages. message_queue. Put ((COARSE_CORRECTOR_SIGNAL,
	                                            Coarse_Corrector));
	      Samplecounter := 0;
	   end if; 

	exception
	      when Error: others   => Put ("Exception in getsamples: ");
	                              Put_Line (Exception_Name (Error));
	                              raise;
	end Get_Samples;
