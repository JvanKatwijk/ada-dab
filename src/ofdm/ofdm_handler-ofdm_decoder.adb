	separate (ofdm_handler)
	task body Ofdm_Decoder is
	   Block      : Natural;
	   Help       : Tu_Sized_Buffer;
	   Reference  : Tu_Sized_Buffer;
	   Ibits      : Ibit_Vector;
	   package my_freqInterleaver is new freq_interleaver (The_Mode);
	   procedure process_Token (Buffer  : in out Tu_Sized_Buffer) is
	   begin
	      new_fft. do_FFT (Buffer);

	      for I in 0 .. Carriers - 1 loop
	         declare
	            Index   : Integer := my_freqInterleaver. Map_In (I);
	            R1      : complexTypes. complex;
	         begin
	            if Index < 0 then
	               Index := Index + Tu;
	            end if;
	            R1 := Buffer (Index) * conj (Reference (Index));
--	Recall:  with this viterbi decoder
--	we have 127 = max pos, -127 = max neg, so we scale
	            if abs R1 < 0.0005 then
	               Ibits (I)            := 0;
	               Ibits (Carriers + i) := 0;
	            else
	               Ibits (I)	     :=
	                     int16_t (R1. Re / abs R1 * 127.0);
	               Ibits (Carriers + I)  :=
	                     int16_t (R1. Im / abs R1 * 127.0);
	            end if;
	         end;
	      end loop;
--
--	save the "old" buffer as a reference for the next round
	      Reference := Buffer;
	   end process_Token;
	begin
	   accept start;
	   put_line ("Decoder started");
	   while Running loop
	      select
	         accept Put (Blkno : Natural; Data : Tu_Sized_Buffer) do
	            Help  := Data;
	            Block := Blkno;
	         end Put;
	         process_Token (Help);
	         if Block <= 4 then
	            process_ficBlock (Ibits, Block);
	         else
	            process_mscBlock (Ibits, Block);
	         end if;
	      or
	         accept Block_0 (Data: Tu_Sized_Buffer) do
	            Reference := Data;
	         end;
	      end select;
	   end loop;
	
	exception
	   when Error: others           => Put ("Exception in ofdm decoder: ");
                                           Put_Line (Exception_Name (Error));

	end Ofdm_Decoder;
