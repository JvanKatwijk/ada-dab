--
--    Copyright (C) 2016
--    Jan van Katwijk (J.vanKatwijk@gmail.com)
--    Lazy Chair Computing
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
--      The number of bits to be processed per incoming block
--      is 2 * p -> K, which still depends on the Mode.
--      for Mode I it is 2 * 1536, for Mode II, it is 2 * 384,
--      for Mode III it is 192, Mode IV gives 2 * 768.
--      for Mode II we will get the 2304 bits after having read
--      the 3 FIC blocks,
--      for Mode IV we will get 3 * 2 * 768 = 4608, i.e. two resulting blocks
--      Note that Mode III is NOT supported
--
with prottables;	use prottables;
with fib_handler;	use fib_handler;
with Interfaces;	use Interfaces;
with viterbi_handler;	use viterbi_handler;
with Text_IO; 		use Text_IO;
with simple_messages;	use simple_messages;
with Generic_Buffer;

--	This version is the asynchronous one
package body Fic_Handler is
	Buffer_Index	: Integer	:= 0;
--	Fic_Number	: Integer	:= 0;
	Fic_Blocks	: Integer	:= 0;
	Bitbuffer_Out	: byteArray (0 .. 768 - 1);
	Prbs_Vector	: byteArray (0 .. 768 - 1);
	Deconvolver	: Viterbi_Processor (768);
	BitsperBlock	: Integer	:= 2 * header. K (The_Mode);
	subtype	Ficblock	is shortArray (0 .. 2304 - 1);
	subtype Viterbi_Block	is shortArray (0 .. 3072 + 24 - 1);
	ofdm_data	: Ficblock;
	procedure depuncture_FICblock (Data_In  : ficBlock;
	                               Data_out : out Viterbi_Block);

	task Fic_Processor is
	   entry Restart;
	   entry Stop;
	   entry Reset;
	   entry Data_for_Audioservice (Program_Name : String;
	                                Data         : out audioData);
	   entry Handle (Fic_Number: Natural; Data : Ficblock);
	   entry Sync_Reached (result : out Boolean);
	end Fic_Processor;
--
	procedure Stop is
	begin
	   Fic_Processor. Stop;
	   abort Fic_Processor;
	end stop;
--
	procedure Reset is
	begin
	   Fic_Processor. Reset;
	end reset;

	procedure Restart is
	begin
	   Fic_Processor. Restart;
	end Restart;

	function Sync_Reached return Boolean is
	   result : Boolean := false;
	begin
	   Fic_Processor. Sync_Reached (result);
	   return result;
	end Sync_Reached;

	procedure Data_for_Audioservice (Name_of_Program : String;
	                                 Data            : out audioData) is
	begin
	   Fic_Processor. Data_for_AudioService (Name_of_Program, Data);
	end Data_for_AudioService;

	function Check_ficCRC	(vector : fib_buffer) return Boolean;

	PI_X	: constant byteArray (0 .. 23) :=
	                          (1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0,
	                           1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0);

	procedure Process_Ficblock (Data   : shortArray;
	                            Blkno  : Natural) is
	begin
	   if Blkno = 2 then
	      Buffer_Index	:= 0;
--	      Fic_Number	:= 0;
	   end if;

	   if 2 <= Blkno and then Blkno <= 4 then
	      for I in 0 .. bitsperBlock - 1 loop
	         ofdm_data (Buffer_Index) := Data (I);
	         Buffer_Index	:= Buffer_Index + 1;
	         if Buffer_Index >= 2304 then 
	            Fic_Processor. Handle (0, ofdm_data);
--	            Fic_Processor. Handle (Fic_Number, ofdm_data);
	            Buffer_Index            := 0;
--	            Fic_Number              := Fic_Number + 1;
	         end if;
	      end loop;
	   else
	      put ("we should not be here at all with ");	
	      Put_Line (Integer' Image (Blkno));
	   end if;
	end Process_FicBlock;
--
--	we have a vector of 2304 (0 .. 2303) soft bits that has
--	to be de-punctured and de-conv-ed into a block of 768 bits
--	In this approach we first create the full 3072 block (i.e.
--	depuncturing, and then we apply the deconvolution

	task body Fic_Processor is
--	   Fic_Number      : Natural      := 0;
	   Viterbi_Data    : Viterbi_Block;
	   Missed_Blocks   : Natural      := 0;
	   Terminate_FIC   : exception;
	   running	   : Boolean      := true;
	begin
	   while running loop
	      select
	         accept Reset do
	            fib_handler. Reset;
	         end;
	      or
	         accept Stop do
	            running := false;
	         end;
	      or
	         accept Restart;
	      or
	         accept Sync_Reached (result : out Boolean) do
	            result := fib_handler. syncReached;
	         end Sync_Reached;
	      or
	         accept Data_for_Audioservice (Program_Name : String;
	                                       Data         : out audioData) do
	            fib_handler. Data_for_Audioservice (Program_Name, Data);
	         end Data_for_Audioservice;
	      or
	         accept Handle (Fic_Number : Natural; Data : Ficblock) do
	            depuncture_FICblock (Data, Viterbi_Data);
	         end Handle;
                 Deconvolver. deconvolve (Viterbi_Data, Bitbuffer_out);
--
--	if everything worked as planned, we now have a
--	768 bit vector containing three FIB's
--	first step: prbs handling
	         for I in BitBuffer_out' Range loop
	            Bitbuffer_out (I) :=
	                 Bitbuffer_out (I) xor Prbs_Vector (I);
	         end loop;

--	second step, look at the individual 3 FIB blocks
--	in the Bitbuffer_out vector of 768 bits
	         for I in 0 .. 2 loop
	            Fic_blocks	:=  Fic_blocks + 1;
	            if Fic_Blocks >= 100 then
	               Missed_Blocks := 100 -  Missed_Blocks;
	               simple_messages. message_queue.
	                               Put ((FIC_RESULTS, Missed_Blocks));
	               Fic_Blocks  := 0;
	               Missed_Blocks := 0;
	            end if;
	   
	            declare
	               My_Fib_Buffer : Fib_Buffer renames
	                         Bitbuffer_out (I * 256 .. (I + 1) * 256 - 1);
	            begin
--	check the slice of the bitBuffer
	               if Check_ficCRC (My_Fib_Buffer) then
	                  fib_handler. process_FIB (My_Fib_Buffer);
	               else
	                  Missed_Blocks := Missed_Blocks + 1;
	               end if;
	            end;
	         end loop;
	      end select;
	   end loop;
	end Fic_Processor;
--
--
	CRC_Polynome	: constant byteArray (0 .. 14) :=
                (0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0);  -- MSB .. LSB

	function Check_ficCRC (Vector : Fib_Buffer) return Boolean is
	   Buffer:	byteArray (0 .. 15)	:= (others => 1);
	   Sum:         Integer		:= 0;
	   Temp:        fib_buffer	:= vector;
	begin
	   for I in Temp' Last - 16 + 1 .. Temp' Last loop
	      Temp (I) := Temp (I) xor 1;
	   end loop;

	   for I in Temp' range loop
	      if (buffer (0) xor temp (I)) = 1 then
	         for F in 0 .. 15 - 1 loop
	            Buffer (f) := CRC_Polynome (F) xor buffer (F + 1);
	         end loop;
	         Buffer (15) := 1;
	      else
	         Buffer (0 .. 14) := Buffer (1 .. 15);
	         Buffer (15) := 0;
	      end if;
	   end loop;

	   for I in Buffer' range loop
	      Sum	:= Sum + Integer (Buffer (i));
	   end loop;

	   return Sum = 0;
	end Check_ficCRC;
--
--	Depuncture according to the manual, i.e. data block from the
--	fic assembly (2304 values) is input, viterbi block (3072 + ..)
--	is output
	procedure depuncture_FICblock (Data_In  : ficBlock;
	                               Data_Out : out Viterbi_Block) is
	   Inputcounter    : Natural      := ficBlock' First;
	   Fillpointer     : Natural      := 0;
	begin
--	a block of 2304 bits is considered to be a codeword
--	In three 2048 * 2 bit words - as is the case in Mode 1 -
--	we have 4 blocks of 2304 data elements.
--	a: First we have 21 blocks punctured according to PI_16
--	each 128 bit block contains 4 subblocks of 32 bits
--	on which the given puncturing is applied
	   Data_Out := (Others => 0);
	   for I in 1 .. 21 loop
	      for K in 0 .. 3 loop
	         for L in 0 .. 31 loop
	            if get_PCode (16, short (L)) /= 0 then
	               Data_Out (Fillpointer) := Data_In (Inputcounter);
	               Inputcounter           := Inputcounter + 1;
	            end if;
	            Fillpointer  := Fillpointer + 1;
	         end loop;
	      end loop;
	   end loop;

--	b Second, we have 3 blocks with puncturing according to PI_15
--	each 128 bit block contains 4 subblocks of 32 bits
--	on which the given puncturing is applied
	   for I in 1 .. 3 loop
	      for K in 0 .. 3 loop
	         for L in 0 .. 31 loop
	            if get_PCode (15, short (L)) /= 0 then
	               Data_Out (Fillpointer) := Data_In (Inputcounter);
	               Inputcounter           := Inputcounter + 1;
	            end if;
	            Fillpointer  := Fillpointer + 1;
	         end loop;
	      end loop;
	   end loop;

--	we have a final block of 24 bits  with puncturing according to PI_X
--	This block constitues the 6 * 4 bits of the register itself.
--	   for K in 0 .. 24 - 1 loop
--	      if PI_X (K) /= 0 then
--	         Data_Out (Fillpointer) := Data_In (Inputcounter);
--	         Inputcounter          := Inputcounter + 1;
--	      end if;
--	      Fillpointer  := Fillpointer + 1;
--	   end loop;
	   for K in 0 .. 5 loop
	      Data_Out (FillPointer)     := Data_In (InputCounter);
	      Data_Out (FillPointer + 1) := Data_in (InputCounter + 1);
	      FillPointer                := FillPointer + 4;
	      InputCounter               := InputCounter + 2;
	   end loop;
	end depuncture_FICblock;
begin
--	here we create the prbs vector once 
	declare
	   Shift_Register: byteArray (0 .. 8) := (others => 1);
	begin
	   for I in 0 .. 768 - 1 loop
	      Prbs_Vector (i) := Shift_Register (8) xor Shift_Register (4);
	      for J in reverse 1 .. 8 loop
	         Shift_Register (J) := Shift_Register (J - 1);
	      end loop;
	      Shift_Register (0) := Prbs_Vector (I);
	   end loop;
	   Buffer_Index	:= 0;
--	   Fic_Number	:= 0;
	end;

end Fic_Handler;

