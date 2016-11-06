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
	Fic_Number	: Integer	:= 0;
	Fic_Blocks	: Integer	:= 0;
	Bitbuffer_Out	: byteArray (0 .. 768 - 1);
	Prbs_Vector	: byteArray (0 .. 768 - 1);
	Deconvolver	: Viterbi_Processor (768);
	BitsperBlock	: Integer	:= 2 * header. K (The_Mode);
	subtype	Ficblock	is shortArray (0 .. 2304 - 1);
	subtype Viterbi_Block	is shortArray (0 .. 3072 + 24 - 1);

	procedure depuncture_FICblock (Data_In  : ficBlock;
	                               Data_out : out Viterbi_Block);

	type Kind_of_Data is (Data_Buffer, Stop_Command,
	                      Reset_Command, Restart_Command);
	type fic_data is record
	   command    : Kind_of_Data;
	   ficNo      : Natural;
	   ofdm_data  : ficBlock;
	end record;
	Ofdm_Inputdata	: fic_data;
	package ficBuffer is new Generic_Buffer (fic_data);
	the_ficBuffer : ficBuffer. Buffer (10);
	fic_Locker : locker;

	task Fic_Processor;
--
	procedure Stop is
	   command : fic_data;
	begin
	   command. command   := Stop_Command;
	   command. ficNo     := 0;
	   command. ofdm_data := (Others => 0);
	   the_ficBuffer. Put (command);
	end stop;
--
--
	procedure Reset is
	   command : fic_data;
	begin
	   command. command    := Reset_Command;
	   command. ficNo      := 0;
	   command. ofdm_data  := (Others => 0);
	   the_ficBuffer. Put (command);
	end reset;

	procedure Restart is
	   command : fic_data;
	begin
	   command. command	:= Restart_Command;
	   command. ficNo      := 0;
	   command. ofdm_data  := (Others => 0);
	   the_ficBuffer. Put (command);
	end Restart;

	procedure Data_for_Audioservice (Name_of_Program : String;
	                                 Data            : out audioData) is
	begin
	   fic_Locker. lock;
	   fib_handler. Data_for_AudioService (Name_of_Program, Data);
	   fic_Locker. unlock;
	end Data_for_AudioService;

	function Check_ficCRC	(vector : fib_buffer) return Boolean;

	PI_X	: constant byteArray (0 .. 23) :=
	                          (1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0,
	                           1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0);

	procedure Process_Ficblock (Data   : shortArray;
	                            Blkno  : Integer) is
	begin
	   if Blkno = 2 then
	      Buffer_Index	:= 0;
	      Fic_Number	:= 0;
	   end if;

	   if 2 <= Blkno and then Blkno <= 4 then
	      for I in 0 .. bitsperBlock - 1 loop
	         Ofdm_Inputdata . ofdm_data (Buffer_Index) := Data (I);
	         Buffer_Index	:= Buffer_Index + 1;
	         if Buffer_Index >= 2304 then 
	            Ofdm_Inputdata. command := Data_Buffer;
	            Ofdm_InputData. ficNo   := Fic_Number;
	            the_ficBuffer. Put (Ofdm_inputData);
	            Buffer_Index            := 0;
	            Fic_Number              := Fic_Number + 1;
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
	   buffer_Element  : fic_data;
	   Fic_Number      : Natural      := 0;
	   Viterbi_Data    : Viterbi_Block;
	   Missed_Blocks   : Natural      := 0;
	   Terminate_FIC   : exception;
	   Working_Mode    : Boolean      := false;
	begin
	   loop
	      the_ficBuffer. Get (buffer_Element);
	      if buffer_Element. command = Stop_Command then
	         raise Terminate_FIC;
	      elsif buffer_Element. command = Reset_Command then
	         fic_Locker. lock;
	         fib_handler. reset;
	         fic_Locker. unlock;	                -- ... and continue
	         Working_Mode  := false;
	      elsif buffer_Element. command = Restart_Command then
	         Working_Mode  := true;
	      else
	         if Working_Mode then
	            depuncture_FICblock (buffer_Element. ofdm_data,
	                                 Viterbi_Data);
	                         
	            Fic_Number := buffer_Element. ficno;

--      It is all in, apply:
                    Deconvolver. deconvolve (Viterbi_Data, Bitbuffer_out);
--
--	if everything worked as planned, we now have a
--	768 bit vector containing three FIB's
--	first step: prbs handling
	            for I in BitBuffer_out' Range loop
	               Bitbuffer_out (I) :=
	                      Bitbuffer_out (i) xor Prbs_Vector (i);
	            end loop;

	            for I in Fic_Number * 3 ..  (Fic_Number + 1) * 3 - 1 loop
	               Fic_blocks	:=  Fic_blocks + 1;
	               if Fic_Blocks >= 100 then
	                  Missed_Blocks := 100 -  Missed_Blocks;
	                  simple_messages. message_queue.
	                                  Put ((FIC_RESULTS, Missed_Blocks));
	                  Fic_Blocks  := 0;
	                  Missed_Blocks := 0;
	               end if;
	   
	               declare
	                  LowerBound: Integer :=
	                        Integer (uint16_t (I) mod 3) * 256;
	                  UpperBound: Integer := Lowerbound + 256 - 1;
	                  My_Fib_Buffer	: Fib_Buffer renames
	                         Bitbuffer_out (LowerBound .. UpperBound);
	               begin
--	check the slice of the bitBuffer
	                  if Check_ficCRC (My_Fib_Buffer) then
	                     fic_Locker. lock;
	                     fib_handler. process_FIB (My_Fib_Buffer);
	                     fic_Locker. unlock;
	                  else
	                     Missed_Blocks := Missed_Blocks + 1;
	                  end if;
	               end;
	            end loop;
	         end if;
	      end if;
	   end loop;
	end Fic_Processor;

	function Sync_Reached return Boolean is
	   result : Boolean;
	begin
	   fic_Locker. lock;
	   result := fib_handler. syncReached;
	   fic_Locker. unlock;
	   return result;
	end Sync_Reached;
--
--
	CRC_Polynome	: constant byteArray (0 .. 14) :=
                (0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0);  -- MSB .. LSB

	function Check_ficCRC (Vector : fib_buffer) return Boolean is
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
--	a: First we have 21 blocks with punctured according to PI_16
--	each 128 bit block contains 4 subblocks of 32 bits
--	on which the given puncturing is applied
	   for I in 1 .. 21 loop
	      for K in 0 ..  32 * 4 - 1 loop
	         if get_PCode (16, short (uint16_t (K) mod 32)) /= 0 then
	            Data_Out (Fillpointer) := Data_In (Inputcounter);
	            Inputcounter           := Inputcounter + 1;
	         else -- a real "do not know"	
	            Data_Out (FillPointer) := 0;
	         end if;
	         Fillpointer  := Fillpointer + 1;
	      end loop;
	   end loop;

--	b Second, we have 3 blocks with puncturing according to PI_15
--	each 128 bit block contains 4 subblocks of 32 bits
--	on which the given puncturing is applied
	   for I in 1 .. 3 loop
	      for K in 0 .. 32 * 4 - 1 loop
	         if get_PCode (15, short (uint16_t (K) mod 32)) /= 0 then
	            Data_Out (Fillpointer) := Data_In (Inputcounter);
	            Inputcounter           := Inputcounter + 1;
	         else -- a real "do not know"
	            Data_Out (Fillpointer) := 0;
	         end if;
	         Fillpointer  := Fillpointer + 1;
	      end loop;
	   end loop;

--	we have a final block of 24 bits  with puncturing according to PI_X
--	This block constitues the 6 * 4 bits of the register itself.
	   for K in 0 .. 24 - 1 loop
	      if PI_X (K) /= 0 then
	         Data_Out (Fillpointer) := Data_In (Inputcounter);
	         Inputcounter          := Inputcounter + 1;
	      else
	         Data_Out (Fillpointer) := 0;
	      end if;
	      Fillpointer  := Fillpointer + 1;
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
	   Fic_Number	:= 0;
	end;

end Fic_Handler;

