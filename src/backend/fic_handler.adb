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
with fic_handler;
with prottables;	use prottables;
with fib_handler;	use fib_handler;
with Interfaces;	use Interfaces;
with viterbi_handler;	use viterbi_handler;
with Text_IO; 		use Text_IO;
with simple_messages;	use simple_messages;
package body fic_handler is
bufferIndex	: Integer	:= 0;
ficno		: Integer	:= 0;
ficBlocks	: Integer	:= 0;
ficMissed	: Integer	:= 0;
bitBuffer_out	: byteArray (0 .. 768 - 1);
prbs_vector	: byteArray (0 .. 768 - 1);
deconvolver	: viterbiProcessor (768);
bitsperBlock	: Integer	:= 2 * 1536;	-- default
subtype	ficBlock	is shortArray (0 .. 2304 - 1);
ofdm_input	: ficBlock;

task fic_processor is
	entry process_ficInput (buffer: ficBlock; ficno	: Integer);
	entry stop;
	entry reset;
	entry dataforAudioService (s: programmeName;
	                           d: out audioData);
end fic_processor;

procedure set_bitsperBlock (mode: dabMode) is
begin
	bitsperBlock	:= 2 * header. K (mode);
end;
--
--
procedure stop is
begin
	fic_processor. stop;
end stop;
--
--
procedure reset is
begin
	fic_processor. reset;
end reset;

procedure dataforAudioService (s: programmeName; d: out audioData) is
begin
	fic_processor. dataforAudioService (s, d);
end dataforAudioService;

function check_ficCRC	(vector : fib_buffer) return Boolean;

PI_X	: constant byteArray (0 .. 23) :=
	(1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0,
	 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0);

procedure process_ficBlock (data	: shortArray;
	                    blkno	: Integer) is
begin
	if blkno = 2
	then
	   bufferIndex	:= 0;
	   ficno	:= 0;
	end if;

	if 2 <= blkno and then blkno <= 4
	then
	   for i in 0 .. bitsperBlock - 1 loop
	      ofdm_input (bufferIndex) := data (i);
	      bufferIndex	:= bufferIndex + 1;
	      if bufferIndex >= 2304
	      then 
	         fic_processor. process_ficInput (ofdm_input, ficno);
	         bufferIndex	:= 0;
	         ficno		:= ficno + 1;
	      end if;
	   end loop;
	else
	   put ("we should not be here at all with ");	
	   Put_Line (Integer' Image (blkno));
	   null;
	end if;
end process_ficBlock;

--
--	we have a vector of 2304 (0 .. 2303) soft bits that has
--	to be de-punctured and de-conv-ed into a block of 768 bits
--	In this approach we first create the full 3072 block (i.e.
--	depuncturing, and then we apply the deconvolution

task body fic_processor is
localBuffer	: ficBlock;
ficNumber	: Integer;
inputCounter	: Integer	:= ficBlock' First;
fillPointer	: Integer	:= 0;
viterbiBlock	: shortArray (0 .. 3072 + 24 - 1);
ficRatio	: Integer;
ficEnd		: exception;
begin
	loop
	   select
	      accept process_ficInput (buffer	: ficBlock;
	                               ficno	: Integer) do
	         localBuffer	:= buffer;
	         ficNumber	:= ficno;
	      end process_ficInput;
	      fillPointer	:= 0;
	      inputCounter	:= 0;
--	a block of 2304 bits is considered to be a codeword
--	In three 2048 * 2 bit words - as is the case in Mode 1 -
--	we have 4 blocks of 2304 data elements.
--	a: First we have 21 blocks with punctured according to PI_16
--	each 128 bit block contains 4 subblocks of 32 bits
--	on which the given puncturing is applied
	      for i in 1 .. 21 loop
	         for k in 0 ..  32 * 4 - 1 loop
	            if get_PCode (16, short (uint16_t (k) mod 32)) /= 0
	            then
                       viterbiBlock (fillPointer) := localBuffer (inputCounter);
	               inputCounter := inputCounter + 1;
                    else -- a real "do not know"	
                       viterbiBlock (fillPointer) := 0;
	            end if;
	            fillPointer	:= fillPointer + 1;
                 end loop;
              end loop;

--	b Second, we have 3 blocks with puncturing according to PI_15
--	each 128 bit block contains 4 subblocks of 32 bits
--	on which the given puncturing is applied
              for i in 1 .. 3 loop
                 for k in 0 .. 32 * 4 - 1 loop
                    if get_PCode (15, short (uint16_t (k) mod 32)) /= 0
	            then
                       viterbiBlock (fillPointer) := localBuffer (inputCounter);
	               inputCounter	:= inputCounter + 1;
	            else -- a real "do not know"
                       viterbiBlock (fillPointer) := 0;
	            end if;
	            fillPointer	:= fillPointer + 1;
	         end loop;
	      end loop;

--	we have a final block of 24 bits  with puncturing according to PI_X
--	This block constitues the 6 * 4 bits of the register itself.
              for k in 0 .. 24 - 1 loop
                 if PI_X (k) /= 0
	         then
                    viterbiBlock (fillPointer) := localBuffer (inputCounter);
	            inputCounter	:= inputCounter + 1;
                 else
                    viterbiBlock (fillPointer) := 0;
	         end if;
	         fillPointer	:= fillPointer + 1;
	      end loop;

--      It is all in, apply:
              deconvolver. deconvolve (viterbiBlock, bitBuffer_out);
--
--	if everything worked as planned, we now have a
--	768 bit vector containing three FIB's
--	first step: prbs handling
	      for i in 0 .. 768 - 1 loop
	         bitBuffer_out (i) := bitBuffer_out (i) xor prbs_vector (i);
	      end loop;

	      for i in ficno * 3 ..  (ficno + 1) * 3 - 1 loop
	         ficBlocks	:=  ficBlocks + 1;
	         if ficBlocks >= 100
	         then
	            ficRatio := 100 -  ficMissed;
	            simple_messages. message_queue.
	                                  Put ((FIC_RESULTS, ficRatio));
	            ficBlocks	:= 0;
	            ficMissed	:= 0;
	         end if;
	   
	         declare
	            lowerBound	: Integer := Integer (uint16_t (i) mod 3) * 256;
	            upperBound	: Integer := lowerBound + 256 - 1;
	            my_fib_buffer	: fib_buffer renames
	                         bitBuffer_out (lowerBound .. upperBound);
	         begin
--	check the slice of the bitBuffer
	            if check_ficCRC (my_fib_buffer)
	            then
	               fib_handler. process_FIB (my_fib_buffer);
	            else
	               ficMissed :=  ficMissed + 1;
	            end if;
	         end;
	      end loop;
	   or
	      accept dataforAudioService (s: programmeName;
	                                  d: out audioData) do
	         fib_handler. dataforAudioService (s, d);
	      end dataforAudioService;
	   or
	      accept stop;
	      raise ficEnd;
	   or
	      accept reset do
	         put_line ("de reset is gehoord");
	         fib_handler. reset;
	         put_line ("aan het resetten");
	      end reset;
	   end select;
	end loop;
end fic_processor;

function syncReached return Boolean is
begin
	return fib_handler. syncReached;
end syncReached;
--
--
crcPolynome	: constant byteArray (0 .. 14) :=
    (0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0);  -- MSB .. LSB

function check_ficCRC (vector : fib_buffer) return Boolean is
buffer	:	byteArray (0 .. 15)	:= (others => 1);
Sum	:	Integer		:= 0;
temp	:	fib_buffer	:= vector;
begin

	for i in temp' Last - 16 + 1 .. temp' Last loop
	   temp (i) := temp (i) xor 1;
	end loop;

	for i in temp' range loop
	   if (buffer (0) xor temp (i)) = 1
	   then
	      for f in 0 .. 15 - 1 loop
	         buffer (f) := crcPolynome (f) xor buffer (f + 1);
	      end loop;
	      buffer (15) := 1;
	   else
	      buffer (0 .. 14) := buffer (1 .. 15);
	      buffer (15) := 0;
	   end if;
	end loop;

	for i in buffer' range loop
	   Sum	:= Sum + Integer (buffer (i));
	end loop;

	return Sum = 0;
end check_ficCRC;
begin
	declare
	   shiftRegister: byteArray (0 .. 8) := (others => 1);
	begin
	   for i in 0 .. 768 - 1 loop
	      prbs_vector (i) := shiftRegister (8) xor shiftRegister (4);
	      for j in reverse 1 .. 8 loop
	         shiftRegister (j) := shiftRegister (j - 1);
	      end loop;
	      shiftRegister (0) := prbs_vector (i);
	   end loop;
	   bufferIndex	:= 0;
	   ficno	:= 0;
	end;
end fic_handler;

