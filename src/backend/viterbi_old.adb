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
with viterbi_old;
with Text_IO; use Text_IO;
package body viterbi_old is

procedure Initialize (Object: in out viterbi_data) is
blockLength:	Integer renames Object. blockLength;
begin
	Object. transCosts	:= new intBlock (short_Integer Range 0 ..
	                                   short_Integer (blockLength + 6 - 1),
	                              short_Integer Range 0 .. NumofStates - 1);
	Object. history	:=	new uintBlock (short_Integer Range 0 ..
	                                   short_Integer (blockLength + 6 - 1),
	                             short_Integer Range 0 .. NumofStates - 1);
	Object. sequence	:= new uintArray (short_Integer Range 0 ..
	                                   short_Integer (blockLength + 6 - 1));

	for i in Object. sequence' Range loop
	   Object. sequence (i)		:= 0;
	   for j in short_Integer Range 0 .. NumofStates - 1 loop
	      Object. transCosts (i, j) := 0;
	      Object. history	 (i, j) := 0;
	   end loop;
	end loop;

--	These tables give a mapping from (state * bit * Poly -> outputbit)
	Object. poly1_table	:= new ByteArray (0 .. 2 * NumofStates - 1);
	Object. poly2_table	:= new ByteArray (0 .. 2 * NumofStates - 1);
	Object. poly3_table	:= new ByteArray (0 .. 2 * NumofStates - 1);
	Object. poly4_table	:= new ByteArray (0 .. 2 * NumofStates - 1);
	for i in 0 .. 1 loop
	   for j in 0 .. NumofStates - 1 loop
	      Object. poly1_table (i * NumofStates + j) :=
	                          bitFor (uint16_t (j), Poly1, uint8_t (i));
	      Object. poly2_table (i * NumofStates + j) :=
	                          bitFor (uint16_t (j), Poly2, uint8_t (i));
	      Object. poly3_table (i * NumofStates + j) :=
	                          bitFor (uint16_t (j), Poly3, uint8_t (i));
	      Object. poly4_table (i * NumofStates + j) :=
	                          bitFor (uint16_t (j), Poly4, uint8_t (i));
	   end loop;
	end loop;

	for i in  short_Integer Range 0 .. short_Integer (NumofStates - 1) loop
	   predecessor_for_0 (i) :=
	              (Interfaces. Shift_Left (uint16_t (i), 1) + 00) and
	                                            uint16_t (NumofStates - 1);
	   predecessor_for_1 (i) :=
	              (Interfaces. Shift_Left (uint16_t (i), 1) + 01) and
	                                            uint16_t (NumofStates - 1);
	end loop;
end Initialize;

procedure Finalize (Object: in out viterbi_data) is
begin
	Free_intBlock	(Object. transCosts);
	Free_uintBlock	(Object. history);
	Free_uintArray  (Object. sequence);
	Free_byteArray  (Object. poly1_table);
	Free_byteArray  (Object. poly2_table);
	Free_byteArray  (Object. poly3_table);
	Free_byteArray  (Object. poly4_table);
end Finalize;
--
--	Just a local one
function costsFor (Object	: in out viterbi_data;
	           lIndex	: uint16_t;
	           sym		: shortArray;
	           index	: short_Integer) return Integer is

res		: Integer	:= 0;
targetBit	: uint8_t	:= 0;
begin
	targetBit	:= Object. poly1_table (Integer (lIndex));
	if targetBit = 0
	then
	   res	:= res + 256 - Integer (sym (Integer (index) + 0));
	else
	   res	:= res + 256 + Integer (sym (Integer (index) + 0));
	end if;

	targetBit	:= Object. poly2_table (Integer (lIndex));
	if targetBit = 0
	then
	   res	:= res + 256 - Integer (sym (Integer (index) + 1));
	else
	   res 	:= res + 256 + Integer (sym (Integer (index) + 1));
	end if;

	targetBit	:= Object. poly3_table (Integer (lIndex));
	if targetBit = 0
	then
	   res	:= res + 256 - Integer (sym (Integer (index) + 2));
	else
	   res	:= res + 256 + Integer (sym (Integer (index) + 2));
	end if;

	targetBit	:=  Object. poly4_table (Integer (lIndex));
	if targetBit = 0
	then
	   res	:= res + 256 - Integer (sym (Integer (index) + 3));
	else
	   res	:= res + 256 + Integer (sym (Integer (index) + 3));
	end if;

	return res;
end costsFor;
--
--	and another local one
--	as an aid, we give a function "bitFor" that, given
--	the register state, the polynome and the bit to be inserted
--	returns the bit coming from the engine

function bitFor (state	: uint16_t;
	         poly	: uint16_t;  bit: uint8_t) return uint8_t is
Register	: uint16_t;
resBit		: uint8_t	:= 0;
begin
--	the register after shifting "bit" in would be:
	Register	:= (if bit = 0 then state else state + NumofStates);
	Register	:= Register and poly;

--	now for the individual bits
	for i  in  0 .. K loop
	   resBit	:= resBit xor  uint8_t (Register and 01);
	   Register	:= Interfaces. Shift_Right (Register, 1);
	end loop;

	return resBit;
end bitFor;

--	sym is the sequence of soft bits
--	its length = 4 * blockLength + 4 * 6
procedure	deconvolve (Object: in out viterbi_data;
	                    sym: shortArray; result: out ByteArray) is
bestState	: uint16_t;
prev_0		: uint16_t;
prev_1		: uint16_t;
costs_0		: Integer;
costs_1		: Integer;
minimalCosts	: Integer;
begin
--	first step is to "pump" the soft bits into the state machine
--	and compute the cost matrix.
--	we assume the overall costs for state 0 are zero
--	and remain zero

	for i in short_Integer Range 1 ..
	           short_Integer (Object. blockLength + 6 - 1) loop
	   for cState in short_Integer Range 0 ..
	                        short_Integer (NumofStates / 2 - 1) loop
--	we know that entrybit = 0 here
	      prev_0	:= predecessor_for_0 (cState);
	      prev_1	:= predecessor_for_1 (cState);

--	we compute the minimal costs, based on the costs of the
--	prev states, and the additional costs of arriving from
--	the previous state to the current state with the symbol "sym"
--	entrybit - 0, so the index for the cost function is prev_xx

	      costs_0 := Object. transCosts (i - 1,  short_Integer (prev_0)) +
	                    Object. costsFor (prev_0, sym , 4 * (i - 1));
	      costs_1 :=  Object. transCosts (i - 1,  short_Integer (prev_1)) +
	                    Object. costsFor (prev_1, sym, 4 * (i - 1));
	      if costs_0 < costs_1
	      then
	         Object.transCosts (i, cState) 	:= costs_0;
	         Object.history    (i, cState)	:= prev_0;
	      else
	         Object.transCosts (i, cState) 	:= costs_1;
	         Object.history    (i, cState)	:= prev_1;
	      end if;
	   end loop;

	   for cState in short_Integer Range NumofStates / 2 .. numofStates - 1 loop
--	entryBit = 1
	      prev_0	:= predecessor_for_0 (cState);
	      prev_1	:= predecessor_for_1 (cState);
--	we compute the minimal costs, based on the costs of the
--	prev states, and the additional costs of arriving from
--	the previous state to the current state with the symbol "sym"
--
--	entrybit is here "1", so the index is id cost function
--	is prev_xx + NumofStates
	      costs_0 := Object. transCosts (i - 1, short_Integer (prev_0)) +
	                 Object. costsFor (prev_0 + NumofStates, sym, 4 * (i - 1));
	      costs_1 := Object. transCosts (i - 1,  short_Integer (prev_1)) +
	                 Object. costsFor (prev_1 + NumofStates, sym, 4 * (i - 1));
	      if costs_0 < costs_1
	      then
	         Object. transCosts (i, cState) := costs_0;
	         Object. history    (i, cState) := prev_0;
	      else
	         Object. transCosts (i, cState) := costs_1;
	         Object. history    (i, cState) := prev_1;
	      end if;
	   end loop;
	end loop;

--
--	Once all costs are computed, we can look for the minimal cost
--	Our "end state" is somewhere in column blockLength + 6
	minimalCosts	:= Integer' Last;
	bestState	:= 0;
	for i in short_Integer Range 0 .. NumofStates - 1 loop
	   if Object. transCosts (short_Integer (Object. blockLength), i) < minimalCosts
	   then
	      minimalCosts	:= Object. transCosts (short_Integer (Object. blockLength), i);
	      bestState 	:=  uint16_t (i);
	   end if;
	end loop;

	Object. sequence (short_Integer (Object. blockLength))	:= bestState;
--
--	Trace backgoes back to state 0, and builds up the
--	sequence of decoded symbols
--
	for i in reverse short_Integer Range 1 ..
	                        short_Integer (Object. blockLength + 6 - 1) loop 
	   Object. sequence (i - 1) 	:=
	                 Object. history (i,
	                              short_Integer (Object. sequence (i)));
	end loop;

	for i in  short_Integer Range 1 ..
	              short_Integer (Object. blockLength) loop 
	   result (Integer (i - 1)) :=
	      (if Object. sequence (i) >= NumofStates / 2 then 01  else 00);
	end loop;
end deconvolve;
end viterbi_old;

