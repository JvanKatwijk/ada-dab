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
with Interfaces; use Interfaces;
package viterbi_old is
	type viterbi_data (blockLength: Integer) is
	          new Ada. Finalization. Controlled with private;
	type viterbi_data_P is access all viterbi_data;

	procedure	deconvolve (Object: in out viterbi_data;
	                    sym: shortArray; result: out ByteArray);
private
	K	: constant	:=   7;
	Poly1	: constant	:=  91;		-- 0133
	Poly2	: constant	:= 121;		-- 0171
	Poly3	: constant	:= 101;		-- 0145
	Poly4	: constant	:=  91;		-- 0133
	NumofStates: constant	:=  64;		-- 1 << (K - 1)

	type intBlock is array (short_Integer range <>,
	                        short_Integer range <>) of Integer;
	type intBlock_P is access intBlock;
	procedure Free_intBlock is new Ada. Unchecked_Deallocation (
		Object => intBlock, Name => intBlock_P);

	type uintBlock is
	             array (short_Integer range <>,
	                    short_Integer range <>) of uint16_t;
	type uintBlock_P is access uintBlock;
	procedure Free_uintBlock is new Ada. Unchecked_Deallocation (
		Object => uintBlock, Name => uintBlock_P);

	type uintArray	is Array (short_Integer range <>) of uint16_t;
	type uintArray_P is access all uintArray;
	procedure Free_uintArray is new Ada. Unchecked_Deallocation (
		Object => uintArray, Name => uintArray_P);

	type viterbi_data (blockLength: Integer) is
	          new Ada. Finalization. Controlled with
	record
		transCosts	: intBlock_P;
		history		: uintBlock_P;
		sequence	: uintArray_P;
		poly1_Table	: byteArray_P;
		poly2_Table	: byteArray_P;
		poly3_Table	: byteArray_P;
		poly4_Table	: byteArray_P;
	end record;

	predecessor_for_0	: uintArray (0 .. NumofStates - 1);
	predecessor_for_1	: uintArray (0 .. NumofStates - 1);
	procedure Initialize (Object: in out viterbi_data);
	procedure Finalize   (Object: in out viterbi_data);
	function bitFor (state	: uint16_t;
	                 poly	: uint16_t; 
	                 bit	: uint8_t) return uint8_t;
end viterbi_old;

