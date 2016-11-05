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
--	header file for many, many global definitions etc
with Ada. Numerics. Generic_Complex_Types;
with Ada. Numerics. Generic_Elementary_Functions;
with Ada. Unchecked_Deallocation;
with Interfaces;
with Interfaces. C;

package Header is
	package complexTypes is new Ada. Numerics. Generic_Complex_Types (float);
	package Math is new Ada. Numerics. Generic_Elementary_Functions (float);
	use Math;
	Input_Rate	: constant Integer := 2048000;
	inputRate	: constant Integer := 2048000;
	type oscillatorRange is mod inputRate;

	subtype Byte is Interfaces. Unsigned_8;
	subtype uint8_t is Interfaces. Unsigned_8;
	subtype int8_t is Interfaces. Integer_8;
	type byteArray is Array (Natural range <>) of Byte;
	pragma Convention (C, byteArray);
	type byteArray_P is access all byteArray;
	procedure Free_byteArray is new Ada. Unchecked_Deallocation (
	          Object => byteArray, Name => byteArray_P);

	type intArray is Array (integer range <>) of integer;
	pragma Convention (C, intArray);
	type intArray_P is access all intArray;
	procedure Free_intArray is new Ada. Unchecked_Deallocation (
	          Object => intArray, Name => intArray_P);

	subtype short is short_Integer;
	type shortArray is Array (Integer Range <>) of short_Integer;
	pragma Convention (C, shortArray);
	type shortArray_P is access all shortArray;
	procedure Free_shortArray is new Ada. Unchecked_Deallocation (
	          Object => shortArray, Name => shortArray_P);

	type shortshortArray is Array (short_Integer Range <>) of short_Integer;
	pragma Convention (C, shortshortArray);
	type shortshortArray_P is access all shortshortArray;
	procedure Free_shortshortArray is new Ada. Unchecked_Deallocation (
	          Object => shortshortArray, Name => shortshortArray_P);

	type floatArray is array (integer range <>) of Float;
	pragma Convention (C, floatArray);
	type floatArray_P is access all floatArray;
	procedure Free_floatArray is new Ada. Unchecked_Deallocation (
	          Object => floatArray, Name => floatArray_P);

	type complexArray is array (integer range <>) of complexTypes. complex;
	pragma Convention (C, complexArray);
	type complexArray_P is access complexArray;
	procedure Free_complexArray is new Ada. Unchecked_Deallocation (
	          Object => complexArray, Name => complexArray_P);

--	type Root is tagged private;
	type Dabmode is (Mode_1, Mode_2, Mode_3, Mode_4);
	function	L	(Mode: Dabmode) return integer;
	function	K	(Mode: Dabmode) return integer;
	function	T_F	(Mode: Dabmode) return integer;
	function	T_null	(Mode: Dabmode) return integer;
	function	T_s	(Mode: Dabmode) return integer;
	function	T_u	(Mode: Dabmode) return integer;
	function	T_g	(Mode: Dabmode) return integer;
	function	Carrier_Diff (Mode: Dabmode) return integer;
	function	kHz	(Frequency: Integer) return Integer;
	function	mHz	(Frequency: Integer) return Integer;
	function	cifs	(The_Mode : dabMode) return Integer;
	M_PI : constant := 3.1415926535;

	subtype uint16_t is Interfaces. Unsigned_16;
	subtype uint32_t is Interfaces. Unsigned_32;
	subtype uint64_t is Interfaces. Unsigned_64;
	type Dabband	is (BAND_III, L_BAND);

	type dataMode	is (DAB, DAB_PLUS);
	subtype programmeName is String (1 .. 16);
	type audioData is record
	   dataisThere	: Boolean;
	   subchId	: short_Integer;
	   startAddr	: short_Integer;
	   uepFlag	: short_Integer;
	   protLevel	: short_Integer;
	   length	: short_Integer;
	   bitRate	: short_Integer;
	   ASCTy	: short_Integer;
	   language	: short_Integer;
	   programType	: short_Integer;
	end record;
	type fftMode is (FORWARD, BACKWARD);
	for fftMode use (FORWARD => 0, BACKWARD => 1);

--
	type Get_Samples_Access is access procedure (X : out complexArray;
	                                             Y: out Integer);
	type Available_Samples_Access is access function return Integer;

	type process_mscBlock_Access is access procedure (Fbits : shortArray;
	                                                  Blkn  : Integer);
	type process_ficBlock_Access is access procedure (Fbits : shortArray;
	                                                  Blkn  : Integer);
	type Sync_Reached_Access is access function return Boolean;
	protected type locker is
	   entry lock;
	   procedure unlock;
	private
	   count:	Natural := 1;
	end;
end Header;

