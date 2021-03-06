--
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
--	straightforward wrapper around the generic viterbi decoder from
--	by the spiral project
with header;	use header;
with System;	use System;
with Ada. Finalization;
with Ada. Unchecked_Deallocation;
with Interfaces; 
with Interfaces. C; 

package Viterbi_Handler is
	type Viterbi_Processor (WordLength : Natural) is
	          new Ada. Finalization. Controlled with private;
	type Viterbi_Processor_P is access all Viterbi_Processor;

	procedure Deconvolve (Object : in out Viterbi_Processor;
	                      input  : shortArray;
	                      output : out byteArray);

private
	type c_intArray is Array (Natural Range <>) of Interfaces. C. int;
	type c_intArray_P is access all c_intArray;
	procedure Free_c_intArray is
	               new Ada. Unchecked_Deallocation (
	                                    Object => c_intArray,
	                                    Name => c_intArray_P);
	type Viterbi_Processor (WordLength: Natural) is
	          new Ada. Finalization. Controlled with 
	   record
	      Handler:   System. Address;
	      isOK:      Boolean;
	      Symbols:   c_intArray_P;
	   end record;

	K:	constant Integer	:= 7;
	Rate:	constant Integer	:= 4;
	POLYS:	constant byteArray (0 .. 3) :=
	                      (8#0155#,  8#0117#,  8#0123#, 8#0155#);
--	#define	POLYS	{109, 79, 83, 109}
--	In the reversed form the polys look:
--	#define POLYS { 0133, 0171, 0145, 0133 }
--	#define POLYS { 91, 121, 101, 91 }

	function Create_Viterbi (Wordlength : Interfaces. C. int)
	                                          return system. address;
	pragma Import (C, Create_Viterbi, "create_viterbi");

	procedure Init_Viterbi (Handle      : system. address;
	                        startState  : Interfaces. C. int);
	pragma Import (C, Init_Viterbi, "init_viterbi");

	procedure Delete_Viterbi   (Handle : system. Address);
	pragma Import	(C, Delete_Viterbi, "delete_viterbi");

	procedure Initialize	(Object : in out Viterbi_Processor);
	procedure Finalize	(Object : in out Viterbi_Processor);
end Viterbi_Handler;

