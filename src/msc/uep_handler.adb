--
--    Copyright (C) 2013
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
-- 	The deconvolution for both uep and eep
--
with uep_handler;
with header; use header;
with prottables; use prottables;
with Ada. Unchecked_Deallocation;
package body uep_handler is

type	protectionProfile is record
	bitRate		: short_Integer;
	protLevel	: short_Integer;
	L1		: short_Integer;
	L2		: short_Integer;
	L3		: short_Integer;
	L4		: short_Integer;
	PI1		: short_Integer;
	PI2		: short_Integer;
	PI3		: short_Integer;
	PI4		: short_Integer;
end record;

type profiles is array (short_Integer Range <>) of protectionProfile;
profileTable	: profiles := (
	(32,	5,	3, 4, 17, 0,	5, 3, 2, -1),
	(32,	4,	3, 3, 18, 0,	11, 6, 5, -1),
	(32,	3,	3, 4, 14, 3,	15, 9, 6, 8),
	(32,	2,	3, 4, 14, 3,	22, 13, 8, 13),
	(32,	1,	3, 5, 13, 3,	24, 17, 12, 17),

	(48,	5,	4, 3, 26, 3,	5, 4, 2, 3),
	(48,	4,	3, 4, 26, 3,	9, 6, 4, 6),
	(48,	3,	3, 4, 26, 3,	15, 10, 6, 9),
	(48,	2,	3, 4, 26, 3,	24, 14, 8, 15),
	(48,	1,	3, 5, 25, 3,	24, 18, 13, 18),

	(64,	5,	6, 9, 31, 2,	5, 3, 2, 3),
	(64,	4,	6, 9, 33, 0,	11, 6, 6, -1),
	(64,	3,	6, 12, 27, 3,	16, 8, 6, 9),
	(64,	2,	6, 10, 29, 3,	23, 13, 8, 13),
	(64,	1,	6, 11, 28, 3,	24, 18, 12, 18),

	(80,	5,	6, 10, 41, 3,	6, 3, 2, 3),
	(80,	4,	6, 10, 41, 3,	11, 6, 5, 6),
	(80,	3,	6, 11, 40, 3,	16, 8, 6, 7),
	(80,	2,	6, 10, 41, 3,	23, 13, 8, 13),
	(80,	1,	6, 10, 41, 3,	24, 7, 12, 18),

	(96,	5,	7, 9, 53, 3,	5, 4, 2, 4),
	(96,	4,	7, 10, 52, 3,	9, 6, 4, 6),
	(96,	3,	6, 12, 51, 3,	16, 9, 6, 10),
	(96,	2,	6, 10, 53, 3,	22, 12, 9, 12),
	(96,	1,	6, 13, 50, 3,	24, 18, 13, 19),
--
--	Thanks to Kalle Riis, who found that the "112" was missing
	(112,	5,	14, 17, 50, 3,	5, 4, 2, 5),
	(112,	4,	11, 21, 49, 3,	9, 6, 4, 8),
	(112,	3,	11, 23, 47, 3,	16, 8, 6, 9),
	(112,	2,	11, 21, 49, 3,	23, 12, 9, 14),

	(128,	5,	12, 19, 62, 3,	5, 3, 2, 4),
	(128,	4,	11, 21, 61, 3,	11, 6, 5, 7),
	(128,	3,	11, 22, 60, 3,	16, 9, 6, 10),
	(128,	2,	11, 21, 61, 3,	22, 12, 9, 14),
	(128,	1,	11, 20, 62, 3,	24, 17, 13, 19),

	(160,	5,	11, 19, 87, 3,	5, 4, 2, 4),
	(160,	4,	11, 23, 83, 3,	11, 6, 5, 9),
	(160,	3,	11, 24, 82, 3,	16, 8, 6, 11),
	(160,	2,	11, 21, 85, 3,	22, 11, 9, 13),
	(160,	1,	11, 22, 84, 3,	24, 18, 12, 19),

	(192,	5,	11, 20, 110, 3,	6, 4, 2, 5),
	(192,	4,	11, 22, 108, 3,	10, 6, 4, 9),
	(192,	3,	11, 24, 106, 3, 16, 10, 6, 11),
	(192,	2,	11, 20, 110, 3, 22, 13, 9, 13),
	(192,	1,	11, 21, 109, 3,	24, 20, 13, 24),

	(224,	5,	12, 22, 131, 3,	8,  6, 2, 6),
	(224,	4,	12, 26, 127, 3,	12, 8, 4, 11),
	(224,	3,	11, 20, 134, 3, 16, 10, 7, 9),
	(224,	2,	11, 22, 132, 3,	24, 16, 10, 15),
	(224,	1,	11, 24, 130, 3,	24, 20, 12, 20),

	(256,	5,	11, 24, 154, 3,	6, 5, 2, 5),
	(256,	4,	11, 24, 154, 3,	12, 9, 5, 10),
	(256,	3,	11, 27, 151, 3,	16, 10, 7, 10),
	(256,	2,	11, 22, 156, 3,	24, 14, 10, 13),
	(256,	1,	11, 26, 152, 3,	24, 19, 14, 18),

	(320,	5,	11, 26, 200, 3,	8, 5, 2, 6),
	(320,	4,	11, 25, 201, 3,	13, 9, 5, 10),
	(320,	2,	11, 26, 200, 3,	24, 17, 9, 17),
	
	(384,	5,	11, 27, 247, 3,	8, 6, 2, 7),
	(384,	3,	11, 24, 250, 3,	16, 9, 7, 10),
	(384,	1,	12, 28, 245, 3,	24, 20, 14, 23),
	(0,	-1,	-1, -1, -1, -1,	-1, -1, -1, -1)
);

--
function	findIndex (bitRate	: short_Integer;
	                   protLevel	: short_Integer) return short_Integer is
begin
	for i in profileTable' Range loop
	   if profileTable (i). bitRate = bitRate and then
	      profileTable (i). protLevel = protLevel
	   then
	      return i;
	   end if;
	end loop;
	return -1;
end findIndex;

procedure Initialize (Object: in out uepProcessor) is
Index	: short_Integer;
begin
	Index	:= findIndex (Object. bitRate, Object. protLevel);
	if index = -1	-- should not happen
	then
	   index := 1;
	end if;

	Object. outSize		:= 24 * Object. bitRate;
	Object. viterbi		:= new viterbiProcessor (Integer (Object. outSize));
	Object. viterbiBlock	:= new shortArray (0 ..
	                               Integer (Object. outSize * 4 + 24 - 1));
	Object. L1	:= profileTable (index). L1;
        Object.	L2	:= profileTable (index). L2;
        Object.	L3	:= profileTable (index). L3;
        Object.	L4	:= profileTable (index). L4;

        Object. PI1_Index	:= profileTable (index). PI1;
        Object. PI2_Index	:= profileTable (index). PI2;
        Object. PI3_Index	:= profileTable (index). PI3;
        if profileTable (index). PI4 /= 0
	then
           Object. PI4_Index	:= profileTable (index). PI4;
        else
           Object. PI4_Index	:= -1;
	end if;
end Initialize;

procedure Finalize (Object: in out uepProcessor) is
procedure Free_viterbi is new Ada. Unchecked_Deallocation (
	Object => viterbiProcessor, Name => viterbiProcessor_P);
begin
	Free_viterbi (Object. viterbi);
	Free_shortArray (Object. viterbiBlock);
end Finalize;

procedure deconvolve (Object	: in out uepProcessor;
	              inBuffer	: shortArray;
	              outBuffer	: out  byteArray) is
inputCounter	: Integer	:= 0;
viterbiCounter	: Integer	:= 0;
first		: Integer	:= inBuffer' first;
begin
--	according to the standard we process the logical frame
--	with a pair of tuples
--	(L1, PI1), (L2, PI2), (L3, PI3), (L4, PI4)

--	clear the bits in the viterbiBlock,
--	only the non-punctured ones are set
	Object. viterbiBlock. all	:= (Others => 0);
	for i in 0 .. Object. L1 - 1 loop
	   for j in short_Integer Range 0 .. 128 - 1 loop
	      if get_PCode (Object. PI1_Index, j mod 32) = 1
	      then
	         Object. viterbiBlock (viterbiCounter) :=
	                                           inBuffer (first + inputCounter);
	         inputCounter	:= inputCounter + 1;
	      end if;
	      viterbiCounter	:= viterbiCounter + 1;
	   end loop;
	end loop;

	for i in 0 .. Object. L2 - 1 loop
           for j in short_Integer Range 0 .. 128 - 1 loop
              if get_PCode (Object. PI2_Index, j mod 32) = 1
	      then
                 Object. viterbiBlock (viterbiCounter) :=
	                                           inBuffer (first + inputCounter);
	         inputCounter	:= inputCounter + 1;
	      end if;
              viterbiCounter 	:= viterbiCounter + 1;
	   end loop;
	end loop;

	for i in 0 .. Object. L3 - 1 loop
	   for j in short_Integer Range 0 .. 128 - 1 loop
              if get_PCode (Object. PI3_Index, j mod 32) = 1
	      then
                 Object. viterbiBlock (viterbiCounter)	:=
	                                           inBuffer (first + inputCounter);
	         inputCounter	:= inputCounter + 1;
	      end if;
              viterbiCounter	:= viterbiCounter + 1;
	   end loop;
	end loop;

	if Object. PI4_Index /= -1
	then
           for i in 0 .. Object. L4 - 1 loop 
              for j in short_Integer Range 0 .. 128 - 1 loop
                 if get_PCode (Object. PI4_Index, j mod 32) = 1
	         then
                    Object. viterbiBlock (viterbiCounter) :=
	                                       inBuffer (first + inputCounter);
	            inputCounter	:= inputCounter + 1;
	         end if;
	         viterbiCounter	:= viterbiCounter + 1;
	      end loop;
	   end loop;
	end if;
--
--	we have a final block of 24 bits  with puncturing according to PI_X
--	This block constitues the 6 * 4 bits of the register itself.

        for i in 0 .. 24 - 1 loop
           if PI_X (i) /= 0
	   then
              Object. viterbiBlock (viterbiCounter) :=
	                                       inBuffer (first + inputCounter);
	      inputCounter	:= inputCounter + 1;
	   end if;
           viterbiCounter 	:= viterbiCounter + 1;
	end loop;
--
--	The actual deconvolution is done by the viterbi decoder

        Object. viterbi. deconvolve (Object. viterbiBlock. all, outBuffer);
end deconvolve;
end uep_handler;


