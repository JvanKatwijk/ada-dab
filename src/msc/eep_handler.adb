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
-- 	The deconvolution for  the equal error protection
--
with header;		use header;
with prottables;	use prottables;
with Text_IO;		use Text_IO;
with Ada. Unchecked_Deallocation;

package body eep_handler is
	procedure Initialize (Object: in out eepProcessor) is
	begin
	   Object. outSize  := 24 * Object. bitRate;
	   Object. viterbi  := new Viterbi_Processor (Integer (Object. outSize));
	   Object. viterbiBlock	:= new shortArray (0 ..
	                              Integer (Object. outSize * 4 + 24 - 1));
-- set A profile
	   if (uint16_t (Object. protLevel) and 8#0100#) /= 0 then
	      case Object. protLevel mod 8 is
	         when 1   =>
	            Object. L1	        := 6 * Object. bitRate / 8 - 3;
	            Object. L2	        := 3;
	            Object. PI1_Index   := 24;
	            Object. PI2_Index   := 23;
	         when 2   =>
	            if Object. bitRate = 8 then
	               Object. L1       := 5;
	               Object. L2       := 1;
	               Object. PI1_Index := 13;
	               Object. PI2_Index := 12;
	            else
	               Object. L1       := 2 * Object. bitRate / 8 - 3;
	               Object. L2       := 4 * Object. bitRate / 8 + 3;
	               Object. PI1_Index := 14;
	               Object. PI2_Index := 13;
	            end if;
	         when 3   =>
	            Object. L1	        := 6 * Object. bitRate / 8 - 3;
	            Object. L2	        := 3;
	            Object. PI1_Index   := 8;
	            Object. PI2_Index   := 7;
	         when 4   =>
	            Object. L1          := 4 * Object. bitRate / 8 - 3;
	            Object. L2          := 2 * Object. bitRate / 8 + 3;
	            Object. PI1_Index   := 3;
	            Object. PI2_Index   := 2;
	         when others   => -- should not happen
	            Object. L1	        := 4 * Object. bitRate / 8 - 3;
	            Object. L2	        := 2 * Object. bitRate / 8 + 3;
	            Object. PI1_Index   := 3;
	            Object. PI2_Index   := 2;
	      end case;
-- B series
	   elsif (uint16_t (Object. protLevel) and 8#0200#) /= 0 then
	      case Object. protLevel mod 8 is
	         when 4   =>
	            Object. L1          := 24 * Object. bitRate / 32 - 3;
	            Object. L2          := 3;
	            Object. PI1_Index   := 2;
	            Object. PI2_Index   := 1;
	         when 3   =>
	            Object. L1          := 24 * Object. bitRate / 32 - 3;
	            Object. L2          := 3;
	            Object. PI1_Index   := 4;
	            Object. PI2_Index   := 3;
	         when 2   =>
	            Object. L1          := 24 * Object. bitRate / 32 - 3;
	            Object. L2          := 3;
	            Object. PI1_Index   := 6;
	            Object. PI2_Index   := 5;
	         when 1	=>
	            Object. L1          := 24 * Object. bitRate / 32 - 3;
	            Object. L2          := 3;
	            Object. PI1_Index   := 10;
	            Object. PI2_Index   := 9;
	         when others   =>
	            Object. L1          := 24 * Object. bitRate / 32 - 3;
	            Object. L2          := 3;
	            Object. PI1_Index   := 10;
	            Object. PI2_Index   := 9;
	      end case;
	   end if;
	end Initialize;

	procedure Finalize (Object: in out eepProcessor) is
	   procedure Free_viterbi_data is
	                         new Ada. Unchecked_Deallocation (
	                              Object => Viterbi_Processor,
	                              Name => Viterbi_Processor_P);
	begin
	   Free_viterbi_data    (Object. viterbi);
	   Free_shortArray      (Object. viterbiBlock);
	end Finalize;

	procedure deconvolve    (Object:    in out eepProcessor;
	                         inBuffer:  shortArray;
	                         outBuffer: out byteArray) is
	   inputCounter:    Integer  := 0;
	   viterbiCounter:  Integer  := 0;
	   first:           Integer  := inBuffer' first;
	begin
	   Object. viterbiBlock. all   := (Others   => 0);
--
--	according to the standard we process the logical frame
--	with a pair of tuples
--	(L1, PI1), (L2, PI2), (L3, PI3), (L4, PI4)
--
           for i in 0 .. Object. L1 - 1 loop
              for j in int16_t Range 0 .. 128 - 1 loop
                 if get_PCode (Object. PI1_Index, j mod 32) = 1 then
                    Object. viterbiBlock (viterbiCounter) :=
	                                    inBuffer (first + inputCounter);
	            inputCounter     := inputCounter + 1;
	         end if;
                 viterbiCounter := viterbiCounter + 1;
	      end loop;
	   end loop;

           for i in 0 ..  Object. L2 - 1 loop
              for j in int16_t Range 0 .. 128 - 1 loop
                 if get_PCode (Object. PI2_Index,  j mod 32) = 1 then
                    Object. viterbiBlock (viterbiCounter) :=
	                                    inBuffer (first + inputCounter);
	            inputCounter    := inputCounter + 1;
	         end if;
                 viterbiCounter	:= viterbiCounter + 1;
	      end loop;
	   end loop;

--	we have a final block of 24 bits  with puncturing according to PI_X
--	This block constitues the 6 * 4 bits of the register itself.
	   for i in 0 .. 24 - 1  loop
	      if PI_X (i) = 1 then
	         Object. viterbiBlock (viterbiCounter) :=
	                                 inBuffer (first + inputCounter);
	         inputCounter    := inputCounter + 1;
	      end if;
	      viterbiCounter     := viterbiCounter + 1;
	   end loop;
	   Object. viterbi. deconvolve (Object. viterbiBlock. all, outBuffer);
	end deconvolve;

end eep_handler;


