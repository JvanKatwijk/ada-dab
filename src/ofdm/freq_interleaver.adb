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
--	Frequency Interleaver according to LRM for a specified Mode
package body Freq_Interleaver is
	procedure Create_Mapper (V1	: integer;
	                         Lwb	: integer; Upb	: integer) is
	   Tmp	: intArray (0 .. Tu - 1);
	   Index	: integer	:= 0;
	begin
	   Tmp (0)		:= 0;
	   for I in 1 .. Tu - 1 loop
	      Tmp (I) := (13 * Tmp (I - 1) + V1) mod Tu;
	   end loop;
	   for I in 0 .. Tu - 1 loop
	      if not (Tmp (I) = Tu / 2 or else
	                        Tmp (I) < Lwb or else Tmp (I) > Upb) then
--	we now have a table with values from lwb .. upb
	         Mapper_Table (Index) := Tmp (i) - Tu / 2;
	         Index	:= Index + 1;
	      end if;
	   end loop;
--	we now have a table with values from lwb - Tu / 2 .. lwb + Tu / 2
	end Create_Mapper;

--	according to the standard, the map is a function from
--	0 .. K - 1 -> -K / 2 .. K / 2 (with exclusion of {0}

	function Map_In (N: integer) return integer is
	begin
	   if N = 0 then
	      return 0;
	   else
	      return Mapper_Table (N);
	   end if;
	end Map_In;

begin
	case The_Mode is
	   when Mode_1	=> 
	              Create_Mapper (511, 256, 256 +  K (Mode_1));
	   when Mode_2	=> 
	              Create_Mapper (127,  64,  64 +  K (Mode_2));
	   when Mode_3	=> 
	              Create_Mapper ( 63,  32,  32 +  K (Mode_3));
	   when others  =>
	              Create_Mapper (255, 128, 128 + K (Mode_4));
	end case;
end Freq_Interleaver;

