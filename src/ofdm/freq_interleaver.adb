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
package body Freq_Interleaver is
	procedure Create_Mapper (Mode	: Dabmode; 
	                         Object	: in out Interleaver;
	                         V1	: integer;
	                         Lwb	: integer; Upb	: integer) is
	   T_u	: integer	:= header. T_u (mode);
	   Tmp	: intArray (0 .. T_u - 1);
	   Index	: integer	:= 0;
	begin
	   Object. Mapper_Table	:= new intArray (0 .. T_u - 1);
	   Tmp (0)		:= 0;
	   for I in 1 .. T_u - 1 loop
	      Tmp (I) := (13 * Tmp (I - 1) + V1) mod T_u;
	   end loop;
	   for I in 0 .. T_u - 1 loop
	      if not (Tmp (I) = T_u / 2 or else
	                        Tmp (I) < Lwb or else Tmp (I) > Upb) then
--	we now have a table with values from lwb .. upb
	         Object. Mapper_Table (Index) := Tmp (i) - T_u / 2;
	         Index	:= Index + 1;
	      end if;
	   end loop;
--	we now have a table with values from lwb - T_u / 2 .. lwb + T_u / 2
	end Create_Mapper;

	procedure Initialize (Object: in out Interleaver) is
	begin
	   case Object. Mode is
	      when Mode_1	=> 
	              Create_Mapper (Mode_1, Object,
	                             511, 256, 256 +  K (Mode_1));
	      when Mode_2	=> 
	              Create_Mapper (Mode_2, Object,
	                             127, 64, 64 +  K (Mode_2));
	      when Mode_3	=> 
	              Create_Mapper (Mode_3, Object,
	                             63, 32, 32 +  K (Mode_3));
	      when others	=>
	              Create_Mapper (Mode_4, Object,
	                             255, 128, 128 + K (Mode_4));
	   end case;
	end initialize;

	procedure Finalize (Object: in out Interleaver) is
	begin
	   Free_intArray (Object. Mapper_Table);
	end finalize;

--	according to the standard, the map is a function from
--	0 .. K - 1 -> -K / 2 .. K / 2 (with exclusion of {0}

	function Map_In (Object: Interleaver; N: integer) return integer is
	begin
	   if N = 0 then
	      return 0;
	   else
	      return Object. Mapper_Table (N);
	   end if;
	end Map_In;
end Freq_Interleaver;

