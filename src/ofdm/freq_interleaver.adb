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
package body freq_interleaver is
procedure createMapper (mode	: dabMode; 
	                Object	: in out interleaver;
	                V1	: integer;
	                lwb	: integer; upb	: integer) is
T_u	: integer	:= header. T_u (mode);
tmp	: intArray (0 .. T_u - 1);
index	: integer	:= 0;
begin
	Object. mapperTable	:= new intArray (0 .. T_u - 1);
	tmp (0)		:= 0;
	for i in 1 .. T_u - 1 loop
	   tmp (i) := (13 * tmp (i - 1) + V1) mod T_u;
	end loop;
	for i in 0 .. T_u - 1 loop
	   if not (tmp (i) = T_u / 2 or else
	                        tmp (i) < lwb or else tmp (i) > upb)
	   then
--	we now have a table with values from lwb .. upb
	      Object. mapperTable (index) := tmp (i) - T_u / 2;
	      index	:= index + 1;
	   end if;
	end loop;
--	we now have a table with values from lwb - T_u / 2 .. lwb + T_u / 2
end createMapper;

procedure initialize (Object: in out interleaver) is
begin
	case Object. mode is
	   when Mode_1	=>  createMapper (Mode_1, Object,
	                                  511, 256, 256 +  K (Mode_1));

	   when Mode_2	=>  createMapper (Mode_2, Object,
	                                  127, 64, 64 +  K (Mode_2));

	   when Mode_3	=>  createMapper (Mode_3, Object,
	                                  63, 32, 32 +  K (Mode_3));

	   when others	=>  createMapper (Mode_4, Object,
	                                  255, 128, 128 + K (Mode_4));
	end case;
end initialize;

procedure finalize (Object: in out interleaver) is
begin
	Free_intArray (Object. mapperTable);
end finalize;

--	according to the standard, the map is a function from
--	0 .. K - 1 -> -K / 2 .. K / 2 (with exclusion of {0}

function mapIn (Object: interleaver; n : integer) return integer is
begin
	if n = 0
	then
	   return 0;
	else
	   return Object. mapperTable (n);
	end if;
end mapIn;
end freq_interleaver;
