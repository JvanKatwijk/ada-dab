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
with phaseTable; use phaseTable;

package body phaseTable is

function getElement (n: integer; mode: DabMode) return phasetableElement is
begin
	case mode is
	   when Mode_1 =>
	      return modeI_table (modeI_table' first + n);
	   when Mode_2 =>
	      return modeII_table (modeII_table' first + n);
	   when Mode_3 =>		-- should not happen
	      return modeI_table (modeI_table' first + n);
	   when Mode_4 =>
	      return modeIV_table (modeIV_table' first + n);
	end case;
end getElement;

function h_table (i: Integer; j: Integer) return Integer is
begin
	case i is
	   when 0 => return h0 (h0' first + j);
	   when 1 => return h1 (h1' first + j);
	   when 2 => return h2 (h2' first + j);
	   when 3 => return h3 (h3' first + j);
	   when others => return h0 (h0'first + j);-- should not happen
	end case;
end h_table;

function get_Phi (k : Integer; mode: DabMode) return Float is
k_prime	: Integer;
i	: Integer;
n	: Integer;
currentElement : phasetableElement;
index: Integer := 0;
begin

	currentElement := getElement (index, mode);
	while currentElement. kmin /= -1000 loop
	   if currentElement. kmin <= k and then k <= currentElement. kmax
	   then
	      k_prime	:= currentElement. kmin;
	      i		:= currentElement. i;
	      n		:= currentElement. n;
	      return M_PI / 2.0 * Float (h_table (i, k - k_prime) + n);
	   end if;
	   index := index + 1;
	   currentElement := getElement (index, mode);
	end loop;
	return 0.0;
end get_Phi;
end phaseTable;

