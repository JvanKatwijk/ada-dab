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

package body Phasetable is

	function Get_Element (N:	integer;
	                      Mode:	Dabmode) return Phasetable_Element is
	begin
	   case Mode is
	      when Mode_1 =>
	         return ModeI_table (ModeI_table' first + N);
	      when Mode_2 =>
	         return ModeII_table (ModeII_table' first + N);
	      when Mode_3 =>		-- should not happen
	         return ModeI_table (ModeI_table' first + N);
	      when Mode_4 =>
	         return modeIV_table (ModeIV_table' first + N);
	   end case;
	end Get_Element;

	function H_table (I: Integer; J: Integer) return Integer is
	begin
	   case I is
	      when 0 => return H0 (H0' first + J);
	      when 1 => return H1 (H1' first + J);
	      when 2 => return H2 (H2' first + J);
	      when 3 => return H3 (H3' first + J);
	      when others => return H0 (H0'first + j);-- should not happen
	   end case;
	end H_table;

	function Get_Phi (K: Integer; Mode: Dabmode) return Float is
	   K_prime:	Integer;
	   I:		Integer;
	   N:		Integer;
	   Current_Element : Phasetable_Element;
	   Index:	Integer := 0;
	begin
	   Current_Element := Get_Element (index, mode);
	   while Current_Element. Kmin /= -1000 loop
	      if Current_Element. Kmin <= k and then
	                           K <= Current_Element. Kmax then
	         K_prime	:= Current_Element. Kmin;
	         I		:= Current_Element. I;
	         N		:= Current_Element. N;
	         return M_PI / 2.0 * Float (H_table (I, K - K_prime) + N);
	      end if;
	      Index	:= Index + 1;
	      Current_Element := Get_Element (Index, Mode);
	   end loop;
	   return 0.0;
	end Get_Phi;
end Phasetable;

