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
package body Galois is
	function modnn (x: short_Integer) return short_Integer is
	   A:  short_Integer := x;
	begin
	   while A >= codeLength loop
	      A := A -  codeLength;
	      A := short_Integer (Shift_Right (uint16_t (A), Natural (symsize)) +
	                          (uint16_t (A) and  uint16_t (codeLength)));
	   end loop;
	   return short_Integer (A);
	end modnn;

	function addPoly  (A: short_Integer;
	                   B: short_Integer) return short_Integer is
	begin
	   return short_Integer (uint16_t (A) xor uint16_t (B));
	end addPoly;

	function addPower (A: short_Integer;
	                   B: short_Integer) return short_Integer is
	begin
	   return index_of (short_Integer (uint16_t (alpha_to (a)) xor
	                                   uint16_t (alpha_to (b))));
	end addPower;

	function multiplyPower	(A: short_Integer;
	                         B: short_Integer) return short_Integer is
	begin
	   return modnn (a + b);
	end multiplyPower;

	function multiplyPoly (A: short_Integer;
	                       b: short_Integer) return short_Integer is
	begin
	   if A = 0 or else B = 0 then
	      return 0;
	   end if;
	   return alpha_to (multiplyPower (index_of (A), index_of (B)));
	end multiplyPoly;

	function dividePower (A	: short_Integer;
	                      B	: short_Integer) return short_Integer is
	begin
	   return modnn (d_q - 1 + A - B);
	end dividePower;

	function dividePoly (A	: short_Integer;
	                     B	: short_Integer) return short_Integer is
	begin
	   if A = 0 then
	      return 0;
	   end if;
	   return alpha_to (dividePower (index_of (A), index_of (B)));
	end dividePoly;

	function powPoly (A: short_Integer;
	                  N: short_Integer) return short_Integer is
	begin
	   return alpha_to (powPower (index_of (A), N));
	end powPoly;

	function powPower (A: short_Integer;
	                   N: short_Integer) return short_Integer is
	   AA: uint16_t := uint16_t (A);
	   NN: uint16_t := uint16_t (N);
	begin
	   if AA = 0 then
	      return 0;	
	   else
	      return short_Integer ((AA * NN) mod (uint16_t (d_q) - 1));
	   end if;
	end powPower;

	function inversePoly (A: short_Integer) return short_Integer is
	begin
	   return alpha_to (inversePower (index_of (A)));
	end inversePoly;

	function inversePower (A: short_Integer) return short_Integer is
	begin
	   return d_q - 1 - A;
	end inversePower;

	function poly2Power (A:	short_Integer) return short_Integer is
	begin
	   return index_of (A);
	end poly2Power;

	function power2Poly (A: short_Integer) return short_Integer is
	begin
	   return alpha_to (A);
	end power2Poly;

	SR:   short_Integer  := 1;
begin
	Index_of (0)            := codeLength;
	Alpha_to (CodeLength)   := 0;
	for I in 0 .. CodeLength - 1 loop
	   Index_of (SR)    := short_Integer (i);
	   Alpha_to (I)     := SR;
	   SR               := short_Integer (Shift_Left (uint16_t (sr), 1));
	   if (uint16_t (SR) and
	                 (Shift_Left (1, Natural (symsize)))) /= 0 then
	      SR := short_Integer (uint16_t (SR) xor uint16_t (gfpoly));
	   end if;
	   SR  :=  short_Integer (uint16_t (sr) and  uint16_t (codeLength));
	end loop;
end galois;

