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
with Galois;
package body Galois is

function	modnn		(x	: short) return short is
a	: short_Integer	:= x;
begin
	while a >= codeLength loop
	   a	:= a -  codeLength;
	   a	:= short_Integer (Shift_Right (uint16_t (a),  Natural (symsize)) +
	                          (uint16_t (a) and  uint16_t (codeLength)));
	end loop;
	return short (a);
end modnn;

function	addPoly		(a	: short;
	                         b	: short) return short is
begin
	return short (uint16_t (a) xor uint16_t (b));
end addPoly;

function	addPower	(a	: short;
	                         b	: short) return short is
begin
	return index_of (short (uint16_t (alpha_to (a)) xor
	                                   uint16_t (alpha_to (b))));
end addPower;

function	multiplyPower	(a	: short;
	                         b	: short) return short is
begin
	return modnn (a + b);
end multiplyPower;

function	multiplyPoly	(a	: short;
	                         b	: short) return short is
begin
	if a = 0 or else b = 0
	then
	   return 0;
	end if;
	return alpha_to (multiplyPower (index_of (a), index_of (b)));
end multiplyPoly;

function	dividePower	(a	: short;
	                         b	: short) return short is
begin
	return modnn (d_q - 1 + a - b);
end dividePower;

function	dividePoly	(a	: short;
	                         b	: short) return short is
begin
	if a = 0
	then
	   return 0;
	end if;
	return alpha_to (dividePower (index_of (a), index_of (b)));
end dividePoly;

function	powPoly		(a	: short;
	                         n	: short) return short is
begin
	return alpha_to (powPower (index_of (a), n));
end powPoly;

function	powPower	(a	: short;
	                         n	: short) return short is
aa	: uint16_t	:= uint16_t (a);
nn	: uint16_t	:= uint16_t (n);
begin
	if aa = 0
	then
	   return 0;	
	else
	   return short_Integer ((aa * nn) mod (uint16_t (d_q) - 1));
	end if;
end powPower;

function	inversePoly	(a:	short) return short is
begin
	return alpha_to (inversePower (index_of (a)));
end inversePoly;

function	inversePower	(a:	short) return short is
begin
	return d_q - 1 - a;
end inversePower;

function	poly2Power	(a:	short) return short is
begin
	return index_of (a);
end poly2Power;

function	power2Poly	(a:	short) return short is
begin
	return alpha_to (a);
end power2Poly;

sr	: short	:= 1;
begin
	index_of (0)		:= codeLength;
	alpha_to (codeLength)	:= 0;
	for i in 0 .. codeLength - 1 loop
	   index_of (sr)		:= short (i);
	   alpha_to (i)			:= sr;
	   sr				:= short (Shift_Left (uint16_t (sr), 1));
	   if (uint16_t (sr) and (Shift_Left (1, Natural (symsize)))) /= 0
	   then
	      sr :=  short (uint16_t (sr) xor uint16_t (gfpoly));
	   end if;
	   sr	:=  short (uint16_t (sr) and  uint16_t (codeLength));
	end loop;
end galois;

