--
--
--	-*- c++ -*- */
--
--	Copyright 2004,2010 Free Software Foundation, Inc.
--	This file is part of GNU Radio
--	GNU Radio is free software; you can redistribute it and/or modify
--	it under the terms of the GNU General Public License as published by
--	the Free Software Foundation; either version 3, or (at your option)
--	any later version.

--	GNU Radio is distributed in the hope that it will be useful,
--	but WITHOUT ANY WARRANTY; without even the implied warranty of
--	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--	GNU General Public License for more details.

--	You should have received a copy of the GNU General Public License
--	along with GNU Radio; see the file COPYING.  If not, write to
--	the Free Software Foundation, Inc., 51 Franklin Street,
--	Boston, MA 02110-1301, USA.
--	
--
--	This is a (partial) rewrite of the GNU radio code, for use
--	within the DAB/DAB+ sdr-j receiver software
--	all rights are acknowledged.

--	g(x)=(x^11+1)(x^5+x^3+x^2+x+1)=1+x+x^2+x^3+x^5+x^11+x^12+x^13+x^14+x^16

with header; use header;
with Interfaces; use Interfaces;
package firecode_checker is
subtype checkVector	is byteArray (0 .. 10);
function check (x: checkVector) return Boolean;

private
procedure  run8 (regs: in out byteArray; result: out uint16_t);
end firecode_checker;

