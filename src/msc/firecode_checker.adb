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
with firecode_checker;
with Ada. Text_IO; use Ada. Text_IO;
package body firecode_checker is
type uintArray is Array (Integer Range <>) of uint16_t;
tab	: uintArray (0 .. 255);

function check (x: checkVector) return Boolean is
state	: uint16_t	:= Shift_Left (uint16_t (x (2)), 8) or uint16_t (x (3));
istate	: uint16_t;
begin
	for i in 4 .. 11 - 1 loop
	   istate	:= tab (Integer (Shift_Right (state, 8)));
	   state	:= ((istate and 16#00ff#) xor uint16_t (x (i))) or
	                   ((istate xor Shift_Left (state, 8)) and 16#ff00#);
	end loop;

	for i in 0 .. 2 - 1 loop
	   istate	:= tab (Integer (Shift_Right (state, 8)));
	   state	:= ((istate and 16#00ff#) xor uint16_t (x (i))) or
	                   ((istate xor Shift_Left (state, 8)) and 16#ff00#);
	end loop;

	return state = 0;
end check;

g1	: constant byteArray (0 .. 15) := (1,1,1,1,0,1,0,0,0,0,0,1,1,1,1,0);
procedure  run8 (regs: in out byteArray; result: out uint16_t) is
z	: uint16_t;
v 	: uint16_t	:= 0;
begin
        for i in 0 .. 8 - 1 loop
           z	:=  uint16_t (regs (15));
           for j in Reverse 1 .. 15 loop
	      regs (j)	:= regs (j - 1) xor
	                        uint8_t ((z and  uint16_t (g1 (j))) and 8#077#);
	   end loop;
           regs (0)	:=  uint8_t (z and 8#077#);
	end loop;

	for i in Reverse 0 .. 15 loop
           v 	:= Shift_Left (v, 1) or  uint16_t (regs (i));
	end loop;
        result	:=  v;
end run8;

begin
	declare
--	prepare the table
	   regs : byteArray (0 .. 16 - 1);
	   itab	: uintArray (0 .. 8 - 1);
	begin
	   for i in 0 .. 8 - 1 loop
	      regs		:= (Others => 0);
	      regs (8 + i) 	:= 1;
	      run8 (regs, itab (i));
	   end loop;
	   for i in 0 ..  256 - 1 loop
	      tab (i)	:= 0;
	      for j in 0 .. 8 - 1 loop
	         if ((uint16_t (i) and Shift_Left (uint16_t (1), j))) /= 0
	         then
	            tab (i)	:= uint16_t (tab (i)) xor itab (j);
	         end if;
	      end loop;
	   end loop;
	end;
end firecode_checker;

