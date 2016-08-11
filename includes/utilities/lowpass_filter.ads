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
with header; use header;
with Ada. Numerics. Generic_Elementary_Functions;

package lowpass_filter is
	package Math is new Ada. Numerics. Generic_Elementary_Functions (float);
	M_PI : constant := 3.1415926535;
	type lowpass (filterSize	:integer;
	              rate		:integer;
	              frequency		:integer)
	                     is new header. root with private;
	procedure pass (f: in out lowpass; v: float; o : out float);
private
	type g is array (integer range <>) of float;
	function lowpassKernel (filterSize: integer;
	                        rate: integer;
	                        frequency: integer) return g;
	type lowpass (filterSize : integer;
	              rate: integer;
	              frequency: integer) is new header. root with
	record
	   input_buffer: g (1 .. filterSize) := (others => 0.0);
	   kernel: g (1 .. filterSize) :=
	                 lowpassKernel (filterSize, rate, frequency);
	   ip: integer := 1;
	end record;
end lowpass_filter;

