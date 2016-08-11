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
with lowpass_filter; use lowpass_filter;
with Text_IO; use text_IO;
--
--	Note
--	while in C we have a vector range 0 .. N - 1 for an N-sized
--	vector, here we have ranges 1 .. N
package body lowpass_filter is

procedure  pass (f: in out lowpass; v : float; o: out float) is
sum : float	:= 0.0;
begin
	f. input_buffer (f. ip)	:= v;
	for i in 1 .. f. filterSize loop
	declare
	   index : integer := f. ip - i;
	begin
	   if index <= 0
	   then
	      index := index + f. filterSize;
	   end if;
	   sum := sum + f. input_buffer (index) * f. kernel (i);
	end;
	end loop;
	f. ip := f. ip + 1;
	if (f. ip > f. filterSize)
	then
	   f. ip := 1;
	end if;
	o := sum;
end;

function lowpassKernel (filterSize	: integer;
	                rate		: integer;
	                frequency	: integer) return g is
tmp: g (1 .. filterSize);
f  : float := float (frequency) / float (rate);
sum: float := 0.0;
begin
	for i in 1 .. filterSize loop
	   if (i - 1) = filterSize / 2
	   then
	      tmp (i) := 2.0 * M_PI * f;
	   else
	      tmp (i) := Math. sin (2.0 * M_PI * f *
	                           float (i - 1 - filterSize / 2)) /
	                             float (i - 1 - filterSize / 2);
	   end if;

--	apply Blackman window
	   tmp (i) := tmp (i) *
	              (0.42 -
	               0.5 * Math. cos (2.0 * M_PI * float (i - 1) / float (filterSize)) +
	               0.08 * Math. cos (4.0 * M_PI * float (i - 1) / float (filterSize)));
	   sum := sum + tmp (i);
	end loop;
--	scaling
	for i in 1 .. filterSize loop
	   tmp (i) := tmp (i) / sum;
	end loop;
	return tmp;
end lowpassKernel;

end lowpass_filter;

