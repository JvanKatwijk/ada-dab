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
package body fft_handler is
	procedure do_FFT (Vector : in out complexArray) is
	   subtype vectorType is complexArray (Vector' Range);
	   procedure real_FFT (the_fft : System. Address;
	                       Vector  : in out vectorType);
--	                       vector  : System. Address);
	   pragma Import (C, real_FFT, "execute_fft");
	begin
	   real_FFT (field, Vector);
	end do_FFT;

	function createDescriptor (Kind : Integer; size : Integer)
	                                             return System. Address;
	pragma Import (C, createDescriptor, "createDescriptor");
begin
	field  := createDescriptor ((if The_Kind = FORWARD then 0 else 1),
	                            T_u (The_Mode));
end fft_handler;

