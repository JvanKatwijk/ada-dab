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
	procedure do_FFT (Object:	in out FFT_Processor;
	                  Vector:	in out complexArray) is
	   procedure real_FFT (the_fft: System. Address;
	                       vector: System. Address);
	   pragma Import (C, real_FFT, "execute_fft");
	begin
	   real_FFT (Object. field, Vector' Address);
	end do_FFT;

        procedure Initialize (Object : in out FFT_Processor) is
	   function createDescriptor (mode: Integer; size : Integer)
	                                             return System. Address;
	   pragma Import (C, createDescriptor, "createDescriptor");
        begin
	   Object. field	:=
	               createDescriptor (
	                           (if Object. mode = FORWARD then 0 else 1),
	                            Object. size);
	end Initialize;

	procedure Finalize (Object : in out FFT_Processor) is
	   procedure deleteDescriptor (d : System. Address);
	   pragma Import (C, deleteDescriptor, "deleteDescriptor");
	begin
	   deleteDescriptor (Object. field);
	end Finalize;
end fft_handler;

