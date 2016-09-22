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
--
with header;		use header;
with Interfaces;	use Interfaces;
with Interfaces. C;	use Interfaces. C;
with Ada. Finalization;
with system;

package FFT_Handler is
	type FFT_Processor (Mode: fftMode; Size : Integer) is new
	                         Ada. Finalization. Controlled with private;
	type FFT_Processor_P	is access all FFT_Processor;
	procedure do_FFT (Object: in out FFT_Processor;
	                             Vector: in out complexArray);
private
	type FFT_Processor (Mode: fftMode; size : Integer) is new
	                      Ada. Finalization. Controlled with 
	record
	   Field	: System. Address;
	end record;
	procedure Initialize (Object : in out FFT_Processor);
	procedure Finalize   (Object : in out FFT_Processor);
end FFT_Handler;

