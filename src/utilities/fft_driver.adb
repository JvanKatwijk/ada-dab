--
--    Copyright (C) 2016
--    Jan van Katwijk (J.vanKatwijk@gmail.com)
--    Lazy Chair Computing
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
with Text_IO;           use Text_IO;
package body fft_driver is
	FFTW_ESTIMATE : constant Interfaces. C. Int := 64;
	function fftwf_plan_dft_1d (size   : Interfaces. C. int;
	                            inVec  : System. Address;
	                            outVec : System. Address;
	                            kind   : Interfaces. C. int;
	                            approach : Interfaces. C. Int) 
	                               return System. Address;
	pragma Import (C, fftwf_plan_dft_1d, "fftwf_plan_dft_1d");

	procedure fftwf_execute (plan :  System. Address);
	pragma Import (C, fftwf_execute, "fftwf_execute");

	fft_Vector_w : fftVector;
	thePlan      : System. Address;
--
--	Note that the only time we need an inverse
--	FFT, we do not need to scale since we only want
--	the maximum value (compared to the average).
	procedure do_FFT (v : in out fftVector) is
	begin
	   fft_Vector_w := v;
	   fftwf_execute (thePlan);
	   v            := fft_Vector_w;
	end do_FFT;
	
begin
	thePlan := fftwf_plan_dft_1d (Interfaces. C. int (fftSize),
	                              fft_Vector_w' Address,
	                              fft_Vector_w' Address,
	                              (if direction = FFTW_FORWARD
	                                   then -1 else 1),
	                              FFTW_ESTIMATE
	                              );
end fft_driver;

