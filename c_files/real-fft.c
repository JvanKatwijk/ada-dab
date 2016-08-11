#
/*
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
 */
#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>
#include	<stdint.h>
#include	<fftw3.h>
/*
 *	a simple wrapper for fftw
 *	interface between ada and the libfftw 
 */

typedef struct {
	uint32_t	fft_size;
	fftwf_complex	*fft_Vector;
	fftwf_plan	fft_plan;
	uint8_t		fft_soort;
} my_fft;

extern
void	*createDescriptor (int m, int s) {
my_fft	*the_fft = (my_fft *)malloc (sizeof (my_fft));

	the_fft	-> fft_soort	= m;
	the_fft	-> fft_size	= s;
	the_fft ->  fft_Vector	=
	        (fftwf_complex *)fftwf_malloc (sizeof (fftwf_complex) * the_fft -> fft_size);

	the_fft -> fft_plan =  fftwf_plan_dft_1d (the_fft -> fft_size,
	                                          the_fft -> fft_Vector,
	                                          the_fft -> fft_Vector,
	                                          the_fft -> fft_soort == 0 ?
	                                                FFTW_FORWARD :
	                                                FFTW_BACKWARD,
	                                          FFTW_ESTIMATE);
	return the_fft;
}

void	deleteDescriptor (my_fft *the_fft) {
	fftwf_free ((fftwf_complex *)the_fft -> fft_Vector);
	free (the_fft);
}

extern void execute_fft (my_fft *the_fft,
	                 fftwf_complex *vector) {
int i;
	memcpy (the_fft -> fft_Vector,
	        vector,
	        the_fft -> fft_size * sizeof (fftwf_complex));
	fftwf_execute (the_fft -> fft_plan);	
	memcpy (vector,
	        the_fft -> fft_Vector,
	        the_fft -> fft_size * sizeof (fftwf_complex));
}

