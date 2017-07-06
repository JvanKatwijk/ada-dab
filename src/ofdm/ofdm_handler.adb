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
with header;		use header;
with Text_IO;		use Text_IO;
with Ada. Exceptions;	use Ada. Exceptions;
with simple_messages;	use simple_messages;
-- the generic components
with phase_handler;
with freq_interleaver;
with fft_driver;

package body Ofdm_Handler is
	use header. ComplexTypes;
	function "abs" (Right: header. ComplexTypes. complex)
	                                        return Float renames
                                     header. ComplexTypes. "abs";
	function arg (Right: header. ComplexTypes. complex)
	                                        return Float renames
                                     header. complexTypes. Argument;
	function conj (val: complexTypes. complex)
	                                        return complexTypes. complex
	                             renames complexTypes. Conjugate;
--
--	Since the following three packages contain "plan" initializations
--	for the fft, we put them here, garanteeing that there
--	is no reentring the plan code
	package my_phaseHandler is new phase_handler (The_Mode);
	package my_fft is new fft_driver (FFTW_FORWARD, The_Mode);
	package new_fft is new fft_driver (FFTW_FORWARD, the_Mode);

	type framebufferElement is record
	   blkno : Natural;
	   Data  : Tu_Sized_Buffer;
	end record;
	package frameBuffer is new generic_buffer (framebufferElement);
	theBuffer : frameBuffer. buffer (20);

--	we might consider a hard reset
	procedure reset is
	begin
	   Fine_Corrector	:= 0;
	   Coarse_Corrector	:= 0;
	   Correction_Flag	:= true;
	end reset;
--
	procedure stop is
	begin
	   if Running then
	      Running := false;
	   end if;
	   delay 0.5;
	   if not Ofdm_Worker' Terminated then
	      abort Ofdm_Worker;
	   end if;
	   if not Ofdm_Decoder' Terminated then
	      abort Ofdm_Decoder;
	   end if;
	end stop;

	function Is_Stopped return Boolean is
	begin
	   return Ofdm_Worker' Terminated;
	end is_stopped;
	
	procedure Start is
	begin
	   if Running then
	      return;
	   end if;

	   Running         := true;
	   Ofdm_Decoder. Start;
	   Ofdm_Worker. Start;
	end Start;

--	In this version we have split up the ofdm handling
--	in two tasks, one for getting the samples and doing
--	some synchronization on the input stream,
--	the other one making (soft) bits from the
--	ofdm blocks
--
--	Since the code for the tasks is pretty lengthy, we make
--	them separate

--	Note that - in Mode 1 - we have app 10 frames a second,
--	with 76 blocks + null period per frame, so that we get
--	roughly speaking one ofdm block per msec

--	Someone (external to the get_Samples function) will eventually
--	set "Running" to false, indicating that the task, which
--	will call Get_Samples, need to terminate
	procedure Get_Samples (Out_V      : out complexArray;
	                       Phase_Ind  : Integer) is separate;
	function Compute_Offset (Block_0_Buffer: Tu_Sized_Buffer)
	                                            return Integer is separate;
	task body Ofdm_Decoder is separate;
	task body Ofdm_Worker is separate;
begin
	for i in OscillatorTable' Range loop
	   OscillatorTable (i) :=
	              (Math. cos (float (i) * 2.0 * M_PI / float (Input_Rate)),
	               Math. sin (float (i) * 2.0 * M_PI / float (Input_Rate)));
	end loop;
--	   put_line ("Initialization complete");
end Ofdm_Handler;

