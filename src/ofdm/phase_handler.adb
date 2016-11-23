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

with text_IO; use Text_IO;
with phasetable; 
with fft_handler;
--
--	Determines the start of the Tu section of the first 
--	data block in a DAB frame
package body phase_handler is
	use complexTypes;

	package Phases_for_Table is new phaseTable (The_Mode);
	package Forward_fft is new fft_handler (FORWARD, The_Mode);
	package Backward_fft is new fft_handler (BACKWARD, The_Mode);

--	we will ensure that inputBuffer' length = Tu
	function	Find_Index (inputBuffer : bufferType;
	                            threshold   : integer) return integer is
	   Res_Vector  : bufferType     := inputBuffer;
	   Max_Index   : Integer        := -1;
	   Sum         : Float          := 0.0;
	   Max         : Float          := 0.0;
	   Avg         : Float          := 0.0;
	begin
	   Forward_fft. do_FFT (Res_Vector);

--	back into the frequency domain, now correlate
	   for I in Res_Vector' range loop
	      Res_Vector (I) := Res_Vector (I) *
	                        complexTypes. Conjugate (Ref_Table (I));
	   end loop;

--	and, again, back into the time domain
	   Backward_fft. do_FFT (Res_Vector);
--	normalize and
--	compute the average signal value ...
	   for I in Res_Vector' range loop
	      Res_Vector (I) := (Res_Vector (I). Re / float (Res_Vector'Length),
	                         Res_Vector (I). Im / float (Res_Vector'Length));
	      Sum		:= Sum + abs Res_Vector (i);
	   end loop;
--
	   Max	:= -10000.0;
	   for I in Res_Vector' range loop
	      if abs (Res_Vector (I)) > Max then
	         Max_Index 	:= I;
	         Max 		:= abs Res_Vector (I);
	      end if;
	   end loop;

	   Avg	:= Sum / float (Res_Vector' length);
--  that gives us a basis for defining the threshold
	   if Max < Avg * float (Threshold) then
--	      put (Max' Image); put_line (Avg' Image);
	      return -1;
	   else
	      return Max_Index;	
	   end if;
	end Find_Index;
begin
	for i in 1 ..  K / 2 loop
	   declare
	      Phi_k : float;
	   begin
	      Phi_k		:= Phases_for_Table. get_Phi (i);
	      Ref_Table (i) 	:=
	                     (Math. Cos (Phi_k), Math. Sin (Phi_k));
              Phi_k 		:= Phases_for_Table. get_Phi (-i);
              Ref_Table (Tu - i) :=
	                      (Math. Cos (Phi_k), Math. sin (Phi_k));
	   end;
	end loop;
end Phase_Handler;

