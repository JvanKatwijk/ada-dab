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
with Ada. Unchecked_Deallocation;
with text_IO; use Text_IO;
package body phase_handler is
	use complexTypes;

	procedure Initialize (Object : in out Phase_Synchronizer) is
	begin
	   Object. Tu             := header. T_u (Object. Mode);
	   Object. K              := header. K   (Object. Mode);
	   Object. Ref_Table      := new complexArray (0 .. Object. Tu - 1);
	   Object. Ref_Table. all := (Others => (0.0, 0.0));
	   for i in 1 ..  Object. K / 2 loop
	      declare
	         Phi_k : float;
	      begin
	         Phi_k		:= Phasetable. get_Phi (i, Object. Mode);
	         Object. Ref_Table (i) 	:=
	                        (Math. Cos (Phi_k), Math. Sin (Phi_k));
                 Phi_k 		:= Phasetable. get_Phi (-i, Object. mode);
                 Object. Ref_Table (Object. Tu - i) :=
	                         (Math. Cos (Phi_k), Math. sin (Phi_k));
	      end;
	   end loop;
	end Initialize;

	procedure Finalize (Object : in out Phase_Synchronizer) is
	begin
	   Free_complexArray (Object. Ref_Table);
	end Finalize;

--	we will ensure that inputBuffer' length = Tu
	function	Find_Index (Object      : in out Phase_Synchronizer;
	                            inputBuffer : complexArray;
	                            threshold   : integer) return integer is
	   Res_Vector  : complexArray (0 .. inputBuffer' Length - 1) := 
	                                                          inputBuffer;
	   Max_Index   : Integer        := -1;
	   Sum         : Float          := 0.0;
	   Max         : Float          := 0.0;
	   Avg         : Float          := 0.0;
	begin
	   Object. Forward_fft. do_FFT (Res_Vector);

--	back into the frequency domain, now correlate
	   for I in Res_Vector' range loop
	      Res_Vector (I) := Res_Vector (I) *
	                        complexTypes. Conjugate (Object. Ref_Table (I));
	   end loop;

--	and, again, back into the time domain
	   Object. Backward_fft. do_FFT (Res_Vector);
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
end Phase_Handler;

