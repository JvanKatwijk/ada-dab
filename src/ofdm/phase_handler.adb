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
with phase_handler; 
with Ada. Unchecked_Deallocation;
with text_IO; use Text_IO;
package body phase_handler is
use complexTypes;

	procedure Initialize (Object: in out phaseSynchronizer) is
	begin
	   Object.Tu	:= header. T_u (Object. mode);
	   Object.K	:= header. K   (Object. mode);
	   Object. refTable := new complexArray (0 .. Object. Tu - 1);
	   Object. refTable. all := (Others => (0.0, 0.0));
	   Object. forward_fft	:= new fft_handler. fft (FORWARD, Object. Tu);
	   Object. backward_fft	:= new fft_handler. fft (BACKWARD, Object. Tu);
	   for i in 1 ..  Object. K / 2 loop
	      declare
	         Phi_k : float;
	      begin
	         Phi_k		:= phasetable. get_Phi (i, Object. mode);
	         Object. refTable (i) 	:=
	                        (Math. Cos (Phi_k), Math. Sin (Phi_k));
                 Phi_k 		:= phasetable. get_Phi (-i, Object. mode);
                 Object. refTable (Object. Tu - i) :=
	                         (Math. Cos (Phi_k), Math. sin (Phi_k));
	      end;
	   end loop;
	end Initialize;

	procedure Finalize (Object : in out phaseSynchronizer) is
	   procedure Free_fft is new Ada. Unchecked_Deallocation (
	                Object	=> fft_handler. fft,
	                Name	=> fft_handler. fft_P);
	begin
	   Free_complexArray (Object. refTable);
	   Free_fft (Object. forward_fft);
	   Free_fft (Object. backward_fft);
	end Finalize;

--	we will ensure that v'length = Tu
	function	findIndex (Object : in out phaseSynchronizer;
	                   v : complexArray; threshold : integer)
	                                           return integer is
	resVector	: complexArray (0 .. v' Length - 1) := v;
	maxIndex	: integer	:= -1;
	sum		: float		:= 0.0;
	Max		: float		:= 0.0;
	Avg		: float		:= 0.0;
	length		: Integer	:= v' length;
	begin
	   Object. forward_fft. do_FFT (resVector' Address);
--	back into the frequency domain, now correlate
	   for i in  resVector' range loop
	      resVector (i) := resVector (i) *
	                   complexTypes. Conjugate (Object. refTable (i));
	   end loop;

--	and, again, back into the time domain
	   Object. backward_fft. do_FFT (resVector' Address);
--	normalize and
--	compute the average signal value ...
	   for i in resvector' range loop
	      resvector (i) := (resvector (i). Re  / float (length),
	                        resvector (i). Im  / float (length));
	      sum		:= sum + abs resVector (i);
	   end loop;
--
	   Max	:= -10000.0;
	   for i in resVector' range loop
	      if abs (resVector (i)) > Max
	      then
	         maxIndex 	:= i;
	         Max 	:=  abs resVector (i);
	      end if;
	   end loop;

	   Avg	:= sum / float (resVector' length);
--  that gives us a basis for defining the threshold
	   if Max < Avg * float (threshold)
	   then
--	   put (Max' Image); put_line (Avg' Image);
	      return  - 1;
	   else
	      return maxIndex;	
	   end if;
	end findIndex;
end phase_handler;
