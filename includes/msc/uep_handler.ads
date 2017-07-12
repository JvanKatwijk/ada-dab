--
--    Copyright (C) 2013
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
-- 	The deconvolution for uep
--
with header;     use header;
with protection_handler;  use protection_handler;
with Interfaces; use Interfaces;
with viterbi_handler; use viterbi_handler;

package uep_handler is

   type uepProcessor  is new protectionProcessor with private;
   type uepProcessor_P is access all uepProcessor;
   overriding
   procedure deconvolve (Object		: in out uepProcessor;
	                 inBuffer	: shortArray;
	                 outBuffer	: out byteArray);
private
   function	findIndex (bitRate	: int16_t;
	                   protLevel	: int16_t) return int16_t;
   procedure Initialize	(Object: in out uepProcessor);
   procedure Finalize	(Object: in out uepProcessor);
-- type uepProcessor (bitRate	: int16_t;
--	              protLevel	: int16_t) is
	type uepProcessor is                        new protectionProcessor with 
	record 
	   outSize		: int16_t;
	   viterbi		: Viterbi_Processor_P;
	   viterbiBlock		: shortArray_P;
	   L1			: int16_t;
	   L2			: int16_t;
	   L3			: int16_t;
	   L4			: int16_t;
	   PI1_Index		: int16_t;
	   PI2_Index		: int16_t;
	   PI3_Index		: int16_t;
	   PI4_Index		: int16_t;
	end record;
end uep_handler;

