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
with header; use header;
with protection_handler;  use protection_handler;
with Interfaces; use Interfaces;
with viterbi_handler; use viterbi_handler;

package uep_handler is

--type uepProcessor (bitRate	: short_Integer;
--	              protLevel	: short_Integer) is
   type uepProcessor  is
	                        new protectionProcessor with private;
   type uepProcessor_P is access all uepProcessor;
   overriding
   procedure deconvolve (Object		: in out uepProcessor;
	                 inBuffer	: shortArray;
	                 outBuffer	: out byteArray);
private
   function	findIndex (bitRate	: short_Integer;
	                   protLevel	: short_Integer) return short_Integer;
   procedure Initialize	(Object: in out uepProcessor);
   procedure Finalize	(Object: in out uepProcessor);
-- type uepProcessor (bitRate	: short_Integer;
--	              protLevel	: short_Integer) is
	type uepProcessor is                        new protectionProcessor with 
	record 
	   outSize		: short_Integer;
	   viterbi		: viterbiProcessor_P;
	   viterbiBlock		: shortArray_P;
	   L1			: short_Integer;
	   L2			: short_Integer;
	   L3			: short_Integer;
	   L4			: short_Integer;
	   PI1_Index		: short_Integer;
	   PI2_Index		: short_Integer;
	   PI3_Index		: short_Integer;
	   PI4_Index		: short_Integer;
	end record;
end uep_handler;

