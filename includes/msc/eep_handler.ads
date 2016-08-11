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
-- 	The deconvolution for eep
--
with header; use header;
with protection_handler; use protection_handler;
with Interfaces; use Interfaces;
with viterbi_handler; use viterbi_handler;
package eep_handler is

   type eepProcessor (bitRate	: short_Integer;
	              protLevel	: short_Integer) 
	               is new protectionProcessor with private; 
   type eepProcessor_P is access all eepProcessor;
   procedure deconvolve (Object		: in out eepProcessor;
	                 inBuffer	: shortArray;
	                 outBuffer	: out byteArray);
private
   procedure Initialize	(Object: in out eepProcessor);
   procedure Finalize	(Object: in out eepProcessor);
   type eepProcessor (bitRate	: short_Integer;
	              protLevel	: short_Integer) 
	               is new protectionProcessor with 
	record 
	   outSize		: short_Integer;
	   viterbi		: viterbiProcessor_P;
	   viterbiBlock		: shortArray_P;
	   L1			: short_Integer;
	   L2			: short_Integer;
	   PI1_Index		: short_Integer;
	   PI2_Index		: short_Integer;
	end record;
end eep_handler;

