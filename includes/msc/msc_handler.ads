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
--	Driver program for processing the MSC.
--	Three operations here (apart from selecting
--	the local frame in the MSC vector)
--	1. deinterleaving
--	2. deconvolution (including depuncturing)
--	3. energy dispersal
--	4. in case of DAB: creating MP2 packets
--	5. in case of DAB+: assembling superframes and creating MP4 packets

--	Note CIF counts from 0 .. 3
with header; use header;
package msc_handler is
	procedure set_Mode		(mode	: dabMode);
	procedure reset;
	procedure stop;
	procedure set_audioData		(d	: audioData);
	procedure process_mscBlock	(fbits	: shortArray;
	                                  blkno	: Integer);
end msc_handler;


