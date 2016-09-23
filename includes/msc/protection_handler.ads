
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
-- 	The deconvolution for eep and uep
--
with header; use header;
with Ada. Finalization; use Ada. Finalization;
package protection_handler is

	type protectionProcessor (bitRate:      short_Integer;
	                          protLevel:    short_Integer) 
	     is new Ada. Finalization. Controlled with record null; end record;
	type protectionProcessor_P is access all protectionProcessor'Class;
	procedure deconvolve (Object:    in out protectionProcessor;
	                      inBuffer:  shortArray;
	                      outBuffer: out byteArray);

	PI_X : constant shortArray (0 .. 24 - 1) := (
	                         1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0,
	                         1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0);
end protection_handler;

