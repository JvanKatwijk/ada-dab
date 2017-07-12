--
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
-- 	generic audio handler for the ada-da decoder


with header; use header;
with Ada. Finalization; use Ada. Finalization;
with Interfaces; use Interfaces;
with audiopackage;
package audio_handler is
	type Audio_Processor (bitRate:    int16_t;
	                      pcmHandler: audiopackage. audioSink_P) is
	           new Ada. Finalization. Controlled with
	   record
	       null;
	   end record;
	type Audio_Processor_P is access all Audio_Processor' Class;

	procedure Add_to_Frame (Object  : in out Audio_Processor;
	                        Data    : byteArray;
	                        Nbits   : int16_t);
end audio_handler;

