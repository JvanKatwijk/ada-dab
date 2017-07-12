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
--
with header;	use header;
package pad_handler is
	procedure Process_PAD         (theAU : byteArray);
	procedure Reset;
private
	procedure Handle_Short_PAD    (Buffer                  : byteArray;
	                               Contents_Indicator_flag : uint8_t);
	procedure Handle_Variable_PAD (Buffer                  : byteArray;
	                               Contents_Indicator_flag : uint8_t);
	procedure Dynamic_Label       (Data    : byteArray;
	                               Length  : int16_t;
	                               CI      : uint8_t);

	function Map_Length (EncodedLength : uint8_t) return uint8_t;
end pad_handler;
