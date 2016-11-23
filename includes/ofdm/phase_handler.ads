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

with header;	use header;
Generic
	The_Mode : DabMode;
package Phase_Handler is
	Tu		: Natural        := header. T_u (The_Mode);
	subtype	bufferType is complexArray (0 .. Tu - 1);
	function Find_Index (inputBuffer : bufferType;
	                     Threshold   : Integer) return Integer;
private
	K		: Natural        := header. K   (The_Mode);
	Ref_Table	: bufferType     := (Others => (0.0, 0.0));
end Phase_Handler;

