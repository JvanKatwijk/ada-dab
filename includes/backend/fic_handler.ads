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
with header; use header;
generic
	the_Mode : dabMode;
package fic_handler is
	procedure	Process_Ficblock (Data	: shortArray;
	                                  Blkno	: Integer);
	procedure	Stop;
	procedure	Reset;
	procedure	Restart;
	procedure	Data_for_Audioservice (Name_of_Program: String;
	                                       Data           : out audioData);
	function	Sync_Reached return Boolean;
end fic_handler;

