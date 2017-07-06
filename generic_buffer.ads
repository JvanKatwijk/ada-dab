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
with header; 
generic
	type element_type is private;
package Generic_Buffer is
	use header;
	type Buffer_Data is array (Natural Range <>) of element_type;
	protected type Buffer (Size: Integer) is
	   entry Put (Item : element_type);
	   entry Get (Item : out element_type);
	   function amount return Integer;
	   procedure Reset;
	private
	   Values:	Buffer_Data (0 .. Size);
	   Next_In:	Integer	:= 0;
	   Next_Out:	Integer := 0;
	   Count:	Natural := 0;
	end Buffer;
end Generic_Buffer;

