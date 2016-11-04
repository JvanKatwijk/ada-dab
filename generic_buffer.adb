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
package body Generic_Buffer is
	protected body Buffer is
	   entry Put (Item : element_type) when Count < Size is
	   begin
	      Values (Next_In)	:= Item;
	      Next_In 		:= (Next_In mod Size) + 1;
	      Count		:= Count + 1;
	   end Put;

	   entry Get (Item: out element_type) when Count > 0 is
	   begin
	      Item		:= Values (Next_Out);
	      Next_Out		:= (Next_Out mod Size) + 1;
	      Count		:= Count - 1;
	   end Get;

	   function amount return Integer is
	   begin
	      return Count;
	   end;

	   procedure Reset is
	   begin
	      Next_In		:= 1;
	      Next_Out		:= 1;
	      Count	        := 0;
	   end Reset;
	end Buffer;
end Generic_Buffer;

