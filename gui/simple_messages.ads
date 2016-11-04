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
with generic_buffer;
package simple_messages is
	type SIGNAL is (SOUND_SIGNAL,
	                FIC_RESULTS,
	                MSC_RESULTS,
	                FINE_CORRECTOR_SIGNAL,
	                COARSE_CORRECTOR_SIGNAL,
	                TEXT_SIGNAL);
	type message is
	record
	   key: SIGNAL;
	   val: Integer;
	end record;
	package message_handler is new Generic_Buffer (message);
	use message_handler;
	message_queue: message_handler. Buffer (50);
end simple_messages;


