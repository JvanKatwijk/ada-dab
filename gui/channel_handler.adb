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
package body channel_handler is
	procedure Setup_Channels (Channel_Selector: Gtk_Combo_Box_Text;
	                          Selected_Band:    dabBand)  is
	begin
	   if Selected_Band = BAND_III then
	      for I in BandIII_Channels' range loop
	         Channel_Selector. Prepend_Text (bandIII_Channels (I). Key);
	      end loop;
	      Our_Band	:= BAND_III;
	   else
	      for I in L_Band_Channels' range loop
	         Channel_Selector. Prepend_Text (L_Band_Channels (I). Key);
	      end loop;
	      Our_Band	:= L_Band;
	   end if;
	end Setup_Channels;

	function Set_Channelselect (S : String) return Integer is
	begin
	   if Our_Band = BAND_III then
	      for I in BandIII_Channels' range loop
	         if BandIII_Channels (I). key = S then
	            return BandIII_Channels (I). Frequency;
	         end if;
	      end loop;
	      return 0;
	   else
	      for i in L_Band_Channels' range loop
	         if L_Band_Channels (I). key = S then
	            return L_Band_Channels (I). Frequency;
	         end if;
	      end loop;
	      return 0;
	   end if;
	end Set_Channelselect;
end Channel_Handler;

