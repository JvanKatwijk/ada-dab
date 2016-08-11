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
with channel_handler;
package body channel_handler is
procedure setupChannels (channelSelector: Gtk_Combo_Box_Text;
	                 band: dabBand)  is

begin
	if band = BAND_III
	then
	   for i in BandIII_channels' range loop
	      declare
	         freq: Integer := BandIII_channels (i). Freq;
	      begin
	         channelSelector. Prepend_Text (bandIII_channels (i). key);
	      end ;
	   end loop;
	   theBand	:= BAND_III;
	else
	   for i in L_Band_channels' range loop
	      declare
	         freq: Integer := L_Band_channels (i). Freq;
	      begin
	         channelSelector. Append_Text (L_Band_channels (i). key);
	      end ;
	   end loop;
	   theBand	:= L_Band;
	end if;
end setupChannels;

function	set_channelSelect (s : String) return Integer is
begin
	if theBand = BAND_III
	then
	   for i in BandIII_channels' range loop
	      if BandIII_channels (i). key = s
	      then
	         return BandIII_channels (i). Freq;
	      end if;
	   end loop;
	   return 0;
	else
	   for i in L_Band_channels' range loop
	      if L_Band_channels (i). key = s
	      then
	         return L_Band_channels (i). Freq;
	      end if;
	   end loop;
	   return 0;
	end if;
end set_channelSelect;
end channel_handler;

