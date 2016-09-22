
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
with Gtk.Main;
with Gtk.Window;	use Gtk.Window;
with Gtk.Widget;	use Gtk.Widget;
with Glib.Object;
with Glib.Main; use Glib.Main;
with Gtk.Grid;		use Gtk.Grid;
with Gtk.Button;	use Gtk.Button;
with Gtk.Label;		use Gtk.Label;
with Gtk.Combo_Box;	use Gtk. Combo_Box;
with Gtk.Combo_Box_Text;	use Gtk. Combo_Box_Text;
with header; use header;

package  Channel_Handler is

	procedure Setup_Channels (Channel_Selector: Gtk_Combo_Box_Text;
	                          Selected_Band: Dabband);
	function  Set_Channelselect (S: String) return Integer;

private
	Our_Band:	Dabband;
	type Dab_Frequency is record 
	   Key:		String (1 .. 3);
	   Frequency:	Integer;
	end record;

	type Channeltable is array (Integer range <>) of Dab_Frequency;
	BandIII_Channels: constant Channeltable := (
	   (" 5A", 174928),
	   (" 5B", 176640),
	   (" 5C", 178352),
	   (" 5D", 180064),
	   (" 6A", 181936),
	   (" 6B", 183648),
	   (" 6C", 185360),
	   (" 6D", 187072),
	   (" 7A", 188928),
	   (" 7B", 190640),
	   (" 7C", 192352),
	   (" 7D", 194064),
	   (" 8A", 195936),
	   (" 8B", 197648),
	   (" 8C", 199360),
	   (" 8D", 201072),
	   (" 9A", 202928),
	   (" 9B", 204640),
	   (" 9C", 206352),
	   (" 9D", 208064),
	   ("10A", 209936),
	   ("10B", 211648),
	   ("10C", 213360),
	   ("10D", 215072),
	   ("11A", 216928),
	   ("11B", 218640),
	   ("11C", 220352),
	   ("11D", 222064),
	   ("12A", 223936),
	   ("12B", 225648),
	   ("12C", 227360),
	   ("12D", 229072),
	   ("13A", 230748),
	   ("13B", 232496),
	   ("13C", 234208),
	   ("13D", 235776),
	   ("13E", 237488),
	   ("13F", 239200)
);

	L_Band_Channels : Channeltable := (
	   (" LA", 1452960),
	   (" LB", 1454672),
	   (" LC", 1456384),
	   (" LD", 1458096),
	   (" LE", 1459808),
	   (" LF", 1461520),
	   (" LG", 1463232),
	   (" LH", 1464944),
	   (" LI", 1466656),
	   (" LJ", 1468368),
	   (" LK", 1470080),
	   (" LL", 1471792),
	   (" LM", 1473504),
	   (" LN", 1475216),
	   (" LO", 1476928),
	   (" LP", 1478640)
);
end Channel_Handler;

