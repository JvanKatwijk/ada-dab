--
--    Copyright (C) 2010, 2011, 2012
--    Jan van Katwijk (J.vanKatwijk@gmail.com)
--    Lazy Chair Programming
--
--    This file is part of the SDR-J.
--    Many of the ideas as implemented in SDR-J are derived from
--    other work, made available through the GNU general Public License. 
--    All copyrights of the original authors are recognized.
--
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
-- 	File reader:
--	For the (former) files with 8 bit raw data from the
--	dabsticks  Very simplified here, just for testing
--
with header; use header;
with ringbuffer;
with Interfaces. C_streams; use Interfaces. C_streams;
with Gtk.Combo_Box;	use Gtk. Combo_Box;
with Gtk.Combo_Box_Text;	use Gtk. Combo_Box_Text;
package rawfiles is
	procedure	setupGainTable  (gainSelector: Gtk_Combo_Box_Text);
	procedure	restartReader   (res : out Boolean);
	procedure	stopReader;
	function	Samples return Integer;
	procedure	getSamples      (output : out complexArray;
	                                 amount : out Integer);
	function	isValid	return Boolean;
	procedure	setVFOFrequency	(freq: Integer);
	procedure	setExternalGain (gain: Integer);
end rawfiles;
