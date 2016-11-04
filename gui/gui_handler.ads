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
with Glib.Main;		use Glib.Main;
with Gtk.Grid;		use Gtk.Grid;
with Gtk.Button;	use Gtk.Button;
with Gtk.Label;		use Gtk.Label;
with Gtk.Combo_Box;	use Gtk. Combo_Box;
with Gtk.Combo_Box_Text;	use Gtk. Combo_Box_Text;
with Gtkada.Handlers;	use Gtkada.Handlers;
with Gtk. Enums;	use Gtk. Enums;
with simple_messages;   use simple_messages;
with ofdm_handler;
with header; use header;
--with rtlsdr_wrapper; use rtlsdr_wrapper;
--with rawfiles; use rawfiles;
with airspy_wrapper; use airspy_wrapper;
--with sdrplay_wrapper; use sdrplay_wrapper;
with Text_IO; use Text_IO;
with Ada. Unchecked_Deallocation;
with Gdk.Event;

package Gui_Handler is
	function Dispatch return boolean;
	procedure Create_GUI;
--
--	apart from start/stop, we have two selectors
	Gain_Selector	: Gtk_Combo_Box_Text;
	Channel_Selector	: Gtk_Combo_Box_Text;
---------------------------------------------------------------------------
---------------------------------------------------------------------------
-- to select an input device, uncomment the line for
--	and the appropriate line "with xxxx"
--	package My_Device renames rawfiles;
--	package My_Device renames rtlsdr_wrapper;
	package My_Device renames airspy_wrapper;
--	package My_Device renames sdrplay_wrapper;
	running		: Boolean	:= false;
	dummy		   : G_Source_Id;
--	for the GUI we need widgets
	Win		   : Gtk_Window;
	Grid	           : Gtk_Grid;
	startButton        : Gtk_Button;
	quitButton         : Gtk_Button;
	label_channel      : Gtk_Label;
	label_gain         : Gtk_Label;
	label_fine_corr    : Gtk_Label;
	label_coarse_corr  : Gtk_Label;
	label_fic_results  : Gtk_Label;
	label_msc_results  : Gtk_Label;
	label_ensemble     : Gtk_Label;
	label_text         : Gtk_Label;
	programSelector    : Gtk_Combo_Box_Text;

private	
	procedure build_new_text_line (val : Integer);
--	and for (some of the) widgets, we need a callback
--	procedure start_clicked      (Self : access Gtk_Button_Record'Class);
--	procedure button_quit        (Self : access Gtk_Button_Record'Class);
--	procedure channelSelector_clicked (Self : access Gtk_Combo_Box_Record' Class);
--	procedure gainSelector_clicked (Self : access Gtk_Combo_Box_Record' Class);
--	procedure programSelector_clicked (Self : access Gtk_Combo_Box_Record' Class);
--
end GUI_Handler;
