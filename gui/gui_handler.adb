--
--    Copyright (C) 2016
--    Jan van Katwijk (J.vanKatwijk@gmail.com)
--    Lazy Chair Computing
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
with Channel_Handler;
with fic_handler; 
with Simple_Messages; use Simple_Messages;
with String_Messages; use String_Messages;

package body Gui_Handler is

------------------------------------------------------------------------
------------------------------------------------------------------------
--	Internal dispatcher for messages from other tasks

	function Dispatch return boolean is
	   Simple_Message: simple_messages. message;
	   String_Message: string_messages. message;
	begin
	   while simple_messages. message_queue. amount > 0 loop
	      simple_messages. message_queue. Get (Simple_Message);
	      case Simple_Message. key is
	         when FINE_CORRECTOR_SIGNAL =>
	                 label_fine_corr. Set_Label (Integer' Image (Simple_Message.val));
	         when COARSE_CORRECTOR_SIGNAL =>
	                 label_coarse_corr. Set_Label (Integer' Image (Simple_Message. val));
	         when FIC_RESULTS	=>
	                label_fic_results. Set_Label (Integer' Image (Simple_message. val));
	         when MSC_RESULTS	=>
	                label_msc_results. Set_Label (Integer' Image (Simple_message. val));
	         when TEXT_SIGNAL       =>
	                 build_new_text_line (Simple_message. val);

	         when others	=> null;
	      end case;
	   end loop;

	   while string_messages. string_messages. amount > 0 loop
	      string_messages. string_messages. Get (String_Message);
	      case String_Message. key is
	         when ENSEMBLE_SIGNAL	=>
	                 label_ensemble. Set_Label (String_Message. val);
	         when PROGRAM_SIGNAL	=>
	                 programSelector. Append_Text (String_Message. val);
	                 put_line (String_Message. val);
	         when others	=> null;
	      end case;
	   end loop;
	   return true;
	end dispatch;

	messageString : String (1 .. 24) := (others => ' ');

	procedure build_new_text_line (val : Integer) is
	   myChar : Character :=  Character' Val (val);
	begin
	   messageString (1 .. 23) := messageString (2 .. 24);
	   messageString (24)      := myChar;
	   label_Text. set_Label (messageString);
	end build_new_text_line;

--	we handle the GUI on package level, since that gives the opportunity
--	to handle processing requests that require
--	calling functions and procedures in the different packages
--	in front and backend.
	procedure Create_GUI is
	begin
	   --  Initialize GtkAda.
	   Gtk.Main.Init;
	   dummy	:= Timeout_Add (500, Dispatch' access);

	   -- create a top level window
	   Gtk_New (Win);
	   Win.Set_Title ("dab-ada");

	   -- set the border width of the window
	   Win.Set_Border_Width (10);

	   -- Here we construct the container that is going pack our buttons
	   Gtk_New (Grid);
	   -- Packed the container in the Window
	   Win.Add (Grid);
	   Gtk_New (startButton, "Start");
	   Grid. Attach (startButton, 0, 0, 1, 2);

	   Gtk_New (quitButton, "Quit");
	   Grid. Attach_Next_To (quitButton, startButton, POS_BOTTOM, 1, 2);

	   Gtk_New (Channel_Selector);
	   Grid. Attach_Next_To (Channel_Selector, quitButton, POS_RIGHT, 2, 1);

	   Gtk_New (Gain_Selector);
	   Grid.Attach_Next_to (Gain_Selector, Channel_Selector, POS_RIGHT, 2, 1);

	   Gtk_New (label_channel, "channels");
	   Grid.Attach_Next_To (label_channel, startButton, POS_RIGHT, 2, 1);

	   Gtk_New (label_gain, "gains");
	   Grid.Attach_Next_To (label_gain, label_channel, POS_RIGHT, 2, 1);

	   Gtk_New (label_fine_corr, "     ");
	   Grid. Attach_Next_To (label_fine_corr, quitButton, POS_BOTTOM, 2, 1);
--
	   Gtk_New (label_coarse_corr, "     ");
	   Grid. Attach_Next_To (label_coarse_corr,
	                           label_fine_corr, POS_RIGHT, 2, 1);
--
	   Gtk_New (label_fic_results, "    ");
	   Grid. Attach_Next_To (label_fic_results,
	                           label_fine_corr, POS_BOTTOM, 2, 1);

	   Gtk_New (label_msc_results, "    ");
	   Grid. Attach_Next_To (label_msc_results,
	                           label_fic_results, POS_BOTTOM, 2, 1);

	   Gtk_New (label_ensemble, "ensemble name");
	   Grid. Attach_Next_To (label_ensemble,
	                           label_fic_results, POS_RIGHT, 2, 1);

	   Gtk_New (label_Text, " dynamic label text                 ");
	   Grid. Attach_Next_To (label_Text,
	                           label_msc_results, POS_BOTTOM, 2, 3);

	   Gtk_New (Programselector);
	   Grid. Attach_Next_To (Programselector,
	                           label_ensemble, POS_BOTTOM, 2, 1);
--
	   for I in Integer Range 0 .. 100 loop
	      Gain_Selector. Insert_text (Glib. Gint (i), Integer' Image (i));
           end loop;

	   Win. Show_All;
	end Create_GUI;

end Gui_Handler;
