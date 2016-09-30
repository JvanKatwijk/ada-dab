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
with Channel_Handler; use Channel_Handler;
with fic_handler; 
with msc_handler;
with Simple_Messages; use Simple_Messages;
with String_Messages; use String_Messages;

package body Gui_Handler is
	Deleting	: Boolean	:= false;
--	called upon pressing the X to delete the window
	procedure main_quit (Self : access Gtk_Widget_Record'Class) is
	begin
	   if Running then
	      Running := false;
	      my_P. stop;
	      fic_handler. stop;
	      msc_handler. stop;
	      My_Device. Stop_Reader;
	   end if;
	   Gtk.Main.Main_Quit;
	end main_quit;
--
--
	procedure start_clicked (Self : access Gtk_Button_Record' Class) is
	   Result	: Boolean;
	begin
	   Put_Line ("start clicked");
	   My_Device. Restart_Reader (result);
	   if not Result
	   then
	      put_line ("device did not start");
	   else
	      Running	:= true;
	      programSelector. Remove_All;
	      fic_handler. reset;
	      my_P. reset;
	   end if;
	end start_clicked;
--
--	will be used for selecting a frequency
	procedure Channelselector_Clicked (Self : access Gtk_Combo_Box_Record' Class) is
	   El:		String	:= Self. Get_Active_Text;
	   Frequency:	Integer;
	begin
	   Frequency	:= Channel_Handler. Set_Channelselect (el);
	   msc_handler. reset;
	   my_P. reset;
	   fic_handler. reset;	-- go for a new fib
	   if My_Device. Valid_Device then
	      My_Device. Set_VFOFrequency (kHz (Frequency));
	      fic_handler. reset;	-- go for a new fib
	   end if;
	   string_messages. string_messages. Cleanup;
	   delay 2.0;	-- we have to clean up the fields in programselector
	   Deleting	:= true;
	   programSelector. Remove_All;
	   Deleting	:= false;
	   label_ensemble. Set_Label ("ensemble name");
	end channelSelector_clicked;
--
	procedure Gainselector_clicked (Self : access Gtk_Combo_Box_Record' Class) is
	   El : String	:= Self. Get_Active_Text;
	begin
	   if My_Device. Valid_Device then
	      My_Device. Set_Gain (Integer' Value (el));
--	      put ("Gain set to "); put_line (el);
	   end if;
	end Gainselector_clicked;

	procedure Programselector_clicked
	              (Self : access Gtk_Combo_Box_Record' Class) is
	   Program_Descriptor: audioData;
	   El: String	:= Self. Get_Active_Text;
	begin
	   if Deleting then
	      return;
	   end if;
	   put ("program "); put (el); put_line ("selected");
	   fic_handler. Data_for_Audioservice (El, Program_Descriptor);
	   if Program_Descriptor. dataisThere then
	      put ("startaddress ");
	      put_line (short_Integer' Image (Program_Descriptor. startAddr));
	      put ("Length ");
	      put_line (short_Integer' Image (Program_Descriptor. length));
	      put ("bitRate ");
	      put_line (short_Integer' Image (Program_Descriptor. bitRate));
	      put ("ASCTy");
	      put_line (short_Integer' Image (Program_Descriptor. ASCTy));
	      put ("protLevel");
	      put_line (short_Integer' Image (Program_Descriptor. protLevel));
	      put ("uepFlag ");
	      put_line (short_Integer' Image (Program_Descriptor. uepFlag));
	   end if;
	   if Program_Descriptor. length > 0 and then
	                          Program_Descriptor. bitrate > 0 then
	      msc_handler. set_audioData (Program_Descriptor);
	   else
	      put_line ("sorry, do not know where to find the audio right now");
	   end if;
	end Programselector_clicked;

--	called after pressing the quit button
	procedure button_quit (Self : access Gtk_Widget_Record' Class) is
	begin
	   if Running
	   then
	      Running	:= false;
	      my_P. stop;
	      My_Device. Stop_Reader;
	      fic_handler. stop;
	      msc_handler. stop;
	   end if;
	   Put_Line ("button_quit is called");
	   Destroy (Self);
	   Gtk.Main.Main_Quit;
	end button_quit;
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
--	to handle requests and having access to the data
	procedure Setup_GUI is
	begin
	   --  Initialize GtkAda.
	   Gtk.Main.Init;
	   dummy	:= Timeout_Add (500, dispatch' access);

	   -- create a top level window
	   Gtk_New (Win);
	   Win.Set_Title ("dab-ada");

	   -- set the border width of the window
	   Win.Set_Border_Width (10);
	   -- connect the "destroy" signal
	   Win.On_Destroy (main_quit'Access);

	   -- Here we construct the container that is going pack our buttons
	   Gtk_New (Grid);
	   -- Packed the container in the Window
	   Win.Add (Grid);
	   Gtk_New (startButton, "Start");
	   Grid. Attach (startButton, 0, 0, 1, 2);
	   startButton. On_Clicked (start_clicked' access);
--
	   Gtk_New (quitButton, "Quit");
	   Grid. Attach_Next_To (quitButton, startButton, POS_BOTTOM, 1, 2);
	   Widget_Callback.Object_Connect (quitButton,
                                           "clicked",
                     Widget_Callback.To_Marshaller (button_quit'Access),
                                         Win);

	   Gtk_New (Channel_Selector);
	   Grid. Attach_Next_To (Channel_Selector, quitButton, POS_RIGHT, 2, 1);
	   Channel_Selector. On_Changed (Channelselector_clicked' access);

	   Gtk_New (Gain_Selector);
	   Grid.Attach_Next_to (Gain_Selector, Channel_Selector, POS_RIGHT, 2, 1);
	   Gain_Selector. On_Changed (Gainselector_clicked' access);

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
	    programSelector.On_Changed (Programselector_clicked' access);
--
--
	   Win. Show_All;
	end Setup_GUI;
begin
	null;
end Gui_Handler;
