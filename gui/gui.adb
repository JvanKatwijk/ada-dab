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
with gui;
with channel_handler; use channel_handler;
with fic_handler; 
with msc_handler;
with simple_messages; use simple_messages;
with string_messages; use string_messages;
package body gui is
deleting	: Boolean	:= false;
--	called upon pressing the X to delete the window
	procedure main_quit (Self : access Gtk_Widget_Record'Class) is
	begin
	   if running
	   then
	      running := false;
	      my_P. stop;
	      fic_handler. stop;
	      msc_handler. stop;
	      myDevice. stopReader;
	   end if;
	   Gtk.Main.Main_Quit;
	end main_quit;
--
--
	procedure start_clicked (Self : access Gtk_Button_Record' Class) is
	   result	: Boolean;
	begin
	   Put_Line ("start clicked");
	   myDevice. restartReader (result);
	   if not result
	   then
	      put_line ("device did not start");
	   else
	      running	:= true;
	      programSelector. Remove_All;
	      fic_handler. reset;
	      my_P. reset;
	   end if;
	end start_clicked;
--
--	will be used for selecting a frequency
	procedure channelSelector_clicked (Self : access Gtk_Combo_Box_Record' Class) is
	el : String	:= Self. Get_Active_Text;
	freq	: Integer;
	begin
	   Freq	:= channel_handler. set_channelSelect (el);
	   msc_handler. reset;
	   my_P. reset;
	   fic_handler. reset;	-- go for a new fib
	   if myDevice. isValid
	   then
	      myDevice. setVFOFrequency (kHz (Freq));
	      fic_handler. reset;	-- go for a new fib
	   end if;
	   string_messages. string_messages. Cleanup;
	   delay 2.0;	-- we have to clean up the fields in programselector
	   deleting	:= true;
	   programSelector. Remove_All;
	   deleting	:= false;
	   label_ensemble. Set_Label ("ensemble name");
	end channelSelector_clicked;
--
	procedure gainSelector_clicked (Self : access Gtk_Combo_Box_Record' Class) is
	el : String	:= Self. Get_Active_Text;
	begin
	   if myDevice. isValid
	   then
	      myDevice. setExternalGain (Integer' Value (el));
	      put ("Gain set to "); put_line (el);
	   end if;
	   null;
	end gainSelector_clicked;

	procedure programSelector_clicked
	              (Self : access Gtk_Combo_Box_Record' Class) is
	data	: audioData;
	el : String	:= Self. Get_Active_Text;
	begin
	   if deleting
	   then
	      return;
	   end if;
	   put ("program "); put (el); put_line ("selected");
	   fic_handler. dataforAudioService (el, data);
	   if data. dataisThere
	   then
	      put ("startaddress ");
	      put_line (short_Integer' Image (data. startAddr));
	      put ("Length ");
	      put_line (short_Integer' Image (data. length));
	      put ("bitRate ");
	      put_line (short_Integer' Image (data. bitRate));
	      put ("ASCTy");
	      put_line (short_Integer' Image (data. ASCTy));
	      put ("protLevel");
	      put_line (short_Integer' Image (data. protLevel));
	      put ("uepFlag ");
	      put_line (short_Integer' Image (data. uepFlag));
	   end if;
	   if data. length > 0 and then data. bitrate > 0
	   then
	      msc_handler. set_audioData (data);
	   else
	      put_line ("sorry, do not know where to find the audio right now");
	   end if;
	end programSelector_clicked;

--	called after pressing the quit button
	procedure button_quit (Self : access Gtk_Widget_Record' Class) is
	begin
	   if running
	   then
	      running := false;
	      my_P. stop;
	      myDevice. stopReader;
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

	function dispatch return boolean is
	simple_mess	: simple_messages. message;
	string_mess	: string_messages. message;
	begin
	   while simple_messages. message_queue. amount > 0 loop
	      simple_messages. message_queue. Get (simple_mess);
	      case simple_mess. key is
	         when FINE_CORRECTOR_SIGNAL =>
	                 label_fine_corr. Set_Label (Integer' Image (simple_mess.val));
	         when COARSE_CORRECTOR_SIGNAL =>
	                 label_coarse_corr. Set_Label (Integer' Image (simple_mess. val));
	         when FIC_RESULTS	=>
	                label_fic_results. Set_Label (Integer' Image (simple_mess. val));
	         when MSC_RESULTS	=>
	                label_msc_results. Set_Label (Integer' Image (simple_mess. val));
	         when others	=> null;
	      end case;
	   end loop;
	   while string_messages. string_messages. amount > 0 loop
	      string_messages. string_messages. Get (string_mess);
	      case string_mess. key is
	         when ENSEMBLE_SIGNAL	=>
	                 label_ensemble. Set_Label (string_mess. val);
	         when PROGRAM_SIGNAL	=>
	                 programSelector. Append_Text (string_mess. val);
	                 put_line (string_mess. val);
	         when others	=> null;
	      end case;
	   end loop;
	   return true;
	end dispatch;
--
--	we handle the GUI on package level, since that gives the opportunity
--	to handle requests and having access to the data
procedure setup_GUI is
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

	Gtk_New (channelSelector);
	Grid. Attach_Next_To (channelSelector, quitButton, POS_RIGHT, 2, 1);
	channelSelector.On_Changed (channelSelector_clicked' access);

	Gtk_New (gainSelector);
	Grid.Attach_Next_to (gainSelector, channelSelector, POS_RIGHT, 2, 1);
	gainSelector.On_Changed (gainSelector_clicked' access);

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

	Gtk_New (programSelector);
	Grid. Attach_Next_To (programSelector,
	                           label_ensemble, POS_BOTTOM, 2, 1);
	programSelector.On_Changed (programSelector_clicked' access);
--
--
	Win. Show_All;
end setup_GUI;
begin
	null;
end gui;
