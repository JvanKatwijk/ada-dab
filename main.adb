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
with System;
with Gtk.Main;
with Gtk.Widget;	use Gtk.Widget;
with Gtk.Button;	use Gtk.Button;
with Gtk.Label;		use Gtk.Label;
with Gtk.Combo_Box;	use Gtk. Combo_Box;
with Gtk.Combo_Box_Text;	use Gtk. Combo_Box_Text;

with String_Messages;   use String_Messages;
with header;		use header;
with channel_handler;
with ofdm_handler;
with fic_handler;
with msc_handler;
with Gui_Handler;	use Gui_Handler;
with GNAT. Command_Line; use GNAT. Command_Line;
with Text_IO;		use Text_IO;

with device_handler;    use device_handler;
with rtlsdr_wrapper;    use rtlsdr_wrapper;
with rawfiles;          use rawfiles;
with airspy_wrapper;    use airspy_wrapper;
with sdrplay_wrapper;   use sdrplay_wrapper;
--
--	We do know the device we want to support, the (dab)Mode
--	and the Band may be selected in the command line
--
procedure main is
	The_Mode	: Dabmode	:= Mode_1;	-- default
	The_Band	: Dabband	:= BAND_III;	-- default;
	Result		: Boolean;
---------------------------------------------------------------------------
	thedevice	: device_P;
	Gain		: Natural := 0;
	deviceName	: string (1 .. 10) := (Others => ' ');
begin
--
	theDevice := null;
--	we allow command line parameters to be set for mode, band and device
	loop
           case Getopt ("m:  b: d: g:") is   
	      when 'm' =>
	         if Parameter = "Mode_1" then
	            the_Mode := Mode_1;
	         elsif Parameter = "Mode 2" then
	            the_Mode := Mode_2;
	         else
	            the_Mode := Mode_4;
	         end if;

	      when 'b' =>
	         if Parameter = "L_BAND" then
	            the_Band := L_BAND;
	         else
	            the_Band := BAND_III;
	         end if;

	      when 'd' =>
	         if Parameter = "raw" then
	           theDevice := new rawfiles. raw_device;
	           deviceName := "raw       ";
	         elsif parameter = "airspy" then
	           theDevice := new airspy_wrapper. airspy_device;
	           deviceName := "Airspy    ";
	         elsif Parameter = "sdrplay" then
	           theDevice := new sdrplay_wrapper. sdrplay_device;
	           deviceName := "SDRplay   ";
	         elsif Parameter = "rtlsdr" then
	           theDevice := new rtlsdr_wrapper. rtlsdr_device;
	           deviceName := "rtlsdr    ";
	         else
	           theDevice := new rawfiles. raw_device;
	           devicename := "raw       ";
	         end if;

	      when 'g' =>
	         Gain := Natural' Value (Parameter);
	
	      when others => 
	         exit;
	   end case;
	end loop;


	if Gain /= -1 then
	   theDevice. set_Gain (Gain);
	end if;
--	we set up the gui, just create the objects and layouts
--	but the connects are made later on
	Create_GUI (deviceName);

--	the channel_handler, for the selected band
	Channel_Handler. Setup_Channels (Channel_Selector, The_Band);

	declare
--	here we declare (instantiate) our components, they need
-- 	to be visible within the callbacks

	   package my_mscHandler is new msc_handler (the_Mode);
	   package my_ficHandler is new fic_handler (the_Mode);
	   package my_ofdmHandler is
	                  new ofdm_handler (The_Mode,
	                                    theDevice.     Get_Samples,
	                                    theDevice.     Available_Samples,
	                                    my_mscHandler. process_mscBlock,
	                                    my_ficHandler. process_ficBlock,
	                                    my_ficHandler. Sync_Reached);
	   Deleting	: Boolean	:= false;
--
--	and the callbacks
	   procedure main_quit (Self : access Gtk_Widget_Record'Class) is
	   begin
	      if Running then
	         Running := false;
	         my_ofdmHandler. stop;
	         my_ficHandler. stop;
	         my_mscHandler. stop;
	         theDevice. Stop_Reader;
	      end if;
	      Gtk. Main. Main_Quit;
	   end main_quit;
--
	   procedure button_quit
	                  (Self : access Gtk_Button_Record' Class) is
	   begin
	      if Running then
	         Running := false;
	         my_ofdmHandler. stop;
	         my_ficHandler. stop;
	         my_mscHandler. stop;
	         theDevice. Stop_Reader;
	      end if;
	      Destroy (Self);
	      Gtk. Main. Main_Quit;
	   end button_quit;
--
--	called after pressing the start button
	   procedure start_clicked
	                     (Self : access Gtk_Button_Record' Class) is
	      Result : Boolean;
	   begin
	      if Running then
	         return;
	      end if;

	      theDevice. Restart_Reader (result);	
	      if not Result then
	         put_line ("device did not start");
	         Running	:= false;
	      else
	         Running := true;
	         programSelector. Remove_All;
	         my_ficHandler. reset;
	         my_ofdmHandler. reset;
	      end if;
	   end start_clicked;
--
	   procedure Channelselector_Clicked
	              (Self : access Gtk_Combo_Box_Record' Class) is
	      El         : String  := Self. Get_Active_Text;
	      Frequency  : Integer :=
	                           Channel_Handler. Set_Channelselect (el);
	      result     : Boolean;
	   begin
	      my_mscHandler. reset;
	      my_ofdmHandler. reset;
	      my_ficHandler. reset;	-- go for a new fib
	      if theDevice. Valid_Device then
--	         theDevice. Stop_Reader;
	         theDevice. Set_VFOFrequency (kHz (Frequency));
	      end if;
	      string_messages. string_messages. Reset;

	      delay 2.0; -- we have to clean up the fields in programselector
--
--	We set a "deleting flag", since removing entries generates signals
--	to be handled by the program selector, so the latter knows
--	when to ignore signals
	      Deleting	:= true;
	      programSelector. Remove_All;
	      Deleting	:= false;
	      label_ensemble. Set_Label ("ensemble name");
	      if theDevice. Valid_Device then
	            theDevice. Restart_Reader (result);
	      end if;
	      my_ofdmHandler. Reset;
	      my_ficHandler. Reset;
	   end channelSelector_clicked;

	   procedure Programselector_clicked
	                (Self : access Gtk_Combo_Box_Record' Class) is
	      Program_Descriptor: audioData;
	      El    : String   := Self. Get_Active_Text;
	   begin
	      if Deleting then
	         return;
	      end if;

	      put ("program "); put (el); put_line ("selected");
	      my_ficHandler. Data_for_Audioservice (El, Program_Descriptor);
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
	         my_mscHandler. set_audioData (Program_Descriptor);
	      else
	         put_line ("sorry, cannot find the audio right now");
	      end if;
	   end Programselector_clicked;

	   procedure Gainselector_clicked
	                     (Self : access Gtk_Combo_Box_Record' Class) is
	      El : String	:= Self. Get_Active_Text;
	   begin
	      if theDevice. Valid_Device then
	         theDevice. Set_Gain (Integer' Value (el));
	      end if;
	   end Gainselector_clicked;

	begin
--
--	Time to connect the signals from the buttons
--	One issue still is that actually the scope of the function
--	to which an access is passed should exceed the scope of the
--	function to which the access value is passed (seems pretty
--	logical). Since we want the callbacks to have access to
--	the components, we need a detour, the non portable unrestricted_access
	   Win.                On_Destroy
	                       (main_quit' Unrestricted_Access);
	   Channel_Selector.   On_Changed
	                       (Channelselector_clicked' Unrestricted_Access);
	   programSelector.    On_Changed
	                       (Programselector_clicked' Unrestricted_Access);
	   startButton.        On_Clicked
	                       (start_clicked' Unrestricted_Access);
	   Gain_Selector.      On_Changed
	                       (Gainselector_clicked' Unrestricted_Access);
	   quitButton.         On_Clicked
	                       (button_quit'  Unrestricted_Access);

--	start the device, ....
	   theDevice. Restart_Reader (Result);
	   if not Result then
	      put_line ("Could not open input device");
	      return;
	   end if;
--	and the "processor"
	   my_ofdmHandler. Start;
	   Running	:= true;
--	Control ends here and waits for an event to occur
--	(like a key press or a mouse event),
--	until Gtk.Main.Main_Quit is called.
	   Gtk.Main. Main;
	end;
	put_line ("it is the end");
end main;

