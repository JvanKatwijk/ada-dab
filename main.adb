
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
with System;
with Gtk.Main;
with Gui_Handler;	use Gui_Handler;
with header;		use header;
with channel_handler;	use channel_handler;
with ofdm_handler;
with fib_handler;	use fib_handler;
with fic_handler;
with msc_handler;
with Text_IO;		use Text_IO;
with GNAT. Command_Line; use GNAT. Command_Line;

procedure main is
	The_Mode	: Dabmode	:= Mode_1;	-- default
	The_Band	: Dabband	:= BAND_III;	-- default;
	Result		: Boolean;
	package My_Device renames Gui_Handler. My_Device;
begin
--
--	we allow 2 command line parameters to be set, mode and band
	loop
           case Getopt ("m:  b: ") is   
	      when 'm' =>
	         Put_Line ("Seen -m");   
	      when 'b' =>
	         Put_Line ("Seen -b with arg=" & Parameter); 
	      when others => 
	         exit;
	   end case;
	end loop;

	begin
--	we set up the gui
	   setup_GUI;
--
--	the channel_handler, for the selected band
	   Channel_Handler. Setup_Channels (Channel_Selector, The_Band);
--	Now setting up the engine
	   fib_handler. reset;
	   fic_handler. set_bitsperBlock (The_Mode);	-- basically: init
	   msc_handler. set_Mode	 (The_Mode);
--
--	start the device, ....
--	and the "processor".
	   My_Device. Restart_Reader (Result);
	   if not Result then
	      put_line ("Could not open input device");
	      return;
	   end if;
	   My_Device. Setup_Gaintable (Gain_Selector);
--
--	and off we go
	   my_P		:= new Ofdm_Handler.
	                          Ofdm_Processor (The_Mode,
	                                          My_Device. Get_Samples' Access,
	                                          My_Device. Available_Samples' Access);
	   my_P. Start (my_P);
	   running	:= true;
--	Control ends here and waits for an event to occur
--	(like a key press or a mouse event),
--	until Gtk.Main.Main_Quit is called.
	   Gtk.Main. Main;
	end;
	put_line ("we zijn aan het eind");
end main;


