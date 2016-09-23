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
with System; 		use System;
with ringbuffer;
with Ada. Unchecked_Deallocation;
with header;		use header;
with Interfaces;	use Interfaces;
with Interfaces.C;	use Interfaces.C;

package sdrplay_wrapper is
	procedure Setup_Gaintable  (gainSelector : Gtk_Combo_Box_Text);
	procedure Restart_Reader   (Success      : out Boolean);
	procedure Stop_Reader;
	procedure Set_VFOFrequency (New_Frequency: Integer);
	procedure Set_Gain         (New_Gain     : Integer);
	procedure Get_Samples      (Out_V        : out complexArray;
	                            Amount       : out Integer);
	function Available_Samples return Integer;
	function Valid_Device      return Boolean;
private
	package inputBuffer is new ringbuffer (header. complexTypes. complex);
	use inputBuffer;
	subtype localBuffer is inputBuffer. ringbuffer_data (2048000);
	type localBuffer_P is access localBuffer;
	sdrplayBuffer : localBuffer;
--
	task type sdrplayWorker (frequency : Integer; Gain : Integer);
	type sdrplayWorker_P is access all sdrplayWorker;
	procedure Free_Our_Worker is
	           new Ada. Unchecked_Deallocation (
	                   Object => sdrplayWorker,
	                   Name   => sdrplayWorker_P);
	Our_Worker    : sdrplayWorker_P	  := null;
--
--	functions to be used:
	function mir_sdr_Init (gRdb             : Interfaces. C. int;
	                       fsMhz            : Interfaces. C. double;
	                       rfMhz            : Interfaces. C. double;
	                       bwType           : Interfaces. C. int;
	                       ifType           : Interfaces. C. int;
	                       samplesperPacket : out Interfaces. C. int)
--	                       samplesperPacket : System. Address)
	                             return Interfaces. C. int;
	pragma Import (C, mir_sdr_Init, "mir_sdr_Init");

	procedure mir_sdr_UnInit;
	pragma Import (C, mir_sdr_UnInit, "mir_sdr_Uninit");
--
	procedure mir_sdr_SetDcMode (A  : Interfaces. C. int;
	                             B  : Interfaces. C. int);
	pragma Import (C, mir_sdr_SetDcMode, "mir_sdr_SetDcMode");

	procedure mir_sdr_SetDcTrackTime (A : Interfaces. C. int);
	pragma Import (C, mir_sdr_SetDcTrackTime, "mir_sdr_SetDcTrackTime");

--	function mir_sdr_readPacket (xi        : System. Address;
--	                             xq        : System. Address;
--	                             fsn       : System. Address;
--	                             grChanged : System. Address;
--	                             rfChanged : System. Address;
--	                             fsChanged : System. Address)
--	                                        return Interfaces. C. int;
--	pragma Import (C, mir_sdr_readPacket, "mir_sdr_ReadPacket");

	procedure mir_sdr_SetFs	(dfsHz : Interfaces. C. double;
	                         A     : Interfaces. C. int;
	                         sync  : Interfaces. C. int);
	pragma Import (C, mir_sdr_SetFs, "mir_sdr_SetFs");

	procedure mir_sdr_SetRf (drfHz : Interfaces. C. double;
	                         A     : Interfaces. C. int;
	                         sync  : Interfaces. C. int);
	pragma Import (C, mir_sdr_SetRf, "mir_sdr_SetRf");

	procedure mir_sdr_SetGr	 (gRdb : Interfaces. C. int;
	                          A    : Interfaces. C. int;
	                          sync : Interfaces. C. int);
	pragma Import (C, mir_sdr_SetGr, "mir_sdr_SetGr");

	procedure mir_sdr_SetParam (A  :  Interfaces. C. int;
	                            B  :  Interfaces. C. int);
	pragma Import (C, mir_sdr_SetParam, "mir_sdr_SetParam");

	procedure mir_sdr_SetSyncUpdateSampleNum (S : Interfaces. C. int);
	pragma Import (C, mir_sdr_SetSyncUpdateSampleNum,
	                            "mir_sdr_SetSyncUpdateSampleNum");

	procedure mir_sdr_SetSyncUpdatePeriod (P : Interfaces. C. int);
	pragma Import (C, mir_sdr_SetSyncUpdatePeriod,
	                            "mir_sdr_SetSyncUpdatePeriod");
end sdrplay_wrapper;
