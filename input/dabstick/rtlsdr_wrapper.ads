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
with System; use System;
with ringbuffer;
with Ada. Finalization; use Ada. Finalization;
with Ada. Unchecked_Deallocation;
with header; use header;
with rtlsdr_ada; use rtlsdr_ada;
with Interfaces; use Interfaces;
package rtlsdr_wrapper is
	procedure setupGainTable  (gainSelector: Gtk_Combo_Box_Text);
	procedure setVFOFrequency (frequency: Integer);
	function getVFOFrequency  return Integer;
	procedure restartReader   (result: out Boolean);
	procedure stopReader;
	procedure setExternalGain (gain : Integer);
	procedure getSamples      (outV : out complexArray; amount : out Integer);
	function Samples          return Integer;
	function isValid	  return Boolean;
private

	package rtlsdr_buffer is new ringbuffer (Byte);
	use rtlsdr_buffer;
	task type rtlsdr_reader;
	type rtlsdr_reader_P	is access all rtlsdr_reader;
	device		: aliased devicePointer;
	inputRate	: Integer;
	theBuffer	: rtlsdr_buffer. ringbuffer_data (32 * 32768);
	lastFrequency	: Integer;
	workerHandle	: rtlsdr_reader_P;
	gains		: intArray_P;
	gainsCount	: Integer;
	theGain		: Integer;
	vfoOffset	: Integer := 0;
	valid		: Boolean;

	procedure rtlsdr_Callback (buffer	: System. Address;
	                      size	: Integer;
	                      userData	: System. Address);
   pragma Convention (C, rtlsdr_Callback);
end rtlsdr_wrapper;

