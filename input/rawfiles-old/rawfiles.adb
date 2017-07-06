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
with System; use System;
with Interfaces; use Interfaces;
with Text_IO; use Text_IO;
package body rawfiles is
	function "abs" (Right: header. ComplexTypes. complex) return Float
	                              renames header. complexTypes. "abs";
	type element is record
	   a : uint8_t;
	   b : uint8_t;
	end record;
--	we read in segments of 10 millseconds at a time
	package localBuffer is new ringbuffer (element);
	use localBuffer;

	bufferSize : constant          :=  4 * 32768;
	I_Buffer   : ringbuffer_data (32 * 32768);
	task doRead is
	   entry Start (name : String);
	end doRead;

	procedure Restart_Reader (res : out Boolean) is
	begin
	   put_line ("restart called");
--	   doRead. Start ("/usr/shared/dab-testfiles/mux-12a.raw");
--	   doRead. Start ("/usr/shared/dab-testfiles/mux-12C-a.raw");
	   doRead. Start ("/usr/shared/dab-testfiles/7b-radio-bremen.raw");
--	   doRead. Start ("/usr/shared/dab-testfiles/6A NDR NDS (Visselhovede).raw");
--	   doRead. Start ("/usr/shared/dab-testfiles/11D Radio fuer NRW.raw");
--	   doRead. Start ("/usr/shared/dab-testfiles/5C DR Deutschland (Bremen Walle, SFN).raw");
	   res	:= true;
	end Restart_Reader;

	procedure Stop_Reader is
	begin
	   abort doRead;
	end Stop_Reader;

	function Valid_Device return Boolean is
	begin
	   return true;
	end Valid_Device;

	task body doRead is
	   period      : Integer := (bufferSize * 1000) / (2 * 2048);	
	   stopMaar    : exception;
	   filePointer : FILEs;
	   subtype readerBuffer is buffer_data (0 .. bufferSize - 1);
	   lBuf        : readerBuffer;
	   amount      : Integer;
	   mode        : String	:= "r";
	   procedure  do_readBuffer (data : out readerBuffer;
	                             result: out Integer) is
	      n : Integer;
	   begin
	      n := Integer (fread (data' Address,
	                           size_t (1),
	                           size_t (2 * data' Length),
	                           filePointer));
	      if n < 2 * data' Length then
	         n := fseek (filePointer, 0, SEEK_SET);
	         put_line ("end of file, restarting");
	      end if;
	      result := n / 2;
	   end do_readBuffer;
	begin
	   accept Start (name: String) do
	      put ("going to start ");
	      filePointer := fopen (name' Address, mode' Address );
	      if filePointer = NULL_Stream then
	         put ("could not open "); put_line (name);
	         raise stopMaar;
	      else
	         put (name); put_line (" opened");
	      end if;
	   end Start;
	   loop
	      while I_Buffer. GetRingBufferWriteAvailable < lBuf' length loop
	         delay 0.01;
	      end loop;
	
	      do_readBuffer (lBuf, amount);
	      if amount < 0
	      then
	         lbuf := (others => (uint8_t (0), uint8_t (0)));
	      end if;

	      I_Buffer. putDataIntoBuffer (lBuf);
	   end loop;
	exception
	   when stopMaar => put_line ("doread normally finished");
	   when others   => put_line ("problem");
	end doRead;

	procedure Get_Samples (output : out complexArray;
	                       amount : out Integer) is
	   Buffer : buffer_data (output' Range);
	begin
	   if doRead' terminated then
	      amount := 0;
	      return;
	   end if;

	   while not doRead' Terminated and then
	          I_Buffer. GetRingBufferReadAvailable < output' Length loop
	      delay 0.001;
	   end loop;
	   I_Buffer. getDataFromBuffer (buffer, amount);
	   for i in Buffer' range loop
	      output (i) := (float (Integer (buffer (i). a) - 128) / 128.0,
	                     float (Integer (buffer (i). b) - 128) / 128.0);
	   end loop;
	exception
	   when others => put_line ("een exceptie");
	end Get_Samples;

	function Available_Samples return Integer is
	begin
	   return I_Buffer. GetRingBufferReadAvailable;
	end Available_Samples;

	procedure Set_Gain (gain: Integer) is
	begin
	   null;
	end Set_Gain;

	procedure Set_VFOFrequency (freq: Integer) is
	begin
	   null;
	end;

	procedure Setup_GainTable  (gainSelector: Gtk_Combo_Box_Text) is
	begin
	   null;
	end;

end rawfiles;
