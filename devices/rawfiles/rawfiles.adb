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

with Interfaces. C_streams; use Interfaces. C_streams;
with Text_IO;	use Text_IO;
package body rawfiles is
	function "abs" (Right: header. ComplexTypes. complex) return Float
	                              renames header. complexTypes. "abs";
	procedure Restart_Reader (Object : in out raw_device; Success : out Boolean) is
	begin
	   put_line ("restart called");
	   abort Object. theWorker. All;
	   Object. theWorker := new doRead;
	   Object. theWorker. Start (Object. theBuffer,
	                          "/usr/shared/dab-testfiles/7b-radio-bremen.raw");
	   Success	:= true;
	end Restart_Reader;

	procedure Stop_Reader (Object : in out raw_device) is
	begin
	   abort Object. theWorker. All;
	end Stop_Reader;

	function Valid_Device (Object : raw_device) return Boolean is
	begin
	   return True;
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
	   ourBuffer : Buffer_P;
	begin
	   accept Start (theBuffer : Buffer_P; name: String) do
	      put ("going to start ");
	      filePointer := fopen (name' Address, mode' Address );
	      if filePointer = NULL_Stream then
	         put ("could not open "); put_line (name);
	         raise stopMaar;
	      else
	         put (name); put_line (" opened");
	      end if;
	      ourBuffer := theBuffer;
	   end Start;
	   loop
	      while ourBuffer. GetRingBufferWriteAvailable < lBuf' length loop
	         delay 0.01;
	      end loop;
	
	      do_readBuffer (lBuf, amount);
	      if amount < 0
	      then
	         lbuf := (others => (uint8_t (0), uint8_t (0)));
	      end if;

	      ourBuffer. putDataIntoBuffer (lBuf);
	   end loop;
	exception
	   when stopMaar => put_line ("doread normally finished");
	   when others   => put_line ("problem");
	end doRead;

	procedure Get_Samples (Object : in out raw_device;
	                       out_V   : out complexArray;
	                       amount : out Natural) is
	   Buffer : buffer_data (out_V' Range);
	begin
	   if Object. theWorker. All' Terminated then
	      amount := 0;
	      return;
	   end if;

	   while not Object. theWorker. All' Terminated and then
	      Object. theBuffer. GetRingBufferReadAvailable < out_V' Length loop
	      delay 0.001;
	   end loop;

	   Object. theBuffer. getDataFromBuffer (buffer, amount);
	   for i in Buffer' Range loop
	      out_V (i) := (float (Integer (buffer (i). a) - 128) / 128.0,
	                    float (Integer (buffer (i). b) - 128) / 128.0);
	   end loop;
	exception
	   when others => put_line ("een exceptie");
	end Get_Samples;

	function  Available_Samples (Object : raw_device) return Natural is
	begin
	   return Object. theBuffer. GetRingBufferReadAvailable;
	end Available_Samples;

	procedure Set_Gain (Object    : in out raw_device;
	                    new_Gain  : Natural) is
	begin
	   null;
	end Set_Gain;

	procedure Set_VFOFrequency (Object : in out raw_device;
	                            New_Frequency: Natural) is
	begin
	   null;
	end;

	procedure Initialize (Object: in out raw_device) is
	begin
	   Object. theBuffer := new ringbuffer_data (1024 * 1024);
	   Object. theWorker := new doRead;
	end;

        procedure Finalize   (Object: in out raw_device) is
	begin
	   null;
	end;

end rawfiles;
