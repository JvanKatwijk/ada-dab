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
--	ProcessPAD takes the AU's from the mp4Processor and
--	dispatches the kind of PAD info.
--	The "dynamic labels" are implemented, the MSC groups
--	not
--
with Interfaces;        use Interfaces;
with Text_IO;           use Text_IO;
with simple_messages;   use simple_messages;
with Ada. Exceptions;	use Ada. Exceptions;

package body pad_handler is

	procedure Process_PAD (theAU  : byteArray) is
	   Count                      : Integer       := Integer (theAU (1));
	   Buffer                     : byteArray (0 .. Count - 1);
	   F_PAD_type	              : uint8_t;
	   X_PAD_type	              : uint8_t;
	   Contents_Indicator_Flag    : uint8_t;
	   Z_Bit                      : uint8_t;
	begin
	   for I in Buffer' Range loop
	      Buffer (i) := theAU (2 + i);
	   end loop;
--
--	The buffer now contains the PAD data
	   F_PAD_Type    := Buffer (Count - 2) and 16#C0#;
	   if F_PAD_Type /= 0 then
	      return;
	   end if;
--
	   Z_Bit         := Buffer (Count - 1) and 8#01#;
	   Contents_Indicator_flag :=
	                  Shift_Right (Buffer (Count - 1), 1) and 8#01#;
--	extract the X_PAD_type
	   X_Pad_Type		:= 
	                 Shift_Right (Buffer (Count - 2), 4) and 8#03#;

	   if X_Pad_Type = 01 then   -- short Xpad
	      Handle_Short_PAD (Buffer, Contents_Indicator_Flag);
	      return;
	   end if;

	   if X_Pad_Type = 02 then	-- variable sized Xpad
	      Handle_Variable_PAD (Buffer, Contents_Indicator_flag);
	   end if;
	end;
--
	procedure Handle_Short_PAD (Buffer                  : byteArray;
	                            Contents_Indicator_Flag : uint8_t) is
	   Count                : Integer    := buffer' Length;
	   Data                 : byteArray (0 .. 3);
	   Contents_Indicator	: uint8_t;
	begin
	   if Contents_Indicator_Flag /= 0 then 
	      for I in 0 .. 2 loop
	         Data (i) := Buffer (Count - 4 - i);
	      end loop;
	      Data (3)   := 0;
	      Contents_Indicator := Buffer (Count - 4 -3);
	      if (Contents_Indicator and 8#037#) = 02 or else
	                     (Contents_Indicator and 8#037#) = 03 then
	         Dynamic_Label (Data, Contents_Indicator);
	      end if;
	   end if;
	end Handle_Short_PAD;
--
	procedure Handle_Variable_PAD (Buffer                  : byteArray;
	                               Contents_Indicator_flag : uint8_t) is
	   Contents_Indicator_Table   : byteArray (0 .. 3);
	   Contents_Indicator_Index   : Integer       := 0;
	   Base                       : short_Integer := buffer' Length - 2 - 1;
	begin
	   if Contents_indicator_flag = 0 then
	      return;
	   end if;
--
--	The CI_flag in the F_PAD data is set, so we have local CI's
--	7.4.2.2: Contents Indicators are one byte long

	   while (Buffer (Integer (Base)) and 8#037#) /= 0 and then
	                                     Contents_Indicator_Index < 4 loop
	      Contents_Indicator_Table (Contents_Indicator_Index) :=
	                                               Buffer (Integer (Base));
	      Contents_Indicator_Index   := Contents_Indicator_Index + 1;
	      Base                       := Base - 1;
	   end loop;

-- 	if we have a "0" indicator at the end of the CI list (rather
--	than that the list contains the 4 elements it could) adjust base
	   if Contents_Indicator_Index < 4 then 
	      Base                := Base - 1;
	   end if;

	   for I in 0 ..  Integer (Contents_Indicator_Index) - 1 loop
	      declare
	         Subfield_Length  : uint8_t
	                       :=  Contents_Indicator_Table (I) and 16#E0#;
	         Applicationtype  : uint8_t
	                       := Contents_Indicator_Table (I) and 16#1F#;
	      begin
	         Subfield_Length := Map_Length (Subfield_Length);
--
--	In this preliminary version, we only support dynamic labels
--	i.e. with appTypes 02 and 03
	         if (Applicationtype = 02) or else
	                            (Applicationtype = 03) then
	            declare
	               Data : byteArray (0 .. Integer (Subfield_Length) - 1);
	            begin
	               for J in 0 .. Integer (Subfield_Length) - 1 loop
	                  Data (J) := Buffer (Integer (base) - J);
	               end loop;
	               Dynamic_Label (Data, Contents_Indicator_Table (I));
	            end;
	         end if;
	         Base            := Base -  short_Integer (Subfield_Length);
	      
	       exception
	        when Error: others	=> Put ("Exception in pad handler: ");
	                                   Put (Exception_Name (Error));
	                                   return;
	      end;
	   end loop;
	end;

	dynamicLabelActive     : Boolean       := false;
	segment_Length         : short_Integer := 0;
	segment_Number         : short_Integer := 0;
	current_Fillpoint      : short_Integer := 0;
	dynamicLabelSegment    : byteArray (0 .. 8192);
--
--      A dynamic label is created from a sequence of xpad
--      fields, starting with CI = 2, continuing with CI = 3
	procedure Dynamic_Label (Data    : byteArray;
	                         CI      : uint8_t) is
	begin
	   if (CI and 8#037#) = 02 then     -- start with new segment
	      if dynamicLabelActive then    -- handle previous segment
	         Add_Segment (segment_Number,
	                      dynamicLabelSegment,
	                      current_Fillpoint);
	      end if;

	      if (segment_Number = 1) then
	         dynamicLabelSegment (0) := uint8_t (Character' Pos (' '));
	         current_Fillpoint   := 1;
	      else
	         current_Fillpoint   := 0;
	      end if;
	      dynamicLabelActive  := true;

	      declare
	         prefix    : uint16_t  :=
	                      Shift_Left (uint16_t (data (0)), 8) or
	                                                uint16_t (data (1));
	         field_1   : uint8_t   := data (1) and 8#017#;
	         Cflag     : uint8_t   := Shift_Right (data (0), 4) and 8#01#;
	         firstFlag : uint8_t   := Shift_Right (data (0), 6) and 8#01#;
	         lastFlag  : uint8_t   := Shift_Right (data (0), 5) and 8#01#;
	      begin
	         if firstFlag /= 0 then
	            segment_Number     := 1;
--	            charSet            := Shift_Right (data (1), 4) and 8#017#;
	         else
	            segment_Number     := short_Integer (
	                                  Shift_Right (data (1), 4) and 8#07#);
	         end if;

	         if Cflag /= 0 then
	            current_Fillpoint  := 0;
	            return;		-- do know know how to handle this
	         else
	            segment_Length     := short_Integer (field_1);
	            for I in 0 .. Integer (data' Length) - 2 - 1 loop
	               dynamicLabelSegment (Integer (current_Fillpoint) + I) :=
	                                     data (2 + I);
	            end loop;
	            current_Fillpoint  :=  current_Fillpoint +
	                                      short_Integer (data' Length) - 2;
	         end if;
	      end;
	   elsif dynamicLabelActive and then (CI and 8#037#) = 03 then
	      for I in 0 .. Integer (Data' Length) - 1 loop
	         dynamicLabelSegment (Integer (current_Fillpoint) + I) :=
	                                                  data (I);
	      end loop;
	      current_Fillpoint  := current_Fillpoint + data' Length;
	   else
	      dynamicLabelActive := false;
	   end if;
	end dynamic_Label;
--
--	Ignore stuff that we cannot print
	function Printable (Val : uint8_t) return Boolean is
	   The_Char : Character := Character' Val (Val);
	begin
	   if The_Char = ' ' then
	      return true;
	   elsif 'a' <= The_Char and then The_Char <= 'z' then
	      return true;
	   elsif 'A' <= The_Char and then The_Char <= 'Z' then
	      return true;
	   elsif '0' <= The_Char and then The_Char <= '9' then
	      return true;
	   else
	      return false;
	   end if;
	end;
--
--	Once we have a full "segment", i.e. a sequence of 1 subfield
--	with appType 
	procedure Add_Segment (Segment_Number    : short_Integer;
	                       Segment_Text      : byteArray;
	                       Current_Fillpoint : short_integer) is
	begin
	   for I in 0 .. Current_Fillpoint - 1 loop
	      if Printable (Segment_Text (Integer (I))) then
	         simple_messages. message_queue.
	                       Put ((TEXT_SIGNAL,
	                                 Integer (Segment_Text (Integer (I)))));
	      end if;
	   end loop;
	end;

	function Map_Length (encodedLength : uint8_t)
	                                          return uint8_t is
	  code : uint8_t := Shift_Right (encodedLength, 5);
	  table : constant intArray := (4, 6, 8, 12, 16, 24, 32, 48);
	begin
	   return  uint8_t (table (integer (code) + table' First));
	end;

	procedure reset is
	begin
	   dynamicLabelActive      := false;
	   segment_Length          := 0;
	   segment_Number          := 0;
	   current_Fillpoint       := 0;
	end;
end pad_handler;
