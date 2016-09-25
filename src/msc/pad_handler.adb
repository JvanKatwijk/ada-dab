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
--
--	ProcessPAD takes the AU's from the mp4Processor and
--	dispatches the kind of PAD info
--
with Interfaces; use Interfaces;
with Text_IO;	 use Text_IO;

package body pad_handler is

	xpad_length  : short_Integer := 0;
	last_appType : uint8_t;

	procedure Handle_Short_PAD (buffer  : byteArray) is
	   Count                : Integer := buffer' Length;
	   data                 : byteArray (0 .. 3);
	   Content_Indicator    : uint8_t       := buffer (Count - 3);
	begin
--
--	first, copy the data. Note that the order was reversed
	   for I in 0 .. 2 loop
	      data (i) := buffer (Count - 4 - i);
	   end loop;

	   data (3)   := 0;
	   if (Content_Indicator and 8#037#) = 02 or else
	                  (Content_Indicator and 8#037#) = 03 then
	      dynamic_Label (data, 3, Content_Indicator);
	   end if;
	end Handle_Short_PAD;

	procedure dynamic_Label (data    : byteArray;
	                         Length  : short_Integer;
	                         CI      : uint8_t) is
	begin
	   if (CI and 8#037#) = 02 then  -- start of a segment
--	      put_line ("start of new segment");
	      null;
	   elsif (CI and 8#037#) = 03 then -- continuation
	      null;
	   end if;
	end dynamic_Label;

	procedure Add_Segment (Data : byteArray; 
	                       Segment_Number : short_Integer;
	                       Segment_Length : short_Integer;
	                       isLast         : Boolean) is
	begin
	   null;
	end;
--
	procedure processPAD (theAU  : byteArray) is
	   Count   : Integer        := Integer (theAU (1));
	   buffer  : byteArray (0 .. Count - 1);
	begin
	   for I in buffer' Range loop
	      buffer (i) := theAU (2 + i);
	   end loop;

	   declare
	      F_PadType	: uint8_t	:= 
	                Shift_Right (buffer (Count - 2), 6) and 8#03#;
	      X_Pad_Ind : uint8_t;
	   begin
	      if F_PadType /= 0 then
	         return;
	      end if;

	      X_Pad_Ind		:= 
	                 Shift_Right (buffer (Count - 2), 4) and 8#03#;

	      if X_Pad_Ind = 01 then   -- short Xpad
	         Handle_Short_PAD (buffer);
	         return;
	      end if;

	      if X_Pad_Ind = 02 then	-- variable sized Xpad
	         declare
	            Z_Bit   : uint8_t := buffer (Count - 1) and 8#01#;
	            Contents_Indicator_flag : uint8_t :=
	                  Shift_Right (buffer (Count - 1), 1) and 8#01#;
	         begin
	            Handle_Variable_PAD (buffer, 
	                                 Contents_Indicator_flag);
	         end;
	      end if;
	   end;
	end;
--
--	The procedure is called when F_PAD type = 0 and X_PAD Ind = 0
--
	procedure Handle_Variable_PAD (buffer : byteArray;
	                               Contents_Indicator_flag : uint8_t) is
	   Contents_Indicator_Table   : byteArray (0 .. 3);
	   Contents_Indicator_Index   : Integer       := 0;
	   Base       : short_Integer := buffer' Length - 2 - 1;

	begin
	   if Contents_indicator_flag = 0 then
	      return;
	   end if;
--
--	The CI_flag in the F_PAD data is set, so we have local CI's
--	7.4.2.2: Contents Indicators are one byte long

	   while (buffer (Integer (Base)) and 8#037#) /= 0 and then
	                                     Contents_Indicator_Index < 4 loop
	      Contents_Indicator_Table (Contents_Indicator_Index) :=
	                                               buffer (Integer (Base));
	      Contents_Indicator_Index   := Contents_Indicator_Index + 1;
	      Base                       := Base - 1;
	   end loop;

	   if Contents_Indicator_Index < 4 then    -- we have a "0" indicator, adjust base
	      Base                := Base - 1;
	   end if;

	   for I in 0 ..  Contents_Indicator_Index - 1 loop
--	The byte CI_Table (I) has all information, packed
--	5 bits for the type, and 3 bit for the encoded length
	      declare
	         appType  : uint8_t   :=
	                      Contents_Indicator_Table (I) and 8#037#;
	         Length   : Integer :=
	                      Map_PAD_Length (
	                         Shift_Right (Contents_Indicator_Table (I), 5));
	         Data     : byteArray (0 .. Length);
	      begin
	         if appType = 1 then
	            declare
	               Byte_0 : uint8_t  := buffer (Integer (Base));
	               Byte_1 : uint8_t  := buffer (Integer (Base) - 1);
	               crc    : uint16_t := 
	                          Shift_Left (uint16_t (buffer (Integer (Base) - 2)), 8) or
	                                 uint16_t (buffer (Integer (Base) - 3));
	            begin
	               xpad_length  := short_Integer ( 
	                          Shift_Left (uint16_t (Byte_0 and 8#077#), 8)
	                              or  uint16_t (Byte_1));
	               Base	    := Base - 4;
	               last_appType := 1;
	            end;
	         else
--
--	first a check to see whether we are ready to handle the xpad
	            if appType /= 02 and then appType /= 03 then
	               last_appType := appType;
	               return;	-- sorry, we do not handle this
	            end if;

	            for J in 0 .. Length - 1 loop
	               data (J) := buffer (Integer (Base) - J);
	            end loop;
	            data (Length) := 0;

	            dynamic_Label (Data,
	                           short_Integer (Length),
	                           Contents_Indicator_Table (I));
	            last_appType   := appType;
	            Base           := Base -  short_Integer (Length);
	         end if;
	      end;
	   end loop;
	end;
	 
	function Map_PAD_Length (ind : uint8_t) return Integer is
	   Length : Integer;
	begin
	   case ind is
	      when 0      => Length := 4;
	      when 1      => Length := 6;
	      when 2      => Length := 8;
	      when 3      => Length := 12;
	      when 4      => Length := 16;
	      when 5      => Length := 24;
	      when 6      => Length := 32;
	      when others => Length := 48;
	   end case;
	   return Length;
	end;
end pad_handler;
