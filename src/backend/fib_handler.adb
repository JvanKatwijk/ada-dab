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
with Text_IO;		use Text_IO;
with string_messages;	use string_messages;
with Interfaces;	use Interfaces;

package body fib_handler is
--
--	FIB's are segments of 256 bits. When we arrive here,
--	we already passed the crc and start unpacking
--	
	type Dab_Label is
	   record
	      Label        : String (1 .. 16);
	      Mask         : uint8_t;
	      Has_Name     : Boolean;
	end record;

	type Service_Identifier is
	   record
	      In_Use       : Boolean      := false;
	      Service_Id   : uint32_t;
	      serviceLabel : Dab_Label;
	      Has_Language : Boolean;
	      The_Language : int16_t;
	      Program_Type : int16_t;
	      HasPNum      : Boolean;
	      pNum         : Integer;
	   end record;

	type ServiceComponent is
	   record
	      In_Use       : Boolean      := false;
	      TMid         : int16_t;
	      Service      : int16_t;	-- index in other vector
	      Component_Nr : int16_t;
	      ASCTy        : int16_t;	-- used for audio
	      PS_flag      : int16_t;	-- used for audio
	      Subchannel_Id: int16_t;
	   end record;

	type Subchannel_Map is
	   record
	      Sub_ChId     : Integer;
	      startAddr    : int16_t;
	      Length       : int16_t;
	      UepFlag      : int16_t;	-- could be Boolean
	      Prot_Level   : int16_t;
	      bitRate      : int16_t;
	      language     : int16_t;	-- double ???
	   end record;

	subtype shortRange is int16_t range 0 .. 63;

	type subchannelMaps    is array (shortRange) of Subchannel_Map;
	type serviceComponents is array (shortRange) of serviceComponent;
	type serviceIds        is array (shortRange) of Service_Identifier; 

	ficList         : subchannelMaps;
	listofServices  : serviceIds;
	components      : serviceComponents;
	Has_Name        : Boolean            := False;
	ensembleName    : String (1 .. 16);

-- Tabelle ETSI EN 300 401 Page 50
-- Table is copied from the work of Michael Hoehn
	type Protections is array (shortRange,
	                   int16_t range 0 .. 2) of int16_t;
	Prot_Levels	: constant Protections := (
	(16,	5,	32),		-- Index 0
	(21,	4,	32),	
	(24,	3,	32),	
	(29,	2,	32),	
	(35,	1,	32),		-- Index 4
	(24,	5,	48),	
	(29,	4,	48),	
	(35,	3,	48),	
	(42,	2,	48),	
	(52,	1,	48),		-- Index 9
	(29,	5,	56),	
	(35,	4,	56),	
	(42,	3,	56),	
	(52,	2,	56),	
	(32,	5,	64),		-- Index 14
	(42,	4,	64),	
	(48,	3,	64),	
	(58,	2,	64),	
	(70,	1,	64),	
	(40,	5,	80),		-- Index 19
	(52,	4,	80),	
	(58,	3,	80),	
	(70,	2,	80),	
	(84,	1,	80),	
	(48,	5,	96),		-- Index 24
	(58,	4,	96),	
	(70,	3,	96),	
	(84,	2,	96),	
	(104,	1,	96),	
	(58,	5,	112),		-- Index 29
	(70,	4,	112),	
	(84,	3,	112),	
	(104,	2,	112),	
	(64,	5,	128),	
	(84,	4,	128),		-- Index 34
	(96,	3,	128),	
	(116,	2,	128),	
	(140,	1,	128),	
	(80,	5,	160),	
	(104,	4,	160),		-- Index 39
	(116,	3,	160),	
	(140,	2,	160),	
	(168,	1,	160),	
	(96,	5,	192),	
	(116,	4,	192),		-- Index 44
	(140,	3,	192),	
	(168,	2,	192),	
	(208,	1,	192),	
	(116,	5,	224),	
	(140,	4,	224),		-- Index 49
	(168,	3,	224),	
	(208,	2,	224),	
	(232,	1,	224),	
	(128,	5,	256),	
	(168,	4,	256),		-- Index 54
	(192,	3,	256),	
	(232,	2,	256),	
	(280,	1,	256),	
	(160,	5,	320),	
	(208,	4,	320),		-- index 59
	(280,	2,	320),	
	(192,	5,	384),	
	(280,	3,	384),	
	(416,	1,	384)
);

--
--	offset is now in bits
	procedure process_FIG0	(p      : fib_buffer;
	                         offset : int16_t) is
	   extension : int16_t :=
	            int16_t (get_Bits (p, offset + 8 + 3, 5));
	begin
	   case extension is
--	      when 0	=> FIG0Extension0 (p, offset);
	      when 1	=> FIG0Extension1 (p, offset);
	      when 2	=> FIG0Extension2 (p, offset);
--	      when 3	=> FIG0Extension3 (p, offset);
--	      when 4	=> FIG0Extension4 (p, offset);
--	      when 5	=> FIG0Extension5 (p, offset);
--	      when 6	=> FIG0Extension6 (p, offset);
--	      when 7	=> FIG0Extension7 (p, offset);
--	      when 8	=> FIG0Extension8 (p, offset);
--	      when 9	=> FIG0Extension9 (p, offset);
--	      when 10	=> FIG0Extension10 (p, offset);
--	      when 11	=> FIG0Extension11 (p, offset);
--	      when 12	=> FIG0Extension12 (p, offset);
--	      when 13	=> FIG0Extension13 (p, offset);
--	      when 14	=> FIG0Extension14 (p, offset);
--	      when 15	=> FIG0Extension15 (p, offset);
--	      when 16	=> FIG0Extension16 (p, offset);
--	      when 17	=> FIG0Extension17 (p, offset);
--	      when 18	=> FIG0Extension18 (p, offset);
--	      when 19	=> FIG0Extension19 (p, offset);
--	      when 20	=> FIG0Extension20 (p, offset);
--	      when 21	=> FIG0Extension21 (p, offset);
--	      when 22	=> FIG0Extension22 (p, offset);
	      when others =>
	                 null;
	   end case;
	end process_FIG0;

	procedure FIG0Extension1 (d      : fib_buffer;
	                          offset : int16_t) is
	   bitOffset : int16_t := offset + 2 * 8;	-- offset in bytes
	   Length    : int16_t :=
	                        int16_t (get_Bits (d, offset + 3, 5));
	   PD_Bit    : int16_t :=
	                        int16_t (get_Bits (d, offset + 8 + 2, 1));
	begin
	   while bitOffset / 8 < Length - 1 loop
	      bitOffset := bitOffset / 8;
	      bitOffset := HandleFIG0Extension1 (d, 8 * bitOffset, PD_bit);
	   end loop;
	end FIG0Extension1;
--
--	Note that below the offset is in bytes
	function HandleFIG0Extension1 (d      : fib_buffer;
	                               offset : int16_t;
	                               PD_Bit : int16_t)
	                                          return int16_t is
	   bitOffset     : int16_t   := offset;
	   subChId       : int16_t   :=
	                        int16_t (get_Bits (d, bitOffset, 6));
	   startAddr     : int16_t   :=
	                        int16_t (get_Bits (d, bitOffset + 6, 10));
	   tableIndex    : int16_t;
	   option        : int16_t;
	   Prot_Level    : int16_t;
	   subChannelSize: int16_t;
	begin
	   ficList (subChId). startAddr := startAddr;
	   if Integer (get_Bits (d, bitOffset + 16, 1)) = 0 then -- short form
	      tableIndex                  :=
	                  int16_t (get_Bits (d, bitOffset + 18, 6));
	      ficList (subChId). Length	  := Prot_Levels (tableIndex, 0);
	      ficList (subChId). uepFlag  := 0;
	      ficList (subChId). Prot_Level := Prot_Levels (tableIndex, 1);
	      ficList (subChId). bitRate  := Prot_Levels (tableIndex, 2);
	      bitOffset	                  := bitOffset + 24;
	   else		-- eep long form
	      ficList (subChId). uepFlag  := 1;
	      option       := int16_t (get_Bits (d, bitOffset + 17, 3));
	      if option = 0 then		 -- A Level protection
	         Prot_Level   := int16_t
	                            (get_Bits (d, bitOffset + 20, 2)) + 1;
 --	we encode the A level protection by adding 0100 to the level
                 ficList (subChId). Prot_Level 	:= Prot_Level + 8#100#;
                 subChannelSize	:= int16_t 
	                             (get_Bits (d, bitOffset + 22, 10));
                 ficList (subChId). Length   := subChannelSize;

	         case Prot_Level is
	            when 1	=>
                       ficList (subChId). bitRate := subChannelSize / 12 * 8;
                    when 2	=>
                       ficList (subChId). bitRate := subChannelSize / 8 * 8;
                    when 3	=>
                       ficList (subChId). BitRate := subChannelSize / 6 * 8;
                    when 4	=>
                       ficList (SubChId). BitRate := subChannelSize / 4 * 8;
	            when others	=> null;
	         end case;
	      else		-- option should be 01
	         Prot_Level  := int16_t (get_Bits (d, bitOffset + 20, 2)) + 1;
--	we encode the B protection levels by adding a 0200 to the level
                 ficList (subChId). Prot_Level := Prot_Level + 8#200#;
                 subChannelSize  := int16_t (get_Bits (d, bitOffset + 22, 10));
                 ficList (subChId). Length  := subChannelSize;
	         case Prot_Level is
	            when 1   =>
                       ficList (subChId). bitRate := subChannelSize / 27 * 32;
	            when 2   =>
                       ficList (subChId). bitRate := subChannelSize / 21 * 32;
	            when 3   =>
                       ficList (subChId). bitRate := subChannelSize / 18 * 32;
	            when 4 =>
                       ficList (subChId). bitRate := subChannelSize / 15 * 32;
	            when others	=> null;	-- cannot happen
	         end case;
	      end if;
	      bitOffset	:= bitOffset + 32;
	   end if;
	   return bitOffset;	-- we return bytes
	end HandleFIG0Extension1;

	procedure FIG0Extension2 (d:      fib_buffer;
	                          offset: int16_t) is
	   bitOffset   : int16_t  := offset + 2 * 8; -- offset in bits
	   Length      : int16_t  := int16_t (get_Bits (d, offset + 3, 5));
	   CN          : int16_t  := int16_t (get_Bits (d, offset + 8 + 0, 1));
	   PD_Bit      : int16_t  := int16_t (get_Bits (d, offset + 8 + 2, 1));
	begin
	   while bitOffset / 8 < Length loop
	      bitOffset := bitOffset / 8;
	      bitOffset := HandleFIG0Extension2 (d, bitOffset * 8, CN, PD_bit);
	   end loop;
	end FIG0Extension2;

--
--	Note Offset is in bytes
--	With FIG0/2 we bind the channels to Service Ids
--
--	Terminology
--	SId		service Identifier, can be 32 bits!!!
--	SubChId		SubchannelIdentifier
--	SCId		Service Component Identifier
--
	function HandleFIG0Extension2 (d      : fib_buffer;
	                               offset : int16_t;
	                               cn     : int16_t;
	                               pd     : int16_t)
	                                     return int16_t is
	   bitOffset          : int16_t := offset;
	   SId                : uint32_t;
	   numberofComponents : int16_t;
	begin
	   if pd = 1 then	--  long Sid
	      SId        := getLBits (d, bitOffset, 32);
	      bitOffset  := bitOffset + 32;
	   else
	      SId        := uint32_t (get_Bits (d, bitOffset, 16));
	      bitOffset  := bitOffset + 16;
	   end if;

	   numberofComponents	:=
	                    int16_t (get_Bits (d, bitOffset + 4, 4));
	   bitOffset		:=  bitOffset + 8;

	   for i in 0 ..  int16_t (numberofComponents) - 1 loop
	      if get_Bits (d, bitOffset, 2) = 0 then	      -- TMid
	         declare
	            ASCTy:   int16_t :=
	                    int16_t (get_Bits (d, bitOffset + 2, 6));
	            SubChId: int16_t :=
	                    int16_t (get_Bits (d, bitOffset + 8, 6));
	            PS_flag: int16_t :=
	                    int16_t (get_Bits (d, bitOffset + 14, 1));
	         begin
	            bind_audioService (0, SId, i, SubChId, PS_flag, ASCTy);
	         end;
	      else	-- TMId /= 0, i.e. packetservice
	         null;	-- not supported in the ada version
	      end if;
	      bitOffset  := bitOffset + 16;
	   end loop;

	   return bitOffset;		-- Offset to bits is in Bytes
	end HandleFIG0Extension2;

	procedure process_FIG1  (d      : fib_buffer;
	                         offset : int16_t) is
	   charSet   : uint16_t;
	   oe        : uint16_t;
	   extension : uint16_t;
	   SId       : uint32_t;
	   label     : String (1 .. 16);
	   loffset   : int16_t  := offset;
	   myIndex   : shortRange;
	begin
	   charSet   := get_Bits (d, offset + 8, 4);
           oe        := get_Bits (d, offset + 8 + 4, 1);
           extension := get_Bits (d, offset + 8 + 5, 3);

	   case extension is
	      when 0   =>		-- ensemble label
	         SId       := uint32_t (get_Bits (d, offset + 16, 16));
	         loffset   := offset + 32;
	         for i in 0 .. 15 loop
	            declare
	              t : uint16_t := get_Bits (d,
	                                        loffset + 8 * int16_t (i),                                                 8);
	            begin
	              label (i + 1) := Character' Val (Integer (t));
	            end;
	         end loop;
	         if not Has_Name then
	            ensembleName   := label;
	            Has_Name       := true;
	            string_messages.
	                 string_messages. Put ((ENSEMBLE_SIGNAL, label));
	         end if;
	  
	      when 1   => 	-- 16 bit Identifier field for service label
	         SId      := uint32_t (get_Bits (d, offset + 16, 16));
	         loffset  := offset + 32;
	         myIndex  := findServiceId (SId);
	         if  not listofServices (myIndex). serviceLabel. Has_Name then
	            for i in 0 .. 15 loop
	               label (i + 1) := Character' Val (
	                         Integer (get_Bits (d, loffset + 8 *  int16_t (i), 8)));
	            end loop;
	            listofServices (myIndex). serviceLabel. label := label;
	            listofServices (myIndex). serviceLabel. Has_Name := true;
	            string_messages.
	                    string_messages. Put ((PROGRAM_SIGNAL, label));
--	            put ("new service "); put_line (label);
	         end if;
	      when others	=> null;
	   end case;
	end process_FIG1;

	procedure process_FIG2  (d      : fib_buffer;
	                         offset : int16_t) is
	begin
	   null;
	end process_FIG2;

	procedure process_FIG3  (d      : fib_buffer;
	                         offset : int16_t) is
	begin
	   null;
	end process_FIG3;

	function get_Bits (d:      fib_Buffer;
	                   offset: int16_t;
	                   size:   int16_t) return uint16_t is
	   result:       uint16_t  := uint16_t (0);
	   realOffset:   int16_t	:= offset +  int16_t (d'First);
	begin
	   for I in int16_t Range 0 .. size - 1 loop
	      result := Interfaces. Shift_Left (result, 1);
	      result := result or uint16_t (d (Integer (realOffset + I)));
	   end loop;
	   return result;
	end get_Bits;

	function getLBits (d      : fib_Buffer;
	                   offset : int16_t;
	                   size   : int16_t) return uint32_t is
	   result     : uint32_t        := uint32_t (0);
	   realOffset : int16_t   := offset +  int16_t (d'First);
	begin
	   for I in int16_t Range 0 .. size - 1 loop
	      result := Interfaces. Shift_Left (result, 1);
	      result := result or uint32_t (d (Integer (realOffset + I)));
	   end loop;
	   return result;
	end getLBits;
--
--	locate - and create if needed - a reference to
--	the entry in the list of services
	function findServiceId (Service_Id : uint32_t) return int16_t is
	begin
	   for I in listofServices' Range loop
	      if listofServices (I). In_Use and then
	                 listofServices (I). Service_Id = Service_Id then
	          return I;
	      end if;
	   end loop;
--
--	not found, create one
	   for I in listofServices' Range loop
	      if not listofServices (I) . In_Use then
	         listofServices (I). In_Use     := true;
	         listofServices (I). serviceLabel. Has_Name := false;
	         listofServices (I). Service_Id	:= Service_Id;
	         return  int16_t (I);
	      end if;
	   end loop;
	   return 0;
	end findServiceId;
--
--	bind_audioService is the main processor for - what the name suggests -
--	connecting the description of an audioservice to the Service Identifier
	procedure bind_audioService (TMid    : int16_t;
	                             SId     : uint32_t;
	                             compnr  : int16_t;
	                             subChId : int16_t;
	                             ps_flag : int16_t;
	                             ASCTy   : int16_t) is
	   S         : int16_t := findServiceId (SId);
	   firstFree : int16_t := -1;
	begin
	   for i in shortRange loop
	      if not components (i). In_Use then
	         if firstFree = -1 then
	            firstFree := i;
	         end if;
	      elsif components (i). service = s and then
	            components (i). Component_Nr = compnr and then
	                 components (i). TMid = TMid then
	         return;
	      end if;
	   end loop;
	   components (firstFree). In_Use        := true;
	   components (firstFree). TMid          := TMid;
	   components (firstFree). Component_nr  := compnr;
	   components (firstFree). service       := s;
           components (firstFree). Subchannel_Id := subChId;
           components (firstFree). PS_flag       := ps_flag;
           components (firstFree). ASCTy         := ASCTy;
	end bind_audioService;

	procedure process_FIB (p  : fib_buffer) is
	   FIGtype        : uint16_t;
	   processedBytes : int16_t         := 0;
	   bitOffset      : int16_t         := 0;
	begin
	   while processedBytes < 30 loop
	      FIGtype   := get_Bits (p, bitOffset, 3);
	      case FIGtype is
	         when 0   => process_FIG0 (p, bitOffset);
	         when 1   => process_FIG1 (p, bitOffset);
	         when others   => null;	-- for now
	      end case;
	      processedBytes   := processedBytes +
	                              int16_t (get_Bits (p, bitOffset + 3, 5)) + 1;
	      bitOffset        := processedBytes * 8;
	   end loop;
	end process_FIB;

	procedure Reset is
	begin
	   Has_Name   := false;
	   for I in listofServices' Range loop
	      listofServices (i). In_Use   := false;
	   end loop;

	   for I in components' Range loop
	      components (i). In_Use       := false;
	   end loop;
	end reset;

	function syncReached return Boolean is
	begin
	   return Has_Name;
	end syncReached;
--
	procedure Data_for_AudioService (Name_of_Program : String;
	                                 Data            : out audioData) is
	   Selected_Service:   uint32_t;
	begin
--	first we locate the Service_Id

	   for i in listofServices' Range loop
	      if listofServices (i). In_Use and then
	         listofServices (i). serviceLabel. Has_Name and then
	            listofServices (i). serviceLabel. label = 
	                                               Name_of_Program then
	         Selected_Service := listofServices (i). Service_Id;
	         for J in components' Range loop
	            if components (J). In_Use and then
	               components (J). service = i and then
	                  components (J). TMid = 0 then	
	               declare
	                 Subch_Id: int16_t :=
	                                    components (J). Subchannel_Id;
	               begin
	                  Data. dataIsThere   := true;
	                  Data. SubchId       := Subch_Id;
	                  Data. startAddr     := ficList (Subch_Id). startAddr;
	                  Data. uepFlag       := ficList (Subch_Id). uepFlag;
	                  Data. protLevel     := ficList (Subch_Id). Prot_Level;
	                  Data. length        := ficList (Subch_Id). Length;
	                  Data. bitRate	      := ficList (Subch_Id). BitRate;
	      	          Data. ASCTy         := components (j). ASCTy;
	      	          Data. language      := listofServices (i). The_Language;
	      	          Data. programtype   := listofServices (i). Program_Type;
	                  return;
	               end;
	            end if;
	         end loop;
	      end if;
	   end loop;
--
--	if/when we are here (should not happen though), we apparently 
--	could not locate the data
	   Data. dataIsThere   := false;
	end Data_for_AudioService;
end fib_handler;

