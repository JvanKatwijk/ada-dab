--
--
--    Copyright (C) 2013
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
-- 	superframer for the SDR-J DAB+ receiver
-- 	This processor handles the whole DAB+ specific part
--***********************************************************************
--	may 15 2015. A real improvement on the code
--	is the addition from Stefan Poeschel to create a
--	header for the aac that matches, really a big help!!!!
--***********************************************************************


with header;		use header;
with Interfaces;	use Interfaces;
with Interfaces. C;	use Interfaces. C;
with audio_handler;

package mp4_handler is
	type mp4Processor  is new audio_handler. Audio_Processor with private;
	type mp4Processor_P is access all mp4Processor;

	procedure addtoFrame (Object  :   in out mp4Processor;
	                      V       :   byteArray;
	                      nbits   :   short_Integer);
private
	procedure processSuperframe (Object    :  in out mp4Processor;
	                             frameBytes:  byteArray;
	                             base      :  short_Integer;
	                             result    :  out Boolean);
	procedure Initialize	(Object	: in out mp4Processor);
	procedure Finalize	(Object	: in out mp4Processor);
	function dabPlus_crc	(Data_Vector    : byteArray;
	                         Start_Byte     : uint16_t;
	                         Length         : uint16_t) return Boolean;

   	type mp4Processor is new audio_handler. Audio_Processor with
	   record
	      Superframe_size  :  Integer;
	      RSDims           :  Integer;
	      RSin_Data        :  ByteArray_P;
	      RSout_Data       :  ByteArray_P;
	      Block_FillIndex  :  Integer;
	      Blocks_InBuffer  :  Integer;
	      Framecount       :  Integer;
	      Frameerrors      :  Integer;
	      nErrors          :  short_Integer;
	      au_count         :  Integer;
	      au_errors        :  Integer;
	   end record;
end mp4_handler;

