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
with Interfaces;
with Text_IO; use Text_IO;
with header; use header;

package fib_handler is
   subtype   fib_buffer is ByteArray (0 .. 255);
   procedure process_FIB (p: fib_buffer); --
   procedure reset;
   function  syncReached return Boolean;
   procedure Data_for_AudioService (Name_of_Program : String;
	                            Data            : out audioData);
private
--	FIB;s are segments of 256 bits. When here
--	we already passed the crc and start unpacking
   procedure process_FIG0	(p: fib_buffer; offset: int16_t);
   procedure process_FIG1	(d: fib_buffer; offset: int16_t);
   procedure process_FIG2	(d: fib_buffer; offset: int16_t);
   procedure process_FIG3	(d: fib_buffer; offset: int16_t);

   procedure FIG0Extension1(d: fib_buffer; offset: int16_t);
   function HandleFIG0Extension1 (d	: fib_buffer;
	                          offset	: int16_t;
	                          PD_Bit	: int16_t)
	                                        return int16_t;
   procedure FIG0Extension2	(d	: fib_buffer; offset: int16_t);
   function HandleFIG0Extension2	(d	: fib_buffer;
	                         offset	: int16_t;
	                         cn	: int16_t;
	                         pd	: int16_t) return int16_t;
   function get_Bits	(d	: fib_buffer;
	                 offset	: int16_t;
	                 size	: int16_t) return uint16_t;
   function getLBits	(d	: fib_buffer;
	                 offset	: int16_t;
	                 size	: int16_t) return uint32_t;

   function findServiceId      (Service_Id : uint32_t) return int16_t;
   procedure bind_audioService (TMid       : int16_t;
	                        SId        : uint32_t;
	                        compnr     : int16_t;
	                        subChId    : int16_t;
	                        ps_flag    : int16_t;
	                        ASCTy      : int16_t);
end fib_handler;

