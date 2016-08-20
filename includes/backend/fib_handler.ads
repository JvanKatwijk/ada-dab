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
   subtype fib_buffer is ByteArray (0 .. 255);
   procedure process_FIB (p: fib_buffer); --
   procedure reset;
   function syncReached return Boolean;
   procedure dataforAudioService (s: String; d: out audioData);
private
--	FIB;s are segments of 256 bits. When here
--	we already passed the crc and start unpacking
   procedure process_FIG0	(p: fib_buffer; offset: short_Integer);
   procedure process_FIG1	(d: fib_buffer; offset: short_Integer);

   procedure FIG0Extension1(d: fib_buffer; offset: short_Integer);
   function HandleFIG0Extension1 (d	: fib_buffer;
	                          offset	: short_Integer;
	                          PD_Bit	: short_Integer)
	                                        return short_Integer;
   procedure FIG0Extension2	(d	: fib_buffer; offset: short_Integer);
   function HandleFIG0Extension2	(d	: fib_buffer;
	                         offset	: short_Integer;
	                         cn	: short_Integer;
	                         pd	: short_Integer) return short_Integer;
   function get_Bits	(d	: fib_buffer;
	                 offset	: short_Integer;
	                 size	: short_Integer) return uint16_t;
   function getLBits	(d	: fib_buffer;
	                 offset	: short_Integer;
	                 size	: short_Integer) return uint32_t;

   function findServiceId	(serviceId	: uint32_t) return short_Integer;
   procedure bind_audioService (TMid	: short_Integer;
	                     SId	: uint32_t;
	                     compnr	: short_Integer;
	                     subChId	: short_Integer;
	                     ps_flag	: short_Integer;
	                     ASCTy	: short_Integer);
end fib_handler;

