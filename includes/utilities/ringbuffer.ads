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
with header; use header;
with Ada. Finalization; use Ada. Finalization;
generic
type elementType is private;
package ringbuffer is
	type buffer_data is array (Integer range <>) of elementType;
	type buffer_data_P is access all buffer_data;
	type ringbuffer_data (bufferSize: Integer) is  new 
	                   Ada. Finalization. Controlled with private;
	type ringbuffer_data_P is access all ringbuffer_data;
	function GetRingBufferReadAvailable (buffer: ringbuffer_data)
	                                                return Integer;
	function GetRingBufferWriteAvailable (buffer: ringbuffer_data)
	                                                return Integer;
	procedure FlushRingBuffer (buffer: in out ringbuffer_data);
	procedure putDataIntoBuffer (buffer: in out ringbuffer_data;
	                             data: buffer_data);
	procedure getDataFromBuffer (buffer: in out ringbuffer_data;
	                             data  : out  buffer_data;
	                             amount: out Integer);
--
private
	type ringbuffer_data (bufferSize: Integer) is new
	                          Ada. Finalization. Controlled with 
	record
	   writeIndex		: Integer;
	   readIndex		: Integer;
	   buffer		: buffer_data_P;
	end record;
	procedure Initialize (Object: in out ringbuffer_data);
	procedure Finalize   (Object: in out ringbuffer_data);
	procedure WriteMemoryBarrier;
	pragma Import (C, WriteMemoryBarrier, "RingBuffer_WriteMemoryBarrier");
	procedure ReadMemoryBarrier;
	pragma Import (C, ReadMemoryBarrier, "RingBuffer_ReadMemoryBarrier");
	procedure FullMemoryBarrier;
	pragma Import (C, FullMemoryBarrier, "RingBuffer_FullMemoryBarrier");
end ringbuffer;

