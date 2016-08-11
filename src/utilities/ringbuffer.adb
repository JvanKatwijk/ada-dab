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
with ringbuffer;
with Text_IO; use Text_IO;
package body ringbuffer is

procedure Initialize (Object: in out ringbuffer_data) is
begin
	Object. writeIndex	:= 0;
	Object. readIndex	:= 0;
	Object. buffer		:= new buffer_data (0 .. Object. bufferSize - 1);
end Initialize;

procedure Finalize	(Object: in out ringbuffer_data) is
begin
	null;
end Finalize;
--	ensure that previous writes are seen before we update the write index 
--	(write after write)
procedure AdvanceRingBufferWriteIndex (buffer : in out ringbuffer_data;
	                               elementCount : Integer) is
begin
	WriteMemoryBarrier;
	buffer. writeIndex := 
	         (buffer. writeIndex + elementCount) mod buffer. bufferSize;
end;

--	ensure that previous reads (copies out of the ring buffer) are
--	always completed before updating (writing) the read index. 
--	(write-after-read) => full barrier

procedure AdvanceRingBufferReadIndex (buffer : in out ringbuffer_data;
	                              elementCount : Integer) is
begin
	FullMemoryBarrier;
	buffer. readIndex := 
	         (buffer. readIndex + elementCount) mod buffer. bufferSize;
end;

function GetRingBufferReadAvailable (buffer: ringbuffer_data)
	                                                return Integer is
writeIndex	: Integer renames buffer. writeIndex;
readIndex	: Integer renames buffer. readIndex;
bufferSize	: Integer renames buffer. bufferSize;
begin
	FullMemoryBarrier;
	if (readIndex <= writeIndex)
	then
	   return writeIndex - readIndex;
	else
	   return bufferSize - readIndex + writeIndex;
	end if;
end;

function GetRingBufferWriteAvailable (buffer: ringbuffer_data)
	                                                return Integer is
begin
	return buffer. bufferSize - GetRingBufferReadAvailable (buffer);
end;

procedure FlushRingBuffer (buffer: in out ringbuffer_data) is
begin
	FullMemoryBarrier;
	buffer. readIndex := 0;
	buffer. writeIndex := 0;
end;

procedure putDataIntoBuffer (buffer: in out ringbuffer_data;
	                             data: buffer_data) is
writeIndex	: Integer renames buffer. writeIndex;
bufferSize	: Integer renames buffer. bufferSize;
requested	: Integer := data' length;
available	: Integer := GetRingBufferWriteAvailable (buffer);
begin
	if requested > available
	then
	   requested := available;
	end if;
	if bufferSize - writeIndex > requested
	then
	   buffer. buffer (writeIndex ..  writeIndex + requested - 1) :=
	                   data (data' first .. data' first + requested - 1);
	else	-- split it up
	   declare
	      size_1: Integer := bufferSize - writeIndex;
	      size_2: Integer := requested - size_1;
	   begin
	      buffer. buffer (writeIndex .. writeIndex + size_1 - 1) :=
	               data (data' first .. data' first + size_1 - 1);
	      buffer. buffer (0 .. size_2 - 1) :=
	               data (data' first + size_1 .. data' first + size_1 + size_2 - 1);
	   end;
	end if;

	AdvanceRingBufferWriteIndex (buffer, requested);
end putDataIntoBuffer;

procedure getDataFromBuffer (buffer: in out ringbuffer_data;
	                     data : out  buffer_data; amount: out Integer) is
readIndex	: Integer renames buffer. readIndex;
bufferSize	: Integer renames buffer. bufferSize;
requested	: Integer := data' Length;
available	: Integer := GetRingBufferReadAvailable (buffer);
begin
	if requested > available
	then
	   requested := available;
	end if;

	if bufferSize - readIndex > requested
	then		-- easy
	   data (data' first .. data' first + requested - 1) :=
	            buffer. buffer (readIndex .. readIndex + requested - 1);
	else
	declare
	   size_1 : Integer := bufferSize - readIndex;
	   size_2 : Integer := requested - size_1;
	begin
	   data (data' first .. data' first + size_1 - 1) :=
	                  buffer. buffer (readIndex .. bufferSize - 1);
	   data (data' first + size_1 .. data' first + size_1 + size_2 - 1) :=
	                  buffer. buffer (0 .. size_2 - 1);
	end;
	end if;
	AdvanceRingBufferReadIndex (buffer, requested);
	amount := requested;
end getDataFromBuffer;

function g (s: Integer) return Integer is
x	: Integer	:= 16;
old_x	: Integer	:= x;
begin
	while x < s loop
	   old_x	:= x;
	   x		:= x + x;
	end loop;
	return old_x;
end g;
	   
end ringbuffer;

