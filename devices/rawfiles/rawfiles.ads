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
with device_handler;            use device_handler;
with System;                    use System;
with header;                    use header;
with Interfaces;                use Interfaces;
-- with Interfaces.C;              use Interfaces. C;
with ringbuffer;

package rawfiles is
	use header. complexTypes;
	type raw_device is new device with private;
	type raw_device_p is access all raw_device;

	overriding
	procedure Restart_Reader   (Object       : in out raw_device;
	                            Success      : out Boolean);
	procedure Stop_Reader      (Object       : in out raw_device);
	procedure Set_VFOFrequency (Object       : in out raw_device;
	                            New_Frequency: Natural);
	procedure Set_Gain         (Object       : in out raw_device;
	                            New_Gain     : Natural);
	procedure Get_Samples      (Object       : in out raw_device;
	                            Out_V        : out complexArray;
	                            Amount       : out Natural);
	function Available_Samples (Object       : raw_device) return Natural;
	function Valid_Device      (Object       : raw_device) return Boolean;

private
	bufferSize : constant Integer := 4 * 32768;
	procedure Initialize (Object: in out raw_device);
	procedure Finalize   (Object: in out raw_device);

	type element is record
           a : uint8_t;
           b : uint8_t;
        end record;
--      we read in segments of 10 millseconds at a time
        package raw_Buffer is new ringbuffer (element);
        use raw_Buffer;
	type Buffer_P is access ringbuffer_data;

	task type doRead is
           entry Start (theBuffer: Buffer_P; name : String);
        end doRead;
	type worker_P is access all doRead;

	type raw_device is new device with record
	   theBuffer : Buffer_P;
	   theWorker : worker_P;
	end record;
end rawfiles;
