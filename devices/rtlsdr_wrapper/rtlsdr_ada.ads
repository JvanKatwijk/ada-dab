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
with System;
with header;	use header;
with Interfaces. C;

package rtlsdr_ada is
	type rtlsdr_dev	is new System. Address;
	type rtlsdr_dev_t is access rtlsdr_dev;
	type devicePointer	is access rtlsdr_dev_t;
	READLEN_DEFAULT	: constant	:= 2 * 8192;

	type rtlsdr_CallbackType is access
	                procedure (buffer   : System. Address;
	                           size     : Interfaces. C. int;
	                           userData : System. Address);
	pragma Convention (C, rtlsdr_CallbackType);

	function rtlsdr_open_ada (device_p     : devicePointer;
	                          deviceNumber : Interfaces. C. int)
	                                           return Interfaces. C. int;
	pragma Import (C, rtlsdr_open_ada,  "rtlsdr_open");

	procedure rtlsdr_close_ada (device    : rtlsdr_dev_t);
	pragma Import (C, rtlsdr_close_ada,  "rtlsdr_close");

	procedure rtlsdr_set_center_freq_ada (device    : rtlsdr_dev_t;
	                                      frequency : Interfaces. C. int);
	pragma Import (C, rtlsdr_set_center_freq_ada, "rtlsdr_set_center_freq");

	function rtlsdr_get_center_freq_ada (device : rtlsdr_dev_t)
	                                            return Interfaces. C. int;
	pragma Import (C, rtlsdr_get_center_freq_ada, "rtlsdr_get_center_freq");

	procedure rtlsdr_get_tuner_gains_ada (device : rtlsdr_dev_t;
	                                      gains  : access Interfaces.C. int);
	function rtlsdr_get_tuner_gains_ada (device: rtlsdr_dev_t;
	                                     dummy : Interfaces. C. int)
	                                             return Interfaces. C. int;
	pragma Import (C, rtlsdr_get_tuner_gains_ada, "rtlsdr_get_tuner_gains");

	procedure rtlsdr_set_tuner_gain_mode_ada (device: rtlsdr_dev_t;
	                                          gainMode: Interfaces. C. int);
	pragma Import (C, rtlsdr_set_tuner_gain_mode_ada,
	                                         "rtlsdr_set_tuner_gain_mode");

	procedure rtlsdr_set_tuner_gain_ada (device : rtlsdr_dev_t;
	                                           gain : Interfaces. C. int);
	pragma Import (C, rtlsdr_set_tuner_gain_ada, "rtlsdr_set_tuner_gain");

	function rtlsdr_get_tuner_gain_ada (device : rtlsdr_dev_t)
	                                             return Interfaces. C. int;
	pragma Import (C, rtlsdr_get_tuner_gain_ada, "rtlsdr_get_tuner_gain");

	procedure rtlsdr_set_sample_rate_ada (device : rtlsdr_dev_t;
	                                             rate: Interfaces. C. int);
	pragma Import (C, rtlsdr_set_sample_rate_ada, "rtlsdr_set_sample_rate");

	function rtlsdr_get_sample_rate_ada (device : rtlsdr_dev_t)
	                                              return Interfaces. C. int;
	pragma Import (C, rtlsdr_get_sample_rate_ada,
	                                     "rtlsdr_get_sample_rate");

	function rtlsdr_reset_buffer_ada (device: rtlsdr_dev_t)
	                                              return Interfaces. C. int;
	pragma Import (C, rtlsdr_reset_buffer_ada, "rtlsdr_reset_buffer");

	procedure rtlsdr_read_async_ada (device      : rtlsdr_dev_t;
                                         theCallback : rtlsdr_CallbackType;
                                         userData    : System. Address;
	                                 bufNum      : Interfaces. C. int;
	                                 bufLen      : Interfaces. C. int);
	pragma Import (C, rtlsdr_read_async_ada, "rtlsdr_read_async");

	procedure rtlsdr_cancel_async_ada (device: rtlsdr_dev_t);
	pragma Import (C, rtlsdr_cancel_async_ada, "rtlsdr_cancel_async");

	function rtlsdr_get_device_count_ada return Interfaces. C. int;
	pragma Import (C, rtlsdr_get_device_count_ada,
	                                         "rtlsdr_get_device_count");

	function rtlsdr_set_freq_correction_ada (device : rtlsdr_dev_t;
	                                         amount : Interfaces. C. int)
	                                             return Interfaces. C. int;
	pragma Import (C, rtlsdr_set_freq_correction_ada,
	                                         "rtlsdr_set_freq_correction");
end rtlsdr_ada;
