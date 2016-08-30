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
with Gtk.Combo_Box_Text;	use Gtk. Combo_Box_Text;
with System; use System;
with ringbuffer;
with header; use header;
with Interfaces; use Interfaces;
package airspy_wrapper is
   procedure setupGainTable  (gainSelector: Gtk_Combo_Box_Text);
   procedure setVFOFrequency (frequency	: Integer);
   procedure restartReader   (res	: out Boolean);
   procedure stopReader;
   procedure setExternalGain (gain : Integer);
   procedure getSamples      (outV : out complexArray; amount : out Integer);
   function Samples          return Integer;
   function isValid	     return Boolean;
private
type airspyError is (
        AIRSPY_SUCCESS,
        AIRSPY_TRUE,
        AIRSPY_ERROR_OTHER 
);

for airspyError use (
        AIRSPY_SUCCESS 			=> 0,
        AIRSPY_TRUE 			=> 1,
	AIRSPY_ERROR_OTHER		=> 2
);

type sample_type is (
	AIRSPY_SAMPLE_FLOAT32_IQ,
        AIRSPY_SAMPLE_FLOAT32_REAL,
        AIRSPY_SAMPLE_INT16_IQ,
        AIRSPY_SAMPLE_INT16_REAL,	-- 1 * 16bit int per sample 
        AIRSPY_SAMPLE_UINT16_REAL,	-- 1 * 16bit unsigned int per sample 
        AIRSPY_SAMPLE_END		-- Number of supported sample types 
);

for sample_type use (
	AIRSPY_SAMPLE_FLOAT32_IQ	=> 0, -- 2 * 32bit float per sample 
        AIRSPY_SAMPLE_FLOAT32_REAL	=> 1, -- 1 * 32bit float per sample 
        AIRSPY_SAMPLE_INT16_IQ		=> 2, -- 2 * 16bit int per sample 
        AIRSPY_SAMPLE_INT16_REAL	=> 3, -- 1 * 16bit int per sample 
        AIRSPY_SAMPLE_UINT16_REAL 	=> 4, -- 1 * 16bit unsigned int per sample */
        AIRSPY_SAMPLE_END 		=> 5  -- Number of supported sample types 
);

type airspy_transfer is record
	device		: System. Address;
	ctx		: System. Address;
	samples		: System. Address;
	sampleCount	: Integer;
	airspy_sample_type	: sample_type;
end record;
pragma Convention (C, airspy_transfer);

type airspy_transfer_P	is access all airspy_transfer;

function airspy_init return  airspyError;
pragma Import (C, airspy_init, "airspy_init");

function airspy_open		(deviceP: access system. Address)
	                                           return airspyError;
pragma Import (C, airspy_open, "airspy_open");

procedure airspy_set_vga_gain	(device	: system. Address; vgaGain : Integer);
pragma Import (C, airspy_set_vga_gain, "airspy_set_vga_gain");

procedure airspy_set_mixer_gain	(device : system. Address; mixerGain : Integer);
pragma Import (C, airspy_set_mixer_gain, "airspy_set_mixer_gain");

procedure airspy_set_lna_gain	(device : system. Address; lnaGain : Integer);
pragma Import (C, airspy_set_lna_gain, "airspy_set_lna_gain");

procedure airspy_set_sensitivity (device : system. Address; gain : Integer);
pragma Import (C, airspy_set_sensitivity, "airspy_set_sensitivity_gain");

function airspy_is_streaming (device : system. Address) return uint8_t;
pragma Import (C, airspy_is_streaming, "airspy_is_streaming");

function airspy_set_sample_type (device : system. Address;
	                         the_type : sample_type) return airspyError;
pragma Import (C, airspy_set_sample_type, "airspy_set_sample_type");
--
--	just for this one
subtype rateBuffer is intArray (0 .. 20);
procedure airspy_get_samplerates (device	: system. Address;
	                          outBuffer	: out rateBuffer;
	                          count		: Integer);
pragma Import (C, airspy_get_samplerates, "airspy_get_samplerates");

function airspy_set_samplerate	(device : system. Address;
	                         rate : Integer) return airspyError;
pragma Import (C, airspy_set_samplerate, "airspy_set_samplerate");

function airspy_set_freq (device : system. Address; nf : Integer)
	                                              return airspyError;
pragma Import (C, airspy_set_freq, "airspy_set_freq");

type airspy_CallbackType is access
	function (transfer : airspy_transfer_P) return Integer;
pragma Convention (C, airspy_CallbackType);

function  airspy_start_rx	(device : system. Address;
                                 cb	: airspy_CallbackType;
	                         userdata : system. Address) return airspyError;
pragma Import (C, airspy_start_rx, "airspy_start_rx");

procedure  airspy_stop_rx	(device : system. Address);
pragma Import (C, airspy_stop_rx, "airspy_stop_rx");
function airspy_Callback (transfer	: airspy_transfer_P) return Integer;
pragma Convention (C, airspy_Callback);
--
end airspy_wrapper;

