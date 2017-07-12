with header;             use header;
with Interfaces;	 use Interfaces;
with Interfaces. C;
with System;             use System;
with protection_handler; use protection_handler;
with Ada. Finalization;  use Ada. Finalization;
with audiopackage;       use audiopackage;
with audio_handler;      use audio_handler;
package dab_handler is
	type dabProcessor (dabModus      : dataMode;
	                   fragmentSize  : Natural;
	                   bitRate       : int16_t;
	                   uepFlag       : int16_t;
	                   protLevel     : int16_t;
	                   audio         : audiopackage. audioSink_P) is
                            new Ada. Finalization. Controlled with Private;

	procedure process (Object  : in out dabProcessor; data : shortArray);
	type dabProcessor_P is access all dabProcessor;
private
	procedure initialize (Object : in out dabProcessor);
	procedure finalize (Object : in out dabProcessor);
	type shortBlock	is array (short_integer Range <>,
	                          Integer range <>) of int16_t;
	type shortBlock_P	is access all shortBlock;
	type dabProcessor (dabModus      : dataMode;
	                   fragmentSize  : Natural;
	                   bitRate       : int16_t;
	                   uepFlag       : int16_t;
	                   protLevel     : int16_t;
	                   audio         : audiopackage. audioSink_P) is
                            new Ada. Finalization. Controlled with 
	record
	   interleaveData     : shortBlock (0 .. 15, 0 .. fragmentSize);
           countforInterleaver: int16_t;
           interleaverIndex   : int16_t;
	   The_ProtectionProcessor: protection_handler. protectionProcessor_P;	
	   The_AudioProcessor     : audio_handler. Audio_Processor_P;
	end record;
end dab_handler;

