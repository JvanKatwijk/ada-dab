with header; use header;
with Interfaces;	use Interfaces;
with audiopackage;	use audiopackage;
package dab_handler is
	task type Dabprocessor (dabModus	: dataMode;
	                        fragmentSize	: Integer;
	                        bitRate		: short_Integer;
	                        uepFlag		: short_Integer;
	                        protLevel	: short_Integer;
	                        audio		: audiopackage. audioSink_P) is
	   entry stop;
	   entry process (Data		: shortArray);
	end dabProcessor;
   type dabProcessor_P is access all dabProcessor;
private
   type shortBlock	is array (short_integer Range <>,
	                          Integer range <>) of short_Integer;
   type shortBlock_P	is access all shortBlock;
end dab_handler;

