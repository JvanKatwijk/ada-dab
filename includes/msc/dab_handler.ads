with header; use header;
with Interfaces;	use Interfaces;
with mp4_handler;	use mp4_handler;

package dab_handler is
	task type dabProcessor (dabModus	: dataMode;
	                        fragmentSize	: Integer;
	                        bitRate		: short_Integer;
	                        uepFlag		: short_Integer;
	                        protLevel	: short_Integer) is
	   entry stop;
	   entry process (Data		: shortArray);
	end dabProcessor;
   type dabProcessor_P is access all dabProcessor;
private
   type shortBlock	is array (Integer Range <>,
	                          short_Integer range <>) of short_Integer;
   type shortBlock_P	is access all shortBlock;
end dab_handler;

