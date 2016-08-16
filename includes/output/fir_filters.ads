with Header; use Header;
with Ada. Finalization;
package fir_filters is
use complexTypes;

   type lowPass_filter (filterSize : Integer;
	                filterFreq : Integer;
	                sampleRate : Integer) is
	             new Ada. Finalization. Controlled with private;
   function Pass (filter: in out lowPass_filter;
	          data	: complex) return complexTypes. complex;
private
   type lowPass_filter (filterSize : Integer;
	                filterFreq : Integer;
	                sampleRate : Integer) is
	             new Ada. Finalization. Controlled with 
	record
	   filterKernel	: complexArray_P;
	   buffer	: complexArray_P;
	   ip		: Integer;
	end record;
   procedure Initialize (filter: in out lowPass_filter);
   procedure Finalize	(filter: in out lowPass_filter);
end fir_filters;
