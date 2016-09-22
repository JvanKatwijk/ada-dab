
package body fir_filters is
	procedure Initialize (filter: in out lowPass_filter) is
	   fs:         Integer renames filter. filterSize;
	   sampleRate: Integer renames filter. samplerate;
	   tmp:        FloatArray (0 .. fs  - 1);
	   f:          Float := Float (filter. filterFreq) / Float (sampleRate);
	   sum:        Float := 0.0;
	begin
	   filter. filterKernel := new complexArray (0 .. fs - 1);
	   filter. buffer       := new complexArray (0 .. fs - 1);
	   filter. buffer.all   := (Others => (0.0, 0.0));
	   filter. ip           := 0;

	   for i in 0 .. fs - 1 loop
	      if i = fs / 2 then
	         tmp (i) := 2.0 * M_PI * Float (f);
	      else
	         declare
	            nom:    Float  := 2.0 * M_PI * Float (f);
	            denom:  Float  := Float (i - fs / 2);
	         begin
	            tmp (i)   := Math. sin (nom * denom) / denom;
	         end;
	      end if;
--
--	and the Blackman window applied on this element
	      declare
	         i_f    : Float  := Float (i);
	         fs_f   : Float  := Float (fs);
	      begin
	         tmp (i)   := tmp (i) * (
	                              0.42 -
	                          0.5 * Math. cos (2.0 * M_PI * i_f / fs_f) +
	                          0.08 * Math. cos (4.0 * M_PI * i_f / fs_f));
	         sum       := sum + tmp (i);
	      end;
	   end loop;

	   for i in 0 .. fs - 1 loop
	      filter. filterKernel (i) := (tmp (i) / sum, 0.0);
	   end loop;
	end Initialize;
	
	procedure Finalize  (filter: in out lowPass_filter) is
	begin
	   Free_complexArray (filter. filterKernel);
	   Free_complexArray (filter. buffer);
	end Finalize;

	function Pass   (filter:  in out lowPass_filter;
	                 data:    complex) return complex is
	   tmp:   complex      := (0.0, 0.0);
	begin
	   filter. buffer (filter. ip)  := data;
	   for i in 0 .. filter. filterSize - 1 loop
	      declare
	         index:  Integer    := filter. ip - i;
	      begin
	         if index < 0 then
	            index   := index + filter. filterSize;
	         end if;
	         tmp   := tmp + filter. buffer (index) *
	                                        filter. filterKernel (i);
	      end;
	   end loop;
	   filter. ip   := (filter. ip + 1) mod filter. filterSize;
	   return tmp;
	end Pass;
end fir_filters;

