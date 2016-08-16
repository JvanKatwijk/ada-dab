with mp2_handler;
with Interfaces; use Interfaces;
with Text_IO; use Text_IO;
with audiopackage; use audiopackage;
package body mp2_handler is
KJMP2_SAMPLES_PER_FRAME : constant Integer	:= 1152;

outBuffer	: shortArray (0 .. 2 * KJMP2_SAMPLES_PER_FRAME - 1);
sample_Buf	: complexArray (0 .. KJMP2_SAMPLES_PER_FRAME - 1);

procedure Initialize (Object : in out mp2Processor) is
begin
	kjmp2_init (Object. context' Address);
	Object. MP2framesize	:= 24 * Integer (Object. bitrate);
	Object. MP2Header_OK	:= 0;
	Object. MP2HeaderCount	:= 0;
	Object. MP2bitCount	:= 0;
	Object. baudRate	:= 48000;	-- default
	Object. MP2frame	:= new byteArray (0 .. 24 * Integer (Object. bitRate));
end Initialize;

procedure Finalize (Object : in out mp2processor) is
begin
	null;
end Finalize;

--
--	just a local
procedure addbittoMP2	(v : in out byteArray;
	                 b	: uint8_t;
	                 bitp	: Integer) is
byte	: uint8_t	:= v (bitp / 8);
bitnr	: Integer	:= 7 - Integer (uint32_t (bitp) and 8#07#);
newbyte	: uint8_t	:= Shift_Left (uint8_t (01), bitnr);
begin
	if b = 0
	then
	   byte	:= byte and (not newbyte);
	else
	   byte	:= byte or newbyte;
	end if;
	v (bitp / 8)	:= byte;
end addbittoMP2;

--
--	Note: V, input, is in bits, while the result is
--	in bytes
procedure addtoFrame (Object	: in out mp2Processor;
	              V		: byteArray;
	              nbits	: short_Integer) is
lf	: Integer	:= 
	         (if Object. baudRate = 48000 then Object. MP2framesize
	                                      else 2 * Object. MP2framesize);
count	: Integer	:= 0;
begin
	for i in 0 .. Integer (nbits) - 1 loop
	   if Object. MP2Header_OK = 2
	   then
	      addbittoMP2 (Object. MP2frame. all,
	                   v (i),
	                   Object. MP2bitCount);
	      Object. MP2bitCount	:= Object. MP2bitCount + 1;
	      if Object. MP2bitCount >= lf
	      then
	         count	:= kjmp2_decode_frame (Object. context' Address,
	                                       Object. MP2frame. all' Address,
	                                       outBuffer' Address);
	      
	         if count <= 0
	         then
	            return;	-- something wrong
	         end if;
	         for i in 0 .. KJMP2_SAMPLES_PER_FRAME - 1 loop
	            sample_Buf (i) := (Float (outBuffer (2 * i    )) / 32768.0,
	                               Float (outBuffer (2 * i + 1)) / 32768.0);
	         end loop;
	         Object. pcmHandler. putSamples (sample_Buf,
	                                         uint64_t (Object. baudRate));
	         Object. MP2Header_OK	:= 0;
	         Object. MP2HeaderCount	:= 0;
	         Object. MP2bitCount	:= 0;
	      end if;
	   elsif Object. MP2Header_OK	= 0	-- no sync yet
	   then
	      if v (i) = 01		-- all bits should be a "1"
	      then
	         Object. MP2HeaderCount	:= Object. MP2HeaderCount + 1;
	         if Object. MP2HeaderCount = 12	-- we have 12 '1' bits in a row
	         then
	            Object. MP2bitCount := 0;
	            for j in 0 .. 12 - 1 loop
	               addbittoMP2 (Object. MP2frame. all,
	                            1, Object. MP2bitCount);
	               Object. MP2bitCount	:= Object. MP2bitCount + 1;
	            end loop;
	            Object. MP2Header_OK	:= 1;	-- next state
	         end if;
	      else
	          Object. MP2HeaderCount	:= 0;
	      end if;
	   elsif Object. MP2Header_OK = 1
	   then	
	      addbittoMP2 (Object. MP2frame. all, v (i), Object. MP2bitCount);
	      Object. MP2bitCount	:= Object. MP2bitCount + 1;
	      if Object. MP2bitCount = 24	-- relevant part header
	      then
	         Object. baudRate := 
	                 kjmp2_get_sample_rate (Object. MP2frame. all' Address);

	         if Object. baudRate /= 48000
	                        and then Object. baudRate /= 24000
	         then
	            Object. MP2Header_OK	:= 0;
	            Object. MP2HeaderCount	:= 0;
	            Object. MP2bitCount		:= 0;
	            return;		-- failure
	         end if;
	         Object. MP2Header_OK		:= 2;
	      end if;
	   end if;
	end loop;
end addtoFrame;
end mp2_handler;

