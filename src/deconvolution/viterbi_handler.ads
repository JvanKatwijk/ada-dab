--
--	straightforward wrapper around the generic viterbi decoder from
--	by the spiral project
with header;	use header;
with System;	use System;
with Ada. Finalization;
with Ada. Unchecked_Deallocation;
with Interfaces. C;	use Interfaces. C;
package Viterbi_Handler is
	type Viterbi_Processor (WordLength: Integer) is
	          new Ada. Finalization. Controlled with private;
	type Viterbi_Processor_P is access all Viterbi_Processor;

	procedure Deconvolve (Object:	in out Viterbi_Processor;
	                      input:	shortArray;
	                      output:	out byteArray);

private
	type uintArray is Array (Integer Range <>) of uint32_t;
	type uintArray_P is access all uintArray;
	procedure Free_uintArray is
	               new Ada. Unchecked_Deallocation (
	                                    Object => uintArray,
	                                    Name => uintArray_P);
	type Viterbi_Processor (WordLength: Integer) is
	          new Ada. Finalization. Controlled with 
	   record
	      Handler:   System. Address;
	      isOK:      Boolean;
	      Symbols:   access uintArray;
	   end record;

	K:	constant Integer	:= 7;
	Rate:	constant Integer	:= 4;
	POLYS:	constant byteArray (0 .. 3) :=
	                      (8#0155#,  8#0117#,  8#0123#, 8#0155#);
--	#define	POLYS	{109, 79, 83, 109}
--	In the reversed form the polys look:
--	#define POLYS { 0133, 0171, 0145, 0133 }
--	#define POLYS { 91, 121, 101, 91 }

	function Create_Viterbi (Wordlength: Interfaces. C. int)
	                                          return system. address;
	pragma Import (C, Create_Viterbi, "create_viterbi");

	procedure Init_Viterbi (Handle:     system. address;
	                        startState: Interfaces. C. int);
	pragma Import (C, Init_Viterbi, "init_viterbi");

	procedure Update_Viterbi_Blk (Handle:   system. Address;
	                              Symbols:  system. Address;
	                              nbits:    Interfaces. C. int);
	pragma Import (C, Update_Viterbi_Blk, "update_viterbi_blk_GENERIC");

	procedure Chainback_Viterbi (Handle:      system. Address;
	                             Output:      system. Address;
	                             Wordlength:  Interfaces. C. int;
                                     EndState:    Interfaces. C. int);
	pragma	Import (C, Chainback_Viterbi, "chainback_viterbi");

	procedure Delete_Viterbi   (Handle: system. Address);
	pragma Import	(C, Delete_Viterbi, "delete_viterbi");

	procedure Initialize	(Object: in out Viterbi_Processor);
	procedure Finalize	(Object: in out Viterbi_Processor);
end Viterbi_Handler;

