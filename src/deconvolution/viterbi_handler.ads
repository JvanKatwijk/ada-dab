
--
--	straightforward wrapper around a viterbi decoder generatoed
--	by the spiral project
with header; use header;
with Ada. Finalization; use Ada. Finalization;
with Ada. Unchecked_Deallocation;
with System; use System;
package viterbi_handler is
   type viterbiProcessor (wordLength: Integer) is
	          new Ada. Finalization. Controlled with private;
   type viterbiProcessor_P is access all viterbiProcessor;

procedure	deconvolve (Object	: in out viterbiProcessor;
	                    input	: shortArray;
	                    output	: out byteArray);

private
   type uintArray is Array (Integer Range <>) of uint32_t;
   type uintArray_P is access all uintArray;
   procedure Free_uintArray is new Ada. Unchecked_Deallocation (
	Object => uintArray, Name => uintArray_P);
   type viterbiProcessor (wordLength: Integer) is
	          new Ada. Finalization. Controlled with 
	record
	   handler	: System. Address;
	   isOK		: Boolean;
	   symbols	: access uintArray;
	end record;

K	: constant Integer	:= 7;
rate	: constant Integer	:= 4;
POLYS	: constant byteArray (0 .. 3) :=
	      (8#0155#,  8#0117#,  8#0123#, 8#0155#);
--#define	POLYS	{109, 79, 83, 109}
-- In the reversed form the polys look:
--#define POLYS { 0133, 0171, 0145, 0133 }
--#define POLYS { 91, 121, 101, 91 }

function	create_viterbi (wordlength: Integer) return system. address;
pragma Import (C, create_viterbi, "create_viterbi");

procedure	init_viterbi (handle: system. address; startState: Integer);
pragma Import (C, init_viterbi, "init_viterbi");

procedure	update_viterbi_blk (handle	: system. Address;
	                            symbols	: system. Address;
	                            nbits	: Integer);
pragma Import (C, update_viterbi_blk, "update_viterbi_blk_GENERIC");
--pragma Import (C, update_viterbi_blk, "update_viterbi_blk_SPIRAL");

procedure	chainback_viterbi (handle	: system. Address;
	                           output	: system. Address;
	                           wordlength	: Integer;
                                   endState	: Integer);
pragma	Import (C, chainback_viterbi, "chainback_viterbi");

procedure	delete_viterbi	(handle:	system. Address);
pragma Import	(C, delete_viterbi, "delete_viterbi");

procedure Initialize	(Object: in out viterbiProcessor);
procedure Finalize	(Object: in out viterbiProcessor);
end viterbi_handler;

