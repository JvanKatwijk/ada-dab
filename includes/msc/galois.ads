with header; use header;
with Interfaces; use Interfaces;
generic
	Symsize: int16_t;
	gfpoly:  int16_t;
package Galois is

	function  addPoly       (A: int16_t; B: int16_t)
	                                           return int16_t;
	function  addPower      (A: int16_t; B: int16_t)
	                                           return int16_t;
	function  multiplyPower	(A: int16_t; b: int16_t)
	                                           return int16_t;
	function  multiplyPoly	(A: int16_t; b: int16_t)
	                                           return int16_t;
	function  dividePower   (A: int16_t; B: int16_t)
	                                           return int16_t;
	function  dividePoly    (A: int16_t; B: int16_t)
	                                           return int16_t;
	function  powPoly       (A: int16_t; N: int16_t)
	                                           return int16_t;
	function  powPower      (A: int16_t; N: int16_t)
	                                           return int16_t;
	function  inversePoly   (A: int16_t) return int16_t;
	function  inversePower  (A: int16_t) return int16_t;
	function  poly2Power    (A: int16_t) return int16_t;
	function  power2Poly    (A: int16_t) return int16_t;
private
	type galoisArray is array (int16_t range <>) of int16_t;
	CodeLength:   int16_t :=
	                       int16_t (Shift_Left (uint16_t (1),
	                                      Integer (symsize))) - 1;
	d_q:          int16_t :=
	                       int16_t (Shift_Left (uint16_t (1),
	                                      Integer (symsize)));
	Alpha_to:     galoisArray (0 .. codeLength);
	Index_of:     galoisArray (0 .. codeLength);
end Galois;

