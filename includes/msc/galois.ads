with header; use header;
with Interfaces; use Interfaces;
generic
	Symsize: short_Integer;
	gfpoly:  short_Integer;
package Galois is

	function  addPoly  (A: short_Integer; B: short_Integer)
	                                           return short_Integer;
	function  addPower (A: short_Integer; B: short_Integer)
	                                           return short_Integer;
	function  multiplyPower	(A: short_Integer; b: short_Integer)
	                                           return short_Integer;
	function  multiplyPoly	(A: short_Integer; b: short_Integer)
	                                           return short_Integer;
	function  dividePower (A: short_Integer; B: short_Integer)
	                                           return short_Integer;
	function  dividePoly (A: short_Integer; B: short_Integer)
	                                           return short_Integer;
	function  powPoly (A: short_Integer; N: short_Integer)
	                                           return short_Integer;
	function  powPower (A: short_Integer; N: short_Integer)
	                                           return short_Integer;
	function  inversePoly (A: short_Integer) return short_Integer;
	function  inversePower (A: short_Integer) return short_Integer;
	function  poly2Power (A: short_Integer) return short_Integer;
	function  power2Poly (A: short_Integer) return short_Integer;
private
	type galoisArray is array (short_Integer range <>) of short_Integer;
	CodeLength:   short_Integer :=
	                       short_Integer (Shift_Left (uint16_t (1),
	                                      Integer (symsize))) - 1;
	d_q:          short_Integer :=
	                       short_Integer (Shift_Left (uint16_t (1),
	                                      Integer (symsize)));
	Alpha_to:     galoisArray (0 .. codeLength);
	Index_of:     galoisArray (0 .. codeLength);
end Galois;

