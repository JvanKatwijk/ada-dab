with header; use header;
with Interfaces; use Interfaces;
generic
symsize	: in short;
gfpoly	: in short;
package Galois is

   function	addPoly		(a:	short; b: short) return short;
   function	addPower	(a:	short; b: short) return short;
   function	multiplyPower	(a:	short; b: short) return short;
   function	multiplyPoly	(a:	short; b: short) return short;
   function	dividePower	(a:	short; b: short) return short;
   function	dividePoly	(a:	short; b: short) return short;
   function	powPoly		(a:	short; n: short) return short;
   function	powPower	(a:	short; n: short) return short;
   function	inversePoly	(a:	short) return short;
   function	inversePower	(a:	short) return short;
   function	poly2Power	(a:	short) return short;
   function	power2Poly	(a:	short) return short;
private
   type galoisArray is array (short_Integer range <>) of short_Integer;
   codeLength	: short		:= short (Shift_Left (uint16_t (1), Integer (symsize))) - 1;
   d_q		: short		:= short (Shift_Left (uint16_t (1), Integer (symsize)));
   alpha_to	: galoisArray (0 .. codeLength);
   index_of	: galoisArray (0 .. codeLength);
end Galois;

