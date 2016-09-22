--
--	Reed-Solomon decoder
--	Copyright 2002 Phil Karn, KA9Q
--	May be used under the terms of the GNU General Public License (GPL)
--
--	Rewritten - and slightly adapted while doing so -
--	as a C++ class for use in the sdr-j dab decoder(s)
--	Copyright 2015 Jan van Katwijk
--	May be used under the terms of the GNU General Public License (GPL)
--
--	and now rewriten as an Ada package for use in the Ada version
--	of the DAB decoder

--	Initialize a Reed-Solomon codec
--	a. symsize = symbol size, bits (1-8)
--	b. gfpoly = Field generator polynomial coefficients
--	c. fcr = first root of RS code generator polynomial, index form, 0
--	d. prim = primitive element to generate polynomial roots
--	e. nroots = RS code generator polynomial degree (number of roots)
with header; use header;
with galois;
generic
	symsize:  short_Integer;
	gfPoly:   short_Integer;
	fcr:      short_Integer;
	prim:     short_Integer;
	nroots:   short_Integer;
package reed_solomon is

	procedure encode_rs (data:    byteArray;
	                     cutLen:  short_Integer;
	                     result:  out byteArray);
	procedure decode_rs (data:    byteArray;
	                     cutLen:  short_Integer;
	                     result:  out byteArray;
	                     corrs:   out short_Integer);
private
	type rsArray is Array (short_Integer Range <>) of short_Integer;
	subtype nrootsArray is rsArray (0 .. nroots);
	procedure enc   (data:        rsArray;
	                 parityBytes: out rsArray);
	procedure dec	(data:        in out rsArray;
	                 corrs:       out short_Integer);

	package myGalois is new galois (symsize, gfPoly);
	use myGalois;
	Generator:       nrootsArray := (Others	=> 0);
	codeLength:      short_Integer;
	iprim:           short_Integer;
	index:           short_Integer;
	root:            short_Integer;
end reed_solomon;

