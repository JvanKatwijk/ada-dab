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
	symsize:  int16_t;
	gfPoly:   int16_t;
	fcr:      int16_t;
	prim:     int16_t;
	nroots:   int16_t;
package reed_solomon is

	procedure encode_rs (data   : byteArray;
	                     cutLen : int16_t;
	                     result : out byteArray);
	procedure decode_rs (data   : byteArray;
	                     cutLen : int16_t;
	                     result : out byteArray;
	                     corrs  : out int16_t);
private
	type rsArray is Array (int16_t Range <>) of short_Integer;
	subtype nrootsArray is rsArray (0 .. nroots);
	procedure enc       (data   : rsArray;
	                     parityBytes: out rsArray);
	procedure dec	    (data   : in out rsArray;
	                     corrs  : out int16_t);

	package myGalois is new galois (symsize, gfPoly);
	use myGalois;
	Generator:       nrootsArray := (Others	=> 0);
	codeLength:      int16_t;
	iprim:           int16_t;
	index:           int16_t;
	root:            int16_t;
end reed_solomon;

