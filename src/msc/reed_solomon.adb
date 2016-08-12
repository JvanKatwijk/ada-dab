--
--    Copyright (C) 2016
--    Jan van Katwijk (J.vanKatwijk@gmail.com)
--    Lazy Chair Programming
--
--    This file is part of the SDR-J (JSDR).
--    SDR-J is free software; you can redistribute it and/or modify
--    it under the terms of the GNU General Public License as published by
--    the Free Software Foundation; either version 2 of the License, or
--    (at your option) any later version.
--
--    SDR-J is distributed in the hope that it will be useful,
--    but WITHOUT ANY WARRANTY; without even the implied warranty of
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--    GNU General Public License for more details.
--
--    You should have received a copy of the GNU General Public License
--    along with SDR-J; if not, write to the Free Software
--    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
--
with reed_solomon;
with Interfaces; use Interfaces;
with Text_IO; use Text_IO;
with Ada. Exceptions; use Ada. Exceptions;
package body reed_solomon is
--
--	
procedure	encode_rs (data		: byteArray;
	                   cutLen	: short_Integer;
	                   result	: out byteArray) is
rf	: rsArray (0 .. codeLength - 1) := (Others => 0);
bb	: rsArray (0 .. nroots - 1);
messageLength	: short_Integer	:= codeLength - cutLen - nroots;
begin
	for i in cutLen .. codeLength - 1 loop
	   rf (i) :=  short_Integer (data (data' first + Integer (i - cutLen)));
	end loop;

	enc (rf, bb);
--	copy back the data
	for i in result' first .. result' first +  Integer (messageLength) - 1  loop
	   result (i) :=  Byte (data (data' first + i - result' First));
	end loop;

--	and add the parity bytes
	for i in 0 .. nroots - 1 loop
	   result (result' first +
	                Integer (codeLength - cutLen - nroots + i)) :=
	                                      Byte (bb (i));
	end loop;
end encode_rs;

procedure	decode_rs (data		: byteArray;
	                   cutLen	: short_Integer;
	                   result	: out byteArray;
	                   corrs	: out short_Integer) is
rf	: rsArray (0 .. codeLength - 1);
begin
	rf (0 .. cutLen) := (Others	=> 0);
	for i in cutLen .. codeLength - 1 loop
	   rf (i)	:= short_Integer (data (data' first + Integer (i - cutLen)));
	end loop;

	dec (rf, corrs);

	for i in cutLen .. codeLength - nroots - 1 loop
	   result (result' first +  Integer (i - cutLen)) := Byte (rf (i));
	end loop;
end decode_rs;

--
--	Basic encoder, returns - in outparameter "parityBytes"
procedure	enc (data	: rsArray;
	             parityBytes: out rsArray) is
feedback	: short_Integer;
begin
	parityBytes	:= (Others => 0);

	for i in 0 .. codeLength - nroots - 1 loop
	   feedback	:= poly2Power (addPoly (data (i), parityBytes (0)));
	   if feedback /= codeLength	-- feedback term is non-zero
	   then
	      for j in 1 .. nroots - 1 loop
	         parityBytes (j) := addPoly (parityBytes (j),
	                              power2Poly (multiplyPower (feedback,
	                                          generator (nroots - j))));
	      end loop;
	   end if;
-- shift
	   parityBytes (0 .. nroots - 2) := parityBytes (1 .. nroots - 1);
	   if feedback /= codeLength
	   then
	      parityBytes (nroots - 1) :=
	                    power2Poly (
	                          multiplyPower (feedback, generator (0)));
	   else
	      parityBytes (nroots - 1) := 0;
	   end if;
	end loop;
end enc;
--
--	Apply Horner on the input for root "root"
function	getSyndrome (data : rsArray;
	                     root : short_Integer) return short_Integer is
syn	: short_Integer	:= data (0);
uu1	: short_Integer;
begin
	for j in 1 .. codeLength - 1 loop
	   if syn = 0
	   then
	      syn	:= data (j);
	   else 
	      uu1	:= powPower (multiplyPower (fcr, root), prim);
	      syn 	:= addPoly (data (j),
	                             power2Poly (
	                                  multiplyPower (
	                                         poly2Power (syn), uu1)));
	   end if;
	end loop;
	return syn;
end	getSyndrome;

procedure computeSyndromes (data	: rsArray;
	                    syndromes 	: out rsArray;
	                    no_errors	: out Boolean)  is
synError	: uint16_t	:= 0;
begin
-- form the syndromes; i.e., evaluate data (x) at roots of g(x) */
	for i in 0 .. nroots - 1 loop
	   syndromes (i)	:= getSyndrome (data, i);
	   synError		:= synError or uint16_t (syndromes (i));
	end loop;
	no_errors		:=  synError = 0;
end computeSyndromes;

--	compute Lambda with Berlekamp-Massey
--	syndromes in poly-form in, Lambda in power form out
procedure	computeLambda (syndromes	: rsArray;
	                       Lambda		: out rsArray;
	                       deg_lambda	: out short_Integer) is
Corrector	: rsArray (syndromes' Range) := (others => 0);
K		: short_Integer	:= 1;
L		: short_Integer	:= 0;
error		: short_Integer	:= syndromes (0);
oldLambda 	: rsArray (Lambda' Range);
begin
	Lambda	:= (others	=> 0);

--	Initializers: 
	Lambda	(0)	:= 1;
	Corrector (1)	:= 1;
--
	while K <= nroots loop
	   oldLambda	:=  Lambda;
--
--	Compute new lambda
	   for i in Lambda' Range loop
	      Lambda (i) := addPoly (Lambda (i),
	                             multiplyPoly (error, Corrector (i)));
	   end loop;
	   if 2 * L <  K and then error /= 0
	   then
	      L	:= K - L;
	      for i in Corrector' Range loop 
	         Corrector (i) := dividePoly (oldLambda (i), error);
	      end loop;
	   end if;
--
--	multiply x * C (x), i.e. shift to the right, the 0-th order term is left
	   Corrector (1 .. nroots) :=
	                      Corrector (0 .. nroots - 1);
	   Corrector (0) 	:= 0;

--	and compute a new error
	   error	:= syndromes (K);	
	   for i in 1 .. K loop
	      error := addPoly (error,
	                        multiplyPoly (syndromes (K - i),
	                                         Lambda (i)));
	   end loop;
	   K 	:=  K + 1;
	end loop;	-- end of berlekamp loop

	for i in 0 .. nroots loop
	   if Lambda (i) /= 0
	   then
	      deg_lambda := i;
	   end if;
	   Lambda (i) := poly2Power (Lambda (i));
	end loop;
end computeLambda;

--	Compute the roots of lambda by evaluating the
--	lambda polynome for all (inverted) powers of the symbols
--	of the data (Chien search)
procedure	computeErrors (Lambda		: rsArray;
	                       deg_lambda	: short_Integer;
	                       rootTable	: out rsArray;
	                       locTable		: out rsArray;
	                       rootCount	: out short_Integer) is
workRegister	: rsArray := lambda;
k		: short_Integer;
result		: short_Integer;
begin
	rootCount	:= 0;
--
--	reg is lambda in power notation
	k	:= iprim - 1;
	for i in 1 .. codeLength loop
	   result	:= 1;	-- lambda [0] is always 1
--	Note that for i + 1, the powers in the workregister just need
--	to be increased by "j".
	   for j in reverse 1 .. deg_lambda loop
	      if workRegister (j) /= codeLength
	      then
	         workRegister (j) := multiplyPower (workRegister (j), j);
	         result		:= addPoly (result,
	                                    power2Poly (workRegister (j)));
	      end if;
	   end loop;

	   if result = 0		-- it is a root
	   then
	      rootTable (rootCount)	:= i;
	      locTable  (rootCount)	:= k;
	      rootCount 		:= rootCount + 1;
	   end if;
	   k		:= k + iprim;
	end loop;
	if rootCount /= deg_lambda
	then
	   rootCount := -1;
	end if;
end computeErrors;
--
--	Compute error evaluator poly
--	omega(x) = s(x)*lambda(x) (modulo x**NROOTS)
--	in power form, and  find degree (omega).
--
--	Note that syndromes are in poly form, while lambda in power form

procedure	computeOmega (syndromes		: rsArray;
	                      lambda		: rsArray;
	                      deg_lambda	: short_Integer;
	                      omega		: out rsArray;
	                      deg_omega		: out short_Integer)  is
tmp 	: short_Integer;
xxx	: short_Integer;
res	: short_Integer;
begin
	for i in 0 .. nroots - 1 loop
	   tmp	:= 0;
	   xxx 	:= (if deg_lambda < i  then deg_lambda  else  i);
	   for j in Reverse 0 .. xxx loop
	      if poly2Power (syndromes (i - j)) /= codeLength and then
	                                            lambda (j) /= codeLength
	      then
	         res	:= power2Poly (multiplyPower (
	                                     poly2Power (syndromes (i - j)), 
	                                     lambda (j)));
	         tmp	:= addPoly (tmp, res);
	      end if;
	   end loop;

	   if tmp /= 0
	   then
	      deg_omega 	:= i;
	   end if;
	   omega (i) 	:= poly2Power (tmp);
	end loop;

	omega (nroots) 	:= codeLength;
end computeOmega;

procedure	dec	(data	: in out rsArray;
	                 corrs	: out short_Integer) is
syndromes	: rsArray (0 .. nroots);
Lambda		: rsArray (0 .. nroots);
rootTable	: rsArray (0 .. nroots - 1);
locTable	: rsArray (0 .. nroots - 1);
omega		: rsArray (0 .. nroots);
lambda_degree	: short_Integer;
omega_degree	: short_Integer;
rootCount	: short_Integer;
nr_errors	: Boolean;
begin
--	step 1: syndromes in poly notation
	computeSyndromes (data, syndromes, nr_errors);
	if nr_errors
	then
	   corrs	:= 0;
	   return;
	end if;
--	step 2: Berlekamp-Massey, lambda in power notation
	computeLambda (syndromes, Lambda, lambda_degree);
--	step 3 evaluate lambda and compute error locations
	computeErrors (Lambda, lambda_degree, rootTable, loctable, rootCount);
	if rootCount < 0
	then
	   corrs	:= -1;
	   return;
	end if;
	computeOmega (syndromes, Lambda, lambda_degree, omega, omega_degree);
--
--      Compute error values in poly-form.
--      num1 = omega (inv (X (l))),
--      num2 = inv (X (l))**(FCR-1) and
--      den = lambda_pr(inv(X(l))) all in poly-form
--
	declare
	   num1	: short_Integer;
	   num2	: short_Integer;
	   den	: short_Integer;
	   tmp	: short_Integer;
	   tmp1	: short_Integer;
	   tmp2	: short_Integer;
	   corrTerm	: short_Integer;
	begin
	   for j in Reverse 0 .. rootCount - 1 loop
	      num1 	:= 0;
	      for i in Reverse  0 .. Omega_degree loop
	         if omega (i) /= codeLength
	         then
	            tmp	:=  multiplyPower (omega (i),
	                                   powPower (i, rootTable (j)));
	            num1 := addPoly (num1, power2Poly (tmp));
	         end if;
	      end loop;
	      tmp 	:= multiplyPower (
	                              powPower (
	                                     rootTable (j),
	                                     dividePower (fcr, 1)),
	                              codeLength);
	      num2	:= power2Poly (tmp);
	      den 	:= 0;
--
--	lambda [i + 1] for i even is the formal derivative
--	lambda_pr of lambda [i]

	      for i in Reverse 0 .. 
	               short_Integer' min (lambda_degree, nroots - 1) / 2 loop
	         if Lambda (2 * i + 1) /= codeLength
	         then
	            tmp := multiplyPower (Lambda (2 * i + 1),
	                                  powPower (2 * i, rootTable (j)));
	            den	:= addPoly (den, power2Poly (tmp));
	         end if;
	      end loop;

	      if den = 0
	      then
	         corrs	:= -1;
	         return;
	      end if;
--
--	apply error correction to data
	      if num1 /= 0
	      then
	         if locTable (j) >= codeLength - nroots
	         then
	            rootCount := rootCount - 1;
	         else
	            tmp1	:= codeLength - poly2Power (den);
	            tmp2	:= multiplyPower (
	                                        poly2Power (num1),
	                                        poly2Power (num2));
	            tmp2	:= multiplyPower (tmp2, tmp1);
	            corrTerm	:= power2Poly (tmp2);
	            data (locTable (j)) :=
	                      addPoly (data (locTable (j)), corrTerm);
	         end if;
	      end if;
	   end loop;
	exception
	when Error: others		=> Put ("Exception in reed solomon: ");
	                                Put_Line (Exception_Name (Error));
	                                raise;
	end;
	corrs	:= rootCount;
end dec;
p1	: short_Integer;
begin
	codeLength	:= short (Shift_left (uint16_t (1),
	                          Integer (symSize))) - 1;
	iprim		:= 1;
	while (iprim mod prim) /= 0 loop
	   iprim	:= iprim + codeLength;
	end loop;
	iprim		:= iprim / prim;
	generator (0)	:= 1;
	index		:= 0;
	root		:= fcr * prim;

	while index < nroots loop
	   generator (index + 1) := 1;
	   for j in reverse 1 ..  index loop
	      if generator (j) /= 0
	      then
	         p1	:= multiplyPower (
	                               poly2Power (generator (j)),
	                               root);
	         generator (j)	:= addPoly (
	                             generator (j - 1), power2Poly (p1));
	      else
	         generator (j)	:= generator (j - 1);
	      end if;
	   end loop;
--	rsHandle . genPoly (0) can never be zero
	   generator (0) :=
	         power2Poly (
	                  multiplyPower (root,
	                          poly2Power (generator (0))));
--	end of loop, increment
	   index	:= index + 1;
	   root		:= root  + 1;
	end loop;
--
--	and finally:
	for i in 0 ..  nroots loop
	   generator (i) := poly2Power (generator (i));
	end loop;
end reed_solomon;

