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

with header; use header;
package Phasetable is
	function Get_Phi (k : Integer; mode: Dabmode) return Float;
private
	type Phasetable_Element is
	   record
	      Kmin:    Integer;
	      Kmax:    Integer;
	      I:       Integer;
	      N:       Integer;
	   end record;

	type Modetable	is array (Integer range <>) of Phasetable_Element;

	ModeI_table: constant Modetable := (
        (-768, -737, 0, 1),
        (-736, -705, 1, 2),
        (-704, -673, 2, 0),
        (-672, -641, 3, 1),
        (-640, -609, 0, 3),
        (-608, -577, 1, 2),
        (-576, -545, 2, 2),
        (-544, -513, 3, 3),
        (-512, -481, 0, 2),
        (-480, -449, 1, 1),
        (-448, -417, 2, 2),
        (-416, -385, 3, 3),
        (-384, -353, 0, 1),
        (-352, -321, 1, 2),
        (-320, -289, 2, 3),
        (-288, -257, 3, 3),
        (-256, -225, 0, 2),
        (-224, -193, 1, 2),
        (-192, -161, 2, 2),
        (-160, -129, 3, 1),
        (-128,  -97, 0, 1),
	(-96,   -65, 1, 3),
        (-64,   -33, 2, 1),
        (-32,    -1, 3, 2),
        (  1,    32, 0, 3),
        ( 33,    64, 3, 1),
        ( 65,    96, 2, 1),
--      ( 97,   128, 2, 1),  found bug 2014-09-03 Jorgen Scott
        ( 97,   128, 1, 1),
        ( 129,  160, 0, 2),
        ( 161,  192, 3, 2),
        ( 193,  224, 2, 1),
        ( 225,  256, 1, 0),
        ( 257,  288, 0, 2),
        ( 289,  320, 3, 2),
        ( 321,  352, 2, 3),
        ( 353,  384, 1, 3),
        ( 385,  416, 0, 0),
        ( 417,  448, 3, 2),
        ( 449,  480, 2, 1),
        ( 481,  512, 1, 3),
        ( 513,  544, 0, 3),
        ( 545,  576, 3, 3),
        ( 577,  608, 2, 3),
	( 609,  640, 1, 0),
        ( 641,  672, 0, 3),
        ( 673,  704, 3, 0),
        ( 705,  736, 2, 1),
        ( 737,  768, 1, 1),
        ( -1000, -1000, 0, 0)
);

	ModeII_table: constant Modetable := (
	(-192,	-161,	0,	2),
	(-160,	-129,	1,	3),
	(-128,	-97,	2,	2),
	(-96,	-65,	3,	2),
	(-64,	-33,	0,	1),
	(-32,	-1,	1,	2),
	(1,	32,	2,	0),
	(33,	64,	1,	2),
	(65,	96,	0,	2),
	(97,	128,	3,	1),
	(129,	160,	2,	0),
	(161,	192,	1,	3),
	(-1000,	-1000,	0,	0)
);

	ModeIV_table: constant Modetable := (
	(-384, -353, 0, 0),
	(-352, -321, 1, 1),
	(-320, -289, 2, 1),
	(-288, -257, 3, 2),
	(-256, -225, 0, 2),
	(-224, -193, 1, 2),
	(-192, -161, 2, 0),
	(-160, -129, 3, 3),
	(-128,  -97, 0, 3),
	(-96,   -65, 1, 1),
	(-64,   -33, 2, 3),
	(-32,    -1, 3, 2),
	(  1,    32, 0, 0),
	( 33,    64, 3, 1),
	( 65,    96, 2, 0),
	( 97,   128, 1, 2),
	( 129,  160, 0, 0),
	( 161,  192, 3, 1),
	( 193,  224, 2, 2),
	( 225,  256, 1, 2),
	( 257,  288, 0, 2),
	( 289,  320, 3, 1),
	( 321,  352, 2, 3),
	( 353,  384, 1, 0),
	(-1000, -1000, 0, 0)
);

	H0:	constant intArray :=
                       (0, 2, 0, 0, 0, 0, 1, 1, 2, 0, 0, 0, 2, 2, 1, 1,
                        0, 2, 0, 0, 0, 0, 1, 1, 2, 0, 0, 0, 2, 2, 1, 1);

	H1:	constant intArray :=
	               (0, 3, 2, 3, 0, 1, 3, 0, 2, 1, 2, 3, 2, 3, 3, 0,
                        0, 3, 2, 3, 0, 1, 3, 0, 2, 1, 2, 3, 2, 3, 3, 0);

	H2:	constant intArray :=
	               (0, 0, 0, 2, 0, 2, 1, 3, 2, 2, 0, 2, 2, 0, 1, 3,
                        0, 0, 0, 2, 0, 2, 1, 3, 2, 2, 0, 2, 2, 0, 1, 3);

	H3:	constant intArray :=
	               (0, 1, 2, 1, 0, 3, 3, 2, 2, 3, 2, 1, 2, 1, 3, 2,
                        0, 1, 2, 1, 0, 3, 3, 2, 2, 3, 2, 1, 2, 1, 3, 2);

end phaseTable;
