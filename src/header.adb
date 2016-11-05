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
package body Header is
	function	L	(Mode: Dabmode) return integer is
	begin
	   case Mode is
	      when Mode_1	=> return 76;
	      when Mode_2	=> return 76;
	      when Mode_3	=> return 153;
	      when others	=> return 76;
	   end case;
	end L;

	function	K	(Mode: Dabmode) return integer is
	begin
	   case Mode is
	      when Mode_1	=> return 1536;
	      when Mode_2	=> return 384;
	      when Mode_3	=> return 192;
	      when others	=> return 768;
	   end case;
	end K;

	function	T_F	(Mode: Dabmode) return integer is
	begin
	   case Mode is
	      when Mode_1	=> return 196608;
	      when Mode_2	=> return 49152;
	      when Mode_3	=> return 49152;
	      when others	=> return 98304;
	   end case;
	end T_F;

	function	T_null	(Mode: Dabmode) return integer is
	begin
	   case Mode is
	      when Mode_1	=> return 2656;
	      when Mode_2	=> return 49152;
	      when Mode_3	=> return 345;
	      when others	=> return 1328;
	   end case;
	end T_null;

	function	T_s	(Mode: Dabmode) return integer is
	begin
	   case Mode is
	      when Mode_1	=> return 2552;
	      when Mode_2	=> return 638;
	      when Mode_3	=> return 319;
	      when others	=> return 1276;
	   end case;
	end T_s;

	function	T_u	(Mode: Dabmode) return integer is
	begin
	   case Mode is
	      when Mode_1	=> return 2048;
	      when Mode_2	=> return 512;
	      when Mode_3	=> return 256;
	      when others	=> return 1024;
	   end case;
	end T_u;

	function	T_g	(Mode: Dabmode) return integer is
	begin
	   case Mode is
	      when Mode_1	=> return 504;
	      when Mode_2	=> return 126;
	      when Mode_3	=> return 63;
	      when others	=> return 252;
	   end case;
	end T_g;

	function	Carrier_Diff (Mode: Dabmode) return integer is
	begin
	   case Mode is
	      when Mode_1	=> return 1000;
	      when Mode_2	=> return 4000;
	      when Mode_3	=> return 2000;
	      when others	=> return 2000;
	   end case;
	end Carrier_Diff;

	function cifs (the_Mode : dabMode) return Integer is
	   Blocks_per_CIF : Integer;
	begin
	   case the_Mode is
	      when Mode_1	=> Blocks_per_CIF := 18;
	      when Mode_2	=> Blocks_per_CIF := 72;
	      when Mode_4	=> Blocks_per_CIF := 36;
	      when others	=> Blocks_per_CIF := 18; -- should not happen
	   end case;
	   return Blocks_per_CIF;
	end;

	function	kHz	(Frequency: Integer) return Integer is
	begin
	   return 1000 * Frequency;
	end kHz;

	function	mHz	(Frequency: Integer) return Integer is
	begin
	   return 1000 * kHz (Frequency);
	end mHz;

	protected body locker is
	   entry lock when Count > 0 is
	   begin
	      Count := Count - 1;
	   end lock;

	   procedure unlock is
	   begin
	      Count := Count + 1;
	   end unlock;
	end locker;
end Header;


