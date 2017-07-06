	separate (ofdm_handler)
	function Compute_Offset (Block_0_Buffer: Tu_Sized_Buffer)
	                                                  return Integer is
	   Search_Range   : constant Integer	:= 36;
	   Res_Vector     : Tu_Sized_Buffer renames Block_0_Buffer;
	   M_min          : Float            := 1000.0;
	   Index          : Integer          := Tu;
	begin
--	we look at a special pattern consisting
--	of zeros in the row of args between successive carriers.
	   for I in Tu - Search_Range / 2 .. Tu + Search_Range / 2 loop
	      declare
                    A1: Float := 
	                 abs (abs  (arg (Res_Vector ((I + 1) mod Tu) *
                            conj (Res_Vector ((I + 2) Mod Tu))) / M_PI) - 1.0);
	         A2: Float :=
	                 abs arg (Res_Vector ((I + 1) mod Tu) *
	         	            conj (Res_Vector ((I + 3) Mod Tu)));
	         A3: Float :=
	                 abs arg (Res_Vector ((I + 3) mod Tu) *
	         	            conj (Res_Vector ((I + 4) mod Tu)));
	         A4: Float :=
	                 abs arg (Res_Vector ((I + 4) mod Tu) *
	         	            conj (Res_Vector ((I + 5) mod Tu)));
	         A5: Float :=
	                 abs arg (Res_Vector ((I + 5) mod Tu) *
	         	            conj (Res_Vector ((I + 6) mod Tu)));
	         B1: Float :=
	                 abs (abs (arg (Res_Vector ((I + 16 + 1) mod Tu) *
	         	            conj (Res_Vector ((I + 16 + 3) mod Tu))) / M_PI) - 1.0);
	         B2: Float :=
	                 abs (arg (Res_Vector ((I + 16 + 3) mod Tu) *
	         	             conj (Res_Vector ((I + 16 + 4) mod Tu))));
	         B3: Float :=
	                 abs (arg (Res_Vector ((I + 16 + 4) mod Tu) *
	         	             conj (Res_Vector ((I + 16 + 5) mod Tu))));
	         B4: Float :=
	                 abs (arg (Res_Vector ((I + 16 + 5) mod Tu) *
	         	             conj (Res_Vector ((I + 16 + 6) mod Tu))));
	         Sum: Float := A1 + A2 + A3 + A4 + A5 + B1 + B2 + B3 + B4;
	      begin
	         if Sum < M_min then
	            M_min	:= Sum;
	            Index	:= I;
	         end if;
	      end;
	   end loop;
	   return Index - Tu;
	end Compute_Offset;

