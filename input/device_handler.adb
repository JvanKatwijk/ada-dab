
package body device_handler is
	procedure Restart_Reader   (Object       : in out device;
	                            Success      : out Boolean) is
	begin
	   null;
	end Restart_Reader;
        procedure Stop_Reader      (Object       : in out device) is
	begin
	   null;
	end Stop_Reader;
        procedure Set_VFOFrequency (Object       : in out device;
	                            New_Frequency: Natural) is
	begin
	   null;
	end Set_VFOFrequency;
        procedure Set_Gain         (Object       : in out device;
	                            New_Gain     : Natural) is
	begin
	   null;
	end Set_Gain;
        procedure Get_Samples      (Object       : in out device;
	                            Out_V        : out complexArray;
                                    Amount       : out Natural) is
	begin
	   Amount := 0;
	end Get_Samples;
        
	function  Available_Samples (Object      : device) return Natural is
	begin
	   return 0;
	end Available_Samples;

        function Valid_Device       (Object       : device) return Boolean is
	begin
	   return false;
	end Valid_Device;
end device_handler;


