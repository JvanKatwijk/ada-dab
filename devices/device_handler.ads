with header; use header;
with Ada. Finalization; use Ada. Finalization;
package device_handler is
        type device is
	    new Ada. Finalization. Controlled with record null; end record;
        type device_P is access all device' Class;
	procedure Restart_Reader   (Object       : in out device;
	                            Success      : out Boolean);
        procedure Stop_Reader      (Object       : in out device);
        procedure Set_VFOFrequency (Object       : in out device;
	                            New_Frequency: Natural);
        procedure Set_Gain         (Object       : in out device;
	                            New_Gain     : Natural);
        procedure Get_Samples      (Object       : in out device;
	                            Out_V        : out complexArray;
                                    Amount       : out Natural);
        function Available_Samples (Object       : device) return Natural;
        function Valid_Device      (Object       : device) return Boolean;
end device_handler;


