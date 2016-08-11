
package body PortAudioAda is

   type Rec_Error is
      record
         errorCode : PaError;
         value     : Integer;
      end record;

   errorMapping : constant array (Natural range <>) of Rec_Error :=
                    (
                     (paNotInitialized,                        -10_000),
                     (paUnanticipatedHostError,                 -9_999),
                     (paInvalidChannelCount,                    -9_998),
                     (paInvalidSampleRate,                      -9_997),
                     (paInvalidDevice,                          -9_996),
                     (paInvalidFlag,                            -9_995),
                     (paSampleFormatNotSupported,               -9_994),
                     (paBadIODeviceCombination,                 -9_993),
                     (paInsufficientMemory,                     -9_992),
                     (paBufferTooBig,                           -9_991),
                     (paBufferTooSmall,                         -9_990),
                     (paNullCallback,                           -9_989),
                     (paBadStreamPtr,                           -9_988),
                     (paTimedOut,                               -9_987),
                     (paInternalError,                          -9_986),
                     (paDeviceUnavailable,                      -9_985),
                     (paIncompatibleHostApiSpecificStreamInfo,  -9_984),
                     (paStreamIsStopped,                        -9_983),
                     (paStreamIsNotStopped,                     -9_982),
                     (paInputOverflowed,                        -9_981),
                     (paOutputUnderflowed,                      -9_980),
                     (paHostApiNotFound,                        -9_979),
                     (paInvalidHostApi,                         -9_978),
                     (paCanNotReadFromACallbackStream,          -9_977),
                     (paCanNotWriteToACallbackStream,           -9_976),
                     (paCanNotReadFromAnOutputOnlyStream,       -9_975),
                     (paCanNotWriteToAnInputOnlyStream,         -9_974),
                     (paIncompatibleStreamHostApi,              -9_973),
                     (paBadBufferPtr,                           -9_972),
                     (paNoError,                                     0)
                    );

   ---------------------
   -- Pa_ErrorMapping --
   ---------------------

   function Pa_ErrorMapping (value : Integer)
                             return PaError
   is
   begin
      for i in errorMapping'Range loop
         if errorMapping (i).value = value then
            return errorMapping (i).errorCode;
         end if;
      end loop;

      return paNoError;
   end Pa_ErrorMapping;

   ---------------------
   -- Pa_ErrorMapping --
   ---------------------

   function Pa_ErrorMapping (errorCode : PaError)
                             return Integer
   is
   begin
      for i in errorMapping'Range loop
         if errorMapping (i).errorCode = errorCode then
            return errorMapping (i).value;
         end if;
      end loop;

      return 0;
   end Pa_ErrorMapping;

   -----------------------
   -- Pa_GetDeviceIndex --
   -----------------------

   function Pa_GetDeviceIndex (name : String) return PaDeviceIndex
   is
   begin
      for i in 0 .. Pa_GetDeviceCount - 1 loop
         if name = ICS.Value (Pa_GetDeviceInfo (i).all.name) then
            return i;
         end if;
      end loop;

      return -1;
   end Pa_GetDeviceIndex;

   ---------------------
   -- Pa_GetErrorText --
   ---------------------

   function Pa_GetErrorText (errorCode : PaError)
                             return String
   is
      function Internal (error : PaError) return ICS.chars_ptr;
      pragma Import (C, Internal, "Pa_GetErrorText");
   begin
      return ICS.Value (Internal (errorCode));
   end Pa_GetErrorText;

   ---------------------
   -- Pa_GetHostIndex --
   ---------------------

   function Pa_GetHostIndex (name : String) return PaHostApiIndex
   is
   begin
      for i in 0 .. Pa_GetHostApiCount - 1 loop
         if name = ICS.Value (Pa_GetHostApiInfo (i).all.name) then
            return i;
         end if;
      end loop;

      return -1;
   end Pa_GetHostIndex;

   -----------------------
   -- Pa_GetVersionText --
   -----------------------

   function Pa_GetVersionText return String is
      function Internal return ICS.chars_ptr;
      pragma Import (C, Internal, "Pa_GetVersionText");
   begin
      return ICS.Value (Internal);
   end Pa_GetVersionText;

end PortAudioAda;
