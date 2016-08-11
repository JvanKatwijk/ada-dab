--------------------------------------------------------------------------------
--  Thin binding of PortAudio library into Ada 2005 language.
--  V19 devel

with Interfaces.C;
with Interfaces.C.Strings;

with System;

package PortAudioAda is

   package IC  renames Interfaces.C;
   package ICS renames Interfaces.C.Strings;

   -----------------------------------------------------------------------------

   function Pa_GetVersion return Integer;
   --  Retrieve the release number of the currently running PortAudio build,
   --  eg 1900.
   pragma Import (C, Pa_GetVersion, "Pa_GetVersion");

   function Pa_GetVersionText return String;
   --  Retrieve a textual description of the current PortAudio build,
   --  eg "PortAudio V19-devel 13 October 2002".
   pragma Inline (Pa_GetVersionText);

   -----------------------------------------------------------------------------
   --  Error codes returned by PortAudio functions
   type PaErrorCode is (
      paNotInitialized,
      paUnanticipatedHostError,
      paInvalidChannelCount,
      paInvalidSampleRate,
      paInvalidDevice,
      paInvalidFlag,
      paSampleFormatNotSupported,
      paBadIODeviceCombination,
      paInsufficientMemory,
      paBufferTooBig,
      paBufferTooSmall,
      paNullCallback,
      paBadStreamPtr,
      paTimedOut,
      paInternalError,
      paDeviceUnavailable,
      paIncompatibleHostApiSpecificStreamInfo,
      paStreamIsStopped,
      paStreamIsNotStopped,
      paInputOverflowed,
      paOutputUnderflowed,
      paHostApiNotFound,
      paInvalidHostApi,
      paCanNotReadFromACallbackStream,
      paCanNotWriteToACallbackStream,
      paCanNotReadFromAnOutputOnlyStream,
      paCanNotWriteToAnInputOnlyStream,
      paIncompatibleStreamHostApi,
      paBadBufferPtr,
      paNoError
     );

   for PaErrorCode use (
      paNotInitialized                        => -10_000,
      paUnanticipatedHostError                => -9_999,
      paInvalidChannelCount                   => -9_998,
      paInvalidSampleRate                     => -9_997,
      paInvalidDevice                         => -9_996,
      paInvalidFlag                           => -9_995,
      paSampleFormatNotSupported              => -9_994,
      paBadIODeviceCombination                => -9_993,
      paInsufficientMemory                    => -9_992,
      paBufferTooBig                          => -9_991,
      paBufferTooSmall                        => -9_990,
      paNullCallback                          => -9_989,
      paBadStreamPtr                          => -9_988,
      paTimedOut                              => -9_987,
      paInternalError                         => -9_986,
      paDeviceUnavailable                     => -9_985,
      paIncompatibleHostApiSpecificStreamInfo => -9_984,
      paStreamIsStopped                       => -9_983,
      paStreamIsNotStopped                    => -9_982,
      paInputOverflowed                       => -9_981,
      paOutputUnderflowed                     => -9_980,
      paHostApiNotFound                       => -9_979,
      paInvalidHostApi                        => -9_978,
      paCanNotReadFromACallbackStream         => -9_977,
      paCanNotWriteToACallbackStream          => -9_976,
      paCanNotReadFromAnOutputOnlyStream      => -9_975,
      paCanNotWriteToAnInputOnlyStream        => -9_974,
      paIncompatibleStreamHostApi             => -9_973,
      paBadBufferPtr                          => -9_972,
      paNoError                               => 0
     );

   for PaErrorCode'Size use 32;
   pragma Convention (C, PaErrorCode);

   PortAudio_Error : exception;

   subtype PaError is PaErrorCode;

   function Pa_GetErrorText (errorCode : PaError)
                             return String;
   --  Translate the supplied PortAudio error code into a human readable
   --  message.
   pragma Inline (Pa_GetErrorText);

   -----------------------------------------------------------------------------
   --  Pa_ErrorMapping
   --
   function Pa_ErrorMapping (errorCode : PaError)
                             return Integer;
   pragma Inline (Pa_ErrorMapping);

   function Pa_ErrorMapping (value : Integer)
                             return PaError;

   pragma Inline (Pa_ErrorMapping);

   function Pa_Initialize return PaError;
   --  Library initialization function - call this before using PortAudio.
   --  This function initializes internal data structures and prepares
   --  underlying host APIs for use.  With the exception of Pa_GetVersion,
   --  Pa_GetVersionText, and Pa_GetErrorText, this function MUST be called
   --  before using any other PortAudio API functions.
   --
   --  If Pa_Initialize is called multiple times, each successful
   --  call must be matched with a corresponding call to Pa_Terminate.
   --  Pairs of calls to Pa_Initialize/Pa_Terminate may overlap, and are not
   --  required to be fully nested.
   --
   --  Note that if Pa_Initialize returns an error code, Pa_Terminate should
   --  NOT be called.
   --
   --  Return value
   --    paNoError if successful, otherwise an error code indicating the
   --    cause of failure.
   --
   --  See Also: Pa_Terminate
   pragma Import (C, Pa_Initialize, "Pa_Initialize");

   -----------------------------------------------------------------------------
   --  Pa_Terminate
   --
   --  Library termination function - call this when finished using PortAudio.
   --  This function deallocates all resources allocated by PortAudio since it
   --  was initialized by a call to Pa_Initialize. In cases where Pa_Initialise
   --  has been called multiple times, each call must be matched with a
   --  corresponding call to Pa_Terminate. The final matching call to
   --  Pa_Terminate will automatically close any PortAudio streams that are
   --  still open.
   --
   --  Pa_Terminate MUST be called before exiting a program which uses
   --  PortAudio. Failure to do so may result in serious resource leaks, such
   --  as audio devices not being available until the next reboot.
   --
   --  Return value
   --    paNoError if successful, otherwise an error code indicating the
   --    cause of failure.
   --
   --   See Also: Pa_Initialize
   function Pa_Terminate return PaError;
   pragma Import (C, Pa_Terminate, "Pa_Terminate");

   -----------------------------------------------------------------------------
   --  The type used to refer to audio devices. Values of this type usually
   --  range from 0 to (Pa_GetDeviceCount - 1), and may also take on the
   --  PaNoDevice and paUseHostApiSpecificDeviceSpecification values.
   --
   --  See Also: Pa_GetDeviceCount, paNoDevice,
   --            paUseHostApiSpecificDeviceSpecification
   subtype PaDeviceIndex is Integer;

   --  A special PaDeviceIndex value indicating that no device is available,
   --  or should be used.
   --
   --  See Also: PaDeviceIndex
   paNoDevice : constant PaDeviceIndex := -1;

   --  A special PaDeviceIndex value indicating that the device(s) to be used
   --  are specified in the host API specific stream info structure.
   --
   --  See Also: PaDeviceIndex
   paUseHostApiSpecificDeviceSpecification : constant PaDeviceIndex := -2;

   -----------------------------------------------------------------------------
   --  The type used to enumerate to host APIs at runtime. Values of this type
   --  range from 0 to (Pa_GetHostApiCount - 1).
   --
   --  See Also: Pa_GetHostApiCount
   subtype PaHostApiIndex is Integer;

   -----------------------------------------------------------------------------
   --  Pa_GetHostApiCount
   --
   --  Retrieve the number of available host APIs. Even if a host API is
   --  available it may have no devices available.
   --
   --  Return Value
   --    A non-negative value indicating the number of available host APIs
   --    or, a PaErrorCode (which are always negative) if PortAudio is not
   --    initialized or an error is encountered.
   --
   --  See Also: PaHostApiIndex
   function Pa_GetHostApiCount return PaHostApiIndex;
   pragma Import (C, Pa_GetHostApiCount, "Pa_GetHostApiCount");

   -----------------------------------------------------------------------------
   --  Pa_GetDefaultHostApi
   --
   --  Retrieve the index of the default host API. The default host API will be
   --  the lowest common denominator host API on the current platform and is
   --  unlikely to provide the best performance.
   --
   --  @return A non-negative value ranging from 0 to (Pa_GetHostApiCount - 1)
   --  indicating the default host API index or, a PaErrorCode (which are always
   --  negative) if PortAudio is not initialized or an error is encountered.
   function Pa_GetDefaultHostApi return PaHostApiIndex;
   pragma Import (C, Pa_GetDefaultHostApi, "Pa_GetDefaultHostApi");

   -----------------------------------------------------------------------------
   --  Unchanging unique identifiers for each supported host API. This type is
   --  used in the PaHostApiInfo structure. The values are guaranteed to be
   --  unique and to never change, thus allowing code to be written that
   --  conditionally uses host API specific extensions.
   --
   --  New type ids will be allocated when support for a host API reaches
   --  "public alpha" status, prior to that developers should use the
   --  paInDevelopment type id.
   --
   --  See Also: PaHostApiInfo
   type PaHostApiTypeId is
     (
      paInDevelopment,
      paDirectSound,
      paMME,
      paASIO,
      paSoundManager,
      paCoreAudio,
      paOSS,
      paALSA,
      paAL,
      paBeOS,
      paWDMKS,
      paJACK,
      paWASAPI,
      paAudioScienceHPI
     );

   for PaHostApiTypeId use
     (
      paInDevelopment   => 0,
      paDirectSound     => 1,
      paMME             => 2,
      paASIO            => 3,
      paSoundManager    => 4,
      paCoreAudio       => 5,
      paOSS             => 7,
      paALSA            => 8,
      paAL              => 9,
      paBeOS            => 10,
      paWDMKS           => 11,
      paJACK            => 12,
      paWASAPI          => 13,
      paAudioScienceHPI => 14
     );
   pragma Convention (C, PaHostApiTypeId);

   -----------------------------------------------------------------------------
   --  A structure containing information about a particular host API.
   type PaHostApiInfo is
      record
      --  this is struct version 1
         structVersion       : aliased Integer;

         --  The well known unique identifier of this host API
         --  See Also: PaHostApiTypeId
         typeId              : aliased PaHostApiTypeId;

         --  A textual description of the host API for display on user
         --  interfaces.
         name                : aliased ICS.chars_ptr;

         --  The number of devices belonging to this host API. This field may
         --  be used in conjunction with Pa_HostApiDeviceIndexToDeviceIndex
         --  to enumerate all devices for this host API.
         --  See Also: Pa_HostApiDeviceIndexToDeviceIndex
         deviceCount         : aliased Integer;

         --  The default input device for this host API. The value will be
         --  a device index ranging from 0 to (Pa_GetDeviceCount - 1), or
         --  paNoDevice if no default input device is available.
         defaultInputDevice  : aliased PaDeviceIndex;

         --  The default output device for this host API. The value will be
         --  a device index ranging from 0 to (Pa_GetDeviceCount - 1), or
         --  paNoDevice if no default output device is available.
         defaultOutputDevice : aliased PaDeviceIndex;

      end record;
   pragma Convention (C, PaHostApiInfo);

   type PaHostApiInfo_Ptr is access all PaHostApiInfo;
   pragma Convention (C, PaHostApiInfo_Ptr);

   -----------------------------------------------------------------------------
   --  Retrieve a pointer to a structure containing information about a specific
   --  host API.
   --
   --  Parameters
   --    hostApi - A valid host API index ranging from
   --              0 to (Pa_GetHostApiCount - 1)
   --
   --  Return Value
   --    A pointer to an immutable PaHostApiInfo structure describing
   --    a specific host API. If the hostApi parameter is out of range or an
   --    error is encountered, the function returns null.
   --
   --  The returned structure is owned by the PortAudio implementation and must
   --  not be manipulated or freed. The pointer is only guaranteed to be valid
   --  between calls to Pa_Initialize and Pa_Terminate.
   function Pa_GetHostApiInfo (hostApi : PaHostApiIndex)
                               return access PaHostApiInfo;
   pragma Import (C, Pa_GetHostApiInfo, "Pa_GetHostApiInfo");

   -----------------------------------------------------------------------------
   --  Pa_HostApiTypeIdToHostApiIndex
   --
   --  Convert a static host API unique identifier, into a runtime host
   --  API index.
   --
   --  Parameters
   --    type A unique host API identifier belonging to the
   --         PaHostApiTypeId enumeration.
   --
   --  Return Value
   --    A valid PaHostApiIndex ranging from 0 to
   --   (Pa_GetHostApiCount - 1) or, a PaErrorCode (which are always negative)
   --   if PortAudio is not initialized or an error is encountered.
   --
   --  The paHostApiNotFound error code indicates that the host API specified
   --  by the type parameter is not available.
   --
   --  See Also: PaHostApiTypeId
   function Pa_HostApiTypeIdToHostApiIndex (typeId : PaHostApiTypeId)
                                            return PaHostApiIndex;
   pragma Import (C,
                  Pa_HostApiTypeIdToHostApiIndex,
                 "Pa_HostApiTypeIdToHostApiIndex");

   -----------------------------------------------------------------------------
   --  Pa_HostApiDeviceIndexToDeviceIndex
   --
   --  Convert a host-API-specific device index to standard PortAudio device
   --  index. This function may be used in conjunction with the deviceCount
   --  field of PaHostApiInfo to enumerate all devices for the specified host
   --  API.
   --
   --  Parameters
   --    hostApi - A valid host API index ranging from
   --              0 to (Pa_GetHostApiCount - 1)
   --
   --    hostApiDeviceIndex
   --               A valid per-host device index in the range
   --               0 to (Pa_GetHostApiInfo (hostApi).all.deviceCount - 1)
   --
   --  Return Value
   --    A non-negative PaDeviceIndex ranging from 0 to
   --    (Pa_GetDeviceCount - 1) or, a PaErrorCode (which are always negative)
   --    if PortAudio is not initialized or an error is encountered.
   --
   --    A paInvalidHostApi error code indicates that the host API index
   --    specified by the hostApi parameter is out of range.
   --
   --    A paInvalidDevice error code indicates that the hostApiDeviceIndex
   --    parameter is out of range.
   --
   --  See Also: PaHostApiInfo
   function Pa_HostApiDeviceIndexToDeviceIndex
     (hostApiIdx         : PaHostApiIndex;
      hostApiDeviceIndex : Integer)
      return PaDeviceIndex;
   pragma Import (C,
                  Pa_HostApiDeviceIndexToDeviceIndex,
                 "Pa_HostApiDeviceIndexToDeviceIndex");

   -----------------------------------------------------------------------------
   --  Structure used to return information about a host error condition.
   type PaHostErrorInfo is
      record
         --  the host API which returned the error code
         hostApiType : aliased PaHostApiTypeId;

         --  the error code returned
         errorCode   : aliased IC.long;

         --  a textual description of the error if available, otherwise
         --  a zero-length string
         errorText   : aliased ICS.chars_ptr;
      end record;
   pragma Convention (C, PaHostErrorInfo);

   type PaHostErrorInfo_Ptr is access all PaHostErrorInfo;
   pragma Convention (C, PaHostErrorInfo_Ptr);

   -----------------------------------------------------------------------------
   --  Pa_GetLastHostErrorInfo
   function Pa_GetLastHostErrorInfo return access PaHostErrorInfo_Ptr;
   pragma Import (C, Pa_GetLastHostErrorInfo, "Pa_GetLastHostErrorInfo");

   -----------------------------------------------------------------------------
   --  Device enumeration and capabilities  ------------------------------------
   -----------------------------------------------------------------------------

   -----------------------------------------------------------------------------
   --  Pa_GetDeviceCount
   --
   --  Retrieve the number of available devices. The number of available
   --  devices may be zero.
   --
   --  Return Value
   --    A non-negative value indicating the number of available devices
   --    or, a PaErrorCode (which are always negative) if PortAudio is not
   --    initialized or an error is encountered.
   function Pa_GetDeviceCount return PaDeviceIndex;
   pragma Import (C, Pa_GetDeviceCount, "Pa_GetDeviceCount");

   -----------------------------------------------------------------------------
   --  Pa_GetDefaultInputDevice
   --
   --  Retrieve the index of the default input device. The result can be used
   --  in the inputDevice parameter to Pa_OpenStream.
   --
   --  Return Value
   --    The default input device index for the default host API, or
   --    paNoDevice if no default input device is available or an error was
   --    encountered.
   function Pa_GetDefaultInputDevice return PaDeviceIndex;
   pragma Import (C, Pa_GetDefaultInputDevice, "Pa_GetDefaultInputDevice");

   -----------------------------------------------------------------------------
   --  Pa_GetDefaultOutputDevice
   --
   --  Retrieve the index of the default output device. The result can be used
   --  in the outputDevice parameter to Pa_OpenStream ().
   --
   --  Return Value
   --    The default output device index for the default host API, or
   --    paNoDevice if no default output device is available or an error was
   --    encountered.
   --
   --  Note
   --    On the PC, the user can specify a default device by
   --    setting an environment variable. For example, to use device #1.
   --
   --      set PA_RECOMMENDED_OUTPUT_DEVICE=1
   --
   --  The user should first determine the available device ids by using
   --  the supplied application "pa_devs".
   function Pa_GetDefaultOutputDevice return PaDeviceIndex;
   pragma Import (C, Pa_GetDefaultOutputDevice, "Pa_GetDefaultOutputDevice");

   -----------------------------------------------------------------------------
   --  The type used to represent monotonic time in seconds that can be used
   --  for synchronisation. The type is used for the outTime argument to the
   --  PaStreamCallback and as the result of Pa_GetStreamTime.
   --
   --  See Also: PaStreamCallback, Pa_GetStreamTime
   subtype PaTime is  Long_Float;

   -----------------------------------------------------------------------------
   --  A type used to specify one or more sample formats. Each value indicates
   --  a possible format for sound data passed to and from the stream callback,
   --  Pa_ReadStream and Pa_WriteStream.
   --
   --  The standard formats paFloat32, paInt16, paInt32, paInt24, paInt8 and
   --  aUInt8 are usually implemented by all implementations.
   --
   --  The floating point representation (paFloat32) uses +1.0 and -1.0 as
   --  the maximum and minimum respectively.
   --
   --  paUInt8 is an unsigned 8 bit format where 128 is considered "ground"
   --
   --  The paNonInterleaved flag indicates that a multichannel buffer is
   --  passed as a set of non-interleaved pointers.
   --
   --  See Also: Pa_OpenStream, Pa_OpenDefaultStream, PaDeviceInfo
   --  See Also: paFloat32, paInt16, paInt32, paInt24, paInt8
   --  See Also: paUInt8, paCustomFormat, paNonInterleaved
   subtype PaSampleFormat is IC.unsigned_long;

   paFloat32        : constant PaSampleFormat := 16#0000_0001#;
   paInt32          : constant PaSampleFormat := 16#0000_0002#;
   paInt24          : constant PaSampleFormat := 16#0000_0004#;
   paInt16          : constant PaSampleFormat := 16#0000_0008#;
   paInt8           : constant PaSampleFormat := 16#0000_0010#;
   paUInt8          : constant PaSampleFormat := 16#0000_0020#;
   paCustomFormat   : constant PaSampleFormat := 16#0001_0000#;

   paNonInterleaved : constant PaSampleFormat := 16#8000_0000#;

   -----------------------------------------------------------------------------
   --  A structure providing information and capabilities of PortAudio devices.
   --  Devices may support input, output or both input and output.
   type PaDeviceInfo is
      record
      --  this is struct version 2
         structVersion            : aliased Integer;
         name                     : aliased ICS.chars_ptr;

         --  note this is a host API index, not a type id
         hostApi                  : aliased PaHostApiIndex;

         maxInputChannels         : aliased Integer;
         maxOutputChannels        : aliased Integer;

         --  Default latency values for interactive performance.
         defaultLowInputLatency   : aliased PaTime;
         defaultLowOutputLatency  : aliased PaTime;

         --  Default latency values for robust non - interactive applications
         --  (eg. playing sound files).

         defaultHighInputLatency  : aliased PaTime;
         defaultHighOutputLatency : aliased PaTime;

         defaultSampleRate        : aliased Long_Float;
      end record;
   pragma Convention (C, PaDeviceInfo);

   type PaDeviceInfo_Ptr is access all PaDeviceInfo;
   pragma Convention (C, PaDeviceInfo_Ptr);

   -----------------------------------------------------------------------------
   --  Pa_GetDeviceInfo
   --
   --  Retrieve a pointer to a PaDeviceInfo structure containing information
   --  about the specified device.
   --
   --  Return Value
   --    A pointer to an immutable PaDeviceInfo structure. If the device
   --    parameter is out of range the function returns null.
   --
   --  Parameters
   --    device - A valid device index in the range 0 to
   --             (Pa_GetDeviceCount - 1)
   --
   --  Note
   --    PortAudio manages the memory referenced by the returned pointer,
   --    the client must not manipulate or free the memory. The pointer is only
   --    guaranteed to be valid between calls to Pa_Initialize and Pa_Terminate.
   --
   --  See Also: PaDeviceInfo, PaDeviceIndex
   function Pa_GetDeviceInfo (device : PaDeviceIndex)
                              return access PaDeviceInfo;
   pragma Import (C, Pa_GetDeviceInfo, "Pa_GetDeviceInfo");

   -----------------------------------------------------------------------------
   --  Parameters for one direction (input or output) of a stream.
   type PaStreamParameters is
      record
         --  A valid device index in the range 0 to (Pa_GetDeviceCount - 1)
         --  specifying the device to be used or the special constant
         --  paUseHostApiSpecificDeviceSpecification which indicates that the
         --  actual device(s) to use are specified in
         --  hostApiSpecificStreamInfo. This field must not be set to
         --  paNoDevice.
         device : aliased PaDeviceIndex;

         --  The number of channels of sound to be delivered to the stream
         --  callback or accessed by Pa_ReadStream or Pa_WriteStream. It can
         --  range from 1 to the value of maxInputChannels in the PaDeviceInfo
         --  record for the device specified by the device parameter.
         channelCount : aliased Integer;

         --  The sample format of the buffer provided to the stream callback,
         --  a_ReadStream or Pa_WriteStream. It may be any of the formats
         --  described by the PaSampleFormat enumeration.
         sampleFormat : aliased PaSampleFormat;

         --  The desired latency in seconds. Where practical, implementations
         --  should configure their latency based on these parameters,
         --  otherwise they may choose the closest viable latency instead.
         --  Unless the suggested latency is greater than the absolute upper
         --  limit for the device implementations should round the
         --  suggestedLatency up to the next practical value - ie to provide an
         --  equal or higher latency than suggestedLatency wherever possible.
         --  Actual latency values for an open stream may be retrieved using
         --  the inputLatency and outputLatency fields of the PaStreamInfo
         --  structure returned by Pa_GetStreamInfo.
         --  See Also: default*Latency in PaDeviceInfo, *Latency in PaStreamInfo
         suggestedLatency : aliased PaTime;

         --  An optional pointer to a host API specific data structure
         --  containing additional information for device setup and/or stream
         --  processing. hostApiSpecificStreamInfo is never required for
         --  correct operation, if not used it should be set to null.
         hostApiSpecificStreamInfo : aliased System.Address
           := System.Null_Address;
      end record;
   pragma Convention (C, PaStreamParameters);

   type PaStreamParameters_Ptr is access all PaStreamParameters;
   pragma Convention (C, PaStreamParameters_Ptr);

   -----------------------------------------------------------------------------
   --  Return code for Pa_IsFormatSupported indicating success.
   paFormatIsSupported : constant PaError := paNoError;

   -----------------------------------------------------------------------------
   --  Pa_IsFormatSupported
   --
   --  Determine whether it would be possible to open a stream with the
   --  specified parameters.
   --
   --  Parameters
   --    inputParameters
   --       A structure that describes the input parameters
   --       used to open a stream. The suggestedLatency field is ignored.
   --       See PaStreamParameters for a description of these parameters.
   --       inputParameters must be null for output-only streams.
   --
   --    outputParameters
   --       A structure that describes the output parameters used to open
   --       a stream. The suggestedLatency field is ignored.
   --       See PaStreamParameters for a description of these parameters.
   --       outputParameters must be null for input-only streams.
   --
   --    sampleRate
   --      The required sampleRate. For full-duplex streams it is the sample
   --      rate for both input and output
   --
   --  Return Value
   --    Returns 0 if the format is supported, and an error code indicating why
   --    the format is not supported otherwise. The constant paFormatIsSupported
   --    is provided to compare with the return value for success.
   --
   --  See Also: paFormatIsSupported, PaStreamParameters
   function Pa_IsFormatSupported (inputParameters  : access PaStreamParameters;
                                  outputParameters : access PaStreamParameters;
                                  sampleRate       : Long_Float)
                                  return PaError;
   pragma Import (C, Pa_IsFormatSupported, "Pa_IsFormatSupported");

   -----------------------------------------------------------------------------
   --  Streaming types and functions  ------------------------------------------
   -----------------------------------------------------------------------------

   -----------------------------------------------------------------------------
   --  A single PaStream can provide multiple channels of real-time streaming
   --  audio input and output to a client application. A stream provides access
   --  to audio hardware represented by one or more PaDevices. Depending on the
   --  underlying Host API, it may be possible to open multiple streams using
   --  the same device, however this behavior is implementation defined.
   --  Portable applications should assume that a PaDevice may be simultaneously
   --  used by at most one PaStream.
   --
   --  Pointers to PaStream objects are passed between PortAudio functions that
   --  operate on streams.
   --
   --  See Also: Pa_OpenStream, Pa_OpenDefaultStream, Pa_OpenDefaultStream,
   --  Pa_CloseStream, Pa_StartStream, Pa_StopStream, Pa_AbortStream,
   --  Pa_IsStreamActive, Pa_GetStreamTime, Pa_GetStreamCpuLoad
   subtype PaStream is System.Address;

   -----------------------------------------------------------------------------
   --  Can be passed as the framesPerBuffer parameter to Pa_OpenStream or
   --  Pa_OpenDefaultStream to indicate that the stream callback will accept
   --  buffers of any size.
   paFramesPerBufferUnspecified : constant IC.unsigned_long := 0;

   -----------------------------------------------------------------------------
   --  Flags used to control the behavior of a stream. They are passed as
   --  parameters to Pa_OpenStream or Pa_OpenDefaultStream. Multiple flags may
   --  be ORed together.
   --
   --  See Also: Pa_OpenStream, Pa_OpenDefaultStream
   --  See Also: paNoFlag, paClipOff, paDitherOff, paNeverDropInput,
   --  paPrimeOutputBuffersUsingStreamCallback, paPlatformSpecificFlags
   type PaStreamFlags is new IC.unsigned_long;
   pragma Convention (C, PaStreamFlags);

   paNoFlag         : constant PaStreamFlags := 0;

   --  Disable default clipping of out of range samples.
   --  See Also: PaStreamFlags
   paClipOff        : constant PaStreamFlags := 16#0000_0001#;

   --  Disable default dithering.
   --  See Also: PaStreamFlags
   paDitherOff      : constant PaStreamFlags := 16#0000_0002#;

   --  Flag requests that where possible a full duplex stream will not discard
   --  overflowed input samples without calling the stream callback. This flag
   --  is only valid for full duplex callback streams and only when used in
   --  combination with the paFramesPerBufferUnspecified (0) framesPerBuffer
   --  parameter. Using this flag incorrectly results in a paInvalidFlag error
   --  being returned from Pa_OpenStream and Pa_OpenDefaultStream.
   --
   --  See Also: PaStreamFlags, paFramesPerBufferUnspecified

   paNeverDropInput : constant PaStreamFlags := 16#0000_0004#;
   --  Call the stream callback to fill initial output buffers, rather than the
   --  default behavior of priming the buffers with zeros (silence). This flag
   --  has no effect for input-only and blocking read/write streams.
   --
   --  See Also: PaStreamFlags

   paPrimeOutputBuffersUsingStreamCallback
                    : constant PaStreamFlags := 16#0000_0008#;
   --  A mask specifying the platform specific bits.
   --  See Also: PaStreamFlags
   paPlatformSpecificFlags : constant PaStreamFlags := 16#FFFF_0000#;

   -----------------------------------------------------------------------------
   --  Timing information for the buffers passed to the stream callback.
   type PaStreamCallbackTimeInfo is
      record
         inputBufferAdcTime  : aliased PaTime;
         --  The time when the first sample of the input buffer was captured at
         --  the ADC input

         currentTime         : aliased PaTime;
         --  The time when the stream callback was invoked

         outputBufferDacTime : aliased PaTime;
         --  The time when the first sample of the output buffer will output
         --  the DAC
      end record;
   pragma Convention (C, PaStreamCallbackTimeInfo);

   type PaStreamCallbackTimeInfo_Ptr is access all PaStreamCallbackTimeInfo;
   pragma Convention (C, PaStreamCallbackTimeInfo_Ptr);

   -----------------------------------------------------------------------------
   --  Flag bit constants for the statusFlags to PaStreamCallback.
   --  See Also: paInputUnderflow, paInputOverflow, paOutputUnderflow,
   --  paOutputOverflow, paPrimingOutput
   type PaStreamCallbackFlags is new IC.unsigned_long;
   pragma Convention (C, PaStreamCallbackFlags);

   --  In a stream opened with paFramesPerBufferUnspecified, indicates that
   --  input data is all silence (zeros) because no real data is available.
   --  In a stream opened without paFramesPerBufferUnspecified, it indicates
   --  that one or more zero samples have been inserted into the input buffer
   --  to compensate for an input underflow.
   --  See Also: PaStreamCallbackFlags
   paInputUnderflow  : constant PaStreamCallbackFlags := 16#0000_0001#;

   --  In a stream opened with paFramesPerBufferUnspecified, indicates that
   --  data prior to the first sample of the input buffer was discarded due
   --  to an overflow, possibly because the stream callback is using too much
   --  CPU time. Otherwise indicates that data prior to one or more samples in
   --  the input buffer was discarded.
   --  See Also: PaStreamCallbackFlags
   paInputOverflow   : constant PaStreamCallbackFlags := 16#0000_0002#;

   --  Indicates that output data (or a gap) was inserted, possibly because
   --  the stream callback is using too much CPU time.
   --  See Also: PaStreamCallbackFlags
   paOutputUnderflow : constant PaStreamCallbackFlags := 16#0000_0004#;

   --  Indicates that output data will be discarded because no room is
   --  available.
   --  See Also: PaStreamCallbackFlags
   paOutputOverflow  : constant PaStreamCallbackFlags := 16#0000_0008#;

   --  Some of all of the output data will be used to prime the stream, input
   --  data may be zero.
   --  See Also: PaStreamCallbackFlags
   paPrimingOutput   : constant PaStreamCallbackFlags := 16#0000_0010#;

   -----------------------------------------------------------------------------
   --  Allowable return values for the PaStreamCallback.
   --  See Also: PaStreamCallback
   type PaStreamCallbackResult is
     (
      paContinue,
      --  Signal that the stream should continue invoking the callback and
      --  processing audio.

      paComplete,
      --  Signal that the stream should stop invoking the callback and finish
      --  once all output samples have played.

      paAbort
      --  Signal that the stream should stop invoking the callback and finish
      --  as soon as possible.
     );

   for PaStreamCallbackResult use
     (
      paContinue => 0,
      paComplete => 1,
      paAbort => 2
     );
   pragma Convention (C, PaStreamCallbackResult);

   -----------------------------------------------------------------------------
   --  Functions of type PaStreamCallback are implemented by PortAudio clients.
   --  They consume, process or generate audio in response to requests from an
   --  active PortAudio stream.
   --
   --  When a stream is running, PortAudio calls the stream callback
   --  periodically. The callback function is responsible for processing buffers
   --  of audio samples  passed via the input and output parameters.
   --
   --  The PortAudio stream callback runs at very high or real-time priority. It
   --  is required to consistently meet its time deadlines. Do not allocate
   --   memory, access the file system, call library functions or call other
   --  functions  from the stream callback that may block or take an
   --  unpredictable amount of time to complete.
   --
   --  In order for a stream to maintain glitch-free operation the callback must
   --  consume and return audio data faster than it is recorded and/or played.
   --  PortAudio anticipates that each callback invocation may execute for
   --  a duration approaching the duration of frameCount audio frames at
   --  the stream  sample rate. It is reasonable to expect to be able to
   --  utilise 70% or more of the available CPU time in the PortAudio callback.
   --  However, due to buffer size  adaption and other factors, not all host
   --  APIs are able to guarantee audio stability under heavy CPU load with
   --  arbitrary fixed callback buffer sizes. When high callback CPU utilisation
   --  is required the most robust behavior can be achieved by using
   --  paFramesPerBufferUnspecified as the Pa_OpenStream framesPerBuffer
   --  parameter.
   --
   --  Parameters
   --    input and output
   --      are arrays of interleaved samples, the format, packing and number
   --      of channels used by the buffers are determined by parameters to
   --      Pa_OpenStream.
   --
   --    frameCount
   --      The number of sample frames to be processed by the stream callback.
   --
   --    timeInfo
   --      Timestamps indicating the ADC capture time of the first sample in
   --      the input buffer, the DAC output time of the first sample in the
   --      output buffer and the time the callback was invoked.
   --      See PaStreamCallbackTimeInfo and Pa_GetStreamTime
   --
   --    statusFlags
   --      Flags indicating whether input and/or output buffers have been
   --      inserted or will be dropped to overcome underflow or overflow
   --      conditions.
   --
   --    userData
   --      The value of a user supplied pointer passed to Pa_OpenStream intended
   --      for storing synthesis data etc.
   --
   --  Return Value
   --    The stream callback should return one of the values in the
   --    PaStreamCallbackResult enumeration. To ensure that the callback
   --    continues to be called, it should return paContinue. Either
   --    paComplete or paAbort can be returned to finish stream processing,
   --    after either of these values is returned the callback will not be
   --    called again. If paAbort is returned the stream will finish as soon
   --    as possible. If paComplete is returned, the stream will continue until
   --    all buffers generated by the callback have been played. This may be
   --    useful in applications such as soundfile players where a specific
   --    duration of output is required. However, it is not necessary to utilize
   --    this mechanism as Pa_StopStream, Pa_AbortStream or Pa_CloseStream can
   --    also be used to stop the stream. The callback must always fill the
   --    entire output buffer irrespective of its return value.
   --
   --  See Also: Pa_OpenStream, Pa_OpenDefaultStream
   --
   --  Note
   --    With the exception of Pa_GetStreamCpuLoad() it is not permissible to
   --    call PortAudio API functions from within the stream callback.
   type PaStreamCallback is access function
     (input       :        System.Address;
      output      :        System.Address;
      frameCount  :        IC.unsigned_long;
      timeInfo    : access PaStreamCallbackTimeInfo;
      statusFlags :        PaStreamCallbackFlags;
      userData    :        System.Address)
      return PaStreamCallbackResult;
   pragma Convention (C, PaStreamCallback);

   -----------------------------------------------------------------------------
   --  Pa_OpenStream
   --
   --  Opens a stream for either input, output or both.
   --
   --  Parameters
   --    stream
   --      The address of a PaStream pointer which will receive a pointer to the
   --      newly opened stream.
   --
   --    inputParameters
   --      A structure that describes the input parameters used by the opened
   --      stream. See PaStreamParameters for a description of these parameters.
   --      inputParameters must be null for output-only streams.
   --
   --    outputParameters
   --      A structure that describes the output parameters used by the opened
   --      stream. See PaStreamParameters for a description of these parameters.
   --      outputParameters must be null for input-only streams.
   --
   --    sampleRate
   --      The desired sampleRate. For full-duplex streams it is the sample rate
   --      for both input and output
   --
   --    framesPerBuffer
   --      The number of frames passed to the stream callback function, or the
   --      preferred block granularity for a blocking read/write stream. The
   --      special value paFramesPerBufferUnspecified (0) may be used to request
   --      that the stream callback will receive an optimal (and possibly
   --      varying) number of frames based on host requirements and the
   --      requested latency settings.
   --      Note
   --        With some host APIs, the use of non-zero framesPerBuffer for a
   --        callback stream may introduce an additional layer of buffering
   --        which could introduce additional latency. PortAudio guarantees
   --        that the additional latency will be kept to the theoretical minimum
   --        however, it is strongly recommended that a non-zero framesPerBuffer
   --        value only be used when your algorithm requires a fixed number of
   --        frames per stream callback.
   --
   --    streamFlags
   --      Flags which modify the behaviour of the streaming process. This
   --      parameter may contain a combination of flags ORed together. Some
   --      flags may only be relevant to certain buffer formats.
   --
   --    streamCallback
   --      A pointer to a client supplied function that is responsible for
   --      processing and filling input and output buffers. If this parameter
   --      is null the stream will be opened in 'blocking read/write' mode. In
   --      blocking mode, the client can receive sample data using Pa_ReadStream
   --      and write sample data using Pa_WriteStream, the number of samples
   --      that may be read or written without blocking is returned by
   --      Pa_GetStreamReadAvailable and Pa_GetStreamWriteAvailable
   --      respectively.
   --
   --    userData
   --      A client supplied pointer which is passed to the stream callback
   --      function. It could for example, contain a pointer to instance data
   --      necessary for processing the audio buffers. This parameter is ignored
   --      if streamCallback is null.
   --
   --  Return value
   --    Upon success Pa_OpenStream returns paNoError and places a pointer to a
   --    valid PaStream in the stream argument. The stream is inactive
   --    (stopped).
   --    If a call to Pa_OpenStream fails, a non-zero error code is returned see
   --    PaError for possible error codes) and the value of stream is invalid.
   --
   --  See Also: PaStreamParameters, PaStreamCallback, Pa_ReadStream,
   --  Pa_WriteStream, Pa_GetStreamReadAvailable, Pa_GetStreamWriteAvailable
   function Pa_OpenStream
     (stream           : access PaStream;
      inputParameters  : access PaStreamParameters;
      outputParameters : access PaStreamParameters;
      sampleRate       :        Long_Float;
      framesPerBuffer  :        IC.unsigned_long;
      streamFlags      :        PaStreamFlags;
      streamCallback   :	PaStreamCallback;
      userData         :        System.Address)
      return PaError;
   pragma Import (C, Pa_OpenStream, "Pa_OpenStream");

   -----------------------------------------------------------------------------
   --  Pa_OpenDefaultStream
   --
   --  A simplified version of Pa_OpenStream that opens the default input
   --  and/or output devices.
   --
   --  Parameters
   --    stream
   --      The address of a PaStream pointer which will receive a pointer to the
   --      newly opened stream.
   --
   --    numInputChannels
   --      The number of channels of sound that will be supplied to the stream
   --      callback or returned by Pa_ReadStream. It can range from 1 to the
   --      value of maxInputChannels in the PaDeviceInfo record for the default
   --      input device. If 0 the stream is opened as an output-only stream.
   --
   --    numOutputChannels
   --      The number of channels of sound to be delivered to the stream
   --      callback or passed to Pa_WriteStream. It can range from 1 to the
   --      value of maxOutputChannels in the PaDeviceInfo record for the default
   --      output device. If 0 the stream is opened as an output-only stream.
   --
   --    sampleFormat
   --      The sample format of both the input and output buffers provided to
   --      the callback or passed to and from Pa_ReadStream and Pa_WriteStream.
   --      sampleFormat may be any of the formats described by the
   --      PaSampleFormat enumeration.
   --
   --    sampleRate
   --      Same as Pa_OpenStream parameter of the same name.
   --
   --    framesPerBuffer
   --      Same as Pa_OpenStream parameter of the same name.
   --
   --    streamCallback
   --      Same as Pa_OpenStream parameter of the same name.
   --
   --    userData
   --      Same as Pa_OpenStream parameter of the same name.
   --
   --  Return Value
   --    As for Pa_OpenStream
   --
   --  See Also: Pa_OpenStream, PaStreamCallback
   function Pa_OpenDefaultStream
     (stream            : access PaStream;
      numInputChannels  :        Integer;
      numOutputChannels :        Integer;
      sampleFormat      :        PaSampleFormat;
      sampleRate        :        Long_Float;
      framesPerBuffer   :        IC.unsigned_long;
      streamCallback    : 	PaStreamCallback;
      userData          :        System.Address)
      return PaError;
   pragma Import (C, Pa_OpenDefaultStream, "Pa_OpenDefaultStream");

   -----------------------------------------------------------------------------
   --  Pa_CloseStream
   --
   --  Closes an audio stream. If the audio stream is active it discards any
   --  pending buffers as if Pa_AbortStream had been called.
   function Pa_CloseStream (stream : PaStream) return PaError;
   pragma Import (C, Pa_CloseStream, "Pa_CloseStream");

   -----------------------------------------------------------------------------
   --  Functions of type PaStreamFinishedCallback are implemented by PortAudio
   --  clients. They can be registered with a stream using the
   --  Pa_SetStreamFinishedCallback function. Once registered they are called
   --  when the stream becomes inactive (ie once a call to Pa_StopStream will
   --  not block).
   --  A stream will become inactive after the stream callback returns non-zero,
   --  or when Pa_StopStream or Pa_AbortStream is called. For a stream providing
   --  audio output, if the stream callback returns paComplete, or Pa_StopStream
   --  is called, the stream finished callback will not be called until all
   --  generated sample data  has been played.
   --
   --  Parameters
   --    userData - The userData parameter supplied to Pa_OpenStream
   --
   --  See Also: Pa_SetStreamFinishedCallback
   type PaStreamFinishedCallback is
     access procedure (userData : System.Address);
   pragma Convention (C, PaStreamFinishedCallback);

   -----------------------------------------------------------------------------
   --  Pa_SetStreamFinishedCallback
   --
   --  Register a stream finished callback function which will be called when
   --  the  stream becomes inactive. See the description of
   --  PaStreamFinishedCallback for  further details about when the callback
   --  will be called.
   --
   --  Parameters
   --    stream
   --      a pointer to a PaStream that is in the stopped state - if the stream
   --      is not stopped, the stream's finished callback will remain unchanged
   --      and an error code will be returned.
   --
   --    streamFinishedCallback
   --      a pointer to a function with the same signature as
   --      PaStreamFinishedCallback, that will be called when the stream becomes
   --      inactive. Passing null for this parameter will un-register a
   --      previously registered stream finished callback function.
   --
   --  Return Value
   --    On success returns paNoError, otherwise an error code indicating the
   --    cause of the error.
   --
   --  See Also: PaStreamFinishedCallback
   function Pa_SetStreamFinishedCallback
     (stream                 : PaStream;
      streamFinishedCallback : PaStreamFinishedCallback)
      return PaError;
   pragma Import (C,
                  Pa_SetStreamFinishedCallback,
                  "Pa_SetStreamFinishedCallback");

   -----------------------------------------------------------------------------
   --  Pa_StartStream
   --
   --  Commences audio processing.
   function Pa_StartStream (stream : PaStream) return PaError;
   pragma Import (C, Pa_StartStream, "Pa_StartStream");

   -----------------------------------------------------------------------------
   --  Pa_StopStream
   --
   --  Terminates audio processing. It waits until all pending audio buffers
   --  have been played before it returns.
   function Pa_StopStream (stream : PaStream) return PaError;
   pragma Import (C, Pa_StopStream, "Pa_StopStream");

   -----------------------------------------------------------------------------
   --  Pa_AbortStream
   --
   --  Terminates audio processing immediately without waiting for pending
   --  buffers to complete.
   function Pa_AbortStream (stream : PaStream) return PaError;
   pragma Import (C, Pa_AbortStream, "Pa_AbortStream");

   -----------------------------------------------------------------------------
   --  Pa_IsStreamStopped
   --
   --  Determine whether the stream is stopped.
   --  A stream is considered to be stopped prior to a successful call
   --  to Pa_StartStream and after a successful call to Pa_StopStream or
   --  Pa_AbortStream. If a stream callback returns a value other than
   --  paContinue the stream is NOT considered to be stopped.
   --
   --  Return Value
   --    Returns one (1) when the stream is stopped, zero (0) when the stream
   --    is running or, a PaErrorCode (which are always negative) if PortAudio
   --    is not initialized or an error is encountered.
   --
   --  See Also: Pa_StopStream, Pa_AbortStream, Pa_IsStreamActive
   function Pa_IsStreamStopped (stream : PaStream) return integer;
   pragma Import (C, Pa_IsStreamStopped, "Pa_IsStreamStopped");

   -----------------------------------------------------------------------------
   --  Pa_IsStreamActive
   --
   --  Determine whether the stream is active.
   --  A stream is active after a successful call to Pa_StartStream, until it
   --  becomes inactive either as a result of a call to Pa_StopStream or
   --  Pa_AbortStream, or as a result of a return value other than paContinue
   --  from the stream callback. In the latter case, the stream is considered
   --  inactive after the last buffer has finished playing.
   --
   --  Return Value
   --    Returns one (1) when the stream is active (ie playing or recording
   --    audio), zero (0) when not playing or, a PaErrorCode (which are always
   --    negative) if PortAudio is not initialized or an error is encountered.
   --
   --  See Also: Pa_StopStream, Pa_AbortStream, Pa_IsStreamStopped
   function Pa_IsStreamActive (stream : PaStream) return PaError;
   pragma Import (C, Pa_IsStreamActive, "Pa_IsStreamActive");

   -----------------------------------------------------------------------------
   --  A structure containing unchanging information about an open stream.
   --  See Also: Pa_GetStreamInfo
   type PaStreamInfo is
      record
         --  this is struct version 1
         structVersion : aliased Integer;

         --  The input latency of the stream in seconds. This value provides
         --  the most accurate estimate of input latency available to the
         --  implementation. It may differ significantly from the
         --  suggestedLatency value passed to Pa_OpenStream. The value of this
         --  field will be zero (0.) for output - only streams.
         --
         --  See Also: PaTime
         inputLatency  : aliased PaTime;

         --  The output latency of the stream in seconds. This value provides
         --  the most accurate estimate of output latency available to the
         --  implementation. It may differ significantly from the
         --  suggestedLatency value passed to Pa_OpenStream. The value of this
         --  field will be zero (0.) for input - only streams.
         --  See Also: PaTime
         outputLatency : aliased PaTime;

         --  The sample rate of the stream in Hertz (samples per second). In
         --  cases where the hardware sample rate is inaccurate and PortAudio
         --  is aware of it, the value of this field may be different from the
         --  sampleRate parameter passed to Pa_OpenStream. If information about
         --  the actual hardware sample rate is not available, this field will
         --  have the same value as the sampleRate parameter passed to
         --  Pa_OpenStream.
         sampleRate    : aliased Long_Float;
      end record;
   pragma Convention (C, PaStreamInfo);

   type PaStreamInfo_Ptr is access all PaStreamInfo;
   pragma Convention (C, PaStreamInfo_Ptr);

   -----------------------------------------------------------------------------
   --  Pa_GetStreamInfo
   --
   --  Retrieve a pointer to a PaStreamInfo structure containing information
   --  about the specified stream.
   --
   --  Return Value
   --    A pointer to an immutable PaStreamInfo structure. If the stream
   --    parameter invalid, or an error is encountered, the function returns
   --    null.
   --
   --  Parameters
   --    stream A pointer to an open stream previously created with
   --    Pa_OpenStream.
   --
   --  Note
   --    PortAudio manages the memory referenced by the returned pointer, the
   --    client must not manipulate or free the memory. The pointer is only
   --    guaranteed to be valid until the specified stream is closed.
   --
   --  See Also: PaStreamInfo
   function Pa_GetStreamInfo (stream : PaStream)
                              return access PaStreamInfo;
   pragma Import (C, Pa_GetStreamInfo, "Pa_GetStreamInfo");

   -----------------------------------------------------------------------------
   --  Pa_GetStreamTime
   --
   --  Returns the current time in seconds for a stream according to the same
   --  clock used to generate callback PaStreamCallbackTimeInfo timestamps. The
   --  time values are monotonically increasing and have unspecified origin.
   --
   --  Pa_GetStreamTime returns valid time values for the entire life of the
   --  stream, from when the stream is opened until it is closed. Starting and
   --  stopping the stream does not affect the passage of time returned by
   --  Pa_GetStreamTime.
   --  This time may be used for synchronizing other events to the audio stream,
   --  for  example synchronizing audio to MIDI.
   --
   --  Return Value
   --    The stream's current time in seconds, or 0 if an error occurred.
   --
   --  See Also: PaTime, PaStreamCallback, PaStreamCallbackTimeInfo
   function Pa_GetStreamTime (stream : PaStream) return PaTime;
   pragma Import (C, Pa_GetStreamTime, "Pa_GetStreamTime");

   -----------------------------------------------------------------------------
   --  Pa_GetStreamCpuLoad
   --
   --  Retrieve CPU usage information for the specified stream.
   --  The "CPU Load" is a fraction of total CPU time consumed by a callback
   --  stream's audio processing routines including, but not limited to the
   --  client supplied stream callback. This function does not work with
   --  blocking read/write streams.
   --
   --  This function may be called from the stream callback function or the
   --  application.
   --
   --  Return Value
   --    A floating point value, typically between 0.0 and 1.0, where 1.0
   --    indicates that the stream callback is consuming the maximum number of
   --    CPU cycles possible to maintain real-time operation. A value of 0.5
   --    would imply that PortAudio and the stream callback was consuming
   --    roughly 50% of the available CPU time. The return value may exceed
   --    1.0. A value of 0.0 will always be returned for a blocking read/write
   --    stream, or if an error occurs.
   function Pa_GetStreamCpuLoad (stream : PaStream) return Long_Float;
   pragma Import (C, Pa_GetStreamCpuLoad, "Pa_GetStreamCpuLoad");

   -----------------------------------------------------------------------------
   --  Pa_ReadStream
   --
   --  Read samples from an input stream. The function doesn't return until
   --  the entire buffer has been filled - this may involve waiting for the
   --  operating  system to supply the data.
   --
   --  Parameters
   --    stream
   --      A pointer to an open stream previously created with Pa_OpenStream.
   --
   --    buffer
   --      A pointer to a buffer of sample frames. The buffer contains samples
   --      in the format specified by the inputParameters.sampleFormat field
   --      used to open the stream, and the number of channels specified by
   --      inputParameters.numChannels. If non-interleaved samples were
   --      requested, buffer is a pointer to the first element of an array of
   --      non-interleaved buffer pointers, one for each channel.
   --
   --    frames
   --      The number of frames to be read into buffer. This parameter is not
   --      constrained to a specific range, however high performance
   --      applications will want to match this parameter to the
   --      framesPerBuffer parameter used when opening the stream.
   --
   --  Return Value
   --    On success PaNoError will be returned, or PaInputOverflowed if input
   --    data was discarded by PortAudio after the previous call and before this
   --    call.
   function Pa_ReadStream (stream : PaStream;
                           buffer : System.Address;
                           frames : IC.unsigned_long)
                           return PaError;
   pragma Import (C, Pa_ReadStream, "Pa_ReadStream");

   -----------------------------------------------------------------------------
   --  Pa_WriteStream
   --
   --  Write samples to an output stream. This function doesn't return until the
   --  entire buffer has been consumed - this may involve waiting for the
   --  operating system to consume the data.
   --
   --  Parameters
   --    stream
   --      A pointer to an open stream previously created with Pa_OpenStream.
   --
   --    buffer
   --      A pointer to a buffer of sample frames. The buffer contains samples
   --      in the format specified by the outputParameters.sampleFormat field
   --      used to open the stream, and the number of channels specified by
   --      outputParameters.numChannels. If non-interleaved samples were
   --      requested, buffer is a pointer to the first element of an array of
   --      non-interleaved buffer pointers, one for each channel.
   --
   --    frames
   --      The number of frames to be written from buffer. This parameter is not
   --      constrained to a specific range, however high performance
   --      applications will want to match this parameter to the framesPerBuffer
   --      parameter used when opening the stream.
   --
   --  Return Value
   --    On success PaNoError will be returned, or paOutputUnderflowed if
   --    additional output data was inserted after the previous call and before
   --    this call.
   function Pa_WriteStream (stream : PaStream;
                            buffer : System.Address;
                            frames : IC.unsigned_long)
                            return PaError;
   pragma Import (C, Pa_WriteStream, "Pa_WriteStream");

   -----------------------------------------------------------------------------
   --  Pa_GetStreamReadAvailable
   --
   --  Retrieve the number of frames that can be read from the stream without
   --  waiting.
   --
   --  Return Value
   --    Returns a non-negative value representing the maximum number of frames
   --    that can be read from the stream without blocking or busy waiting or,
   --    a PaErrorCode (which are always negative) if PortAudio is not
   --    initialized or an error is encountered.
   function Pa_GetStreamReadAvailable (stream : PaStream)
                                       return IC.long;
   pragma Import (C, Pa_GetStreamReadAvailable, "Pa_GetStreamReadAvailable");

   -----------------------------------------------------------------------------
   --  Pa_GetStreamWriteAvailable
   --
   --  Retrieve the number of frames that can be written to the stream without
   --  waiting.
   --
   --  Return Value
   --    Returns a non-negative value representing the maximum number of frames
   --    that can be written to the stream without blocking or busy waiting or,
   --    a PaErrorCode (which are always negative) if PortAudio is not
   --    initialized or an error is encountered.
   function Pa_GetStreamWriteAvailable (stream : PaStream)
                                        return IC.long;
   pragma Import (C, Pa_GetStreamWriteAvailable, "Pa_GetStreamWriteAvailable");

   -----------------------------------------------------------------------------
   --  Pa_GetSampleSize
   --
   --  Retrieve the size of a given sample format in bytes.
   --
   --  Return Value
   --    The size in bytes of a single sample in the specified format, or
   --    paSampleFormatNotSupported if the format is not supported.
   function Pa_GetSampleSize (format : PaSampleFormat)
                              return Integer;
   pragma Import (C, Pa_GetSampleSize, "Pa_GetSampleSize");

   -----------------------------------------------------------------------------
   --  Pa_Sleep
   --
   --  Put the caller to sleep for at least 'msec' milliseconds. This function
   --  is provided only as a convenience for authors of portable code (such as
   --  the tests and examples in the PortAudio distribution.)
   --
   --  The function may sleep longer than requested so don't rely on this for
   --  accurate musical timing.
   procedure Pa_Sleep (msec : Integer);
   pragma Import (C, Pa_Sleep, "Pa_Sleep");

   -----------------------------------------------------------------------------
   --  Ada Utilities  ----------------------------------------------------------
   -----------------------------------------------------------------------------

   function Pa_GetHostIndex (name : String) return PaHostApiIndex;

   function Pa_GetDeviceIndex (name : String) return PaDeviceIndex;

end PortAudioAda;
