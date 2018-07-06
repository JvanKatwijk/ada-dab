# ada-dab
an ada implementation of a dab decoder

Just to freshen up my knowledge of the Ada programming langage (after a period of over 20 years) a - simplified - version of the dab software
was encoded in Ada.  
Ada and C++ are different languages, languages with different idioms, there
are some minor architectural differences between the Ada and the C++ version(s).

![ada-dab with sdrplay input](/ada-dab.png?raw=true)

The ada-dab program, though functioning quite well with devices as
SDRplay, AIRspy and DABsticks, is not actively maintained: it was just an
exercise.

-------------------------------------------------------------------------

Compilation of the program

-------------------------------------------------------------------------

For compiling the program you will need
a. Ada (preferably Gnat)
b. GtkAda for the very, very simple GUI
c. C libraries for portaudio, fftw3, rtlsdr and faad
d. C libraries for the devices, rtlsdr, airspy and sdrplay unless
   you comment out the lines for the devices you do not want to support.

Note that the functioning also depends on some C libraries, for those
libraries, wrappers are included. Compilation of the system does require
compiling C functions.

The GPR file, for generating an executable is in the main directory of the
sources. Settings are for including a library for the airspy and
for the SDRplay. Just adapt to your needs.

On my laptop, building an executable is simply by

  gprbuild dab-receiver.gpr

-------------------------------------------------------------------------

Command line parameters

-------------------------------------------------------------------------

The Mode may be set in the command line with the -m  switch.
Choose among (Mode_1, Mode_2 and Mode_4).  Default is Mode 1,
The Band may be set in the command line with the -b switch,
Choose between BAND_III and L_BAND. Default is BAND_III.

The device may be set in the command line with the -d switch.
Choose among (raw, rtlsdr, sdrplay, airspy).

If you do not want to support any of these devices, comment the
appropriate line out in main.adb

	with rtlsdr_wrapper;    use rtlsdr_wrapper;
	with rawfiles;          use rawfiles;
	with airspy_wrapper;    use airspy_wrapper;
	with sdrplay_wrapper;   use sdrplay_wrapper;

and
comment the appropriate line(s) out in main.adb

	when 'd' =>
	   if Parameter = "raw" then
	     theDevice := new rawfiles. raw_device;
	     deviceName := "raw       ";
	   elsif parameter = "airspy" then
	     theDevice := new airspy_wrapper. airspy_device;
	     deviceName := "Airspy    ";
	   elsif Parameter = "sdrplay" then
	     theDevice := new sdrplay_wrapper. sdrplay_device;
	     deviceName := "SDRplay   ";
	   elsif Parameter = "rtlsdr" then
	     theDevice := new rtlsdr_wrapper. rtlsdr_device;
	     deviceName := "rtlsdr    ";
	   else
	     theDevice := new rawfiles. raw_device;
	     devicename := "raw       ";
	   end if;


-------------------------------------------------------------------------

Implementation description

-------------------------------------------------------------------------

A reasonably detailed description of the implementation is given in
"dab-ada.pdf".

