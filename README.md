# ada-dab
an ada implementation of a dab decoder

Just to freshen up my knowledge of the Ada programming langage (after a period of over 20 years) a - simplified - version of the dab software
was encoded in Ada.  The Ada version differs in some architectural points
from the sdr-j-dab C++ version. 
The Ada software is - as always - a version 0.0x, however, on my PC it does work with the dabstick, the SDRplay and the Airspy.

For compiling the program you will need
a. Ada (preferably Gnat)
b. GtkAda for the very, very simple GUI
c. C libraries for portaudio, fftw3, rtlsdr and faad
d. C libraries for the device you want to have supported.

The implementation contains "handlers" for the devices mentioned,
interfacing the Ada code to the C libraries.
It is a little primitive right now, but for selecting an input device
one needs to adapt the main.adb file and comment or uncomment the
line indicating the package for supporting the preferred device.

a. uncomment the "with XXX"  for the package handling the input device
   in the fi
   and comment out the others
b. uncomment the appropriate line 
--	package myDevice renames rtlsdr_wrapper;
--	package myDevice renames airspy_wrapper;
	package myDevice renames sdrplay_wrapper;
c. add/remove the library names in the build file.

The GPR file, for generating an executable is in the main directory of the
sources. Settings are for including a library for the airspy and
for the SDRplay. Just adapt to your needs.

