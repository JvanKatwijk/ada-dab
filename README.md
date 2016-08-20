# ada-dab
an ada implementation of a dab decoder

Just to freshen up my knowledge of the Ada programming langage (after a period of over 20 years) a - simplified - version of the dab software
was encoded in Ada. The software is - as always - a version 0.0x, however, on my PC it does work with the dabstick, the SDRplay and the Airspy.

For compiling the program you will need
a. Ada (preferably Gnat)
b. GtkAda for the very, very simple GUI
c. libraries for portaudio, fftw3, rtlsdr and faad
d. libraries for the device you want to have supported.

The GPR file, for generating an executable is in the main directory of the
sources. Settings are for including a library for the airspy and
for the SDRplay. Just adapt to your needs.

It is a little primitive right now, but for selecting an input device
one needs to adapt the gui/gui.ads file

a. uncomment the "with XXX"  for the package handling the input device
   and comment out the others
b. uncomment the appropriate line 
--	package myDevice renames rtlsdr_wrapper;
--	package myDevice renames airspy_wrapper;
	package myDevice renames sdrplay_wrapper;
c. add/remove the library names in the build file.

