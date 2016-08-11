# ada-dab
an ada implementation of a dab decoder

Just to freshen up my ada knowledge (after a period of over 20 years) a - simplified - version of the dab software
was encoded in Ada. The software is - as always - a version 0.0x, however, on my PC it does work with the dabstick, the SDRplay and the Airspy.
For compiling the program you will need
a. Ada (preferably Gnat)
b. GtkAda for the very, very simple GUI
c. libraries for portaudio, fftw3, rtlsdr and faad

The GPR file, for generating an exectable is in the main directory of the
sources. Settings are for including a library for the airspy and
for the SDRplay. Just adapt to your needs.

