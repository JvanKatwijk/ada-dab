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


The GPR file, for generating an executable is in the main directory of the
sources. Settings are for including a library for the airspy and
for the SDRplay. Just adapt to your needs.

-------------------------------------------------------------------------
-------------------------------------------------------------------------
Some notes on the program

The overall structure of the program is pretty straightforward:
we have samples being read-in by an appropriate handler, the
ofdm handler does the ofdm things, i.e. synchronize time and frequency,
and building up of the ofdm frames.
The first three data blocks of each ofdm frame are handed over to the FIC
handler, the remaining ones are handed over to the MSC handler.

While the GUI for the C++ version of the program are created using the
Qt framework, the Ada version of the program uses the gtkAda library
for setting up the (simplified) GUI.
The GUI is simplified in that there is no selection of the input device,
no selection for the Mode nor for the Band, and the options for saving
output into a file are gone. The selections left are - apart from start
and stop - selecting a channel, selecting a program (once the ensemble
transmitted through the channel is identified), and selecting the gain
for the input device.

The input device cannot be selected (the only reason actually being that if
the input device could have been selected by e.g. a command line parameter,
all code for handling the devices would be included. That code makes
use of "development files", in C, linking the Ada handler code to the
C libraries. At least the "development files" for the devices then 
should be present.), however, by convention, the package implementing
the device handler provides functions for reading samples and for determining
how many samples are currently available.

The Mode may be set in the command line and therefore does not change
during program execution. Given that the "Mode" is known,
the relevant parameters for the ofdm coding, for the FIC decoding
and the set up of the MSC decoding are known. 

The main packages implementing the ofdm handling, the FIC and the
MSC handling are therefore made into generic ones. Instantiation takes place
in the "main" program, after detecting the mode.

	   package my_mscHandler is new msc_handler (the_Mode);
	   package my_ficHandler is new fic_handler (the_Mode);
	   package my_ofdmHandler is
	                  new ofdm_handler (The_Mode,
	                                    my_Device.     Get_Samples,
	                                    my_Device.     Available_Samples,
	                                    my_mscHandler. process_mscBlock,
	                                    my_ficHandler. process_ficBlock,
	                                    my_ficHandler. Sync_Reached);


The ofdm handler then needs the parameters as given above, functions for
reading samples and functions for handing over the data blocks of the frame.

One issue arises with this setup. The GUI functions, that are activated
in Callbacks when selecting items on the GUI, need access to the interfaces
of the packages mentioned above. E.g., selecting a program from
the current ensemble requires access to the FIC package to obtain specific
data on where to find and to decode the program, and the the MSC package
to instruct the MSC handler how to fetch the data from the incoming data stream
and how to decode it.

This implies that the functions implementing the call backs only can be
created after the elaboration of the packages.
Since the creation of the GUI is on library level, the scope of the GUI
elements is wider than the scope of the callbacks, which is not appreciated
by the language.

Gnat provides the opportunity of "Unrestricted_Access", which is 
dangerous, but applied here in the binding of the Callbacks to the
GUI elements.

	   Win.                On_Destroy
	                       (main_quit' Unrestricted_Access);
	   Channel_Selector.   On_Changed
	                       (Channelselector_clicked' Unrestricted_Access);
	   programSelector.    On_Changed
	                       (Programselector_clicked' Unrestricted_Access);
	   startButton.        On_Clicked
	                       (start_clicked' Unrestricted_Access);
	   Gain_Selector.      On_Changed
	                       (Gainselector_clicked' Unrestricted_Access);
	   quitButton.         On_Clicked
	                       (button_quit'  Unrestricted_Access);

The obvious solution seems to be to create the GUI only after having
instantiated the packages. However, since many parts of the program
send data to the GUI for display, it seems that the GUI should
be created on library level.

So, there is something to puzzle.
--------------------------------------------------------------------------
--------------------------------------------------------------------------

Synchronous vs asynchronous communication.

Both the FIC and the MSC handler get their data input from the
ofdm handler (see above). The FIC handler will get the first three
data blocks from an ofdm frame, the MSC handler the remaining ones 
(over 70 for a Mode 1 transmission).

The GUI allows the user to interfere in the processing: a different
program may be selected or a different channel may be selected
(note that setting the gain to a different value only affects
the input device handling package).

Both FIC and MSC handlers are implemented as (hidden) tasks, which allows
concurrent processing. Different tasks may be run on different cores
of the CPU, potentially leading to a better balancing of the load.

The original implementation used the rendez-vous mechanism for passing on
the data to the FIC and MSC hander, as well as for exchanging data between
the GUI and the handlers.

An issue that arose was that the CPU load of the FIC and the MSC handlers
is not uniformly distributed: at some points the FIC needs quite some
processing power for a short while, at some other points the MSC
is a heavy CPU user.

Since the ofdm handler  - which is implemented as a hidden task -
potentially may be blocked by synchronous data transfer, the second
implementation uses different forms of communication between
the components.

It must be noted that the ofmd handler is the real CPU burner of
the program: it processes 2048000 complex samples/second and does lots of
interpretation on the data.
Just the function Get_Samples, responsible for fetching data from a device
and adjusting the phase of each of the samples according to the
computed coarse and fine frequency offsets, takes on average 30 % of the load.

It is therefore undesirable that the ofdm handler is blocked
when passing on data to the FIC and the MSC handler.

In the second implementation,
a complete asynchronous form was chosen for the FIC handler, invisible
for the clients though.
A buffer - implemented as a regular protected object - was created and
the FIC handler basically executes a loop to fetch a data element
from the buffer and execute what is to be done.

While this is obvious for the data transfer, i.e. the receiving function just
puts the data into the buffer, the "commands" from the GUI also have
to be made into data for buffer elements (Note that commands do not
have a higher priority than the data has).

	procedure Restart is
	   command : fic_data;
	begin
	   command. command    := Restart_Command;
	   command. ficNo      := 0;
	   command. ofdm_data  := (Others => 0);
	   the_ficBuffer. Put (command);
	end Restart;

While for the Restart and Stop commands this is quite simple,
one command is not easily transformed into a push of data into the
buffer though. When a user selects a program, the callback of the GUI involved
will ask the FIC handler for the parameters, describing the program.
This command then is implemented as a direct call into the FIC data.

	procedure Data_for_Audioservice (Name_of_Program : String;
	                                 Data            : out audioData) is
	begin
	   fic_Locker. lock;
	   fib_handler. Data_for_AudioService (Name_of_Program, Data);
	   fic_Locker. unlock;
	end Data_for_AudioService;

Obviously, since different tasks are manipulating the data,
additional locking is needed.


For the MSC handler a different approach was taken. Since the issue was
the blocking of the ofdm handler by the FIC and/or the MSC handler rather
than the other way around, there is as such no need to change the MSC handler
as long as it can be ensured that the MSC handler does not block the ofdm
handler.

An intermediate task, just handling a buffer, was created

	task body helper is
	   Element : buffer_Element;
	begin
	   loop
	      the_mscBuffer. Get (Element);
	      msc_processor. process_mscBlock (Element);
	   end loop;
	end;

and the interface function does not do more than putting the
data into the buffer.

	procedure process_mscBlock	(Fbits	: shortArray;
	                                 Blkno	: Integer) is
	   Element : buffer_Element := (Blkno, FBits);
	begin
	   the_mscBuffer. Put (Element);
	end process_mscBlock;

Now, a new issue arises, how to stop the tasks at program termination.
For the approach taken for the FIC handling, this is easy: just a
command for the task to commit suicide suffices. For the intermediate
task in the MSC handler we just commit a plain murder by an abort statement.

