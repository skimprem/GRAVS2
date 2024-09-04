INSTALLATION OF GRAVS2

Next parts are just recommentation for successful installation. There could be other
possible ways more suitable for user to install GRAVS2.


1) Create GRAVS2 folder to the suitable location with permission to do that.
In LINUX, user's home folder is generally good location for that. In WINDOWS, it might 
be more tricky, especially without administrator rights. Anyway, it is not recommended 
to create GRAVS2 folder to drive C (root folder) containing Win OS installation. 
A better place would be somewhere in user's home folder (C:\Users\<username>\),
but then again other users have no access to it. For computer with multiple users 
a separate user 'gravs2' can be made with common password for all specialist doing 
gravity data analysis.
Also the path with spaces (eg C:\Users\<username>\My Documents) is not recommended for 
GRAVS2 installation. Separate parition (D:\,E:\,..), physical drive or even memory stick 
could be good location for GRAVS2 with full access by user. After folder creation unpack 
the content of GRAVS2 zip file there.


2) To use GRAVS2 command line tools in different data folders (made for separate 
campaigns, projects, days etc), the path to GRAVS2 binaries and shared input files
should be defined. 

In LINUX .bashrc text file in user's home folder can be modified
to add GRAVS2 paths (search instructions on the Internet).

In WINDOWS 'Environment Variables' for user (or system) can be modified by carefully
following instructions on the Internet. As a result 'Path' variable get additional
line, eg 'D:\GRAVS2\bin' and 'D:\GRAVS2\share' (also 'D:\GRAVS2' for 'GRAVS2.cmd', see
below). However, without administrator rights it might be impossible to modify 'Path'.
Then the options are:
i) The 'Path' is modified by IT-specialist with administrator rights
ii) User can execute GRAVS2 terminal 'GRAVS2.cmd'
iii) User modifies script files reduce.bat, adjust.bat by adding abs path to binary files
     (*.exe) and then copy scripts to data folder for processing.

Option ii) works quite well if path to 'GRAVS2.cmd' is short (eg 'X:\GRAVS2.cmd' where 
X = D, E, ...). Then by opening file explorer, navigating to data processing folder and 
typing 'D:\GRAVS2' to the address bar GRAVS2 terminal is opened (with GRAVS2 paths properly 
defined) to start processing of data files in current folder. However, user should check 
and edit (with e.g. notepad) the variable 'gr2main' in 'GRAVS2.cmd'.

NB!
a) After unpacking of GRAVS2 files a test batch 'test_GRAVS2_bin.cmd' in folder .\testdata can 
   be run to test binaries. For further testing (of GRAVS2 successful installation) and better 
   understanding (of GRAVS2) the examples (in folder ..\GRAVS2\data\Examples) should be followed 
   and tried to process.
b) Few batch scripts need additional freely available software like GMT (Generic Mapping Tools, 
   www.generic-mapping-tools.org), gawk (GNU awk), 7-Zip (www.7-zip.org)

GRAVS2 folders:
\bin - executables and script files
\data\example - several examples with real data
\doc - pdf manual
\share - data and parameter files
\source - programs source codes
\testdata - for quick GRAVS2 testing purposes


3) COMPILATION OF SOURCE CODE*
*Compiled binaries for 32-bit win10 (win7,..,win xp) and 64-bit for ubuntu are included.

The code can be compiled (by making exe file) by using FOSS* software like gfortran 
(both in WIN and LINUX), or older g77 compiler (not developed anymore). 
* FOSS - Free and open-source software

From the web page http://force.lepsch.com/p/download.html a software 
"Force 2.0" can be downloaded - it offers simple IDE (editor, g77 compiler
and debugger) to modify and compile GRAVS2 codes IN WINDOWS env only (32-bit
and 64-bit, works also in WIN10). "Force 2.0" is free (but with closed 
source) and is also not developed anymore (no official webpage exists).
It is recommended to use version 
"Force 2.0.9 + GNU Fortran 77 (G77)" Force209G77Setup.exe (2.03MiB)

COMPILATION HINTS:
To compile with gfortran, the command line examples are (where subfolder 
/include contains subroutines for INCLUDE statements):
gfortran -std=legacy -o Grredu303.e Grredu303.f -Iinclude/
gfortran -std=legacy -o Gradj305.e Gradj305.f -Iinclude/

In "Force 2.0" choose "Run -> Compilation options" and fill "Libraries:" 
with path to your subfolder /include containing subroutines,
eg "-Id:\GRAVS2\include".

The other programs can  be compiled in a similar way:
gfortran -std=legacy -o GTPAR.e GTPAR.f -I../include/
gfortran -std=legacy -o CG5form2f.e CG5form2f.f -I../include/
gfortran -std=legacy -o tform12.e tform12.f -I../include/
gfortran -std=legacy -o tform22.e tform22.f -I../include/
gfortran -std=legacy -o Wzz21.e Wzz21.f -I../include/

There are also 64-bit binaries of GRREDU3, GRADJ3 for WIN10 compiled via Windows Subsystem for Linux (WSL):
x86_64-w64-mingw32-gfortran -std=legacy -o Grredu303_x64.exe Grredu303.f -static -s -O2 -Iinclude/
x86_64-w64-mingw32-gfortran -std=legacy -o Gradj305_x64.exe Gradj305.f -static -s -O2 -Iinclude/

For large projects (eg nationwide network adjustment) the usage of *_x64.exe binaries could be advantageous (results shorter processing time etc).
Test computation with Estonian gravity network data (about 23.5k readings) showed that processing time shortened from 40 s to 10 s (4x).

Compiled by
Tõnis Oja
2022-01-06
