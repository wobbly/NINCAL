..testjul
...this is a quick program by Robb Whiting to test what date CVTJUL should put out given a certain day

PC EQU 0

.Include some files

           Include Common.inc
           Include Cons.inc           
           
           
Release       Init      "1.00"    RVW  Initial release
reldate       Init      "2015 October 6th"
           
Main           
           Call Paint


           
           Call CvtJul
           
           
           
           Pause "15"
           Stop


           Include Comlogic.inc