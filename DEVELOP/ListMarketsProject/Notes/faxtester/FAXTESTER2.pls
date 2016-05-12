........................................
. Program:      FAXTESTER.PLS
. Function:     Tester Program for .DCX files
. Author:       Andrew Harkins
. Date:         July 27,1998
. Release:      1.0
........................................

PC      EQU     1
.Include Files
        include common.inc
        include cons.inc

.Var Used In order To Load Includes
release dim     10

.Vars for .dcx file
pict1   pict
file1   pfile
FILE2   FILE

FirstFlag init  "Y"

DCX     dim     30
DCXFile dim     120
SPOOLF  dim     120
FilePath init   "F:\data\samples\"
.................
x       plform  faxtester2
        winhide
.Load Forms, Always declare parent form first
        formload x
                
        move    "f:\library\develop\drooltest.prn",SPOOLF
        open    file2,"f:\library\develop\sample.smp"        

MainLoop
        loop
                waitevent
        repeat
        
Next    TRAP    ERRORMSSG IF object
        GOTO    ERRORMSSG IF (N9 = 0) 
        CREATE  faxtester2;PICT1=70:470:100:500:
        DCXFile,BORDER,SCROLLBAR,AUTOZOOM,PAGE=N9
        activate pict1
        trapclr object
        RETURN

ERRORMSSG
.        ALERT   CAUTION,"No More Pages To Print!",N2
        CREATE  faxtester2;PICT1=70:470:100:500:
        DCXFile,BORDER,SCROLLBAR,AUTOZOOM,PAGE=N8
        move    N8,N9
        activate pict1
        return

   
.Include IO Files
        include comlogic.gui                
