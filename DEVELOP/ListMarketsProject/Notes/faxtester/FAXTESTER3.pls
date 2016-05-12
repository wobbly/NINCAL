........................................
. Program:      FAXTESTER.PLS
. Function:     Tester Program for .DCX files
. Author:       Andrew Harkins
. Date:         December 1,1998
. Release:      1.1
........................................

PC      EQU     0
.Include Files
        include common.inc
        include cons.inc

.Var Used In order To Load Includes
release dim     10

.Vars for .dcx file
pict1   pict
file1   pfile
FILE2   FILE
FILE3   FILE

FirstFlag init  "Y"

DCX     dim     30
DCXFile dim     120
SPOOLF  dim     120
.FilePath init   "\\nts0\c\DATA\samples\"
FilePath init   "\\nts1\d\USERS\AHARKIN\"
.................
x       plform  faxtester3
        winhide
.Load Forms, Always declare parent form first
        formload x
                
        move    "f:\data\fax\test\drooltest.prn",SPOOLF
        open    file2,"\\nts0\c\library\develop\sample.smp"
        PREPARE FILE3,"\\nts1\d\USERS\AHARKIN\GOOBER.DAT"

MainLoop
        loop
                waitevent
        repeat
        
Next    TRAP    ERRORMSSG IF object
        GOTO    ERRORMSSG IF (N9 = 0) 
        CREATE  faxtester3;PICT1=70:470:100:500:
        DCXFile,BORDER,SCROLLBAR,AUTOZOOM,PAGE=N9
        activate pict1
        trapclr object
        RETURN

ERRORMSSG
.        ALERT   CAUTION,"No More Pages To Print!",N2
        CREATE  faxtester3;PICT1=70:470:100:500:
        DCXFile,BORDER,SCROLLBAR,AUTOZOOM,PAGE=N8
        move    N8,N9
        activate pict1
        return

   
.Include IO Files
        include comlogic.INC                
