.program opens .DCX files and searches through pages
.program only works on two consecutive .dcx FILES listed
.in .smp file.  Look at logic regarding NORETURN's in faxtester2.plf
.for solution for muliple or no files
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

DCX     dim     30
DCXFile dim     120
FilePath dim   120
.................
.taskname        dim     120
.
.        clear   N9
..Create Sorted Version of File on the Fly
.        clear   taskname
.        append  "\NETUTILS\SORT g:\DATA\TEXT\DIRECT.DAT,g:\DATA\TEXT\DIRECT.SRT" to taskname
.        append  " /S(1,25,C,A)" to taskname
.        reset   taskname
.        execute taskname
.        clear   taskname
.................

x       plform  faxtester
        winhide
.Load Forms, Always declare parent form first
        formload x
                
        move    "F:\data\samples\s0004001.dcx",dcxfile

        clear   N9
        move    C1,N9
        CREATE  faxtester;PICT1=70:470:100:500:                              
                DCXFile,BORDER,SCROLLBAR,AUTOZOOM,PAGE=N9             
        activate PICT1
        

MainLoop
        loop
                waitevent
        repeat
        
Next    TRAP    ERRORMSSG IF object
        GOTO    ERRORMSSG IF (N9 = 0) 
        CREATE  faxtester;PICT1=70:470:100:500:
        DCXFile,BORDER,SCROLLBAR,AUTOZOOM,PAGE=N9
        activate pict1
        trapclr object
        RETURN

ERRORMSSG
.        ALERT   CAUTION,"No More Pages To Print!",N2
        CREATE  faxtester;PICT1=70:470:100:500:
        DCXFile,BORDER,SCROLLBAR,AUTOZOOM,PAGE=N8
        move    N8,N9
        activate pict1
        return

   
.Include IO Files
        include comlogic.gui                
