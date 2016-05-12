........................................
. Program:      PRINTER.PLS
. Function:     Tester Program for printers
. Author:       Andrew Harkins
. Date:         July 27,1998
. Release:      1.0
........................................
.01/19/99

.code which displays available printers.  Code
.acquired through Sunbelt.

.ASH
PC      EQU     1
.Include Files
        include common.inc
        include cons.inc

.Var Used In order To Load Includes
release dim     10

.................
x       plform  printer
        winhide
.Load Forms, Always declare parent form first
        formload x
                
        setprop PrinterDataList,printer=1
MainLoop
        loop
                waitevent
        repeat
        

.Include IO Files
        include comlogic.inc               
