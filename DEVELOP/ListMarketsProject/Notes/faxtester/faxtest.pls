........................................
. Program:      order.PLS
. Function:     Order Report Program (development)
. Author:       Andrew Harkins
. Orig. Date:   December 2,1998
. Release:      1.0
........................................

PC      EQU     0
.Include Files
        include common.inc
        include cons.inc

prfile  pfile
howmany form    9
result  form    9
release init    "1.0"   ASH 02DEC98  DEVELOPMENT RELEASE

x       plform  faxtest
        winhide

.Load Forms, Always load parent form first
        formload x
.pict1   pict

.        CREATE  PICT1=1:15:1:10:
.                "c:\progra~1\nincal\HEADER.BMP",AUTOZOOM
 
        loop
                waitevent
        repeat
        
ViewFile
        trap    noprint if spool
        prtplay "","@"
.        clear   taskname
.        append  "c:\work\",taskname
.        append  filename,taskname
.        reset   taskname
.        PRTPLAY taskname,"@",jobname=filename
noprints  return
                             
.Include IO file
        include comlogic.inc

