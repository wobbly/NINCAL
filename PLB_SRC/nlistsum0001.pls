PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc

RELEASE  INIT      "1.0"       DMB 05MAR02  initial release for a dialog box for the title of listsumv

        MOVE    "NLISTSUM0001" to PROGRAM
        MOVE    "Names in the News Ca Inc" TO COMPNME
        move    "NLISTSUM0001.PLS",Wprognme
        move    "Title for Summary Report",Wfunction
        move    "David Baca",Wauthor
        move    "1.0",Wrelease
        move    "March 05, 2002",Wreldate


abt     plform  About
x       plform  NLISTSUM0001

        winhide
        formload x
        formload abt

          setfocus NLISTSUMEditTitle0001
.====================================================================================
        loop
        waitevent
        repeat
.====================================================================================
START1
        call     OrderSetMouseBusy
        getitem  NLISTSUMEditTitle0001,0,str50
        pack     str55,"'",str50,"'"
        pack    taskname,"\\Nins1\winbatch\testlistsumv.exe ",comment,b1,"b=",user,b1,"q=",str55
.;        pack    taskname,"f:\apps\winbatch\testlistsumv.exe ",comment,b1,"b=",user,b1,"q=",str55
.;        pack    taskname,"f:\apps\winbatch\testlistsumv.exe ",comment,b1,"b=",user,b1,"q=",str55
          execute taskname
          setprop NLISTSUM0001,visible=c0
.        pack     taskname with "!c:\progra~1\lanbatch\batch -X -SA -Qf:\lanbat~2 f:\apps\winbatch\butil job=listsumV infile=",comment," b=",user," levels=",str55
.        pack     taskname with "!c:\progra~1\lanbatch\batch -X -SA -Qf:\lanbat~2 f:\apps\winbatch\butil job=listsumV infile=",comment," b=",user," levels=",str55
.        pack     taskname with "!c:\progra~1\lanbatch\batch -X -SA -Qf:\lanbat~2 f:\apps\winbatch\butil job=listsumV infile=",comment," b=",user," c=1 prin=",func," levels=",str55
.        execute  taskname
        alert    Note,"Submitting Request!",result,"Done"
        call     OrderSetMouseFree
        winshow
        CHAIN     "NORD0006"
        stop

OrderSetMouseBusy
        setmode *mcursor=*wait
        return
OrderSetMouseFree
        setmode *mcursor=*arrow
        return

        INCLUDE   COMLOGIC.inc
