.
. steve.pls - fka js.pls
.
SEQ	  FORM	    "-1"
N0	  FORM	    "0"
N1	  FORM	    "1"
N2	  FORM	    "2"
TRAP	  FORM	    1
WRESULT	  FORM	    9
.
. mail-merge stuff
.
MyApp	  Automation Class="Word.Application"
Doc	  Automation
Docs	  Automation
MyMerge	  Automation
MyDataSource Automation
.
NAME_VAR  DIM	    200
WAAF1	  INTEGER   1
FILE_NAME DIM	   200
F_SOURCE  INIT	   "C:\work\data.dat"
F_MAINDOC INIT	   "c:\work\maindoc.doc"
.F_SOURCE  INIT	   "s:\eagle\data.dat"
.F_MAINDOC INIT	   "s:\eagle\maindoc.doc"
COL_HEADS INIT	    "field1,field2,field3,field4,field5"
ONE	  INTEGER   1,"1"
ZERO	  INTEGER   1,"0"
AUTOTRUE  INTEGER   4,"0xffffffff"
LASTREC	  INTEGER   4,"0xfffffff0"
FIRSTREC  INTEGER   4,"1"
DESTIN	  INTEGER   4,"1"
FILE	  FILE
.
STEVE	  DISPLAY   *ES;
	  CALL	    PROCESS
	  STOP
.
.
PROCESS
	  LOOP
 
	     MOVE      N0,TRAP
	     TRAP      THE_TRAP	IF OBJECT
	     Create    MyApp
.
	     IF	       (TRAP)
		alert  caution,"MS Word	97 or later is not installed":
		       WRESULT,"Help Message"
		BREAK
	     ENDIF
	     TRAPCLR   OBJECT
 
	     Getprop   MyApp,*Application=Docs
.
..... see if document is free
	     PACK      FILE_NAME WITH F_MAINDOC
	     MOVE      N0,TRAP
	     TRAP      THE_TRAP	IF IO
	     OPEN      FILE,FILE_NAME,EXCLUSIVE
	     IF	       (TRAP)
	      alert    caution,"Mail-merge document cannot be found":
		       WRESULT,"Help Message"
	      MyApp.Quit using 0
	      Destroy  MyApp
	      BREAK
	     ENDIF
	     CLOSE     FILE
.
. see if source	data ok
 
	     PACK      FILE_NAME WITH F_SOURCE
	     PREPARE   FILE,FILE_NAME
	     WRITE     FILE,SEQ;COL_HEADS
	     IF	       (TRAP)
	      ALERT    CAUTION,"Data Source is not available.":
		       WRESULT,"Help Message"
	      MyApp.Quit USING 0
	      Destroy  MyApp
	      BREAK
	     ENDIF
	     BREAK
	  REPEAT
	  IF	    (TRAP)
	     RETURN
	  ENDIF
.
.
	  WRITE	    FILE,SEQ;*cdfon,"KATANA THE	RAT","BBBBB","CCCCC","DDDDD","EEEEE"
	  WRITE	    FILE,SEQ;*cdfon,"GGGGG","HHHHH","IIIII","JJJJJ","KKKKK"
	  WEOF	    FILE,SEQ
	  CLOSE	    FILE
.
	  PACK	    FILE_NAME WITH F_MAINDOC
..... object trap if no	records	in data	source file
	  MOVE	    N0,TRAP
	  TRAP	    THE_TRAP IF	OBJECT
	  docs.Documents.Open Giving Doc Using FILE_NAME
	  IF	    (TRAP = 0)
.
	     Getprop   Doc,*mailmerge=Mymerge
	     Getprop   MyMerge,*datasource=MyDataSource
	     Getprop   MyDataSource,*name=name_var
	     Getprop   MyMerge,*state=waaf1
.
	     IF	       (WAAF1 != 2)
	     ALERT     CAUTION,"Invalid	mail-merge/datasource document ":
		       WRESULT,"Help"
	     ELSE
		setprop	  MyDataSource,*firstrecord=firstrec:
			  *lastrecord=lastrec
		setprop	  MyMerge,*destination=destin
		MOVE	  N0,TRAP
		TRAP	  THE_TRAP IF OBJECT
		MYMERGE.EXECUTE	using ONE	(TRUE=DISPLAY DIALOG)
	     ENDIF
	  ENDIF
	  MyApp.Quit  using 0
	  Destroy   MyApp
	  RETURN
.
. Trap Handler
THE_TRAP MOVE	   N1,TRAP
	 RETURN
