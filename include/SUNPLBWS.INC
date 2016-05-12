*==============================================================================
. Process for MENUX items for ANY MENU
.
SETBG
	 DISPLAY   *BGCOLOR=*WHITE,*FGCOLOR=*BLACK;
	 RETURN
RESETBG
	 DISPLAY   *BGCOLOR=*YELLOW,*FGCOLOR=*BLACK;
	 RETURN
.------------------------------------------------------------------------------
SETINP
	 MOVE	   "1",INPFLG
	 CREATE	   INPUTS=1:2:3:15,INPUTQ,"'>Courier New'(12)"
	 ACTIVATE  INPUTS
	 CREATE	   INPUTF=1:2:18:58,FONT="FIXED(12)",BORDER
         SETITEM   INPUTF,0,DIM40
	 RETURN
.------------------------------------------------------------------------------
SETOUT
	 MOVE	   "1",OUTFLG
	 CREATE	   OUTPUTS=3:4:3:15,"Output:  ","FIXED(12)"
	 ACTIVATE  OUTPUTS
	 CREATE	   OUTPUTF=3:4:18:58,FONT="FIXED(12)",BORDER
	 RETURN
.------------------------------------------------------------------------------
SETLEN
	 MOVE	   "1",LENFLG
	 CREATE	   LENS=5:6:3:15,"Length:  ","FIXED(12)"
	 ACTIVATE  LENS
	 CREATE	   LENF=5:6:18:58,FONT="FIXED(12)",BORDER
	 RETURN
.------------------------------------------------------------------------------
SETST
	 MOVE	   "1",STFLG
	 CREATE	   STARTS=5:6:3:15,"Start:  ","FIXED(12)"
	 ACTIVATE  STARTS
	 CREATE	   STARTF=5:6:18:58,FONT="FIXED(12)",BORDER
	 RETURN
.------------------------------------------------------------------------------
SETSPL
	 MOVE	   "1",SPLFLG
	 CREATE	   SPOOLS=7:8:3:15,"Spool:  ","FIXED(12)"
	 ACTIVATE  SPOOLS
	 CREATE	   SPOOLF=7:8:18:58,FONT="FIXED(12)",BORDER
	 RETURN
.------------------------------------------------------------------------------
SETTMP
	 MOVE	   "1",TMPFLG
	 CREATE	   TEMPS=7:8:3:15,"Temp:  ","FIXED(12)"
	 ACTIVATE  TEMPS
	 CREATE	   TEMPF=7:8:18:58,FONT="FIXED(12)",BORDER
	 RETURN
.------------------------------------------------------------------------------
SETOPT
	 MOVE	   "1",OPTFLG
	 CREATE	   OPTS=11:12:3:15,"Options:   ","FIXED(12)"
	 ACTIVATE  OPTS
	 CREATE	   OPTF=11:12:18:58,FONT="FIXED(12)",BORDER
	 RETURN
.------------------------------------------------------------------------------
.
SETOPTC
	 MOVE	   "2",OPTFLG
	 CREATE	   CGOPT1=9:24:05:40,CGOPT1D
	 CREATE	   CGOPT2=9:24:41:79,CGOPT2D
	 RETURN
.------------------------------------------------------------------------------
. Create OK Button to activate the utility operation.
.
SETOK
	 CREATE	   MNUXOK=3:4:62:71,"&OK",DEFAULT
	 ACTIVATE  MNUXOK,MNUXOK,RES
	 RETURN
.------------------------------------------------------------------------------
. Create CANCEL	Button to abort	the utility operation.
.
SETCAN
	 CREATE	   MNUXCAN=7:8:62:71,"&Cancel",CANCEL
	 ACTIVATE  MNUXCAN,MNUXCAN,RES
	 RETURN
.------------------------------------------------------------------------------
. Get input file name
.
MNUXINP
	 GETITEM   INPUTF,0,INPUT
	 RETURN
.------------------------------------------------------------------------------
. Get output file name
.
MNUXOUT
	 GETITEM   OUTPUTF,0,OUTPUT
	 RETURN
.------------------------------------------------------------------------------
MNUXLEN
	 GETITEM   LENF,0,LENFLD
	 RETURN
.------------------------------------------------------------------------------
MNUXST
	 GETITEM   STARTF,0,STFLD
	 RETURN
.------------------------------------------------------------------------------
MNUXTMP
	 GETITEM   TEMPF,0,TMPFLD
	 RETURN
.------------------------------------------------------------------------------
. Get print file name
.
MNUXSPL
	 GETITEM   SPOOLF,0,SPOOL
	 RETURN
.------------------------------------------------------------------------------
. Get options
.
MNUXOPT
	 GETITEM   OPTF,0,OPTIONS
	 RETURN
.------------------------------------------------------------------------------
. Process the OK button
.
MNUXOK
	 BRANCH	   MENUSEL,,MNU2OK,MNU3OK,,MNU5OK
	 RETURN
.------------------------------------------------------------------------------
. Process the Cancel button
.
MNUXCAN
	 MOVE	   "1",ABORT
	 CALL	   CLEANUP
	 CALL	   CLEANUP2
	 RETURN
.------------------------------------------------------------------------------
.
GENEXEC
	 IF ( INPFLG = 1 )
	   TYPE	     INPUT
	   RETURN    IF	EOS	       ;Nothing	to do if no input
	 ENDIF
.
	 CLEAR	   DIM40
	 UNPACK	   DIM40,S$CMDLIN
.
	 PACK	   S$CMDLIN:
		   UNAME," ":
		   INPUT
	 DISPLAY   *COLOR=0x04,*ES;    ;Set WHITE on BLACK
	 PACK	   DIM40,"PLB_NAME=",INPUT
	 CLIPSET   DIM40
.
	 CALL	   CLEANUP
	 CHAIN	   S$CMDLIN
.
	 CALL	   CLEANUP2
	 CHAIN	   "SUNPLBWS"
	 RETURN
.------------------------------------------------------------------------------
. Check	for pre-defined	name
.
GETCLIP
	 CLIPGET   DIM40
	 MATCH	   "PLB_NAME=",DIM40
	 IF EQUAL
	   RESET    DIM40,10
	   IF	    EOS
	     CLEAR     DIM40
	   ELSE
	     MOVE     DIM40,TEMP
	     COUNT    INT1,TEMP
	     SETLPTR  TEMP,INT1
	     MOVE     TEMP,DIM40
	   ENDIF
	 ELSE
	   CLEAR     DIM40
	 ENDIF
	 RETURN
.------------------------------------------------------------------------------
ACTALL
	 IF ( OPTFLG = 1 )
	   ACTIVATE  OPTF,MNUXOPT,MNUXRES
	 ELSE IF ( OPTFLG = 2 )
	   ACTIVATE  CGOPT1
	   ACTIVATE  CGOPT2,OPT2R,MNUXRES
	 ENDIF
	 IF ( SPLFLG = 1 )
	   ACTIVATE  SPOOLF,MNUXSPL,MNUXRES
	 ENDIF
	 IF ( TMPFLG = 1 )
	   ACTIVATE  TEMPF,MNUXTMP,MNUXRES
	 ENDIF
	 IF ( STFLG = 1	)
	   ACTIVATE  STARTF,MNUXST,MNUXRES
	 ENDIF
	 IF ( LENFLG = 1 )
	   ACTIVATE  LENF,MNUXLEN,MNUXRES
	 ENDIF
	 IF ( OUTFLG = 1 )
	   ACTIVATE  OUTPUTF,MNUXOUT,MNUXRES
	 ENDIF
	 IF ( INPFLG = 1 )
	   ACTIVATE  INPUTF,MNUXINP,MNUXRES
	 ENDIF
	 RETURN
.------------------------------------------------------------------------------
. Turn on menus
.
MENUSON
	 CALL	   ENAMNU2
	 CALL	   ENAMNU3
	 CALL	   ENAMNU4
	 CALL	   ENAMNU5
	 RETURN
.------------------------------------------------------------------------------
. Turn off menus
.
MENUSOFF
	 CALL	   DISMNU2
	 CALL	   DISMNU3
	 CALL	   DISMNU4
	 CALL	   DISMNU5
	 RETURN
.------------------------------------------------------------------------------
.
CLEANUP
	 DESTROY   MNUXOK
	 DESTROY   MNUXCAN
DEACTALL
	 CALL	   DEACT5
	 IF ( INPFLG = 1 )
	   DESTROY   INPUTS
	   DESTROY   INPUTF
	 ENDIF
	 IF ( OUTFLG = 1 )
	   DESTROY   OUTPUTS
	   DESTROY   OUTPUTF
	 ENDIF
	 IF ( LENFLG = 1 )
	   DESTROY   LENS
	   DESTROY   LENF
	 ENDIF
	 IF ( STFLG = 1	)
	   DESTROY   STARTS
	   DESTROY   STARTF
	 ENDIF
	 IF ( TMPFLG = 1 )
	   DESTROY   TEMPS
	   DESTROY   TEMPF
	 ENDIF
	 IF ( SPLFLG = 1 )
	   DESTROY   SPOOLS
	   DESTROY   SPOOLF
	 ENDIF
	 IF ( OPTFLG = 1 )
	   DESTROY   OPTS
	   DESTROY   OPTF
	 ELSE IF ( OPTFLG = 2 )
	   DESTROY   CGOPT1
	   DESTROY   CGOPT2
	 ENDIF
	 RETURN
.------------------------------------------------------------------------------
.
CLEANUP2
	 SETWTITLE TITLE
	 CLEAR	   INPUT
	 DISPLAY   *ES;
	 CREATE	   SUN,1,1,75
	 ACTIVATE  SUN
	 CALL	   MENUSON
	 RETURN
*==============================================================================
