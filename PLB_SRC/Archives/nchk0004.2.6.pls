.PURPOSE - PRINT CHECK RECONCILIATION STATEMENT AFTER EACH CHECK RUN.
...............................................................................
PC       EQU       0
         INCLUDE   COMMON.inc
         INC       CONS.inc
         inc       hp.inc
.
Release   init     "2.6"               DLH Handle neg AP like the other check programs
Reldate	Init	"30 November 2007"
.Release   init     "2.5"               11Jul2007 DLH PLI
.Release   init     "2.4"               04OCT2000 ASH NEW SERVER ADDED
.Release   init     "2.3"               20aug99 DLH adj y2k .no  change nec?
.Release   init     "2.2"               25jun99 JD INV y2k
.Release   init     "2.1"               06Mar99 DLH apmask fix
.Release  init      "2.0"             11June98 DLH check for duplicate checks and print warning
.release  init      "1.5"             08Apr98 DLH Duplex
.release  init      "1.4"             17Dec96 change rpayor to rpayee
.RELEASE  INIT      "1.3"             23sep96 added flat file write NY.
.RELEASE  INIT      "1.2"            ADDED DSINIT 1/28/93 JOSE D.
.RELEASE  INIT      "1.1"           CONVERTED 5/6/92 JOSE D.
.CREATED 6JUN88. DH. 
.
* *****************************************************************************
* NAMES IN THE NEWS MASTER RECONN1 FILE.
* *****************************************************************************
.
.    FILE:      RECONN1
.  LENGTH:
.COMPRESS:      SPACE
.    TYPE:      VAR/RANDOM
.     KEY:
...............................................................................
.
RECON    FILE      
recfile  file     
.
RCMLR    DIM       4       1-4          MAILER #
RCCNT    DIM       3       5-7          CONTACT #
RCLRNO   DIM       6       8-13         LR #
RCCHEK   DIM       6      14-19         CHECK NUMBER
RCLONO   DIM       4      20-23         LIST OWNER NUMBER
RCACREC  FORM      10.2   24-36         ACCOUNTS RECEIVABLE
RCAPAY   FORM      10.2   34-49         ACCOUNTS PAYABLE
RCNNINC  FORM      9.2    44-61         NIN INCOME
RCLRINC  FORM      9.2    53-73         LR INCOME
RCSTAX   FORM      9.2    64-85         STATE TAX
RCCTAX   FORM      9.2    74-97         CITY TAX
RCPOST   FORM      3.2    84-103         POSTAGE
RCCNTLO  FORM      3      90-106         CONTROL NUMBER
RCDATE   DIM       6      92-112         CHECK DATE
rpayee   dim       25     99-137           payee
.begin patch 2.5
RCOmpId	Dim	1
RXNinc	FOrm	9.2	     Xninc
.end patch 2.5

. 
. 
TWO5     INIT      "                         "
TWO9     INIT      "                            "
DATE     DIM       6
SYSDATE  DIM       8
DETDATE  DIM       8
FORTY5  FORM      "45"
FOUR     FORM      "4"
ONETHOUS FORM      "1000"
DASH5    INIT      "-----"
DASH6    INIT      "------"
DASH7    INIT      "-------"
DASH8    INIT      "--------"
DASH9    INIT      "---------"
PAGECNTR FORM      "00"
LINECNTR FORM      2
PAGENUM  FORM      2
HEADATE  DIM       8
.begin patch 2.1
.paymask  dim      11
.begin patch 2.6
.PAYMASK  DIM       17
PAYMASK  DIM       18
.end patch 2.6
.end patch 2.1
TOTMASK  DIM       14
CHECKOUT DIM       6
RCPAYOUT FORM      9.2
DATEOUT  DIM       8
RLCRNOUT DIM       6
RCNTLOUT FORM      3
HEADISP  INIT      "NIN CHECK LOG PRINT PROGRAM"
TOTALAP  FORM      9.2
PRTFLAG  DIM       1
LOCAL    INIT      "LOCAL"
.START PATCH 2.4 REPLACED LOGIC
.gpath    init      "\\nts0\d\data\"
.END PATCH 2.4 REPLACED LOGIC
rfile    dim       55
chkcnt   form      3
holdcheck dim      6
. 
+..............................................................................
          IFNZ      PC
         CLOCK     DATE TO DATE
         MOVE      "99/99/99" TO DETDATE
         EDIT      DATE TO DETDATE
         MOVE      DETDATE TO TODAY
         UNPACK    DATE INTO MM,DD,YY
         XIF
.
         IFZ       PC
         CLOCK     DATE TO DETDATE
         MOVE      DETDATE TO TODAY
         UNPACK    DETDATE INTO MM,STR1,DD,STR1,YY
         XIF

         MOVE      "NCHK0004" TO PROGRAM
         MOVE      "CHECK RECONN PRINT PROGRAM" TO STITLE
         MOVE      "Names in the News" TO COMPNME
         IFZ       PC
         MOVE      "CHKRECON" TO INPNAME
         XIF
         IFNZ       PC
         MOVE      "CHKRECON" TO INPNAME
         XIF
         CALL      PAINT
           MOVE      "Exit" TO PF5
         TRAP      END IF F5
         CALL      FUNCDISP
         DISPLAY   *P01:06,"Input File  : ":
                   *P01:07,"Print File  : ":
                   *P01:08,"Input Count : "
         reset     comment
         clear     rfile
.START PATCH 2.4 REPLACED LOGIC
.         pack      rfile from gpath,comment,".dat"
         pack      rfile from NTWKPATH1,comment,".dat"
.END PATCH 2.4 REPLACED LOGIC
         prepare   recfile,rfile
.
INPGET   TRAP      FILENG IF IO
         OPEN      TESTFILE,INPNAME
         TRAPCLR   IO
         DISPLAY   *P15:06,INPNAME
         CLOSE     TESTFILE
         OPEN      RECON,INPNAME
          GOTO      PRTGET
FILENG   NORETURN
         KEYIN     *P01:24,*EL,"The Input file is not on-line. ":
                   *P15:06,INPNAME
         GOTO      INPGET
.
PRTGET   MATCH     B8 TO PRTNAME
         GOTO      PRTNG IF EQUAL
         MOVE      C1 TO PRTFLAG
         MATCH     LOCAL TO PRTNAME
         GOTO      START IF EQUAL
         MOVE      C2 TO PRTFLAG
         PACK      PRTFILE WITH PDRIVE,PRTNAME
         SPLOPEN   PRTFILE
         DISPLAY   *P15:07,PRTNAME
         GOTO      START
PRTNG    KEYIN     *P01:24,*EL,"Print File answer is invalid.":
                   *P15:07,PRTNAME
         GOTO      PRTGET

. 
START
         CALL      HEADINGS
.
.begin patch 2.5
.LOOPER  READ      RECON,SEQ;RCMLR,RCCNT:
.                   RCLRNO:
.                   RCCHEK:
.                   RCLONO:
.                   RCACREC:
.                   RCAPAY:
.                   RCNNINC:
.                   RCLRINC:
.                   RCSTAX:
.                   RCCTAX:
.                   RCPOST:
.                   RCCNTLO:
.                   RCDATE:
.                   rpayee
LOOPER  READ      RECON,SEQ;RCMLR,RCCNT:
                   RCLRNO:
                   RCCHEK:
                   RCLONO:
                   RCACREC:
                   RCAPAY:
                   RCNNINC:
                   RCLRINC:
                   RCSTAX:
                   RCCTAX:
                   RCPOST:
                   RCCNTLO:
                   RCDATE:
                   rpayee:
	       RCOmpId:
	       RXNinc
.end patch 2.5
.
         GOTO      DONE IF OVER
         GOTO      PROCESS
. 
. 
. 
*   REPORT HEADINGS   *
. 
. 
. 
HEADINGS MOVE      C0 TO LINECNTR
         ADD       C1 TO PAGENUM
         compare    c1 to pagenum
         if         equal
;         print      hp17ptch,hptop,hpdupl,*f
         PRINT     HPtmsr17,hpdupl,hptop:                .compressed
                   033,"&l66P":               page length
                   033,"&l65F":               number lines
                   *f
         endif
.begin patch 2.5
.         PRINT     *F,*n,*4,"CONFIDENTIAL",TWO9,"* * * NIN    CHECK   LOG":
.                   *69,"  REPORT   * * * ",TWO5,"DATE ",TODAY
	If	(Company = c2)
		PRINT     	*F,*n,*4,"CONFIDENTIAL",TWO9,"* * * PLI    CHECK   LOG":
                   	*69,"  REPORT   * * * ",TWO5,"DATE ",TODAY
	else
         		PRINT     	*F,*n,*4,"CONFIDENTIAL",TWO9,"* * * NIN    CHECK   LOG":
                   	*69,"  REPORT   * * * ",TWO5,"DATE ",TODAY
	endif
.end patch 2.5
         PRINT     *112,"PAGE ",PAGENUM
         PRINT     *N,*23,"CHECK ##        DATE               $ A/P":
                   *75,"LR ##     CONTROL ##":
                   *L,*23,DASH6:
                   *38,DASH5:
                   *56,DASH8:
                   *75,DASH5:
                   *84,DASH9
         PRINT     *N,*FLUSH
         RETURN
. 
. 
. 
PROCESS
         UNPACK    RCDATE INTO MM,DD,YY
         REP       ZFILL,MM
         REP       ZFILL,DD
         REP       ZFILL,YY
         PACK      DETDATE FROM MM,SLASH,DD,SLASH,YY
         TYPE      RCCHEK
         GOTO      LOOPER IF NOT EQUAL
         add       c1 to chkcnt                 .number of checks    DLH 11June98
         MOVE      RCCHEK TO CHECKOUT
         MOVE      DETDATE TO DATEOUT
         MOVE      RCLRNO TO RLCRNOUT
         MOVE      RCCNTLO TO RCNTLOUT
.begin patch 2.1
.         MOVE      "$$$$,$$9.99" TO PAYMASK
.begin patch 2.6
.         MOVE      "$,$$$,$$$,$$9.99" TO PAYMASK
         MOVE      "$,$$$,$$$,$$9.99-" TO PAYMASK
.end patch 2.6
.end patch 2.1
.begin patch 2.6
	if	(RCApay >= c0)
         ADD       RCAPAY TO TOTALAP
         	endif
.         ADD       RCAPAY TO TOTALAP
.end patch 2.6
         EDIT      RCAPAY TO PAYMASK
         ADD       C1 TO LINECNTR
         match     RCChek to holdcheck
         if        equal
         sub       c1 from chkcnt                .record in file twice adjust count
         sub       rcapay from totalap           .record in file twice adjust AP total
         
         PRINT     Hpbon,*1,"Duplicate **":
                   *23,CHECKOUT:
                   *37,DATEOUT:
                   *53,PAYMASK:
                   *75,RLCRNOUT,"     "," ",RCNTLOUT,hpboff                 
         else
         move      RCChek to holdcheck
         PRINT     *23,CHECKOUT:
                   *37,DATEOUT:
                   *53,PAYMASK:
                   *75,RLCRNOUT,"     "," ",RCNTLOUT
         write     recfile,seq;checkout,",",rcdate,",",rcapay,",":
                   rlcrnout,",",rcntlout,",","#"",rpayee,"#""
         endif          
         COMPARE   LINECNTR TO FORTY5
         CALL      HEADINGS IF EQUAL
         GOTO      LOOPER
DONE      
         MOVE      "$$$,$$$,$$9.99" TO TOTMASK
         EDIT      TOTALAP TO TOTMASK
         PRINT     *L,*L,*23,chkcnt,b1,"Checks",*45,"TOTAL :",TOTMASK
         PRINT     hpreset
         GOTO      EOJ
. 
         INCLUDE   COMLOGIC.inc
. 
. 
EOJ

         CLOSE     RECON
         weof      recfile,seq
         close     recfile
         DISPLAY   *P1:1,*ES,*P10:12,"JOB DONE!!!!!!",*W2
         shutdown  "cls"
         STOP
.         CHAIN     "CASHVERN"
. 
. 
ABORT      BEEP
         DISPLAY   *P1:1,*HON,*ES,"JOB ABORTED VIA F4 KEY, OH NO!!!!",*W3
         GOTO      EOJ
         
IO       TRAPCLR   IO 
         NORETURN
         DISPLAY   *P12:15,"ERROR IS ",ERROR,*W5
         shutdown  "cls"
         STOP
. 
. 

