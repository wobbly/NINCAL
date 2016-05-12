*******************************************************************************
. THIS IS THE EXCHANGE STATUS  PRINT PROGRAM    APRIL 1983              *
*******************************************************************************
. Written for Names in the News California By David Herrick                   *
*******************************************************************************
.
* VARIABLES USED BY THE EXCHANGE STATUS PROGRAM.
.
.
PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
.Patch4.0
                              include compdd.inc
                              include   cntdd.inc
.         INCLUDE   NMLRDD.inc
.Patch4.0
         include   nxrfdd.inc
         include   ndatdd.inc
         include   norddd.inc
         INCLUDE   HP.inc
.START PATCH 3.0 - ADDED LOGIC
         INCLUDE   NOFRDD.INC
.END PATCH 3.0 - ADDED LOGIC
.START PATCH 3.9 ADDED LOGIC
          INCLUDE   NSEL2DD.INC
.END PATCH 3.9 ADDED LOGIC
.START PATCH 4.02 ADDED LOGIC
          INCLUDE   NXNGDD.INC
          INCLUDE   NXCHDD.INC
.END PATCH 4.02 ADDED LOGIC

release   init      "4.02"    ASH       12MAY2005 NINXNUM/NINXCHNG CONVERSION
reldate   init      "MAY 12, 2005"
.release  init      "4.01"    DLH       29July2004          Copyrite footer
.release  init      "4.0"     DMB       26MAY2004 Mailer CONVERSION
.release  init      "3.9"     ASH       29JAN2004  DATACARD CONVERSION
.reldate  init      "JANUARY 29, 2004"
.release  init       "3.8"            DMB 05JUN03 Fixed possible bug that would skip revsion date an/or never used flag.
.release  init       "3.7"            DMB 22NOV02 Modified Code to show Revision Date if requested
.release  init       "3.5"            DMB 24MAY02 Modified code to bold mailer who owed names to client which client never used
.release  init      "3.4"            DLH 11Dec01  >BUG<
.release  init      "3.3"            DLH 29Sep99  Various chances to support acrobat.
.release  init      "3.2"            DLH 21Jun99  qty conversion.
.release  init      "3.2"            DLH 21Jun99  qty conversion.
.RELEASE  INIT      "3.1"            JD  26MAY99 UPDATED COPY RIGHT YEAR 99.
.RELEASE  INIT      "3.0"           ASH 07MAY99 REPLACED OODES{NINORD.DAT} --> OFDESC{NINOFR.DAT}
.RELEASE  INIT      "2.9"           ASH 30DEC98 NINORD Y2K, File expansion
.release  init      "2.84"         .ASH 24SEP98 NINMLR Y2K File expansion
.Release  init      "2.83"         .JD 28SEP98 Fixed font on list desc to fit full lname.
.Release  init      "2.82"         .DLH 7May98 add MLR po to detail report
.                                 .fix page numbers on detail report
.release  init      ."2.81"      .JD  12may97 change how trans date prints.
.release  init      ."2.8"       .DLH 29Apr97 add date of last transaction to
.                                 .totals report
.release  init      ."2.7"        .DLH 09jun95 add consortium print.
.release  init      ."R2.6"       .DLH 26oct94 add universe option
.RELEASE  INIT      ."R2.5"       .DLH 14SEP94 ADD (V1) .
.RELEASE  INIT      ."2.4"        .DLH 31MAR94 CHANGE (V) HEADER.
.RELEASE  INIT      ."2.3"        .DLH 02FEB94 COPYRIGHT SYMBOL
.RELEASE  INIT      ."2.2"       .DLH 05MAR93 PRINT DETAIL ON LASER.
.RELEASE   INIT      ."2.1"      .DLH 14MAY92 ADDED COMMENT TO DETAIL PRINT.
.RELEASE  INIT      ."2.0" "
.
.
.Start Patch #2.84 - File expanded to reflect NINMLR expansion
.INPUT    FILE      FIXED=150
.START PATCH 4.02 REPLACED LOGIC
.INPUT    FILE      FIXED=170
INPUT    FILE      FIXED=260
.END PATCH 4.02 REPLACED LOGIC
.End Patch #2.84 - File expanded to reflect NINMLR expansion
. .........................................
.START PATCH 4.02 REPLACED LOGIC
.. ..ACCOUNT MASTER
..
.ENTRY    FORM      5              ENTRY NUMBER 17-21
..
..
.+ ..EXCHANGE MASTER
..
.EXKEY    DIM       13             ISAM KEY MAILER#1,MAILER#2,ENTRY
.LIST     DIM       6              LIST NUMBER
.LR       DIM       6              LR NUMBER 14-19
..begin patch 3.2
..QTY      FORM      7              QUANTITY OF EXCHANGE
.QTY      FORM      9              QUANTITY OF EXCHANGE
..end patch 3.2
.
..DAT      DIM       6              DATE OF EXCHANGE
.STAT     DIM       1              STATUS OF EXCHANGE (C) IF CANCELLED
..begin patch 3.2
..uSAGE1   FORM      9              TOTAL USAGE TO DATE MAILER 1
..uSAGE2   FORM      9              TOTAL USAGE TO DATE MAILER 2
.USAGE1   FORM      10              TOTAL USAGE TO DATE MAILER 1
.USAGE2   FORM      10             TOTAL USAGE TO DATE MAILER 2
..end patch 3.2
.TYPE     DIM       2              59-60   TYPIST INITIALS
.MLRSW    DIM       1              58-58   PLACED BY MLR1 HOLDS "1"
..                                 PLACED BY MLR2 SWITCH HOLDS "2"
.EXCOMMNT DIM       30             61-90  COMMENTS.
..
..
.MLR1     DIM       4              MAILER#1
.MLR2     DIM       4              MAILER # 2
MLR1     DIM       6              MAILER # 1
MLR2     DIM       6              MAILER # 2
.END PATCH 4.02 REPLACED LOGIC
.Start Patch #2.84 - var expanded to reflect NINMLR expansion
.MDES1    DIM       25
MDES1    DIM       45
.End Patch #2.84 - var expanded to reflect NINMLR expansion
.
+ .WORK FIELDS
.
DATE     DIM       8
FMESG    DIM       24
WORK04   DIM       4
SENTRY   DIM       5              *TEMP. FIELD FOR ENTRY NUMBER.
NSW      DIM       1              SWITCH FOR NEW RECORD CONTAINS "N" IF
.                                 RECORD IS TO BE CREATED
PRTSW    DIM       1              PRINT SWITCH  'T'=totals
LNAME    DIM       35             LIST DESC.
recsin   form      4
.
+ PRINT VARIABLES
MLRTAB   FORM      3
. ...............
.
LN       DIM       6
LINES    FORM      2
NOFILE   DIM       8
PMO      DIM       2
PDAY     DIM       2
PYR      DIM       2
SUBTOT1  FORM      10
GRNDTOT1 FORM      10
GRNDTOT2 FORM      10
PAGE     FORM      3
PAGENUM  DIM       3
PAGEMASK INIT      "ZZ9"
.START PATCH 4.02 REPLACED LOGIC
.TEMPMLR1 DIM       4
TEMPMLR1 DIM       6
.END PATCH 4.02 REPLACED LOGIC
PICSW    DIM       1                   HOLDS (1) IF PAYABLE PICK,(2) IF REC.
.                                      (3) IF EVEN, 4 IF ALL
STOPLOOP DIM       1
STATMSG  DIM       11                 HOLDS "*CANCELLED*" IF ORDER CANCELLED.
.Start Patch #2.84 - vars expanded to reflect NINMLR expansion
.MLRDES1  DIM       25
.MLRDES2  DIM       25                 USED TO SAVE MASTER MAILER NAMES FOR
MLRDES1  DIM       45
MLRDES2  DIM       45                 USED TO SAVE MASTER MAILER NAMES FOR
.End Patch #2.84 - vars expanded to reflect NINMLR expansion
.                                     DETAILED REPORTS.
holdsw   dim       1
DONESW   DIM       1
.begin patch 3.2
.QTYMASK  INIT      "Z,ZZZ,ZZ9"
.QTYOUT   DIM       9
QTYMASK  INIT      "ZZZ,ZZZ,ZZ9"
QTYOUT   DIM       11
.MASK     INIT      "ZZZ,ZZZ,ZZ9"
.MASKA    DIM       11                 USED FOR EDITING OUTPUT.
MASK     INIT      "Z,ZZZ,ZZZ,ZZ9"
MASKA    DIM       13                 USED FOR EDITING OUTPUT.
.end patch 3.2
NUM      FORM      1                  BRANCHING CONSTANT.
TOTAL    FORM      9                  PRINT TOTAL BALANCE ON TOTAL'S REPORT.
TITLE    DIM       40                 FOR KEYED IN TITLE LINE.
DASHLINE DIM       40                 CUSTOM UNDERLINE FOR KEYED IN TITLE LINE.
KEYS     FORM      2                  NUMBERS OF CHARACTERS IN TITLE.
placemnt FORM      3                  STARTING POSITION FOR TITLE PRINT LINE.
LOCAL    INIT      "LOCAL"
univflag form      1                  1=dont print, 2=print
unimask  dim       13
umbrflag form      1                  1=no, 2=umbrella exch org.
DUPFLAG  form      1                  1=simplex, 2=duplex
listflag form      1                  1=no list number 2=list number (V)
TRANflag form      1                  1=no list number 2=list number (V)
detflag  form      1                  1=no, 2=print offer, select, & MLR PO if avail.
DimPtr   dim       4                  Mailer Number
DimPtr1  dim       4                  List Number
USEDFLAG init      "Y"                .Flag to see if mailer ever used
NEVERUFLAG dim     1
.Patch3.7
REVVFLAG  DIM      1                  ;flag to see if inserting revision date
REVISIONDATE    DIM     16            DIM to insert REVISION DATE
.Patch3.7
.
. DEFAULT OPTIONS ARE : NONE
.                       laser PRINTER FORMAT
.
. COMMENT CAN MODIFY THE DEFAULTS:
.                                   duplex :  PRINT both sides
.
+******************************************************************************
. PROGRAM
*******************************************************************************
.
PREP     CMATCH    B1 TO PROGRAM
         IF        EOS
         MOVE      "NXCH0003" TO PROGRAM
         MOVE      "EXCHANGE PRINT PRGRAM" TO STITLE
         MOVE      "NINCAL" TO COMPNME
         MOVE      "EXSTAT" TO INPNAME
         MOVE      "LOCAL" TO PRTNAME
         ENDIF
         move      c2 to nxrfpath
         MOVE      "EXIT" TO PF5
         CLOCK     DATE TO DATE
         IFNZ      PC
         MOVE      "Z9/99/99" TO TODAY
         MOVE      DATE TO N6
         EDIT      N6 TO TODAY
         XIF
         IFZ       PC
         MOVE      DATE TO TODAY
         XIF
         CALL      PAINT
         CALL      FUNCDISP
         move      c2 to detflag            .temp
BEGIN    DISPLAY   *P01:05,"Options     :":
                   *P01:06,"Input File  :":
                   *P01:07,"Print File  :":
                   *P01:09,"Record Count:"
.
         GOTO      OPTION
OPTGET   MOVE      C0 TO DUPFLAG
         RESET     COMMENT
         KEYIN     *P20:10,"DUPLEX  :  Print Both sides":
                   *P20:11,"List    :  Print List ## on (V)":
                   *P01:24,*EL,"The Option setting is invalid.":
                   *P15:05,COMMENT:
                   *P20:10,*EL,*P20:11,*EL,*P20:12,*EL,*P20:13,*EL:
                   *P20:14,*EL,*P20:15,*EL,*P20:16,*EL,*P20:17,*EL;
         MATCH     "                         " TO COMMENT
         GOTO      OPTDEFLT IF EQUAL
OPTION   MOVE      C0 TO DUPFLAG
         RESET     COMMENT
         SETLPTR   COMMENT
         DISPLAY   *P15:05,COMMENT
         MATCH     "                         " TO COMMENT
         GOTO      OPTDEFLT IF EQUAL
         RESET     COMMENT
         SCAN      "DUPLEX" IN COMMENT
         CALL      OPTDUPLX IF EQUAL
         RESET     COMMENT
         SCAN      "LIST" IN COMMENT
         CALL      OPTlist IF EQUAL
         RESET     COMMENT
         GOTO      INPGET
OPTNG    KEYIN     *P20:10,"Duplex  :  Print both sides":
                   *P20:11,"List    :  Print List ## on (V)":
                   *P01:24,*EL,"The Option setting is invalid.":
                   *P15:05,COMMENT:
                   *P20:10,*EL,*P20:11,*EL,*P20:12,*EL,*P20:13,*EL:
                   *P20:14,*EL,*P20:15,*EL,*P20:16,*EL,*P20:17,*EL;
         GOTO      OPTGET
OPTDEFLT MOVE      C1 TO DUPFLAG
         move      c1 to listflag
         goto      inpget
OPTDUPLX MOVE      C2 TO DUPFLAG
         RETURN
OPTLIsT  MOVE      C2 TO LISTFLAG
         RETURN
.
.
.
INPGET   TRAP      INPNG GIVING ERROR IF IO
         OPEN      INPUT,INPNAME,exclusive
         TRAPCLR   IO
         DISPLAY   *P15:06,INPNAME
         GOTO      PRTGET
INPNG    NORETURN
         TRAPCLR   IO
         KEYIN     *P01:24,*EL,"The Input file is not on-line, or Busy. ":
                   *DV,ERROR:
                   *P15:06,INPNAME
         GOTO      INPGET
.
PRTGET   MATCH     B8 TO PRTNAME
         GOTO      PRTNG IF EQUAL
         MATCH     LOCAL  TO PRTNAME
         GOTO      PRESTART IF EQUAL
         PACK      PRTFILE WITH pdrive,PRTNAME
         SPLOPEN   PRTFILE
         DISPLAY   *P15:07,PRTNAME
         GOTO      PRESTART
PRTNG    KEYIN     *P01:24,*EL,"Print File answer is invalid.":
                   *P15:07,PRTNAME
         GOTO      PRTGET
.         DISPLAY   *P15:24,"FILES ARE BEING OPENED";
.
PRESTART
         MOVE      B1,STAT              CLEAR FIELD SO CORRECT STATUS WRITTEN
         MOVE      B1 TO DONESW
.         TRAP      INT IF INT
.         TRAP      ERROR IF IO
.         TRAP      EXIT IF F5
.         MOVE      "MASTER INPUT" TO FMESG
.         OPEN      INPUT,"EXSTAT"
.         TRAPCLR   IO
.         DISPLAY   *P1:24,*EL;
.
...............................................................................
.patch3.7
         READ      INPUT,SEQ;TEMPMLR1,WORK04,PICSW,univflag,TRANFLAG,NEVERUFLAG,REVVFLAG
.         READ      INPUT,SEQ;TEMPMLR1,WORK04,PICSW,univflag,TRANFLAG,NEVERUFLAG
.patch3.7
         MATCH     "H   " TO WORK04
         IF        EQUAL
         MOVE      "T" TO PRTSW
         compare   c2 to dupflag
         if        equal
         print     FAXPORT,hpreset,hpdupl,HPTOP
         else
         print     FAXPORT,hpreset,HPTOP
         endif
         GOTO      PRT1
                   ELSE
         MOVE      "D" TO PRTSW
         compare   c2 to dupflag
         if        equal
         print     hpreset,hpland,hptop,hpdupl,hplin8,hp10ptch
         else
         print     hpreset,hpland,hptop,hplin8,hp12ptch
         endif
.         PRINT     FAXLAND
         CLOSE     INPUT
         GOTO      DETAILED
         ENDIF
PRT1     branch    univflag to nuni,yuni
nuni     DISPLAY   *P15:05,COMMENT
         goto      prt1a
yuni     DISPLAY   *P15:05,COMMENT,b1,"UNIVERSE"
         goto      prt1a
prt1a    KEYIN     *P35:09,"CHANGE REPORT TITLE ? ",*T5,STR1;
         CMATCH    YES TO STR1
         GOTO      TITLEKEY IF EQUAL
         CLEAR     TITLE
         CLEAR     DASHLINE
         APPEND    "NIN EXCHANGE STATUS REPORT" TO TITLE
         APPEND    "    === ======== ====== ======" TO DASHLINE
.         MOVE      "20" TO placemnt
         CALL      NEWTTL
         GOTO      PRT11
.
NEWTTL   SETLPTR   TITLE
         ENDSET    TITLE
CHKTTL   CMATCH    B1 TO TITLE
         GOTO      SETTTL IF NOT EQUAL
         BUMP      TITLE BY -1
         GOTO      CHKTTL IF NOT EOS
SETTTL   MOVEFPTR  TITLE TO N3
         MOVE      C80 TO placemnt
         SUBTRACT  N3 FROM placemnt
         DIVIDE    C2 INTO placemnt
         RESET     TITLE
         SETLPTR   TITLE
         RETURN
.
TITLEKEY MOVE      C0 TO KEYS
         KEYIN     *P10:09,*EL,"WOULD YOU LIKE TO KEEP PREVIOUS TITLE? ",STR1;
         CMATCH    YES TO STR1
         GOTO      PRT11 IF EQUAL
         KEYIN     *P10:09,*EL,"TITLE: ",TITLE,*P77:09,"OK?",STR1,*P77:09,*EL;
         CMATCH    B1 TO TITLE
         GOTO      PRT1 IF EQUAL
         GOTO      PRT1 IF EOS
         CMATCH    YES TO STR1
         GOTO      TITLEKEY IF NOT EQUAL
         MOVELPTR  TITLE TO KEYS
MAKEDASH CLEAR     DASHLINE
DODASH   CMATCH    B1 TO TITLE
         GOTO      DASHDONE IF EOS
         GOTO      BUMPDASH IF EQUAL
         APPEND    "=" TO DASHLINE
         BUMP      TITLE BY 1
         GOTO      DASHDONE IF EOS
         GOTO      DODASH
DASHDONE RESET     DASHLINE
         RESET     TITLE
.         GOTO      POSTN
         CALL      NEWTTL
         GOTO      PRT11
BUMPDASH APPEND    B1 TO DASHLINE
         BUMP      TITLE BY 1
         GOTO      DODASH
POSTN
.         DISPLAY   *P1:24,"POSTN KEYS = ",KEYS,*W2;
         DIVIDE    C2 INTO KEYS
         SUBTRACT  C1 FROM KEYS
         MOVE      "40" TO placemnt
         SUB       KEYS FROM placemnt
         GOTO      PRT11
ENDPRT   DISPLAY   *P01:03,*EL,"PRINTING IS COMPLETED ! ",*B:
                   "----------------------";
         MOVE      C1,PAGE
         MOVE      C0,LINES
         PRINT     HPPORT,HPLIN6,HPRESET,hpdupoff
         CLOSE     INPUT
         splclose
         release
         shutdown  "CLS"
         STOP
.
...............................................................................
DETAILED
         OPEN      INPUT,INPNAME
         MOVE      C0 TO PAGE
         call      oh
.         CALL      NEWPAGE
...............................................................................
PRT1D
.START PATCH 4.02 REPLACED LOGIC
.         READ      INPUT,SEQ;EXKEY,LR,USAGE1,USAGE2:
.                   QTY,LIST,cc,Pyr,Pmo,Pday,STAT,MLRSW,TYPE,EXCOMMNT,LNAME:
.                   MCOMP
         READ      INPUT,SEQ;EXKEY,LR2,LR,USAGE1,USAGE2:
                   QTYFILL,QTY,LIST,cc,Pyr,Pmo,Pday,STAT,MLRSW,TYPE,XCHCOMNT,LNAME:
                   COMPCOMP
.END PATCH 4.02 REPLACED LOGIC
         GOTO      EOJ2 IF OVER
. MOVE MAILER NUMBERS TO WORK FIELDS
         add       c1 to recsin
         display   *p15:09,recsin
         call      rotdial
         PACK      DATE FROM PMO,SLASH,PDAY,SLASH,PYR
         UNPACK    EXKEY INTO MLR1,MLR2,SENTRY
         MOVE      SENTRY TO ENTRY
         CMATCH    YES,STOPLOOP
         CALL      LOADLN IF EQUAL
.START PATCH 4.02 REPLACED LOGIC
.         MATCH     "    ",MLR1
.         GOTO      PRT1D IF EQUAL
.         MATCH     "    ",MLR2
.         GOTO      PRT1D IF EQUAL
         MATCH     "      ",MLR1
         GOTO      PRT1D IF EQUAL
         MATCH     "      ",MLR2
         GOTO      PRT1D IF EQUAL
.END PATCH 4.02 REPLACED LOGIC
. ............................................................................
BREAK1   COMPARE   "50",LINES
.         CALL      NEWPAGE IF NOT LESS
         call       oh if not less
.
.         COMPARE   C0 TO ENTRY
.         CALL      newpage IF EQUAL
.         CALL      OH IF EQUAL
         COMPARE   "50",LINES
.         CALL      NEWPAGE IF NOT LESS
         CALL      oh IF NOT LESS
         COMPARE   C0 TO LINES
         CALL      newpage IF EQUAL
.         CALL      OH IF EQUAL
.
         COMPARE   C0,ENTRY
         CALL     BEGBAL IF EQUAL
         CALL      DETAIL IF NOT EQUAL
.
         COMPARE   "50",LINES
.         CALL      NEWPAGE IF NOT LESS
         call      oh if not less
.         COMPARE   C0 TO LINES
.         CALL      newpage IF EQUAL
.
         GOTO      PRT1D
.
. ............................................................................
* THIS STOPS A BREAK FROM HAPPENING AFTER THE BEGINNING BALANCE IS PRINTED
. NORMAL BREAKS ARE STARTED AFTER THE FIRST DETAIL RECORD.
LOADLN
         MOVE      LIST,LN
         MOVE      "N",STOPLOOP
         CALL      LHEAD
         RETURN
. ............................................................................
* BEGINNING BALANCE
BEGBAL
         MOVE      YES,STOPLOOP
. WILL CAUSE SUBROUTINE LOADLN TO BE CALLED.
.START PATCH 4.02 REPLACED LOGIC
.         PACK      MKEY FROM MLR1,Z3
.         CALL      NMLRKEY
.         CALL      NOMAILER IF OVER
.         MOVE      MCOMP TO MDES1
.         reset     mdes1
.         MOVE      MDES1,MLRDES1
.         PACK      MKEY FROM MLR2,Z3
.         CALL      NMLRKEY
.         CALL      NOMAILER IF OVER
.         MOVE      MCOMP,MLRDES2
.................................
          pack      COMPFLD,MLR1
          move      "BEGBAL-COMPKEY",Location
          pack      KeyLocation,"Key: ",COMPFLD
          call      COMPKEY
          if over
                    call      NOMAILER
          elseif (COMPMLRFLG <> "T")
                    call      NOMAILER
          endif
          move      COMPCOMP,MDES1
          move      MDES1,MLRDES1
.
          pack      COMPFLD,MLR2
          move      "BEGBAL,2-COMPKEY",Location
          pack      KeyLocation,"Key: ",COMPFLD
          call      COMPKEY
          if over
                    call      NOMAILER
          elseif (COMPMLRFLG <> "T")
                    call      NOMAILER
          endif
          move      COMPCOMP,MLRDES2
.END PATCH 4.02 REPLACED LOGIC
.
         move      mask to maska
         edit      usage1 to maska
.START PATCH 4.02 REPLACED LOGIC
.         PRINT     *L,*L,*L,*10,"BEGINNING BALANCE ## ",MLR1,hpt475,"OPENING":
.                  hpt800,"BEGINNING":
.                   " BALANCE ## ",MLR2:
.                   *L,*10,hpunon,MDES1,HPUNOff,hpt475,HPUNON," DATE  ":
.                   HPUNOff,hpt800,HPUNON,MCOMP:
.                   HPUNOFF:
.                   *L,*10,maska,"  NAMES",hpt475,DATE;
         PRINT     *L,*L,*L,*10,"BEGINNING BALANCE ## ",MLR1,hpt475,"OPENING":
                  hpt800,"BEGINNING":
                   " BALANCE ## ",MLR2:
                   *L,*10,hpunon,MDES1,HPUNOff,hpt475,HPUNON," DATE  ":
                   HPUNOff,hpt800,HPUNON,COMPCOMP:
                   HPUNOFF:
                   *L,*10,maska,"  NAMES",hpt475,DATE;
.END PATCH 4.02 REPLACED LOGIC
         move      mask to maska
         edit      usage2 to maska
         print     hpt800,maska,"  NAMES":
                   *L,*L,*L,*L,*L
.          .DLH 05JUL
.                   *10,"------------------------",*63,"--------":
.                  *97,"-------------------------":
..                 *L,*14,USAGE1,"  NAMES",*61,DATE,*101,USAGE2,"  NAMES":
.                   *L,*L,*L,*L,*L
.
         ADD       USAGE1,GRNDTOT1
         ADD       USAGE2,GRNDTOT2
.
         ADD       "10",LINES
.         ADD       "11",LINES          .DLH 05JUL
.
         move     c1 to holdsw
         NORETURN
         GOTO      PRT1D
. .............................................................................
* LIST TOTALS
LTOT
         COMPARE   "50",LINES
         CALL      NEWPAGE IF NOT LESS
         PRINT *L,*L,*L,*3,"LIST *":
               *L,*3,"TOTAL*",*19,"--------":
                   *L,*16,SUBTOT1,"   ","NAMES"
.
         MOVE      C0 TO SUBTOT1
         ADD       C3,LINES
        RETURN
.
. .............................................................................
* GRAND TOTAL
OTOT
         call      subtprt2
         COMPARE   "50",LINES
.         CALL      NEWPAGE IF NOT LESS
         call       oh if not less
.
         move      mask to maska
         edit      grndtot1 to maska
         PRINT   *L,*L,*L,*10," MAILER **  ",MLRDES1,B1:
                hpt550," MAILER **  ",MLRDES2:
                  *L,*10," Total Usage **  ",maska;
.                   hpt800," TOTAL USAGE ** ",":
.                   *L,*28,maska;
         move      mask to maska
         edit      grndtot2 to maska
.         print    *76,maska
         print    hpt550," Total Usage ** ",maska
         ADD       C4 TO LINES
         COMPARE   "50",LINES
         CALL      NEWPAGE IF NOT LESS
. CALC FOR BAL.
         COMPARE   GRNDTOT1 TO GRNDTOT2
         GOTO      OTOTE IF EQUAL
         GOTO      OTOTL IF LESS
         GOTO      OTOTO
.         GOTO      OTEXIT
.
OTOTE    PRINT     *L,*L,*L,*25,*RPTCHAR "_":60:
                   *L,*40,MLRDES1," AND ",MLRDES2," ARE EVEN"
         add       c5 to lines
         GOTO      OTEXIT
.
OTOTL    SUBTRACT  GRNDTOT2 FROM GRNDTOT1
.         PRINT     *L,*L,*L,*25,"--------------------------------------------":
         MOVE      MASK TO MASKA
         EDIT      GRNDTOT1 TO MASKA
         PRINT     *L,*L,*L:
                   *L,*20,MLRDES1,HPBON," OWES ",HPBOFF,MLRDES2,B1,HPBON,MASKA,HPBOFF," NAMES"
         add       c5 to lines
         GOTO      OTEXIT
.
OTOTO    SUBTRACT  GRNDTOT1 FROM GRNDTOT2
         MOVE      MASK TO MASKA
         EDIT      GRNDTOT2 TO MASKA
         PRINT     *L,*L,*L:
                   *L,*20,MLRDES2,HPBON," OWES ",HPBOFF,MLRDES1,B1,HPBON,MASKA,HPBOFF," NAMES"
         add       c5 to lines
         GOTO      OTEXIT
.
OTEXIT
         CALL      PAGENUM
         PRINT     *FLUSH
         MOVE      C0,GRNDTOT1
         MOVE      C0,GRNDTOT2
.
         ADD       C4,LINES
.
         RETURN
.
. .............................................................................
* DETAIL HEADING
OH
         COMPARE   C0 TO PAGE
         IF        EQUAL
         ADD       C1 TO PAGE
                   ELSE
         CALL      PAGENUM
         ENDIF
         MOVE      LIST,LN
.         MOVE      PAGEMASK TO PAGENUM
.         EDIT      PAGE TO PAGENUM
.         PRINT     *f,*L,*1,hp10ptch,"CONFIDENTIAL",hpt950,TODAY:
         compare   c1 to page
         if        equal
         PRINT     *L,*1,hp12ptch,"CONFIDENTIAL",hpt950,TODAY:
                   hpt325,"*** NIN EXCHANGE STATUS REPORT ***":
                   hp14ptch
         else
         PRINT     *f,hpland,*l,*L,*1,hp10ptch,"CONFIDENTIAL",hpt950,TODAY:
                   hpt325,"*** NIN EXCHANGE STATUS REPORT ***":
                   hp14ptch
         endif
.                   *l,hpt950,"PAGE : ",PAGE,hp14ptch
.                     *L,*49,"=== ======== ====== ======":
.         MOVE      C3,LINES
         MOVE      C2,LINES
         COMPARE   C0 TO ENTRY
         RETURN    IF EQUAL
...............................................................................
* DETAIL HEADING
LHEAD
         PRINT     *L,*L:
                   *22,"  ",*65,"   ":
                   *L,*1,HPUNON,"LR / PO ",hpunoff,hpt050,hpunon,"  DATE  ",hpunoff:
                   hpt125,hpunon,"        MAILER           ",hpunoff,hpt350,hpunon:
                   "QUANTITY ",hpunoff:
                   hpt450,hpunon," STATUS  ",hpunoff,hpt550,hpunon,"LIST DESCRIPTION                   ":
                   hpunoff,hpt800,hpunon,"Maildate",hpunoff,hpt875,hpunon:
                   "COMMENTS         ",HPUNOFF
         ADD       C5,LINES
         RETURN
...............................................................................
DETAIL   cmatch    mlrsw to holdsw
         call      subtprt if not equal
         CMATCH    "1",MLRSW
         GOTO      DETAILA IF EQUAL
         CMATCH    "2",MLRSW
         GOTO      DETAILB IF EQUAL
.
DETAILA
.START PATCH 4.02 REPLACED LOGIC
.         PACK      MKEY FROM MLR1,Z3
         PACK      COMPFLD,MLR1
.END PATCH 4.02 REPLACED LOGIC
         CMATCH    "C",STAT                          ORDER IS CANCELLED
         GOTO      PRCANCEL IF EQUAL
         CMATCH    "R",STAT                          ORDER CHANGED TO RENTAL
         GOTO      PRRENT IF EQUAL
         ADD       QTY TO SUBTOT1
         ADD       QTY TO GRNDTOT1
         CLEAR     STATMSG
         CMATCH    "X" TO STAT
         GOTO      PRCANCL1 IF EQUAL                 ORDER CANCELLED
.                                                    AFTER MAILDATE
         GOTO      DETAILC
.
DETAILB
.START PATCH 4.02 REPLACED LOGIC
.         PACK      MKEY FROM MLR2,Z3
         PACK      COMPFLD,MLR2
.END PATCH 4.02 REPLACED LOGIC
         CMATCH    "C",STAT                          ORDER IS CANCELLED
         GOTO      PRCANCEL IF EQUAL
         CMATCH    "R",STAT                          ORDER CHANGED TO RENTAL
         GOTO      PRRENT IF EQUAL
         ADD       QTY TO SUBTOT1
         ADD       QTY TO GRNDTOT2
         CLEAR     STATMSG
         CMATCH    "X" TO STAT
         GOTO      PRCANCL1 IF EQUAL                 ORDER CANCELLED
.                                                    AFTER MAILDATE
         GOTO      DETAILC
.
subtprt  move      mlrsw to holdsw
         move      mask to maska
         edit      grndtot1 to maska
         print     hpt125,hpunon,"Mailer total : ",hpt450,maska,hpunoff,*l
         add       c2 to lines
         return
subtprt2
         move      mask to maska
         edit      grndtot2 to maska
         print     hpt125,hpunon,"Mailer total : ",hpt450,maska,hpunoff,*l
         add       c2 to lines
         return
.
PRCANCEL
         MOVE      "*CANCELLED*",STATMSG
         GOTO      DETAILC
PRRENT
         MOVE      "*RENTAL*",STATMSG
         GOTO      DETAILC
PRCANCL1 MOVE      "CANC/NO ADJ",STATMSG
         GOTO      DETAILC
DETAILC
         COMPARE   "50",LINES
         CALL      OH IF NOT LESS
.START PATCH 4.02 REPLACED LOGIC
.         CALL      NMLRKEY
.         CALL      NOMAILER IF OVER
.         MOVE      MCOMP TO MDES1
          move      "DETAILC-COMPKEY",Location
          pack      KeyLocation,"Key: ",COMPFLD
          CALL      COMPKEY
          if over
                    call      NOMAILER
          elseif (COMPMLRFLG <> "T")
                    call      NOMAILER
          endif
          MOVE      COMPCOMP TO MDES1
.END PATCH 4.02 REPLACED LOGIC
         move      lr to nordfld
         move      c1 to nordpath
.Start Patch #2.9 - replaced var
.         clear     str8
         clear     str10
.End Patch #2.9 - replaced var
         unpack    date into mm,str1,dd,str1,yy
         call      cvtjul
         move      juldays to n6
         move      "01" to mm
         move      "01" to dd
         move      "88" to yy
         call      cvtjul
         compare   juldays to n6
         if       not less
         call      nordkey
         if        not over
.Start Patch #2.9 - replaced var
.         pack      str8 from OMDTEM,slash,OMDTEd,slash,OMDTEy
         pack      str10 from OMDTEM,slash,OMDTEd,slash,OMDTEC,OMDTEy
.End Patch #2.9 - replaced var
.START PATCH 3.0 - NEW LOGIC
.EXTRACT OFFER DESCRIPTION FROM OFFER FILE AS OPPOSED TO RELYING ON NINORD.DAT
         bump      OODNUM,4
         pack      NOFRFLD,OMLRNUM,OODNUM
         reset     OODNUM
         move      "Rest-NOFRKEY",Location
         call      NOFRKEY
.END PATCH 3.0 - NEW LOGIC
         endif
         endif
         MOVE      QTYMASK TO QTYOUT
         EDIT      QTY TO QTYOUT
.Start Patch #2.9 - replaced var
.         PRINT     *1,LR,hpt050,DATE,hpt125,hp17ptch,MDES1,hpt350,hp14ptch,QTYout:
.                   hpt450,STATMSG,hpt550,hp17ptch,LNAME,hpt800,hp14ptch,str8,hpt875,EXCOMMNT
.START PATCH 4.02 REPLACED LOGIC
.         PRINT     *1,LR,hpt050,DATE,hpt125,hp17ptch,MDES1,hpt350,hp14ptch,QTYout:
.                   hpt450,STATMSG,hpt550,hp17ptch,LNAME,hpt800,hp14ptch,str10,hpt875,EXCOMMNT
         PRINT     *1,LR,hpt050,DATE,hpt125,hp17ptch,MDES1,hpt350,hp14ptch,QTYout:
                   hpt450,STATMSG,hpt550,hp17ptch,LNAME,hpt800,hp14ptch,str10,hpt875,XCHCOMNT
.END PATCH 4.02 REPLACED LOGIC
.End Patch #2.9 - replaced var
.
         ADD       C1,LINES
         compare   c2 to detflag
         if        equal
         clear     oodes
         clear     o2des
         call      nordkey
         if        not over
.START PATCH 3.0 - NEW LOGIC
.EXTRACT OFFER DESCRIPTION FROM OFFER FILE AS OPPOSED TO RELYING ON NINORD.DAT
.         print     *3,omlrpon,hpt125,oodes,hpt550,o2des
         bump      OODNUM,4
         pack      NOFRFLD,OMLRNUM,OODNUM
         reset     OODNUM
         move      "Rest-NOFRKEY",Location
         call      NOFRKEY
.START PATCH 3.9 ADDED LOGIC
.         print     *3,omlrpon,hpt125,ofdesc,hpt550,o2des
          packkey   NSEL2FLD,"1",OLRN
          move      "NSEL2KEY",Location
          pack      KeyLocation,"Key: ",NSEL2FLD
          call      NSEL2KEY
          if over
                    move      O2DES,NSEL2NAME
          endif
         print     *3,omlrpon,hpt125,ofdesc,hpt550,NSEL2NAME
.END PATCH 3.9 ADDED LOGIC
.END PATCH 3.0 - NEW LOGIC

         ADD       C1,LINES
         endif
         endif
         RETURN
...............................................................................
...............................................................................
NOFILE   DISPLAY   *B,*P01:23,*EL,NOFILE," FILE IS NOT ON LINE NOTIFY YOUR ":
                   "PROGRAMER !!!!!",*W,*B,*W,*B,*W5;
         shutdown  "CLS"
         STOP
...............................................................................
NOMAILER
.START PATCH 4.02 REPLACED LOGIC
.         MOVE      "NO MAILER FOUND" TO MCOMP
         MOVE      "NO MAILER FOUND" TO COMPCOMP
.END PATCH 4.02 REPLACED LOGIC
         RETURN
...............................................................................
EOJ2
         CALL      OTOT
.         PRINT     *F
         BEEP
         GOTO      ENDPRT
+..............................................................................
. NODETAIL - TOTALS REPORT.
NODETAIL
...............................................................................
PRT11
.START PATCH 4.02 REPLACED LOGIC
.         PACK      MKEY FROM TEMPMLR1,Z3
.         CALL      NMLRKEY
.         CALL      NOMAILER IF OVER
.         MOVE      MCOMP TO MDES1
         PACK      COMPFLD,TEMPMLR1
         move      "PRT11-COMPKEY",Location
         pack      KeyLocation,"Key: ",COMPFLD
         CALL      COMPKEY
          if over
                    call      NOMAILER
          elseif (COMPMLRFLG <> "T")
                    call      NOMAILER
          endif
         MOVE      COMPCOMP TO MDES1
.END PATCH 4.02 REPLACED LOGIC
         CALL      OH1
PRT12    call      rotdial
.START PATCH 4.02 REPLACED LOGIC
.          READ    INPUT,SEQ;MLR1,MLR2,ENTRY,LR,USAGE1,USAGE2:
.                   QTY,LIST,cc,pyr,PMO,PDAY,STAT,MLRSW,TYPE,EXCOMMNT,LNAME:
.                   MCOMP
          READ    INPUT,SEQ;MLR1,MLR2,ENTRY,LR2,LR,USAGE1,USAGE2:
                   QTYFILL,QTY,LIST,cc,pyr,PMO,PDAY,STAT,MLRSW,TYPE,XCHCOMNT,LNAME:
                   COMPCOMP
.END PATCH 4.02 REPLACED LOGIC
         GOTO      EOJ22 IF OVER
.START PATCH 4.02 REPLACED LOGIC
.         MATCH     "    ",MLR1
.         GOTO      PRT12 IF EQUAL
.         MATCH     "    ",MLR2
.         GOTO      PRT12 IF EQUAL
.         MATCH     "       " TO MCOMP
...............................
         MATCH     "      ",MLR1
         GOTO      PRT12 IF EQUAL
         MATCH     "      ",MLR2
         GOTO      PRT12 IF EQUAL
         MATCH     "       " TO COMPCOMP
.END PATCH 4.02 REPLACED LOGIC
         GOTO      PRT12 IF EQUAL        *SUPPRESS BLANK PRINT OF 1ST REC.
         add       c1 to recsin
         display   *p15:09,recsin
         move      c1 to umbrflag
         compare   c2 to univflag        *universe wanted ?
         if        equal                 *yes
         clear     nxrffld2
         clear     universe
         move       c0 to n9
         move       c0 to n10
         MATCH     TEMPMLR1,MLR1
            IF        EQUAL
              move      mlr2 to nxrffld2
            else
              move      mlr1 to nxrffld2
            endif
         rep       zfill in nxrffld2
         move      c1 to umbrflag
.         display   *p1:24,*el,nxrffld2
         call      nxrfkey
            if        not over
            clear     ndatfld
            clear     universe
            move       c0 to n9
            move      nxrflist to ndatfld
            move      c1 to ndatpath
            rep       zfill in ndatfld
            call      ndatkey
            goto      uniexit if over
            CMATCH     b1  TO status
            goto      moreuni if not equal
.START PATCH 3.9 REPLACED LOGIC
.            move      c0 to n9
.            move      universe to n9
.            add       n9 to n10
            move      c0 to n11
                                        call trim using universe
                                        call zfillit using universe
            move      universe to n11
            add       n11 to n10
.END PATCH 3.9 REPLACED LOGIC
moreuni         call      nxrfks
                if        not over                .an umbrella
                  match     nxrffld2 to nxrfmlr
                  goto      uniexit if not equal
.                move      c2 to umbrflag          *yes an umbrella
                  move      c1 to ndatpath
                  move      nxrflist to ndatfld
                  rep       zfill in ndatfld
                  clear     universe
                  call      ndatkey
                  CMATCH     b1  TO status
                  goto      moreuni if not equal
.START PATCH 3.9 REPLACED LOGIC
.                  move      c0 to n9
.                  move      universe to n9
.                  add       n9 to n10
                  move      c0 to n11
                                                            call trim using universe
                                                            call zfillit using universe
                  move      universe to n11
                  add       n11 to n10
.END PATCH 3.9 REPLACED LOGIC
                  goto     moreuni
                endif
            else
            display      *p1:24,*el,*red,*dion,"NO X-REF",*dioff,*white:
                         *b,*p1:24,*el;
            endif
         endif
uniexit
         PACK      DATE FROM PMO,SLASH,PDAY,SLASH,PYR
         GOTO      BREAK11
.
. ............................................................................
BREAK11  COMPARE   "55",LINES
         CALL      OH1 IF NOT LESS
         CALL      LTOT1
.
.         COMPARE   "48",LINES
.         CALL      OH1 IF NOT LESS
.
.
         COMPARE   "55",LINES
         CALL      OH1 IF NOT LESS
         GOTO      PRT12
. ............................................................................
* LIST TOTALS
LTOT1
         MATCH     TEMPMLR1,MLR1
         GOTO      LTOT1A IF EQUAL
         MATCH     TEMPMLR1,MLR2
         GOTO      LTOT1B IF EQUAL
         RETURN

.
LTOT1A
         MOVE      PICSW TO NUM
         BRANCH    NUM OF LTOT11P,LTOT11R,LTOT11E,LTOTALL
. LTOT11P-PAYABLES REPORT. LTOT11R-RECIEVABLES REPORT
.
LTOT1B
         MOVE      PICSW TO NUM
         BRANCH    NUM OF LTOT12P,LTOT12R,LTOT11E,LTOTALL
. LTOT12P-PAYABLES REPORT. LTOT12R-RECIEVABLES REPORT
.
LTOT11P  SUBTRACT  USAGE2 FROM USAGE1
         MOVE      MASK TO MASKA
         EDIT      USAGE1 TO MASKA
.         PRINT *L,*L,*3,"*  *":
.               *14,MCOMP,*50,MASKA
.*12,hp12ptch,MCOMP,hp10ptch,*50,MASKA;
.         PRINT     *L,*14,MCOMP,b1,str7,*50,MASKA;
.
         ADD       "2",LINES
         ADD       USAGE1 TO TOTAL           *FOR FINAL TOTALS.
        RETURN
.
LTOT11R  SUBTRACT  USAGE1 FROM USAGE2
         MOVE      MASK TO MASKA
         EDIT      USAGE2 TO MASKA
.         PRINT *L,*L,*3,"*  *":
.               *14,MCOMP,*50,MASKA
.*12,hp12ptch,MCOMP,hp10ptch,*50,MASKA;
.         PRINT     *L,*L,*14,MCOMP,b1,str7,*50,MASKA;
.
         ADD       "3",LINES
         ADD       USAGE2 TO TOTAL           *FOR FINAL TOTALS.
        RETURN
.
.LTOT11E  PRINT     *L,*L,*3,"*  *":
.                   *14,MCOMP,*56,"*  *"
LTOT11E
.START PATCH 4.02 REPLACED LOGIC
.         PRINT     *L,*11,MCOMP
         PRINT     *L,*11,COMPCOMP
.END PATCH 4.02 REPLACED LOGIC
         ADD       "2" TO LINES
         RETURN
.
.
LTOT12P  SUBTRACT  USAGE1 FROM USAGE2
         MOVE      MASK TO MASKA
         EDIT      USAGE2 TO MASKA
.         PRINT *L,*L,*3,"*  *":
.*12,hp12ptch,MCOMP,hp10ptch,*50,MASKA;
.         PRINT     *L,*14,MCOMP,b1,str7,*50,MASKA;
.
         ADD       "2",LINES
         ADD       USAGE2 TO TOTAL           *FOR FINAL TOTALS.
        RETURN
.
LTOT12R  SUBTRACT  USAGE2 FROM USAGE1
         MOVE      MASK TO MASKA
         EDIT      USAGE1 TO MASKA
.         PRINT *L,*L,*3,"*  *":
.               *14,MCOMP,*50,MASKA
.*12,hp12ptch,MCOMP,hp10ptch,*50,MASKA;
.
         ADD       "2",LINES
         ADD       USAGE1 TO TOTAL           *FOR FINAL TOTALS.
        RETURN
.
LTOTALL
.START PATCH 4.02 REPLACED LOGIC
.         clear     str7
         clear     str9
.END PATCH 4.02 REPLACED LOGIC
         compare   c2 to listflag                .lst numbers requested?
         if        equal                         .yes
         clear     nxrffld2
         MATCH     TEMPMLR1,MLR1
            IF        EQUAL
            move      mlr2 to nxrffld2
            else
            move      mlr1 to nxrffld2
            endif
         call      nxrfkey
            if        not over
.                call      nxrfks
.                if        not over                .an umbrella
.                clear     str7
.                goto      getout
.                endif
          compare  c2 to umbrflag
.patch3.8
.bug fix that would skip these two option if certain criteria met.
.          goto     getout if equal
          goto     cont if equal
.patch3.8
.START PATCH 4.02 REPLACED LOGIC
.                clear     str7
.                append    "##" to str7
.                append    nxrfmlr to str7
.                reset      str7
................................
                clear     str9
                append    "##" to str9
                append    nxrfmlr to str9
                reset      str9
.END PATCH 4.02 REPLACED LOGIC
            endif
         endif
.Patch3.5
.patch3.8
CONT
.bug fix that would skip these two option if certain criteria met.
.patch3.8
         cmatch YES,NEVERUFLAG  ;if want never been used option
         if equal     .yes
.START PATCH 4.02 REPLACED LOGIC - TEMPORARY PATCH
.                  pack NORDFLD1,"01X",TEMPMLR1
......................
.                   pack      COMPFLD,TEMPMLR1
.                   move      "CONT-COMPKEY",Location
.                   pack      KeyLocation,"Key: ",COMPFLD
.                   call      COMPKEY
.PRT11 does call to get COMPOLDMLR
                    pack      NORDFLD1,"01X",COMPOLDMLR
.END PATCH 4.02 REPLACED LOGIC - TEMPORARY PATCH
                  clear NORDFLD3
                  clear NORDFLD4
                  move C2,NXRFPATH
                  clear NXRFFLD
                  MATCH     TEMPMLR1,MLR1
                  IF        EQUAL
                        move      mlr2 to nxrffld2
                  else
                        move      mlr1 to nxrffld2
                  endif
                  call NXRFKEY
                  loop
                       until over
                       until (NXRFFLD2 <> NXRFMLR)
                       pack NORDFLD2,"02X",NXRFLIST
                       call NORDLAST
                       goto overread if over
                       loop
                            if (OSTAT = "0" | OSTAT = "B")
                                move YES to usedflag
.patch3.8
                      goto cont2
.                     goto getout
.patch3.8
                            else
                                move NO to usedflag
                            endif
OverRead
                            move NO to usedflag
                            call NORDKGP
                            until over
.START PATCH 4.02 REPLACED LOGIC
.                            until (OMLRNUM <> TEMPMLR1)
                            until (OMLRNUM <> COMPOLDMLR)
.END PATCH 4.02 REPLACED LOGIC
                       repeat
                       call NXRFKS
                   until over
                   repeat
         endif
.subpatch3.5
.patch3.8
CONT2
.bug fix that would skip these two option if certain criteria met.
.patch3.8
.patch3.7
                  cmatch YES,REVVFLAG ;if want never been used option
                  if equal
               move  C2,NXRFPATH
               clear NXRFFLD
                         MATCH     TEMPMLR1,MLR1
                         IF        EQUAL
                               move      mlr2 to nxrffld2
                         else
                               move      mlr1 to nxrffld2
                         endif
                         call  NXRFKEY
                         if over
.START PATCH 3.9 REPLACED LOGIC
.                    move "00/00/0000" to revdate
                     move "00/00/0000" to str10
.END PATCH 3.9 REPLACED LOGIC
                              else
                     call zfillit using NXRFLIST
                               move c1 to ndatpath
                     clear ndatfld
                     move nxrflist to ndatfld
                     call ndatkey
                               if over
                            pack REVISIONDATE with "REV: ","00/00/0000"
                     else
.START PATCH 3.9 REPLACED LOGIC
.                           pack REVISIONDATE with "REV: ",REVDATE
                              unpack    REVDATE,CC,YY,MM,DD
                              pack      str10,MM,SLASH,DD,SLASH,CC,YY
                            pack REVISIONDATE with "REV: ",str10
.END PATCH 3.9 REPLACED LOGIC
                     endif
               endif
                  else
               clear REVISIONDATE
                  endif
.patch3.7
getout   ADD       C2 TO LINES
         COMPARE   USAGE1 TO USAGE2
         IF        EQUAL
         MOVE      MASK TO MASKA
         EDIT      C0 TO MASKA
.         PRINT     *L,*L,*14,MCOMP,*50,MASKA,*62,MASKA
.START PATCH 4.02 REPLACED LOGIC
.           PRINT     *L,*2,str7,*09,hp12ptch,MCOMP,hp10ptch,*88,"X";
           PRINT     *L,*2,str9,*09,hp12ptch,COMPCOMP,hp10ptch,*88,"X";
.END PATCH 4.02 REPLACED LOGIC
.         PRINT     *L,*09,MCOMP,b1,str7,*71,"X";
.         PRINT     *L,*09,MCOMP,*91,"X",*38,hp20ptch,date,hp12ptch;
         goto      ltotallx
.         RETURN
         ENDIF

         MATCH     TEMPMLR1,MLR1
         IF        EQUAL
         SUBTRACT  USAGE1 FROM USAGE2
         MOVE      MASK TO MASKA
         EDIT      USAGE2 TO MASKA
         COMPARE   C0 TO USAGE2
                   IF        LESS
.START PATCH 4.02 REPLACED LOGIC
.                  PRINT     *L,*2,str7,*09,hp12ptch,MCOMP,hp10ptch,*41,MASKA;
                   PRINT     *L,*2,str9,*09,hp12ptch,COMPCOMP,hp10ptch,*41,MASKA;
.END PATCH 4.02 REPLACED LOGIC
.         PRINT     *L,*09,MCOMP,b1,str7,*41,MASKA;
                   ADD       USAGE2 TO GRNDTOT1
         ELSE
.patch3.5
                    if (usedflag = NO)
                              if (usage1 > c0)
.if have no usage in exchange file and have nothing in our order file   bold
.START PATCH 4.02 REPLACED LOGIC
.                             PRINT     *L,*2,str7,*09,hp12ptch,MCOMP,hp10ptch,*53,MASKA;
.                                                                      else
..Patch3.6
..                             PRINT     *L,*2,str7,*09,hp12ptch,"*",MCOMP,hp10ptch,*53,MASKA;   .bold
.                             PRINT     *L,*2,str7,*09,hp12ptch,HPBON,MCOMP,HPBOFF,hp10ptch,*53,MASKA;   .bold
..EndPatch3.6
.                      endif
.                                   else
..subpatch3.5
.                    PRINT     *L,*2,str7,*09,hp12ptch,MCOMP,hp10ptch,*53,MASKA;
.                                   endif
......................................
                                        PRINT     *L,*2,str9,*09,hp12ptch,COMPCOMP,hp10ptch,*53,MASKA;
                              else
                                        PRINT     *L,*2,str9,*09,hp12ptch,HPBON,COMPCOMP,HPBOFF,hp10ptch,*53,MASKA;   .bold
                              endif
                    else
                              PRINT     *L,*2,str9,*09,hp12ptch,COMPCOMP,hp10ptch,*53,MASKA;
                    endif
.END PATCH 4.02 REPLACED LOGIC
.         PRINT     *L,*09,MCOMP,b1,str7,*53,MASKA;
                   ADD       USAGE2 TO GRNDTOT2
                   ENDIF
         ENDIF

         MATCH     TEMPMLR1,MLR2
         IF        EQUAL
         SUB       USAGE2 FROM USAGE1
         MOVE      MASK TO MASKA
         EDIT      USAGE1 TO MASKA
         COMPARE   C0 TO USAGE1
                   IF        LESS
.START PATCH 4.02 REPLACED LOGIC
.                  PRINT     *L,*2,str7,*09,hp12ptch,MCOMP,hp10ptch,*41,MASKA;
..         PRINT     *L,*09,MCOMP,b1,str7,*41,MASKA;
..begin patch 3.4
..                 ADD       USAGE2 TO GRNDTOT1
.                  ADD       USAGE1 TO GRNDTOT1
.                  ELSE
..patch3.5
.                             if (usedflag = NO)
..if have no usage in exchange file and have nothing in our order file bold
.                      if (usage2 > c0)
.                             PRINT     *L,*2,str7,*09,hp12ptch,MCOMP,hp10ptch,*53,MASKA;
.                                                                      else
..Patch3.6
..                             PRINT     *L,*2,str7,*09,hp12ptch,"*",MCOMP,hp10ptch,*53,MASKA;  .Yes
.                             PRINT     *L,*2,str7,*09,hp12ptch,HPBON,MCOMP,HPBOFF,hp10ptch,*53,MASKA;  .Yes
..EndPatch3.6
.                      endif
..               PRINT     *L,*2,str7,*09,HP12PT,MCOMP,hp10ptch,*53,MASKA;
.                             else
..subpatch3.5
.                  PRINT     *L,*2,str7,*09,hp12ptch,MCOMP,hp10ptch,*53,MASKA;
.                             endif
........................................
                              PRINT     *L,*2,str9,*09,hp12ptch,COMPCOMP,hp10ptch,*41,MASKA;
                              ADD       USAGE1 TO GRNDTOT1
                    ELSE
                              if (usedflag = NO)
.if have no usage in exchange file and have nothing in our order file bold
                                        if (usage2 > c0)
                                                  PRINT     *L,*2,str9,*09,hp12ptch,COMPCOMP,hp10ptch,*53,MASKA;
                                        else
                                                  PRINT     *L,*2,str9,*09,hp12ptch,HPBON,COMPCOMP,HPBOFF,hp10ptch,*53,MASKA;  .Yes
                                        endif
                              else
                                        PRINT     *L,*2,str9,*09,hp12ptch,COMPCOMP,hp10ptch,*53,MASKA;
                              endif
.END PATCH 4.02 REPLACED LOGIC
.         PRINT     *L,*09,MCOMP,b1,str7,*53,MASKA;
.                  ADD       USAGE2 TO GRNDTOT2
                   ADD       USAGE1 TO GRNDTOT2
.end patch 3.4
                   ENDIF
         ENDIF
.
ltotallx compare    c2 to univflag                .universes requested?
         if         equal                          .yes
         compare    c2 to umbrflag               .umbrella organization?
          if        equal                         .yes
          print     *l,*09,hpitalic,hp10ptch,"Consortium",hpuprght,hp10ptch;
          move       c0 to n10
          RETURN
          endif          else
         move     "Z,ZZZ,ZZZ,ZZ9" to unimask
         edit       n10 to unimask
         compare   c2 to TRANflag                .TRANS DATE requested?
         if        not equal
         clear     date
         endif                        .yes
.Patch3.7
         print      *l,*09,unimask,*29,hp20ptch,revisiondate,hp10ptch,*38,hp20ptch,date,hp10ptch;
.         print      *l,*09,unimask,*29,hp20ptch,HPBON,revisiondate,HPBOFF,hp10ptch,*38,hp20ptch,date,hp10ptch;
.         print      *l,*09,unimask,*38,hp20ptch,date,hp10ptch;
.patch3.7
         else
         compare   c2 to TRANflag
         if        not equal
         clear     date
         endif                        .yes
.patch3.7
         print      *l,*09,unimask,*29,hp20ptch,revisiondate,hp10ptch,*38,hp20ptch,date,hp10ptch;
.         print      *l,*09,unimask,*29,hp20ptch,HPBON,revisiondate,HPBOFF,hp10ptch,*38,hp20ptch,date,hp10ptch;
.         print      *l,*09,b10,*38,hp20ptch,date,hp10ptch;
.patch3.7
         endif
         move       c0 to n10
         RETURN
. .............................................................................
* totals report HEADING
OH1
         COMPARE   C0 TO PAGE
         IF        EQUAL
         ADD       C1 TO PAGE
                   ELSE
         CALL      PAGENUM
         ENDIF
         CALL      CNTRMLR
         compare   c1 to page
         if        equal
.begin patch 3.3
.START PATCH 4.02 REPLACED LOGIC
.         PRINT     *N,*N,*N,*N,*N,*N,*N,*N,*N,*36,"CONFIDENTIAL":
.                   *N,*N:
.                   *N,*09,TITLE:
.                   *N,*09,"Date: ",TODAY:
.                   *L,*09,"Client: ","(",MNUM,") ",MDES1
.          else
..         PRINT     *N,*N,*N,*N,*N,*N,*N,*N,*N,*36,"CONFIDENTIAL":
.         PRINT     *F,*N,*N,*N,*N,*N,*N,*N,*N,*N,*36,"CONFIDENTIAL":
.                   *N,*N:
.                   *N,*09,TITLE:
.                   *N,*09,"Date: ",TODAY:
.                   *L,*09,"Client: ","(",MNUM,") ",MDES1
.................................................................
                    PRINT     *N,*N,*N,*N,*N,*N,*N,*N,*N,*36,"CONFIDENTIAL":
                              *N,*N:
                              *N,*09,TITLE:
                              *N,*09,"Date: ",TODAY:
                              *L,*09,"Client: ","(",COMPNUM,") ",MDES1
          else
                    PRINT     *F,*N,*N,*N,*N,*N,*N,*N,*N,*N,*36,"CONFIDENTIAL":
                              *N,*N:
                              *N,*09,TITLE:
                              *N,*09,"Date: ",TODAY:
                              *L,*09,"Client: ","(",COMPNUM,") ",MDES1
.END PATCH 4.02 REPLACED LOGIC
.end patch 3.3
          endif
.
         MOVE      C7 TO LINES
         ADD       C7 TO LINES
         MOVE      C0 TO N1
         MOVE      PICSW TO N1
         BRANCH    N1 OF OH1PAY,OH1REC,OH1EVEN,OH1ALL
.
OH1PAY
.         PRINT     *L,*L,*16,MDES1," OWES NAMES TO THE FOLLOWING"
         PRINT     *L,*09,"Owes names to:":
                   *N:
                   *N,*N,*14,"Mailer",*53,"Quantity",*FLUSH;
         PRINT     *14,"______",*53,"________"
         GOTO      OH1EXIT
.
OH1REC
.         PRINT     *L,*L,*16,MDES1," IS OWED NAMES BY THE FOLLOWING"
.         PRINT     *25,"All Mailers who owe you names":
.                   *L,*16,"-------------------------":
         PRINT     *L,*09,"Is owed names from:":
                   *N,*N,*N,*N,*N:
                   *N,*N,*14,"Mailer",*53,"Quantity",*FLUSH;
         PRINT     *14,"______",*53,"________"
.                   *41,"--------------------------------",*L
         GOTO      OH1EXIT
.
OH1EVEN
.         PRINT     *L,*L,*16,MDES1," IS EVEN WITH THE FOLLOWING"
.         PRINT     *21,"All Mailers with whom you have exchanged,":
.                *L,*25,"but the balance is currently even":
.                   *L,*16,"-------------------------":
.                   *41,"----------------------------",*L
         PRINT     *L,*09,"Has exchanged an even number of names with:":
                   *N,*N,*N,*N,*N:
                   *N,*N,*14,"Mailer",*FLUSH;
         PRINT     *14,"______"
.         ADD       C1 TO LINES
         GOTO      OH1EXIT
.
OH1ALL
.         PRINT     *L,*09,"Has exchange history with":
.                   " the organizations listed below:":
           PRINT   *L,*09,"Has an exchange history with the";
           PRINT   " organizations listed below.":
                   *N,*09,"Currently this mailer",*N,*N,*N:
                   *N,*44,hp12ptch,mdes1,hp10ptch,*N:
                   *44,"   Owes",*55," Is owed",*67,"Is even":
                   *FLUSH;
         PRINT     *44,"________",*55,"_________",*67,"________"
OH1EXIT
         ADD       C5 TO LINES
        RETURN
...............................................................................
PAGENUM
         cmatch    "T" to prtsw
         goto      DETCHK if not equal
         COMPARE   "56" TO LINES
         IF        NOT EQUAL
         GOTO      LINE1
         endif
         goto      pagemask
.
detchk   COMPARE   "55" TO LINES
         IF        NOT EQUAL
         GOTO      LINE1
         endif
.
pagemask
         MOVE      PAGEMASK TO PAGENUM
         EDIT      PAGE TO PAGENUM
         cmatch    "T" to prtsw
         if        equal
.begin patch 3.3
                   PRINT     *L,*1,CopyRight,hpptch,"1991-2016, Names in the News":
.                  PRINT     *L,*1,CopyRight,hpptch,"1991-2004, Names in the News/CA":
                   hp10ptch,*39,PAGENUM;
.                   hp10ptch,*39,PAGENUM,*l
         else
                   PRINT     *L,*1,CopyRight,hpptch,"1995-2016, Names in the News":
.                  PRINT     *L,*1,CopyRight,hpptch,"1995-2004, Names in the News/CA":
                   hp14ptch,hpt500,PAGENUM;
.                   hp14ptch,hpt500,PAGENUM,*l
.end patch 3.3
         endif
         ADD       C1 TO PAGE
         RETURN
.
LINE1    PRINT     B1
         ADD       C1 TO LINES
         GOTO      PAGENUM
...............................................................................
CNTRMLR  SETLPTR   MDES1
         ENDSET    MDES1
CHKMHEAD CMATCH    B1 TO MDES1
         GOTO      SETMHEAD IF NOT EQUAL
         BUMP      MDES1 BY -1
         GOTO      CHKMHEAD IF NOT EOS
SETMHEAD MOVEFPTR  MDES1 TO N3
         MOVE      C80 TO MLRTAB
         SUBTRACT  N3 FROM MLRTAB
         DIVIDE    C2 INTO MLRTAB
         RESET     MDES1
         SETLPTR   MDES1
         RETURN
...............................................................................
NEWPAGE
.          CMATCH    "D" TO PRTSW
.        IF        EQUAL
.          COMPARE     C0 TO PAGE
.          IF          EQUAL
.          PRINT       HPLIN8,HPLAND
.          ENDIF
         PRINT     *F,*N,*N
         MOVE      C0,LINES
         ADD       C1,PAGE
         RETURN
.        ELSE                   .TOTALS TYPE REPORT
.        PRINT     *F,*N,*N
.         MOVE      C0,LINES
.         ADD       C1,PAGE
.         RETURN
.        ENDIF
...............................................................................
EOJ22
         COMPARE   "55" TO LINES
         CALL      OH1 IF NOT LESS
         MOVE      MASK TO MASKA
         EDIT      TOTAL TO MASKA
         MOVE      PICSW TO NUM
         BRANCH    NUM OF EOJ22A,EOJ22B,EOJ22C,EOJ22ALL
EOJ22A
         PRINT     *L,*L,*16,"*************************":
                   *41,"****************************",*L:
                   *16,"TOTAL PAYABLE NAMES:",*50,MASKA
         GOTO      EOJ22D
.
EOJ22B
         PRINT     *L,*L,*16,"*************************":
                   *41,"****************************",*L:
                   *16,"TOTAL RECEIVABLE NAMES:",*50,MASKA
         GOTO      EOJ22D
.
EOJ22C   PRINT     *L,*L,*L
         GOTO      EOJ22D
.
EOJ22ALL compare   "52" to lines
         call      oh1 if not less
         MOVE      MASK TO MASKA
         EDIT      GRNDTOT1 TO MASKA
         PRINT     *L,*L,*L:
                   *09,"TOTAL NAMES:",*41,MASKA;
         MOVE      MASK TO MASKA
         EDIT      GRNDTOT2 TO MASKA
         PRINT     *53,MASKA
         GOTO      EOJ22D
EOJ22D
         MOVE      C0 TO TOTAL
         PRINT     *FLUSH;
         ADD       C4 TO LINES
         CALL      PAGENUM
         BEEP
         GOTO      ENDPRT
. .............................................................................
.
EXIT
         CLOSE     INPUT
         shutdown  "CLS"
         STOP
ERROR    DISPLAY   *P03:24,*EL,FMESG,"NOT ON LINE PLEASE INFORM COMPUTER ":
                   "PERSONEL";
         KEYIN     *P30:24,*EOFF,STR1;
         CMATCH    "Q",STR1
         GOTO      EXIT IF EQUAL
         GOTO      ERROR
.
INT
         TRAPCLR   INT
         NORETURN
         TRAP      INT IF INT
         TRAPCLR   F1
INT1
         DISPLAY   *P1:1,*ES,*B,*B,*B,*B,*HON:
                   *P8:4,"**********************************************":
                   *P8:5,"*   INT. ERROR CALL COMPUTER PERSONNEL.":    *
                   *P8:6,"**********************************************",*HOFF
         PAUSE     C1
         BEEP
.         CONSOLE   *P14:1,"INT. ERROR"
         PAUSE     C1
         BEEP
         KEYIN     *P30:24,*EOFF,STR1;
         CMATCH    "I",STR1
         GOTO      EXIT IF EQUAL
         GOTO      INT1
.Patch4.0
                              include compio.inc
                              include   cntio.inc
.         INCLUDE   NMLRIO.inc
.Patch4.0
         include   nxrfio.inc
         include   ndatio.inc
         include   nordio.inc
.START PATCH 3.0 - ADDED LOGIC
         INCLUDE   NOFRIO.INC
.END PATCH 3.0 - ADDED LOGIC
.START PATCH 3.9 ADDED LOGIC
          INCLUDE   NSEL2IO.INC
.END PATCH 3.9 ADDED LOGIC
.START PATCH 4.02 ADDED LOGIC
          INCLUDE   NXNGIO.INC
          INCLUDE   NXCHIO.INC
.END PATCH 4.02 ADDED LOGIC
          INCLUDE   COMLOGIC.inc