.
.
.  INPUT FILE IS SORTED
.
PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
           INCLUDE   NDATDD.inc
         INCLUDE   NOWNDD.inc
         INCLUDE   HP.inc
.START PATCH 1.9 ADDED LOGIC
          INCLUDE   NCATDD.INC
          INCLUDE   NREFDD.INC
.END PATCH 1.9 ADDED LOGIC
RELEASE  INIT      "2.1"         ASH 07APR2005    COMMPER Conversion
.RELEASE  INIT      "2.0"         DMB 11JUN2004   Added code to account for report for DE cards used in past week not upd past 3mos with order qty usage for year
.RELEASE  INIT      "1.9"         ASH 30JAN2004  DATACARD CONVERSION
.RELEASE  INIT      "1.8"         JD LASER
.RELEASE  INIT      "1.7"        DLH 29SEP92    GENERAL CLEANUP, NO OWNER OPTION
.                                       USE DSINIT.
.RELEASE  INIT      "1.6"       DLH 16MAR92    NWONXX INCLUDES.
.RELEASE  INIT      "1.5"
.               02/21/86 -- INCREASE EFFICIENCY OF READS ....
.               01/16/86 -- CHANGE CATAGORY CODES TO 3 BYTES.
.               05/07/85 -- ADD EXCLUDE WITHDRAWN OPTION.
.               08/18/83 -- ADD KILL FILE & REPRINT OPTIONS.
. LAST REVISION 05/19/83 -- SPEED UP PRINT BY MULTIPLE LINE OUTPUT
.                          & ADDED PRINT WITHDRAWN IF FIELD01=(W)
NAME     DIM       19 (FILE NAME)
INPUT    FILE      fix=126 (INPUT  FILE)
LINECT   FORM      2           LINE COUNTER
PAGE     FORM      3           PAGE NUMBER
LR       FORM      "0"         '1'=LAST RECORD PENDING
LRTOT    FORM      7          GRAND TOTAL
DATE     DIM       8
.START PATCH 1.9 REPLACED LOGIC
.LINE1    DIM       126
.LINE2    DIM       126
.LINE3    DIM       126
.LINE4    DIM       126
.LINE5    DIM       126
LINE1    DIM       150
LINE2    DIM       150
LINE3    DIM       150
LINE4    DIM       150
LINE5    DIM       150
.END PATCH 1.9 REPLACED LOGIC
LINE1A    DIM       5
LINE2A    DIM       5
LINE3A    DIM       5
LINE4A    DIM       5
LINE5A    DIM       5
PRSW     FORM      "0"
ONAME    DIM       15
PSTATUS   DIM       4
BLANK126 DIM       126
COUNT    FORM      "00000"
EXSW     DIM       1           * "Y" = DO NOT PRINT WITHDRAWN CARDS.
SW1P     INIT      "Y"         * "Y" = 1ST PASS.
*............................................................
.
EXCL     DIM       4                    EXCLUSIVE PRINT FIELD
PRTFLAG    FORM      1
OWNFLAG  FORM          1
WITHFLAG FORM      1
LASRFLAG FORM      1
NINPPATH FORM      1
.
.patch2.0
dim11a    dim       11
.patch2.0
+............................................................
.
. FILE OPENING SEQUENCE
         TRAP      TRAP IF F5
         MOVE      "EXIT" TO PF5
         TRAP      TRAP IF INT
           CMATCH    B1 TO PROGRAM          .CHAINED FROM DSINIT?
           IF        EOS                    .NO
         MOVE      "Names In The News" TO COMPNME
         MOVE      "NDAT0020" TO PROGRAM
           CLEAR      COMMENT
           MOVE       "LOCAL" TO PRTNAME
           ENDIF
         MOVE      "MASTER DATACARD LISTING" TO STITLE
.         DISPLAY   *P1:1,*ES,*P25:01,*BLINKON:
.                   *HON,"MASTER DATACARD PRINT",*HOFF
         CLOCK     DATE TO DATE
           IFNZ        PC
         UNPACK    DATE INTO MM,DD,YY
           XIF
           IFZ         PC
           UNPACK    DATE INTO MM,STR1,DD,STR1,YY
           XIF
         REP       ZFILL,DD
         REP       ZFILL,MM
         CLEAR     TODAY
         PACK      TODAY FROM MM,SLASH,DD,SLASH,YY
         MOVE      "Options" TO PF4
         TRAP      OPTGET IF F4
         CALL      PAINT
         CALL      FUNCDISP
         TRAP      IO IF IO
.
         TRAPCLR   IO
         MOVE      C0 TO COUNT
         MOVE      NO TO STR1
BEGIN    DISPLAY   *P01:05,"Options     :":
                   *P01:06,"Input File  :":
                   *P01:07,"Print File  :":
                   *P01:09,"Record Count:"
.
         GOTO      OPTION
OPTGET
         RESET     COMMENT
         KEYIN     *P20:10,"WITHDRN:  INCLUDE WITHDRAWN CARDS":
                       *P20:11,"NOWNER :  EXCLUDE OWNER INFO     ":
                   *P20:12,"LASER  :  PRINT ON LASER":
                   *P01:24,*EL,"The Option setting is invalid.":
                   *P15:05,COMMENT:
                   *P20:10,*EL,*P20:11,*EL,*P20:12,*EL,*P20:13,*EL:
                   *P20:14,*EL,*P20:15,*EL;
         MATCH     "                         " TO COMMENT
         GOTO      OPTDEFLT IF EQUAL
OPTION
           MOVE        C0 TO WITHFLAG
         RESET     COMMENT
         SETLPTR   COMMENT
         DISPLAY   *P15:05,COMMENT
         MATCH     "                         " TO COMMENT
         GOTO      OPTDEFLT IF EQUAL
         RESET     COMMENT
           SCAN      "WITHDRN" IN COMMENT
           CALL      OPTWITH IF EQUAL
           RESET       COMMENT
           SCAN        "NOWNER" IN COMMENT
           CALL      OPTOWN IF EQUAL
           SCAN      "LASER" IN COMMENT
           CALL      OPTLASER IF EQUAL
         GOTO      INPGET
OPTNG    KEYIN         *P20:10,"WITHDRN:  INCLUDE WITHDRAWN CARDS":
                       *P20:11,"NOWNER :  EXCLUDE OWNER INFO     ":
                   *P20:12,"LASER  :  PRINT ON LASER":
                   *P01:24,*EL,"The Option setting is invalid.":
                   *P15:05,COMMENT:
                   *P20:10,*EL,*P20:11,*EL,*P20:12,*EL,*P20:13,*EL:
                   *P20:14,*EL,*P20:15,*EL;
         GOTO      OPTGET
OPTDEFLT
           MOVE      C1 TO OWNFLAG
           MOVE      C1 TO WITHFLAG
         MOVE      C1 TO LASRFLAG
         GOTO      INPGET
OPTWITH    MOVE        C2 TO WITHFLAG
           RETURN
OPTOWN     MOVE        C2 TO OWNFLAG
           RETURN
OPTLASER MOVE      C2 TO LASRFLAG
         RETURN
.
INPGET   TRAP      INPNG GIVING ERROR IF IO
         BRANCH    NINPPATH TO PRTGET
         OPEN      TESTFILE,INPNAME
         TRAPCLR   IO
         CLOSE     TESTFILE
           MOVE        C1 TO NINPPATH
         DISPLAY   *P15:06,INPNAME
         MOVE      INPNAME TO NAME
         OPEN      INPUT,INPNAME,EXCLUSIVE
         GOTO      PRTGET
INPNG    NORETURN
         TRAPCLR   IO
         KEYIN     *P01:24,*EL,"The Input file is not on-line. ":
                   *DV,ERROR:
                   *P15:06,INPNAME
         GOTO      INPGET
.
PRTGET   MATCH     B8 TO PRTNAME
         GOTO      PRTNG IF EQUAL
         MOVE      C1 TO PRTFLAG
         MATCH     "LOCAL"  TO PRTNAME
         GOTO      START IF EQUAL
         MOVE      C2 TO PRTFLAG
         PACK      PRTFILE WITH pdrive,PRTNAME
         SPLOPEN   PRTFILE
         DISPLAY   *P15:07,PRTNAME
         GOTO      START
PRTNG    KEYIN     *P01:24,*EL,"Print File answer is invalid.":
                   *P15:07,PRTNAME
         GOTO      PRTGET
.         KEYIN     *P10:10,*EL,"Do you want to spool the output ? ",*RV,*T120:
.                   STR1
.         CMATCH    NO TO STR1
.         GOTO      EXCLUDE IF EQUAL
.         IFNZ      PC
.         SPLOPEN   "DATAMRH/PRT:PRINT"
.         DISPLAY   *P10:10,*EL,"Your output file is 'DATAMRH/PRT'";
.         XIF
.         IFZ       PC
.         SPLOPEN   "g:\DATA\DATAMRH.PRN"
.         DISPLAY   *P10:10,*EL,"Your output file is 'DATAMRH'";
.         XIF
.EXCLUDE
.         MOVE      NO TO STR1
.         KEYIN     *P10:12,*EL,"Do you want to exclude Withdrawn cards? ":
.                   EXSW," OK? ",*RV,*T30,STR1;
.         CMATCH    YES TO STR1
.         GOTO      EXCLUDE IF NOT EQUAL
START
         CALL      HEADER
.
*............................................................
. READ A RECORD FROM THE FILE
.
READ     DISPLAY   *P1:24,*EL,*HON,"READING";
.START PATCH 1.9 REPLACED LOGIC
.          READ      INPUT,SEQ;STATUS,LSTNUM,OWNNUM,CATCDE1,CATCDE2:
.                   CATCDE3,CATCDE4,CATCDE5,CATCDE6,CATCDE7,CATCDE8,CATCDE9:
.                   CATCDE10,NLSTCDE,ELSTCDE,COMMPER,HOTLINE,NEWDATE:
.                   REVDATE,PASSWORD,MLSTNAME,UNIVERSE,TEXT1
.;patch2.0
.START PATCH 2.1 REPLACED LOGIC
.         if (func = "2")
.                   READ      INPUT,SEQ;STATUS,LSTNUM,OWNNUM,NLSTCDE,ELSTCDE,COMMPER,HOTLINE,NEWDATE:
.                             REVDATE,PASSWORD,MLSTNAME,UNIVERSE,DIM11A
.         else
.                   READ      INPUT,SEQ;STATUS,LSTNUM,OWNNUM,NLSTCDE,ELSTCDE,COMMPER,HOTLINE,NEWDATE:
.                             REVDATE,PASSWORD,MLSTNAME,UNIVERSE
.         endif
          if (func = "2")
                    READ      INPUT,SEQ;STATUS,LSTNUM,OWNNUM,NLSTCDE,ELSTCDE,str3,HOTLINE,NEWDATE:
                              REVDATE,PASSWORD,MLSTNAME,UNIVERSE,DIM11A
          else
                    READ      INPUT,SEQ;STATUS,LSTNUM,OWNNUM,NLSTCDE,ELSTCDE,str3,HOTLINE,NEWDATE:
                              REVDATE,PASSWORD,MLSTNAME,UNIVERSE
          endif
.END PATCH 2.1 REPLACED LOGIC
.;Patch2.0
.END PATCH 1.9 REPLACED LOGIC
         DISPLAY   *P1:24,*EL;
*............................................................
. TEST FOR END OF FILE
. IF END OF FILE: TURN ON LR AND L1-L9
.
         GOTO      RIDON IF NOT OVER
        CALL       OWNER
         CALL      SETLR
         GOTO      TOTCALC
*............................................................
. TURN ON LEVEL-INDICATOR SWITCHES
.
SETLR    MOVE      C1 TO LR
         RETURN
*............................................................
.
RIDON
         ADD       C1 TO COUNT
         DISPLAY   *P15:12,*EL,"RECORDS READ = ",COUNT;
        CALL       OWNER
+............................................................
. TOTAL CALCULATIONS
.
TOTCALC  TRAPCLR   PARITY  (NOP)
TOTCLX   TRAPCLR   PARITY  (NOP)
         CALL      TOTOUT
*............................................................
. SEE IF LR INDICATOR IS ON
. IF SO: END JOB
.
TESTLR   BRANCH    LR OF EOJ
         GOTO      MOVEDATA  (NOP)
*............................................................
. MOVE DATA FROM INPUT AREA TO FIELDS
.
MOVEDATA
         MOVE      B4 TO EXCL        CLEAR EXCLUSIVE PRINT FIELD.
*............................................................
. DETAIL CALCULATIONS
.
DETCALC  MOVE      "        ",PSTATUS
         CMATCH    "W",STATUS
         CALL      WITHDRAW IF EQUAL
         CMATCH    "T" TO STATUS
         CALL      TEMPWITH IF EQUAL
          if        (elstcde = "C" or Elstcde = "P")
.         CMATCH    "C" TO ELSTCDE        EXCLUSIVE?
         CALL      EXCL IF EQUAL         YES
          endif
         ADD       C1,PRSW
         ADD       C1 TO LRTOT
         GOTO      DETLOAD
* ...........................................................
WITHDRAW
.         CMATCH    YES TO EXSW
           BRANCH    WITHFLAG OF SKIP
.         GOTO      SKIP IF EQUAL
         MOVE      "WDRN",STATUS
         RETURN
TEMPWITH MOVE      "TMPW" TO STATUS
         RETURN
EXCL     MOVE      "EXCL" TO STATUS
         RETURN
SKIP
         NORETURN
         GOTO      READ
*............................................................
DETLOAD
. LOAD PRINT LINE
         DISPLAY   *P1:24,*EL,*HON,"LOADING PRINT BUFFER";
.START PATCH 1.9 ADDED LOGIC
          unpack    REVDATE,CC,YY,MM,DD
          pack      str10,MM,SLASH,DD,SLASH,CC,YY
.
          packkey   taskname,taskname
          clear     taskname
          pack      NCATFLD1,"01X",LSTNUM
          move      "NCATAIM",Location
          pack      KeyLocation,"Key: ",NCATFLD1
          move      C0,N1
          call      NCATAIM
          loop
                    until over
                    pack      NREFFLD,"T",NCATCODE,NCATNUM
                    move      "NREFKEY",Location
                    pack      KeyLocation,"Key: ",NREFFLD
                    call      NREFKEY
                    if not over
                              if (N1 <> C0)
                                        append    COMMA,taskname
                              endif
                              pack      str3,NCATCODE,NCATNUM
                              append    str3,taskname
                              move      C1,N1
                    endif
                    move      "NCATKG",Location
                    pack      KeyLocation,"Key: ",NCATFLD1
                    call      NCATKG
          repeat
          reset     taskname

.END PATCH 1.9 ADDED LOGIC
         BRANCH    PRSW OF LINE1,LINE2,LINE3,LINE4,LINE5
LINE1
.START PATCH 1.9 REPLACED LOGIC
.         PACK      LINE1 FROM STATUS,B1,LSTNUM,B1,MLSTNAME,OWNNUM:
.                   ONAME,B3,REVDATE,B1,CATCDE1,",",CATCDE2,",":
.                   CATCDE3,",",CATCDE4,",",CATCDE5,",",CATCDE6,",",CATCDE7
          MOVE      MLSTNAME,STR55
.Patch2.0
          if (func = "2")
         PACK      LINE1 FROM STATUS,B1,LSTNUM,B1,STR55,OWNNUM:
                   ONAME,B3,str10,B1,dim11A,b2,taskname
          else
         PACK      LINE1 FROM STATUS,B1,LSTNUM,B1,STR55,OWNNUM:
                   ONAME,B3,str10,B1,taskname
          endif
.patch2.0
.END PATCH 1.9 REPLACED LOGIC
.                   ",",CATCDE8,",",CATCDE9
.         PACK      LINE1A FROM ",",CATCDE10
         GOTO      LOADEXIT
.
LINE2
.START PATCH 1.9 REPLACED LOGIC
.         PACK      LINE2 FROM STATUS,B1,LSTNUM,B1,MLSTNAME,OWNNUM:
.                   ONAME,B3,REVDATE,B1,CATCDE1,",",CATCDE2,",":
.                   CATCDE3,",",CATCDE4,",",CATCDE5,",",CATCDE6,",",CATCDE7
          MOVE      MLSTNAME,STR55
.Patch2.0
          if (func = "2")
                     PACK      LINE2 FROM STATUS,B1,LSTNUM,B1,STR55,OWNNUM:
                   ONAME,B3,str10,B1,dim11A,b2,taskname
          else
                     PACK      LINE2 FROM STATUS,B1,LSTNUM,B1,STR55,OWNNUM:
                   ONAME,B3,str10,B1,taskname
          endif
.Patch2.0
.END PATCH 1.9 REPLACED LOGIC
.                   ",",CATCDE8,",",CATCDE9
.         PACK      LINE2A FROM ",",CATCDE10
         GOTO      LOADEXIT
.
LINE3
.START PATCH 1.9 REPLACED LOGIC
.         PACK      LINE3 FROM STATUS,B1,LSTNUM,B1,MLSTNAME,OWNNUM:
.                   ONAME,B3,REVDATE,B1,CATCDE1,",",CATCDE2,",":
.                   CATCDE3,",",CATCDE4,",",CATCDE5,",",CATCDE6,",",CATCDE7
          MOVE      MLSTNAME,STR55
.Patch2.0
          if (func = "2")
                     PACK      LINE3 FROM STATUS,B1,LSTNUM,B1,STR55,OWNNUM:
                   ONAME,B3,str10,B1,dim11A,b2,taskname
          else
                     PACK      LINE3 FROM STATUS,B1,LSTNUM,B1,STR55,OWNNUM:
                   ONAME,B3,str10,B1,taskname
          endif
.Patch2.0
.END PATCH 1.9 REPLACED LOGIC
.                   ",",CATCDE8,",",CATCDE9
.         PACK      LINE3A FROM ",",CATCDE10
         GOTO       LOADEXIT
LINE4
.START PATCH 1.9 REPLACED LOGIC
.         PACK      LINE4 FROM STATUS,B1,LSTNUM,B1,MLSTNAME,OWNNUM:
.                   ONAME,B3,REVDATE,B1,CATCDE1,",",CATCDE2,",":
.                   CATCDE3,",",CATCDE4,",",CATCDE5,",",CATCDE6,",",CATCDE7
          MOVE      MLSTNAME,STR55
.Patch2.0
          if (func = "2")
                     PACK      LINE4 FROM STATUS,B1,LSTNUM,B1,STR55,OWNNUM:
                   ONAME,B3,str10,B1,dim11A,b2,taskname
          else
                     PACK      LINE4 FROM STATUS,B1,LSTNUM,B1,STR55,OWNNUM:
                   ONAME,B3,str10,B1,taskname
          endif
.Patch2.0
.END PATCH 1.9 REPLACED LOGIC
.                   ",",CATCDE8,",",CATCDE9
.         PACK      LINE4A FROM ",",CATCDE10
         GOTO      LOADEXIT
.
LINE5
.START PATCH 1.9 REPLACED LOGIC
.         PACK      LINE5 FROM STATUS,B1,LSTNUM,B1,MLSTNAME,OWNNUM:
.                   ONAME,B3,REVDATE,B1,CATCDE1,",",CATCDE2,",":
.                   CATCDE3,",",CATCDE4,",",CATCDE5,",",CATCDE6,",",CATCDE7
          MOVE      MLSTNAME,STR55
.Patch2.0
          if (func = "2")
                     PACK      LINE5 FROM STATUS,B1,LSTNUM,B1,STR55,OWNNUM:
                   ONAME,B3,str10,B1,dim11A,b2,taskname
          else
                     PACK      LINE5 FROM STATUS,B1,LSTNUM,B1,STR55,OWNNUM:
                   ONAME,B3,str10,B1,taskname
          endif
.Patch2.0
.END PATCH 1.9 REPLACED LOGIC
.                   ",",CATCDE8,",",CATCDE9
.         PACK      LINE5A FROM ",",CATCDE10
         GOTO      LOADEXIT
.
LOADEXIT
         MOVE      B4 TO EXCL
         DISPLAY   *P1:24,*EL;
         ADD       C1 TO LINECT
         COMPARE   C5 TO PRSW
         GOTO      DETOUT IF EQUAL
         GOTO      DETOUT IF NOT LESS
         GOTO      READ
* ...........................................................
. HEADING AND DETAIL OUTPUT
.
DETOUT   DISPLAY   *P1:24,*EL,*HON,"PRINTING";
         COMPARE   "57" TO LINECT
         CALL      HEADER IF NOT LESS
         MOVE      C0,PRSW
         PRINT     *L,*1,LINE1,LINE1A
         PRINT     *1,LINE2,LINE2A
         PRINT     *1,LINE3,LINE3A
         PRINT     *1,LINE4,LINE4A
         PRINT     *1,LINE5,LINE5A;
         PRINT     *FLUSH;
.
         MOVE      BLANK126,LINE1
         MOVE      BLANK126,LINE2
         MOVE      BLANK126,LINE3
         MOVE      BLANK126,LINE4
         MOVE      BLANK126,LINE5
         MOVE      B5 TO LINE1A
        MOVE      B5 TO LINE2A
         MOVE      B5 TO LINE3A
         MOVE      B5 TO LINE4A
         MOVE      B5 TO LINE5A
         DISPLAY   *P1:24,*EL;
*............................................................
. TURN OFF R.I.D. & ALL L-INDICATORS
.
TURNOFF
         COMPARE   C1 TO LR
         GOTO      RETURN IF EQUAL
         NORETURN
*............................................................
. GO READ ANOTHER RECORD
.
         GOTO      READ
* ...........................................................
. GOTO PRINT TOTAL COUNT
RETURN
         RETURN
*............................................................
. PAGE HEADING ROUTINE
.
PAGE     ADD       C1 TO PAGE
PAGE1      BRANCH    LASRFLAG TO PAGE1B,PAGE1A
PAGE1A   COMPARE   C1 TO PAGE
         IF        EQUAL
         PRINT     HP17PTCH,hptop,hpdupl,*F
         ENDIF
page1b
.         PRINT     *F,*C,*L:
         PRINT     *F,*L:
                   *1,"CONFIDENTIAL":
                   *50,"*** NAMES IN THE NEWS MASTER LISTING ***":
                   *118,"PAGE ",PAGE:
                   *L,*118,"DATE: ",TODAY,*FLUSH,*L,*L,*C
         MOVE      C5 TO LINECT
         CMATCH    YES TO SW1P
         CALL      ZEROLINE IF EQUAL
         RETURN
HEADER   CALL       PAGE
           BRANCH     OWNFLAG OF HD1,HD2
HD1
.patch2.0
                              if (func = "2")
                          PRINT     *7,"LIST##":
                   *14,"MASTER LIST NAME":
                   *68,"OWNER INFORMATION":
                   *90,"REVISED":
                   *105,"ORDERS/CATEGORIES";
             PRINT     *FLUSH;
          PRINT     *7,"_____":
                   *14,"________________":
                   *68,"_________________":
                   *90,"________":
                   *105,"_________________":
                    *C,*L
.patch2.0
                              else
                                        PRINT     *7,"LIST##":
                   *14,"MASTER LIST NAME":
                   *68,"OWNER INFORMATION":
                   *90,"REVISED":
                   *105,"CATEGORIES";
                PRINT     *FLUSH;
             PRINT     *7,"_____":
                   *14,"________________":
                   *68,"_________________":
                   *90,"________":
                   *105,"__________":
                    *C,*L
                              endif
         RETURN
HD2
.patch2.0
                              if (func = "2")
                                        PRINT     *7,"LIST##":
                   *14,"MASTER LIST NAME":
                   *90,"REVISED":
                   *105,"ORDERS/CATEGORIES";
                                        PRINT     *FLUSH;
                                        PRINT     *7,"_____":
                   *14,"________________":
                   *90,"________":
                   *105,"_________________":
                    *C,*L
.patch2.0
                              else
                                        PRINT     *7,"LIST##":
                   *14,"MASTER LIST NAME":
                   *90,"REVISED":
                   *105,"CATEGORIES";
                PRINT     *FLUSH;
             PRINT     *7,"_____":
                   *14,"________________":
                   *90,"________":
                   *105,"__________":
                    *C,*L
                              endif
         RETURN
ZEROLINE MOVE      C0 TO LINECT
         MOVE      NO TO SW1P
         RETURN
*............................................................
. TOTAL OUTPUT
.
TOTOUT   TRAPCLR   PARITY  (NOP)
         COMPARE   C1 TO LR
         RETURN    IF NOT EQUAL
         CALL      DETOUT
         COMPARE   "52" TO LINECT
         CALL      PAGE IF NOT LESS
         PRINT      *L,*35,"NUMBER OF RECORDS FOUND ",LRTOT,"****"
         RETURN
*............................................................
. READ OWNER
OWNER    CLEAR     NOWNFLD
           CLEAR     ONAME
           BRANCH    OWNFLAG OF OWN1,OWNX
OWN1
.START PATCH 1.9 REPLACED LOGIC
.         MOVE      OWNNUM,NOWNFLD
          unpack    OWNNUM,str2,NOWNFLD
.END PATCH 1.9 REPLACED LOGIC
           REP       ZFILL IN NOWNFLD
           CMATCH    B1 TO NOWNFLD
           IF        EOS
           MOVE      "NO OWNER NUMBER" TO ONAME
           GOTO      OWNX
           ENDIF
         CALL      NOWNKEY
           IF          NOT OVER
         MOVE      OWNOCPY,ONAME
           ELSE
           MOVE      "NO OWNER FOUND!!!" TO ONAME
           ENDIF
OWNX     RETURN
* ...........................................................
. TRAP - JOB INTERUPTED.
.
TRAP     TRAPCLR   INT
         TRAPCLR   F5
         TRAP      TRAP IF F5
         TRAP      TRAP IF INT
         DISPLAY   *P1:24,*EL,*B,"JOB ABORTED!!!!!",*B,*W2;
         NORETURN
         GOTO      EOJ
*............................................................
. END OF JOB
.
EOJ      PRINT     *F
         CLOSE     INPUT
         SPLCLOSE
         RELEASE
         shutdown     "cls"
         STOP
*...........................................................................
*............................................................
. IO ERROR
.
IO       DISPLAY   *P1:1,*ES,"FILE NOT FOUND ",*B,*B ;
         shutdown  "CLS"
         STOP
         INCLUDE   NOWNIO.inc
.START PATCH 1.9 ADDED LOGIC
          INCLUDE   NCATIO.INC
          INCLUDE   NREFIO.INC
.END PATCH 1.9 ADDED LOGIC
         INCLUDE   COMLOGIC.inc
