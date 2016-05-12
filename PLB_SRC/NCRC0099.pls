PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
release  init      "1.3"        29SEP99 ASH NEW SERVER ADDED
.release  init      "1.2"        29Nov99 ASH Small fixes
.release  init      "1.1"        7Apr98 DLH new keys and year 2000 compliance
.release  init      "1.0"
FILE6    iFILE    keyl=7
file2    ifile    keyl=6
KEY      DIM      7
key6     dim       6
key7     dim       7
AKEY1    DIM       3                  USED TO BUILD MAILER & LIST FILE AIM KEY.
PASS     DIM       5
NAME     DIM       20
KEYLEN   FORM      2
ANS      DIM       1
lr       dim       6
NcrcFLD1 DIM       10      nordfle2 AIM KEY 1 MLR
dim2     dim        2
PASSCD   move      "NCRC0003" TO PROGRAM
         MOVE      "NAMES IN THE NEWS" TO COMPNME
         MOVE      "DELETE CORR/CANC RECORDS" TO STITLE
         CALL      PAINT
         KEYIN     *P1:4,"PLEASE ENTER PASSWORD: ",*EOFF,PASS,*EON
         RESET     PASS TO 4
         RESET     PASS
         MATCH     "COSMO",PASS
         GOTO      END1 IF NOT EQUAL
         TRAP      END1 IF INT
         MOVE       "EXIT" TO PF5
         CALL       FUNCDISP
         TRAP      END1 IF F5
.START PATCH 1.3 REPLACED LOGIC
.        execute    "f:\netutils\sunidxnt g:\DATA\text\ninord3.DAT,g:\DATA\index\ninord3b, -1-6,15"
        pack    taskname,NTWKPATH2,"sunidxnt ",NTWKPATH1,"text\ninord3.DAT,",NTWKPATH1,"index\ninord3b, -1-6,15"
        execute TASKNAME
.END PATCH 1.3 REPLACED LOGIC
. 
QUES     DISPLAY   *P1:7,"NUMBER:":
                      *P1:8,"(this number WILL NOT be zero-filled or right":
                      "-justified....enter exact key)",*EF

K6       KEYIN     *P8:7,KEY7
.         BUMP      KEY BY 7
         DISPLAY   *P20:7,"AFTER BUMP, KEY=",KEY
         MOVE      KEY7,KEY
         DISPLAY   *P50:7,"AFTER MOVE, KEY=",KEY
         OPEN      FILE6,"NINORD3b"
         OPEN      FILE2,"NINORD3"
         TRAP      NOFILE GIVING ERROR IF IO
         FILEPI    1;FILE6
.         PACK      NcrcFLD1 FROM AKEY1,key7
         read       file6,key7;lr,cc,yy,mm,dd,str1
.         readkg      file6;ncrcfld1,lr,cc,yy,mm,dd,str1
         DISPLAY   *EL,*P10:10,"KEY ",KEY,"    ",b1,str1,b1,mm,slash,dd,slash,cc,yy
         KEYIN     *EL,*P1:22,"IS THIS RECORD YOU WANT ??",ANS
         goto      again if eos
         CMATCH    "*" TO ANS
         GOTO      ques IF EQUAL
.START PATCH 1.2 -REPLACED LOGIC
         CMATCH    "N" TO ANS
         GOTO      ques IF EQUAL
         CMATCH    "Y" TO ANS
         GOTO      KILL6 IF EQUAL
.         IF (ANS = "Y")
.                   MOVE KEY,LR
.                   GOTO KILL6
.         ENDIF
.END PATCH 1.2 -REPLACED LOGIC
         filepi    1;file6
.         readkgp    FILE6;lr,cc,yy,mm,dd,str1
         match      lr to key        hit ??
         goto      keyseq if not equal
show     DISPLAY   *EL,*P10:10,"KEY ",KEY,"    ",b1,str1,b1,mm,slash,dd,slash,cc,yy
AGAIN    KEYIN     *EL,*P1:22,"IS THIS RECORD YOU WANT ??",ANS
         goto      again if eos
         CMATCH    "*" TO ANS
         GOTO      ques IF EQUAL
         CMATCH    "Y" TO ANS
         GOTO      KILL6 IF EQUAL
         filepi    1;file6
keyseq   
.         readkgp    FILE6;cc,yy,mm,dd,str1
         goto     norec if over
         match     lr to key
         goto      norec if not equal
         goto      show
KILL6
         filepi    6;file6,file2
.         READ      FILE6,KEY;;
.         DISPLAY   *EL,*P10:10,"KEY ",KEY,"    ",b1,ans,b1,mm,slash,dd,slash,yy
.         KEYIN     *EL,*P1:22,"are you sure ??",str1
.        CMATCH    "Y" TO str1
.         GOTO       keyseq IF not EQUAL
          pack      key6 from lr
         display   *p10:14,key6,*w3
.         bump      key by 7
.         move      key6 to key
./         READ      FILE6,KEY6;;
./         if        over
./         display   *p10:15,"WE ARE OVER",*w3
./         goto      end1
./         endif
         DELETE   FILE6,key7
         TRAPCLR   IO
         CLOSE     FILE6
         unpack      key7 into key6,str1
         READ      FILE2,KEY6;;
         goto      next1 if over
         DELETEK   FILE2,key6
next1    KEYIN     *P1:22,*EL,*W4,*P14:22,"Would you like to do another??",STR1
         CMATCH     YES,STR1
         GOTO       K6 IF EQUAL
         GOTO      END1
KEY6     RESET     KEY
         BUMP      KEY BY 13
         MOVE      KEY6 TO KEY
         FILEPI    1;FILE6
         DELETE   FILE6
         TRAPCLR   IO
         CLOSE     FILE6
         GOTO      NOREC IF OVER
         KEYIN     *P1:22,*EL,*W4,*P14:22,"Would you like to do another??",STR1
         CMATCH     YES,STR1
         GOTO       K6 IF EQUAL
         GOTO      END1
NOREC    BEEP
         BEEP
         DISPLAY   *P1:24,*EL,"NO RECORD WITH THIS KEY ON FILE !!!!!",*W;
         DISPLAY   *W,*W;
         GOTO      QUES
.
NOFILE   BEEP
         BEEP
         DISPLAY   *P1:23,*EL,"ERROR IS",ERROR
         DISPLAY   *P1:24,*EL,"FILE NOT ONLINE !!",*W,*W,*W;
         TRAPCLR   IO
END1
         STOP
         include   comlogic.inc
