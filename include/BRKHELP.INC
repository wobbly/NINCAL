.............................................................................
.
.ENTER WITH - TOPWIND,BOTWIN,LWIND,RWIND -  TO DEFINE WINDOW.
.EXIT  WITH - brkNUM, brCOMP,  NEED TO MOVE TO OTHER VARIABLES AND RETURN AT   .
.                           EXIT PARAGRAPH.
.
. sep 93 DLH.
.............................................................................
brkHELP  CLEAR     STR2
         MOVE      TOPWIND TO STR2
         TYPE      STR2    *VALID NUMBER?
         CALL      bsetWIN IF NOT EQUAL
         CALL      bsetWIN IF EOS
         COMPARE   C0 TO TOPWIND
         CALL      bsetWIN IF NOT GREATER
         DISPLAY   *SETSWALL  TOPWIND:BOTWIND:LWIND:RWIND;
         KEYIN     *P1:1,*ES:
                   *P1:6,*EL,"I WILL HELP YOU FIND THE Broker NUMBER",*R:
                   *P1:6,*EL,"THAT YOU WANT":
                   *R,*P1:6;
         goto      ffloat
         MOVE      "01L" TO str3
KEYbrk   KEYIN     *P1:1,*ES,"TYPE IN Broker NAME : ":
                   *P1:2,*DV,brCOMP:
                   *P1:2,*T60,*EDIT,brCOMP
         MOVE      brcomp TO STR24
         CMATCH    "*" IN brcomp
         GOTO      brkAEXT1 IF EQUAL
         KEYIN     *P1:2,*DV,brcomp,*P1:3,"OK ? ",*T60,STR1:
                   *P1:3,*EL;
         CMATCH    NO TO STR1
         GOTO      KEYbrk IF EQUAL
         CMATCH    B1 TO brcomp
         GOTO      notnof IF EQUAL
         GOTO      notnof IF EOS
         MATCH     "01L" TO str3
         GOTO      packb1 IF EQUAL
         PACK      nbrkfld2 FROM str3,brcomp
         CALL      chkbkey
         GOTO      brkEADA
packb1   PACK      nbrkfld2 FROM str3,brcomp,QUEStion,question,question
         CALL      chkbkey
         GOTO      brkEADA
ffloat   MOVE      "01F" TO str3
         GOTO      KEYbrk
brkEADA
         CLEAR     brknum
         MOVE      "*****NO brknum FOUND*****" TO brcomp
         CALL      NbrkAIM
         IF        OVER
         MOVE      "*****NO brknum FOUND*****" TO brcomp
         GOTO      brkAEXIT
         clear     str4
         clear     brknum
         else
         MOVE      brknum TO STR4
         ENDIF
         GOTO      DISbrkA
chkbkey   MOVELPTR  nbrkfld2 TO n2
         GOTO      notnof IF ZERO
         COMPARE   C6 TO n2
         GOTO      notnof IF LESS         *NOT ENOUGH INFO, LET THEM KNOW
         RETURN                             *OK, RETURN
brkEADA1 CLEAR     brknum
         MOVE      "*****NO brknum FOUND*****" TO brcomp
         CALL      NbrkKG
         IF        OVER
.         MOVE      STR24 TO brcomp
         clear     brfax
         GOTO      brkAEXIT
         ENDIF
         MATCH     brknum TO STR4
         GOTO      brkEADA1 IF EQUAL
         MOVE      brknum TO STR4
         GOTO      DISbrkA
brkEADA2 CLEAR     brknum
         MOVE      "*****NO brknum FOUND*****" TO brcomp
         IFZ       PC
         CALL      NbrkKGP
         XIF
         IF        OVER
.         MOVE      STR24 TO brcomp
         GOTO      brkAEXIT
         ENDIF
         MATCH     brknum TO STR4
         GOTO      brkEADA2 IF EQUAL
         MOVE      brknum TO STR4
         GOTO      DISbrkA
DISbrkA  DISPLAY   *P1:6,*EL,"##",brknum,"  ",brcntct:
                    *p8:7,brcomp;
         ENDSET    brcomp
         IFZ       PC
         KEYIN     *P1:5,"ENTER TO CONTINUE, (P)rior  (*) TO END",*T254,STR1;
         XIF
         IFNZ      PC
         KEYIN     *P1:5,"ENTER TO CONTINUE  (*) TO END",*T254,STR1;
         XIF
         CMATCH    STAR TO STR1
         GOTO      brkAEXT1 IF EQUAL
         CMATCH    "P" TO STR1
         GOTO      brkEADA2 IF EQUAL
         GOTO      brkEADA1
.............................................................................
notnof   DISPLAY   *P1:24,*BLINKON,*HON,*EL,"NOT ENOUGH INFO TO SEARCH":
                   *HOFF;
         GOTO      brkHELP
.............................................................................
brkAEXIT 
         clear     brknum
         MOVE      YES TO OVER
brkAEXT1 DISPLAY   *P1:1,*ES,*RESETSW:
                   *P20:24,*HON,"ENTER (*) TO EXIT, (<) TO BACKUP":
                   " or (?) FOR HELP",*HOFF;
.         MOVE      STR24 TO brcomp
         RETURN
................
bsetWIN  MOVE      "18" TO TOPWIND    NO VALUES GIVEN,SET TO DEFAULTS
         MOVE      C24 TO BOTWIND
         MOVE      "40" TO LWIND
         MOVE      C80 TO RWIND
         RETURN
............................................................................
