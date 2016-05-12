PC       EQU       0
         INCLUDE       COMMON.inc
         INCLUDE   CONS.inc
         include   consacct.inc
         include   hp.inc
         INCLUDE   NadjDD.inc
           INCLUDE   NjstDD.inc
release  init       "1.2"       09/29/2000 ASH - NEW SERVER ADDED
.release  init       "1.1"
.RELEASE  INIT      "1.0"            DLH 21Mar95
.V        FORM      "07"         VERTICAL DISPLAY VARIABLE
break    dim       8
savecnt  dim       3
DATE     DIM       8
.CVTFLD   DIM       10             WORK FIELD USED FOR MP CONVERSION.
.MPCHANGE INIT      "}0J1K2L3M4N5O6P7Q8R9"
.MPCHARS  INIT      "}JKLMNOPQR"   VALID MINUS OVERPUNCH CHARACTERS
NUM10    FORM      10
holdsub  dim       2
HOLD     DIM       7
holdit   dim       7
sameacct dim       7
holdlr   dim       6
samelr   dim       6
seqcount FORM      7
lrincj   form      10.2
arj      form      10.2
FORMAP2j FORM      10.2
ap1j     form      10.2
INPUT    FORM      7
OUTOFBAL FORM      7
NODET    FORM      7
nomastr  form      7
TOTAL    FORM      7.2
detcnt   form      7
HMLR     DIM       4
.
.
.
PAGE     FORM          "0001"
LINES    FORM      2
lastrec  dim       1
+..............................................................................
.MAIN
...............................................................................
         MOVE      "Nadj0007" TO PROGRAM
         MOVE      "NAMES IN THE NEWS CAL" TO COMPNME
         MOVE      "VERIFY Adjustments" TO STITLE
         move      c7 to v
.START PATCH 1.2 REPLACED LOGIC
.           SPLOPEN   "g:\data\BADadj.LST"
        pack    taskname,NTWKPATH1,"BADadj.LST"
           SPLOPEN   taskname
.END PATCH 1.2 REPLACED LOGIC
           CLOCK     DATE TO TODAY
           MOVE      "EXIT" TO PF5
           TRAP      theend IF F5
         CALL      PAINT
           CALL      FUNCDISP
           KEYIN     *CL
           CALL      HD
         print     *40,hpitalic,"Pass One",hpuprght
         move      c1 to str1
         call      paint
         call      funcdisp
MENU     
         goto      pass2
         display   *p1:5,"Verify mode"
         CALL      Nadjks
         GOTO      DONE IF OVER
           ADD       C1 TO INPUT
         PACK      NjstFLD FROM asinvno,c1
           DISPLAY   *P10:12,"MASTER RECORDS IN ",INPUT     
         move      asinvno to sameacct
         CALL      NjstKEY
         goto      BAD IF over
           add       c1 to detcnt
           DISPLAY   *P10:13,"DETAIL RECORD  ",DETCNT
         MATCH     asamnum to jstsubno
         goto      menu if equal                     .only one and we have it
         move      jstsubno to holdsub
MENUA  
           CALL      NjstKS
         GOTO      CHKTOT IF OVER
         MATCH     asinvno to jstinvno
         GOTO      CHKTOT IF NOT EQUAL           .DLH 23JUL93

           add       c1 to detcnt
         move      jstsubno to holdsub
           DISPLAY   *P10:13,"DETAIL RECORD  ",DETCNT
           GOTO      MENUA
CHKTOT   
.         COMPARE   TOTAL TO BALANCE
.          CALL      BAD2 IF NOT EQUAL
           MOVE        C0 TO TOTAL
         match     asamnum to holdsub
         call      bad2 if not equal
           GOTO      MENU      
BAD      COMPARE   "60" TO LINES
           CALL      HD IF not LESS
           PRINT     *L,*1,"No  DETAIL : ",asinvno,b1,asamnum,b1,aslrnum
           ADD       C2 TO LINES
           ADD       C1 TO NODET
           DISPLAY   *P10:14,"RECORDS WITHOUT DETAIL ",NODET
         GOTO      MENU
.
BADar   COMPARE   "60" TO LINES
           CALL      HD IF not LESS
           PRINT     *L,*1,"A/R no match : ",asinvno,b1,asamnum,b1,aslrnum,formar,b1,arj
           ADD       C2 TO LINES
         RETURN
.
BADlr   COMPARE   "60" TO LINES
           CALL      HD IF not LESS
           PRINT     *L,*1,"LRinc no match : ",asinvno,b1,asamnum,b1,aslrnum,lrinc,b1,lrincj
           ADD       C2 TO LINES
         RETURN
.
BADap1   COMPARE   "60" TO LINES
           CALL      HD IF not LESS
           PRINT     *L,*1,"A/1 no match : ",asinvno,b1,asamnum,b1,aslrnum,ap,b1,ap1j
           ADD       C2 TO LINES
         RETURN
.
BADap2   COMPARE   "60" TO LINES
           CALL      HD IF not LESS
           PRINT     *L,*1,"A/P2 no match : ",asinvno,b1,asamnum,b1,aslrnum,formap2,b1,formap2j
           ADD       C2 TO LINES
         RETURN
.
BAD2      COMPARE   "60" TO LINES
           CALL      HD IF not LESS
           PRINT     *L,*1,"Incorrect Number of  DETAILs : ",asinvno,b1,asamnum,b1,aslrnum
           ADD       C2 TO LINES
           ADD       C1 TO NODET
           DISPLAY   *P10:14,"RECORDS WITH wrong DETAIL ",NODET
         RETURN

.DONE - END OF PASS ONE
DONE     
         COMPARE   "60" TO LINES
           CALL      HD IF not LESS
           PRINT     *L,*1,"MASTER RECORDS INPUT   : ",INPUT:
                       *L,*1,"RECORDS W/O DETAIL     : ",nodet:
                     *L,*1,"RECORDS detail nomatch : ",OUTOFBAL
           PRINT     *FLUSH
        move       c0 to detcnt
pass2
.         close     njstfile
.         move      c0 to njstflag
           display   *p10:10,"pass 2"
           move      c0 to detcnt
           move      c0 to input
         COMPARE   "60" TO LINES
           CALL      HD IF not LESS
         print    *l,hpitalic,"---Pass Two---",hpuprght        
loop2    
         match     yes to lastrec
         goto      printtot if equal
         CALL      NjstKS
           GOTO      ALLDONE IF over
           add       c1 to detcnt
           DISPLAY   *P10:13,"DETAIL RECORD  ",DETCNT
           compare    c1 to detcnt
           if        equal              1st account
           move      jstlr to samelr
           endif
           match     jstlr to samelr
           goto      chktot2 if not equal

.         MOVE      jstar TO CVTFLD
.         CALL      CVT
.         MOVE      c0 TO FORM82
.         MOVE      CVTFLD TO FORM82
.         MULTIPLY  ".01"  BY FORM82
.         ADD       FORM82 TO arj
          add       jstar to arj
. 
.         MOVE      jstlrinc TO CVTFLD
.         CALL      CVT
.         MOVE      c0 TO FORM72
.         MOVE      CVTFLD TO FORM72
.         MULTIPLY  ".01"  BY FORM72
.         ADD       FORM72 TO LRINCj
         add        jstlrinc to lrincj
.
.         MOVE      jstap1 TO CVTFLD
.         CALL      CVT
.         MOVE      c0 TO FORM82
.         MOVE      CVTFLD TO FORM82
.         DIV       HUND INTO FORM82
.         ADD       FORM82 TO AP1j
          add       jstap1 to ap1j
.
.         MOVE      jstap2 TO CVTFLD
.         CALL      CVT
.         MOVE      c0 TO FORM82
.         MOVE      CVTFLD TO FORM82
.         DIV       HUND INTO FORM82
.         ADD       FORM82 TO FORMAP2j
          add       jstap2 to formap2j
           GOTO      loop2

CHKTOT2  
         move      jstlr to holdlr
         clear     nadjfld
         pack      nadjfld from samelr
         rep       zfill in nadjfld
           call      nadjkey
         goto      nobal if over
.         MOVE      ASRECADJ TO CVTFLD
.         CALL      CVT
.         MOVE      c0 TO FORM82
.         MOVE      CVTFLD TO FORM82
.         MULTIPLY  ".01"  BY FORM82
.         move       FORM82 TO FORMAR
         compare    asrecadj to arj
         call       badar if not equal
. 
.         MOVE      ASLRINC TO CVTFLD
.         CALL      CVT
.         MOVE      c0 TO FORM72
.         MOVE      CVTFLD TO FORM72
.         MULTIPLY  ".01"  BY FORM72
.         move       FORM72 TO LRINC
         compare    aslrinc to lrincj
         call       badlr if not equal
.
.         MOVE      ASPAYAD1 TO CVTFLD
.         CALL      CVT
.         MOVE      c0 TO FORM82
.         MOVE      CVTFLD TO FORM82
.         DIV       HUND INTO FORM82
.         move       FORM82 TO AP
         compare    aspayad1 to ap1j
         call       badap1 if not equal
.
.         MOVE      ASPAYAD2 TO CVTFLD
.         CALL      CVT
.         MOVE      c0 TO FORM82
.         MOVE      CVTFLD TO FORM82
.         DIV       HUND INTO FORM82
.         move       FORM82 TO FORMAP2
         compare    aspayad2 to formap2j
         call       badap2 if not equal
chktot2b move       c0 to arj
         move       c0 to lrincj
         move       c0 to ap1j
         move       c0 to formap2j

.         MOVE      jstar TO CVTFLD
.         CALL      CVT
.         MOVE      c0 TO FORM82
.         MOVE      CVTFLD TO FORM82
.         MULTIPLY  ".01"  BY FORM82
         ADD       jstar TO arj
. 
.         MOVE      jstlrinc TO CVTFLD
.         CALL      CVT
.         MOVE      c0 TO FORM72
.         MOVE      CVTFLD TO FORM72
.         MULTIPLY  ".01"  BY FORM72
.         ADD       FORM72 TO LRINCj
          add       jstlrinc to lrincj
.
.         MOVE      jstap1 TO CVTFLD
.         CALL      CVT
.         MOVE      c0 TO FORM82
.         MOVE      CVTFLD TO FORM82
.         DIV       HUND INTO FORM82
.         ADD       FORM82 TO AP1j
         add        jstap1 to ap1j
.
.         MOVE      jstap2 TO CVTFLD
.         CALL      CVT
.         MOVE      c0 TO FORM82
.         MOVE      CVTFLD TO FORM82
.         DIV       HUND INTO FORM82
.         ADD       FORM82 TO FORMAP2j
          add       jstap2 to formap2j
         ADD       C1 TO INPUT
           DISPLAY   *P10:12,"MASTER RECORDS IN ",INPUT
         move      holdlr to samelr              .restore for next break
           GOTO      loop2     
.
nobal    COMPARE   "60" TO LINES
           CALL      HD IF not LESS
           PRINT     *L,*1,"NO Summary RECORD: ",jstinvno,b1,jstlr
           ADD       C2 TO LINES
           ADD       C1 TO nomastr
           DISPLAY   *P10:16,*EL,"NO MASTER BALANCE RECORDS ",NOMASTR
           MOVE        C0 TO TOTAL
           move      holdlr to samelr
           goto      chktot2b
HD       PRINT     *F,*L,*L,*35,"Nadj0007 ",*60,"Date: ",TODAY:
                     *L,*25,"CHECKING BAD ADJ ACCOUNTS",*60,"Page: ",PAGE
           ADD        C1 TO PAGE
           MOVE     C4 TO LINES
           RETURN

alldone  move      yes to lastrec
         goto      chktot2
printtot COMPARE   "60" TO LINES
           CALL      HD IF not LESS
           PRINT     *L,*1,"Detail RECORDS INPUT   : ",INPUT:
                       *L,*1,"Accounts W/O Balance     : ",nomastr:
                     *L,*1,"RECORDS OUT OF BALANCE : ",OUTOFBAL
           PRINT     *FLUSH
           display   *p10:10,"pass 3"
         close     njstfile
         move      c0 to njstflag
pass3 
         call      njstseq
         goto      end3 if over
         add       c1 to seqcount
           DISPLAY   *P10:16,"Seq DETAIL RECORD  ",seqcount
         goto      pass3
end3    
         COMPARE   "60" TO LINES
           CALL      HD IF not LESS
           PRINT     *L,*1,"Detail RECORDS count isam   : ",detcnt:
                       *L,*1,"Detail record count Seq     : ",seqcount:
                   *l
           PRINT     *FLUSH
         move      c0 to seqcount
         close     nadjfile
         move       c0 to nadjflag
           display   *p10:10,"pass 4"
         
pass4
         call      nadjseq
         goto      end4 if over
         add       c1 to seqcount
           DISPLAY   *P10:18,"Seq Bal RECORD  ",seqcount," record "
         goto      pass4
end4    
         COMPARE   "60" TO LINES
           CALL      HD IF not LESS
           PRINT     *L,*1,"Balance RECORDS count isam   : ",input:
                       *L,*1,"Balance record count Seq     : ",seqcount:
                   *l
theend     PRINT     *FLUSH
         splclose
           stop

*............................................................
.
.CVT      ENDSET    CVTFLD                        CHECK LAST BYTE.
.         RESET     MPCHARS
.         SCAN      CVTFLD IN MPCHARS             IS IT A MINUSOVRPNCH?
.         GOTO      CVTMP IF EQUAL                YES.
.         RESET     CVTFLD                        NO.
.         TYPE      CVTFLD                        CHECK NUMERIC VALIDITY.
.         RETURN    IF EQUAL                      ITS OK.
.FORMERR  DISPLAY   *P1:23,*EL,*B,"FORMAT ERROR READING LR: ",asLRNum
.         RETURN                                POP THE STACK.
.CVTMP    REPLACE   MPCHANGE IN CVTFLD            CHANGE MP TO NUMBER.
.         RESET     CVTFLD
.         TYPE      CVTFLD                        VALID NUMERIC?
.         GOTO      FORMERR IF NOT EQUAL          NO.
.         MOVE      CVTFLD TO NUM10               MOVE INTO NUMERIC.
.         MULTIPLY  "-1"   BY NUM10               CHANGE TO MINUS.
.         MOVE      NUM10  TO CVTFLD              MOVE BACK TO DIM.
.         RETURN
.
           INCLUDE   NadjiO.inc
           INCLUDE   NjstIO.inc
         INCLUDE   COMLOGIC.inc

