PC       EQU       0
         INC       COMMON.inc
         INCLUDE   CONS.inc
         include   hp.inc
         INC       NMOADD.inc
         INCLUDE   NMOBDD.inc

RELEASE  INIT      "1.6"           DLH use pdf print driver & email results
Reldate   Init      "02 October 2012"
.RELEASE  INIT      "1.501"           DLH 08OCT2004 Print straight to printer
.RELEASE  INIT      "1.5"            ASH 02OCT2000 NEW SERVER ADDED
.RELEASE  INIT      "1.4"            JD modified to use brk/mlr key.
.RELEASE  INIT      "1.3"           DLH 23JUL93   FIX TO NMOAIO
.RELEASE         INIT      "1.2"           DLH 07MAY92    NMOAXX, NMOBXX.
.
.RELEASE  INIT      "1.1"          DLH 12MAR92    CONVERT TO PCBUS
.

.begin patch 1.6
Laser    pfile
.Define Fonts to be used
font1     font
font2     font
font3     font
FileCheck FIle
trapcount form      4
.end patch 1.6


time     init      "  :  :  "
funcbr   form       1
.V        FORM      "07"         VERTICAL DISPLAY VARIABLE
break    dim       8
savecnt  dim       3
CHANGE   FORM      7.2         CHANGE TO BE APPLIED TO BALANCE.
SAVEAMNT FORM      7.2
SELECT   FORM      2           BRANCH FOR REASON
SEQ3     FORM      "-3"
SAMEMLR  DIM       7           USED TO VERIFY STILL READING CORRECT ACCOUNT
SAMEacct DIM       8           USED TO VERIFY STILL READING CORRECT ACCOUNT
holdit   dim       8
FORM7    FORM      7           NUMERIC WORK VARIABLE USED FOR ENTRY NUMBER
REVMODE  FORM      1           "1" = BALANCE ONLY, "2" = DETAILS.
DATE     DIM       8
RN       FORM      "05"        FOR RECNUM FILE READ
KEY      DIM       6           USED FOR INVOICE RECORD READ
HOLD     DIM       7
seqcount FORM      7
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
.REASON TABLE.
.............
.REAS1    INIT      "OVERPAYMENT"
.REAS2    INIT      "UNIDENT. PAYMNT"
.REAS3    INIT      "INV. CANCELLED"
.REAS4    INIT      "MISSING"
.REAS5    INIT      "REFUND"
.REAS6    INIT      "UNUSED CREDIT"
.REAS7    INIT      "USING CREDIT"
.REAS8    INIT      "TRF ENTRY"
.REAS9    INIT      "ADV PAY POLITIC"
.REAS10   INIT      "ADV PAY NEW MLR"
.REAS11   INIT      "PAYMENT ON A/C"
.REAS12   INIT      "FROM BOOKS"
.REAS13   INIT      "DUP. PAYMENT"
.REAS14   INIT      "WRITEOFF"
.REAS99   INIT      "ENTRY CORRECTN"
.RDESC    DIM       15
.begin patch 1.6
.PAGE     FORM      "0001"
PAGE     FORM      "0000"
.end patch 1.6
LINES    FORM      2
lastrec  dim       1
+..............................................................................
.MAIN
...............................................................................
         MOVE      "NONA0006" TO PROGRAM
         MOVE      "NAMES IN THE NEWS CAL" TO COMPNME
         MOVE      "VERIFY MOA" TO STITLE
.begin patch 1.6
NINLogo   PICT
          CREATE    NINLogo=3:13:30:50:
                    "\\nins1\e\netutils\NIN logo black outline.jpg"
          move      "250",column
          move      "1000",column1
          move      "4200",column2
          move      "5200",column3
          move      "6200",column4
          create    font1,"Arial",size=14,bold
          create    font2,"Arial",size=11
          create    font3,"Arial",size=10
        create  font10,"Times New Roman",size=24,bold
        create  font11,"Times New Roman",size=24,italic
        create  font12,"Times New Roman",size=12
        create  font13,"Arial",size=9
.end patch 1.6

.START PATCH 1.5 REPLACED LOGIC
.         SPLOPEN   "g:\data\BADMOA.LST"
.;         PACK      STR35,NTWKPATH1,"BADMOA.LST"
.;         SPLOPEN   STR35
.begin patch 1.6
          Call      PDF995Auto
          PRTOPen   Laser,"PDF995","Nona0006.pdf"
.         splopen        "\\NINs2\Laser8","R"
.end patch 1.6

.END PATCH 1.5 REPLACED LOGIC
         move      c7 to v
         CLOCK     DATE TO TODAY
         clock     time to time
         MOVE      C4 TO NMOAPATH
         move      c2 to nmobpath
         MOVE      "EXIT" TO PF5
         TRAP      exit IF F5
         CALL      PAINT
         CALL      FUNCDISP
         KEYIN     *CL
.begin patch 1.6
         CALL      HD
          add       eightlpi,row
          add       eightlpi,row
          prtpage   Laser;*pcolumn:row,*font=font3,*boldoff,"---Pass One---"
          add       eightlpi,row
.         print     *40,hpitalic,"Pass One",hpuprght
.end patch 1.6
         move      c1 to str1
input    keyin     *p1:12,*cyan,"1",*white,")Verify":
                   *p1:13,"2)Create new balance file":
                   *p1:14,*t60,*RV,str1
         move      c0 to funcbr
         move      str1 to funcbr
         call      paint
         call      funcdisp
         branch    funcbr of menu,create
         goto      input          
MENU     
         display   *p1:5,"Verify mode"
         MOVE      NO TO OVER
         move      c0 to detcnt
         CALL      NMOBks
         GOTO      DONE IF OVER
         CMATCH    YES TO OVER
         GOTO      DONE IF EQUAL
         ADD       C1 TO INPUT
         PACK      NMOAFLD4 FROM nmobbrk,nmobmlr
         DISPLAY   *P10:12,"MASTER RECORDS IN ",INPUT, " account ",nmoafld4
         MOVE      NO TO OVER
         CALL      NMOAKEY
         goto      BAD IF over
         add       c1 to detcnt
         DISPLAY   *P10:13,"DETAIL RECORD  ",DETCNT," record ",nmobmlr,slash,nmobbrk
         ADD       ONAMOUNT TO TOTAL
         ADD       INAMOUNT TO TOTAL
MENUA  
         CALL      NMOAKS
         GOTO      CHKTOT IF OVER
         PACK      sameacct FROM nmoabrk,mlr
         REP       ZFILL IN sameacct
         MATCH     sameacct TO NMOAFLD4
         GOTO      CHKTOT IF NOT EQUAL           .DLH 23JUL93
         add       c1 to detcnt
         DISPLAY   *P10:13,"DETAIL RECORD  ",DETCNT," record ",mlr,slash,nmoabrk
         ADD       ONAMOUNT TO TOTAL
         ADD       INAMOUNT TO TOTAL
         GOTO      MENUA
CHKTOT   COMPARE   TOTAL TO BALANCE
         CALL      BAD2 IF NOT EQUAL
         MOVE      C0 TO TOTAL
         GOTO      MENU  
.begin patch 1.6
BAD      
.          COMPARE   "60" TO LINES
.         CALL      HD IF not LESS
          if (row >= 10220)
          prtpage Laser;*NEWPAGE;
          call    HD
          endif
          add       eightlpi,row
          prtpage   Laser;*pcolumn:row,*font=font3,*boldoff,"NO DETAIL : ",NMOAFLD4;
.         PRINT     *L,*1,"NO DETAIL : ",NMOAFLD4
.end patch 1.6
         ADD       C2 TO LINES
         ADD       C1 TO NODET
         DISPLAY   *P10:14,"RECORDS WITHOUT DETAIL ",NODET
         GOTO      MENU
.
.begin patch 1.6
BAD2      
.          COMPARE   "60" TO LINES
.         CALL      HD IF not LESS
.         PRINT     *L,*1,"OUT OF BALANCE : ",NMOAFLD4," DETAIL(S): ",TOTAL:
.                   "BALANCE FILE: ",BALANCE
          if (row >= 10220)
          prtpage Laser;*NEWPAGE;
          call    HD
          endif
         ADD       C2 TO LINES
         ADD       C1 TO OUTOFBAL
          add       eightlpi,row
          prtpage   Laser;*pcolumn:row,*font=font3,*boldoff,"OUT OF BALANCE : ",NMOAFLD4," DETAIL(S): ",TOTAL," BALANCE FILE: ",BALANCE
.end patch 1.6
          DISPLAY   *P10:16,*EL,"OUT OF BALANCE ",OUTOFBAL
         RETURN
.DONE - END OF PASS ONE
DONE     
.Begin patch 1.6
.         COMPARE   "60" TO LINES
.         CALL      HD IF not LESS
.         print     *L,*1,"Start time             : ",time:
.                   *L,*1,"MASTER RECORDS INPUT   : ",INPUT:
.                   *L,*1,"RECORDS W/O DETAIL     : ",nodet:
.                   *L,*1,"RECORDS OUT OF BALANCE : ",OUTOFBAL
.         PRINT     *FLUSH
          if (row >= 10220)
          prtpage Laser;*NEWPAGE;
          call    HD
          endif
          add       eightlpi,row
          prtpage   Laser;*pcolumn:row,*font=font3,*boldoff,"Start time             : ",time
          add       eightlpi,row
          prtpage   Laser;*pcolumn:row,*font=font3,*boldoff,"MASTER RECORDS INPUT   : ",INPUT
          add       eightlpi,row
          prtpage   Laser;*pcolumn:row,*font=font3,*boldoff,"RECORDS W/O DETAIL     : ",nodet
          add       eightlpi,row
          prtpage   Laser;*pcolumn:row,*font=font3,*boldoff,"RECORDS OUT OF BALANCE : ",OUTOFBAL
.end patch 1.6
        move       c0 to detcnt
pass2
         display   *p10:10,"pass 2"
         close     nmoafle4
         move      c0 to nmoaflg4
         clear     nmoafld4
         move      c0 to detcnt
         move      c0 to input
         clear     sameacct
.Begin patch 1.6
.         COMPARE   "60" TO LINES
.         CALL      HD IF not LESS
.         print    *l,hpitalic,"---Pass Two---",hpuprght        
          if (row >= 10220)
          prtpage Laser;*NEWPAGE;
          call    HD
          endif
          add       eightlpi,row
          add       eightlpi,row
          prtpage   Laser;*pcolumn:row,*font=font3,*boldoff,"---Pass Two---"
          add       eightlpi,row
          add       eightlpi,row
.end patch 1.6
loop2    
         match     yes to lastrec
         goto      printtot if equal
         clear     mlr
         clear     nmoabrk
         CALL      NMOAKS
         GOTO      ALLDONE IF over
.         match     yes to lastrec
.         goto      printtot if equal
         add       c1 to detcnt
         DISPLAY   *P10:13,"DETAIL RECORD  ",DETCNT," record ",mlr,slash,nmoabrk
         pack      nmoafld4 from nmoabrk,mlr
         rep       zfill in nmoafld4
         compare    c1 to detcnt
         if        equal              1st account
         move      nmoafld4 to sameacct
         endif
         match     nmoafld4 to sameacct
         goto      chktot2 if not equal
         ADD       ONAMOUNT TO TOTAL
         ADD       INAMOUNT TO TOTAL
         GOTO      loop2

CHKTOT2  
         move      no to over
         move      nmoafld4 to holdit
         move      sameacct to nmoafld4
         rep       zfill in nmoafld4
         call      nmobkey
         CMATCH    YES TO OVER
         GOTO      nobal IF EQUAL
         ADD       C1 TO INPUT
         DISPLAY   *P10:12,"MASTER RECORDS IN ",INPUT, " account ",nmoafld4
         COMPARE   TOTAL TO BALANCE
         CALL      BAD2 IF NOT EQUAL
         MOVE      C0 TO TOTAL                     .clear totals
         move      holdit to sameacct              .restore for next break
.        pack      nmoafld4 from nmoabrk,mlr
.         rep       zfill in nmoafld4
.         move      nmoafld4 to sameacct
         ADD       ONAMOUNT TO TOTAL              .start new totals
         ADD       INAMOUNT TO TOTAL
         GOTO      loop2         
.Begin patch 1.6
nobal    
.          COMPARE   "60" TO LINES
.         CALL      HD IF not LESS
.         PRINT     *L,*1,"NO BALANCE RECORD: ",NMOAFLD4," DETAIL(S): ",TOTAL
          if (row >= 10220)
          prtpage Laser;*NEWPAGE;
          call    HD
          endif
          add       eightlpi,row
          prtpage   Laser;*pcolumn:row,*font=font3,*boldoff,"NO BALANCE RECORD: ",NMOAFLD4," DETAIL(S): ",TOTAL
.end patch 1.6
         ADD       C2 TO LINES
         ADD       C1 TO nomastr
         DISPLAY   *P10:16,*EL,"NO MASTER BALANCE RECORDS ",NOMASTR
         MOVE      C0 TO TOTAL
         move      holdit to sameacct
         ADD       ONAMOUNT TO TOTAL
         ADD       INAMOUNT TO TOTAL
         goto      loop2
.begin patch 1.6
.HD       PRINT     *F,*L,*L,*35,"NONA0006 ",*60,"Date: ",TODAY:
.                   *L,*25,"CHECKING BAD MOA ACCOUNTS",*60,"Page: ",PAGE
.         ADD      C1 TO PAGE
.         MOVE     C4 TO LINES
HD
.Starting point for first row set here - if this changes border value needs to change!!!!!!!
          add       C1,page
          prtpage   Laser;*UNITS=*HIENGLISH;
          move      "300",row
          prtpage   Laser;*p7000:50,*font=font2,*uloff,"page ",page;
          prtpage   Laser;*Pictrect=*off,*PICT=0:1300:500:8100:NINLogo
          add       eightlpi,row
          add       eightlpi,row
          add       eightlpi,row
          add       "60",row
          add       eightlpi,row
          add       eightlpi,row
          add       eightlpi,row
          add       eightlpi,row
          add       eightlpi,row
          prtpage   Laser;*p3000:row,*font=font1,"CONFIDENTIAL";
          add       eightlpi,row
          add       eightlpi,row
          prtpage   Laser;*p2800:row,*font=font2,"CHECKING BAD MOA ACCOUNTS";
          add       twelvelpi,row
          add       twelvelpi,row
          prtpage   Laser;*pcolumn:row,"Date: ",today
          add       eightlpi,row
.end patch 1.6

         RETURN

alldone  move      yes to lastrec
         goto      chktot2
printtot
.begin patch 1.6
.          COMPARE   "60" TO LINES
.         CALL      HD IF not LESS
.         PRINT     *L,*1,"Detail RECORDS INPUT   : ",INPUT:
.                   *L,*1,"Accounts W/O Balance     : ",nomastr:
.                   *L,*1,"RECORDS OUT OF BALANCE : ",OUTOFBAL
.         PRINT     *FLUSH
          if (row >= 10220)
          prtpage Laser;*NEWPAGE;
          call    HD
          endif
          add       eightlpi,row
          add       eightlpi,row
          prtpage   Laser;*pcolumn:row,*font=font3,*boldoff,"Detail RECORDS INPUT   : ",detcnt
          add       eightlpi,row
          prtpage   Laser;*pcolumn:row,*font=font3,*boldoff,"Accounts W/O Balance     : ",nomastr
          add       eightlpi,row
          prtpage   Laser;*pcolumn:row,*font=font3,*boldoff,"RECORDS OUT OF BALANCE : ",OUTOFBAL
.end patch 1.6
         move      c0 to nmoaflg3
         move      c0 to nmoaflag
         display   *p10:10,"pass 3"
         move      c0 to nmoapath
.begin patch 1.6
          if (row >= 10220)
          prtpage Laser;*NEWPAGE;
          call    HD
          endif
          add       eightlpi,row
          add       eightlpi,row
          prtpage   Laser;*pcolumn:row,*font=font3,*boldoff,"---Pass Three---"
          add       eightlpi,row
          add       eightlpi,row
.end patch 1.6

pass3 
         call      nmoaseq
         goto      end3 if over
         add       c1 to seqcount
         DISPLAY   *P10:16,"Seq DETAIL RECORD  ",seqcount," record ",mlr,slash,nmoabrk
         goto      pass3
end3    
.begin patch 1.6
.         COMPARE   "60" TO LINES
.         CALL      HD IF not LESS
.         PRINT     *L,*1,"Detail RECORDS count isam   : ",detcnt:
.                   *L,*1,"Detail record count Seq     : ",seqcount:
.                   *l
.         PRINT     *FLUSH
          if (row >= 10220)
          prtpage Laser;*NEWPAGE;
          call    HD
          endif
          add       eightlpi,row
          add       eightlpi,row
          prtpage   Laser;*pcolumn:row,*font=font3,*boldoff,"Detail RECORDS count isam   : ",detcnt
          add       eightlpi,row
          prtpage   Laser;*pcolumn:row,*font=font3,*boldoff,"Detail record count Seq     : ",seqcount
          add       eightlpi,row
.end patch 1.6
.START PATCH 1.5 REPLACED LOGIC
.         open      testfile,"g:\data\text\ninmob"

          PACK      STR255 from "NINMOB.dat|10.10.30.103:502"
         open      testfile,STR255
.END PATCH 1.5 REPLACED LOGIC
         move      c0 to seqcount
         display   *p10:10,"pass 4"
.begin patch 1.6
          if (row >= 10220)
          prtpage Laser;*NEWPAGE;
          call    HD
          endif
          add       eightlpi,row
          prtpage   Laser;*pcolumn:row,*font=font3,*boldoff,"---Pass Four---"
          add       eightlpi,row
.end patch 1.6
         
pass4
         read      testfile,seq;str1
         goto      end4 if over
         add       c1 to seqcount
         DISPLAY   *P10:18,"Seq Bal RECORD  ",seqcount," record "
         goto      pass4
end4    
.begin patch 1.6
.         COMPARE   "60" TO LINES
.         CALL      HD IF not LESS
         clock     time to time
.         PRINT     *L,*1,"Balance RECORDS count isam   : ",input:
.                   *L,*1,"Balance record count Seq     : ",seqcount:
.                   *l,*l,*1,"end time ",time
          if (row >= 10220)
          prtpage Laser;*NEWPAGE;
          call    HD
          endif
          add       eightlpi,row
          add       eightlpi,row
          prtpage   Laser;*pcolumn:row,*font=font3,*boldoff,"Balance RECORDS count isam   : ",input
          add       eightlpi,row
          prtpage   Laser;*pcolumn:row,*font=font3,*boldoff,"Balance record count Seq     : ",seqcount
          add       eightlpi,row
          prtpage   Laser;*pcolumn:row,*font=font3,*boldoff,"end time ",time
.exit     PRINT     *FLUSH
exit     Prtclose   Laser
.         splclose
CheckFile
          pack      MailAttach from "c:\WORK\pdf\nona0006.pdf"
          Move      MailAttach,Str55
          trap      WaitForEnd giving error if IO
          open      FileCheck,STR55,Exclusive     
          Close     FIleCHeck
          trap      IOMssg GIVING ERROR IF IO


          move      "MOA Verification Report",MailSubjct
          Clear     MailBody
          append    str45,mailbody
          append    CRLF,Mailbody
          Reset     MailBOdy
                             
          pack      Mailto from "DavidHerrick@nincal.com,GemmaSpranza@nincal.com"
          pack      MailFrom from "CReques@nincal.com"
          Call      SendMail
          erase     mailattach
          Call      PDF995Auto0
.end patch 1.6
         stop
*******************************************************************************
create   
         move      c1 to prio
         call      paint
.;what the DLH 4March 2004
.START PATCH 1.5 REPLACED LOGIC
.         open      nmoafle3,"g:\data\text\ninmoa.dat"
.;         PACK      STR35,NTWKPATH1,"ninmoa.dat"
.;         open      nmoafle3,STR35
               Move           c4 to Nmoapath
.;what the DLH 4March 2004
.END PATCH 1.5 REPLACED LOGIC
         call      funcdisp
         display   *p1:5,"Create mode"
.START PATCH 1.5 REPLACED LOGIC
.         prepare   testfile,"g:\data\text\ninmob.new"
         PACK      STR35,NTWKPATH6,"ninmob.What"
         prepare   testfile,STR35
.END PATCH 1.5 REPLACED LOGIC
         move      c0 to n7
loopie   
.;what the DLH 4March 2004
.;READ      NMOAFLE3,SEQ;moavars
               Call           Nmoaks
.;what the DLH 4March 2004
         goto      eoj if over
         add       c1 to n7
         display   *p1:12,*el,"records in ",n7
         compare   c1 to n7 
         if        equal
         pack      break from mlr,nmoabrk
         move      mcnt to savecnt
         endif
         pack      nmoafld from mlr,nmoabrk
         display   *p1:12,*el,"records in ",n7,b1,nmoafld
         match     nmoafld to break        
         goto      newbal if not equal
         ADD       ONAMOUNT TO TOTAL
         ADD       INAMOUNT TO TOTAL
         goto      loopie
newbal   move      c0 to balance
         move      total to balance
         unpack    break into nmobmlr,nmobbrk
         move      savecnt to nmobmcnt
         WRITE     testfile,seq;nmobmlr,nmobmcnt,balance,nmobbrk
         pack      break from mlr,nmoabrk
         move      mcnt to savecnt
         add       c1 to n5
         display   *p1:14,"new balances written ",n5
         move       c0 to total
         ADD       ONAMOUNT TO TOTAL
         ADD       INAMOUNT TO TOTAL
         goto      loopie

eoj            pack           nmoafld from mlr,nmoabrk
               ADD            ONAMOUNT TO TOTAL
               ADD            INAMOUNT TO TOTAL
               move           c0 to balance
               move           total to balance
               unpack         Nmoafld into nmobmlr,nmobbrk
               Move           Mcnt to NmobMcnt
               WRITE     testfile,seq;nmobmlr,nmobmcnt,balance,nmobbrk
               display   *p1:14,"new balances written ",n5
               weof     testfile,seq
               close    testfile
               stop
.begin patch 1.6
WaitForEnd
                    TrapClr   IO
.check the error if file does not exist just get out
                    add       C1,Trapcount
                    move      "3000",str4
                    call      waitin using str4
                    noreturn
                   if        (trapcount > 60)   . 5 min are you kidding me. clearly not waiting 5 min
                    Pack       MailSubjct,"MOA Verification Report - "
                    append    CRLF,MailBOdy
                    append    Mailattach,MailBody
                    append    CRLF,MailBOdy
                    append    "I am sorry I could not send the file",Mailbody
                    reset     Mailbody
                    Clear     Mailto
                    Pack      MailTO,"CReques@nincal.com"
                    Move      B1,Mailattach
                    call      SendMail
                    return
                    
                    endif
          
                    goto      checkfile
.end patch 1.6

         INCLUDE   NMOAIO.inc
         INCLUDE   NMOBIO.inc
         INCLUDE   COMLOGIC.inc

