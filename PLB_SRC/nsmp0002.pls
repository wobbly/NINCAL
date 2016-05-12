.
.
PC             EQU            0
               INCLUDE        COMMON.inc
               INCLUDE        CONS.inc
;patch2.5
                                                  include compdd.inc
                                                  include cntdd.inc
.               INCLUDE        NMLRDD.inc
;patch2.4
               include        nsmpdd.inc
               include        hp.inc
               include        oslspern.inc
          include        PRTPAGEDD.INC

.
.INPUT    IFILE     KEYLEN=29,STATIC=12,dup
INPUT          FILE           STATIC=12
release   init    "2.8"                 08Mar2007  DLH      Oslspern.inc expansion
.release  init      "2.7"       ASH  01NOV2004 Mailer increased to 6 bytes
.release  init      "2.6"       ASH  09AUG2004 Logo Conversion
.release  init      "2.5"       DMB  26MAY2004 Mailer Conversion
.release  init      "2.4"       DLH  01Apr2002 rewrite Gui, prtpage, new options.
.release  init      "2.3"       ASH  02OCT2000 NEW SERVER ADDED
.release  init      "2.2"       ASH  02Nov98 NINSAMPL Y2K, File expansion
.release  init      "2.1"       ASH  24Sep98 NINMLR Y2K File expansion
.release  init      "2.0"       DLH  11Jul95 Really added print by client
.RELEASE   INIT     "1.1"      JD   25apr95  added print by client name.
.RELEASE   INIT     "pre"      DLH  16feb94   new.
.
. ..MISCELLANEOUS FIELDS
.
ReportTitle    Dim            50
SlsFlag        Dim            1
Page           FORM      "   0"    PAGE NUMBER
LINCTR         FORM      2         LINE COUNTER
holddes        dim       35
.START PATCH 2.7 REPLACED LOGIC
.HOLDMLR        DIM       4         CHECKS FOR NEW CLIENT NUMBER
HOLDMLR        DIM       6         CHECKS FOR NEW CLIENT NUMBER
.END PATCH 2.7 REPLACED LOGIC
Holdsls        Dim            2
COHEAD         DIM       5         COMPANY HEADING
DOT60          INIT      "............................................................"
RECOUNT        FORM      4         NUMBER OF RECORDS READ.
DATE           DIM       8
pass           form      1        1=1 2=2
SortVar        Dim            300
sales          dim     15
+
.Colors
white   color
grey    color
.START PATCH 1.21 ADDED VARS
RED       COLOR
BLACK     COLOR
Yellow    Color
.END PATCH 1.21 ADDED VARS

.Set Up Menu Bar
mFile   menu
mEdit   menu
         CLOCK     DATE TO DATE
         MOVE      DATE TO TODAY
         TRAP      ABORT IF F5
.         MOVE      "MINI SAMPLE BOOK PRINT " TO STITLE
               MOVE      "Names In The News Ca Inc" TO COMPNME
.         MOVE      "Nsmp0002" TO PROGRAM
               MOVE      "Nsmp0002" TO WPROGNme
.         TRAP      NOISAM GIVING ERROR IF IO
               move           c1 to pass
.START PATCH 2.7 REPLACED LOGIC
.               move           c1 to nmlrpath
               move           c1 to COMPpath
.END PATCH 2.7 REPLACED LOGIC
.Set Vars used for About Box
               move           "NSMP0002.PLS",Wprognme
               move           "Master Sample Description Print Program",Wfunction
               move           "David Herrick",Wauthor
               move           release,Wrelease
               move           "April 1, 2002",Wreldate
               Move           Yes to Slsflag
.START PATCH 2.6 ADDED LOGIC
.NINLogo  PICT
          CREATE    NINLogo=3:13:30:50:
                    "\\nins1\e\netutils\NIN logo black outline.jpg"
.END PATCH 2.6 ADDED LOGIC
open
          prtopen   Laser,"",WPrognme
          prtpage   Laser;*UNITS=*HIENGLISH;
          prtpage   Laser;*ORIENT=*Portrait;
         TRAP      NOFILE IF IO
         TRAPCLR   IO
.         MOVE      "N I N",COHEAD
               Prepare        Input,"c:\work\Samples.wrk",exclusive
Looper0
               Call           NsmpSeq
               goto           Sort0 if over
.START PATCH 2.7 REPLACED LOGIC
.               pack           mkey from nsmpmlr,z3
.               rep            zfill in mkey
.               call           nmlrkey
.               rep            zfill in Mslsper
.               cmatch         b1 to Mslsper
.               If             Eos
.               move           "00" to Mslsper
.               endif
.               write          input,seq;MSLSPER,Mcomp,Nsmpnum,nsmpvars
..................................................
               pack           COMPFLD,nsmpmlr
               rep            zfill in COMPFLD
               call           COMPkey
               rep            zfill in COMPCONTACT
               cmatch         b1 to COMPCONTACT
               If             Eos
               move           "00" to COMPCONTACT
               endif
               write          input,seq;COMPCONTACT,COMPCOMP,Nsmpnum,nsmpvars
.END PATCH 2.7 REPLACED LOGIC
               goto           looper0
Sort0
               weof           input,seq
               close          input
          clear     SortVar
          append    "c:\work\Samples.wrk,c:\work\Samples.srt;",sortvar
               if             (slsflag = Yes)        .they wanted Sls info
.START PATCH 2.7 REPLACED LOGIC
.         append    "1-2,3-45,46-48",SortVar
.               else
.         append    "3-45,46-48",SortVar
                              append    "1-2,3-57,58-60",SortVar
               else
                              append    "3-57,58-60",SortVar
.END PATCH 2.7 REPLACED LOGIC
               endif
          reset     SortVar
          sort      SortVar
          if over
                    alert     Caution,Sortvar,result
                    alert     caution,S$ERROR$,result
                    return
          endif
               Open           INput,"c:\work\samples.srt",exclusive
...............................................................................
looper1
.START PATCH 2.7 REPLACED LOGIC
.         Read     INput,seq;MSLSPER,Mcomp,Nsmpnum,nsmpvars
          Read     INput,seq;COMPCONTACT,COMPCOMP,Nsmpnum,nsmpvars
.END PATCH 2.7 REPLACED LOGIC
...............................................................................
         GOTO      EOJ IF OVER
               If             (recount = c0)
               call           Header
                              If             (SlsFlag = Yes)
                              call           PrntSls
                              endif
               endif
         cmatch    b1 to nsmpmlr
         if        equal
.START PATCH 2.7 REPLACED LOGIC
.         move     "0000" to nsmpmlr
         move     "000000" to nsmpmlr
.END PATCH 2.7 REPLACED LOGIC
         endif

         cmatch    b1 to nsmpmlr
         if        eos
.START PATCH 2.7 REPLACED LOGIC
.         move     "0000" to nsmpmlr
         move     "000000" to nsmpmlr
.END PATCH 2.7 REPLACED LOGIC
         endif

.START PATCH 2.7 REPLACED LOGIC
.               If             (SlsFlag = yes & Mslsper <> HoldSls)
               If             (SlsFlag = yes & COMPCONTACT <> HoldSls)
.END PATCH 2.7 REPLACED LOGIC
                    prtpage   Laser;*p=1:10450,"Page ",page
                         prtpage        Laser;*NEWPAGE;
                              call           Header
                              call           prntSls
               endif
         match     nsmpmlr to holdmlr
         if        not equal
         move      nsmpmlr to holdmlr
         call      prntcomp
         endif
         ADD       c1 TO RECOUNT
.        DISPLAY   *P10:14,"Number of records read: ",RECOUNT
         call      prntsmpl
         goto      Looper1
..............................................................................
HEADER
               Move           "   M A S T E R   S A M P L E   F I L E  ",ReportTitle
.START PATCH 2.6 REPLACED LOGIC
.         prtpage   Laser;*ROWSpaceE=0,*COLSPACE=0,*Overlayon:
.                   *p=25:102,*font=PRTpg10B,"Names  ":
.                   *font=PRTpg10i," in the News":
.                   *p=5:240,*Line=1260:240:
.                   *p=10:250,*font=PRTpg10,"C a l i f o r n i a    I n c .":
.                   *p=3500:250,ReportTitle,*p=7200:250,Today
.               PrtPage        Laser;*p=50:525,*font=PRTpg10,"Company",*p=6500:525,"Mlr##"
.               PrtPage        Laser;*p=100:725,*font=PRTpg10,"Sample ## & Description"
.               PrtPage        Laser;*p=100:925,*font=PRTpg10,"Date of Sample",*p=6500:925,"Date Entered"
.    
.               ADD            c1,Page
.               move           "1200" to Row
          prtpage   Laser;*ROWSpaceE=0,*COLSPACE=0,*Overlayon:
                    *Pictrect=*off,*PICT=0:800:0:5000:NINLogo:
                    *p=3500:250,*font=PRTpg10,ReportTitle,*p=7200:250,Today
               PrtPage        Laser;*p=50:1025,*font=PRTpg10,"Company",*p=6500:1025,"Mlr##"
               PrtPage        Laser;*p=100:1225,*font=PRTpg10,"Sample ## & Description"
               PrtPage        Laser;*p=100:1425,*font=PRTpg10,"Date of Sample",*p=6500:1425,"Date Entered"

               ADD            c1,Page
               move           "1700" to Row
.END PATCH 2.6 REPLACED LOGIC
               RETURN
..............................................................................
PRNTCOMP
          if             (row >= 10000)
          prtpage   Laser;*p=1:10450,"Page ",page
          prtpage   Laser;*NEWPAGE;
          call      header
               endif

.START PATCH 2.7 REPLACED LOGIC
.         prtPage   Laser;*p=1:Row,*font=Prtpg10B,Mcomp,*font=Prtpg10,*p=3500:row,dot60,*p=6500:row,NsmpMlr
          prtPage   Laser;*p=1:Row,*font=Prtpg10B,COMPCOMP,*font=Prtpg10,*p=3500:row,dot60,*p=6500:row,NsmpMlr
.END PATCH 2.7 REPLACED LOGIC
          add       "200",row

              return
. 
..............................................................................
PRNTSLS
.START PATCH 2.7 REPLACED LOGIC
.               move           MSLSPER,HowMany
               move           COMPCONTACT,HowMany
.END PATCH 2.7 REPLACED LOGIC
               move           osls0,sales
               load           sales from HowMany of osls1,osls2,osls3,osls4,osls5,osls6,osls7:
                              osls8,osls9,osls10,osls11,osls12,osls13,osls14,osls15,osls16:
                              osls17,osls18,osls19,osls20,osls21,osls22,osls23,osls24,osls25:
                          osls26,osls27,osls28,osls29,osls30,osls31,osls32,osls33,osls34,osls35
               PrtPage        Laser;*p3000:row,"Sales Person: ##",Mslsper,b2,Sales
.START PATCH 2.7 REPLACED LOGIC
.               mOVE           mSLSpER TO Holdsls
               mOVE           COMPCONTACT TO Holdsls
.END PATCH 2.7 REPLACED LOGIC
          add       "200",row
               return
..............................................................................
PRNTsmpl
          if             (row >= 10000)
          prtpage   Laser;*p=1:10450,"Page ",page
          prtpage   Laser;*NEWPAGE;
          call      header
.START PATCH 2.7 REPLACED LOGIC
.         prtPage   Laser;*p=1:Row,*font=Prtpg10B,Mcomp,*font=Prtpg10,*p=3500:row,dot60,*p=6500:row,NsmpMlr
          prtPage   Laser;*p=1:Row,*font=Prtpg10B,COMPCOMP,*font=Prtpg10,*p=3500:row,dot60,*p=6500:row,NsmpMlr
.END PATCH 2.7 REPLACED LOGIC
          add       "200",row
               endif

               unpack         nsmpdte into str2,yy,mm,dd
               PrtPage        Laser;*p350:Row,Nsmpnum,b4,NsmpDes1,b1,Nsmpdes2
               add            "200" to row
               PrtPage        Laser;*p750:Row,mm,slash,dd,slash,str2,yy
               unpack         nsmpdate into str2,yy,mm,dd
               PrtPage        Laser;*p6500:Row,mm,slash,dd,slash,str2,yy
          add       "200",row
         return
..............................................................................
. 
ABORT    TRAPCLR   F5
         NORETURN
               PrtPage        Laser;*p1:row,"*****************JOB ABORTED BY OPERATOR"
          add       "200",row
               PrtPage        Laser;*p1:row,"*****************JOB ABORTED BY OPERATOR"
          add       "200",row
               PrtPage        Laser;*p1:row,"*****************JOB ABORTED BY OPERATOR"
EOJ
         BEEP
                    prtpage   Laser;*p=1:10450,"Page ",page
               PrtClose       laser
SHUTDOWN
        CLOSE     INPUT,delete
         STOP
..............................................................................
NOFILE
. DISPLAY   *P1:24,*EL,"FILE NOT FOUND",*B,*B,*W30
         STOP
         INCLUDE   NsmpIO.inc
;patch2.5
                                                  include compio.inc
                                                  include cntio.inc
.         INCLUDE   NMLRIO.inc
;patch2.5
         INCLUDE   COMLOGIC.inc

