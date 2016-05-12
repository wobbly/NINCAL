.NAMES IN THE NEWS CALIF. COMPUTER VERIFICATION PRINT
.
PC       EQU       0
         INCLUDE   COMMON.INC
         INCLUDE   CONS.INC
         INCLUDE   NMRGDD.INC
.patch1.9
           INCLUDE   compdd.INC
           INCLUDE   cntdd.INC
.           INCLUDE   NMLRDD.INC
.patch1.9
           INCLUDE   NRTNDD.INC
           INCLUDE   NORDDD.INC
.begin patch 2.1           
.         include   ninvdd.inc
          include             ninvdd.inc
.end patch 2.1           
         include   hp.inc
.begin patch 2.7
          include winapi.inc
.end patch 2.7
........................................
RElease   Init      "3.11"     DLH       .reset Row when called from nmrg0001
Reldate   Init      "2014 January 23"
.RElease   Init      "3.1"     DLH       .reset Row when called from nmrg0001
.Reldate   Init      "2013 August 21"
.RElease   Init      "3.0"     DLH       .Convert to Sunbelt PDF
.Reldate   Init      "15 April 2013"
.RElease   Init      "2.9"     DLH       .make callable routine from Nmrg0001 no need for butil
.Reldate   Init      "15 January 2013"
.RElease   Init      "2.8"     DLH       .change to external calls for Pdf995.ini work
.Reldate   Init      "27 February 2012"
.Release   Init      "2.7"     DLH       .Adlt PDF checks
.REldate   Init      "14 September 2010"
.Release   Init      "2.6"     DLH  osflag check on printer open , old way started generating a spool error?
.REldate   Init      "26 May 2010"
.Release   Init      "2.5"     DLH  sendmail
.REldate   Init      "26 Feb 2009"
.Release  init        "2.4"         JD17Sep2007   New var nmrgcnr nmrgdd.inc
.Release  init        "2.3"         DLH PLI
.Reldate  INit      "17 Sep 2007"
.Release  init        "2.2"         JD06OCT2005   New var nmrgdd.inc
.Release            init                "2.1"         DLH 10March2005           Invoice Conversion
.Release  init        "2.0"         JD 25JUNE2004 New var nmrgdd.inc
.Release  init        "1.9"        DMB 26MAY2004  Mailer Conversion
.RELEASE  INIT      "1.8"          ASH 02OCT2000 NEW SERVER ADDED
.RELEASE  INIT      "1.7"          DLH 10Feb00 letterhead form
.RELEASE  INIT      "1.6"          JD 12mar99 updated print for odate.
.RELEASE  INIT      "1.5"         ASH 28Dec98 NINORD Y2K, File expansion
.RELEASE  INIT      "1.4"          JD 23sep96 added hot print option.
.RELEASE  INIT      "1.3"         JD 07FEB96  NO LONGER SUPRESS NMRGCUST
.                                REN NMRGELMX NMRGCNR WHICH CONTAINS REP NAMES
.RELEASE  INIT      "1.2"        jd 17oct95  turned off rep names deduct
.RELEASE  INIT      "1.2"        jd 24aug95 added add of rep names if net order.
.RELEASE  INIT      "1.1"       JD  02FEB95 ADDED CONV DROPS TO PRINT.
.RELEASE  INIT      "1.0"      DLH 06JAN93
........................................
DimPtr    Dim       ^
DimPtr1   Dim       ^
DimPtr2   Dim       ^
FrmPtr    form      ^
FrmPtr1   form      ^
.begin patch 2.5
FileCheck FIle
trapcount form      4
.end patch 2.5

IN         FORM        5
.Start Patch #1.5 - increase var to hold century
.ODATE    DIM       8
ODATE    DIM       10
.end Patch #1.5 - increase var to hold century
PRTFLAG  DIM       1
PERCENT  FORM      4.2
CALCPER  FORM      7.4
DATE     DIM       8
RTNTAB   FORM      3
TOTREJ   FORM      8
TOTNCOA  FORM      8
TOTBILL  FORM      8
REP      DIM       1
SYSMO    DIM       2             current
SYSDAY   DIM       2             
SYSYR    DIM       2             
febdat  form      5
feb      dim       1 
HOTFLAG  FORM       1                 "1=daily print, 2=hot print"
INPUT    FILE    
font1   font
font2   font
font3   font
font4   font
font5   font
.....
font6   font
.begin patch 2.3
FontO7    font
FontM11    font
FontM11B    font
FontO18B  font

.end          create    fontO7,"Times New Roman",size=7
          create    fontO18B,"Times New Roman",size=18,Bold
.end patch 2.3

mss1    plform  Error
FUNCBR   FORM      "0"       PROGRAM CONTROL BRANCH.
....
.END PATCH 1.6 REMMED LOGIC
        formload mss1
.START PATCH 1.6 REMMED LOGIC
.Create fonts to be used
.START PATCH 1.83 REPLACED LOGIC
.        create  font1,"Arial",size=12,bold
.        create  font2,"Arial",size=8 
.        create  font3,"Helvetica",size=9
.        create  font4,"Fixed",size=9
.        create  font5,"Arial",size=10
        create  font1,"Arial",size=12,bold
        create  font2,"Arial",size=9 
        create  font3,"Helvetica",size=9
        create  font4,"Fixed",size=10
        create  font5,"Arial",size=11
.END PATCH 1.83 REPLACED LOGIC
......
        create  font6,"Arial",size=14
prfile  pfile
NINLogo   PICT



begin
          CREATE    NINLogo=3:13:30:50:
                    "\\nins1\e\netutils\NIN logo black outline.jpg"
............INPUT '\DATA\NINMRGE' SORTED BY TYPIST & LR TO 'DAILYMRG'.
           MOVE      "Names In The News" TO COMPNME
           MOVE      "COMP VERIFICATION PRINT" TO STITLE
           CALL      PAINT
          call      GetWinVer
           
         MATCH     "NMRG0002" TO PROGRAM
         IF         NOT EQUAL
           MOVE      "NMRG0002" TO PROGRAM
           MOVE      "COMP VERIFICATION PRINT" TO STITLE
         MOVE      C1 TO hotflag
           MOVE      "dailymrg" TO INPNAME
         ELSE
         unpack    INPNAME TO Nmrgfld,str3
         MOVE      C2 TO hotflag
         MOVE      "HOT CV PRINT" TO STITLE
         MOVE      C1 TO Nmrgpath
         MOVE      NmrgNAME TO INPNAME
         ENDIF
           MOVE        C1 TO NORDPATH
         MOVE      C1 TO NINVPATH
         MOVE      DATE TO TODAY
         CLOCK     DATE TO DATE
         unpack    date into sysmo,str1,sysday,str1,sysyr
         CALL      PAINT
          MOVE      "Exit" TO PF5
         TRAP      END IF F5
         CALL      FUNCDISP
         MOVE      "02" TO MM
         MOVE      "09" TO DD
         MOVE      "96" TO YY
         CALL      CVTJUL
         move      juldays to febdat
         DISPLAY   *P01:06,hotflag,*w4
         MOVE      FUNC TO FUNCBR       
                              call      getwinver
         BRANCH    hotflag OF DLYPRT,HOTPRT
.         
.START PATCH 1.8 REPLACED LOGIC
.DLYPRT   SPLOPEN   "g:\DATA\Nmrg2.LST"
DLYPRT   PACK      STR35,NTWKPATH1,"NMRG2.LST"
          if (osflag = c1 | osflag = c5 | osflag = c6 | osflag = c8 | osflag = c9)         .nt
                    PRTOPEN PrFile,"\\NINs2\Laser2","FAXFILE.PRN"
          elseif (osflag = C3 | osflag = C4)
                    PRTOPEN PrFile,"Laser2","FAXFILE.PRN"
          else   .(osflag = c0)         .Don't know prompt for printer
                    PRTOPEN PrFile,"-","FAXFILE.PRN"
          endif

.         PRTOPEN prfile,"\\NINs2\Laser2","FAXFILE.PRN"
.         SPLOPEN   STR35
.END PATCH 1.8 REPLACED LOGIC
         GOTO       PREPIT
HOTPRT
.                                     
                              branch     funcbr of hotprta,hotprtb
hotprta
          if (osflag = c1 | osflag = c5 | osflag = c6 | osflag = c8 | osflag = c9)         .nt
                    PRTOPEN PrFile,"\\NINs2\Laser2","FAXFILE.PRN"
          elseif (osflag = C3 | osflag = C4)
                    PRTOPEN PrFile,"Laser2","FAXFILE.PRN"
          else   .(osflag = c0)         .Don't know prompt for printer
                    PRTOPEN PrFile,"-","FAXFILE.PRN"
          endif

.PRTOPEN prfile,"\\NINs2\Laser2","FAXFILE.PRN"
                              goto       prepit
.         SPLOPEN    PRTfile
hotprtb
.begin patch 2.5
.begin patch 2.8
.begin patch 3.0
.          Call      PDF995Auto
.          call      SetPDFFlag
.end patch 2.8

.         PRTOPEN     prfile,"Faxfile","FaxFile.prn"
.          PRTOPEN   PrFile,"PDF995","HotCV"
          PRTOPEN   PrFile,"PDF:","c:\work\pdf\HotCV.pdf",FLAGS=4
.end patch 3.0
.Begin patch 2.7 Use the Flag to check if pdf has been created
.begin patch 2.8
.        call       GetPDFPath
.                              pack      str45 from PDFPATH,"\res\pdf995.ini"
.
.                              call      "GU$INI;WRITE_TO_INI" USING str45:
.                                        "Parameters":
.                                        "ProcessPDF":
..                                        "\\nins1\e\apps\plb\code\pdftest.bat":
.                                        "\\nins1\e\apps\winbatch\Del995flag.exe":
.                                        result
.                              if (result = C0)
..Prepare Flag file
.                                        pack      str45 from PDFPATH,"\flag.dat"
.                                        prep      tempfile,str45
.                                        write     tempfile,SEQ;"flag set"
.                                        close     tempfile
.                              endif
..
.                             Call      PDF995Auto
.end patch 2.8
.end patch 2.7

.end patch 2.5
prepit   DISPLAY   *P01:06,"Input File  : ":
                   *P01:07,"Print File  : ":
                   *P01:08,"Input Count : "
.
INPGET   if         (hotflag <> c2)
         TRAP      FILENG IF IO
         OPEN      TESTFILE,INPNAME
         TRAPCLR   IO
         DISPLAY   *P15:06,INPNAME
         CLOSE     TESTFILE
.begin patch 3.11  --- old plb would substitute a font and not kick an error (sometimes) if font was not yet created
          create    FontM11,"Times New Roman",size=11
          create    FontM11B,"Times New Roman",size=11,Bold
.end patch 3.11
          endif
         BRANCH    hotflag OF OPENINP,process
OPENINP  OPEN      input,INPNAME
         GOTO      loop
.
FILENG   NORETURN
         KEYIN     *P01:24,*EL,"The Input file is not on-line. ":
                   *P15:06,INPNAME
         GOTO      INPGET
.
LOOP     READ          INPUT,SEQ;NMRGFLD,STR3
           GOTO        STOP IF OVER
.         
process  MATCH     B1 TO NMRGFLD
         GOTO      LOOP IF EOS
           ADD         C1 TO IN
           DISPLAY   *P15:08,nmrgfld
           MOVE      NMRGFLD TO NORDFLD
           REP       ZFILL IN NORDFLD
           REP       ZFILL IN NMRGFLD
         move      nmrgfld to ninvfld
         rep       zfill in ninvfld
           CALL        NMRGKEY
           CALL      NORDKEY
         call      ninvkey
           MOVE      ORTNNUM TO NRTNFLD
           REP       ZFILL IN NRTNFLD
           CALL      NRTNKEY
         CALL      CNTRTN
           PACK      MKEY  FROM OMLRNUM,OCOBN
           CALL      NMLRKEY
.Start Patch #1.5 - include century
.         PACK      ODATE FROM OODTEM,SLASH,OODTED,SLASH,OODTEY
         PACK      ODATE FROM OODTEM,SLASH,OODTED,SLASH,OODTEC,OODTEY
.End Patch #1.5 - include century
.         compare    c0 to nmrgnnet
.         if         equal
.         MOVE      NMRGNET TO CALCPER
.         else
.         move       c0 to n2
.         move       onetper to n2
.         compare    c0 to n2
.         if         not equal
.         move      nmrgnnet to calcper
.         add        nmrgrep to totrej
.         move       nmrgnnet to nmrgnet
.         endif
.         endif
         MOVE      NMRGNET TO CALCPER
         MOVE      NMRGIQTY TO N7
         DIVIDE    N7 INTO CALCPER
         MULT      "100" BY CALCPER
         MOVE      C0 TO PERCENT
         ADD       CALCPER TO PERCENT
         ADD        NMRGID TO TOTREJ 
         ADD        NMRGERR TO TOTREJ 
         ADD        NMRGDISF TO TOTREJ 
         ADD        NMRGNPER TO TOTREJ 
         ADD        NMRGDMA TO TOTREJ 
         ADD        NMRGZ4 TO TOTREJ 
         ADD        NMRGPRIS TO TOTREJ 
         ADD        NCOAMNF TO TOTREJ 
.start Patch #2.0 - new var.
                              add        nmrgdpv to totrej
.end Patch #2.0 - new var.
.start Patch #2.2 - new var.
                              add        nmrgdisa to totrej
.end Patch #2.4 - new var.
.start Patch #2.4 - new var.
                              add        nmrgcnr to totrej
.end Patch #2.2 - new var.
.         MOVE      "02" TO MM
.         MOVE      "09" TO DD
.         MOVE      "96" TO YY
.         CALL      CVTJUL
.         move      juldays to febdat
         MOVE      invdtem TO MM
         MOVE      invdted TO DD
         MOVE      invdtey TO YY
         CALL      CVTJUL
         compare   juldays to febdat
         goto      nocust if equal
         if        not less
         move      yes to feb
         ADD        NMRGCUST TO TOTREJ  *turned off 2/07/96
         endif
.         add        nmrgelmx to totrej   *turned off 2/07/96
nocust   add        nmrgconv to totrej
         MOVE       NMRGRQTY TO TOTBILL
         SUB        TOTREJ FROM TOTBILL
         add        ncoanix1 to totncoa
         add        ncoanix2 to totncoa
         add        ncoanix3 to totncoa
.begin patch 
.         PRINT     *F;
.         call       PortraitLTRHEAD
.           PRINT     *F,*L,*L,*31,"COMPUTER VERIFICATION"
        prtpage prfile;*UNITS=*HIENGLISH;
.begin patch 2.3               ....Needs to be Ocompid for this one????
          IF        (company = c2)
          prtpage   PrFile;*p=1:25,*font=fontO18b,"Pacific Lists, Inc.":
                    *p=451:343,*font=fontO7,"1300 Clay St. 11th Floor":
                    *p=451:443,"Oakland, CA 94612-1492":
                    *p=317:543,"415-945-9450 ","·"," Fax 415-945-9451":
                    *p=317:643,"A Division of Names in the News"
          else
          prtpage   prfile;*Pictrect=*off,*PICT=0:1300:500:8100:NINLogo
          endif

.         prtpage   prfile;*Pictrect=*off,*PICT=0:1300:500:8100:NINLogo
.end patch 2.3
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
        add     "60",row
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row 
        add     eightlpi,row 
        add     eightlpi,row 
.begin patch 3.0 ????? argh font statement generates a S17 error
.        prtpage prfile;*p3000:row,*font=font5,*boldon,"COMPUTER VERIFICATION",*boldoff;
.        prtpage prfile;*p3000:row,*boldon,"COMPUTER VERIFICATION",*boldoff;
        prtpage prfile;*p3000:row,*font=FontM11B,"COMPUTER VERIFICATION",*font=FontM11;
.end patch 3.0
          add     eightlpi,row 
        add     eightlpi,row 
.        prtpage prfile;*pcolumn:row,"Attention:";
.         PRINT     HPTMSRMN,hpfixed,*n,*n,*n,*n,*n,*n,*n,*n,*n:
.                  *L,*L,*31,"COMPUTER VERIFICATION"
.end patch
        add     eightlpi,row 
        add     eightlpi,row 
        add     eightlpi,row 
        prtpage prfile;*pcolumn:row,"Mailer:";
        prtpage prfile;*p900:row,mcomp;
        prtpage prfile;*p5200:row,"Order Date:";
        prtpage prfile;*p6400:row,odate;
        add     sixlpi,row
        prtpage prfile;*pcolumn:row,"List:";
        prtpage prfile;*p900:row,o1des;
        prtpage prfile;*p5200:row,"LR##";
        prtpage prfile;*p5700:row,olrn;
        add     sixlpi,row 
        add     sixlpi,row 
        prtpage prfile;*pcolumn:row,"Input Qty:";
        prtpage prfile;*p900:row,nmrgrqty;
        prtpage prfile;*p5200:row,"M/P ##";
        prtpage prfile;*p5700:row,omlrky;
        add     sixlpi,row 
        prtpage prfile;*pcolumn:row,"Net Output:";
        prtpage prfile;*p900:row,nmrgnet;
        prtpage prfile;*p5100:row,percent;
        prtpage prfile;*p5700:row,"% Names Mailed"
        add     sixlpi,row 
        prtpage prfile;*pcolumn:row,"Total Billable Names:",totbill;
.                     goto        stop       
.         COMPARE    C0 TO NMRGID
.         IF         NOT EQUAL
.         PRINT      *L,*L,*5,"INTRA DUPES: ",*25,NMRGID;
        add     sixlpi,row 
        add     sixlpi,row 
        add     sixlpi,row 
        prtpage prfile;*pcolumn:row,"Intra Dupes:";
        prtpage prfile;*p1500:row,nmrgid;
..                    *40,"ELIMINIX HITS: ",*65,NMRGELIM;
.        ENDIF    
         COMPARE    C0 TO NMRGERR
         IF         NOT EQUAL
.         PRINT      *L,*5,"ERROR REJECTS: ",*25,NMRGERR;
        add     sixlpi,row 
        prtpage prfile;*pcolumn:row,"Error Rejects:";
        prtpage prfile;*p1500:row,nmrgerr;
.                    *40,"TDMC REJECTS: ",*65,NMRGTDMC;
         ENDIF    
         COMPARE    C0 TO NMRGdisf
         IF         NOT EQUAL
.         PRINT      *L,*5,"DECEASED REJECTS: ",*25,NMRGDISF;
        add     sixlpi,row 
        prtpage prfile;*pcolumn:row,"Deceased Rejects:";
        prtpage prfile;*p1500:row,nmrgdisf;
.                    *40,"EXIT REJECTS: ",*65,NMRGEXIT;
         ENDIF    
         COMPARE    C0 TO NMRGNPER
        IF         NOT EQUAL
.        PRINT      *L,*5,"NONPERSONAL REJECTS: ",*25,NMRGNPER;
        add     sixlpi,row 
        prtpage prfile;*pcolumn:row,"Nonpersonal Rejects:";
        prtpage prfile;*p1500:row,nmrgnper;
.                    *40,"MAILDROP REJECTS: ",*65,NMRGDROP;
        ENDIF    
         COMPARE    C0 TO NMRGDMA
         IF         NOT EQUAL
.        PRINT      *L,*5,"DMA REJECTS: ",*25,NMRGDMA;
        add     sixlpi,row 
        prtpage prfile;*pcolumn:row,"DMA Rejects:";
        prtpage prfile;*p1500:row,nmrgdma;
.                    *40,"UNUSED DUPES: ",*65,NMRGUDUP;
         ENDIF    
.         COMPARE    C0 TO NMRGELMX
.         IF         NOT EQUAL
.         PRINT      *L,*5,"ELIMINIX REJECTS: ",*30,NMRGELMX;
.         ENDIF    
         COMPARE    C0 TO NMRGZ4
         IF         NOT EQUAL
.         PRINT      *L,*5,"ZIP+4 CRT REJECTS: ",*25,NMRGZ4;
        add     sixlpi,row 
        prtpage prfile;*pcolumn:row,"Zip+4 Rejects:";
        prtpage prfile;*p1500:row,nmrgz4;
.                    *40,"NIXIE REJECTS: ",*65,NMRGNIX;
         ENDIF    
. .        COMPARE    C0 TO NMRGTDMC
.         IF         NOT EQUAL
.         PRINT      *L,*5,"TDMC REJECTS: ",*30,NMRGTDMC;
.         ENDIF    
         COMPARE    C0 TO NMRGPRIS
         IF         NOT EQUAL
.         PRINT      *L,*5,"PRISON REJECTS: ",*25,NMRGPRIS;
        add     sixlpi,row 
        prtpage prfile;*pcolumn:row,"Prison Rejects:";
        prtpage prfile;*p1500:row,nmrgpris;
         ENDIF    
         COMPARE    C0 TO NCOAMNF
         IF         NOT EQUAL
.         PRINT      *L,*5,"NCOA MTCH NON FWD: ",*25,NCOAMNF;
        add     eightlpi,row 
        prtpage prfile;*pcolumn:row,"NCOA Mtch Non Fwd:";
        prtpage prfile;*p1500:row,ncoamnf;
         ENDIF
         cmatch     yes to feb
         if         equal
         COMPARE    C0 TO NMRGCUST
         IF         NOT EQUAL
.         PRINT      *L,*5,"CUSTOMER REJECTS: ",*25,NMRGCUST;
        add     sixlpi,row 
        prtpage prfile;*pcolumn:row,"Customer Rejects:";
        prtpage prfile;*p1500:row,nmrgcust;
         ENDIF    
         endif
         COMPARE    C0 TO NMRGconv
         IF         NOT EQUAL
.         PRINT      *L,*5,"CONVERSION REJECTS: ",*25,NMRGCONV;
        add     sixlpi,row 
        prtpage prfile;*pcolumn:row,"Conversion Rejects:";
        prtpage prfile;*p1500:row,nmrgconv;
         ENDIF    
.start Patch #2.0 - new var.
         COMPARE    C0 TO NMRGDPV
         IF         NOT EQUAL
.         PRINT      *L,*5,"DPV DROPS: ",*25,NMRGDPV;
        add     sixlpi,row 
        prtpage prfile;*pcolumn:row,"DPV Drops:";
        prtpage prfile;*p1500:row,nmrgdpv;
         ENDIF    
.end Patch #2.0 - new var.
.start Patch #2.2 - new var.
         COMPARE    C0 TO NMRGDisa
         IF         NOT EQUAL
.         PRINT      *L,*5,"Disaster DROPS: ",*25,NMRGDisa;
        add     sixlpi,row 
        prtpage prfile;*pcolumn:row,"Disaster Drops:";
        prtpage prfile;*p1500:row,nmrgdisa;
         ENDIF    
.end Patch #2.2 - new var.
.start Patch #2.4 - new var.
         COMPARE    C0 TO NMRGCNR
         IF         NOT EQUAL
        add     sixlpi,row 
        prtpage prfile;*pcolumn:row,"CNR Matches:";
        prtpage prfile;*p1500:row,nmrgCNR;
         ENDIF    
.end Patch #2.4 - new var.
.         move       c0 to n2
.         move       onetper to n2
.         compare    c0 to n2
.         if         not equal
.         COMPARE    C0 TO NMRGrep
.         IF         NOT EQUAL
.         PRINT      *L,*5,"REPUBLICAN REJECTS: ",*25,NMRGrep;
.         ENDIF    
.         endif
.         COMPARE    C0 TO NMRGDROP
.         IF         NOT EQUAL
.         PRINT      *L,*5,"MAILDROP REJECTS: ",*30,NMRGDROP;
.         ENDIF
.         PRINT      *L,*25,"____________";
        add     sixlpi,row
        prtpage prfile;*pcolumn:row,*pensize=10,*line=3000:row;
        add     sixlpi,row 
        prtpage prfile;*pcolumn:row,"Total Rejects:";
        prtpage prfile;*p1500:row,totrej;
.        PRINT      *L,*5,"TOTAL REJECTS: ",*25,TOTREJ;
        add     sixlpi,row 
        add     sixlpi,row 
        prtpage prfile;*p3000:row,"Eliminator Hits:";
        prtpage prfile;*p5500:row,nmrgelim;
.          PRINT      *l,*l,*40,"ELIMINATOR HITS: ",*65,NMRGELIM;
.         PRINT      *l,*40,"TDMC REJECTS: ",*65,NMRGTDMC;
        add     sixlpi,row 
        prtpage prfile;*p3000:row,"Service B. Rejects:";
        prtpage prfile;*p5500:row,nmrgtdmc;
.         PRINT      *l,*40,"NCOA REJECTS: ",*65,totncoa;
        add     sixlpi,row 
        prtpage prfile;*p3000:row,"NCOA Rejects:";
        prtpage prfile;*p5500:row,totncoa;
.         PRINT      *l,*40,"MAILDROP REJECTS: ",*65,NMRGDROP;
        add     sixlpi,row 
        prtpage prfile;*p3000:row,"Maildrop Rejects:";
        prtpage prfile;*p5500:row,nmrgdrop;
.        PRINT      *l,*40,"UNUSED DUPES: ",*65,NMRGUDUP;
        add     sixlpi,row 
        prtpage prfile;*p3000:row,"Unused Dupes:";
        prtpage prfile;*p5500:row,nmrgudup;
.         PRINT      *l, *40,"FAMILY REJECTS: ",*65,NMRGFAM;
        add     sixlpi,row 
        prtpage prfile;*p3000:row,"Family Rejects:";
        prtpage prfile;*p5500:row,nmrgfam;
.         PRINT      *l, *40,"NIXIE REJECTS: ",*65,NMRGNIX;
        add     sixlpi,row 
        prtpage prfile;*p3000:row,"Nixie Rejects:";
        prtpage prfile;*p5500:row,nmrgnix;
.         PRINT      *l, *40,"HOUSE HITS: ",*65,NMRGHH;
        add     sixlpi,row 
        prtpage prfile;*p3000:row,"House Hits:";
        prtpage prfile;*p5500:row,nmrghh;
.         PRINT      *l, *40,"CUSTOMER SUPPRESS: ",*65,NMRGCS;
        add     sixlpi,row 
        prtpage prfile;*p3000:row,"Customer Suppress:";
        prtpage prfile;*p5500:row,nmrgcs;
         cmatch     yes to feb
         if         not equal
.         PRINT      *l, *40,"CUSTOMER REJECTS: ",*65,NMRGCust;
        add     sixlpi,row 
        prtpage prfile;*p3000:row,"Customer Rejects:";
        prtpage prfile;*p5500:row,nmrgcust;
         endif
         COMPARE    C0 TO NMRGrep
         IF         NOT EQUAL
         move       yes to rep
.         PRINT      *L,*40,"INDEPENDENT REJECTS: ",*65,NMRGrep;
        add     sixlpi,row 
        prtpage prfile;*p3000:row,"Independent Rejects:";
        prtpage prfile;*p5500:row,nmrgrep;
         endif
         COMPARE    C0 TO NMRGelmx
         if         not equal
         goto       printrep
         else
         cmatch     yes to rep
          if         equal         
printrep
.         PRINT      *L,*40,"CNR REJECTS: ",*65,NMRGelmx;
        add     sixlpi,row 
        prtpage prfile;*p3000:row,"CNR Rejects:";
        prtpage prfile;*p5500:row,nmrgelmx;
.         PRINT      *N,*N,*5,"NOTE: INPUT QTY MINUS TOTAL REJECTS,";
.         PRINT      *l,*5,"ELIMINATOR, TDMC, NCOA TOTAL , MAILDROP,";
.         PRINT      *l,*5,"UNUSED DUPES, NIXIE, FAMILY, HOUSE HITS,";
.         cmatch     yes to feb
.         if         not equal
.         PRINT      *l,*5,"CUSTOMER SUPPRESS, CUSTOMER REJECTS, CNR REJECTS";
.         else
.         PRINT      *l,*5,"CUSTOMER SUPPRESS, CNR REJECTS,";
.         endif
.         PRINT      *l,*5,"INDEPENDENT REJECTS = NET OUTPUT"
.         PRINT       *N,*N,*N,*2,STR3
.         goto       zerotots
.         ENDIF
.         endif    
        add     sixlpi,row 
        add     sixlpi,row 
        prtpage prfile;*pcolumn:row,"Note: Input Qty minus Total Rejects,";
        add     sixlpi,row 
        prtpage prfile;*pcolumn:row,"Eliminator, Service B., Ncoa Total, Maildrop,";
        add     sixlpi,row 
        prtpage prfile;*pcolumn:row,"Unused Dupes, Nixie, Family, House Hits,";
         cmatch     yes to feb
         if         not equal
.         PRINT      *l,*5,"CUSTOMER SUPPRESS, CUSTOMER REJECTS = NET OUTPUT";
        add     sixlpi,row 
        prtpage prfile;*pcolumn:row,"Customer Suppress, Customer Rejects, CNR Rejects,";
         else
        prtpage prfile;*pcolumn:row,"Customer Suppress, CNR Rejects,";
                      endif
        add     sixlpi,row 
        prtpage prfile;*pcolumn:row,"Independent Rejects = Net Output";
        add     sixlpi,row 
        add     sixlpi,row 
        prtpage prfile;*pcolumn:row,STR3
         goto       zerotots
         ENDIF
         endif
PRINTREG
        add     sixlpi,row 
        add     sixlpi,row 
        prtpage prfile;*pcolumn:row,"Note: Input Qty minus Total Rejects,";
        add     sixlpi,row 
        prtpage prfile;*pcolumn:row,"Eliminator, Service B., Ncoa Total, Maildrop,";
        add     sixlpi,row 
        prtpage prfile;*pcolumn:row,"Unused Dupes, Nixie, Family, House Hits,";
         cmatch     yes to feb
         if         not equal
.         PRINT      *l,*5,"CUSTOMER SUPPRESS, CUSTOMER REJECTS = NET OUTPUT";
        add     sixlpi,row 
        prtpage prfile;*pcolumn:row,"Customer Suppress, Customer Rejects = Net Output";
        add     sixlpi,row 
        add     sixlpi,row 
        prtpage prfile;*pcolumn:row,STR3
         else
        add     sixlpi,row 
        prtpage prfile;*pcolumn:row,"Customer Suppress = Net Output";
        add     sixlpi,row 
        add     sixlpi,row 
        prtpage prfile;*pcolumn:row,STR3
         endif
zerotots MOVE        C0 TO TOTREJ
         MOVE        C0 TO TOTNCOA
         move        no to feb
         move        no to rep
         compare   c2 to hotflag
         if        not equal
         GOTO    LOOP
         else
                              goto     stop
         endif
.
CNTRTN   SETLPTR   RTCOMP
         ENDSET    RTCOMP
CHKRHEAD CMATCH    B1 TO RTCOMP
         GOTO      SETRHEAD IF NOT EQUAL
         BUMP      RTCOMP BY -1
         GOTO      CHKRHEAD IF NOT EOS
SETRHEAD MOVEFPTR  RTCOMP TO N3
         MOVE      "80" TO RTNTAB
         SUBTRACT  N3 FROM RTNTAB
         DIVIDE    C2 INTO RTNTAB
         RESET     RTCOMP
         SETLPTR   RTCOMP
         RETURN
STOP     
.         PRINT      *F
.         RELEASE
          PRTCLOSE prfile 
                               branch      funcbr of fini1,fini2
fini1     goto        done
.
fini2
.begin patch 2.5
CheckFile
.//Patch 2.7 Use the Flag to check if pdf has been created
.begin patch 3.0
.                              call      GetPDfPAth
.                              pack      str45 from PDFPATH,"\flag.dat"
.
.                              pack      APIFileName,STR45,hexzero
.                              loop
.                                        call      FindFirstFile
.                              until (APIResult = 0 | APIResult = hexeight)
.                                        pause     "1"
.                              repeat
.                              pause     "5"
.                              erase     STR35
.end patch 3.0
.end Patch 2.7 Use

          pack      str55 from "C:\WORK\pdf\Hotcv.pdf"
          trap      WaitForEnd giving error if IO
          open      FileCheck,STR55,Exclusive     
          Close     FIleCHeck
          trap      IOMssg GIVING ERROR IF IO


          Move      "Here is your PDF File",MailSubjct
          Clear     MailBody
          append    "c:\work\pdf\hotcv.pdf",mailbody
          append    CRLF,Mailbody
          Reset     MailBOdy
          pack      Mailto from User,"@nincal.com"
          pack      MailFrom from User,"@nincal.com"
          pack      MailAttach from "C:\WORK\pdf\Hotcv.pdf"
          call      Sendmail
.          
.               Pack           str45,"C:\WORK\FAXFILE.PRN"
.          If            (osflag = c1 | osflag = c5)
.               append  "!c:\winnt\system32\cmd.exe",taskname
.               ElseIf                  (osflag = c3 | osflag = c4)
.               append  "!c:\command.com",taskname
.               ElseIf                  (osflag = c6)
.               append  "!c:\windows\system32\cmd.exe",taskname
.               endif
.                append        " /c copy ",taskname
.                append        str45,taskname
.                append        B1,taskname
.                append        NTWKPATH1,taskname
.                append        Prtname,taskname
.                reset         taskname
.                execute       taskname
.               erase          str45
.begin patch 3.0
.          Call      PDF995Auto0
.end patch 3.0
.begin patch 2.7
          erase     mailattach
.end patch 2.7

.end patch 2.5
done
          if        (hotflag = c1)
                               shutdown
                   Else            
                               return
                    endif
.                splclose
         shutdown  "cls"
         STOP
WaitForEnd
                    TrapClr   IO
.check the error if file does not exist just get out
                    add       C1,Trapcount
                    pause     c5
                    noreturn
.                   if        (trapcount > 240)   . 20 min are you kidding me
.                   if        (trapcount > 60)   . 5 min are you kidding me
                    if        (trapcount > 36)   . 3 min are you kidding me
                    Pack       MailSubjct,"HotCV - ",str35,b1,str55
                    Move      "CReques@nincal.com",MailFrom
                    Move      "CReques@nincal.com",MailTO
.                   Move      "dherric@nincal.com",MailTO
                    append    CRLF,MailBOdy
                    append    str55,MailBody
                    append    CRLF,MailBOdy
                    append    "I am sorry I could not send the file",Mailbody
                    reset     Mailbody
                    Move      B1,Mailattach
                    call      SendMail
                    return

                    endif
          
                    goto      checkfile
.end patch 2.5
.PRINTCV Routine DimPtr,DimPtr1,DimPtr2,FrmPtr,FrmPtr1
          call      PrintCV using nmrgfld,INITS,user,C1,C1

..DimPtr  = LR ##
..DimPtr1 = Initials
..DimPtr2 = User Name
..FrmPtr  = Printer Choice
..FrmPtr1 = PDF FLAG
PRintCV Routine DimPtr,DimPtr1,DimPtr2,FrmPtr,FrmPtr1
          create    FontM11,"Times New Roman",size=11
          create    FontM11B,"Times New Roman",size=11,Bold

          if        (FrmPtr1 = c1)
                    move      c2,funcbr             .PDF
                    else
                    move      c1,funcbr
          endif     
          MOve      dimptr,Nmrgfld
          move      c2,hotflag
          move      dimptr1,str3
          move      dimptr1,Inits
          move      dimptr2,User
          move      "NMRG0002",program
         pack       INPNAME TO Nmrgfld,str3
.begin patch 3.1
          move      c1,row
.end patch 3.1

          call      Begin
          goto      Done
         INCLUDE   NMRGIO.INC
           INCLUDE   NRTNIO.INC
           INCLUDE   NORDIO.INC
.patch1.9
           INCLUDE   compio.INC
           INCLUDE   cntio.INC
.           INCLUDE   NMLRIO.INC
.patch1.9

.begin patch 2.1           
.         include   ninvio.inc
          include             ninvio.inc
.end patch 2.1           
         include   hpio.inc
         INCLUDE   COMLOGIC.INC

