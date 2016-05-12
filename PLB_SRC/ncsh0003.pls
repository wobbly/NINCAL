.............................................................................
..
.. PROGRAM    : NCSH0003
.. DATE       : 11/01/94
.. AUTHOR     : D.L. HERRICK
.............................................................................
.. DESCRIPTION: PRODUCES NIN DAILY DEPOSIT REGISTER.
.............................................................................
..
PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   Cons.inc
+
         INCLUDE   NCSHDD.inc
+
         INCLUDE   NORDDD.inc
+
.START PATCH 1.56 REPLACED LOGIC
.         INCLUDE   NMLRDD.inc
.         include   nbrkdd.inc
          INCLUDE   COMPDD.inc
          INCLUDE   CNTDD.inc
.END PATCH 1.56 REPLACED LOGIC
         include   hp.inc
.START PATCH 1.53 ADDED LOGIC
           include   NCHKDD.INC
.END PATCH 1.53 ADDED LOGIC
.START PATCH 1.59 ADDED LOGIC
.          include   NCTRDD.INC
.END PATCH 1.59 ADDED LOGIC
.
release  init      "1.59"               DLH 11Jul2007       PLI
.release  init      "1.58"               ASH 24FEB2005      SMALL PATCH
.release  init      "1.57"               ASH 11JAN2005      MAILER/BROKER FIELD CONVERSION
.release  init      "1.56"               ASH 27MAY2004 MAILER CONVERSION
.release  init      "1.55"               ASH 15JAN2003 REMOVED TYPEing OF CHECK NUMBER
.release  init      "1.54"               ASH17JUN2002 CONVERSION OF CONTROLS.DAT, NINCHK.DAT, DAT25N.DAT
.release  init      "1.53"               19APR2001 ASH CASH FILE CONVERSION
.release  init      "1.52"               29OCT2000 ASH NEW SERVER ADDED
.release  init      "1.51"               17DECJD fixed sort.
.release  init      "1.5"               06Nov98 ASH DAT25N Y2K, File expansion
.release  init      "1.4"               09Nov98 ASH fixed printing problem due to BRCOMP/MCOMP 
.release  init      "1.3"               25Feb97 DLH force header on empty input file
.release  init      "1.2"               14aug96 DLH panasonic
.RELEASE  INIT      "1.1"              jd17mar95    change depo handle new cnt# size
.RELEASE  INIT      "1"
............................................................................
.
MPCHANGE INIT      "}0J1K2L3M4N5O6P7Q8R9"
MPCHARS  INIT      "}JKLMNOPQR"   VALID MINUS OVERPUNCH CHARACTERS
CVTFLD   DIM       13             WORK FIELD USED FOR MP CONVERSION.
PRTFLAG  FORM      1
LOCAL    INIT      "LOCAL"
PAGE     FORM      2
LINES    FORM      2
DATEMASK INIT      "XX/XX/XX"
sort     init      "\\nins1\e\netutils\sort "
sdepfile dim       45
.START PATCH 1.57 REPLACED LOGIC
.keyspec  init      " /s(55,25,c,a,41,6,c,a) "
keyspec  init      " /s(63,25,c,a,43,12,c,a) "
.END PATCH 1.57 REPLACED LOGIC
.updated from f: tp \\nins1\d  14aug00 DLH
.fpath    init      "\\nts2\d\data\"
.START PATCH 1.52 REPLACED LOGIC
.fpath    init      "g:\data\"
.END PATCH 1.52 REPLACED LOGIC
depfile  dim       45
CSHDATE  DIM       8
SYSDATE  DIM       8
wrkaMT   dim       13
ARAMT    FORM      10.2
Totalar  FORM      10.2
checkar  FORM      10.2
.START PATCH 1.57 REPLACED LOGIC
.holdchk  dim       6
holdchk  dim       12
.str20    dim       20
str20a    dim       20
.END PATCH 1.57 REPLACED LOGIC
control  init      "001"
DOLLAR   INIT      "$,$$$,$$$,$$$.99-"
armask   dim       17
.Start patch #1.5 - var expanded to hold century
.cdate    dim       8
cdate    dim       10
.End patch #1.5 - var expanded to hold century
lasrflag init      "T"            generally true unless a/p clerk has req
+........................................................................
depofile file     
depofil2 file
.fix=122
.begin patch 1.59
CmpPrtNme Dim       18
.end patch 1.59

+........................................................................
         move      c1 to nordpath
         move      c1 to nbrkpath
         move      c1 to nmlrpath
         move      c1 to ncshpath
         move      "NCSH0003" TO PROGRAM
         clock     date to today
         clock    date to datemask
         MOVE      "Names In The News" to compnme
         call      paint
         scan      "NOLASER" in comment
         if        equal
         move      "F" to lasrflag
         bump      comment by -1
         lenset    comment
         reset     comment
         move      comment to str10
         clear     comment
         move      str10 to comment
         endif
         reset     comment
         clear     depfile
         clear     sdepfile
.START PATCH 1.52 REPLACED LOGIC
.         pack      depfile from fpath,comment,".dat"
.         pack      sdepfile from fpath,comment,".srt"
         pack      depfile from NTWKPATH1,comment,".dat"
         pack      sdepfile from NTWKPATH1,comment,".srt"
.END PATCH 1.52 REPLACED LOGIC
         prepare   depofile,depfile
INPGET   DISPLAY   *P15:06,INPNAME
         TRAP      INPNG IF IO
         OPEN      TESTFILE,INPNAME
         TRAPCLR   IO
         DISPLAY   *P15:06,INPNAME
         MOVE      INPNAME TO NCSHNAME
         GOTO      passone
INPNG    NORETURN
         KEYIN     *P01:24,*EL,"The Input file is not on-line. ":
                   *P15:06,INPNAME
         GOTO      INPGET
passone
         call      ncshseq
.START PATCH 1.58 REPLACED LOGIC
.         goto      passtwo if over
          if over
                    if (N4 = C0)        .File empty, no records read!!
                              display    *p01:07,"Input File is empty!  Closing down program."
                              goto io
                    endif
                    goto passtwo
          endif
.Start PATCH 1.59 REPLACED LOGIC
.              pack           NCTRFLD,CNUM,CNUMDATE
.              rep            zfill,NCTRFLD
.                     move    "START-NCTRKEY",Location
.              pack    KeyLocation,"Key: ",NCTRFLD
.                      call    NCTRKEY

          if        (Company = c2)
          Move      "Pacific Lists Inc",CmpPrtNme
          else
          MOVe      "Names in the News",CmpPRtNme
          endif
.end patch 1.59
          
.END PATCH 1.58 REPLACED LOGIC
         add       c1 to n4
         display    *p01:07,"Records Input ",n4
         Cmatch     B1 TO NCSHCHK
         goto      passone if EOS
         SCAN      "MOA" IN NCSHCHK
         GOTO       PASSONE IF EQUAL
         RESET      NCSHCHK
         SCAN      "PREPAY" IN NCSHCHK
         GOTO       PASSONE IF EQUAL
         RESET      NCSHCHK
.START PATCH 1.55 REMOVED LOGIC
.         TYPE       NCSHCHK
.         GOTO       PASSONE IF NOT EQUAL
.END PATCH 1.55 REMOVED LOGIC
.START PATCH 1.57 REPLACED LOGIC
.         cmatch    " " to cmlr
.         if        equal
.         move      "Unidentified" to mcomp
.         clear     brcomp
.         goto      write
.         endif
.         clear     mkey
.         pack      mkey from cmlr,z3
.         rep       zfill in mkey
.         type      mkey
.         if        not equal
.         move      "0000000" to mkey
.         endif
.         call      nmlrkey
.         if        over
.         move      "No Mailer found " to mcomp
.         endif  
.         cmatch    " " to clr
.         if        equal
.         clear     brcomp
.         goto      write
.         endif
.         move      clr to nordfld
.         call      nordkey
.         clear     nbrkfld
.         clear     brcomp
.         type      obrknum
.         goto      brk if equal
.         goto      write
.brk      pack      nbrkfld from obrknum,obrkcnt
.         call      nbrkkey
.write   
.         write     depofile,seq;cashvars,mcomp,brcomp
.         goto      passone
........................................
          call      Trim using CMLR
          if (CMLR = "")
                    move      "Unidentified",str45
                    clear     COMPCOMP
                    goto write
          endif
          pack      COMPFLD,CMLR
          move      "passone-COMPKEY",Location
          pack      KeyLocation,"Key: ",COMPFLD
          call      COMPKEY
          if over
                    move      "No Mailer found ",str45
          elseif (COMPMLRFLG <> "T")
                    move      "No Mailer found ",str45
          else
                    move      COMPCOMP,str45
          endif
          cmatch    " ",clr
          if equal
                    clear     str45
                    goto write
          endif
          move      clr to nordfld
          call      nordkey
          clear     COMPCOMP
          call      Trim using OBRKNUM
          if (OBRKNUM <> "")
                    goto brk
          endif
          goto      write
brk
.Start Temporary Patch
.         pack      COMPFLD,OBRKNUM
.         move      "brk-COMPKEY",Location
.         pack      KeyLocation,"Key: ",COMPFLD
.         call      COMPKEY
.         if over
.                   clear     COMPCOMP
.         elseif (COMPBRKFLG <> "T")
.                   clear     COMPCOMP
.         endif
          pack      COMPFLD4,OBRKNUM
          move      "brk-COMPKEY2",Location
          pack      KeyLocation,"Key: ",COMPFLD4
          call      COMPKEY2
          if over
                    clear     COMPCOMP
          elseif (COMPBRKFLG <> "T" & COMPCLRFLG <> "T")
                    clear     COMPCOMP
          endif
.End Temporary Patch
write   
          write     depofile,seq;cashvars,str45,COMPCOMP
          goto      passone
.END PATCH 1.57 REPLACED LOGIC
.
cvtmp    REPLACE   MPCHANGE IN cvtfld            CHANGE MP TO NUMBER.
         RESET     cvtfld
         TYPE      CVTFLD                        VALID NUMERIC?
         GOTO      FORMERR IF NOT EQUAL          NO.
         MOVE      CVTFLD TO N10               MOVE INTO NUMERIC.
         MULTIPLY  seq   BY N10               CHANGE TO MINUS.
         MOVE      N10  TO CVTFLD              MOVE BACK TO DIM.
         RETURN
FORMERR  DISPLAY   *P1:23,*EL,*B,"FORMAT ERROR READING LR: ",clr
         RETURN                                POP THE STACK.
         
.
passtwo  trap      io giving error if io
         weof      depofile,seq
         close     depofile
         compare   c0 to n4            .zero input records?
         if        equal
         move      c0 to control
         call      header              .yes, no deposit assoc with control
.                                      .force a header for the report         
         endif
         compare   "58" to lines
         call      header if not less
         pack       Taskname from Depfile,comma,SDepfile," -63-87,43-55"
         Sort       taskname

.         pack      taskname from sort,depfile,b1,sdepfile,keyspec
.         execute   taskname
.         pause     "10"
.         execute   "sort e:\data\depofile.dat dep.srt /s(47,25,c,a,33,6,c,a) f(tab)"
         DISPLAY   *P1:24,*R,*EL,"opening FILE ",sdepfile,*W
         open      depofile,sdepfile
         PACK      PRTFILE WITH PDRIVE,PRTNAME
.         SPLOPEN   PRTFILE
         SPLOPEN   "\\nins2\laser2","R"
         display   *p10:12,"print file ",prtfile
         move      c0 to n4
loop
.START PATCH 1.57 REPLACED LOGIC
.         read      depofile,seq;cashvars,mcomp,brcomp
         read      depofile,seq;cashvars,str45,COMPCOMP
.END PATCH 1.57 REPLACED LOGIC
         goto      done if over
         add       c1 to n4
         compare   c1 to n4
         if        equal
         move      ncshchk to holdchk
.Start patch #1.5 - var expanded to hold century
.         pack      cdate from cmo,slash,cdy,slash,cyr
         pack      cdate from cmo,slash,cdy,slash,cce,cyr
.End patch #1.5 - var expanded to hold century
         MOVE      CNUM TO CONTROL
         call      header
         endif
.         clear     cvtfld
.         move      camount to cvtfld
.         endset    cvtfld
.         RESET     MPCHARS
.         SCAN      CVTFLD IN MPCHARS             IS IT A MINUSOVRPNCH?
.         if        equal
.         display   *p1:21,cvtfld
.         call      CVTMP                 YES.
.         bump      cvtfld,2
.         move      cvtfld to wrkamt
.         reset     cvtfld
.         display   *p1:22,cvtfld,b1,wrkamt,*w3
.         else
.         move      camount to wrkamt
.         endif
.         RESET     CVTFLD                        NO.
         display    *p01:08,"Records printed ",n4
         match     ncshchk to holdchk
         call      break if not equal
         compare   "58" to lines
         call      header if not less
         sub       aramt from aramt
         move      camount to aramt
.         mult      ".01" by aramt
         move      dollar to armask
         edit      aramt to armask
         add       aramt to totalar
         add       aramt to checkar
.Start Patch #1.5 - remmed and replaced lines
..Start Patch #1.4 - corrected printing problem due to extension of brcomp & mcomp
..        if         equal      
..         print     mcomp,hpt175,"|",b1,nckdtem,slash,nckdted,slash,nckdtey:
..                   b1,"|",b1,ncshchk,"|",b1,clr,"|":
..                   armask,b1,"| ",brcomp," | ",npayor
..         else
..         print     mcomp,*tab=33,"|",b1,nckdtem,slash,nckdted,slash,nckdtey:
..                   b1,"|",b1,ncshchk,"|",b1,clr,"|":
..                   armask,b1,"| ",brcomp," | ",npayor
..         endif
.         unpack    brcomp,str24,str1
.         move      mcomp,str25         
.         if         equal      
.         print     str25,hpt175,"|",b1,nckdtem,slash,nckdted,slash,nckdtey:
.                   b1,"|",b1,ncshchk,"|",b1,clr,"|":
.                   armask,b1,"| ",str24,str1," | ",npayor
.         else
.         print     str25,*tab=33,"|",b1,nckdtem,slash,nckdted,slash,nckdtey:
.                   b1,"|",b1,ncshchk,"|",b1,clr,"|":
.                   armask,b1,"| ",str24,str1," | ",npayor
.         endif 
.START PATCH 1.53 REPLACED LOGIC
.         unpack    brcomp,str24,str1
.         move      mcomp,str25
.         if        equal
.         print     str25,hpt175,"|",nckdtem,slash,nckdted,slash,nckdtec,nckdtey:
.                   "|",b1,ncshchk,"|",b1,clr,"|":
.                   armask,b1,"| ",str24,str1," | ",npayor
.         else
.         print     str25,*tab=33,"|",nckdtem,slash,nckdted,slash,nckdtec,nckdtey:
.                   "|",b1,ncshchk,"|",b1,clr,"|":
.                   armask,b1,"| ",str24,str1," | ",npayor
.         endif
.
.START PATCH 1.54 REPLACED LOGIC
.        pack    NCHKFLD,CNUM,NCSHCHK
        pack    NCHKFLD,CNUM,CNUMDATE,NCSHCHK
.END PATCH 1.54 REPLACED LOGIC
        move    "NCHKKEY",Location
        pack    KeyLocation,"Key: ",NCHKFLD
        call    NCHKKEY
        if not over
                unpack  NCHKDATE,str2,YY,MM,DD
                pack    str10,MM,SLASH,DD,SLASH,str2,YY
        else
                clear   NCHKPAYOR
                clear   str10
        endif        
.
.START PATCH 1.57 REPLACED LOGIC
.         unpack    brcomp,str24,str1
.         move      mcomp,str25
.        cmatch     true to lasrflag
.         if        equal
.         print     str25,hpt175,"|",str10:
.                   "|",b1,ncshchk,"|",b1,clr,"|":
.                   armask,b1,"| ",str24,str1," | ",NCHKPAYOR
.         else
.         print     str25,*tab=33,"|",str10:
.                   "|",b1,ncshchk,"|",b1,clr,"|":
.                   armask,b1,"| ",str24,str1," | ",NCHKPAYOR
.         endif
.......................................
         unpack    COMPCOMP,str20a
         move      str45,str20
.Note that the following line was stuck up before the above patch 1.5 start line, thus throwing off the EQUAL flag.  I moved it with patch 1.57 ASH
        cmatch     true to lasrflag
         if        equal
         print     str20,hpt175,"|",str10:
                   "|",b1,ncshchk,"|",b1,clr,"|":
                   armask,b1,"| ",str20a," | ",NCHKPAYOR
         else
         print     str20,*tab=33,"|",str10:
                   "|",b1,ncshchk,"|",b1,clr,"|":
                   armask,b1,"| ",str20a," | ",NCHKPAYOR
         endif
.END PATCH 1.57 REPLACED LOGIC
.END PATCH 1.53 REPLACED LOGIC
.End Patch #1.4 - corrected printing problem due to extension of brcomp  
.End Patch #1.5 - remmed and replaced lines                   
         add       c1 to lines
         goto      loop
break    
         move      dollar to armask
         edit      checkar to armask
         compare   "58" to lines
         call      header if not less
        cmatch     true to lasrflag
        if         equal      
         print     hpbon,hpitalic,"Check Total ",hpboff,hpt175,"|",b10,"|":
                   hpbon,"##",holdchk,hpboff:
                   "|",b6,b1,"|",hpbon,armask,hpboff,b1,"|",b10,b10,b7:
                   "|",hpuprght,*n
         else
         print     p24bon,p24ital,"Check Total ",p24boff,*tab=36,"|",b10,"|":
                   p24bon,"##",holdchk,p24boff:
                   "|",b6,b1,"|",p24bon,armask,p24boff,b1,"|",b10,b10,b7:
                   "|",P24UPRT,*n
         endif
         add       c2 to lines
         move      c0 to checkar
         move      ncshchk to holdchk
         return                           
header   add      c1 to page
         compare  c1 to page
         if        equal
         cmatch     true to lasrflag
         if         equal
         print     hpreset:
                   hpport:
                   033,"&l66P":               page length
                   033,"&l65F":
                   033,"&l1E",033,"&a0c0R":     top margin * print position
                   hpuniver,hp15ptch,hpdupl,*f
         else
         print      p2417cpi
         endif
         endif
         cmatch     true to lasrflag
         if         equal
.begin patch 1.59
.         print     *f,*n,hp10ptch,"Deposit Report ",hpt675,datemask:
         print     *f,*n,hp10ptch,CmpPrtNme," Deposit Report ",hpt675,datemask:
                   *n,"Control ## ",control,hpt675,"Page: ",page:
                   hp15ptch:
                   *l,*n,hpbon,"Mailer",hpt175," Check Date":
                   " Check ##    LR       Amount     Broker/Consultant":
                   "              Payor",hpboff:
                    *n,*rptchar "_":130    
         else
.         print     *f,*n,p24pic10,"Deposit Report ",*tab=123,datemask:
         print     *f,*n,p24pic10,CmpPrtNme," Deposit Report ",*tab=123,datemask:
                   *n,"Control ## ",control,*tab=122,"Page: ",page:
                   p2417cpi:
                   *l,*n,p24bon,"Mailer",*tab=32," Check Date":
                   " Check ##    LR         Amount         Broker/Consultant":
                   "              Payor",p24boff:
                    *n,*rptchar "_":130    
         endif
.end patch 1.59
         move      c6 to lines
         return
done     
         call      break
         move      dollar to armask
         edit      totalar to armask
         compare   "56" to lines
         call      header if not less
         cmatch     true to lasrflag
         if         equal
         print     *1,*rptchar "_":130:    
                   *n,hpt475,033,"*c375A":      width
                   033,"*c75B":       height
                   033,"*c2G":        area fill%
                   033,"*c2P":          print it shaded
                   *n,hp10ptch,hpbon,"Control ",control,b1:
                   cdate,hpt400,"Total:":
                   hpt475,armask,hpboff
         print      hpreset,*flush
         else
         print     *1,*rptchar "_":130:    
                   *n,p24pic10,p24bon,"Control ",control,b1:
                   cdate,*tab=80,"Total: ":
                   armask,p24boff
         print     *flush
         endif
         splclose
         shutdown   "cls"

.         stop         
io       display    *p1:24,*el,*b,"error ",error,*b,*w7;
         shutdown   "cls"
.
         include    ncshio.inc
.START PATCH 1.56 REPLACED LOGIC
.         INCLUDE   NMLRIO.inc
.         include   nbrkIO.inc
          INCLUDE   COMPIO.inc
          INCLUDE   CNTIO.inc
.END PATCH 1.56 REPLACED LOGIC
         include    nordio.inc
         include    comlogic.inc
.START PATCH 1.53 ADDED LOGIC
           include   NCHKIO.INC
.END PATCH 1.53 ADDED LOGIC
.START PATCH 1.59 ADDED LOGIC
.          include   NCTRIO.INC
.END PATCH 1.59 ADDED LOGIC

         
