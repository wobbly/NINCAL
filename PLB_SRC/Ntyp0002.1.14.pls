.
. PURPOSE - PRINT YTD REPORT FOR data entry
.           1)TOTAL ORDERS PRINTED.
.           2)NUMBER OF NEW ORDERS PER TYPIST.
.           3)NUMBER OF REPRINTS PER TYPIST.
.           4)PERCENTAGES OF ABOVE COMPARED TO TOTALS.
. .............................................................................
.
PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
         INCLUDE   NTYPDD.inc
         INCLUDE   NORDDD.inc
         INCLUDE   NUSEDD.inc
.
INPUT   FILE      FIXED=696,STATIC=12

RElease   Init            "1.14"        DH remove Reuben add Denise
Reldate   Init      "2015 May 18"
.RElease   Init            "1.13"        DH add Amy, Jennifer and Krsni
.Reldate   Init      "2015 March 2"
.RElease   Init            "1.12"        DH CHange date checking to allow rerun at end of any particular year & month
.Reldate   Init      "2014 March 12"
.RElease   Init            "1.11"        DH counters that reference reprints are now updates
.Reldate   Init      "2013 October 07"
.RElease   Init            "1.10"        DH 
.Reldate   Init      "2013 April 24"
.RElease   Init            "1.00"        DH 
.Reldate   Init      "27 November 2012"
. .............................
FILL3    DIM       3
DATE     DIM       8
.
prfile   pfile
Title1   form    9
Title2   form    9
Title3   form    9
Title4   form    9
PgCnt    form    9

FileCheck FIle
trapcount form      4


.Define Fonts to be used
font1   font
font2   font
font3   font
font4   font
font5   font
font6   font
font7   font
font8   font
font9   font
        create  font1,"Arial",size=12,bold
        create  font2,"Arial",size=9
        create  font3,"Helvetica",size=9
        create  font4,"Fixed",size=10
        create  font5,"Arial",size=11
        create  font6,"Arial",size=14
        create  font7,"Times New Roman",size=9
        create  font8,"Times New Roman",size=10
        create  font9,"Times New Roman",size=10,italic
. ..............
. OTHER VARIABLES.
NOrdTypRec     REcord         (40)  
TypeREc       Dim             3                 ;typist inits
Countrec      Form            5                 ;number of new orders            
RepRec        Form            5                 ;number reprint orders
Nqtyrec       Form            10                 ;order qty
LCRRec        Form            5                 ;new lcr count
LCRrRec       Form            5                 ;reprint lcr count
invrec        Form            5                 .new invoice count
invRrec       Form            5                 .reprint invoice count
adjrec        Form            5                   .new adjustments
PordRec       form            5                 ;pending order count
PordURec       form            5                 ;pending order update count
DatRec        Form            5                   .new datacards
DatUrec       Form            5                   .update datacards
              RecordEnd









NQTY          FORM      10            NEW ORDER QTY
NCOUNT        FORM      5            NUMBER OF NEW ORDERS
REPCNT        FORM      5            NUMBER OF REPRINT ORDERS
Pcount        form      5            pending orders
PUcount        form            4            pending orders Updates
Lcount        form      5              lcr's
SUBRTY        FORM      10                    order reprint qty 
COUNT         FORM      5
BRANCH        FORM      "00"
QTY           FORM      9
HUNDRED       FORM      "100"
TOTREP        FORM      5
Calc53        Form            5.3
REPCALC       FORM      3.2        *percentage of reprint orders
TOTCALC       FORM      3.2        *percentage of orders
LTCALC        FORM      3.2        *PERCENTAGE OF NEW LCR'S.
LRCALC        FORM      3.2        *PERCENTAGE OF REPRINT LCR'S.
ATCALC        FORM      3.2        *PERCENTAGE fcalcpndOF NEW APPROVALS.
pucalc        FORM      3.2        *PERCENTAGE OF Updated Pending.
ADCALC        FORM      3.2        *PERCENTAGE OF NEW adjustments.
PCalc         Form      3.2        *percentage of Pending orders
INVCALC       FORM      3.2
INVRCALC      FORM      3.2
LstCalc       Form            3.2
LstUCalc      Form            3.2
LINES         FORM      2
ENDSW         DIM       1
ANS           DIM       1
PRTDATE       DIM       8          *DATE FORMAT MM/DD/YY
IOERROR       FORM      1          *INDEX FOR IO ERROR BRANCH.
PAGE          FORM      "  1"
CANC          FORM      4
LSTOTAL       FORM      5          *LCR TOTALS
LRTOTAL       FORM      5          *LCR TOTALS
OrdSum        Form            5                 ;holding place # of new orders
OrdrSum       Form            5                 ;holding place # of reprint orders
qtySum        Form            5                 ;holding place # of new order names
LcrSum        Form            5                 ;holding place # of new lcrs
LcrRSum       Form            5                 ;holding place # of reprint Lcrs
POrdSum       Form            5                 ;holding place # of new Pending orders
POrdUSum       Form            5                 ;holding place # of updated Pending orders
InvSUm        form            5
InvRsum       Form            5
CorSum        form            5
CancSum       form            5
.appsum        form            5
PndUsum        form            5
adjsum        form            5
lstsum        form            5
lstUsum       form            5
Datecheck     dim             8
M01       INIT      "January"
M02       INIT      "February"
M03       INIT      "March"
M04       INIT      "April"
M05       INIT      "May"
M06       INIT      "June"
M07       INIT      "July"
M08       INIT      "August"
M09       INIT      "September"
M010      INIT      "October"
M011      INIT      "November"
M012      INIT      "December"
bigdate   dim       25

. PROGRAM MAIN.
         MOVE      "NIN" TO COMPNME
         MOVE      "ORDER TYPIST REPORTING" TO STITLE
         MOVE      "EXIT" TO PF5
         CALL       PAINT
         CALL       FUNCDISP
         CLOCK     DATE TO DATE
.temp
.          move       "02/28/13",date
         IFNZ      PC
         UNPACK    DATE INTO MM,DD,YY
         XIF
         IFZ       PC 
         UNPACK    DATE INTO MM,STR1,DD,STR1,YY
         XIF
         REP       ZFILL IN YY
         REP       ZFILL IN MM
         REP       ZFILL IN DD
           
         PACK      PRTDATE FROM MM,SLASH,DD,SLASH,YY

         clock     timestamp,str4
.temp
.          move       "2013",str4
         unpack    str4,str2,yy
         Move            str2 to cc         
.begin Patch 1.12
.         pack      TypDate from str4
         pack      TypDate from str4,mm
         Pack      Today from MM,SLASH,DD,SLASH,YY        
         rep       Zfill in typDate
.         pack      Datecheck from cc,yy
         pack      Datecheck from cc,yy,mm
.end Patch 1.12
.         
.testing
.          move      "20090622",datecheck

          move      "NTYP0002",program
          Rep       LowUP,Program
          if         (PROGRAM = "NTYP0002")  .chained from dsinit
                    move      "P",ans
                    unpack today into mm,str1,dd,str1,yy
                    PACK      PRTDATE FROM MM,SLASH,DD,SLASH,YY
                    pack      TypDate from cc,yy,mm
                    move      MM,N2
                    load      STR9 USING N2 FROM M01,m02,m03,m04,m05,m06:
                              m07,m08,m09,m010,m011,m012
                    move      DD,STR2
                    reset     STR2,1
                    setlptr   STR2,1
                    rep       "0 ",STR2
                    setlptr   STR2
                    clear     bigDATE
                    append    STR9,bigDATE
                    append    B1,bigDATE
                    append    STR2,bigDATE
                    append    B1,bigDATE
                    append    ",",bigDATE
                    append    cc,bigDATE
                    append    YY,bigDATE
                    reset     bigDATE
                    goto      autoprint
          else
          shutdown  "cls"
          Stop
          endif




Autoprint
              PACK            PRTDATE FROM MM,SLASH,DD,SLASH,YY
              PACK            DATE FROM MM,SLASH,DD,SLASH,YY
.begin Patch 1.12
.              pack            TypDate from cc,yy
.              pack            Datecheck from cc,yy
              pack            TypDate from cc,yy,mm
              pack            Datecheck from cc,yy,mm
.end Patch 1.12
              
                  
.begin patch 1.1
.        call       GetPDFPath
.                    pack      str45 from PDFPATH,"\res\pdf995.ini"
. 
.                   call      "GU$INI;WRITE_TO_INI" USING str45:
.                             "Parameters":
.                             "ProcessPDF":
.                             "\\nins1\e\apps\winbatch\Del995flag.exe":
.          result
.          if (result = C0)
..Prepare Flag file
.          pack      str45 from pdfpath,"\flag.dat"
.          prep      tempfile,str45
.          write     tempfile,SEQ;"flag set"
.          close     tempfile
.          endif
.. Logic Addition for PDF Quality Control         
.          PRTOPEN prfile,"PDF995","TypistRep.pdf"
          PRTOPEN prfile,"PDF:","c:\work\pdf\TypistRep.pdf"
.end patch 1.1
          prtpage prfile;*UNITS=*HIENGLISH:
          *ORIENT=*LANDSCAPE;

.set position in index
.begin Patch 1.12
          unpack    TypDate,str4
.          packkey   NTYPFLD,"190101"
          packkey   NTYPFLD,str4,"01"
          rep         zfill,ntypfld
.end Patch 1.12
          call      Ntypkey
.
PRocess
          loop
          call      NtypKs
          until     over
.begin Patch 1.12
.          unpack    TypDate,str4
.          if        (datecheck = str4)
          unpack    TypDate,str6
          if        (str6 <= datecheck)
.end Patch 1.12
                    FOR       Branch,"1","40"
                              call      trim using NtypType
                              call      debug              
                              If             (NTypTYPe = NordTypRec(branch).TypeRec)                            
                                             add           Subqty,NordTypREc(Branch).Nqtyrec              ;# new Names
                                             add           SUBCOUNT,NordTypREc(Branch).Countrec
                                             add           RepCOUNt,NordTypREc(Branch).Reprec
                                             add           PndCount,NordTypREc(Branch).PordRec
                                             add           PndUCount,NordTypREc(Branch).PordURec
                                             add           LsubCnt,NordTypREc(Branch).LcrRec
                                             add           LREPCNT,NordTypREc(Branch).LcrRRec
                                             add           INVCOUNT,NordTypREc(Branch).INvRec
                                             add           INVRCNT,NordTypREc(Branch).INvrRec
                                             add           ADJCount,NordTypREc(Branch).AdjRec
                                             add           lstCount,NordTypREc(Branch).DatRec
                                             add           lstUCount,NordTypREc(Branch).DatURec


                             Break
                             endif
                             if              (Nordtyprec(Branch).Typerec = "")
                                             move          Subqty,NordTypREc(Branch).Nqtyrec              ;# new Names
                                             move          SubCount,NordTypREc(Branch).Countrec
                                             move          RepCount,NordTypREc(Branch).Reprec
                                             move          PndCount,NordTypREc(Branch).PordRec
                                             move          PndUCount,NordTypREc(Branch).PordURec
                                             move          LSubCnt,NordTypREc(Branch).LcrRec
                                             move          LREPCNT,NordTypREc(Branch).LcrRRec
                                             move          INVCOUNT,NordTypREc(Branch).INvRec
                                             move          INVRCNT,NordTypREc(Branch).INvrRec
                                             move          ADJCount,NordTypREc(Branch).AdjRec
                                             move          lstCount,NordTypREc(Branch).DatRec
                                             move          lstUCount,NordTypREc(Branch).DatURec
                                             Move           NTypTYPe,NordTypREc(Branch).Typerec
                             break                
                             endif                
                    repeat
          endif
          Repeat
.

.
OUTPUT
         DISPLAY   *P1:24,"PREPARING FILE OUTPUT",*B;
         SUB       TOTREP FROM COUNT   *SUBTRACT REPRINTS FROM TOTAL.
.Set up columns and title positions
        move    "100",column                      
        move    "2600",column1                    .Order
        move    "3100",column2                    .Order
        move    "3950",column3                    .pending
        move    "4450",column4                    .pending
        move    "5100",column5                    .Lcr
        move    "5600",column6                    .Lcr
        move    "6600",column7                    .invoice
        move    "7100",column8                    .invoice
        move    "8100",column9                    .adjustment
        move    "8600",column10                   .adjustment
        move    "9600",column11                  .list
        move    "10100",column12                  .list
.       7860 row position of pg #
        move    "5260",Title1
        move    "9000",Title2
        move    "9500",Title3
        move    "5260",Title4
              CALL      HEADER
OUTPUT1
.      UNLOAD TABLES.
.get totals first
              FOR           Branch,"1","40"
                                            
                                             Move           NordTypRec(branch).TypeRec,NTypTYPe
                                             Move           NordTypREc(Branch).Nqtyrec,nqty     ;# new Names
                                             Move           NordTypREc(Branch).Countrec,Ncount     ;new orders 
                                             Move           NordTypREc(Branch).Reprec,RepCnt       ;order reprint count
                                             Move           NordTypREc(Branch).PordRec,Pcount      ;pending
                                             Move           NordTypREc(Branch).PordURec,Pcount      ;pending
                                             Move           NordTypREc(Branch).LcrRec,LCount       ;lcrs

                                             Move          NordTypREc(Branch).LcrRRec,LREPCNT
                                             Move          NordTypREc(Branch).INvRec,INVCOUNT
                                             Move          NordTypREc(Branch).INvrRec,INVRCNT
                                             Move          NordTypREc(Branch).AdjRec,ADJCount
                                             Move          NordTypREc(Branch).DatRec,lstCount
                                             Move          NordTypREc(Branch).DatURec,lstUCount
                                             call      trim using NtypType
                               if        (NtypType = "99")
                                        move            Ncount     to ordsum
                                        move            nqty   to QtySum
                                        move            repcnt,OrdRsum
                                        move            Lcount  to LCrsum
                                        move            LREPCNT  to LcrRSum
                                        move            INVCOUNT to Invsum     
                                        move            INVRCNT  to InvRsum    
                                        move            CORCOUNT To CorSum     
                                        move            CANCOUNT to Cancsum    
.                                        move            APPCOUNT to AppSum     
                                        move            ADJCount to AdjSum     
                                        move            PCOunt to Pordsum    
                                        move            PUCOunt to PordUsum    
                                        move            lstCount to Lstsum     
                                        move            LStUCOunt to LstUSum
                             Break
                             endif
              Repeat               
              FOR           Branch,"1","40"
                                            
                                             Move           NordTypRec(branch).TypeRec,NTypTYPe
                             if              (Nordtyprec(Branch).Typerec = "")
                             break  
                             goto            eoj
                             else
                                             call      trim using NtypType
                                             if        (NtypType <> "99")
                                             Move           NordTypREc(Branch).Nqtyrec,nqty     ;# new Names
                                             Move           NordTypREc(Branch).Countrec,Ncount     ;new orders 
                                             Move           NordTypREc(Branch).Reprec,RepCnt       ;order reprint count
                                             Move           NordTypREc(Branch).PordRec,Pcount      ;pending
                                             Move           NordTypREc(Branch).PordURec,PUcount      ;pending
                                             Move           NordTypREc(Branch).LcrRec,LCount       ;lcrs

                                             Move          NordTypREc(Branch).LcrRRec,LREPCNT
                                             Move          NordTypREc(Branch).INvRec,INVCOUNT
                                             Move          NordTypREc(Branch).INvrRec,INVRCNT
                                             Move          NordTypREc(Branch).AdjRec,ADJCount
                                             Move          NordTypREc(Branch).DatRec,lstCount
                                             Move          NordTypREc(Branch).DatURec,lstUCount
                                        call      debug
                                        call      CALCPER
                                        call      debug
                                        call       Detail
                                             endif
                             endif                        
            Repeat                 
          call      debug
            goto            eoj
..........................................................................................
.
CALCPER
.      CALCULATE PERCENTAGE OF NEW ORDERS.
         MOVE      C0 TO TOTCALC
         MOve      c0 to Calc53         
         COMPARE   C0 TO ncount
         GOTO      CALCREP IF EQUAL
         Move      Ordsum to count
         MOVE      ncount TO CALC53
         DIVIDE    COUNT INTO CALC53
         MULT      HUNDRED BY CALC53
        Move            calc53 to totcalc
CALCREP
.      CALCULATE PERCENTAGE OF REPRINTS.
         MOVE      C0 TO REPCALC
         MOve      c0 to Calc53         
         COMPARE   C0 TO RePCNT
         GOTO      CALCLCR IF EQUAL
         Move      Ordrsum to totrep
         MOVE      REPCNT TO CALC53
         DIVIDE    TOTREP INTO CALC53
         MULT      HUNDRED BY CALC53
         Move      calc53 to repcalc
.
.CALCLCR - CALC LIST CLEARANCE REQ'S.
CALCLCR
         MOVE      C0 TO LTCALC
         MOve            c0 to Calc53         
         COMPARE   C0 TO Lcount
         GOTO      CALCLCR1 IF EQUAL
         MOVE      LCrsum TO COUNT
         MOVE      Lcount TO CALC53
         DIVIDE    COUNT INTO CALC53
         MULT      HUNDRED BY CALC53
         MOve            calc53 to ltcalc
.
CALCLCR1
         MOVE      C0 TO LRCALC
         MOve            c0 to Calc53         
         COMPARE   C0 TO LREPCNT
         GOTO      CALCINV IF EQUAL
         MOVE      LCRrsum TO COUNT
         MOVE      LREPCNT TO CALC53
         DIVIDE    COUNT INTO CALC53
         MULT      HUNDRED BY CALC53
              Move            Calc53 to LRcalc         
.
.CALCINV - CALC INCVOICES.
CALCINV
         MOVE      C0 TO INVCALC
              MOve            c0 to Calc53         
         COMPARE   C0 TO INVCOUNT
         GOTO      CALCINV1 IF EQUAL
         MOVE      INVsum TO COUNT
         MOVE      INVCOUNT TO CALC53
         DIVIDE    COUNT INTO CALC53
         MULT      HUNDRED BY CALC53
              Move            Calc53 to invcalc
.
CALCINV1
         MOVE      C0 TO INVRCALC
              MOve            c0 to Calc53         
         COMPARE   C0 TO INVRCNT
.         GOTO      CALCAPP IF EQUAL
         GOTO      CALcPndU IF EQUAL
         MOVE      INVRsum TO COUNT
         MOVE      INVRCNT TO CALC53
         DIVIDE    COUNT INTO CALC53
         MULT      HUNDRED BY CALC53
              Move            Calc53 to invrcalc
.
calcPndU
         MOVE      C0 TO PUCALC
         MOve            c0 to Calc53         
         COMPARE   C0 TO PndUCOUNT
         GOTO      calcadj IF EQUAL
         MOVE      pndUsum TO COUNT
         MOVE      PndUcount TO CALC53
         DIVIDE    COUNT INTO CALC53
         MULT      HUNDRED BY CALC53
         Move      calc53 to PUcalc
.CALCAPP  MOVE      C0 TO ATCALC
.              MOve            c0 to Calc53         
.         COMPARE   C0 TO APPCOUNT
.         GOTO      calcadj IF EQUAL
.         MOVE      APPsum TO COUNT
.         MOVE      APPCOUNT TO CALC53
.         DIVIDE    COUNT INTO CALC53
.         MULT      HUNDRED BY CALC53
.              Move            calc53 to Atcalc
.
CALCAdj  MOVE      C0 TO AdCALC
              MOve            c0 to Calc53         
         COMPARE   C0 TO ADJCount
         GOTO      calcpnd IF EQUAL
         MOVE      Adjsum TO COUNT
         MOVE      AdjCOUNT TO CALC53
         DIVIDE    COUNT INTO CALC53
         MULT      HUNDRED BY CALC53
              Move            Calc53 to adcalc
.
CALCPnd  MOVE      C0 TO pCALC
              MOve            c0 to Calc53         
         COMPARE   C0 TO PCOUNT
         GOTO      calclst IF EQUAL
         MOVE      Pordsum TO COUNT
         MOVE      PCOUNT TO CALC53
         DIVIDE    COUNT INTO CALC53
         MULT      HUNDRED BY CALC53
         Move      calc53 to pcalc
CALClst  MOVE      C0 TO lstCALC
              MOve            c0 to Calc53         
         COMPARE   C0 TO lstCOUNT
         GOTO      CalcUlst IF EQUAL
         MOVE      lstsum TO COUNT
         MOVE      lstCOUNT TO CALC53
         DIVIDE    COUNT INTO CALC53
         MULT      HUNDRED BY CALC53
         Move      calc53 to lstcalc
.         
CALCUlst  MOVE      C0 TO lstuCALC
              MOve            c0 to Calc53         
         COMPARE   C0 TO lstuCOUNT
         GOTO      TYPREAD IF EQUAL
         MOVE      lstusum TO COUNT
         MOVE      lstuCOUNT TO CALC53
         DIVIDE    COUNT INTO CALC53
         MULT      HUNDRED BY CALC53
         Move      calc53 to lstucalc
.
TYPREAD
.      READ TYPIST FILE TO GET TYPIST'S NAME.
         CMATCH    " " TO NTypType
         CALL      OOPS IF EQUAL
         CALL      OOPS IF EOS
         MOVE      C2 TO NUSEPATH
         CLEAR     NUSEFLD2
         PACK      NUSEFLD2 FROM NTypType,B1
         CALL      NUSEKEY
         CALL      NOTYPIST IF OVER
.
         DISPLAY   *P1:23,*EL,"WORKING ON ",NTypType,B1,NUSEFLD,B1,NUSEUSER;
          return
.
OOPS     MOVE      "**" TO NTypType
         RETURN
.
NOTYPIST
         MOVE      "TYPIST UNKNOWN " TO NUSEUSER
         RETURN
DETAIL
         MOVE      CANCOUNT,CANC
          if (row >= 7100)        .Position of Largest Possible Last Record
          call      Header
          endif
          prtpage   prfile;*pcolumn:row,*ALIGNMENT=*Left,*font=font8,*boldon,NTypType,B1,nuseuser;
          add       eightlpi,row
          prtpage   prfile;*p1500:row,*ALIGNMENT=*Right,"New";
          prtpage   prfile;*pcolumn1:row,*ALIGNMENT=*Right,ncount:
                    *pcolumn2:row,*ALIGNMENT=*Right,Totcalc:
                    *pcolumn3:row,*ALIGNMENT=*Right,Pcount:
                    *pcolumn4:row,*ALIGNMENT=*Right,Pcalc:
                    *pcolumn5:row,*ALIGNMENT=*Right,Lcount:
                    *pcolumn6:row,*ALIGNMENT=*Right,LTcalc:
                    *pcolumn7:row,*ALIGNMENT=*Right,INVcount:
                    *pcolumn8:row,*ALIGNMENT=*Right,Invcalc:
                    *pcolumn9:row,*ALIGNMENT=*Right,adjcount:
                    *pcolumn10:row,*ALIGNMENT=*Right,adcalc:
                    *pcolumn11:row,*ALIGNMENT=*Right,LStcount:
                    *pcolumn12:row,*ALIGNMENT=*Right,Lstcalc
          add       eightlpi,row
          prtpage   prfile;*p1500:row,*ALIGNMENT=*Right,"Updates";
          prtpage   prfile;*pcolumn1:row,*ALIGNMENT=*Right,REPCNT:
                    *pcolumn2:row,*ALIGNMENT=*Right,REpcalc:
                    *pcolumn3:row,*ALIGNMENT=*Right,PUcount:
                    *pcolumn4:row,*ALIGNMENT=*Right,PUcalc:
.                    *pcolumn5:row,*ALIGNMENT=*Right,Lrepcnt:
.                    *pcolumn6:row,*ALIGNMENT=*Right,Lrcalc:
                    *pcolumn7:row,*ALIGNMENT=*Right,INVRcnt:
                    *pcolumn8:row,*ALIGNMENT=*Right,InvRcalc:
                    *pcolumn11:row,*ALIGNMENT=*Right,LStucount:
                    *pcolumn12:row,*ALIGNMENT=*Right,Lstucalc:
                    *boldoff;
          add       eightlpi,row
          add       eightlpi,row

          return          


HEADER
        add c1 to Pgcnt
        if (pgcnt = c1)
          prtpage prfile;*UNITS=*HIENGLISH:
                       *ORIENT=*LANDSCAPE;        
        else
          prtpage prfile;*NEWPAGE:
                 *UNITS=*HIENGLISH:
                       *ORIENT=*LANDSCAPE;
          endif                       
        clear   row
          move    "300",row
          prtpage prfile;*pcolumn:row,*font=font12,*boldon,"Confidential";
          prtpage prfile;*pTitle1:row,*ALIGNMENT=*CENTER,*font=font12,*ll,"Names in the News";
          prtpage prfile;*pTitle2:row,*ALIGNMENT=*Left,*font=font12,"Date:",*boldoff;
          clock timestamp,str8
          unpack str8,str2,yy,mm,dd
          clear str10
          pack  str10,mm,slash,dd,slash,str2,yy
          prtpage prfile;*pTitle3:row,*font=font12,str10;
          add     eightlpi,row
          add     eightlpi,row
          prtpage prfile;*pTitle4:row,*ALIGNMENT=*CENTER,*font=font12,*ULON,*ll,"YTD Data Entry Analysis",*ULOFF;
          prtpage prfile;*pTitle2:row,*ALIGNMENT=*Left,*font=font12,"Page:",*boldoff;
          prtpage prfile;*pTitle3:row,*font=font12,Pgcnt;
          add     eightlpi,row
          add     eightlpi,row
          prtpage prfile;*pTitle4:row,*ALIGNMENT=*CENTER,*font=font12,*ULON,*ll,BigDate,*ULOFF;
          add     eightlpi,row
          add     eightlpi,row
          prtpage prfile;*pcolumn:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ulon,"Typist",*uloff,*boldoff;
          prtpage prfile;*pcolumn1:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ulon,"Orders",*uloff,*boldoff;
          prtpage prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ulon,"   %   ",*uloff,*boldoff;
          prtpage prfile;*pcolumn3:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ulon,"Pending",*uloff,*boldoff;
          prtpage prfile;*pcolumn4:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ulon,"   %   ",*uloff,*boldoff;
          prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ulon,"Lcr's",*uloff,*boldoff;
          prtpage prfile;*pcolumn6:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ulon,"   %   ",*uloff,*boldoff;
          prtpage prfile;*pcolumn7:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ulon,"Invoices",*uloff,*boldoff;
          prtpage prfile;*pcolumn8:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ulon,"   %   ",*uloff,*boldoff;
          prtpage prfile;*pcolumn9:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ulon,"Adjst",*uloff,*boldoff;
          prtpage prfile;*pcolumn10:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ulon,"   %   ",*uloff,*boldoff;
          prtpage prfile;*pcolumn11:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ulon,"List's",*uloff,*boldoff;
          prtpage prfile;*pcolumn12:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ulon,"   %   ",*uloff,*boldoff;
          add     eightlpi,row
          add     eightlpi,row
          add     eightlpi,row
          
         RETURN
.
EOJ
          if (row >= 7100)        .Position of Largest Possible Last Record
          call      Header
          endif
          add     eightlpi,row
          add     eightlpi,row
          add     eightlpi,row
          add     eightlpi,row
          prtpage   prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font8,*ll,"Total number of new orders:":
                    *pcolumn3:row,*ALIGNMENT=*RIGHT,Ordsum;
          prtpage   prfile;*pcolumn9:row,*ALIGNMENT=*RIGHT,*font=font8,*ll,"Total number of Updates:":
                    *pcolumn10:row,*ALIGNMENT=*RIGHT,OrdRsum;
          add     eightlpi,row
          prtpage prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font8,*ll,"Total number of pnd orders:":
                    *pcolumn3:row,*ALIGNMENT=*RIGHT,POrdsum;
          prtpage   prfile;*pcolumn9:row,*ALIGNMENT=*RIGHT,*font=font8,*ll,"Total number of updates:":
                    *pcolumn10:row,*ALIGNMENT=*RIGHT,PndUsum;
          add     eightlpi,row
          prtpage prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font8,*ll,"Total number of new lcrs:":
                    *pcolumn3:row,*ALIGNMENT=*RIGHT,Lcrsum;
          prtpage   prfile;*pcolumn9:row,*ALIGNMENT=*RIGHT,*font=font8,*ll,"Total number of Updates:":
                    *pcolumn10:row,*ALIGNMENT=*RIGHT,LcrRsum;
          add     eightlpi,row
          prtpage prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font8,*ll,"Total number of new invoices:":
                    *pcolumn3:row,*ALIGNMENT=*RIGHT,Invsum;
          prtpage   prfile;*pcolumn9:row,*ALIGNMENT=*RIGHT,*font=font8,*ll,"Total number of Updates:":
                    *pcolumn10:row,*ALIGNMENT=*RIGHT,InvRsum;
          add     eightlpi,row
          prtpage prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font8,*ll,"Total number of new adjustments:":
                    *pcolumn3:row,*ALIGNMENT=*RIGHT,ADJsum;
          add     eightlpi,row
          prtpage prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font8,*ll,"Total number of new datacards:":
                    *pcolumn3:row,*ALIGNMENT=*RIGHT,Lstsum;
          prtpage   prfile;*pcolumn9:row,*ALIGNMENT=*RIGHT,*font=font8,*ll,"Total number of updates:":
                    *pcolumn10:row,*ALIGNMENT=*RIGHT,Lstusum;
          add     eightlpi,row

STOP     DISPLAY   *P1:24,*EL,*B,"JOB DONE, SHUTTING DOWN TO CONTINUE CHAIN":
                   *W2,*B;
          PRtclose  PrFile
CheckFile

          pack      str55 from "c:\work\pdf\Typistrep.pdf"
          trap      WaitForEnd giving error if IO
          open      FileCheck,STR55,Exclusive     
          Close     FIleCHeck
          trap      IOMssg GIVING ERROR IF IO
          Pause     "3"
          MOVE      C0,TrapCount
          Pack       MailSubjct,"Data Entry report  "
          pack      MailAttach from str55

        
.          move      "DavidHerrick@nincal.com" to mailto
.          move      "DavidHerrick@nincal.com,ReubenHolland@nincal.com,SuzanneMcGuire@nincal.com" to mailto
          move      "DavidHerrick@nincal.com,DeniseHubbard@nincal.com,SuzanneMcGuire@nincal.com" to mailto
          move      "AmyFrey@nincal.com,JenniferCox@nincal.com,KrsniWatkins@nincal.com" to mailcc
          move      "creques@nincal.com" to mailfrom

..First check 995 autolaunch settings
.begin patch 1.10
.        call       GetPDFPath
.                    pack      str45 from PDFPATH,"\res\pdf995.ini"
. 
.                   call      "GU$INI;WRITE_TO_INI" USING str45:
.                             "Parameters":
.                             "ProcessPDF":
.                             "\\nins1\e\apps\winbatch\Del995flag.exe":
.                              result
.                              if (result = C0)
..Prepare Flag file
.                                        pack      str45 from PDFPATH,"\Flag.dat"
.                                        prep      tempfile,str45
.                                        write     tempfile,SEQ;"flag set"
.                                        close     tempfile
.                              endif
.                    Call      PDF995Auto
.end patch 1.10
                    Move      "30",MailTimer
                    call      SendMail
         shutdown   "CLS"
         STOP
WaitForEnd
                    TrapClr   IO
.check the error if file does not exist just get out
                    add       C1,Trapcount
                    pause     c5
                    noreturn
                    if        (trapcount > 36)   . 3 min are you kidding me
                    Pack       MailSubjct,"data Enry report - ",str55
                    Move      "CReques@nincal.com",MailFrom
                    Move      "CReques@nincal.com",MailTO
                    append    CRLF,MailBOdy
                    append    str55,MailBody
                    append    CRLF,MailBOdy
                    append    "I am sorry I could not send the file",Mailbody
                    append    CRLF,MailBOdy
                    append    "c:\work\pdf\typistrep.pdf",Mailbody
                    append    CRLF,MailBOdy
                    reset     Mailbody
                    Move      B1,Mailattach
                    call      SendMail
                    return

                    endif
          
                    goto      checkfile


. IO - I/O ERROR TRAPS
IO
         TRAPCLR   IO
         NORETURN
IOBRANCH
         DISPLAY   *P1:23,*EL,ERROR
         BRANCH    IOERROR OF FILE1
         DISPLAY   *P1:24,*EL,"UNKNOWN RUN TIME IO ERROR",*B;
         GOTO      IOEXIT
FILE1
         DISPLAY   *P1:24,"ORDER PRINT FILE ERROR",*B,*W2;
         GOTO      IOEXIT
IOEXIT
         KEYIN     *P50:24,ANS;
         CMATCH    "Q" TO ANS
         GOTO      IOBRANCH IF NOT EQUAL
         shutdown   "CLS"
         STOP
         INCLUDE   NUSEIO.inc
         INCLUDE   NTYPIO.inc
         INCLUDE   COMLOGIC.inc
