.
. PURPOSE - PRINT REPORT FOR DAILY ORDERS SHOWING:
.           1)TOTAL ORDERS PRINTED.
.           2)NUMBER OF NEW ORDERS PER TYPIST.
.           3)NUMBER OF REPRINTS PER TYPIST.
.           4)PERCENTAGES OF ABOVE COMPARED TO TOTALS.
. .............................................................................
. FILES.
. ......
.INPUT   FILE      ORDER DAILY PRINT FILE 
.
PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
         INCLUDE   NTYPDD.inc
         INCLUDE   NORDDD.inc
         INCLUDE   NUSEDD.inc
         INCLUDE   HP.inc
.
INPUT   FILE      FIXED=696,STATIC=12

RElease       Init            "2.32"          JD updated input file nprint.tmp
Reldate   Init      "18 June 2008"
.RElease       Init            "2.31"         DLH Internal index
.Reldate  Init      "23 April 2008"
.RElease       Init            "2.3"          Feb2006 DLH New file stucture new logic etc
. .............................
FILL3    DIM       3
DATE     DIM       8
.
. ..............
. OTHER VARIABLES.
NOrdTypRec     REcord         (40)  
TypeREc       Dim             3                 ;typist inits
Nqtyrec       Form            9                 ;order qty
Countrec      Form            4                 ;number of new orders            
RepRec        Form            4                 ;number reprint orders
RePQtyREc     Form            9                 ;reprint qty            
PordRec       form            4                 ;pending order count
LCRRec        Form            4                 ;new lcr count
              RecordEnd

NQTY          FORM      9            NEW ORDER QTY
NCOUNT        FORM      4            NUMBER OF NEW ORDERS
REPCNT        FORM      4            NUMBER OF REPRINT ORDERS
Pcount        form            4            pending orders
Lcount        form            4              lcr's
SUBRTY        FORM      9                    order reprint qty 
COUNT         FORM      5
BRANCH        FORM      "00"
.R             INIT      "R"
QTY           FORM      9
.END           FORM      2
HUNDRED       FORM      "100"
TOTREP        FORM      3
Calc53        Form            5.3
REPCALC       FORM      3.2        *percentage of reprint orders
TOTCALC       FORM      3.2        *percentage of orders
LTCALC        FORM      3.2        *PERCENTAGE OF NEW LCR'S.
LRCALC        FORM      3.2        *PERCENTAGE OF REPRINT LCR'S.
ATCALC        FORM      3.2        *PERCENTAGE OF NEW APPROVALS.
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
InvSUm        form            5
InvRsum       Form            5
CorSum        form            5
CancSum       form            5
appsum        form            5
adjsum        form            5
lstsum        form            5
lstUsum       form            5
Datecheck     dim             8
. PROGRAM MAIN.
         MOVE      "NINCAL" TO COMPNME
         MOVE      "ORDER TYPIST DATE PROGRAM" TO STITLE
         MOVE      "EXIT" TO PF5
         CALL       PAINT
         CALL       FUNCDISP
         TRAP      DOWN IF F5
         CLOCK     DATE TO DATE
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

         clock     timestamp,str6
         unpack    str6,str2,yy,mm
              Move            str2 to cc         
         pack      TypDate from str6
         
         rep       Zfill in typDate
.patch 2.31
         pack            Datecheck from cc,yy,mm,dd
.patch 2.31        
.         
.testing
.          move      "20090622",datecheck

.begin patch xxx
          Rep       LowUP,Program
          if         (PROGRAM = "NORD0014")  .chained from dsinit
          move      "D" to ans
          goto       Daily
          endif
.end patch xxx

CHOOSE   move      "D" to ans
         KEYIN     *P25:14,"(D)aily processing, (P)rint report, (K)ill file":
                   " ",*T60,*RV,ANS;
         CMATCH    "D" TO ANS
         GOTO      DAILY IF EQUAL
         CMATCH    "R" TO ANS
         GOTO      Repair IF EQUAL
         CMATCH    "P" TO ANS
         IF EQUAL
              KEYIN           *P10:12,"Report DATE ",*p22:12,*el,*+,*dv,mm,"/",*dv,dd,"/",*dv,cc,*dv,yy:
                              *p22:12,*t30,*ZF,*JR,*rv,MM,*DV,SLASH,*rv,DD:
                              *DV,SLASH,*rv,cc,*rv,YY
              PACK            PRTDATE FROM MM,SLASH,DD,SLASH,YY
              PACK            DATE FROM MM,SLASH,DD,SLASH,YY
              pack            TypDate from cc,yy,mm
              pack            Datecheck from cc,yy,mm,dd
              
                   
         PACK      STR35,NTWKPATH1,"ntypst.lst"
.test test test
         splopen   STR35
.          Splopen   "","Q"
.test test test

         print     hpland,hp14ptch,hplin8,hptop
         goto      totcalc
         endif
         CMATCH    "K" TO ANS
         GOTO      CHOOSE IF NOT EQUAL
         KEYIN     *P1:24,"THIS WILL DELETE ALL RECORDS IN THE FILE OK??",ANS;
         CMATCH    "Y" TO ANS
         GOTO      DOWN IF NOT EQUAL
.
              PACK            STR55,NTWKPATH1,"text\typout.dat"
              Erase           str55
.begin patch 2.31
.         PACK      TASKNAME,"\\nts0\c\apps\plb\code\sunindex.exe ",NTWKPATH1,"text\typout.dat ",NTWKPATH1,"index\typout.isi,L83 -1-9,e,n"
          PACK      TASKNAME,NTWKPATH1,"text\typout.dat ",NTWKPATH1,"index\typout.isi,L83 -1-9,e,n"
.         execute   TASKNAME
          INdex   TASKNAME
.end patch 2.31
DOWN
              Shutdown        "CLS"
. ...................
DAILY
.         DISPLAY    *P25:12,"NUMBER OF ORDERS READ ";
.
         TRAP      IO GIVING ERROR IF IO
         MOVE      C1 TO IOERROR

               if             (func <> "1" & Func <> "2")
               move           c1,func
               endif
.begin patch 2.32
          if              (Func = "1")           .hourly run        
          OPEN      INPUT,"NPRINT.tmp",SHARE
         DISPLAY    *P25:12,"NUMBER OF ORDERS READ ";
         Elseif              (Func = "2")           .LCR Nightly run        
          OPEN      INPUT,"LcrPRINT.lcr",SHARE
         DISPLAY    *P25:12,"NUMBER OF LCR's READ ";
          endif
.        OPEN      INPUT,"NPRINT.srt",SHARE
.end patch 2.32

         SUB       IOERROR FROM IOERROR
         TRAPCLR    IO
READ     READ      INPUT,SEQ;ORDVARS
         GOTO      OUTPUT IF OVER
.patch 5    New values for OSTAT  p Lower case = Pending order (awaiting LO/manager Approval)
.                                 x Lower case = Cancellation of above (never approved)
.                                 l Lower case = LCR
.                                 z Lower case = Cancellation of LCR
.add lcr stuff later
               if              (Func = "1")           .hourly run
.patch 2.31
.                              pack      str4 from "xlz"
                              pack      str4 from "xlzp"
.patch 2.31
                              Scan             Ostat in str4
                              goto            read if equal
                              RESET     CANCODES
                              SCAN      OSTAT IN CANCODES
                              GOTO      READ IF EQUAL
.                                      0-Live order
.                                      B-Billed order
.                                      Q-Cancelled/Billed order
.                                      X-Cancelled order
.                                      e-Live Order with Estimated Invoice uses "X" if cancelled         

               ElseIf         (Func = "2")               
                              pack      str4 from "px"            .nightly run include LCr's
                              Scan             Ostat in str4
                              goto            read if equal
.                              
                                             If             (Ostat = "p" or Ostat = "l" or Ostat = "x")    pending or lcr or canc lcr
                                             pack           str8 from OODTEC,OODTEy,OODTEm,OODTEd
                                             match          str8 to datecheck
                                             goto           read if not equal
                                             endif
              Else                                                   ;darn func check failed
.                              pack      str4 from "xlz"
.                              Scan             Ostat in str4
.                              goto            read if equal
              endif              
         MATCH     "      " TO OLRN
         GOTO      READ IF EQUAL     *CHECK FOR NULL 1ST RECORD.
         GOTO      READ IF EOS
         ADD       C1 TO COUNT
         DISPLAY   *P48:12,*EL,COUNT;
         GOTO      BREAK
.
BREAK
              MOVE      ODOWJ TO TYPe
              GOTO      ADD
.
PASSONE
.
ADD
              MOVE      OQTY TO QTY
SUBTOT
              FOR           Branch,"1","40"
                                            
                              If             (Type = NordTypRec(branch).TypeRec)                            
                                             Move           NordTypREc(Branch).Nqtyrec,nqty                  ;# new Names
                                             Move           NordTypREc(Branch).Repqtyrec,subrty
                                             Move           NordTypREc(Branch).Countrec,Ncount
                                             Move           NordTypREc(Branch).PordRec,Pcount
                                             Move           NordTypREc(Branch).LcrRec,LCount
                                             Move           NordTypREc(Branch).Reprec,RepCnt
                                             if             (Ostat = "B" & (type = "ARB" or type = "AMB"))
                                             add            qty to nqty                                  ;# new Names
                                             add            c1 to Ncount
                                             elseif         (ostat = "R")          .reprint
                                             add            qty to subrty
                                             add            c1 to repcnt
                                             Elseif         (Ostat = "p")           .pending
                                             Add            C1 to PCount
                                             Elseif         (Ostat = "l")           .LCR
                                             add            c1 to Lcount
                                             Elseif         (Ostat = "x")           .canc LCR
                                             add            c1 to Lcount
                                             Else                                   .must be regular live order
                                             add            qty to nqty
                                             add            c1 to Ncount
                                             endif
                                             Move           Nqty,NordTypREc(Branch).Nqtyrec              ;# new Names
                                             Move           subrty,NordTypREc(Branch).Repqtyrec
                                             Move           NCount,NordTypREc(Branch).Countrec
                                             Move           PCount,NordTypREc(Branch).PordRec
                                             Move           LCount,NordTypREc(Branch).LcrRec
                                             Move           RepCnt,NordTypREc(Branch).Reprec
                             Break                
                             endif
                             if              (Nordtyprec(Branch).Typerec = "")
                                             if             (Ostat = "B" & (type = "ARB" or type = "AMB"))
                                             add            qty to nqty                ;# new Names
                                             add            c1 to Ncount
                                             ElseIf        (ostat = "R")
                                             add            qty to subrty
                                             add            c1 to repcnt
                                             Elseif         (Ostat = "p")           .pending
                                             Add            C1 to PCount
                                             Elseif         (Ostat = "l")           .LCR
                                             add            c1 to LCount
                                             Elseif         (Ostat = "x")           .canc LCR
                                             add            c1 to LCount
                                             Else                                   .must be regular live order
                                             add            qty to nqty
                                             add            c1 to Ncount
                                             endif
                                             Move           Nqty,NordTypREc(Branch).Nqtyrec              ;# new Names
                                             Move           subrty,NordTypREc(Branch).Repqtyrec
                                             Move           NCount,NordTypREc(Branch).Countrec
                                             Move           RepCnt,NordTypREc(Branch).Reprec
                                             Move           PCount,NordTypREc(Branch).PordRec
                                             Move           LCount,NordTypREc(Branch).LcrRec
                                             Move           Type,NordTypREc(Branch).Typerec
                             break                
                             endif                
           repeat

.
         MOVE      C0 TO NCOUNT
         MOVE      C0 TO LCOUNT
         MOVE      C0 TO PCOUNT
         MOVE      C0 TO SUBRTY
         MOVE      C0 TO NQTY
         MOVE      C0 TO REPCNT
              goto            read
         RETURN
.
OUTPUT
         DISPLAY   *P1:24,"PREPARING FILE OUTPUT",*B;
         SUB       TOTREP FROM COUNT   *SUBTRACT REPRINTS FROM TOTAL.
OUTPUT1
.      UNLOAD TABLES.
              FOR           Branch,"1","40"
                                            
                                             Move           NordTypRec(branch).TypeRec,Type                              
                                             Move           NordTypREc(Branch).Nqtyrec,nqty     ;# new Names
                                             Move           NordTypREc(Branch).Repqtyrec,subrty    ;order reprint qty
                                             Move           NordTypREc(Branch).Countrec,Ncount     ;new orders 
                                             Move           NordTypREc(Branch).Reprec,RepCnt       ;order reprint count
                                             Move           NordTypREc(Branch).PordRec,Pcount      ;pending
                                             Move           NordTypREc(Branch).LcrRec,LCount       ;lcrs
                             if              (Nordtyprec(Branch).Typerec = "")
                             break  
                             goto            outlast
                             else
         MATCH     "  " TO TYPe
         CALL      NOTYP IF EQUAL
         CALL      NOTYP IF EOS
              Move            c0 to n3
              move            type to n3
              if              (n3 = "99")
              goto            outofhere
              endif
.         DISPLAY   *P1:24,*EL,"READ  KEY= ",NTYPFLD;
              MOVE      TYPE TO ODOWJ                            .save it
              MOVE       C1 TO NTYPPATH                         ;detail record
              Move            Str6 to TypDate
              Packkey         Ntypfld from TypDate,type         
.add to totals              
              ADD             NQTY TO QtySum        ;totals         ;# new Names
              ADD             NCOUNT TO OrdSum        ;totals
              ADD             REPCNT TO OrdRSum        ;totals
              add             Pcount to Pordsum        ;totals
              add             LCount to LcrSum        ;totals
.do we have a detail record for this typist this month?
              CALL            NTYPtst
              GOTO            WRITE IF OVER                      .Nope go make one
              CALL            NTYPKEY                    .yes we do read it and update
               ADD            NQTY TO SUBQTY                      ;Detail         ;# new Names
               ADD            NCOUNT TO SUBCOUNT                  ;Detail     
               ADD            REPCNT TO REPCOUNT                  ;detail
               add            Pcount to Pndcount            ;detail
               add            LCount to LSUBCNT             ;detail
.              DISPLAY   *P1:24,*EL,"UPDATE";
              CALL            NTYPUPD
              GOTO            OUTPUTX
NOTYP    
              Move            Str6 to TypDate
              Packkey         Ntypfld from TypDate,"???"
              move            "???" to Type
              RETURN
WRITE
              MOVE            ODOWJ TO TYPe
              Move            str6 to Typdate
              Packkey         Ntypfld from Typdate,Type

               ADD            NQTY TO SUBQTY                      ;Detail         ;# new Names
               ADD            NCOUNT TO SUBCOUNT                  ;Detail     
               ADD            REPCNT TO REPCOUNT                  ;detail
               add            Pcount to Pndcount            ;detail
               add            LCount to LSUBCNT             ;detail
              DISPLAY         *P1:24,*EL,"WRITE  KEY=",NTYPFLD;
              CALL            NTYPWRT
.
. OUTPUTX - OUTPUT SECTION EXIT.
OUTPUTX
                              endif                
OutOfHere
              repeat
OUTLAST
              DISPLAY   *P1:24,*EL,"FINAL READ";
              Move            str6 to Typdate
              Packkey         Ntypfld from Typdate,"99 "
.              move            NTypfld to Idnum
              Move            "99 " to type
.              MOVE      C2 TO NTYPPATH
              CALL      NTYPtst
              GOTO      NONINE IF OVER
              CALL      NTYPKEY
.              add             OrdSum to STotal
.              add             OrdRsum to RTotal
.              Add             QtySum to QtyTotal
.              add             LcrSum to LCSTot
.              add             LcrRsum to LCRTot
.              add             Pordsum to PndTot
              add             OrdSum to SUbCount
              add             OrdRsum to RepCount
              Add             QtySum to SubQty
              add             LcrSum to LSUBCNT
              add             LcrRsum to LREPCNT
              add             Pordsum to PndCOunt
              DISPLAY   *P1:24,*EL,"FINAL WRITE";
               CALL       NTYPUPD
         GOTO      STOP
NONINE   
              Move            str6 to Typdate
              Packkey         Ntypfld from Typdate,"99 "
.              move            nTypfld to Idnum
              Move            "99 " to type
.              MOve            OrdSum to STotal
.              MOve            OrdRsum to RTotal
.              MOve            QtySum to QtyTotal
.              MOve            LcrSum to LCSTot
.              MOve            LcrRsum to LCRTot
.              Move            Pordsum to PndTot
              Move            OrdSum to SUbCount
              Move            OrdRsum to RepCount
              Move            QtySum to SubQty
              Move            LcrSum to LSUBCNT
              Move            LcrRsum to LREPCNT
              Move            Pordsum to PndCOunt
              CALL      NTYPWRT
              GOTO      STOP
.
..........................................................................................
. TOTCALC - READ AND PRINT FROM FILE TYPOUT.
TOTCALC
              CALL      HEADER
.              Packkey         Ntypfld from Typdate,"99 "

.              MOVE      C2 TO NTYPPATH
          Move      DateCheck,str6
.
READKS   MOVE      C1 TO NTYPPATH
         MOVE      C0 TO COUNT
         MOVE      C0 TO TOTREP
         CALL      NTYPKS
         GOTO      EOJ IF OVER
         call       debug
          If             (str6 <> typDate)
          goto           readks                   ;different month
          endif
              call            Trim using type
              move            c0 to n3
              Move            Type to n3     
              if              (n3 = "99")
               Move           Typdate to str6
              Move            SUBCOUNT to ordsum
              Move            REPCOUNT to OrdRsum
              Move            SUBQTY   to QtySum
              Move            LSUBCNT  to LCrsum
              Move            LREPCNT  to LcrRSum
              Move            INVCOUNT to Invsum     
              Move            INVRCNT  to InvRsum    
              Move            CORCOUNT To CorSum     
              Move            CANCOUNT to Cancsum    
              Move            APPCOUNT to AppSum     
              Move            ADJCount to AdjSum     
              Move            PndCOunt to Pordsum    
              Move            lstCount to Lstsum     
              move            LStUCOunt to LstUSum
              
              goto            readks
              endif
.
CALCPER
.      CALCULATE PERCENTAGE OF NEW ORDERS.
         MOVE      C0 TO TOTCALC
              MOve            c0 to Calc53         
         COMPARE   C0 TO SUBCOUNT
         GOTO      CALCREP IF EQUAL
.         MOVE      STotal TO COUNT
              Move            Ordsum to count
         MOVE      SUBCOUNT TO CALC53
         DIVIDE    COUNT INTO CALC53
         MULT      HUNDRED BY CALC53
              Move            calc53 to totcalc
CALCREP
.      CALCULATE PERCENTAGE OF REPRINTS.
         MOVE      C0 TO REPCALC
              MOve            c0 to Calc53         
         COMPARE   C0 TO REPCOUNT
         GOTO      CALCLCR IF EQUAL
              Move            Ordsum to totrep
.         MOVE      RTotal TO TOTREP
         MOVE      REPCOUNT TO CALC53
         DIVIDE    TOTREP INTO CALC53
         MULT      HUNDRED BY CALC53
              Move            calc53 to repcalc
.
.CALCLCR - CALC LIST CLEARANCE REQ'S.
CALCLCR
         MOVE      C0 TO LTCALC
              MOve            c0 to Calc53         
         COMPARE   C0 TO LSUBCNT
         GOTO      CALCLCR1 IF EQUAL
.         MOVE      LCSTot TO COUNT
         MOVE      LCrsum TO COUNT
         MOVE      LSUBCNT TO CALC53
         DIVIDE    COUNT INTO CALC53
         MULT      HUNDRED BY CALC53
              MOve            calc53 to ltcalc
.
CALCLCR1
         MOVE      C0 TO LRCALC
              MOve            c0 to Calc53         
         COMPARE   C0 TO LREPCNT
         GOTO      CALCINV IF EQUAL
.         MOVE      LCRTot TO COUNT
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
.         MOVE      INVTOT TO COUNT
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
.         GOTO      TYPREAD IF EQUAL
         GOTO      CALCAPP IF EQUAL
.         MOVE      INVRTOT TO COUNT
         MOVE      INVRsum TO COUNT
         MOVE      INVRCNT TO CALC53
         DIVIDE    COUNT INTO CALC53
         MULT      HUNDRED BY CALC53
              Move            Calc53 to invrcalc
.
CALCAPP  MOVE      C0 TO ATCALC
              MOve            c0 to Calc53         
         COMPARE   C0 TO APPCOUNT
         GOTO      calcadj IF EQUAL
         MOVE      APPsum TO COUNT
         MOVE      APPCOUNT TO CALC53
         DIVIDE    COUNT INTO CALC53
         MULT      HUNDRED BY CALC53
              Move            calc53 to Atcalc
.
CALCAdj  MOVE      C0 TO AdCALC
              MOve            c0 to Calc53         
         COMPARE   C0 TO AdjCOUNT
         GOTO      calcpnd IF EQUAL
         MOVE      Adjsum TO COUNT
         MOVE      AdjCOUNT TO CALC53
         DIVIDE    COUNT INTO CALC53
         MULT      HUNDRED BY CALC53
              Move            Calc53 to adcalc
.
CALCPnd  MOVE      C0 TO pCALC
              MOve            c0 to Calc53         
         COMPARE   C0 TO PndCOUNT
         GOTO      calclst IF EQUAL
         MOVE      Pordsum TO COUNT
         MOVE      PndCOUNT TO CALC53
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
         CMATCH    " " TO TYPE
         CALL      OOPS IF EQUAL
         CALL      OOPS IF EOS
         MOVE      C2 TO NUSEPATH
.         MOVE      NTYPFLD TO NUSEFLD
         CLEAR     NUSEFLD2
         PACK      NUSEFLD2 FROM TYPE,B1
         CALL      NUSEKEY
         CALL      NOTYPIST IF OVER
.
         DISPLAY   *P1:23,*EL,"WORKING ON ",TYPE,B1,NUSEFLD,B1,NUSEUSER;
         GOTO      DETAIL
.
OOPS     MOVE      "**" TO TYPE
         RETURN
.
NOTYPIST
         MOVE      "TYPIST UNKNOWN " TO NUSEUSER
         RETURN
DETAIL
         MOVE      CANCOUNT,CANC
         COMPARE   "45" TO LINES
         CALL      HEADER IF NOT LESS
         PRINT     *N,TYPE,B1,NUSEUSER;
         PRINT     *N,*20,"NEW",*32,SUBCOUNT,*41,TOTCALC,*51,pndcount,*60,Pcalc;
         PRINT     *70,LSUBCNT,*80,LTCALC,*89,INVCOUNT,*96,INVCALC:
                   *108,AdjCOUNT,*114,AdCALC,*127,lstcount,*136,lstcalc:
                   *N,*20,"REPRINT",*32,REPCOUNT,*41,REPCALC;
         PRINT     *70,LREPCNT,*80,LRCALC,*89,INVRCNT,*96,INVRCALC,*127,lstucount,*136,lstuCalc
         ADD       "3" TO LINES
         GOTO      READKS
HEADER
         MOVE      "7" TO LINES
         PRINT     *F,*l,*l,*41,"***   N A M E S   I N   T H E   ";
         PRINT     *74,"N E W S   ***",*140,PRTDATE:
                   *L,*140,"PAGE :",PAGE:
                   *N,*54,HPBON," TYPIST ANALYSIS":
                   *L,*L,*1,"TYPIST",*32,"ORDERS",*43,"% ",*51,"PENDNG",*62,"%";
         PRINT     *70,"LCR'S",*81,"% ",*89,"INVOICE",*100,"%",*108,"ADJST",*119,"%":
                   *127,"Lists",*138,"%":
                   *L,*1,"------------------------";
         PRINT     *32,"------",*40,"-------",*51,"------",*59,"------",*70,"------",*80,"------",*89;
         PRINT     "--------",*97,"-------":
                   *108,"------",*116,"-------",*127,"------"
         ADD       C1 TO PAGE
         RETURN
.
EOJ
         COMPARE   "45" TO LINES
         CALL      HEADER IF NOT LESS
         PRINT     *L,*L;
         PRINT     *L,*30,"TOTAL NUMBER OF NEW ORDERS     :",Ordsum:
                   *80,"TOTAL NUMBER OF REPRINTS    :",ordRsum:
                   *L,*30,"TOTAL NUMBER OF PND ORDERS     :",Pordsum;
         PRINT     *L,*30,"TOTAL NUMBER OF NEW LCR'S      :",LCrsum;
         PRINT     *80,"TOTAL NUMBER OF REPRINTS    :",LCRrsum;
         PRINT     *L,*30,"TOTAL NUMBER OF NEW INVOICES   :",INVsum:
                   *80,"TOTAL NUMBER OF REPRINTS    :",INVRsum;
.         PRINT     *L,*30,"TOTAL NUMBER OF NEW APPROVALS:",APPTOT
         PRINT     *L,*30,"TOTAL NUMBER OF NEW DATACARDS   :",LSTsum:
                   *80,"TOTAL NUMBER OF UPdates    :",LstUsum;
         PRINT     *L,*30,"TOTAL NUMBER OF NEW ADJUSTMENTS:",ADJsum
.         PRINT     *L,*30,"TOTAL NUMBER OF CORRECTIONS FOR MONTH:",CORsum;
.        PRINT     *L,*30,"TOTAL NUMBER OF CANCELATIONS FOR MONTH:",CANcsum
STOP     DISPLAY   *P1:24,*EL,*B,"JOB DONE, SHUTTING DOWN TO CONTINUE CHAIN":
                   *W2,*B;
         CMATCH    "D" TO ANS
         IF         equal
         Shutdown             "CLS"
         endif
.         STop       IF EQUAL
         PRINT     HPPORT,HPRESET
         SPLCLOSE
         shutdown   "CLS"
         STOP
Repair
              KEYIN           *P10:12,"REPAIR CCYYMM ",*p22:12,*el,*+,*dv,CC,*dv,YY,*dv,MM:
                              *p22:12,*t30,*ZF,*JR,*rv,CC,*rv,YY,*rv,mm
              pack            ntypfld from cc,yy,mm
              move            ntypfld to str6
              call            Ntyptst
RepLoop       call            nTypks
              if              Not over
              scan            "99" in type
              goto            reploop if equal
                              if              (typdate = str6)
                              goto           repair1
                              else
                              goto           RepLoop
                              endif
Repair1       add            SUBCOUNT to ordsum
              add             REPCOUNT to OrdRsum
              add             SUBQTY   to QtySum
              add             LSUBCNT  to LCrsum
              add             LREPCNT  to LcrRSum
              add             INVCOUNT to Invsum     
              add             INVRCNT  to InvRsum    
              add             CORCOUNT To CorSum     
              add             CANCOUNT to Cancsum    
              add             APPCOUNT to AppSum     
              add             ADJCount to AdjSum     
              add             PndCOunt to Pordsum    
              add             lstCount to Lstsum     
              add             LStUCOunt to LstUSum
              goto            reploop
              else
              packkey         ntypfld from str6,"99 "
              call            Ntypkey
              Move            OrdSum to SUbCount
              Move            OrdRsum to RepCount
              Move            QtySum to SubQty
              Move            LcrSum to LSUBCNT
              Move            LcrRsum to LREPCNT
              Move            Pordsum to PndCOunt
              Move            Invsum to invcount
              Move            InvRsum to INVRcnt
              Move            Corsum to Corcount
              move            Cancsum to cancount
              move            appsum to appcount
              move            adjsum to adjcount
              move            lstsum to lstcount
              move            lstusum to lstUcount
              call            ntypupd
              endif
              shutdown        "cls"
              stop
.              



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
.         SHUTDOWN  "ALERT"
         shutdown   "CLS"
         STOP
         INCLUDE   NUSEIO.inc
         INCLUDE   NTYPIO.inc
         INCLUDE   COMLOGIC.inc

