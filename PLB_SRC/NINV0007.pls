PC       EQU       0
         INCLUDE   COMMON.INC
         INCLUDE   CONS.INC
         INCLUDE   NPASDD.INC
         INCLUDE   NSHPDD.INC
         include   ndat3dd.inc
         INCLUDE   NORDDD.INC
.START PATCH 2.72 ADDED LOGIC
          INCLUDE   NSEL2DD.INC
.END PATCH 2.72 ADDED LOGIC
release  init      "2.73"             04OCT07 Check PLI Listmgmt
.release  init      "2.72"             12FEB04 ASH DATACARD CONVERSION
.release  init      "2.71"             01FEB02 added more display for non live orders.
.release  init      "2.7"             28Dec98 ASH NINORD Y2K, File expansion
.release  init      "2.6"             26jan98 DLH Allow new Exchange Management Fee
.release  init      "2.5"             05nov96 DLH
.release  init      "2.4"             12mar96 close if rental. (passworded)
.release  init      "2.3"            28jan96 DLH $/date select code added
.                                   rentals priced at 0/m. 
.release  init      "2.1"            18jul95 if reason given brk then verify
.release  init      "2.0"            ;24feb95 DLH if we bill r/c ask if
.                                    ready to create entry & if true chain
.                                    ninv0008.
.release  init      "1.6"            DLH 17nov94 reason code 5
.RELEASE  INIT      "1.5"            DLH 20MAR92
.
. ...........................................
NOBILLED IFILE     KEYLEN=6,fixed=35
.NOBILLED IFILE     KEYLEN=6,VAR=35,COMP
. ...........
.
BYTE1    INIT      "N"
PASS     DIM       5
PASSWORD DIM       6
PASSNAME DIM       10
KEY      DIM       6
REASON   DIM       1
RUNSW    DIM       1
DATE     DIM       8
. .........................................................................
START    
         MATCH     "NINV0008" TO PROGRAM       *RETURNED FROM RUN CHARGE?
         IF        EQUAL
         MOVE      YES TO RUNSW
         ENDIF
         MOVE      "NINV0007" TO PROGRAM
         MOVE      "Names in the News Cal" TO COMPNME
         MOVE      "MARK ORDERS BILLED/DEAD" TO STITLE
         CLOCK     DATE TO DATE
         IFNZ      PC
         UNPACK    DATE INTO MM,DD,YY
         PACK      TODAY FROM MM,SLASH,DD,SLASH,YY
         XIF
         IFZ       PC
         MOVE      DATE TO TODAY
         UNPACK    DATE INTO MM,STR1,DD,STR1,YY
         XIF
         CALL      PAINT
         MOVE      C1 TO NORDPATH
         CMATCH    YES TO RUNSW
         IF         EQUAL
         CLEAR      RUNSW
         GOTO       main
         ENDIF
         KEYIN     *P12:10,"PASSWORD PLEASE";
         KEYIN     *P1:24,*EOFF,NPASKEY;
         CMATCH    " " TO NPASKEY
         GOTO      NOGOOD IF EOS
         PACK      NPASFLD FROM BYTE1,NPASKEY
         CALL      NPASKEY
         GOTO      NOGOOD IF OVER
         DISPLAY   *P12:10,"PASSWORD ACCEPTED";
main     OPEN      NOBILLED,"NOBILLED"
KEYIN    KEYIN     *P1:5,*EF,*P1:24,*EL,"ENTER (*) TO EXIT":
                   *P5:8,"ENTER LR : ",*JR,*ZF,NORDFLD;
         cmatch    b1 to nordfld
         goto      keyin if eos
         MATCH     "000000" TO NORDFLD
         GOTO      KEYIN IF EQUAL
         MATCH     "00000*" TO NORDFLD
         GOTO      STOP IF EQUAL
         rep       zfill in nordfld
         CALL      NORDKEY
         GOTO      NOLR IF OVER
         RESET     CANCODES
         SCAN      OSTAT IN CANCODES
         GOTO      CANCEL IF EQUAL
.         MATCH     "X" TO OSTAT
.         GOTO      CANCEL IF EQUAL
         MATCH     "B" TO OSTAT
         GOTO      Billed IF EQUAL
         MATCH     "p" TO OSTAT
         GOTO      PENDING IF EQUAL
         MATCH     "l" TO OSTAT
         GOTO      lcr IF EQUAL
.         MATCH     "Q" TO OSTAT
.         GOTO      CANCEL IF EQUAL
.START PATCH 2.72 REPLACED LOGIC
.         DISPLAY   *P5:10,"LR##: ",NORDFLD,"   QTY: ",OQTY,"  PRICE: ",OPPM:
.                   *p5:12,"               splt Qty: ",oexqty:
.                   *P5:14,"LIST : ",O1DES;
          packkey   NSEL2FLD,"1",OLRN
          move      "NSEL2KEY",Location
          pack      KeyLocation,"Key: ",NSEL2FLD
          call      NSEL2KEY
          if over
                    unpack    OPPM,str3,str2
                    pack      str6,str3,".",str2
                    rep       zfill,str6
                    move      str6,NSEL2PRICE
          endif
          DISPLAY   *P5:10,"LR##: ",NORDFLD,"   QTY: ",OQTY,"  PRICE: ",NSEL2PRICE:
                    *p5:12,"               splt Qty: ",oexqty:
                    *P5:14,"LIST : ",O1DES;
.END PATCH 2.72 REPLACED LOGIC
         MOVE      OLNUM TO NDAT3FLD
         CLEAR     NDATTDMC
         rep       zfill in ndat3fld
         CALL      NDAT3KEY
         IF        NOT OVER                  *TDMC BILLING INFO.
         CLEAR     STR10
         CMATCH    "B" TO NDATTDMC
           IF        EQUAL
           MOVE      "BOTH" TO STR10
           ENDIF
           CMATCH    "R" TO NDATTDMC
           IF        EQUAL
           MOVE      "RENT/SPLIT" TO STR10
           ENDIF
           CMATCH    "E" TO NDATTDMC
           IF        EQUAL
           MOVE      "Exch/only" TO STR10
           ENDIF
         cmatch    "Y" to ndatdolc
         if         equal
         move       "$/Date" to str6
         endif

         cmatch     yes to ndat3exh
         if        not equal
         DISPLAY   *P1:20,*EL,"TDMC RUN/CHRGE $ WE BILL : ",STR10,b1:
                   *blinkon,str6,*blinkoff
         else
         DISPLAY   *P1:20,*EL,"TDMC RUN/CHRGE $ WE BILL : ",STR10,b1:
                   *blinkon,str6,*blinkoff," & Exchange Fee"       
         ENDIF
         endif
         KEYIN     *P1:23,"IS THIS THE ORDER YOU WISH TO MARK BILLED?",str1;
         CMATCH    yes TO str1
         GOTO      REASON IF EQUAL
         GOTO      KEYIN
REASON   KEYIN     *P12:15,"SELECT REASON":
                   *P12:17,"1)NO BILLING NEEDED":
                   *P12:18,"2)WRITE OFF":
                   *P12:19,"3)Free Test":
                   *P12:20,"4)BROKER EXCHANGE NO SELECTS CHARGED  ":
                   *P12:21,"5)BROKER Rent->EXCHANGE NO SLCTS CHARGED":
                   *P20:22,REASON,"       OK?",str1;
         TYPE      REASON
         IF        NOT EQUAL
         DISPLAY   *P1:24,*EL,*B,"REASON MUST BE NUMERIC!!",*B;
         GOTO      REASON
         ENDIF
         move      c0 to n1
         move      reason to n1
         branch    n1 to chkok,chkok,chkok,chklm,chklm
         goto      reason

chklm    clear     str2
         move      c0 to n2
         pack      str2 from osales10,osales
         move      str2 to n2
.Start patch #2.73
.         compare   c6 to n2
           if        (n2 <> "06" and n2 <> "27" and n2 <> "28")
.         if       not  equal
.End patch #2.73 
         DISPLAY   *P1:24,*EL,*B,"Not a List Management order!!",*B,*w4,*b:
                   *p1:24,*el;
         goto      reason
         endif
chkok    CMATCH    yes TO str1
         GOTO      chk2 IF EQUAL
         GOTO      REASON
chk2     branch    n1 of chk2a,update,chk2b,chk2a,chk2a
chk2a    
         RESET     EXCODES
         SCAN      OELCODE IN EXCODES            .exchange?
         if       not  equal
.         DISPLAY   *P1:24,*EL,*B,"Not an Exchange!!",*B,*w4,*b:
.                   *p1:24,*el;
         keyin    *P1:24,*EL,*HON,*B,"NOT",*HOFF," AN EXCHANGE ORDER ":
                   "LIKE TO CONTIUE ANYWAY",npaskey,*eon;
         CLEAR     NPASFLD
         MOVE      "I",NPASFLD
         APPEND    NPASKEY,NPASFLD
         RESET     NPASFLD
         CALL      NPASKEY
         GOTO      ORDOK IF NOT OVER
         DISPLAY   *B,*P17:11,"Password NOT VALID!",*b,*w
         goto      keyin
         endif
.
ordok
.Start patch #2.7 - vars increased
.          move       c0 to n7               *rental/exch split? 
.          move       oexqty to n7           *   Is This
.          compare    c0 to n7               *        One ?
          move       c0 to n9               *rental/exch split? 
          move       oexqty to n9           *   Is This
          compare    c0 to n9               *        One ?
.End patch #2.7 - vars increased
          goto      update if equal        * No.
         DISPLAY   *P1:24,*EL,*B,"Rent/Exchange Split!!",*B,*w4,*b:
                   *p1:24,*el;
         goto      keyin
         
chk2b    
.Start patch #2.7 - vars increased
.         move      c0 to n7
.         move      oppm to n7
.         compare   c0 to n7
.START PATCH 2.72 REPLACED LOGIC
.         move      c0 to n9
.         move      oppm to n9
.         compare   c0 to n9
          compare   C0,NSEL2PRICE
.END PATCH 2.72 REPLACED LOGIC
.End patch #2.7 - vars increased
         if       not  equal
         DISPLAY   *P1:24,*EL,*B,"Price is not ZERO!!",*B,*w4,*b:
                   *p1:24,*el;
         goto      keyin
         endif
UPDATE   MOVE      "B" TO OSTAT
         CALL      NORDUPD
         FILEPI    1;NOBILLED
.START PATCH 2.72 REPLACED LOGIC
.         WRITE     NOBILLED,NORDFLD;NORDFLD,REASON,MM,DD,YY,OQTY,OPPM,PASSNAME
          unpack    NSEL2PRICE,str2,str3,str1,str2
          pack      str5,str3,str2
          WRITE     NOBILLED,NORDFLD;NORDFLD,REASON,MM,DD,YY,OQTY,str5,PASSNAME
.END PATCH 2.72 REPLACED LOGIC
.
         RESET     EXCODES
         SCAN      OELCODE IN EXCODES
         goto      keyin if not equal
.         IF        EQUAL
         cmatch    "R" to ndattdmc         *Do We do splits?
          if         equal                  *Yes
.Start patch #2.7 - vars increased
.          move       c0 to n7               *Is
.          move       oexqty to n7           *   This
.          compare    c0 to n7               *        One ?
          move       c0 to n9               *Is
          move       oexqty to n9           *   This
          compare    c0 to n9               *        One ?
.End patch #2.7 - vars increased
          goto      keyin if equal        * No.
          endif
          cmatch    yes to ndat3exh
          goto      runnkey if equal
          cmatch    b1 to ndattdmc
          goto      keyin if eos
          goto      keyin if equal
RUNNKEY
         move      yes to str1
         cmatch    yes to ndat3exh
         if        not equal
         KEYIN     *P1:24,"CREATE RUNNING CHARGE ENTRY NOW? ",*T254,*uc,STR1;
         else
         KEYIN     *P1:24,"RUNNING CHRGE/Exch Fee ENTRY NOW? ",*T254,*uc,STR1;
         endif
         GOTO      RUNNKEY IF EOS
         CMATCH    YES TO STR1
         IF        EQUAL
         move      c0 to squant 
         MOVE      nordfld TO NSHPFLD
         CALL      NSHPKEY                           .get shipping data 
         CLEAR      COMMENT
.Start patch #2.7 - vars increased to reflect OQTY expansion
.         MOVE       C0 TO STR8
.         move       squant to n8
.         compare    c0 to n8
.          if         not equal
.          move       squant to str8   
.          else
.          move       oqty to str8
.          endif
.         PACK       COMMENT FROM NordFLD,B1,STR8
         MOVE       C0 TO STR9
         MOVE       C0 TO N9
         move       squant to n9
         compare    c0 to n9
          if         not equal
          move       squant to str9   
          else
          move       oqty to str9
          endif
         PACK       COMMENT FROM NordFLD,B1,STR9
.end patch #2.7 - vars increased to reflect OQTY expansion         
         RESET      COMMENT
         CHAIN      "NINV0008"
         ENDIF
. ......
         GOTO      KEYIN
.
NOGOOD   DISPLAY   *P12:10,*B,"PASSWORD INVALID!!!!!",*W2;
         STOP
BILLED   DISPLAY   *P1:23,*EL,*B,"THIS LR IS ALL READY BILLED!!",*W2;
         GOTO      KEYIN
CANCEL   DISPLAY   *P1:23,*EL,*B,"THIS LR IS CANCELLED!!",*W2;
         GOTO      KEYIN
.Start patch #2.71
PENDING  DISPLAY   *P1:23,*EL,*B,"THIS LR IS IN PENDING MODE!!",*W2;
         GOTO      KEYIN
LCR      DISPLAY   *P1:23,*EL,*B,"THIS LR IS IN CLEARANCE MODE!!",*W2;
         GOTO      KEYIN
.End patch #2.71
NOLR     DISPLAY   *P1:23,*EL,*B,"NO ORDER FOUND FOR THAT LR",*W2:
                   *P1:23,*EL;
         GOTO      KEYIN
STOP
         CLOSE     NOBILLED
         STOP
         INCLUDE   NPASIO.INC
         INCLUDE   NORDIO.INC
         INCLUDE   NSHPio.INC
         include   ndat3io.inc
.START PATCH 2.72 ADDED LOGIC
          INCLUDE   NSEL2IO.INC
.END PATCH 2.72 ADDED LOGIC
         INCLUDE   COMLOGIC.INC
