.begin patch 1.4
.PC       EQU       1
PC       EQU       0
.end patch 1.4
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
         INCLUDE   NTYPDD.inc
;begin patch 1.5
.begin patch 1.4
;         include   ninvdd.inc
               include        ninvdd.inc
.end patch 1.4
;end patch 1.5.
Release       Init            "1.6"          DLH 21Feb2006  new typist file format
;RELEASE        INIT           "1.5"         DLH  08March2005  NININV conversion
;RELEASE  INIT      "1.4"         DLH  05May99  NININV Y2K
.RELEASE  INIT      "1.3"         DLH  03AUG93  TYPIST TO 3 BYTES.
.RELEASE  INIT      "1.2"          DLH 28APR92  NTYPXX INCLUDES
.
.RELEASE  INIT     "1.1"          DLH 25MAR92 COMMON, CONS, COMLOGIC, ETC

.
.
. ..............
. OTHER VARIABLES.
;begin patch 1.6
NOrdTypRec     REcord         (33)  
TypeREc       Dim             3
Countrec      Form            4
RepRec        Form            4
              RecordEnd
;end patch 1.6
NCOUNT   FORM      4        CALC NEW INVOICES
REPCNT   FORM      4        CALC REPRINT INVOICES
COUNT    FORM      5         TOTAL INVOICES
INVRGRD  FORM      5
BRANCH   FORM      "00"
R        INIT      "R"
QTY      FORM      9
END      FORM      2
HUNDRED  FORM      "100"
ANS      DIM       1
TOTREP   FORM      3
REPCALC  FORM      5.3
TOTCALC  FORM      5.3
LINES    FORM      2
ENDSW    DIM       1
;INVREPR  FORM      4
LCRRCNT  FORM      4
LRCOUNT  FORM      4         *LCR COUNT.
LSCOUNT  FORM      4         *LCR COUNT.
IOERROR  FORM      1          *INDEX FOR IO ERROR BRANCH.
TYPST    DIM       3
InvSum        form            5
InvRsum       Form            5
Lrhold        Dim             6
Input         File
;.
. PROGRAM MAIN.
         MOVE      "NINV0009" TO PROGRAM
         MOVE      "NINCAL" TO COMPNME
         MOVE      "INVOICE TYPIST" TO STITLE
         CALL      PAINT
.
         TRAP      IO IF IO
         MOVE      C1 TO IOERROR
         
         clock     timestamp,str6
         unpack    str6,str2,yy,mm
         pack      TypDate from str6
         rep       Zfill in typDate

               Move           "NINVPRT1" to PINVNAME
              OPEN            Input,"NINVPRT.own"
.
. INCLUDE INV READ.
READ
               READ           INPut,SEQ;invvars:
                              TYPST
         GOTO      OUTPUT IF OVER
         match     lrn to lrhold
         GOTO      READ IF EQUAL        *YES
         move      lrn to lrhold
         ADD       C1 TO COUNT
         DISPLAY   *P48:12,*EL,COUNT;
         GOTO      BREAK
BREAK 
         MOVE      TYPST TO TYPe
.
PASSONE
.
ADD
ADDREP
              FOR           Branch,"1","33"
                                            
                              If             (Type = NordTypRec(branch).TypeRec)                            
                                             Move           NordTypREc(Branch).Countrec,Ncount
                                             Move           NordTypREc(Branch).Reprec,RepCnt
                                             if             (statb = "R")
                                             add            c1 to repcnt
                                             else
                                             add            c1 to Ncount
                                             endif
                                             Move           NCount,NordTypREc(Branch).Countrec
                                             Move           RepCnt,NordTypREc(Branch).Reprec
                             Break                
                             endif
                             if              (Nordtyprec(Branch).Typerec = "")
                                             if             (statb = "R")
                                             add            c1 to repcnt
                                             else
                                             add            c1 to Ncount
                                             endif
                                             Move           NCount,NordTypREc(Branch).Countrec
                                             Move           RepCnt,NordTypREc(Branch).Reprec
                                             Move           Type,NordTypREc(Branch).Typerec
                             break                
                             endif                
           repeat
         MOVE      C0 TO NCOUNT
         MOVE      C0 TO REPCNT
         GOTO      READ
SUBTOT
.
         MOVE      C0 TO NCOUNT
         MOVE      C0 TO REPCNT
         RETURN
.
OUTPUT
         clock     timestamp,str8
         unpack    str8,str2,yy,mm,dd
         pack      TypDate from str8
         rep       Zfill in typDate
;         SUB       INVRTOT FROM COUNT   *SUBTRACT REPRINTS FROM TOTAL.
OUTPUT1
.      UNLOAD TABLES.
              FOR           Branch,"1","33"
                                            
                                             Move           NordTypRec(branch).TypeRec,Type                              
                                             Move           NordTypREc(Branch).Countrec,Ncount
                                             Move           NordTypREc(Branch).Reprec,RepCnt
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
        MOVE      Type TO TYPST
        MOVE      C1 TO NTYPPATH
              Move            Str8 to TypDate
              Packkey         Ntypfld from TypDate,type         
              add             Ncount to Invsum
              add             Repcnt to InvRsum
         CALL      NTYPtst
         GOTO      WRITE IF OVER
         CALL      NTYPKEY
         ADD       NCOUNT TO INVCOUNT
         ADD       REPCNT TO INVRCNT
         CALL      NTYPUPD
         GOTO      OUTPUTX
WRITE
              Move            Str8 to TypDate
              packkey              Ntypfld from Typdate,Type
              MOVE       NCOUNT TO INVcount
              MOVE       REPCNT TO INVRCnt
              CALL       NTYPWRT
. OUTPUTX - OUTPUT SECTION EXIT.
OUTPUTX
                              endif                
OutOfHere
              repeat
OUTLAST       
;             MOVE            C2 TO NTYPPATH
              pack            TypDate from str8
              packkey         Ntypfld from Typdate,"99 "
;              Move            Ntypfld to Idnum
              Move            "99 " to type
              CALL            NTYPtst
              GOTO            NONINE IF OVER
              pack            TypDate from str8
              packkey         Ntypfld from Typdate,"99 "
              Move            "99 " to type
;              Move            Ntypfld to Idnum
              call            NtypKey
;              add             invsum to INvTot
;              add             InvRsum to InvrTot
              add             invsum to INvCount
              add             InvRsum to InvrCnt
              CALL            NTYPUPD
              GOTO            STOP
NONINE
              pack            TypDate from str8
              packkey              Ntypfld from Typdate,"99 "
              Move            "99 " to type
;              Move            Ntypfld to Idnum
;              Move            invsum to INvTOt
;              Move             InvRsum to InvrTot
              Move            invsum to INvCount
              Move             InvRsum to InvrCnt
              CALL            NTYPWRT
              GOTO            STOP
NOTYP
;             MOVE            "??" TO NTYPFLD
              packkey              Ntypfld from Typdate,"???"
              move            "???" to type
              RETURN
.
.
STOP     DISPLAY   *P1:24,*EL,*B,"JOB DONE, SHUTTING DOWN TO CONTINUE JOB":
                   *W2,*B;
         STOP
. IO - I/O ERROR TRAPS
IO
         TRAPCLR   IO
         NORETURN
IOBRANCH
         BRANCH    IOERROR OF FILE1,FILE2
         DISPLAY   *P1:24,*EL,"UNKNOWN RUN TIME IO ERROR",*B;
         GOTO      IOEXIT
FILE1
         DISPLAY   *P1:24,"INVOICE PRINT FILE ERROR",*B,*W2;
         GOTO      IOEXIT
FILE2
         DISPLAY   *P1:24,"TYPOUT FILE ERROR",*B,*W2;
         GOTO      IOEXIT
IOEXIT
         KEYIN     *P50:24,ANS;
         CMATCH    "Q" TO ANS
         GOTO      IOBRANCH IF NOT EQUAL
         STOP
               Include        ninvio.inc

         INCLUDE    NTYPIO.inc
         INCLUDE   COMLOGIC.inc

