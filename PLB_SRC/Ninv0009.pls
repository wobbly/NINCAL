.begin patch 1.4
.PC       EQU       1
PC       EQU       0
.end patch 1.4
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
         INCLUDE   NTYPDD.inc
.begin patch 1.5
.begin patch 1.4
.         include   ninvdd.inc
               include        ninvdd.inc
.end patch 1.4
.end patch 1.5.
.2013 October 07 - no longer needed - see ninv0001
Release   Init        "1.71"          DLH change to Ntypdd
Reldate   INit      "22 March 2010"
.Release   Init        "1.7"          DLH Use invoice date
.Reldate   INit      "19 June 2008"
.Release       Init            "1.6"          DLH 21Feb2006  new typist file format
.RELEASE        INIT           "1.5"         DLH  08March2005  NININV conversion
.RELEASE  INIT      "1.4"         DLH  05May99  NININV Y2K
.RELEASE  INIT      "1.3"         DLH  03AUG93  TYPIST TO 3 BYTES.
.RELEASE  INIT      "1.2"          DLH 28APR92  NTYPXX INCLUDES
.
.RELEASE  INIT     "1.1"          DLH 25MAR92 COMMON, CONS, COMLOGIC, ETC

.
.
. ..............
. OTHER VARIABLES.
.begin patch 1.6
NOrdTypRec     REcord         (33)  
.TypeREc       Dim             3         typist
TypeREc       Dim             11         typist plus invoice date
Countrec      Form            4
RepRec        Form            4
              RecordEnd
.end patch 1.6
ICount   FORM      4        CALC NEW INVOICES
IRepCnt   FORM      4        CALC REPRINT INVOICES
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
.INVREPR  FORM      4
.LCRRCNT  FORM      4
.LRCOUNT  FORM      4         *LCR COUNT.
.LSCOUNT  FORM      4         *LCR COUNT.
IOERROR  FORM      1          *INDEX FOR IO ERROR BRANCH.
TYPST    DIM       3
InvSum        form            5
InvRsum       Form            5
Lrhold        Dim             6
Input         File
..
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
          Count     N2,Typst
          if        (n2 = c0)
          move      b3,Ntyptype
          elseif    (N2 = c1)
          pack      NtypType from typst,b2
          elseif    (N2 = c2)
          pack      NtypType from typst,b1
          else
         MOVE      TYPST TO NtypTYPe
          endif
.DH 2009 June 19   for typist reporting combine the arb & amb
          if        (Ntyptype = "ARB")
          move      "AMB",NtypType
          endif
.end DH 2009 June 19
.
PASSONE
.
ADD
ADDREP
              FOR           Branch,"1","33"
          Pack      Str11 from NtypType,invdtec,invdtey,invdtem,invdted                                            
.                              If             (Type = NordTypRec(branch).TypeRec)                            
                              If             (STR11 = NordTypRec(branch).TypeRec)                            
                                             Move           NordTypREc(Branch).Countrec,ICount
                                             Move           NordTypREc(Branch).Reprec,IRepCnt
                                             if             (statb = "R")
                                             add            c1 to IRepCnt
                                             else
                                             add            c1 to ICount
                                             endif
                                             Move           ICount,NordTypREc(Branch).Countrec
                                             Move           IRepCnt,NordTypREc(Branch).Reprec
                             Break                
                             endif
                             if              (Nordtyprec(Branch).Typerec = "")
                                             if             (statb = "R")
                                             add            c1 to IRepCnt
                                             else
                                             add            c1 to ICount
                                             endif
                                             Move           ICount,NordTypREc(Branch).Countrec
                                             Move           IRepCnt,NordTypREc(Branch).Reprec
.                                             Move           Type,NordTypREc(Branch).Typerec
                                             Move           str11,NordTypREc(Branch).Typerec
                             break                
                             endif                
           repeat
         MOVE      C0 TO ICount
         MOVE      C0 TO IRepCnt
         GOTO      READ
SUBTOT
.
         MOVE      C0 TO ICount
         MOVE      C0 TO IRepCnt
         RETURN
.
OUTPUT
         clock     timestamp,str8
         unpack    str8,str2,yy,mm,dd
         pack      TypDate from str8
         rep       Zfill in typDate
.         SUB       INVRTOT FROM COUNT   *SUBTRACT REPRINTS FROM TOTAL.
OUTPUT1
.      UNLOAD TABLES.
              FOR           Branch,"1","33"
                                            
.                                             Move           NordTypRec(branch).TypeRec,Type                              
                                             Move           NordTypRec(branch).TypeRec,STR11
                                             Move           NordTypREc(Branch).Countrec,ICount
                                             Move           NordTypREc(Branch).Reprec,IRepCnt
                             if              (Nordtyprec(Branch).Typerec = "")
                             break  
                             goto            outlast
                             else
.
          Unpack    str11,NtypType,typDate
.
        MATCH     "  " TO NtypTYPe
        CALL      NOTYP IF EQUAL
        CALL      NOTYP IF EOS
              Move            c0 to n3
              move            Ntyptype to n3
              if              (n3 = "99")
              goto            outofhere
              endif
        MOVE      NtypType TO TYPST
        MOVE      C1 TO NTYPPATH
              Move            Str8 to TypDate
              Packkey         Ntypfld from TypDate,Ntyptype         
              add             ICount to Invsum
              add             IRepCnt to InvRsum
         CALL      NTYPtst
         GOTO      WRITE IF OVER
         CALL      NTYPKEY
         ADD       ICount TO INVCOUNT
         ADD       IRepCnt TO INVRCNT
         CALL      NTYPUPD
         GOTO      OUTPUTX
WRITE
              Move            Str8 to TypDate
              packkey              Ntypfld from Typdate,NtypType
              MOVE       ICount TO INVcount
              MOVE       IRepCnt TO INVRCnt
              CALL       NTYPWRT
. OUTPUTX - OUTPUT SECTION EXIT.
OUTPUTX
                              endif                
OutOfHere
              repeat
OUTLAST       
.             MOVE            C2 TO NTYPPATH
              pack            TypDate from str8
              packkey         Ntypfld from Typdate,"99 "
.              Move            Ntypfld to Idnum
              Move            "99 " to Ntyptype
              CALL            NTYPtst
              GOTO            NONINE IF OVER
              pack            TypDate from str8
              packkey         Ntypfld from Typdate,"99 "
              Move            "99 " to Ntyptype
.              Move            Ntypfld to Idnum
              call            NtypKey
.              add             invsum to INvTot
.              add             InvRsum to InvrTot
              add             invsum to INvCount
              add             InvRsum to InvrCnt
              CALL            NTYPUPD
              GOTO            STOP
NONINE
.........................................................................................................................
          Move      "NINV0009 - output",MailSubjct
          Move      "DavidHerrick@nincal.com",MailFrom
          Move      "DavidHerrick@nincal.com",MailTo
          Clear     MailBody
          Append    "Invoice Typist",MailBody
          Append    CRLF,MailBOdy
          Append    "No 99 record found !   key =     ",Mailbody
          Append    NTypFld,Mailbody
          Append    CRLF,MailBOdy
          Reset     Mailbody  
.              move          "NINV0009 - output" to SmtpSubject Subject
.              append         "Invoice Typist",SmtpTextMessage(1)   Array <Text message >
.              reset          smtpTextMessage(1)
.
.              append         "No 99 record found !   key =     ",SmtpTextMessage(2)   Array <Text message >
.              append         ntypfld,SmtpTextMessage(21)   Array <Text message >
.              reset          smtpTextMessage(2)
.

.              Move       "2",SmtpTextIndexLast                               Index to last entry in TextMessage array
.              move       "DHerric" to str45
.              move       "David Herrick" to str55
.              call       Mailmesg
          Call      SendMail              
              pack            TypDate from str8
              packkey              Ntypfld from Typdate,"99 "
              Move            "99 " to Ntyptype
.              Move            Ntypfld to Idnum
.              Move            invsum to INvTOt
.              Move             InvRsum to InvrTot
              Move            invsum to INvCount
              Move             InvRsum to InvrCnt
              CALL            NTYPWRT            
              GOTO            STOP
NOTYP
.             MOVE            "??" TO NTYPFLD
              packkey              Ntypfld from Typdate,"???"
              move            "???" to Ntyptype
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
