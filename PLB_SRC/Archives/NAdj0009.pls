PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
         INCLUDE   NTYPDD.inc
         INCLUDE   NJSTDD.inc    ADJUSTMENT DETAIL
Release   Init        "1.21"          DLH change to Ntypdd
Reldate   INit      "22 March 2010"
.Release   Init        "1.2"          DLH Use adjustment date
.Reldate   INit      "19 June 2008"
.ReleASE        INIT           "1.1"         DLH  07Dec2006  sendmail
.RELEASE        INIT           "1.0"         DLH  16Feb2006  NININV conversion
.PURPOSE -  GATHER DAILY adj TYPIST INFORMATION.
.           FOR REPORT GENERATED AT EOM AND ON REQUEST BY ORDTYPST/DBC.
. .............................................................................
. FILES.
. ......
. OTHER VARIABLES.
COUNT    FORM      5         TOTAL adjustments
BRANCH   FORM      "00"
QTY      FORM      9
HUNDRED  FORM      "100"
ANS      DIM       1
ENDSW    DIM       1
IOERROR  FORM      1          *INDEX FOR IO ERROR BRANCH.
TYPST    DIM       3
LRHOLD   DIM       6         *LIST RENTAL NUMBER HOLD.
TYPiST    DIM       3

Input         File
adjsum        form            5
NOrdTypRec     REcord         (33)  
.TypeREc       Dim             3         typist
TypeREc       Dim             11         typist plus adjustment date
Countrec      Form            4
RepRec        Form            4
              RecordEnd
adjustments   form            4
.
. PROGRAM MAIN.
         MOVE      "Daily Adjustment Typist" TO STITLE
         MATCH     "Nadj0009" TO PROGRAM        *CHAINED FROM DSINIT?
         IF        NOT EQUAL                    *NO
         MOVE      "Nadj0009 " TO PROGRAM
         MOVE      "ninpadj2" TO INPNAME
         MOVE      "Names in the News" TO COMPNME
         ENDIF
         CALL      PAINT
.
         clock     timestamp,str6
         unpack    str6,str2,yy,mm
         pack      TypDate from str6
         rep       Zfill in typDate

         TRAP      IO IF IO
         MOVE      C1 TO IOERROR
.
INPGET   TRAP      FILENG IF IO
         OPEN      TESTFILE,INPNAME
         TRAPCLR   IO
         DISPLAY   *P15:06,INPNAME
         GOTO      Start
FILENG   NORETURN
         KEYIN     *P01:24,*EL,"The Input file is not on-line. ":
                   *P15:06,INPNAME
         GOTO      INPGET
.
.
START    
              OPen            Input,Inpname
Read
          READ      Input,SEQ;jstvars,str3
          move      str3,Typist

              GOTO      OUTPUT IF OVER
              match     Jstlr to lrhold
              GOTO      READ IF EQUAL        *YES
              move      JstLr to lrhold
              ADD       C1 TO COUNT
              DISPLAY   *P48:12,*EL,COUNT;
          Count     N2,Typist
          if        (n2 = c0)
          move      b3,Ntyptype
          elseif    (N2 = c1)
          pack      NtypType from typist,b2
          elseif    (N2 = c2)
          pack      NtypType from typist,b1
          else
         MOVE      TYPST TO NtypTYPe
          endif

.                    MOVE      TYPiST TO TYPe
              FOR           Branch,"1","33"
                                            
          Pack      Str11 from NtypType,Jstdate
.                              If             (Type = NordTypRec(branch).TypeRec)                            
                              If             (STR11 = NordTypRec(branch).TypeRec)                            
                                             Move           NordTypREc(Branch).Countrec,adjcount
                                             add            c1 to adjcount
                                             Move           adjcount,NordTypREc(Branch).Countrec
                             Break                
                             endif
                             if              (Nordtyprec(Branch).Typerec = "")
                                             add            c1 to adjcount
                                             Move           adjcount,NordTypREc(Branch).Countrec
.                                             Move           Type,NordTypREc(Branch).Typerec
                                             Move           str11,NordTypREc(Branch).Typerec
                             break                
                             endif                
              repeat
              MOve             C0 TO adjcount
              clear            NtypType
              GOTO      READ
.
OUTPUT
              clock     timestamp,str8
              unpack    str8,str2,yy,mm,dd
              pack      TypDate from str8
              rep       Zfill in typDate
.      UNLOAD TABLES.
              FOR           Branch,"1","33"
                                            
.                                             Move           NordTypRec(branch).TypeRec,Type                              
                                             Move           NordTypRec(branch).TypeRec,STR11
                                             Move           NordTypREc(Branch).Countrec,Adjustments
                             if              (Nordtyprec(Branch).Typerec = "")
                             break  
                             goto            outlast
                             else
            Unpack  str11,ntypType,typDate
              rep       Zfill in typDate
              MATCH           "  " TO NtypTYPe
              CALL            NOTYP IF EQUAL
              CALL            NOTYP IF EOS
              Move            c0 to n3
              move            ntyptype to n3
              if              (n3 = "99")
              goto            outofhere
              endif
              add             adjustments to adjsum
              MOVE            C1 TO NTYPPATH
              Move            Str8 to TypDate
              Packkey         Ntypfld from TypDate,Ntyptype         
              CALL            NTYPtst
              GOTO            WRITE IF OVER
              call            Ntypkey
              add             adjustments to adjcount
              CALL            NTYPUPD
              GOTO            OUTPUTX
WRITE
              Move            Str8 to TypDate
              packkey              Ntypfld from Typdate,NtypType
              add             adjustments to adjcount
              CALL            NTYPWRT
.
              endif
outofhere              
              repeat
. OUTPUTX - OUTPUT SECTION EXIT.
OUTPUTX
OUTLAST       
              pack            TypDate from str8
              packkey         Ntypfld from Typdate,"99 "
              MOve            "99 " to Ntyptype
              CALL            NTYPtst
              GOTO            NONINE IF OVER
              call            Ntypkey
              add             Adjsum to adjCount
              CALL            NTYPUPD
              GOTO            STOP
NONINE 
.........................................................................................................................
.begin patch 1.2
          Clear     Mailbody
          append    "Adjustment Typist",Mailbody
          Append    CRLF,MailBody                                               
          Append    "No 99 record found ! Key =  ",Mailbody
          Append    NTypfld,MailBody                                            
          Append    CRLF,MailBody                                               
          Reset     MailBody
                    
.         pack      mailattach,"c:\work\pdf\",str55         
          pack      Mailto,"dherric@nincal.com"
          pack      Mailfrom,"creques@nincal.com"
          move      "NADJ0009 - output",Mailsubjct
          
          call      SendMail

.              move          "NADJ0009 - output" to SmtpSubject Subject
.              append         "Adjustment Typist",SmtpTextMessage(1)   Array <Text message >
.              reset          smtpTextMessage(1)
..
.              append         "No 99 record found !   key =     ",SmtpTextMessage(2)   Array <Text message >
.              append         ntypfld,SmtpTextMessage(21)   Array <Text message >
.              reset          smtpTextMessage(2)
..
.
.              Move       "2",SmtpTextIndexLast                               Index to last entry in TextMessage array
.              move       "DHerric" to str45
.              move       "David Herrick" to str55
.              call       Mailmesg
.end patch 1.1
                            pack            TypDate from str8
              packkey         Ntypfld from Typdate,"99 "
              MOve            "99 " to Ntyptype
              Move             Adjsum to adjCount
              CALL            NTYPWRT
              GOTO            STOP
NOTYP
              Move            "???" to NtypType
              packkey              Ntypfld from Typdate,NtypType
              RETURN
.
.
STOP          DISPLAY   *P1:24,*EL,*B,"JOB DONE, SHUTTING DOWN TO CONTINUE JOB":
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
         DISPLAY   *P1:24,"Adjustment PRINT FILE ERROR",*B,*W2;
         GOTO      IOEXIT
FILE2
         DISPLAY   *P1:24,"TYPOUT FILE ERROR",*B,*W2;
         GOTO      IOEXIT
IOEXIT
         KEYIN     *P50:24,ANS;
         CMATCH    "Q" TO ANS
         GOTO      IOBRANCH IF NOT EQUAL
         STOP

         INCLUDE    NTYPIO.inc
         INCLUDE   COMLOGIC.inc

