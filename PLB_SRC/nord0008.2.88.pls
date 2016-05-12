PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
         INCLUDE   NORDDD.inc
.begin patch 2.6
.START PATCH 2.8 REPLACED LOGIC
.         INCLUDE   CONTACT1.inc
         INCLUDE   NCNTDD.inc
.END PATCH 2.8 REPLACED LOGIC
         include   winapi.inc
.end patch 2.6
.START PATCH 2.872 REPLACED LOGIC - TEMPORARY PATCH
          include   COMPDD.INC
          include   CNTDD.INC
.END PATCH 2.872 REPLACED LOGIC - TEMPORARY PATCH
release   init      "2.88"    DLH
REldate   INit      "24 April 2008"
.release   init      "2.873"       18JUN2005 DMB File Manager IP change
.release   init      "2.872"       05NOV04 ASH Sample Conversion - Increased Mailer field to 6 bytes
.release   init      "2.871"       11Feb04 send SMTP message do Comp Request.
.RELEASE  INIT      "2.87"       14JAN2004 DMB Added code to only write to nprintng at 7pm run
.RELEASE  INIT      "2.86"       07JAN2004 DMB Added Code to Run thru DSINIT
.RELEASE  INIT      "2.85"       11SEP2003 ASH    SAMPLE FORMAT CONVERSION
.RELEASE  INIT      "2.84"       21JAN2002 DLH EXTENDED EMAIL ADDRESS
.RELEASE  INIT      "2.83"       22Feb2001 DLH change infile specs
.RELEASE  INIT      "2.82"       02OCT2000 ASH NEW SERVER ADDED
.RELEASE  INIT      "2.81"       01MAY2000 ASH REPLACED EMAIL LOGIC
.RELEASE  INIT      "2.8"       16MAR2000 ASH REPLACED CONTACT1.INC WITH NCNTDD.INC
.RELEASE  INIT      "2.7"       18FEB00 ASH SAMPLE DIRECTORY MOVED
.release  init      "2.6"       20Dec99 DLH add logic for sample verification
.release  init      "2.5"       12Nov99 ASH - Added logic for In-House List Management Orders
.release  init      "2.4"       23sep99 jd added time check for night pass
.release  init      "2.3"      18aug 99 jd added lcr pass for batch print
.release  init      "2.2"     05JAN99 ASN NINORD Y2K, File expansion
.Release  init      "2.1"     28Sep98 DLH added code to handle pending orders
.                            See norddd.inc patch 5
.release  init      "2.0"      DLH 25MAY94      TAKE ONLY APPROVED ORDERS
.RELEASE  INIT      "1.5"      DLH 18MAR92
.
.Start Patch #2.2 - increased file size
.INFILE   FILE      FIXED=582
.NINPRINT IFILE     KEYLEN=6,FIXED=582
.NPRINT   IFILE     KEYLEN=6,FIXED=582
.NPRINTNG FILE      FIXED=582
.begin patch 2.83 fix file lenght, change to isam - seq
.INFILE   FILE      FIXED=684
.INFILE   FILE      FIXED=696
INFILE   IFILE     Keylen=6,FIXED=696
.end patch 2.83
NINPRINT IFILE     KEYLEN=6,FIXED=696
NPRINT   IFILE     KEYLEN=6,FIXED=696
NPRINTlc IFILE     KEYLEN=6,FIXED=696
.START PATCH 2.5 - ADDED LOGIC
NPRINTlc2 IFILE     KEYLEN=6,FIXED=696
.END PATCH 2.5 - ADDED LOGIC
NPRINTNG FILE      FIXED=696
.end Patch #2.2 - increased file size
KEY      DIM       6
SPCL7    DIM       2
SPCL8    DIM       2
SPCL9    DIM       2
DESC0L1  DIM       47
DESC0L2  DIM       47
DESC991  DIM       47
DESC992  DIM       47
DESC981  DIM       47
DESC982  DIM       47
READCNT  FORM      4
WRITCNT  FORM      4
WRITCNT2  FORM      4
time     dim       15
.hh       form      2
min      dim       2
secs     dim       2
newlcr   dim       1
.begin patch 2.6
dim30    dim       30
.START PATCH 2.7 - REPLACED LOGIC
.dir      init      "\\NTS0\C\data\samples\"
.START PATCH 2.82 REPLACED LOGIC
.dir      init      "\\NTS0\D\data\samples\"
dir      init      "\\nins1\E\data\samples\"
.END PATCH 2.82 REPLACED LOGIC
.END PATCH 2.7 - REPLACED LOGIC
last     form      3
.hexeight           integer  4,"4294967295"
newsflag form      1                        1=order sample, 2=lcr sample, 3= ninprint delete failure, 4= lcr update failure
.end patch 2.6
.
         MOVE      "NORD0008" TO PROGRAM
         MOVE      "Names in the News" TO COMPNME
         MOVE      "ORDER PRINT PREP" TO STITLE
         clock     time to time
         CALL      PAINT
.begin patch 2.83
.         OPEN      INFILE,"\\nins1\e\data\NINPRINT.dat"
.>Patch 2.873 Begin
.         OPEN      INFILE,"NINPRINT.ISI|20.20.30.103:502"
         OPEN      INFILE,"NINPRINT.ISI|NINS1:502"
.end patch 2.83
.         OPEN      NINPRINT,"NINPRINT.ISI|20.20.30.103:502"
         OPEN      NINPRINT,"NINPRINT.ISI|NINS1:502"
.>Patch 2.873 End
.        IFNZ      PC
.         OPEN      NPRINT,"NPRINT/TEXT:PRINT",EXCLUSIVE
.        XIF
.        IFZ       PC
.Start Patch #2.2 - increased file size
.         PREPARE   NPRINT,"G:\DATA\NPRINT","G:\DATA\NPRINT","6","582"
.START PATCH 2.82 REPLACED LOGIC
.         PREPARE   NPRINT,"G:\DATA\NPRINT","G:\DATA\NPRINT","6","696"
.         PREPARE   NPRINTLC,"G:\DATA\NPRINT.LCR","G:\DATA\NPRINTLC","6","696"
..START PATCH 2.5 - ADDED LOGIC
.         PREPARE   NPRINTLC2,"G:\DATA\NPRINT2.LCR","G:\DATA\NPRINTLC2","6","696"
..END PATCH 2.5 - ADDED LOGIC
..end Patch #2.2 - increased file size
.         prepare   NPRINTNG,"G:\DATA\NPRINTNG.DAT"
.
         PACK      STR35,NTWKPATH1,"NPRINT"
         PACK      STR45,NTWKPATH1,"NPRINT"
         PREPARE   NPRINT,STR35,STR45,"6","696"
         PACK      STR35,NTWKPATH1,"NPRINT.LCR"
         PACK      STR45,NTWKPATH1,"NPRINTLC"
         PREPARE   NPRINTLC,STR35,STR45,"6","696"
.START PATCH 2.5 - ADDED LOGIC
         PACK      STR35,NTWKPATH1,"NPRINT2.LCR"
         PACK      STR45,NTWKPATH1,"NPRINTLC2"
         PREPARE   NPRINTLC2,STR35,STR45,"6","696"
.END PATCH 2.5 - ADDED LOGIC
.end Patch #2.2 - increased file size
.patch2.87
                              if (COMMENT = "PM")
                                        PACK      STR35,NTWKPATH1,"NPRINTNG.DAT"
                prepare   NPRINTNG,STR35
                              endif
.patch2.87
.END PATCH 2.82 REPLACED LOGIC
.        XIF
.
.READ     FILEPI    7;INFILE,NPRINT,NINPRINT
         unpack    time into hh,colon,min,colon,secs
.Patch2.86
.         if        (hh >"12")
                              if (COMMENT = "PM")
.patch2.86
                              move      yes to newlcr
         else
         move      no to newlcr
         match     no to newlcr
         if        equal
         move      no to str1
         keyin     *P1:12,*blinkon,*red,"Do you want to include New Lcrs Batch  OK???   ",*RV,*t30,STR1;
         rep       lowup in str1
         cmatch    yes to str1
         if        equal
         move      yes to newlcr
               endif
            endif
         endif

read     filepi    1;infile
         READ      INFILE,SEQ;ORDVARS:
                   SPCL7,DESC0L1,DESC0L2,SPCL8,DESC991,DESC992:
                   SPCL9,DESC981,DESC982
         GOTO      EOJ IF OVER
          if        (olrn = "770854")
          call      debug
          endif
         ADD       C1 TO READCNT
         DISPLAY   *P10:12,"NUMBER OF ORDERS READ : ",READCNT
.begin patch 2.1
         CMATCH    "p" TO OSTAT       Pending order ?
.START PATCH #2.5 - REPLACED LOGIC - NOT YET IMPLEMENTED
.         GOTO      read IF EQUAL     YES, Do not write,leave in file.
..................
        if equal
.Filter out List Management LCR's that are to be sent to the List Owner
.AND Make sure this is the evening run
                if (OHIST = "*" AND newlcr = "Y")
                        goto write2
                else
                        goto read
                endif
        endif
.END PATCH #2.5 - REPLACED LOGIC - NOT YET IMPLEMENTED
         CMATCH    "x" TO OSTAT       Cancelled Pending order ?
         GOTO      read IF EQUAL     YES, Do not write, leave in file.
.note cancodes also updated to skip cancelled pending orders.
.end patch 2.1
         reset     cancodes
.bandage DLH ASH 14DEC99   until cancodes bug resolved
         cmatch    "z" to ostat      .cancelled LCR skip
         goto      read if equal
.End bandage DLH ASH 14DEC99   until cancodes bug resolved
         scan      ostat in cancodes
         goto      write if equal
         cmatch    "R" in ostat
         goto      write if equal
         CMATCH    "S" TO ORCODE
         GOTO      write IF EQUAL
         clear     str2
         pack      str2 from orcode,ostat
         match     "F0" to str2
         goto      writeng if equal
         CMATCH    "l" TO Ostat
         if        equal
                   cmatch    "Y" to newlcr
                   if equal
                             GOTO      write2
                   else
                             goto      read
                   endif
         else
                   goto      read
         endif
.PATCH2.87
writeng
                              if (COMMENT = "PM")
                filepi    1;nprintng
                WRITE     NPRINTng,seq;ORDVARS:
                   SPCL7,DESC0L1,DESC0L2,SPCL8,DESC991,DESC992:
                   SPCL9,DESC981,DESC982
                              endif
.PATCH2.87
                              goto      read
WRITE
.begin patch 2.6
.dave goes bad (mad?)
         type      osamcde
         if        equal
           match     "000" to  osamcde
           if        not equal
           move      "S" to str1
.START PATCH 2.85 REPLACED LOGIC
.           move      ".dcx" to str4
           move      ".TIF" to str4
.END PATCH 2.85 REPLACED LOGIC
.START PATCH 2.872 REPLACED LOGIC - TEMPORARY PATCH
.           pack      str55 from Dir,str1,omlrnum,osamcde,str4
          move      "COMPKEY3",Location
          pack      COMPFLD3,omlrnum
          pack      KeyLocation,"Key: ",COMPFLD3
          call      COMPKEY3
           pack      str55 from Dir,str1,COMPNUM,osamcde,str4
.END PATCH 2.872 REPLACED LOGIC - TEMPORARY PATCH
           pack      apifilename,str55,HexZero
           call      Findfirstfile

                if       (APIResult = 0 | APIResult = hexeight)
                move     c1 to newsflag          .order sample failure
                call     sendnews
                goto     read                .leave it in the print file and continue
                else                         .we found sample
                endif
           endif
         endif
         filepi    1;nprint
         WRITE     NPRINT,OLRN;ORDVARS:
                   SPCL7,DESC0L1,DESC0L2,SPCL8,DESC991,DESC992:
                   SPCL9,DESC981,DESC982
delete   MOVE      OLRN TO KEY
         REP       ZFILL IN KEY
         filepi    2;ninprint
         read      NINPRINT,KEY;;
         DELETE    NINPRINT,KEY
         if        over
         DISPLAY   *P1:24,*EL,*B,"I COULD NOT DELETE LR ## ",OLRN,B1,KEY:
                   "FROM THE PRINT FILE",*B
         move      c3 to newsflag
         call     sendnews
         ENDIF
         ADD       C1 TO WRITCNT
         DISPLAY   *P10:12,"NUMBER OF ORDERS WRITTEN : ",WRITCNT
         GOTO      READ
.
WRITE2
.START PATCH #2.5 - REPLACED LOGIC - NOT YET IMPLEMENTED
.         filepi    1;nprintlc
.         WRITE     NPRINTlc,OLRN;ORDVARS:
.                   SPCL7,DESC0L1,DESC0L2,SPCL8,DESC991,DESC992:
.                   SPCL9,DESC981,DESC982
................
.begin patch 2.6
.dave goes bad (mad?)
         type      osamcde
         if        equal
           match     "000" to  osamcde
           if        not equal
           move      "S" to str1
.START PATCH 2.85 REPLACED LOGIC
.           move      ".dcx" to str4
           move      ".TIF" to str4
.END PATCH 2.85 REPLACED LOGIC
.START PATCH 2.872 REPLACED LOGIC - TEMPORARY PATCH
.           pack      str55 from Dir,str1,omlrnum,osamcde,str4
          move      "COMPKEY3-2",Location
          pack      COMPFLD3,omlrnum
          pack      KeyLocation,"Key: ",COMPFLD3
          call      COMPKEY3
           pack      str55 from Dir,str1,COMPNUM,osamcde,str4
.END PATCH 2.872 REPLACED LOGIC - TEMPORARY PATCH
           pack      apifilename,str55,HexZero
           call      Findfirstfile

                if       (APIResult = 0 | APIResult = hexeight)
                move     c2 to newsflag          .lcr sample failure
                call     sendnews
                goto     read                .leave it in the print file and continue
                else                         .we found sample
                endif
           endif
        endif
         if (OCO2CODE <> "" AND OCO2CODE <> "  ")       .Valid Caller = In-House LCR/Pending
                 filepi    1;nprintlc2
                 WRITE     NPRINTlc2,OLRN;ORDVARS:
                           SPCL7,DESC0L1,DESC0L2,SPCL8,DESC991,DESC992:
                           SPCL9,DESC981,DESC982

         else
                 filepi    1;nprintlc
                 WRITE     NPRINTlc,OLRN;ORDVARS:
                           SPCL7,DESC0L1,DESC0L2,SPCL8,DESC991,DESC992:
                           SPCL9,DESC981,DESC982
         endif
.END PATCH #2.5 - REPLACED LOGIC - NOT YET IMPLEMENTED
delete2  MOVE      OLRN TO KEY
         REP       ZFILL IN KEY
.START PATCH #2.5 - REPLACED LOGIC - NOT YET IMPLEMENTED
.         filepi    2;ninprint
.         read      NINPRINT,KEY;;
.         DELETE    NINPRINT,KEY
.         if        over
.         DISPLAY   *P1:24,*EL,*B,"I COULD NOT DELETE LCR ## ",OLRN,B1,KEY:
.                   "FROM THE PRINT FILE",*B
.         ENDIF
..............
         if (OSTAT = "p")       .List Management In-House LCR
                filepi    1;ninprint
                read      NINPRINT,KEY;;
                if over
                          DISPLAY   *P1:24,*EL,*B,"I COULD NOT UPDATE LCR ## ",OLRN,B1,KEY:
                                    "IN THE PRINT FILE",*B
                move     c4 to newsflag
                call     sendnews
                else
                          filepi    1;ninprint
                          updatab NINPRINT;*176," "
                endif
         else                   .Brokerage LCR
                filepi    2;ninprint
                read      NINPRINT,KEY;;
                DELETE    NINPRINT,KEY
                if        over
                DISPLAY   *P1:24,*EL,*B,"I COULD NOT DELETE LCR ## ",OLRN,B1,KEY:
                          "FROM THE PRINT FILE",*B
                ENDIF
         endif
.END PATCH #2.5 - REPLACED LOGIC - NOT YET IMPLEMENTED
         ADD       C1 TO WRITCNT2
         DISPLAY   *P10:12,"NUMBER OF LCR's WRITTEN : ",WRITCNT2
         GOTO      READ
EOJ
         IFNZ      PC
         FLUSH     NPRINT
         XIF
                              if (COMMENT = "PM")
                   weof      nprintng,seq
                   CLOSE     NPRINTng,EOFSIZE
                              endif
         CLOSE     NPRINT,EOFSIZE
         CLOSE     NPRINTlc,EOFSIZE
         CLOSE     NINPRINT
.begin patch 2.83
         close     infile
.end patch 2.83
.patch2.86
         shutdown  "cls"
.patch2.86
                              STOP
............................................................................
.begin 2.6 more bad dave
sendnews
.need to get contact and send an email
.START PATCH 2.8 REPLACED LOGIC
.        MOVE      ococode,n3
..
.         LOAD      str35 FROM N3 OF ocnt1,ocnt2,ocnt3,ocnt4,ocnt5,ocnt6,ocnt7:
.                   ocnt8,ocnt9,ocnt10,ocnt11,ocnt12,ocnt13,ocnt14,ocnt15,ocnt16,ocnt17
.
.         SCAN      "(" IN str35
.         clear      str25
.         MOVEFPTR  str35 TO LAST
.         sub       c1 from last
.         SETLPTR   str35 TO LAST
.         RESET     STR35
.         APPEND    str35 TO str25
.         endif
.         reset      str25
.         clear       str1
.         append      str25 to str1         .1st init
.         reset        str1
.         scan       b1 in str25
.         bump       str25 by 1
.         clear      str6
.         append     str25 to str6
.         reset      str6
.         call       rtrim using str6
.         pack       str7 from str1,str6
.         RESET      STR25
...............
        pack    NCNTFLD,OCOCODE
        move    "sendnews-NCNTKEY",Location
        pack    KeyLocation,"Key: ",NCNTFLD
        call    NCNTKEY
.START PATCH 2.84 REPLACED LOGIC
.        clear   str1
.        append  CNTNAME to str1         .1st init
.        reset   str1
.        scan    b1 in CNTNAME
.        bump    CNTNAME by 1
..START PATCH 2.81 REPLACED LOGIC
..        clear   str6
..        append  CNTNAME to str6
..        reset   str6
..        call    rtrim using str6
.        move    CNTNAME to str7
.        call    RemoveChar using str7,B1
.        move    str7,str6
..END PATCH 2.81 REPLACED LOGIC
.        pack    str7 from str1,str6
.        RESET   CNTNAME
..END PATCH 2.8 REPLACED LOGIC
          move      CNTNAME,str35
          call      RemoveChar using CNTNAME,B1
.END PATCH 2.84 REPLACED LOGIC
       if      (newsflag = c1)
          Move    "This is a Error e-mail from Orders",MailSubjct
          Clear     MailBOdy
          append    "This is an error message",MailBody
          append    CRLF,MailBody
          append    "record## ",MailBody
          append    olrn,MailBody
          append    b1,MailBody
          append    "Sample ## ",MailBody
          append    osamcde,MailBody
          append    CRLF,Mailbody
          Append              "This is from Outgoing Orders's. Your above LR had",Mailbody
          append    CRLF,Mailbody
          Append              "a Sample number. However I found no sample!!!! ",Mailbody
          append    CRLF,Mailbody
          Append              "The order has not been sent. Please review & correct.",Mailbody
          append    CRLF,Mailbody
          REset     MailBody
       Elseif      (newsflag = c2)
          Move    "This is a Error e-mail from LCR's",MailSubjct
          Clear     MailBOdy
          append    "This is an error message",MailBody
          append    CRLF,MailBody
          append    "record## ",MailBody
          append    olrn,MailBody
          append    b1,MailBody
          append    "Sample ## ",MailBody
          append    osamcde,MailBody
          append    CRLF,Mailbody
          Append              "This is from Outgoing LCR's. Your above LCR had",Mailbody
          append    CRLF,Mailbody
          Append              "a Sample number. However I found no sample!!!! ",Mailbody
          append    CRLF,Mailbody
          Append              "The order has not been sent. Please review & correct.",Mailbody
          append    CRLF,Mailbody
          REset     MailBody

       Elseif      (newsflag = c3)
          Move    "This is a Error e-mail from Orders",MailSubjct
          Clear     MailBOdy
          append    "This is an error message",MailBody
          append    CRLF,MailBody
          append    "record## ",MailBody
          append    olrn,MailBody
          append    b1,MailBody
          append    CRLF,Mailbody
          Append              "This is from Outgoing ORders. This LR",Mailbody
          append    CRLF,Mailbody
          Append              "Could not be deleted from the master print file!!!! ",Mailbody
          append    CRLF,Mailbody
          Append              "IS, Please review & correct.",Mailbody
          append    CRLF,Mailbody
          REset     MailBody

       Elseif      (newsflag = c4)
          Move    "This is a Error e-mail from LCR's",MailSubjct
          Clear     MailBOdy
          append    "This is an error message",MailBody
          append    CRLF,MailBody
          append    "record## ",MailBody
          append    olrn,MailBody
          append    b1,MailBody
          append    CRLF,Mailbody
          Append              "This is from Outgoing LCR's. This LCR could",Mailbody
          append    CRLF,Mailbody
          Append              "not be updated during the nightly run.  ",Mailbody
          append    CRLF,Mailbody
          Append              "Please review & correct.",Mailbody
          append    CRLF,Mailbody
          REset     MailBody

       endif
          pack      MailFrom from CntName,"@nincal.com"


        if      (newsflag = 1 or newsflag = 2)
          PAck      MailTo from "ComputerRequest@nincal.com,",MailFrom

        Elseif      (newsflag = 3 or newsflag = 4)
          PAck      MailTo from "ComputerRequest@nincal.com,"
        endif
        Call    SendMail

        winshow
        return
.end patch 2.6
.START PATCH 2.8 ADDED LOGIC
         INCLUDE   NCNTIO.inc
.END PATCH 2.8 ADDED LOGIC
.START PATCH 2.872 REPLACED LOGIC - TEMPORARY PATCH
          include   COMPIO.INC
          include   CNTIO.INC
.END PATCH 2.872 REPLACED LOGIC - TEMPORARY PATCH
         INCLUDE   COMLOGIC.inc
