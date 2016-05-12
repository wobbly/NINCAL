PC            EQU             0
              INC             COMMON.inc
              INCLUDE         CONS.inc
              INC             NSHPDD.inc
              inc             hp.inc
              Include         compdd.inc
              include         CNTDD.INC
              INC             NORDDD.inc
              Include         DMEXDD.INC
              include         NCNTDD.inc
              Include         Nord7dd.inc
              include         winapi.inc
last          form      3

;
;              "APPLY DMEXCHANGE  INFO" TO STITLE
Release       Init            "1.02"     26JAN2006 DMB added code for data folder restructure
.Release       Init            "1.01"    DLH 12Oct2005  additional info added to email
;Release  init      "1.00"               DLH 20May2005
SAVE     FILE
LOGFILE  IFILE     KEYLEN=6,var=498
;
COUNT    FORM      4
UPDATE   FORM      4
WRITE    FORM      4
UPD      FORM      4
DIFF     FORM      10
TENPER   FORM      10
KEY      DIM       6
LINES    FORM      2
FIFTY4   FORM      "54"
PAGE     FORM      2
DATE     DIM       8
SALES    DIM       2
HOLDMKEY DIM       7
BRKFLAG  FORM      1            0=LIST MANAGEMENT
;
BCNAME   DIM       25
CAREOF   INIT      "C/O"
DATE1    INIT      "99/99/99"
scost    dim       5
Var      DIM       1
faxflag  dim       1
newflag  init      "N"
OKSTATS  INIT      "0"
Str5000       Dim             5000
str255        dim             255
dlFiles       datalist
ndx           form 9
dmFileName    dim 80

              OPEN            LOGFILE,"SHIPDMXIn"
              MOVE            "NDMX001C" TO PROGRAM
              MOVE            "APPLY DMEXCHANGE  INFO" TO STITLE
              MOVE            "Names In The News" TO COMPNME
              MOVE            C0 TO PAGE
              CLOCK           DATE TO DATE
              clock           timestamp to str18
              unpack          str18 to str2,str16
              move            str2 to cc
              call            GetWinVer
              CALL            PAINT
              MOVE            C1 TO NORDPATH
              PACK            STR35,NTWKPATH1,"DMEXSHP.LST"
              SPLOPEN         STR35
              CALL            HEADER
              create          dlFiles=1:10:1:10,visible=0
.Patch 1.02              
.              dlFiles.Dir giving result using *Filespec="\\nins1\e\data\dmexchange\*.dat",*flags=0
              dlFiles.Dir giving result using *Filespec="\\nins1\e\storage\dmexchange\*.dat",*flags=0
.Patch 1.02              
              if ( result = -1 )
               stop
.             alert           note,"no files matching filespec",result,"test"
              else
               for ndx from 0 to result
                              dlFiles.GetText               giving dmFileName using *Index=ndx
                              call           Dofiles
               repeat
              endif
              destroy         dlFiles
              goto eoJob
Dofiles
              Move            DmFilename to str3
              if (str3 = "nin" or str3 = "NIN")            .then don't touch it
               return
              endif
.Patch 1.02                 
.              pack            str255 from "\\nins1\e\data\dmexchange\",dmfilename
              pack            str255 from "\\nins1\e\storage\dmexchange\",dmfilename
.Patch 1.02                 
              open            dmexfile,str255
              Move            ".dat",str4
              move            ".prc",str5
              call            Replaceit using str255,str4,str5
              Prepare         Tempfile,str255,exclusive
              Display 	*p10:12,"Filename: ",str255
             
cleanfile
              read            dmexfile,seq;str5000
              goto eoj if over
              move            "#" ," to str5
              move            "#"," to str4
              call            Replaceit using str5000,str5,str4
              write           tempfile,seq;str5000
              goto            cleanfile
eoj
              weof            tempfile,seq
              close           tempfile
              close           dmexfile
.
              scan            "_rcp_" in dmfilename                      .receipt acknowledgement file ?  if so skip for now
              if equal
               reset          dmfilename
               goto REceipt
              else
               reset          dmfilename
              endif
;
              OPEN            DMEXFile,str255,exclusive
;
;
Shipping
              READ            DMEXFile,SEQ;*CDFON,DMVARS
              GOTO EOFile IF OVER
              call            trim using dmpo                 ;lr
              packkey         nordfld from dmpo
              rep             Zfill in nordfld
              call            nordtst
              goto Noord if over
              pack            slrnum from dmpo
              rep             zfill in slrnum
              call            trim using dmChrg
              move            dmchrg to spost
              Move            Dmqshp to Squant
              unpack          DmSdate into mm,str1,dd,str1,str2,yy
              pack            sdate from str2,yy,mm,dd
              TYPE            SQUANT
              GOTO BADQTY IF NOT EQUAL
              call            Trim using DMVia
              move            DMVia to Sinfo
              MOVE            "D" TO SCODE
              ADD             C1 TO COUNT
              DISPLAY         *P10:12,"RECORDS IN : ",COUNT
              CALL            NORDKEY
;             GOTO            NOORD IF OVER
              RESET           OKSTATS
              MATCH           OSTAT IN OKSTATS
              GOTO NOLIVE IF NOT EQUAL
              MOVE            C0 TO N10
              MOVE            OQTY TO N10
              DIV             C10 INTO N10
              MOVE            N10 TO TENPER
              MOVE            SQUANT TO N10
              MOVE            OQTY TO DIFF
              SUB             N10 FROM DIFF
              COMPARE         C0 TO DIFF
              CALL            NEG IF LESS
              COMPARE         DIFF TO TENPER
              CALL            VAR IF NOT GREATER
              move            str18,SRDATE
              move            "IS",SINITS
              MOVE            SLRNUM TO NSHPFLD
              CALL            NSHPTST
              GOTO DUPE IF NOT OVER
;
              CALL            NSHPWRT
              ADD             C1 TO WRITE
              DISPLAY         *P10:13,"RECORDS WRITTEN : ",WRITE
              CMATCH          YES TO Var
              IF EQUAL
               MOVE           NO TO Var
               GOTO WRITELOG
              ENDIF
              COMPARE         LINES TO FIFTY4
              CALL            HEADER IF LESS
              PRINT           *L,*10,"LR ## ",SLRNUM," RECORD ADDED"
              ADD             C2 TO LINES
              GOTO WRITELOG
;
DUPE
              call            nshpupd
              ADD             C1 TO UPD
              DISPLAY         *P10:14,"RECORDS UPDATED : ",UPD
              COMPARE         LINES TO FIFTY4
              CALL            HEADER IF LESS
              PRINT           *L,*10,"LR ## ",SLRNUM," RECORD UPDATED"
              ADD             C2 TO LINES
              goto writelog
VAR
              COMPARE         LINES TO FIFTY4
              CALL            HEADER IF LESS
              PRINT           *L,*10,"LR ## ",SLRNUM," VARIES > +/- 10%":
               "ORDER QTY ",OQTY," DMExchange QTY ",SQUANT
              ADD             C2 TO LINES
              move            yes to faxflag
              MOVE            YES TO V
              call            sendnews
              RETURN
NOORD
              COMPARE         LINES TO FIFTY4
              CALL            HEADER IF LESS
              PRINT           *L,*10,"LR ## ",SLRNUM," NO ORDER FOUND ",SCODE:
               " ",SQUANT," ",SPOST,*L,*10,SINFO
              ADD             C3 TO LINES
              GOTO Shipping
NOlive
              COMPARE         LINES TO FIFTY4
              CALL            HEADER IF LESS
              if (OSTAT = "B")
               PRINT          *L,*10,"LR ## ",SLRNUM," Already Billed! ",SCODE:
                              " ",SQUANT," ",SPOST,*L,*10,SINFO
               ADD            C3 TO LINES
              else
               PRINT          *L,*10,"LR ## ",SLRNUM," NOT A LIVE ORDER! ",SCODE:
                              " ",SQUANT," ",SPOST,*L,*10,SINFO
               ADD            C3 TO LINES
              endif
              GOTO Shipping
HEADER
              ADD             C1 TO PAGE
              compare         c1 to page
              if equal
              PRINT           *L,*L,*n,032,hpreset:
               hpttray:
               hpport:
               hpdupl:
               033,"&l66P":               page length
               033,"&l65F":
               033,"&l1E",033,"&a0c0R":
               *10,"DMExchange SHIPPING REPORT",*60,DATE:
               *L,*60,"PAGE: ",PAGE,*L
              else
               PRINT          *F,*L,*L,*L,*10,"DMExchange SHIPPING REPORT",*60,DATE:
                              *L,*60,"PAGE: ",PAGE,*L
              endif
              MOVE            C6 TO LINES
              RETURN
BADQTY
              COMPARE         LINES TO FIFTY4
              CALL            HEADER IF LESS
              PRINT           *L,*10,"LR ## ",SLRNUM," QUANTITY NOT NUMERIC"
              ADD             C2 TO LINES
              GOTO Shipping
;
NEG
              MULT            SEQ BY DIFF
              RETURN
;
WRITELOG
              PACK            MKEY FROM OMLRNUM,OCOBN
              MATCH           MKEY TO HOLDMKEY
              IF NOT EQUAL
               CALL           NMLRKEY
               MOVE           MKEY TO HOLDMKEY
              ENDIF
              MOVE            C0 TO N2
              PACK            SALES FROM OSALES10,OSALES
              MOVE            SALES TO N2
              COMPARE         C6 TO N2
              goto wrtalph2 if equal

              COMPARE         "19" TO N2
              goto wrtalph2 if equal
              GOTO Shipping
;
WRTALPH2
              FILEPI          3;LOGFILE
              READ            LOGFILE,NORDFLD;;
              GOTO Shipping IF NOT OVER
              WRITE           LOGFILE,NORDFLD;ORDVARS
              GOTO Shipping
;
EOFile
              CLOSE           DMEXFile
              Erase           str255
              cALL            getwinver
              If (osflag = c1 | osflag = c5)
.Patch 1.02                               
.               PACK           TASKNAME,"c:\winnt\system32\cmd.exe /c Move /Y \\nins1\e\data\dmexchange\",dmfilename," \\nins1\e\data\dmexchange\applied\ "         ;"
               PACK           TASKNAME,"c:\winnt\system32\cmd.exe /c Move /Y \\nins1\e\storage\dmexchange\",dmfilename," \\nins1\e\storage\dmexchange\applied\ "         ;"
.Patch 1.02                                
               Execute        TASKNAME                                                                                                                               ."
              ElseIf (osflag = c3 | osflag = c4)                         ." "
.Patch 1.02                                              
.               PACK           TASKNAME,"c:\command.com /c Move /Y \\nins1\e\data\dmexchange\",dmfilename," \\nins1\e\data\dmexchange\applied\ "
               PACK           TASKNAME,"c:\command.com /c Move /Y \\nins1\e\storage\dmexchange\",dmfilename," \\nins1\e\storage\dmexchange\applied\ "
.Patch 1.02                                               
               EXECUTE        TASKNAME                                                                                                                ."
              Elseif (osflag = c6)
.Patch 1.02                                              
.               PACK           TASKNAME,"c:\windows\system32\cmd.exe /c Move /Y \\nins1\e\data\dmexchange\",dmfilename," \\nins1\e\data\dmexchange\applied\ "
               PACK           TASKNAME,"c:\windows\system32\cmd.exe /c Move /Y \\nins1\e\storage\dmexchange\",dmfilename," \\nins1\e\storage\dmexchange\applied\ "
.Patch 1.02                                               
               Execute        TASKNAME
              endif
              Return
;             "
Receipt
              OPEN            DMEXFile,str255,exclusive
RecLoop
              READ            DMEXFile,SEQ;*CDFON,DMVARS
              GOTO EOFile IF OVER
              call            trim using dmpo                 ;lr
              packkey         nordfld from dmpo
              rep             Zfill in nordfld
              call            nordtst
              goto Noord1 if over
              pack            slrnum from dmpo
              rep             zfill in slrnum
              call            trim using dmChrg
              move            dmchrg to Nord7Chrg
              Move            Dmqshp to Nord7qshp
              unpack          DmSdate into mm,str1,dd,str1,str2,yy,str9
              pack            Nord7sdate from str2,yy,mm,dd,str9
              call            Trim using DMVia
              move            DMVia to Nord7Via
              pack            Nord7Lname from DMLname
              pack            Nord7MName from DMMname
              packkey         Nord7fld from nordfld
              packkey         Nord7Lr from nordfld
              move            DMAirb to Nord7Airb
              unpack          DmTdate into mm,str1,dd,str1,str2,yy,str9
              pack            Nord7Tdate from str2,yy,mm,dd,str9
              Move            DMMedia to Nord7Media
              move            DMSer to Nord7Ser
              Move            DMSel to Nord7Sel
              unpack          DmMDate into mm,str1,dd,str1,str2,yy,str9
              pack            Nord7MDate from str2,yy,mm,dd,str9
              Move            DMLr to Nord7PO
              move            dmoqty to Nord7oqty
              unpack          DmOdate into mm,str1,dd,str1,str2,yy,str9
              pack            Nord7Odate from str2,yy,mm,dd,str9
              Move            DMCNT to Nord7Cnt
              Move            DMPhone to Nord7Phone
              Move            DMEmail to Nord7Email

              ADD             C1 TO COUNT
              DISPLAY         *P10:12,"RECORDS IN : ",COUNT
              CALL            NORDKEY
;             GOTO            NOORD IF OVER
;
              CALL            Nord7WRT
;
NOORD1
              COMPARE         LINES TO FIFTY4
              CALL            HEADER IF LESS
              PRINT           *L,*10,"LR ## ",SLRNUM," NO ORDER FOUND ",SCODE:
               " ",SQUANT," ",SPOST,*L,*10,SINFO
              ADD             C3 TO LINES
              GOTO RecLoop
;
;
EOJob
              COMPARE         C0 TO PAGE
              CALL            HEADER IF EQUAL
              COMPARE         LINES TO FIFTY4
              CALL            HEADER IF LESS
              PRINT           *L,*10,"NUMBER OF RECORDS RECEIVED: ",COUNT
              PRINT           *FLUSH
              RELEASE
              SPLCLOSE
              STOP
;...........................
sendnews
              pack            NCNTFLD,OCOCODE
              move            "SVCREP-NCNTKEY",Location
              pack            KeyLocation,"Key: ",NCNTFLD
              clear           cntname
              clear           str35
              call            NCNTKEY
              clear           str1
              append          CNTNAME,str1         ..1st init
              reset           str1
              scan            B1,CNTNAME
              bump            CNTNAME,1
              move            CNTNAME,str7
              call            RemoveChar using str7,B1
              move            str7,str6
              call            trim using str6
              pack            str7 from str1,str6
              RESET           CNTNAME
              move            cntname to str35
              Move            "This is a Informational e-mail from the DMExchange shipping program",SmtpSubject Subject
;   Set the text message that is send with the attachments
              Move            "This is a Informational e-mail from the DMExchange shipping program",SmtpTextMessage(1)   Array <Text message >
              clear           str25
              append          "record## " to str25
              append          olrn to str25
              append          b1 to str25
              reset           str25
              Move            str25,SmtpTextMessage(2)   Array <Text message >
              Move            " Your above LR had a shipping qty variance:",SmtpTextMessage(3)   Array <Text message >
              clear           str55
              append          "Order qty " to str55
              append          oqty to str55
              append          ", Shipped qty ",str55
              append          squant,str55
              reset           str55
              Move            str55,SmtpTextMessage(4)   Array <Text message >
              Move            "Please review & correct as necessary.",SmtpTextMessage(5)   Array <Text message >
;begin patch 1.01
              MOve            DmCnt,SmtpTextMessage(6)   Array <Text message >
              MOve            DmPhone,SmtpTextMessage(7)   Array <Text message >
              MOve            DmEmail,SmtpTextMessage(8)   Array <Text message >
;              Move            "5",SmtpTextIndexLast                               Index to last entry in TextMessage array
              Move            "8",SmtpTextIndexLast                               Index to last entry in TextMessage array
;end patch 1.01
              clear           taskname
              Move            "NTS4",SmtpEmailServer                   Address of email serverc
              clear           smtpemailaddress
              append          str7 to SmtpEmailAddress
              append          "@nincal.com",SmtpEmailAddress
              reset           smtpemailaddress
              Move            str7,SmtpUserName                                User name
              Move            STR35,SmtpUserFullName              User Full Name
;   Set the destinations of the email. Max 100 (Mime spec)
              MOVE            smtpemailaddress to SmtpDestinations(1,1)           .send a copy to the originator
              MOVE            user,SmtpDestinations(1,2)                          originators UserName
              Move            "1",SmtpDestIndexLast                               Index to last entry in Dest array
              Move            "0",SmtpAttIndexLast                                Index to last entry - Only 1 entry
              Clear           SmtpLogFile                                         'Clear' disables the LogFile
              Call            SmtpSend   ( 'Send' is in Smtp.Pri which is included in TestSmtp.Dbs )
              winshow
              return

              Include         COMPIO.inc
              include         CNTIO.INC
              INCLUDE         NSHPIO.inc
              INCLUDE         NORDIO.inc
              include         NCNTIO.inc
              INclude         Nord7io.inc
              INCLUDE         COMLOGIC.inc