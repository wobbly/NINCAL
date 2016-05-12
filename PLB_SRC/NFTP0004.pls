PC      EQU     0

        include COMMON.INC
        include CONS.INC
.========================================================================================
         MOVE      "NFTP0004" TO PROGRAM
         MOVE      "FINCA FTP PROGRAM" TO STITLE
.         MOVE      "TNC FTP PROGRAM" TO STITLE
         MOVE      "Names in the News" TO COMPNME
         CALL      PAINT
RELEASE   INIT      "1.2"                        DLH CHanged from TNC to FINCA
RelDate   Init      "19 March 2012"
.RELEASE   INIT      "1.1"                        13JAN2003 Added code to email files 
.RELEASE   INIT      "1.0"                        .Program will compare FTPSAV Records for TNC with temp file for new download of records from FTP

FTPTMP   FILE
FTPSAV   FILE
FTPDWN   FILE
SAVRED   DIM    120
FILESTR  DIM    160
str120    DIM   120
.Form Pointer set to grab file name.must change if file structure changes
POINT    FORM   "56"
fstrlen  form  3
..        call Paint
        clear n2
        Display    *P10:11,*EF,*White,"Opening Files!!"
        OPEN FTPTMP,"NINFTPTMP"
.        OPEN FTPTMP,"FTPTMP"
        OPEN FTPSAV,"FTPSAV4",EXCLUSIVE
        PREPARE FTPDWN,"\\nins1\E\DATA\ninftpdwn"

        Display    *P10:11,*EF,*White,"Reading Save file for Files already downloaded!!"
TRYAGAIN
                READ FTPTMP,SEQ;FILESTR
                GOTO OVERNOW IF OVER
..                CALL TRIM USING FILESTR
..Exceptions for the root directories
          uppercase FILESTR,FILESTR
          scan ".ZIP" in FILESTR
          goto READSAV if equal
          scan ".CSV" in FILESTR
          goto READSAV if equal
          scan ".HTM" in FILESTR
          goto TRYAGAIN if equal
          Call ForeignFile

          goto TRYAGAIN
READSAV
          reset FILESTR
          bump      filestr,39
   reposit FTPSAV,c0
   LOOP
        READ FTPSAV,SEQ;SAVRED
   UNTIL OVER
        call trim using savred
        scan SAVRED in FILESTR
        goto TRYAGAIN if equal
   REPEAT
Writer
   add c1 to n2
   Display    *P10:13,*EF,*White,"Files to be downloaded   ",n2
   Pause  C2
   move   FILESTR to str120
   WRITE  FTPDWN,SEQ;str120
   add     c1 to n3
          append    Str120,Mailbody
          append    CRLF,Mailbody
   GOTO   TRYAGAIN
OVERNOW
   Display    *P10:11,*EF,*White,"Job Complete!!"
          If (n2 = c0)
                    Display    *P10:11,*EF,*White,"No Records to Download!!"
                  Close FTPDWN
                  pack  taskname,NTWKPATH1,"NINftpdwn.DAT"
                  erase TASKNAME
          else
.patch1.1
          move    "These Files Are Being Downloaded.",mailsubjct
          reset     Mailbody  
          Pack      MailFrom from "ComputerRequest","@nincal.com"
          Pack      MailTo from "ComputerRequest","@nincal.com"
          call      SendMail
.patch1.1
          endif
   Close   FTPTMP
   pack    taskname,NTWKPATH1,"NINFTPTMP.DAT"
   erase   taskname
   Pause   C5
   STOP
ForeignFile
          move    "There is an unrecognized file on the WEB01 FTP Site. Please Verify",mailsubjct
          append    FILESTR,Mailbody
          append    CRLF,MailBOdy
          reset     MailBOdy
          Pack      MailFrom from "InformationServices@nincal.com"
          Pack      MailTo from "InformationServices@nincal.com"
          call      SendMail
          return
   
          include comlogic.inc
