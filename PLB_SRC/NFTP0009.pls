PC      EQU     0

        include COMMON.INC
        include CONS.INC
.========================================================================================
         MOVE      "NFTP0009" TO PROGRAM
         MOVE      "InfoGroup FTP PROGRAM" TO STITLE
         MOVE      "Names in the News" TO COMPNME
         CALL      PAINT
RELEASE   INIT      "1.0"     DLH

FTPTMP   FILE
FTPSAV   FILE
FTPDWN   FILE
SAVRED   DIM    120
FILESTR  DIM    160
str120    DIM   120
.Form Pointer set to grab file name.must change if file structure changes
POINT    FORM   "56"
fstrlen  form  3
;.        call Paint
        clear n2
        Display    *P10:11,*EF,*White,"Opening Files!!"
        OPEN FTPTMP,"INfOFTPTMP"
        OPEN FTPSAV,"FTPSAV9",EXCLUSIVE
        PREPARE FTPDWN,"\\nins1\E\DATA\INFOftpdwn"
          Clear     Mailbody

        Display    *P10:11,*EF,*White,"Reading Save file for Files already downloaded!!"
TRYAGAIN
                READ FTPTMP,SEQ;FILESTR
                GOTO OVERNOW IF OVER
;.                CALL TRIM USING FILESTR
;.Exceptions for the root directories
.          uppercase FILESTR,FILESTR
.add exceptions 12Feb2013
          scan ".xml" in FILESTR
          goto Tryagain if equal
          reset filestr
          scan ".touch" in FILESTR
          goto Tryagain if equal
          reset filestr


.end add
          scan ".SHP" in FILESTR
          goto READSAV if equal
          reset filestr
          scan ".NOT" in FILESTR
          goto READSAV if equal
          reset filestr
          scan ".csv" in FILESTR
          goto READSAV1 if equal
          reset filestr
          scan ".dat" in FILESTR
          goto READSAV1 if equal
          reset filestr
          scan "CLIENT ACCESS TRANSFER FILES",filestr
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
         goto       Writer      
READSAV1
          reset FILESTR
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
          Append    str120,MailBody
          append    CRLF,MailBody
   GOTO   TRYAGAIN
OVERNOW
   Display    *P10:11,*EF,*White,"Job Complete!!"
          If (n2 = c0)
                    Display    *P10:11,*EF,*White,"No Records to Download!!"
                  Close FTPDWN
                  pack  taskname,NTWKPATH1,"InfoFtpdwn.DAT"
                  erase TASKNAME
          Move      "No Infogroup Files to Download.",MailSubjct
          Move      "ComputerRequest@nincal.com",MailFrom
          Move      "Dherric@nincal.com",MailTo
          reset     Mailbody
          call      SendMail
          else
          reset     Mailbody
          Move      "These Files Are Being Downloaded.",MailSubjct
          Move      "ComputerRequest@nincal.com",MailFrom
          Move      "Dherric@nincal.com",MailTo
          call      SendMail
          endif

   Close   FTPTMP
   pack    taskname,NTWKPATH1,"INFOFtpTMP.DAT"
   erase   taskname
   Pause   C5
   STOP
ForeignFile
          Move      "There is an unrecognized file on the Infogroup FTP Site. Please Verify",MailSubjct
          Move      "ComputerRequest@nincal.com",MailFrom
          Move      "Dherric@nincal.com",MailTo
          move      FILESTR,MailBody
          call      SendMail
          return
   
          include comlogic.inc
