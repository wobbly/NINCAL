PC      EQU     0

        include COMMON.INC
        include CONS.INC
.========================================================================================
         MOVE      "NFTP0001" TO PROGRAM
         MOVE      "FTP DOWNLOAD PROGRAM" TO STITLE
         MOVE      "Names in the News" TO COMPNME
         CALL      PAINT
RELEASE   INIT      "1.51"                                                       ;09May2006 ADDED Source code FILES TO DOWNLOAD
.RELEASE   INIT      "1.5"                                                        ;11JAN2006 Added Code for New Donnelley Site         
.RELEASE   INIT      "1.4"                                                       ;13JAN2004 ADDED EMAIL
.RELEASE   INIT      "1.3"                                                       ;29DEC2003 ADDED MS MERGE TO FILES TO DOWNLOAD
.RELEASE   INIT      "1.2"                                                       ;16JUL2003 Added code to check timestamp and different file sizes
.RELEASE   INIT      "1.1"                        .Added .NOT extension
.RELEASE   INIT      "1.0"                        .Program will compare FTPSAV Records with temp file for new download of records from FTP

FTPTMP   FILE
FTPSAV   FILE
FTPDWN   FILE
.patch1.2
SVTMRED   DIM    12
.SAVRED   DIM    20
SAVRED   DIM    50
.patch1.2
FILESTR  DIM    100
.patch1.2
FILENM    DIM       50
FILETM    DIM       12
.patch1.2

.str20    DIM    20
.Form Pointer set to grab file name.must change if file structure changes
.POINT    FORM   "56"
fstrlen  form  3
..        call Paint
          clear n2
          Display    *P10:11,*EF,*White,"Opening Files!!"
          OPEN FTPTMP,"FTPTMP"
..        OPEN FTPSAV,"FTPTEST",EXCLUSIVE
          OPEN FTPSAV,"FTPSAV",EXCLUSIVE
          PREPARE FTPDWN,"\\nins1\E\DATA\FTPDWN"

        Display    *P10:11,*EF,*White,"Reading Save file for Files already downloaded!!"
TRYAGAIN
          READ FTPTMP,SEQ;FILESTR
          GOTO OVERNOW IF OVER
..                CALL TRIM USING FILESTR
..Exceptions for the root directories
          scan ".zip" in FILESTR
          goto TRYAGAIN if equal
          scan ".lol" in FILESTR
          goto TRYAGAIN if equal
          scan ".lst" in FILESTR
          goto TRYAGAIN if equal
          SCAN ".txt" in FILESTR
          goto TRYAGAIN if equal
.1.1
          SCAN ".touch" in FILESTR
          goto TRYAGAIN if equal
          SCAN ".xml" in FILESTR
          goto TRYAGAIN if equal
.1.1
.>Patch 1.5 Code Commented Out
.         scan "mp" in FILESTR
.         goto READSAV if equal
.>Patch 1.5 End Code Commented Out      
.>Patch 1.5 Code Added        
          scan "BrokerCounts" in FILESTR
          goto READSAV if equal
.>Patch 1.5 End Code Added    
          scan "ninc" in FILESTR
          goto READSAV if equal
          SCAN "nins" in FILESTR
          goto READSAV if equal
          SCAN "dnld" in FILESTR
          goto READSAV if equal
          scan "NINC" in FILESTR
          goto READSAV if equal
.1.1
          SCAN ".not" in FILESTR
.               goto READSAV if equal
          goto READSAV if equal 
.patch1.2
.PATCH1.3
          SCAN "ms" in FILESTR
          goto READSAV if equal
.PATCH1.3
.PATCH1.51
          SCAN "Mailed" in FILESTR
          goto READSAV if equal
.PATCH1.51
.;        reset     filestr
.;        bump filestr,42
.;        unpack filestr,FILETM,b1,FILENM
.                    movefptr filestr,fstrlen 
.standard file name size for prefix
.                    sub  "14" from fstrlen
.                    reset filestr to fstrlen
.         goto READSAV 
.patch1.2
.         endif
.1.1
          goto TRYAGAIN
READSAV
.patch1.2
          reset     filestr
          bump      filestr,42
.Patch 1.5
.Added this code for donnelley unix server.  listing varies from old triplex server
          move filestr to str1
          if (str1 = b1)
                    bump filestr
          endif
.Patch 1.5 
          unpack    filestr,FILETM,b1,FILENM                   ;time,filename
.patch1.2
          reposit FTPSAV,c0
          LOOP
                    READ FTPSAV,SEQ;SAVRED,SVTMRED
          UNTIL OVER
                    call trim using savred
.patch1.2
                    scan SAVRED in FILENM
.                scan SAVRED in FILESTR
.patch1.2
test
                    if equal
test1
                              if (SVTMRED <> "            ")
                                        match SVTMRED,FILETM
                                        goto TRYAGAIN if equal
                              else
                                        goto TRYAGAIN
                              endif
                    endif
.                goto TRYAGAIN if equal
.patch1.2
          
.patch1.2 
..               CALL TRIM USING SAVRED
..               If (str20 = SAVRED)
..               ENDIF
          REPEAT

Writer
          add c1 to n2
          Display    *P30:11,*EF,*White,"Files to be downloaded   ",n2
          Pause   C2
..       reset  FILESTR to POINT
.patch1.2
.        move   FILESTR to STR20
.        WRITE  FTPDWN,SEQ;STR20
          WRITE  FTPDWN,SEQ;FILENM,FILETM
.patch1.4
          add       c1 to n3
          Append    FileNm,Mailbody
          append    CRLF,Mailbody
.patch1.4
.patch1.2
          GOTO   TRYAGAIN
OVERNOW
          Display    *P10:11,*EF,*White,"Job Complete!!"

          If (n2 = c0)
                    Display    *P10:11,*EF,*White,"No Records to Download!!"
                    Close FTPDWN
                    pack  taskname,NTWKPATH1,"FTPDWN.DAT"
                    erase TASKNAME
.patch1.4
          else
                    move    "These Triplex Files Are Being Downloaded.",MailSubjct
                    Pack      MailFrom from "ComputerRequest","@nincal.com"
                    Pack      MailTo from "ComputerRequest","@nincal.com"
                    reset     Mailbody
                    Call      SendMail
.patch1.4
          endif
   Close   FTPTMP
   pack    taskname,NTWKPATH1,"FTPTMP.DAT"
   erase   taskname

   Pause   C5
   STOP
   include comlogic.inc
