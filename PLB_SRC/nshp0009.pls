PC      EQU     0
        Include COMMON.INC
        Include CONS.INC
          Include nftp2dd.inc
          Include nftplogdd.inc                 
.========================================================================================
         MOVE      "NSHP0009" TO PROGRAM
         MOVE      "Shipping Check Program" TO STITLE
         MOVE      "Names in the News" TO COMPNME
         CALL      PAINT


RELEASE   INIT      "1.51"       .DLH removed Reuben added Robb
Reldate   Init      "2015 May 18"
.RELEASE   INIT      "1.5"       .DLH removed Jack added Reuben
.Reldate   Init      "15 September 2011"
.RELEASE   INIT      "1.4"       .Code Added this code has been added to check the ftp log using the new company ftp secondary file
.RELEASE   INIT      "1.3"      .Code Added this code has been added to check the ftp log using the new company ftp secondary file
.RELEASE   INIT      "1.2"      .Turned off code to check for nincmmdd.shp as triplex now gives us two shipping file a day.
.RELEASE   INIT      "1.1"      .Added code to check for nincmmdd"a".shp as triplex now gives us two shipping file a day.
.RELEASE   INIT      "1.0"      .Program will check for shipping the last three days
.FTPSAVIFILE        IFILE     KEYLEN=12,FIXED=20
FTPSAVIFILE         IFILE     KEYLEN=13,FIXED=20
DWNFLD    DIM       13
.DWNFLD   DIM       12
.STR20    DIM       20
CURJUL    FORM      5

.Patch 1.3 New Vars used for scanning matching of file names.  can be reused if needed.
Taskname1  DIM 100


          clear n2
          Display    *P10:10,*EF,*White,"Checking Save File for missing shipping information!!"

.Gather What Date to check for
          clock     WEEKDAY,str1
          goto      overnow   if        (str1="7")          ;skip saturday
          goto      overnow   if        (str1="1")          ;skip sunday

          if (str1 = "2")   ;monday -check three days earlier
                    move      c2 to n2
          endif
          clock     timestamp,str8
          move      str8,timestamp
          unpack    str8,str2,yy,mm,dd  ;yyyymmdd
.           MOVe       "20150630",str8
          call      zfillit   using MM
        call        zfillit   using DD
        call        zfillit   using YY
        call        cvtjul
          move      juldays to curjul
          move      c1 to n2
          for       n9,"1","3"
                    if        (str1 = "2")         ;Monday
                              if        (n9=c1)
                                        sub c3 from curjul
                              else
                                        sub       c1 from curjul
                              endif
                    elseif    (str1 = "3")         ;Tuesday
                              if        (n9=c2)
                                        sub c3 from curjul
                              else
                                        sub       c1 from curjul
                              endif
                    elseif    (str1 = "4")         ;Wednesday
                              if        (n9=c3)
                                        sub c3 from curjul
                              else
                                        sub       c1 from curjul
                              endif
                    else
                              sub       c1 from curjul
                    endif

                    move      curjul to JULDAYS
                    call      CVTGREG



.Patch 1.3 Code Added this code has been added to check the ftp log using the new company ftp secondary file                      
                    unpack  timestamp,str2
;                    PACKKEY   taskname1,"NINCA",str2,yy,mm,dd,".shp"
                    PACKKEY   taskname1,"NINCA",str2,yy,mm,dd,"AM.shp"
                    Pack NFTPLOGFLD2,"02X",taskname1,b55,b55
                    call trim using taskname1
                    CALL NFTPLOGAIM
                    If over                       
.                              move      n9 to str9
.                              call      trim using str9                         
.                              Move      "CReques@nincal.com",Mailfrom
.                              Move      "InformationServices@nincal.com",MailTo
.                              Pack      MAILSubjct,"This Shipping File ",DWNFLD," is ",str9," day(s) overdue."                              
.                              if        (N9 = c3)
.                                        Clear     MailBody
.                                        Append    "This is the Last Day For This Warning",MailBody                      
.                                        append    CRLF,mailbody
.                                        append    taskname1,Mailbody
.                                        append    CRLF,mailbody
.                                        reset     Mailbody
.                                        Call      SendMail
.                                        else                                    
.                                        Clear     MailBody
.                                        append    taskname1,Mailbody
.                                        append    CRLF,mailbody
.                                        reset     Mailbody
.                                        Call      SendMail
.                              Endif
                    Endif
.Patch 1.4 Code Added this code has been added to check the ftp log using the new company ftp secondary file                      

                    PACKKEY   taskname1,"NINCA",str2,yy,mm,dd,"PM.shp"
                    Pack NFTPLOGFLD2,"02X",taskname1,b55,b55
                    call trim using taskname1
                    CALL NFTPLOGAIM
                    If over                       
                              move      n9 to str9
                              call      trim using str9                         
                              Move      "CReques@nincal.com",Mailfrom
.                              Move      "InformationServices@nincal.com",MailTo
.                              Move      "Jamiemittone@nincal.com",MailTo
.                              Move      "JackForder@nincal.com,GemmaSpranza@nincal.com",MailTo
.                              Move      "ReubenHolland@nincal.com,GemmaSpranza@nincal.com",MailTo
                              Move      "RobbWhiting@nincal.com,GemmaSpranza@nincal.com",MailTo
                              Pack      MAILSubjct,"This Shipping File ",DWNFLD," is ",str9," day(s) overdue."                              
                              if        (N9 = c3)
                                        Clear     MailBody
                                        Append    "This is the Last Day For This Warning",MailBody                      
                                        append    CRLF,mailbody
                                        append    taskname1,Mailbody
                                        append    CRLF,mailbody
                                        reset     Mailbody
                                        Call      SendMail
                                        else                                    
                                        Clear     MailBody
                                        append    taskname1,Mailbody
                                        append    CRLF,mailbody
                                        reset     Mailbody
                                        Call      SendMail
                              Endif
                    Endif
.Patch 1.4 Code Added this code has been added to check the ftp log using the new company ftp secondary file                      
          repeat
OverNow   
                    Stop
          Include nftp2io.inc
          Include nftplogio.inc         
        include comlogic.inc
