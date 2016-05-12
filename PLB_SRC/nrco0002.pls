.
PC       EQU       0
         INC       COMMON.inc
         include   cons.inc
         include   nowndd.inc
.Patch1.9
                              include   compdd.inc
                              include   cntdd.inc
.         include   nMLRdd.inc
.Patch1.9
         INCLUDE   NUSEDD.INC
         INCLUDE   NDATDD.INC
         include   winapi.inc
         include   ncntdd.inc
.fax test
          Include   Hp.inc
.fax test

Release  INIT      "2.11"          DLH New Fax gateway
Reldate   Init      "2015 April 14"
.Release  INIT      "2.10"          DLH Sunbelt PDF
.Reldate   Init      "2013 April 23"
.Release  INIT      "2.02"          DLH Add more detail to the email subject line and body
.Reldate   Init      "02 May 2012"
.Release  INIT      "2.01"          DLH change fax server from srv2008a to nins2
.Reldate   Init      "11 January 2012"
.Release   Init      "2.00"         DLH convert to pdf and add check for email
.reldate   Init      "10 Nov 2011"
.Release  INIT      "1.93"          24 May 2007 DLH PLI
.Release  INIT      "1.92"          09DEC2004 ASH FAXFILE
.Release  INIT      "1.91"          09AUG2004 ASH Logo Conversion
.Release  INIT      "1.9"          26MAY2004 DMB Mailer Conversion
.Release  INIT      "1.82"          15JAN2003 DMB Added Our list no. to Request for counts fax so our sls dept could id list.
.Release  INIT      "1.81"           10OCT2002 DMB Added contact name to be used instead of user name/also added perm opt to get name of person providing info.
.Release  INIT      "1.8"           16SEP2002 DMB Added code to include datacard/usage or both at users request
.Release  INIT      "1.7"          10SEP2002 DMB Added code to include datacard at users request
.Release  INIT      "1.6"          12Jul2002 DLh Use GetWinVer
.Release  INIT      "1.5"          06DEC2001 DMB Update replacing SCF Box with text if Omit box check by user
.Release  INIT      "1.0"          14MAY2001 ASH New release after interface written by David Baca
.tempfile file
PICT1   PICT
        CREATE      PICT1=3:13:30:50:
                "\\nins1\e\apps\PLB\CODE\2NDREQUEST7.BMP"
.patch1.7
.PICT2   PICT
.        CREATE     PICT2=1:13:30:200:
.                "\\nins1\e\apps\PLB\CODE\rco0001.BMP"
.patch1.7
RESULT2 FORM    9
RESULT3 FORM    9
.Files to open
prfile  pfile
.
DCARD     DIM   1               ;ask to include a datacard or usage or both
SecondReq dim   1
OMIT      dim   1
.hexeight integer 4,"4294967295"
.Define Fonts to be used
font1   font
font2   font
font3   font
font4   font
font5   font
font6   font
NRCOSLCT DIM       35
NRCOCMNT DIM       60
NRCOG01  DIM       10
NRCOG02  DIM       10
NRCOG03  DIM       10
NRCOG04  DIM       10
NRCOG05  DIM       10
NRCOG06  DIM       10
NRCOG07  DIM       10
NRCOG08  DIM       10
NRCOG09  DIM       10
NRCOG10  DIM       10
NRCOG11  DIM       10
NRCOG12  DIM       10
NRCOG13  DIM       10
NRCOG14  DIM       10
NRCOG15  DIM       10
LONGDIST DIM       1
.begin patch 2.0
.badfaxflag dim     1            =Y if Facsys printer not defined
Emailflag  dim     1            =Y Email/fax the pdf, N = print
faxnum     dim     10
FileCheck  FIle
trapcount  form      4
.end patch 2.0
.begin patch 1.6
.osflag   form   1          1=win 95,98, 2=NT
.end patch 1.6
listname  DIM       6
DROW      FORM      10
        create  font1,"Arial",size=12,bold
        create  font2,"Arial",size=7
        create  font3,"Helvetica",size=9
        create  font4,"Fixed",size=10
        create  font5,"Arial",size=11
        create  font6,"Arial",size=14
.begin patch 1.93
FontCourier                   font
FontO7              font
FontO18B  font

          create    fontCourier,"Courier New",size=10
          create    fontO7,"Times New Roman",size=7
          create    fontO18B,"Times New Roman",size=18,Bold
.end patch 1.93

+..............................................................................
.START PATCH 1.91 ADDED LOGIC
NINLogo   PICT
          CREATE    NINLogo=3:13:30:50:
                    "\\nins1\e\netutils\NIN logo black outline.jpg"
.END PATCH 1.91 ADDED LOGIC
         move      "NRCO0002" TO PROGRAM
         MOVE      "REQUEST FOR COUNTS - PRINT" TO STITLE
         move      "Names In Then News        " to compnme
         CLOCK     DATE TO TODAY
         MOVE      "EXIT" TO PF5
START
.begin patch 1.6
                    call                GetWinVer
.        getinfo  system,str6
.        unpack   str6 into str1,str2
.        unpack   str2 into str1
.        move     c0 to osflag
.        if (str1 = "3" or str1 = "4")
.                 move     c1 to osflag
.        endif
.        if (str1 = "1" or str1 = "5")
.         move     c2 to osflag
.        endif
.end patch 1.6
CashCreatePrint
        pack    str45,"\\nins1\d\users\",USER,"\",INPNAME,".dat"
        open    tempfile,str45,SHARE

.patch1.7
.        read    tempfile,seq;SecondReq,OMIT,OWNLONM,today,LONGDIST,OWNFAX,OWNOCPY,nuseuser,str8,MCOMP,OLSTNAME:
.                nrcoslct,nrcocmnt,nrcog01,nrcog02,nrcog03,nrcog04,nrcog05,nrcog06,nrcog07:
.                nrcog08,nrcog09,nrcog10,nrcog11,nrcog12,nrcog13,nrcog14,nrcog15
.patch1.71
.        read    tempfile,seq;SecondReq,OMIT,DCARD,OWNLONM,today,LONGDIST,OWNFAX,OWNOCPY,nuseuser,str8,MCOMP,OLSTNAME:
.                nrcoslct,nrcocmnt,nrcog01,nrcog02,nrcog03,nrcog04,nrcog05,nrcog06,nrcog07:
.                nrcog08,nrcog09,nrcog10,nrcog11,nrcog12,nrcog13,nrcog14,nrcog15
.patch1.82
.        read    tempfile,seq;SecondReq,OMIT,DCARD,OWNLONM,today,LONGDIST,OWNFAX,OWNOCPY,cntname,str8,MCOMP,OLSTNAME:
.                nrcoslct,nrcocmnt,nrcog01,nrcog02,nrcog03,nrcog04,nrcog05,nrcog06,nrcog07:
.                nrcog08,nrcog09,nrcog10,nrcog11,nrcog12,nrcog13,nrcog14,nrcog15
.begin patch 2.0
.        read    tempfile,seq;SecondReq,OMIT,DCARD,OWNLONM,today,LONGDIST,OWNFAX,OWNOCPY,cntname,str8,MCOMP,OLSTNAME:
.                listname,nrcoslct,nrcocmnt,nrcog01,nrcog02,nrcog03,nrcog04,nrcog05,nrcog06,nrcog07:
.                nrcog08,nrcog09,nrcog10,nrcog11,nrcog12,nrcog13,nrcog14,nrcog15
        read    tempfile,seq;SecondReq,OMIT,DCARD,OWNLONM,today,LONGDIST,OWNFAX,OWNOCPY,cntname,str8,MCOMP,OLSTNAME:
                listname,nrcoslct,nrcocmnt,nrcog01,nrcog02,nrcog03,nrcog04,nrcog05,nrcog06,nrcog07:
                nrcog08,nrcog09,nrcog10,nrcog11,nrcog12,nrcog13,nrcog14,nrcog15,OwnEmail
.end patch 2.0
.patch1.82
.subpatch1.71
.subpatch1.7
        close   tempfile
.Set up columns
        move    "800",column
        move    "1700",column1
.        move    "2000",column1
        move    "2500",column2
.        move    "3450",column3
        move    "4200",column4
.        move    "4950",column5
        move    "5900",column6
.        move    "6750",column7

OpenFile
.begin patch 2.0
.        move    "                                        ",APIFileName
.        clear   APIFileName
..START PATCH 1.92 REPLACED LOGIC
..        pack    APIFileName,NTWKPATH4,"fax\hdrfile.prn",hexzero
..        call    DeleteFile
..        pack    taskname,NTWKPATH4,"fax\hdrfile.prn"
.        pack    APIFileName,"C:\WORK\hdrfile.prn",hexzero
.        call    DeleteFile
.        pack    taskname,"C:\WORK\hdrfile.prn"
..END PATCH 1.92 REPLACED LOGIC
          Clear     EmailFlag
.        clear   badfaxflag
.FOR TESTING PURPOSES
..        move    YES,badfaxflag
.        trap    faxspool if spool
.        SPLOPEN taskname
..patch1.81
.        print   "^[D",longdist,ownfax,"^[N",ownocpy:
.                "^[T",today,b1,str5,"^[S",cntname,"^]"
..        print   "^[D",longdist,ownfax,"^[N",ownocpy:
..                "^[T",today,b1,str5,"^[S",NUSEUSER,"^]"
..patch1.81
.        SPLCLOSE
.        if (APIResult = 0 | APIResult = hexeight)
.        endif
..FOR TESTING PURPOSES
..        move    "4154337796",ownfax
                    call      Trim using OwnEmail
                    if        (OwnEmail <> "")               .we have somthing
                              scan      "@",OwnEmail
                              if        equal                         .presuming valid
                              reset     OwnEmail
                              move      OwnEmail,MailTo
                              move      Yes,Emailflag                    .send it
                              endif
                    Else
                    
                    move    Ownfax,faxnum
                    match   "0000000000",faxnum
                              if      equal                                       .no good zeroed out
                              move      No,EmailFlag
                    
                              else
                                        type    faxnum
                                        if not equal
                                        else
                                        count   N2,faxnum
                                        compare C10,N2
                                                  if equal
                                                  move    yes,EmailFlag      .Looks like we have number, FAX IT
                                                  move    C1,LONGDIST
                                                  unpack  faxnum,str3,str7
                                                  match   "510",str3
                                                            if equal
                                                            move    str7,faxnum
                                                            clear   LONGDIST
                                                            else
                                                            match   B3,str3
                                                                      if equal
                                                                            move    str7,faxnum
                                                                            clear   LONGDIST
                                                                      endif
                                                            endif
.                                                  pack      Mailto from"IMCEAFACSYS-",longdist,faxnum,"@nincal.com"
                                                  pack      Mailto from "+",longdist,faxnum,"@fax.nincal.com"
                                                  endif
                                        endif
                              endif
                    endif


        call    Trim using LONGDIST
.        if (badfaxflag = YES)
        if (Emailflag = No)
                if (PRTNAME = "1" | PRTNAME = "3")      .Laser2
                        PRTOPEN prfile,"\\NINs2\laser2","CountsReq"
                else                                    .Laser3 = Default
                        PRTOPEN prfile,"\\NINs2\laser3 Blankstock","CountsReq"
                endif
        else
..First check 995 autolaunch settings
.begin patch 2.10
.                    call      PDF995Auto
.                    call      SetPDFFlag
.                    PRTOPEN prfile,"PDF995","CountsReq"
                    PRTOPEN prfile,"PDF:","c:\work\pdf\CountsReq.pdf"
.end patch 2.10
.                PRTOPEN prfile,"FAXFILE","FAXFILE.PRN"
        endif
.end patch 2.0
        prtpage prfile;*UNITS=*HIENGLISH,*Font=FontCourier;
.
        if (SecondReq = YES)
                PRTPAGE PRFILE;*PICTRECT=*OFF,*PICT=2655:8575:column:7600:pict1
        endif
.
        move    "300",row
.START PATCH 1.91 REPLACED LOGIC
.        prtpage prfile;*p2700:row,*font=font10,"Names";
.        prtpage prfile;*font=font11,"  in the News";
.        add     eightlpi,row
.        add     eightlpi,row
.        add     eightlpi,row
.        prtpage prfile;*p1000:row,*pensize=10,*line=7100:row;
.        add     "60",row
.        prtpage prfile;*p2700:row,*font=font12,"C  A  L  I  F  O  R  N  I  A        I  N  C .";
.;Go ahead and print the last line now
.;Bullets produced using:  Alt+0149
.        prtpage prfile;*p1500:9950,*font=font13,"1300 Clay St., 11th Floor, Oakland, CA 94612-1429 • 415-989-3350 • Fax 415-433-7796";
...............................
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
        add     "60",row
          add     eightlpi,row
.begin patch 1.93
.          IF        (Company = 2)
.          prtpage   PrFile;*p=1:25,*font=fontO18b,"Pacific Lists, Inc.":
.                    *p=451:343,*font=fontO7,"1300 Clay St. 11th Floor":
.                    *p=451:443,"Oakland, CA 94612-1492":
.                    *p=317:543,"415-945-9450 ","·"," Fax 415-945-9451":
.                    *p=317:643,"A Division of Names in the News"
.          Else

          prtpage   prfile;*Pictrect=*off,*PICT=0:1300:500:8100:NINLogo,*OVERLAYON
.          endif
.         prtpage   prfile;*Pictrect=*off,*PICT=0:1300:500:8100:NINLogo
.end patch 1.93
.END PATCH 1.91 REPLACED LOGIC
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
        prtpage prfile;*p3000:row,*font=font5,*boldon,"REQUEST FOR COUNTS",*boldoff;
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
        prtpage prfile;*pcolumn:row,"Name:";
        prtpage prfile;*pcolumn1:row,OWNLONM;
        prtpage prfile;*p5000:row,"Date:";
        prtpage prfile;*pcolumn6:row,today;
        add     eightlpi,row
        add     eightlpi,row
        prtpage prfile;*pcolumn:row,"Fax #:";
        unpack  OWNFAX,str2,str1,str3,str4
        prtpage prfile;*pcolumn1:row,"(",str2,str1,") ",str3,"-",str4;
.;patch1.81
        prtpage prfile;*p3500:row,"Co: ";
        prtpage prfile;OWNOCPY;
.        add     eightlpi,row
.        add     eightlpi,row
.        prtpage prfile;*pcolumn:row,"Company:";
.        prtpage prfile;*pcolumn1:row,OWNOCPY;
.;subpatch1.81
        add     eightlpi,row
        add     eightlpi,row
        prtpage prfile;*pcolumn:row,"From:";
.patch1.81
        prtpage prfile;*pcolumn1:row,cntname;
.        prtpage prfile;*pcolumn1:row,nuseuser;
.patch1.81
        add     eightlpi,row
        add     eightlpi,row
        call    Trim using str8
        if (str8 = "")
                clear   str10
        else
                    type      str8
                    if equal
                  unpack  str8,MM,DD,str2,YY
                          pack    str10,MM,DASH,DD,DASH,str2,YY
                    else
                              move      str8,str10
                    endif
        endif
        prtpage prfile;*pcolumn:row,"PLEASE PROVIDE THE FOLLOWING COUNTS BY :  ",str10;
        move    row,howmany
        add     eightlpi,row
        add     eightlpi,row
        prtpage prfile;*pcolumn:row,"MAILER:";
        call    Trim using MCOMP
        prtpage prfile;*pcolumn1:row,MCOMP
        add     eightlpi,row
        add     eightlpi,row
.patch1.82
        prtpage prfile;*pcolumn:row,*font=font5,"LIST";
        pack str12,"(","##",listname,") "
        prtpage prfile;*font=font2,*ll,str12;
        prtpage prfile;*font=font5,":";
        call    Trim using OLSTNAME
        prtpage prfile;*pcolumn1:row,*font=font5,OLSTNAME
.patch1.82
.patch1.7
.Patch1.8
.        IF (DCARD = YES)
.                PRTPAGE PRFILE;*boldon," (Please attach Datacard)",*boldoff
.        ENDIF
.EndPatch1.8
.patch1.7
        add     eightlpi,row
        add     eightlpi,row
        prtpage prfile;*pcolumn:row,"SELECT:";
        call    Trim using nrcoslct
        prtpage prfile;*pcolumn1:row,nrcoslct;
        add     eightlpi,row
        add     eightlpi,row
.
        prtpage prfile;*pcolumn:row,"Notes:";
.
        call    Trim using nrcocmnt
        prtpage prfile;*pcolumn1:row,nrcocmnt;
        add     eightlpi,row
        add     eightlpi,row
.Patch1.8
        If (Dcard <>"N")
          if (DCARD = "B")                     ;Both
                    prtpage prfile;*pcolumn:row,*boldon,"Please fax back updated DATACARD and Continuation USAGE with Counts.",*boldoff;
          elseif (DCARD = "D")                 ;Datacard Only
                    prtpage prfile;*pcolumn1:row,*boldon,"Please fax back updated DATACARD with counts.",*boldoff;
          elseif (DCARD = "U")                 ;Usage Only
                    prtpage prfile;*pcolumn1:row,*boldon,"Please fax back Continuation USAGE with counts.",*boldoff;
          endif
          add     eightlpi,row
          add     eightlpi,row

        Else
          add     eightlpi,row
        Endif
.EndSubpatch1.8
.Patch1.5
    if (OMIT = YES)
        add     eightlpi,row
        prtpage prfile;*pcolumn1:row,*boldon,"Names Updated Through ____________________________";
        add     eightlpi,row
        add     eightlpi,row
        prtpage prfile;*pcolumn1:row,*boldon,"Information Provided by __________________________";

    else
.
.        prtpage prfile;*pensize=10,*RECT=row:9700:column2:column6;
.        prtpage prfile;*pcolumn4:row,*line=column4:9700;
        move    row,N10
.        add     "600",N10
.        prtpage prfile;*pensize=20,*RECT=row:N10:column2:column6;
.        prtpage prfile;*pcolumn4:row,*line=column4:N10,*pensize=10;
        add     "500",column2,result2
        add     "500",column4,result3
        add     eightlpi,row
        prtpage prfile;*presult2:row,"Geography";
        add     eightlpi,row
        prtpage prfile;*presult3:row,"Quantity";
        add     eightlpi,row
        prtpage prfile;*presult2:row,"SCF/State";
        add     "200",column2,result2
        add     "200",column4,result3
        for N9 from "1" to "15"
                add     "310",row
                load    str10 from N9 of nrcog01,nrcog02,nrcog03,nrcog04,nrcog05,nrcog06:
                        nrcog07,nrcog08,nrcog09,nrcog10,nrcog11,nrcog12,nrcog13,nrcog14,nrcog15
                call    Trim using str10
                prtpage prfile;*presult2:row,str10
                if (N9 = 2)
                        move    row,N7
.                elseif (N9 = 7)
                elseif (N9 = 13)
                        move    row,N8
                endif
        repeat
        add     "310",row
        prtpage prfile;*presult2:row,"Total:";
.;patch1.81
        add     "310",row
        prtpage prfile;*presult2:row,"Counts provided by:";
.;subpatch1.81
.        PRTPAGE PRFILE;*PICTRECT=*OFF,*PICT=N7:N8:column:7600:pict1
.        if (SecondReq = YES)
.                PRTPAGE PRFILE;*PICTRECT=*OFF,*PICT=howmany:N8:column:7600:pict1
.        endif
.
        move    N10,row
.Patch1.8
        if (DCARD <> "N")
.START PATCH 1.91 REPLACED LOGIC
.                 prtpage prfile;*pensize=10,*RECT=row:9835:column2:column6;
.                 prtpage prfile;*pcolumn4:row,*line=column4:9835;
                  prtpage prfile;*pensize=10,*RECT=row:9970:column2:column6;
                  prtpage prfile;*pcolumn4:row,*line=column4:9970;
.END PATCH 1.91 REPLACED LOGIC
        else
.START PATCH 1.91 REPLACED LOGIC
.                 prtpage prfile;*pensize=10,*RECT=row:9700:column2:column6;
.                 prtpage prfile;*pcolumn4:row,*line=column4:9700;
                  prtpage prfile;*pensize=10,*RECT=row:9835:column2:column6;
                  prtpage prfile;*pcolumn4:row,*line=column4:9835;
.END PATCH 1.91 REPLACED LOGIC
        endif
.subpatch1.8
        add     "600",N10
        prtpage prfile;*pensize=20,*RECT=row:N10:column2:column6;
        prtpage prfile;*pcolumn4:row,*line=column4:N10,*pensize=10;
.
        for N9 from "1" to "14"
                add     "310",N10
                prtpage prfile;*pcolumn2:N10,*line=column6:N10;
        repeat
.        add     "310",row
.        prtpage prfile;*presult2:row,"TOTAL:";
        add     "310",N10
        move    C0,N9
        add     "310",N10,N9
        prtpage prfile;*pensize=20,*RECT=N10:N9:column2:column6;
        prtpage prfile;*pcolumn4:N10,*line=column4:N9,*pensize=10;
    endif
.patch1.7
.        IF (DCARD = YES)
.                PRTPAGE PRFILE;*PICTRECT=*OFF,*PICT=5250:9000:6750:7600:pict2
.        ENDIF
.patch1.7
        PRTCLOSE prfile
.
.begin patch 2.0
          if        (Emailflag = Yes)              .good to go
.begin patch 2.10
.          call      GetPDfPAth
.          pack      str45 from PDFPATH,"\flag.dat"
.          pack      APIFileName,STR45,hexzero
.          loop
.                    call      FindFirstFile
.                    until (APIResult = 0 | APIResult = hexeight)
.                    pause     "1"
.          repeat
          pause     "2"
.          erase     str45          
.end patch 2.10
          Clear     MailSubjct
          append    "Counts Request for List - ",MailSubjct
          append    OLSTNAME,MailSubjct
          reset     MailSubjct
.          move    "Counts Request",MailSubjct
.   Set the text message that is send with the attachments
          Clear     MailBody
          append    Mailto,mailbody
          append    CRLF,mailbody
          append      "CountsReq.pdf",MailBOdy
          append    CRLF,mailbody
          Append    "Please find enclosed counts request for our Mailer:",MailSubjct
          append    CRLF,mailbody
          append    "mcomp, for your list:",mailbody
          append    CRLF,mailbody
          append    OLSTNAME,mailbody
          append    CRLF,mailbody
          reset     MailBody
.          move      "davidHerrick@nincal.com",mailto
          call      trim using cntname
          call      RemoveChar using cntname,b1
          Pack      MailFrom from cntname,"@nincal.com"
          pack      mailcc from cntname,"@nincal.com"
          Pack      MailAttach from "c:\work\pdf\CountsReq.pdf"
CheckFile
          Move      MailAttach,Str55
          trap      WaitForEnd giving error if IO
          open      FileCheck,"c:\work\pdf\CountsReq.pdf",Exclusive     
          Close     FIleCHeck
          trap      IOMssg GIVING ERROR IF IO

          call      waitin using "5"
          call      debug
          call      SendMail
          call      waitin using "10"
.begin patch 2.10
.          call      PDF995Auto0
.end patch 2.10

          erase     mailattach
          endif

.        if (badfaxflag <> YES)
.                clear   taskname
..begin patch 1.6
..                Path    Exist,"c:\windows"
..                if      over
.                    If                  (osflag = c1 | osflag = c5)
.                        append  "!c:\winnt\system32\cmd.exe",taskname
..                else
.                 ElseIf                  (osflag = c3 | osflag = c4)
.                        append  "!c:\command.com",taskname
.                 ElseIf                  (osflag = c6)
.                        append  "!c:\windows\system32\cmd.exe",taskname
..end patch 1.6
.                endif
.                append  " /c copy ",taskname
..START PATCH 1.92 REPLACED LOGIC
..                append  NTWKPATH4,taskname
..                append  "fax\hdrfile.prn /b + ",taskname
..                append  NTWKPATH4,taskname
..                append  "fax\faxfile.prn /b ",taskname
.                append  "C:\WORK\hdrfile.prn /b + ",taskname
.                append  "C:\WORK\faxfile.prn /b ",taskname
..END PATCH 1.92 REPLACED LOGIC
..                append  "c:\work\faxfile.prn /b ",taskname
.                append  NTWKPATH4,taskname
.                append  "fax\",taskname
.                clock   timestamp,timestamp
.                append  timestamp,taskname
.                append  ".prn /b",taskname
.                reset   taskname
.                execute taskname
.                clear   taskname
..begin patch 1.6
..                Path    Exist,"c:\windows"
..                if      over
.                    If                  (osflag = c1 | osflag = c5)
.                        append  "!c:\winnt\system32\cmd.exe",taskname
..                else
.                 ElseIf                  (osflag = c3 | osflag = c4)
.                        append  "!c:\command.com",taskname
.                 ElseIf                  (osflag = c6)
.                        append  "!c:\windows\system32\cmd.exe",taskname
..end patch 1.6
.                endif
.                append  " /c copy ",taskname
.                append  NTWKPATH4,taskname
.                append  "fax\",taskname                                         ."
.                append  timestamp,taskname                                                          ."
.                append  ".prn \\srv2008a\fax",taskname                                        ."
.                reset   taskname
.                execute taskname
..Delete all the work files
.                move    "                                        ",APIFileName
.                clear   APIFileName
..START PATCH 1.92 REPLACED LOGIC
..                pack    APIFileName,NTWKPATH4,"fax\faxfile.prn",hexzero
.                pack    APIFileName,"C:\WORK\faxfile.prn",hexzero
..END PATCH 1.92 REPLACED LOGIC
.                call    DeleteFile
.                if (APIResult = 0 | APIResult = hexeight)
.                endif
..
.                move    "                                        ",APIFileName
.                clear   APIFileName
.                pack    APIFileName,NTWKPATH4,"fax\",timestamp,".prn",hexzero
.                call    DeleteFile
.                if (APIResult = 0 | APIResult = hexeight)
.                endif
.        endif
.end patch 2.0
.
        move    "                                        ",APIFileName
        clear   APIFileName
        pack    APIFileName,"\\nins1\d\users\",USER,"\",INPNAME,".dat",hexzero
        call    DeleteFile
        if (APIResult = 0 | APIResult = hexeight)
        endif
        shutdown
.begin patch 2.0
..faxspool - Facsys printer not available/defined do it a different way
.
.faxspool  trapclr  spool
.          move     yes to badfaxflag
.          return
.end patch 2.0
.begin patch 2.0
WaitForEnd
                    TrapClr   IO
.check the error if file does not exist just get out
                    add       C1,Trapcount
                    call      Waitin using "5"
.                    pause     c5
                    noreturn
.                   if        (trapcount > 240)   . 20 min are you kidding me
                   if        (trapcount > 60)   . 5 min are you kidding me
.                    if        (trapcount > 36)   . 3 min are you kidding me
                    Pack       MailSubjct,"Counts Request - ",str25
                    Move      "CReques@nincal.com",MailTO
                    append    CRLF,MailBOdy
                    append    "c:\work\pdf\CountsReq.pdf",MailBody
                    append    CRLF,MailBOdy
                    append    str55,MailBody
                    append    CRLF,MailBOdy
                    append    "I am sorry I could not send the file",Mailbody
                    reset     Mailbody
                    Move      B1,Mailattach
                    call      SendMail
                    return
                    
                    endif
          
                    goto      checkfile
.end patch 2.0

         INCLUDE   COMLOGIC.inc
