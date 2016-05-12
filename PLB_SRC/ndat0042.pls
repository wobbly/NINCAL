PC EQU 0
          INCLUDE   common.inc
          INCLUDE   cons.inc
          include   ndatdd.inc
          include   nseldd.inc
          include   nrefdd.inc
;         include   nmoddd.inc
          include   nadddd.inc
          include   narrdd.inc
          include   ncatdd.inc
          include   NSLTdd.inc
          include   nsrcdd.inc
          include   nmoddd.inc
          include   ntxtdd.inc
          include   nowndd.inc          
.START PATCH 1.4 REMOVED LOGIC
.         include   nfuldd.inc
.END PATCH 1.4 REMOVED LOGIC
Release INIT "1.4" 21JUN2006 DMS  Fulfillment Conversion
.Release INIT "1.3" 14Feb2005 DLH  After discussion with SMM changed logic from
.                     revdate to NDATUPDDATE
;Release INIT "1.2" 23DEC2004 ASH Employee changes
.Release INIT "1.1" 05AUG2004 ASH Logo Conversion
.Release INIT "1.0" 04APR2004 DMB Initial Release
DataFile  FILE
.tempfile FILE
SingleSpaced        FORM      "180"
PrintFile pfile     
PrintQueueName      DIM       100
PrintFileName       DIM       150
TimesNew5 font
TimesNew7 font
TimesNew11          font
TimesNew16          font
TimesNew16I         font
CourierNew12 font      
CopyVar2 dim        5000                                 
CreateDataCard external "NDAT0002;CreateFaxFileDataCard"
hold      DIM       600
hold2     DIM       600
CardName  DIM       30
faxflag dim         1
LongDist  DIM       1
Count     FORM      5
holdown dim         4
          create    TimesNew5,"Courier New",size=6
          create    TimesNew7,"Times New Roman",size=10
          create    TimesNew11,"Times New Roman",size=12
          create    TimesNew16,"Times New Roman",size=24
          create    TimesNew16I,"Times New Roman",size=24,Italic
          create    CourierNew12,"Courier New",size=11
.START PATCH 1.1 ADDED LOGIC
NINLogo   PICT
          CREATE    NINLogo=3:13:30:50:
                    "\\nins1\e\netutils\NIN logo black outline.jpg"
.END PATCH 1.1 ADDED LOGIC

   CLOCK    DATE TO str10
   UNPACK   str10 INTO MM,STR1,DD,STR1,yy
          move      MM to n2
          load      str12 with n2,"January","February","March","April","May","June","July","August","September","October","November","December"
          pack      str30,str12,b1,dd,comma,b1,cc,yy
   OPEN  DataFile,INPNAME,EXCLUSIVE
          Move  "799" to N3
          Call  PrepCreateCard
          READ  DataFile,SEQ;datvars
;;;;;;;;;;;;
                    LOOP
                    UNTIL OVER
                    add c1 to count
                              if (count = c1)
                                        unpack    ownnum,str2,holdown
                                        move      holdown,nownfld
.                                       packkey hold with datvars
                              endif
                              unpack    ownnum,str2,str4
                              match     str4 to holdown
                              if not equal
                                        packkey hold2 with datvars
                                        unpack hold in datvars 
                                        call PrintLetterAndFax
                                        Call PrepCreateCard
                                        packkey hold with hold2
                                        unpack hold2 in datvars
                              else
                                        packkey hold with datvars
                              endif

                              Call                WriteCreateCard
                              unpack    ownnum,str2,holdown
                              READ     DataFile,SEQ;datvars
                              
                    REPEAT
;;;;;;;;;;;;
                    unpack    ownnum,str2,str4
                    match     str4 to holdown
                    if not equal
                              packkey hold2 with datvars
                              unpack hold in datvars 
                              call PrintLetterAndFax
                    endif

     shutdown  "cls"
          stop

PrintLetterAndFax
                    Call CreateFaxHeader
                    Call PrintPagePrep
                    Call LogoHeader
                    Call Body
                    Call Createcard
                    Call AppendandSend
          return






PrintPagePrep
;
                    clear prtname
                    clear cardname
                    clear printfilename
                    add       c1 to n3
                    move n3 to str3
                    call zfillit using str3
                    rep zfill,str3
                    APPEND      "DATA" TO PRTNAME
                    APPEND    str3 to PRTNAME
                    RESET     PRTNAME
      APPEND    NTWKPATH1 TO PRINTFILENAME
      APPEND    PRTNAME TO PRINTFILENAME
      APPEND    ".LST" TO PRINTFILENAME
      RESET     PRINTFILENAME
;Datacard Print Name
      APPEND    NTWKPATH1 TO CARDNAME
                    APPEND              "card" to CARDNAME
                    append    str3 to CARDNAME
      APPEND    ".LST" TO CARDNAME
                    reset     cardname
      DISPLAY   *P01:07,"Output File :":
                *P15:07,PRTNAME
;
;
;         REP       LOWUP,PRTNAME
;         scan ".LST",PRTNAME
;         if equal 
;                   movefptr prtname,n2
;                   sub c1 from n2
;                   reset prtname
;                   setlptr   prtname,n2
;         else 
;                   move prtname to PRINTQUEUENAME
;                   PACK      PRINTFILENAME,NTWKPATH1,PRINTQUEUENAME,".LST"
;         endif
          PACK      PRINTQUEUENAME,PRTNAME
;         PACK      PRINTFILENAME,NTWKPATH1,PRINTQUEUENAME,".LST"
          PRTOPEN printfile,"faxfile","Faxfile.prn"
          PRTPAGE printfile;*UNITS=*HIENGLISH:
          *ORIENT=*Portrait:
          *MarginL=1;
          move "300" to Row
          return
LogoHeader
.START PATCH 1.1 REPLACED LOGIC
.         prtpage printfile;*p4000:300,*ALIGNMENT=*RIGHT,*font=TimesNew16,*ll,*boldon,"Names ",*boldoff;
.         prtpage printfile;*p4000:300,*ALIGNMENT=*LEFT,*font=TimesNew16I,"in the News ",*boldoff;
.         prtpage printfile;*p800:700,*pensize=15,*line=7300:700;
.         prtpage printfile;*p4200:800,*font=TimesNew11,*ALIGNMENT=*CENTER,*ll,"C  A  L  I  F  O  R  N  I  A    I  N  C .";
          prtpage   printfile;*units=*HIENGLISH,*Pictrect=*off,*PICT=0:1300:500:8100:NINLogo
          return
.END PATCH 1.1 REPLACED LOGIC
Body
          MOVE "2000" TO ROW
          prtpage printfile;*p4000:row,*ALIGNMENT=*CENTER,*font=FONT12,*ll,*boldon,"REQUEST FOR FULFILLMENT",*boldon;
          ADD SINGLESPACED TO ROW
          prtpage printfile;*p4000:row,*ALIGNMENT=*CENTER,*font=FONT12,*ll,*boldon,"FILE UPDATE",*boldon;
          MOVE "3000" TO ROW
          prtpage printfile;*p600:row,*ALIGNMENT=*LEFT,*font=FONT12,*ll,"TO: ";
          prtpage printfile;*ll,ownlonm;
          ADD SINGLESPACED TO ROW
          prtpage printfile;*p600:row,*ALIGNMENT=*LEFT,*font=FONT12,*ll,ownocpy;
          ADD SINGLESPACED TO ROW
          ADD SINGLESPACED TO ROW
          ADD SINGLESPACED TO ROW
          ADD SINGLESPACED TO ROW
          ADD SINGLESPACED TO ROW
          prtpage printfile;*p600:row,*ALIGNMENT=*LEFT,*font=FONT12,*ll,"FR: ";
.START PATCH 1.2 REPLACED LOGIC
.         prtpage printfile;*ll,"Steve Kehrli";
.         ADD SINGLESPACED TO ROW
.         prtpage printfile;*p600:row,*ALIGNMENT=*LEFT,*font=FONT12,*ll,"Vice President";
.         ADD SINGLESPACED TO ROW
.         prtpage printfile;*p600:row,*ALIGNMENT=*LEFT,*font=FONT12,*ll,"List Management";
.         ADD SINGLESPACED TO ROW
.         prtpage printfile;*p600:row,*ALIGNMENT=*LEFT,*font=FONT12,*ll,"510-302-4640";
.         ADD SINGLESPACED TO ROW
.         prtpage printfile;*p600:row,*ALIGNMENT=*LEFT,*font=FONT12,*ll,"Skehrli@nincal.com";
...................................................
          prtpage printfile;*ll,"Becky Chavez";
          ADD SINGLESPACED TO ROW
          prtpage printfile;*p600:row,*ALIGNMENT=*LEFT,*font=FONT12,*ll,"Manager";
          ADD SINGLESPACED TO ROW
          prtpage printfile;*p600:row,*ALIGNMENT=*LEFT,*font=FONT12,*ll,"List Management";
          ADD SINGLESPACED TO ROW
          prtpage printfile;*p600:row,*ALIGNMENT=*LEFT,*font=FONT12,*ll,"510-302-4641";
          ADD SINGLESPACED TO ROW
          prtpage printfile;*p600:row,*ALIGNMENT=*LEFT,*font=FONT12,*ll,"BChavez@nincal.com";
.END PATCH 1.2 REPLACED LOGIC
          ADD SINGLESPACED TO ROW
          ADD SINGLESPACED TO ROW
          prtpage printfile;*p600:row,*ALIGNMENT=*LEFT,*font=FONT12,*ll,"RE: FILE UPDATE REQUEST";
          ADD SINGLESPACED TO ROW
          ADD SINGLESPACED TO ROW
          prtpage printfile;*p600:row,*ALIGNMENT=*LEFT,*font=FONT12,*ll,"Date: ";
          prtpage printfile;*ll,str30;
          ADD SINGLESPACED TO ROW
          ADD SINGLESPACED TO ROW
          prtpage printfile;*p600:row,*ALIGNMENT=*LEFT,*font=FONT12,*ll,"A recent file update is often the determining factor on whether or not a list is selected during the";
          ADD SINGLESPACED TO ROW
          prtpage printfile;*p=600:row,"campaign planning process.  In our continued effort to provide the best service possible, we are reminding";
          ADD SINGLESPACED TO ROW
          prtpage printfile;*p=600:row,"you that by updating your file, it will help maximize list rental income and/or exchange opportunities.";
          ADD SINGLESPACED TO ROW
          ADD SINGLESPACED TO ROW
          prtpage printfile;*p=600:row,"As a result of updating you also remove individuals who have asked not to have their name shared";
          ADD SINGLESPACED TO ROW
          prtpage printfile;*p=600:row,"since your last file update.  Finally, when your file updates, it is included in our #"Lists of the Week#" fax";
          ADD SINGLESPACED TO ROW
          prtpage printfile;*p=600:row,"promotion to top brokers and is updated in our website datacard library.";
          ADD SINGLESPACED TO ROW
          ADD SINGLESPACED TO ROW
          prtpage printfile;*p=600:row,*boldon,"Please note that your file has not updated since: ",*boldoff;
          CALL GetPrettyRevisionDate
          prtpage printfile;*ll,*boldon,str35,*boldoff;
          ADD SINGLESPACED TO ROW
          ADD SINGLESPACED TO ROW
          prtpage printfile;*p=600:row,"For more information on completing this process, please contact your Names in the News representative";
          ADD SINGLESPACED TO ROW
          prtpage printfile;*p=600:row,"or me today.  If you would prefer not to receive this fax in the future please let me know at your";
          ADD SINGLESPACED TO ROW
          prtpage printfile;*p=600:row,"convenience.";
          call footerinfo
          PRTCLOSE printfile
          pause   "5"
          RENAME "c:\work\faxfile.prn",PRINTFILENAME
          return

CreateFaxHeader
                              unpack    ownnum,str2,nownfld
.         move     ownnum to nownfld
         call     nownkey
         cmatch    b1 to ownlonm
         if        eos
                   clear     ownlonm
         endif   
         reset     ownocpy to 25
blankc
                              
                              cmatch   b1 to ownocpy
         if       equal
                                        bump     ownocpy,-1
                                        goto     blankc
                              else
                                        lenset   ownocpy
                                        reset    ownocpy,1
                              endif
         match     "0000000000" to ownfax
         if        equal
          move NO to faxflag
                              else 
                                        move YES to faxflag
         endif
         type      ownfax                      .valid phone?
         IF        EQUAL                       .yes
                                        move YES to faxflag
                                        COUNT     N2,OWNFAX
                                        COMPARE   C10 TO N2
                                        IF        EQUAL
                                                  MOVE      C1 TO LONGDIST
                                                  UNPACK    OWNFAX INTO STR3,STR7
                                                  match     "510" to str3            . local?
.END PATCH 1.1 REPLACED LOGIC
                    IF         EQUAL                    
                                                                      MOVE       STR7 TO OWNFAX
                                                                      CLEAR      LONGDIST
                    endif
             endif
                              else
                                        move NO to faxflag
                              endif
                              pack      taskname,"c:\work\hdrfile.prn"
                              splopen   taskname
                              print   "^[D",longdist,ownfax,"^[N",ownocpy,"^[SQuarterUpdate","^]";
.                             print   "^[D",longdist,"4154337796","^[N",ownocpy,"^[SQuarterUpdate","^]";
.                                       print   HPCOUR,"^[D",PRMPhNum,"^[N",PRMCntName,"^[T",DefDate,"^[S","QuarterUpdate"," ^]"
;                                       print   HPCOUR,"^[D",PRMPhNum,"^[N",PRMCntName,"^[T",DefDate," 20:00 ^[S","QuarterUpdate"," ^]"
                              splclose
          return


AppendandSend
          Call GETWINVER
          clear     copyvar2
          if (OSFLAG = "1" or OSFLAG = "5")  .NT4,NT5
                    append    "!c:\winnt\system32\cmd.exe",CopyVar2
                    append  " /c copy ",CopyVar2
          elseif (OSFLAG = "6")  .XP
                    append    "!c:\windows\system32\cmd.exe",CopyVar2
                    append  " /c copy ",CopyVar2
          else (OSFLAG = "3" or OSFLAG = "4")  .WIN95,98
                    append    "!c:\command.com",CopyVar2
                    append  " /c copy ",CopyVar2
          endif
          append  "c:\work\hdrfile.prn /b + ",CopyVar2
          append  PRINTFILENAME,CopyVar2
          append    " /b +",CopyVar2
          append  CARDNAME,CopyVar2
          append    " /b ",CopyVar2
          add       C1,howmany
          move      howmany,str9
          call      Trim using str9
          pack      taskname,NTWKPATH1,"datafax",str9,".fax"
          append  taskname,CopyVar2
          reset   CopyVar2
.         execute CopyVar2
          clear   CopyVar2
          Call GETWINVER
          if (OSFLAG = "1" or OSFLAG = "5")  .NT4,NT5
                    append    "!c:\winnt\system32\cmd.exe",CopyVar2
          elseif (OSFLAG = "6")  .XP
                    append    "!c:\windows\system32\cmd.exe",CopyVar2
          else (OSFLAG = "3" or OSFLAG = "4")  .WIN95,98
                    append    "!c:\command.com",CopyVar2
          endif
          append  " /c copy ",CopyVar2
          append  taskname,CopyVar2
          if (faxflag = YES)
.                    append  " \\nts3\fax",CopyVar2
                    append  " \\NINS2\fax",CopyVar2
          else 
                    append  " \\NINs2\Laser3",CopyVar2    .no fax num send to sk
          endif
          reset   CopyVar2
.         execute CopyVar2
          erase     taskname
          erase "c:\work\hdrfile.prn"
          erase     cardname
          erase     printfilename
          return



FooterInfo
.START PATCH 1.1 REMOVED LOGIC
.         prtpage printfile;*p4000:10000,*ALIGNMENT=*CENTER,*font=TimesNew7,*ll,"1300 Clay Street - Oakland, CA 94612-1429 · 415-989-3350 · Fax 415-433-7796 ",*boldoff;  
.END PATCH 1.1 REMOVED LOGIC
          return
GetPrettyRevisionDate
;begin patch 1.3
          unpack    REVDATE,str2,yy,mm,dd
;                   UNPACK         NDATUPDDATE,str2,yy,mm,dd
;end patch 1.3
          move      MM to n2
          load      str12 with n2,"January","February","March","April","May","Jun","Jul","August","September","October","November","December"
          pack      str35,str12,b1,dd,comma,b1,str2,yy
          return    

PrepCreateCard
          erase     "c:\work\datacrd.lst"
          prepare   tempfile,"c:\work\datacrd.lst"
          return
WriteCreateCard
.         unpack    hold,DATVARS
          unpack    hold,str1,lstnum
          call      Trim using LSTNUM
                    if (LSTNUM <> "")
                              write     tempfile,SEQ;hold
          endif
          return
CreateCard
          close     tempfile
          move      "c:\work\datacrd.lst",str25
          call CreateDataCard using str25,CARDNAME
          return
          
          include   nselio.inc
          include   nrefio.inc
          include   nmodio.inc
          include   naddio.inc
          include   narrio.inc
          include   ncatio.inc
          include   NSLTio.inc
          include   nsrcio.inc
          include   ntxtio.inc
          include   ndatio.inc
.START PATCH 1.4 REMOVED LOGIC          
.         include   nfulio.inc
.END PATCH 1.4 REMOVED LOGIC
          include   nownio.inc
          INCLUDE   COMLOGIC.INC