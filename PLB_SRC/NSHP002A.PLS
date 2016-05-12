........................................
. Program:      NSHP002A.PLS
. Function:     Shipping Request Report Program
. Author:       Andrew Harkins
. Orig. Date:   May 07, 2003
. Release:      1.0
........................................

PC      EQU     1
.Include Files
        include common.inc
        include cons.inc
        include norddd.inc
;Patch1.1
          include   compdd.inc
          include   cntdd.inc
.        include nmlrdd.inc
;Patch1.1
        include ndatdd.inc
        include nowndd.inc
        include ncntdd.inc
        include winapi.inc
          include   nshpdd.inc
          include   nshp2dd.inc
.START PATCH 1.4 REMOVED LOGIC          
.         include   nfuldd.inc
.END PATCH 1.4 REMOVED LOGIC
.TESTING FOR WATERMARK
.PICT1   PICT
.        CREATE     PICT1=3:13:30:50:
.                "F:\LIBRARY\DEVELOP\2NDREQUEST9.BMP"
.RESULT2 FORM    9
.RESULT3 FORM    9
Release   init      "1.71"        DLH New fax gateway
REldate   Init      "2015 April 14"
.Release   init      "1.70"        DLH Sunbelt PDF
.REldate   Init      "2013 April 23"
.Release   init      "1.64"        DLH Change fax server from srv2008a to nins2
.REldate   Init      "11 January 2012"
.Release   init                "1.63"        DLH fulfillment conversion should have included new sort & break - fix
.REldate   Init      "05 May 2008"
.Release  init      "1.62"        DLH   2008 April 24       MailSend
.Release  init      "1.61"        DLH   OCt2007   new fax server
.Release  init      "1.6"        ASH    19JUN2007 PLI Inclusion
.Release  init      "1.5"        DMB    12OCT2006 Integrated Company/fulfillment Number into the order file and out of the owner file.  Fulfillment number will now be associated withe the datacard.
.release  init      "1.4"        DMS    22JUN2006 Fulfillment Conversion
.release  init      "1.3"        ASH    09DEC2004 FAXFILE
.release  init      "1.2"        ASH    02DEC2004 Conversion from Acrobat Distiller to PDF995
.release init    "1.1"   ASH 09AUG04  Logo Conversion
.release init    "1.0"   ASH 07AUG03  DEVELOPMENT RELEASE

.Files to open
prfile  pfile
input     file
First   init    "Y"
PrtFlag dim     1
SecFlag form        1
HOLDOWN dim     4
COMPHOLD DIM    45
newdate1 dim    10
faxnum  dim     10
LONGDIST dim    1
page    form    9
date    dim     8
line1   dim     88

PrintFlag form  "00"    .Default is Laser2!!!
input2Name dim  40
output1 dim     40
output2 dim     40
userlogn dim    7
.hexeight integer 4,"4294967295"
.timestamp1 dim  16
timestamp2 dim  16
.time1   form    16
.time2   form    16
.time3   form    16
.
mss1    plform  Error
.Define Fonts to be used
font1   font
font2   font
font3   font
font4   font
font5   font
font6   font
.START PATCH 1.6 ADDED LOGIC
font7     font
intrnet   dim       70
input2    file
HOLDEXCL dim        1
HOLDEXCL2 dim       1
.END PATCH 1.6 ADDED LOGIC
.begin patch 1.63
HOLDFUL  DIM       6
.end patch 1.63

          formload mss1
          create  font1,"Arial",size=12,bold
          create  font2,"Arial",size=9
          create  font3,"Arial",size=7
          create  font4,"Fixed",size=9
          create  font5,"Arial",size=10 .11
          create  font6,"Arial",size=14
.START PATCH 1.6 ADDED LOGIC
          create    font7,"Times New Roman",size=9
.END PATCH 1.6 ADDED LOGIC

.START PATCH 1.1 ADDED LOGIC
NINLogo   PICT
          CREATE    NINLogo=3:13:30:50:
                    "\\nins1\e\netutils\NIN logo black outline.jpg"
.END PATCH 1.1 ADDED LOGIC

          move      "NSHP002A",PROGRAM
          move      C0,PrintFlag
          call      Trim using prtname
          move      prtname,PrintFlag
          unpack    inpname,str16,str4
          if (PrintFlag = 2)            .PDF - Run on Batch
                    pack      input2Name,NTWKPATH3,inpname
.START PATCH 1.6 REPLACED LOGIC
.                   pack      output1,NTWKPATH3,str16,str4
                    pack      str40,NTWKPATH3,str16,str4
.END PATCH 1.6 REPLACED LOGIC
                    bump      str4
                    call      Trim using str4
.START PATCH 1.6 ADDED LOGIC
                    pack      output1,NTWKPATH3,str16,str4,".NEW"
.END PATCH 1.6 ADDED LOGIC
                    pack      output2,NTWKPATH3,str16,str4,".SRT"
          else                                    .Local
                    pack      input2Name,inpname
.START PATCH 1.6 REPLACED LOGIC
.                   pack      output1,str16,str4
                    pack      str40,str16,str4
                    pack      output1,str16,".NEW"
.END PATCH 1.6 REPLACED LOGIC
                    pack      output2,str16,".SRT"
          endif
          call      Trim using user
          if (user = "")
                    move      "000",user
          endif
          move      user,NCNTFLD1
          rep       zfill,NCNTFLD1
          move      C3,NCNTPATH
          move      "NCNTKEY",Location
          pack      KeyLocation,"Key: ",NCNTFLD1
          call      NCNTKEY
          if over
.START PATCH 1.6 REPLACED LOGIC
.                   clear     str24
                    clear     intrnet
.END PATCH 1.6 ADDED LOGIC
                    goto cntexit
          endif
          scan    "BILLING",CNTNAME
          goto cntexit if equal
          move    CNTNAME,str1
cntloopy
          bump    CNTNAME,1
          cmatch  B1,CNTNAME
          goto    cntloopy if not equal
          goto    cntexit if eos
          bump    CNTNAME,1
          move    CNTNAME,str7
          call    RemoveChar using str7,B1
          move    str7,str6
.START PATCH 1.6 REPLACED LOGIC
.         clear   str24
.         pack    str24,str1,str6,"@NINCAL.COM"
          clear   intrnet
          pack    intrnet,str1,str6,"@NINCAL.COM"
.END PATCH 1.6 REPLACED LOGIC
cntexit
          reset   CNTNAME
          pack      userlogn,str1,str6
          move    "NSHP002A",WPrognme
          clock   timestamp,timestamp
          unpack    timestamp,CC,YY,MM,DD
          pack    newdate1,MM,SLASH,DD,SLASH,CC,YY
          call      GetWinVer
.Set Flags to Open NINORD/NINSHP/NINSHP2
          move    C1,NORDPATH
          move    C0,NORDFLAG
          move    C0,NORDFLG2
          move      C0,NSHPFLAG
          move      C0,NSHP2FLAG
SortFile
.START PATCH 1.6 ADDED LOGIC
.Rewrite an input file, allowing sorting by Corporate account (PLI vs NIN)
          erase     output1
          prep      input2,output1
          open    input,STR40
          loop
                    read    input,seq;ORDVARS,taskname
                    until over
                    if (OCompID = "P" | OCompID2 = "P")
                              move      "P",str1
                    else
                              clear     str1
                    endif
                    write     input2,seq;str1,ORDVARS,taskname
          repeat
          close     input2
          close     input
.END PATCH 1.6 ADDED LOGIC
          clear   taskname
          append  output1,taskname
          append  COMMA,taskname
          append  output2,taskname
          append  ";",taskname
.START PATCH 1.6 REPLACED LOGIC
.Begin patch 1.63
.        pack    taskname,"\\nins1\e\data\diskin71.tmp,\\nins1\e\data\diskin71.dat;22-25,329-334,214-247"

.         append  "22-25",taskname
.         append  "1-1,23-26",taskname
          append  "1-1,23-26,330-335,215-248",taskname
.end patch 1.63
.END PATCH 1.6 REPLACED LOGIC
          reset   taskname
          sort    taskname
          if over
                    move    s$error$,error
                    move    "Sort did not work!",Location
                    call    IOMssg
                    shutdown
          endif
          clear   taskname
PrintFile
.Set up columns
          move    "500",column
          move    "1200",column1
          move    "2700",column2
          move    "3450",column3
          move    "4200",column4
          move    "4950",column5
          move    "5700",column6
          move    "6750",column7
.Initialize HOLDOWN
          clear   HOLDOWN
          open    input,output2
          loop
.START PATCH 1.6 REPLACED LOGIC
.                   read    input,seq;ORDVARS,taskname
                    read    input,seq;HOLDEXCL,ORDVARS,taskname
.END PATCH 1.6 REPLACED LOGIC
                    goto    LastRec if over
                    call      Trim using taskname
                    unpack    taskname,SHPVARS
                    if (SPDATE <> "")
                              move      C1,SecFlag
                    else
                              move      C0,SecFlag
                    endif
                    call      ShipReadOtherFiles
.START PATCH 1.6 REPLACED LOGIC
.                   if (OLON <> HOLDOWN)
.Begin patch 1.63
.                   if (OLON <> HOLDOWN | HOLDEXCL <> HOLDEXCL2)
                    if (OLON <> HOLDOWN | HOLDEXCL <> HOLDEXCL2 | ofullfIL <> HoldfuL)
.         mOVE      ofullfIL,hoLDfuL     
.end patch 1.63
.END PATCH 1.6 REPLACED LOGIC
                              if (FIRST = YES)
                                        move    OWNOCPY,COMPHOLD
                              endif
                              if (FIRST = NO)
                                        PRTCLOSE prfile
                                        if (PrintFlag = 2)
.testit
.It takes some time for the file to be created, so we must check
.Allow 20 seconds to originally load DISTILLER
                                                  clock   timestamp,timestamp1
                                                  move    timestamp1,time1
                                                  loop
                                                            clock   timestamp,timestamp2
                                                            move    timestamp2,time2
                                                            sub     time1,time2,time3
                                                            if (time3 > 2000) .20 Seconds Maximum
                                                                      break
                                                            endif
                                                  repeat
                                                  call    ShipCreatePDFFile
                                        endif
                                        if (PrtFlag = NO)
.This will never be the case if called from Program 1
                                                  clear   taskname
                                                  if (osflag = C1 | osflag =C5)
                                                            append    "!c:\winnt\system32\cmd.exe",taskname
                                                  elseif              (osflag = C3 | osflag =C4)
                                                            append    "!c:\command.com",taskname
                                                  elseif              (Osflag = C6)
                                                            append    "!c:\windows\system32\cmd.exe",taskname
                                                  endif
.begin patch 1.7
.                                                  append  " /c copy ",taskname
..START PATCH 1.3 REPLACED LOGIC
..                                                 append  NTWKPATH4,taskname
..                                                 append  "fax\hdrfile.prn /b + ",taskname
..                                                 append  NTWKPATH4,taskname
..                                                 append  "fax\faxfile.prn /b ",taskname
.                                                  append  "C:\WORK\hdrfile.prn /b + ",taskname
.                                                  append  "C:\WORK\faxfile.prn /b ",taskname
..END PATCH 1.3 REPLACED LOGIC
.                                                  append  NTWKPATH4,taskname
.                                                  append  "fax\fax",taskname
.                                                  append  HOLDOWN,taskname
.                                                  append  ".prn /b",taskname
.                                                  reset   taskname
.                                                  execute taskname
.                                                  clear   taskname
.                                                  if (osflag = c1 | osflag =C5)
.                                                            append  "!c:\winnt\system32\cmd.exe",taskname
.                                                  elseif              (osflag = C3 | osflag =C4)
.                                                            append  "!c:\command.com",taskname
.                                                  elseif              (Osflag = C6)
.                                                            append  "!c:\windows\system32\cmd.exe",taskname
.                                                  endif
.                                                  append  " /c copy ",taskname
.                                                  append  NTWKPATH4,taskname
.                                                  append  "fax\fax",taskname
.                                                  append  HOLDOWN,taskname
.                                                  append  ".prn \\NINS2\fax",taskname
.                                                  reset   taskname
.                                                  execute taskname
          move    "Here is your PDF File",MailSubjct
          clear     MailBody
          append    Str55,MailBody
          append    CRLF,MailBody
          reset     MailBOdy
          reset     MailBody
          pack      MailFrom from userlogn,"@nincal.com"
          PAck      MailAttach from str55              
          call      SendMail

.end patch 1.7
                                        endif
                                        call      ShipOpenFile
                              else
                                        move    NO,FIRST
                                        call    ShipOpenFile
                              endif
                              move    OLON,HOLDOWN
                              move    OWNOCPY,COMPHOLD
.begin patch 1.63                       
                              mOVE      ofullfIL,hoLDfuL     
.end patch 1.63                         
.START PATCH 1.6 ADDED LOGIC
                              move      HOLDEXCL,HOLDEXCL2
.END PATCH 1.6 ADDED LOGIC
                              clear   page
                              call    ShipPrintHeader
                              call    ShipListHeader
                    endif
                    call      ShipPrintRecord
          repeat
LastRec
          PRTCLOSE prfile         .CLOSE AND PRINT LAST FILE
.This will only happen if called from Program 1
          if (PrintFlag = 2)
.It takes some time for the file to be created, so we must check
                    move    C0,N9
                    move    "                                        ",APIFileName
                    clear   APIFileName
.START PATCH 1.2 REPLACED LOGIC
.                   pack    APIFileName,"C:\WORK\",str55,hexzero
                    pack    APIFileName,"C:\WORK\PDF\",str55,hexzero    ."
.END PATCH 1.2 REPLACED LOGIC
                    clock   timestamp,timestamp1
                    move    timestamp1,time1
                    loop
                              clock   timestamp,timestamp2
                              move    timestamp2,time2
                              sub     time1,time2,time3
                              if (time3 > 2000) .20 Seconds Maximum
                                        break
                              endif
                    repeat
                    call      ShipCreatePDFFile
          endif
          if (PrtFlag = NO)
.This will never be the case if called from Program 1
.begin patch 1.7
.                    clear   taskname
.                    if (osflag = c1 | osflag =C5)
.                              append  "!c:\winnt\system32\cmd.exe",taskname
.                    elseif              (osflag = C3 | osflag =C4)
.                              append  "!c:\command.com",taskname
.                    elseif              (Osflag = C6)
.                              append  "!c:\windows\system32\cmd.exe",taskname
.                    endif
.                    append  " /c copy ",taskname
..START PATCH 1.3 REPLACED LOGIC
..                   append  NTWKPATH4,taskname
..                   append  "fax\hdrfile.prn /b + ",taskname
..                   append  NTWKPATH4,taskname
..                   append  "fax\faxfile.prn /b ",taskname
.                    append  "C:\WORK\hdrfile.prn /b + ",taskname
.                    append  "C:\WORK\faxfile.prn /b ",taskname
..END PATCH 1.3 REPLACED LOGIC
.                    append  NTWKPATH4,taskname
.                    append  "fax\fax",taskname
.                    append  HOLDOWN,taskname
.                    append  ".prn /b",taskname
.                    reset   taskname
.                    execute taskname
.                    clear   taskname
.                    if (osflag = c1 | osflag =C5)
.                              append  "!c:\winnt\system32\cmd.exe",taskname
.                    elseif              (osflag = C3 | osflag =C4)
.                              append  "!c:\command.com",taskname
.                    elseif              (Osflag = C6)
.                              append  "!c:\windows\system32\cmd.exe",taskname
.                    endif
.                    append  " /c copy ",taskname
.                    append  NTWKPATH4,taskname
.                    append  "fax\fax",taskname
.                    append  HOLDOWN,taskname
.                    append  ".prn \\NINS2\fax",taskname
.                    reset   taskname
.                    execute taskname
          move    "Here is your PDF File",MailSubjct
          clear     MailBody
          append    Str55,MailBody
          append    CRLF,MailBody
          reset     MailBOdy
          reset     MailBody
          pack      MailFrom from userlogn,"@nincal.com"
          PAck      MailAttach from str55              
          call      SendMail
.end patch 1.7
          endif
          move    "                                        ",APIFileName
          clear   APIFileName
          pack    APIFileName,output1,hexzero
          call    DeleteFile
          if (APIResult = 0 | APIResult = hexeight)
          endif

          move    "                                        ",APIFileName
          clear   APIFileName
          pack    APIFileName,output2,hexzero
          call    DeleteFile
          if (APIResult = 0 | APIResult = hexeight)
          endif
.
          if (PrintFlag = 0)
                    move    "                                        ",APIFileName
                    clear   APIFileName
.START PATCH 1.3 REPLACED LOGIC
.                   pack    APIFileName,NTWKPATH4,"fax\faxfile.prn",hexzero
                    pack    APIFileName,"C:\WORK\faxfile.prn",hexzero
.END PATCH 1.3 REPLACED LOGIC
                    call    DeleteFile
                    if (APIResult = 0 | APIResult = hexeight)
                    endif
                    PAUSE   C2
          elseif (PrintFlag = 2)
.                   close   input2
                    move    "                                        ",APIFileName
                    clear   APIFileName
                    pack    APIFileName,input2Name,hexzero
                    call    DeleteFile
                    if (APIResult = 0 | APIResult = hexeight)
                    endif
                    PAUSE   C2
          endif
          clear   taskname
          clear   str55
          if (osflag = c1 | osflag =C5)
                    append  "!c:\winnt\system32\cmd.exe",taskname
                    append  "!c:\winnt\system32\cmd.exe",str55
          elseif              (osflag = C3 | osflag =C4)
                    append  "!c:\command.com",taskname
                    append  "!c:\command.com",str55
          elseif              (Osflag = C6)
                    append  "!c:\windows\system32\cmd.exe",taskname
                    append  "!c:\windows\system32\cmd.exe",Str55
          endif
.START PATCH 1.2 REPLACED LOGIC
.         append  " /c del c:\work\*.pdf",taskname
.         append  " /c del c:\work\*.log",str55
          append  " /c del c:\work\PDF\*.pdf",taskname
          append  " /c del c:\work\PDF\*.log",str55
.END PATCH 1.2 REPLACED LOGIC
          reset   taskname
          reset   str55
          execute taskname
          execute str55
          shutdown

ShipCreatePDFFile
          move    "Here is your PDF File",MailSubjct
          clear     MailBody
          append    Str55,MailBody
          append    CRLF,MailBody
          reset     MailBOdy
          reset     MailBody
          pack      MailTo from userlogn,"@nincal.com"
          pack      MailFrom from userlogn,"@nincal.com"
.begin patch 1.70
.          PAck      MailAttach from "c:\work\pdf\",str55              ."
          PAck      MailAttach from str55              
.end patch 1.70
          call      SendMail
          return


ShipOpenFile
.Test logic
.         PRTOPEN prfile,"@?",""
.         return

.Print newly sorted file
          move      YES,PrtFlag
          if (PrintFlag = 4)  .Fax
                    move    OWNFAX,faxnum
.Testing
.                   move      "4154337796",faxnum
                    match   "0000000000",faxnum
                    if not equal
                              type    faxnum
                              if equal
                                        move    NO,PrtFlag      .FAX IT
                                        count   N2,faxnum
                                        compare C10,N2
                                        if equal
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
                                        endif
                              endif
                    endif
          endif
          if (PrtFlag = YES)
.Printer of your choice
                    if (PrintFlag = C0 | PrintFlag = 3 | PrintFlag = 4)     .Laser2 = Default
                              if (osflag = c1 | Osflag = C5 | osflag = c6 or Osflag = c8 or Osflag = c9)         .nt win2k Xp
                                        PRTOPEN prfile,"\\NINS2\Laser2","FAXFILE.PRN"
                              elseif (osflag = c3 | OSflag =c4)         .win 95 98
                                        PRTOPEN prfile,"Laser2","FAXFILE.PRN"
                              else   .(osflag = c0)         .Don't know prompt for printer
                                        PRTOPEN prfile,"-","FAXFILE.PRN"
                              endif
                    elseif (PrintFlag = 1)  .Laser3
                              if (osflag = c1 | osflag = C5 | osflag = c6 or Osflag = c8 or Osflag = c9)         .nt 2k xp
                                        PRTOPEN prfile,"\\NINS2\Laser3 Blankstock","FAXFILE.PRN"
                              elseif (osflag = c3 | osflag =c4)         .win 95 98
                                        PRTOPEN prfile,"Laser3 Blankstock","FAXFILE.PRN"
                              else   .(osflag = c0)         .Don't know prompt for printer
                                        PRTOPEN prfile,"-","FAXFILE.PRN"
                              endif
                    elseif (PrintFlag = 2)  .PDF
                              clear   str25
                              append  OLON,str25
                              append  "_",str25
                              append  OLNUM,str25
                              reset   str25
.START PATCH 1.2 REPLACED LOGIC
.                             PRTOPEN prfile,"Acrobat Distiller",str25
.begin patch 1.70
                              pack    str55,"c:\work\pdf\",str25,".pdf"
.                              PRTOPEN prfile,"PDF995",str25
                              PRTOPEN prfile,"PDF:",str55
.END PATCH 1.2 REPLACED LOGIC
.                              pack    str55,str25,".pdf"
.end patch 1.70
                    endif
          else
.begin patch 1.70
.Create spool file to concatenate with prtfile and send to fax machine
.PRTOPEN will not allow embedded formatting codes and so it has to be done this way :(
.Following line MUST appear in PLBWIN.INI:  PLBVOL_P=F:\DATA\FAX  !!!!
.This will give the path to find HDRFILE.PRN.  All files associated with faxes
.will now appear in this new subdirectory.  (ASH)
.                    move    "                                        ",APIFileName
.                    clear   APIFileName
..START PATCH 1.3 REPLACED LOGIC
..                   pack    APIFileName,NTWKPATH4,"fax\hdrfile.prn",hexzero
.                    pack    APIFileName,"C:\WORK\hdrfile.prn",hexzero
..END PATCH 1.3 REPLACED LOGIC
.                    call    DeleteFile
.                    if (APIResult = 0 | APIResult = hexeight)
.                    endif
..START PATCH 1.3 REPLACED LOGIC
..                   SPLOPEN "\\NINS2\d\DATA\FAX\HDRFILE.PRN"
.                    SPLOPEN "C:\WORK\HDRFILE.PRN"
..END PATCH 1.3 REPLACED LOGIC
.                    print   "^[D",longdist,faxnum,"^[N",OWNOCPY:
.                    "^[S",CNTNAME,B2,CNTPHONE," ^]"
.                    SPLCLOSE
.                    PRTOPEN prfile,"FAXFILE","FAXFILE.PRN"
.          pack      Mailto,"IMCEAFACSYS-",longdist,faxnum,"@nincal.com"
          pack      Mailto,"+",longdist,faxnum,"@fax.nincal.com"
         
          pack    str55,"c:\work\pdf\",str25,".pdf"
.                              PRTOPEN prfile,"PDF995",str25
          PRTOPEN prfile,"PDF:",str55
.END PATCH 1.2 REPLACED LOGIC
.                              pack    str55,str25,".pdf"
.end patch 1.70
          endif
          RETURN

ShipReadOtherFiles
.Open other files to retrieve appropriate information
.Mailer File
          rep     zfill,OMLRNUM
          pack    MKEY,OMLRNUM,"000"      .Master Record
          move    C3,NMLRLOCK
          move    "NMLRKEY",Location
          call    NMLRKEY
          if over
                    pack   MCOMP,"UNKNOWN",B55
          endif
.Owner File
          move    OLON,NOWNFLD
          rep     zfill,NOWNFLD
          move    "NOWNKEY",Location
          call    NOWNKEY
          if over
          else
.Start Patch 1.5 Comment Out Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.    
.                   call      Trim using OWNCTN
.End Patch 1.5 Comment Out Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.                          
.START PATCH 1.4 REMOVED LOGIC
.                   if (OWNCTN <> "")
..Fulfillment File - Triplex records should not be here
.                             move    OWNCTN,NFULFLD
.                             rep     zfill,NFULFLD
.                             move    "NFULKEY",Location
.                             call    NFULKEY
.                             if not over
..If legit Fulfillment House, I move those vars over to OWNVARS!!!!
.                                       move      NFULCOMP,OWNOCPY
.                                       move      NFULCNT,OWNLONM
.                                       move      NFULFAX,OWNFAX
.                                       clear     OWNLOSA
.                                       clear     OWNLOCTY
.                                       clear     OWNLOS
.                                       clear     OWNLOZC
.                             endif
.Start Patch 1.5 Comment Out Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.    
.                   if (OWNCTN <> "")
.Fulfillment File - Triplex records should not be here
.                             move    OWNCTN,COMPFLD6
.                             rep     zfill,COMPFLD6
.                             move    "COMPKEY6",Location
.                             call    COMPKEY6
.                             if not over
.                                       if (COMPSVBFLG = "T")
.If legit Fulfillment House, I move those vars over to OWNVARS!!!!
.                                                 move      COMPCOMP,OWNOCPY
.                                                 move      CNCTFNAME,OWNLONM
.                                                 move      COMPFAX,OWNFAX                                    
.                                                 clear     OWNLOSA
.                                                 clear     OWNLOCTY
.                                                 clear     OWNLOS
.                                                 clear     OWNLOZC
.                                       endif
.                             endif
.END PATCH 1.4 REMOVED LOGIC                                
.End Patch 1.5 Comment Out Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.      
.Start Patch 1.5 Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.      
                    Call Trim Using OFULLFIL
                    if (OFULLFIL <> "")
.Fulfillment File - Triplex records should not be here
                              move      OFULLFIL,COMPFLD
                              call      zfillit using COMPFLD
                              call      COMPKEY
                              If not over
.COMPSVBFLG - did not use this as a double check as this is now read off the order and should be read to reflect the correct fulfillment company at order time                      
.Grab the active contact full name if there is one
                                        Packkey CNCTFLD2 to "01X",COMPNUM
                                        Call      CNCTAIM
                                        loop
                                        until over
                                        until (CNCTTYPE = "4" & CNCTINACTIVE <> "T")
                                                  call      CNCTKG
                                        repeat

                                        move      COMPCOMP,OWNOCPY
                                        move      CNCTFNAME,OWNLONM
                                        move      COMPFAX,OWNFAX                                    
                                        clear     OWNLOSA
                                        clear     OWNLOCTY
                                        clear     OWNLOS
                                        clear     OWNLOZC                       
                              Endif

                    endif
.End Patch 1.5 Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.        
          endif
.Shipping Notes File
          move    OLRN,NSHP2FLD
          rep     zfill,NSHP2FLD
          move    C3,NSHP2LOCK
          move    "NSHP2KEY",Location
          call    NSHP2KEY
          call    Trim Using SNOTES2
          call    Trim Using SNOTES2B
          return

.Print Heading
ShipPrintHeader
.Starting point for first row set here - if this changes border value needs to change!!!!!!!
          clock   date to date
          unpack  date,MM,STR1,DD,STR1,YY
          pack    newdate1,MM,SLASH,DD,SLASH,CC,YY
          add     C1,page
          prtpage prfile;*UNITS=*HIENGLISH;
          move    "300",row
          prtpage prfile;*p7000:50,*font=font2,*uloff,"page ",page;
.START PATCH 1.1 REPLACED LOGIC
.         prtpage prfile;*p2700:row,*font=font10,"Names";
.         prtpage prfile;*font=font11,"  in the News";
.         add     eightlpi,row
.         add     eightlpi,row
.         add     eightlpi,row
.         prtpage prfile;*p1000:row,*pensize=10,*line=7100:row;
.         add     "60",row
.         prtpage prfile;*p2700:row,*font=font12,"C  A  L  I  F  O  R  N  I  A        I  N  C .";
..Go ahead and print the last line now
..Bullets produced using:  Alt+0149
.         prtpage prfile;*p1500:9950,*font=font13,"1300 Clay St., 11th Floor, Oakland, CA 94612-1429 • 415-989-3350 • Fax 415-433-7796";
.START PATCH 1.6 REPLACED LOGIC
          if (HOLDEXCL = "P")
                    prtpage   prfile;*p=1630:50,*font=font10,*Alignment=*Center,"Pacific Lists, Inc.":
                              *p=1630:425,*font=font7,"180 Grand Avenue, Suite 1365":
                              *p=1630:550,"Oakland, CA 94612-3716":
                              *p=1630:675,"415-945-9450 ","·"," Fax 415-945-9451":
                              *p=1630:800,"A Division of Names in the News":
                              *font=font2,*Alignment=*Left
          else
                    prtpage   prfile;*Pictrect=*off,*PICT=0:1300:500:8100:NINLogo
          endif
.END PATCH 1.6 REPLACED LOGIC
          add     eightlpi,row
          add     eightlpi,row
          add     eightlpi,row
          add     "60",row
          add     eightlpi,row
.END PATCH 1.1 REPLACED LOGIC
          add     eightlpi,row
          add     eightlpi,row
          add     eightlpi,row
          add     eightlpi,row
          prtpage prfile;*pcolumn:row,"TO:";
          prtpage prfile;*p800:row,OWNLONM;
          prtpage prfile;*p5500:row,"FROM:";
          prtpage prfile;*p6000:row,CNTNAME;
          add     eightlpi,row
          prtpage prfile;*p800:row,COMPHOLD;
          prtpage prfile;*p6000:row,CNTPHONE;
          add     eightlpi,row
          prtpage prfile;*p800:row,OWNLOSA;
.START PATCH 1.6 REPLACED LOGIC
.         prtpage prfile;*p6000:row,str24;
          if (HOLDEXCL = "P")
                    move      "NINCAL.COM",str10
                    move      "pacificlists.com",str25
                    call      ReplaceIt using intrnet,str10,str25
          endif
          prtpage prfile;*p6000:row,intrnet;
.END PATCH 1.6 REPLACED LOGIC
          add     eightlpi,row
          call    Trim using OWNLOCTY
          if (OWNLOCTY <> "")
                    pack    taskname,OWNLOCTY,COMMA,OWNLOS,B1,OWNLOZC
                    prtpage prfile;*p800:row,taskname;
          endif
          prtpage prfile;*p5500:row,"FAX:";
.START PATCH 1.6 REPLACED LOGIC
.         if (CNTTEAM = "05") .List Management
.                   prtpage prfile;*p6000:row,"(510) 628-8313";
.         else
.                   prtpage prfile;*p6000:row,"(415) 433-7796";
.         endif
          if (HOLDEXCL = "P")
                    prtpage prfile;*p6000:row,"(415) 945-9451";
          else
                    if (CNTTEAM = "05") .List Management
                              prtpage prfile;*p6000:row,"(510) 628-8313";
                    else
                              prtpage prfile;*p6000:row,"(415) 433-7796";
                    endif
          endif
.END PATCH 1.6 REPLACED LOGIC
          add     eightlpi,row
          call    Trim using OWNFAX
          if (OWNFAX <> "")
                    unpack  OWNFAX,str3,str2,str1,str4
                    pack    taskname,"(",str3,") ",str2,str1,"-",str4
                    prtpage prfile;*p800:row,taskname;
          endif
          add     eightlpi,row
          prtpage prfile;*p5500:row,"DATE:  ";
          prtpage prfile;*p6000:row,newdate1;
          add     eightlpi,row
          prtpage prfile;*p2700:row,*font=font1,*boldon,"Shipping Information Request",*boldoff;
          add       sixlpi,row
          add     eightlpi,row
.START PATCH 1.6 REPLACED LOGIC
.         if (CNTTEAM = "05") .List Management
.                   pack      str15,"(510) 628-8313"
.         else
.                   pack      str15,"(415) 433-7796"
.         endif
          if (HOLDEXCL = "P")
                    pack      str15,"(415) 945-9451"
          else
                    if (CNTTEAM = "05") .List Management
                              pack      str15,"(510) 628-8313"
                    else
                              pack      str15,"(415) 433-7796"
                    endif
          endif
.END PATCH 1.6 REPLACED LOGIC
          prtpage prfile;*pcolumn:row,*font=font13,"Please fax Shipping Information back to ",str15,".  If order has not been received, please contact us immediately.";
          add     eightlpi,row
          add     eightlpi,row
          return

ShipListHeader
.START PATCH 1.6 REPLACED LOGIC
.         prtpage prfile;*pcolumn:row,*font=font5,*boldon,"NINCA##";
          prtpage prfile;*pcolumn:row,*font=font5,*boldon,"LR##";
.END PATCH 1.6 REPLACED LOGIC
          prtpage prfile;*pcolumn1:row,"List/";
          prtpage prfile;*pcolumn5:row,"Rtn Date/";
          prtpage prfile;*pcolumn7:row,"Order Qty";
          add     eightlpi,row
          prtpage prfile;*pcolumn1:row,"Mailer";
          prtpage prfile;*pcolumn5:row,"Mail Date";
          add     sixlpi,row
          prtpage prfile;*pcolumn:row,*pensize=20,*line=7800:row;
          add     eightlpi,row
          return

ShipPrintRecord
.TEST FOR ENOUGH ROOM ON PAGE
          if (row >= 8372)        .Position of Largest Possible Last Record (would include 7 lines of Special Instructions)
                    prtpage prfile;*NEWPAGE;
                    call      ShipPrintHeader
                    call      ShipListHeader
          endif
          prtpage prfile;*pcolumn:row,*font=font2,*boldon,OLRN;
          prtpage prfile;*pcolumn1:row,*boldon,O1DES,*boldoff;
          call    TRIM using ORTNDTEM
          count   N2,ORTNDTEM
          if (N2 > 0 )
                    prtpage prfile;*pcolumn5:row,ORTNDTEM,SLASH,ORTNDTED,SLASH,ORTNDTEC,ORTNDTEY;
          endif
          if (SecFlag = 1)
                    add       "100",column6,N10
                    prtpage prfile;*pN10:row,*boldon,"2nd Request",*boldoff;
          endif
          move      OQTY,str9
          call      FormatNumeric using str9,str11
          prtpage prfile;*pcolumn7:row,str11;
          add     eightlpi,row
          prtpage prfile;*pcolumn1:row,MCOMP;
          call    TRIM using OMDTEM
          count   N2,OMDTEM
          if (N2 > 0 AND OMDTEM <> "00" AND OMDTEC <> "11")
                    prtpage prfile;*pcolumn5:row,OMDTEM,SLASH,OMDTED,SLASH,OMDTEC,OMDTEY;
          elseif (OMDTEM = "00" AND OMDTED = "00"  AND OMDTEC = "00" AND OMDTEY = "00")
                    prtpage prfile;*pcolumn5:row,"As Soon As Possible";
.Special Instructions are not printed on this form so skip next branch
.         elseif (OMDTEM = "11" AND OMDTED = "11"  AND OMDTEC = "11" AND OMDTEY = "11")
.                   prtpage prfile;*pcolumn5:row,"See Special Instructions";
          endif
          call    ShipPrintNotes
          add     eightlpi,row
          prtpage prfile;*pcolumn:row,"Ship Date";
          add       "250",column1,result
          prtpage prfile;*presult:row,*font=font3,"Actual";
          calc      N5=(row+90)                   .Bottom
          calc      N6=(result-125)               .Left
          calc      N7=(N6+90)                    .Right
        prtpage prfile;*pensize=10,*RECT=row:N5:N6:N7;                .top,bottom,left,right
          add       "120",row,N8
          prtpage prfile;*presult:N8,"Scheduled",*font=font2;
          calc      N5=(N8+90)                    .Bottom
          calc      N6=(result-125)               .Left
          calc      N7=(N6+90)                    .Right
        prtpage prfile;*pensize=10,*RECT=N8:N5:N6:N7;                 .top,bottom,left,right
          sub       "350",column2,howmany
          prtpage prfile;*phowmany:row,"Ship Qty";
          prtpage prfile;*pcolumn4:row,"Via";
          add       "400",column6,N10
          prtpage prfile;*pN10:row,"Tracking ##";
          add     sixlpi,row
          add     sixlpi,row
          add     eightlpi,row
          add       "1500",column,N9
          prtpage prfile;*pcolumn:row,*pensize=10,*line=N9:row;
          add       "1500",howmany,N9
          prtpage prfile;*phowmany:row,*pensize=10,*line=N9:row;
          add       "1500",column4,N9
          prtpage prfile;*pcolumn4:row,*pensize=10,*line=N9:row;
          add       "1500",N10,result
          prtpage prfile;*pN10:row,*pensize=10,*line=result:row;
          add     eightlpi,row
          add     tenlpi,row
          prtpage prfile;*pcolumn:row,"Order is Pending because:";
          add       "1500",column,N9
          add       "125",row,N7
          prtpage prfile;*pN9:N7,*pensize=10,*line=result:N7;
          add     eightlpi,row
          add     eightlpi,row
          prtpage prfile;*pcolumn:row,"List Manager Contact:";
          add       "1275",column,N9
          add       "1500",howmany
          add       "125",row,N7
          prtpage prfile;*pN9:N7,*pensize=10,*line=howmany:N7;
          prtpage prfile;*pcolumn4:row,"Phone ##/Email:";
          add       "875",column4,N9
          prtpage prfile;*pN9:N7,*pensize=10,*line=result:N7;
          add     eightlpi,row
          add     eightlpi,row
          add     eightlpi,row
          return

ShipPrintNotes
          call    Trim using SNOTES2B
          if (SNOTES2B <> "")
                    div     C2,eightlpi,N9
                    add     N9,row
                    pack    str2,newline,B1
                    rep     str2,SNOTES2B
                    move    C0,howmany
                    move    NO,str1
                    loop
                              call    PARSITUP using line1,SNOTES2B,C1
                              call    Trim using line1
                              if (line1 <> newline AND line1 <> "")
                                        move    YES,str1
                                        add     eightlpi,row
                                        prtpage prfile;*pcolumn:row,*font=font4,line1,*font=font2;
                              endif
                              add     C1,howmany
                              until   (howmany >= 7)
                    repeat
                    if (str1 = YES)
                              add     eightlpi,row
                    endif
          endif
          return

.Include IO file
          include nordio.inc
;Patch1.1
.         include nmlrio.inc
          include   compio.inc
          include   cntio.inc
;Patch1.1
          include ndatio.inc
          include nownio.inc
          include   ncntio.inc
          include   nshpio.inc
          include   nshp2io.inc
.START PATCH 1.4 REMOVED LOGIC          
.         include   nfulio.inc
.END PATCH 1.4 REMOVED LOGIC
          include comlogic.inc
