........................................
. Program:      NLCR0039.PLS
. Function:     In-House LCR HTML Galley Report Program
. Author:       David Herrick
. Orig. Date:   December 16 2015
. Release:      1.0
........................................

PC      EQU     1
.Include Files
          include common.inc
          include cons.inc
          include norddd.inc
          include   compdd.inc
          include   cntdd.inc
        include nxrfdd.inc
        include ndatdd.inc
        include nowndd.inc
        include npnddd.inc
        include nord5dd.inc
        include nspedd.inc
        include ncntdd.inc
        include nsmpdd.inc
        include nofrdd.inc
        include winapi.inc
          INCLUDE   NSEL2DD.INC


release   init    "1.00"     DLH  Write report in Email body instead of PDF- uses  printflag = 7 to set in Butil. 
REldate   Init      "2015 December 16"    
.
.
Samples    dim        1                     .if no samples in excel mode it holds "N"


str60     dim       60
.Counters
COUNTR  FORM    9
COUNTR2 FORM    9
.Files to open
prfile  pfile
input2  file
input2i ifile    Name="NINPRINT.isi|NINS1:502"
input3  file
.
Debugfile  file
.
First   init    "Y"
GoodStat init   "zlpx"
PrtFlag dim     1
HOLDOWN dim     4
HOLDLIST dim    6
Holdnsmpdes1          Dim        30
HoldOco2code          Dim        2
HOLDEXCL dim        1
COMPHOLD DIM    45
newdate1 dim    10
faxnum  dim     10
LONGDIST dim    1
page    form    9
date    dim     8
EditMask init   "ZZZ,ZZZ,ZZZ"
EditQuan dim    20
line1   dim     88
Carr    init    0x7f
DESC003 dim     725     .DESC002 + O2DES


FilePath dim    40


input2Name dim  40
output1 dim     40
output2 dim     80
userinfo dim    500
userlogn dim    7
userlogw dim    7
timestamp2 dim  16
EmailfLAG  iNIT      "N"
FaxfLAG  iNIT      "N"
testint INTEGER 4
PDFFlag   form      1
pict3   pict
mss1    plform  Error
font1   font
font2   font
font3   font
font4   font
font5   font
.....44
font6   font
....
Font7     font
        formload mss1
.Create fonts to be used
        create  font1,"Arial",size=12,bold
        create  font2,"Arial",size=9
        create  font3,"Helvetica",size=9
        create  font4,"Fixed",size=10
        create  font5,"Arial",size=11
......
        create  font6,"Arial",size=14
......
          create    font7,"Times New Roman",size=9
NINLogo   PICT
          CREATE    NINLogo=3:13:30:50:
                    "\\nins1\e\netutils\NIN logo black outline.jpg"



        match   "NLCR0039",PROGRAM   .case sensitive
        if not equal
                move    "NLCR0039",PROGRAM
                PACK    input2Name,NTWKPATH1,"NPRINT2.LCR"
                PACK    output1,NTWKPATH1,"LCRFILE.DAT"
                PACK    output2,NTWKPATH1,"LCRFILE.SRT"
                      move       c0,func                                                .force flag off                             
        else
                clear   input2Name
                append  NTWKPATH3,input2Name
.....................
                append  inpname,input2Name
                append  ".DAT",input2Name
                reset   input2Name
                move    user,userlogn
                call    Trim using userlogn
                clear   output1
                clear   output2
                clear   FilePath
                append  "\\nins1\d\USERS\",output1                           ."
                append  "\\nins1\d\USERS\",output2               ."    
                append  "\\nins1\d\USERS\",FilePath                          ."
.
                if (userlogn <> "")
                        append  userlogn,output1
                        append  "\",output1                           ."
                        append  userlogn,output2
                        append  "\",output2                           ."
                        append  userlogn,FilePath
                        append  "\",FilePath                          ."
.
                     DISPLAY *P1:24,*el,"Sorting";

                              clock     timestamp,timestamp
                              append    timestamp,output1
                              append    ".DAT",output1
                              append    timestamp,output2
                              append    ".SRT",output2

                     DISPLAY *P1:24,*el,"Processing";
                reset   output1
                reset   output2
                reset   FilePath
........................
        endif
        move    "NLCR0039",WPrognme
        clock   timestamp,timestamp
        unpack  timestamp,CC,YY,MM,DD

        pack    newdate1,MM,SLASH,DD,SLASH,CC,YY

.Find out system information
                    call                GetWinVer
        move    C0,NORDFLAG
        move    C0,NORDFLG2

                call    Paint

                DISPLAY *P1:24,"OPENING FILES";
.Open Files
        open    input2,input2Name
        PACK    STR35,"NINPRINT.ISI|NINS1:502"
        open    input2i,STR35
        prepare input3,output1
LCRLoop
        loop
                read    input2,seq;ordvars
                until over
                    if (OSTAT <> "l" OR OHIST <> "t")       .Prevent Tentative Approvals from appearing on report
                              call    OrderReadOtherFiles
                              call    OrderReadPendFiles
                              if (OSTAT <> "l" OR NORD5STAT <> "9")   .extra safey precaution to prevent Tentative Approvals from appearing on report
                                filepi  1;input3
                                        write   input3,seq;OLON,OLNUM,OMLRNUM,OLRN,MCOMP,O1DES,OWNLONM,OSTAT:
                                                  OWNOCPY,OWNLOSA,OWNLOCTY,OWNLOS,OWNLOZC,OWNFAX,OWNTELE,OwnEmail,NSEL2NAME,OFDESC:
                                                  OMDTEM,OMDTED,OMDTEC,OMDTEY,ORENT,OELCODE,OQTY,OCO2CODE:
                                                  OSCODE,OSAMCDE,NSMPDES1,NORD5STAT,DESC001,DESC002,CNTNAME,CNTPHONE,UNIVERSE,NPNDDESC,COMPNUM,HOLDEXCL:
                                                  oodtec,oodtey,oodted,oodtem
                              endif
                    endif
        repeat

                DISPLAY *P1:24,"SORTING FILE ";
.Sortfile sorting by list owner #,List #, lr #
SortFile
        clear   taskname
        append  output1,taskname
        append  COMMA,taskname
        append  output2,taskname
        append  ";",taskname
        append  "1-4,5-10,15-20",taskname
        reset   taskname
. avoid collision with user running mult jobs at the same time
DBLcheck
          FindFIle  output2
          move      C0,N1
          for N2,1,10
                    sort    taskname
                    if not over
                              move      C0,N1
                              break
                    else
                              move      C1,N1
                    endif
                    pause     "5"
        repeat
          if (N1 = C1)
                    move    s$error$,error
                    move    "Sort did not work!",Location
                    clear     KeyLocation
                    call    IOMssg
                    shutdown
          endif
                CALL    PAINT
                DISPLAY *P1:24,"PRINTING FILES        ";
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
.Initialize HOld
        clear   HOLDOWN
.close unsorted file
.
.
.open newly sorted file
          open      input3,output2,exclusive
          loop
printread
                    read    input3,seq;OLON,OLNUM,OMLRNUM,OLRN,MCOMP,O1DES,OWNLONM,OSTAT:
                              OWNOCPY,OWNLOSA,OWNLOCTY,OWNLOS,OWNLOZC,OWNFAX,OWNTELE,OwnEmail,NSEL2NAME,OFDESC:
                              OMDTEM,OMDTED,OMDTEC,OMDTEY,ORENT,OELCODE,OQTY,OCO2CODE:
                              OSCODE,OSAMCDE,NSMPDES1,NORD5STAT,DESC001,DESC002,CNTNAME,CNTPHONE,UNIVERSE,NPNDDESC,COMPNUM,HOLDEXCL:
                                 oodtec,oodtey,oodted,oodtem
                 DISPLAY *P10:10,"Reading MLR/LR## ",Omlrnum," ",olrn;
.                      call       debug

.                if (olon = "2006")
.                        call    debug
.                endif
                goto    LastRec if over
                if (OLON <> HOLDOWN | OLNUM <> HOLDLIST)
. NOTE:  If the logic goes back to breaking only on LO Number,
.then we would need to sort on Company association (PLI/NIN) as well as
.break on it.
                        if (FIRST = YES)
                                move    OWNOCPY,COMPHOLD
                                 move    OLON,HOLDOWN
                                 move    OLNUM,HOLDLIST
                                 move    OWNOCPY,COMPHOLD
                                move    NO,FIRST
                                 call       OrderPrintHeader
                                 call  OrderListHeader
                        Elseif (FIRST = NO)
                                call        emailuserhtml
                                 move    OLON,HOLDOWN
                                 move    OLNUM,HOLDLIST
                                 move    OWNOCPY,COMPHOLD
                                 Move     oco2code,HoldOco2code
                                 call       OrderPrintHeader
                                 call  OrderListHeader
                       endif 

                endif
.                call    OrderReadOtherFiles
                call    OrderPrintRecord
        repeat

LastRec
.
.This will only happen if callded from Program 1
.
           call        emailuserhtml
          Close     input3,delete
.dont forget to sendit

        shutdown


OrderBadSample
        move    "This is a Error e-mail from LCR Galley Report",MailSubjct
          Clear     Mailbody
          Append    "You have included a Bad Sample!!",Mailbody
          Append    "<br>",MailBody
          clear   taskname
        unpack  str9,str6,str3
        pack    taskname,"LR: ",OLRN,B1,"MAILER: ",str6,B1,"SAMPLE: ",str3
          append    taskname,Mailbody
          Append    "<br>",MailBody
          reset     Mailbody
          MOVe      "ComputerRequest@nincal.com",Mailfrom
          pack      Mailto from Mailfrom,",",user,"@nincal.com"
.
            move      c1,MailType         .force e-mail body to HTML message
.
............................                


          call      SendMail
        return


OrderReadOtherFiles
.Open other files to retrieve appropriate information
          move    OLNUM,NDATFLD
          move    C1,NDATPATH
          move    "Driver-NDATKEY,1rst",Location
          call    NDATKEY
          if over
                    clear     HOLDEXCL
          else
                    move      ELSTCDE,HOLDEXCL
          endif
          //.Clean up afterwards
          move    C0,UNIVERSE
.Mailer File
        rep     zfill,OMLRNUM
        pack    MKEY,OMLRNUM,"000"      .Master Record
        move    C3,NMLRLOCK
        move    "Driver-NMLRKEY,1rst",Location
        call    NMLRKEY
        if over
                pack   MCOMP,"UNKNOWN",B55
        endif
.Owner File
        move    OLON,NOWNFLD
        rep     zfill,NOWNFLD
        move    "Driver-NOWNKEY,1rst",Location
        call    NOWNKEY
        if over

        endif
.Special Instructions
        move    OLRN,NSPEFLD
        rep     zfill,NSPEFLD
        move    C3,NSPELOCK
        move    "Driver-NSPEKEY,1rst",Location
        call    NSPEKEY
        call    Trim Using DESC001
        pack    NCNTFLD,OCO2CODE
        move    "NCNTKEY",Location
        pack    KeyLocation,"Key: ",NCNTFLD
          Move      C1,NCntPAth
        call    NCNTKEY
.Data Card Universe
        clear   UNIVERSE
          pack      COMPFLD3,OMLRNUM
          move      "COMPKEY3",Location
          pack      KeyLocation,"Key: ",COMPFLD3
          call      COMPKEY3
        move    COMPNUM,NXRFFLD2
        rep     zfill,NXRFFLD2
        move    C2,NXRFPATH
        move    "Driver-NXRFKEY",Location
        call    NXRFKEY
        if not over
                move    NXRFLIST,NDATFLD
                move    C0,UNIVERSE
                move    C1,NDATPATH
                move    "Driver-NDATKEY",Location
                call    NDATKEY
                if over
                       move     C0,UNIVERSE
                endif
        endif
.Sample File
          move      "Driver-COMPKEY3",Location
          pack      COMPFLD3,OMLRNUM
          pack      KeyLocation,"Key: ",COMPFLD3
          call      COMPKEY3
          if over
                    clear     COMPNUM
          endif
          pack      NSMPFLD,COMPNUM,OSAMCDE

        move    "Driver-NSMPKEY",Location
        clear   NSMPDES1
        call    NSMPKEY
        if over
                pack    NSMPDES1,"Sample Not Found!"
        else
                if (NSMPDES1 = "")
                pack    NSMPDES1,"Description Not Found"
                endif
        endif
.Offer file
        bump    OODNUM,4
        pack    NOFRFLD,OMLRNUM,OODNUM
        rep     zfill in NOFRFLD
        move    "O.LoadOffer-NOFRKEY",Location
        call    NOFRKEY
        if over
                clear   OFDESC
        endif
          packkey   NSEL2FLD,"1",OLRN
          move      "NSEL2KEY",Location
          pack      KeyLocation,"Key: ",NSEL2FLD
          call      NSEL2KEY
          if over
                    move      O2DES,NSEL2NAME
          endif
        return

OrderReadPendFiles
.NINORD5 File
        move    OLRN,NORD5FLD
        rep     zfill,NORD5FLD
        call    NORD5KEY
        if over
                move    "No Status Found!",NPNDDESC
        else
.NINPND File
                pack    NPNDFLD,OSTAT,NORD5STAT
                rep     zfill,NPNDFLD
                move    "Driver-NPDNKEY",Location
                call    NPNDKEY
                if over
                        move    "No Status Found!",NPNDDESC
                endif
        endif
        return

.Print Heading
OrderPrintHeader
        clock   date to date
        unpack  date,MM,STR1,DD,STR1,YY
        pack    newdate1,MM,SLASH,DD,SLASH,CC,YY
........................................
          clear     str60
          call      Trim using CNTNAME
          if (CNTNAME <> "" & CNTNAME <> "BILLING")
                    call      RemoveChar using CNTNAME,B1
                              pack      str60,CNTNAME,"@nincal.com"
          endif

           Clear      Mailbody
           append     "<!DOCTYPE HTML PUBLIC #"-//W3C//DTD HTML 4.01 Transitional//EN#">",Mailbody   
           append     "<html>",Mailbody
           append     "<head>",Mailbody
.           append     "<meta http-equiv=#"content-type#" content=#"text/html; charset=UTF-8#">",Mailbody         
.           append     "<meta http-equiv=#"Content-Type: text/html; charset=ISO-8859-1 Content-Transfer-Encoding: base64#">",Mailbody         
           append     "</head>",Mailbody
           append     "<body>",Mailbody
           append     "<br><br>",mailbody

           pack       NCNTFLD,OCO2CODE
           move       "NCNTKEY",Location
           pack       KeyLocation,"Key: ",NCNTFLD
           move       c1,NCntPath
           call       NCNTKEY
           endif
        return

OrderListHeader
           append     "<br><h3>",mailbody
           append     "List: ",mailbody
           append     O1DES,mailbody
           append     "</h3><br>",mailbody
           append     "<br>",mailbody
          return

OrderPrintRecord
.detail table 4 sections
           append     "<table width=#"100%#" border=#"1#" cellpadding=#"2#" cellspacing=#"2#" table-layout=fixed>",mailbody
           append     "<tbody>",mailbody
           append     "<tr>",mailbody
           append     "<td width=#"25%#" valign=#"top#"><b>NIN##: ",mailbody
           append     OLRN,mailbody
           append     "</b></td>",mailbody
           append     "<td width=#"25%#" valign=#"top#"><b>Status: ",mailbody      

           if         (OSTAT = "l" | OSTAT = "z")
                append     NPNDDESC,mailbody                           
           else
               append     "Pending Order",mailbody                           
           endif
           
           append     "</b></td>",mailbody
           append     "<td width=#"25%#" valign=#"top#"><b>Mail Date: ",mailbody
           call       TRIM using OMDTEM
           count      N2,OMDTEM
           if         (N2 > 0 AND OMDTEM <> "00" AND OMDTEC <> "11")
                      pack       str10 from OMDTEM,SLASH,OMDTED,SLASH,OMDTEC,OMDTEY
                      append     str10,mailbody
           elseif     (OMDTEM = "00" AND OMDTED = "00"  AND OMDTEC = "00" AND OMDTEY = "00")
                      append     "As Soon As Possible",mailbody
           elseif     (OMDTEM = "11" AND OMDTED = "11"  AND OMDTEC = "11" AND OMDTEY = "11")
                      append     "See Special Instructions",mailbody
           endif
           append     "</b></td>",mailbody
           append     "<td width=#"25%#" valign=#"top#"><b>Answer: ",mailbody      
           append     "<div style=#"width:30px;height:10px;border:1px solid ##000;#">    </div>",mailbody
           append     "</b></td>",mailbody
           append     "</tr>",mailbody
           append     "<tr>",mailbody
           append     "<td valign=#"top#"><b>Mailer: ",mailbody
           call       trim using mcomp
           append     MCOMP,mailbody
           append     "</b></td>",mailbody
           append     "<td valign=#"top#"><b>Offer: ",mailbody
           call       trim using ofdesc
           append     OFDESC,mailbody
           append     "</b></td>",mailbody
           append     "<td valign=#"top#"><b>Qty: ",mailbody

           move    OQTY,N9
           if         (OELCODE = "1" OR OELCODE = "3")
                      if         (N9 > 0)
                                 move    EditMask,str11
                                 edit    N9,str11
                                 call      Trim using str11
                                 pack      EditQuan,str11,"/ALL"
                      else
                                 move    "All",EditQuan
                      endif
           else
                      move    EditMask,EditQuan
                      edit    N9,EditQuan
           endif
           append     EditQuan,mailbody
           append     "</b></td>",mailbody
           append     "<td valign=#"top#"><b>Sample: ",mailbody
.check do we have comp and sample data>???
.make sure we still have valid company ##  DH 2015 Dec 14
           rep     zfill,OMLRNUM
           pack    MKEY,OMLRNUM,"000"      .Master Record
           move    C3,NMLRLOCK
           move    "Driver-NMLRKEY,1rst",Location
           call    NMLRKEY

           move      "SAM1-COMPKEY",Location
           pack      COMPFLD,compnum
           pack      KeyLocation,"Key: ",COMPFLD
           call      COMPKEY
           packkey   NSMPFLD,COMPNUM,OSAMCDE
           rep       zfill,NSMPFLD
           move      "SAM1-NSMPKEY",Location
           pack      KeyLocation,"Key: ",NSMPFLD
           call      NSMPKEY
.Begin test dh 2015 july 30
           if        Not Over                          .triple check
           call   trim using compcomp
           call   trim using nsmpdes1
           clear   taskname
           pack    Taskname from "<a href=#"http://www.nincal.com/data/samples/S",Compnum,osamcde,".pdf","#">",nsmpdes1,"</a>"
           append  Taskname,MailBody
           endif
.end test dh 2015 july 30

.           call       debug
           append     "</b> </td>",mailbody
           append     "</tr>",mailbody
           append     "</tbody>",mailbody
           append     "</table> <table width=#"100%#" border=#"1#" cellpadding=#"2#" cellspacing=#"2#" table-layout=fixed>",mailbody
           append     "<tbody>",mailbody
           append     "<tr>",mailbody
           append     "<td width=#"33%#" valign=#"top#"><b>Reco: ",mailbody

           clear   str9
           if         (ORENT = "1")
                                 if         (OELCODE = "2" OR OELCODE = "3")
                                            append  "RENT/EXC",str9
                                 else
                                            append  "RENTAL",str9
                                 endif
           else
                      append  "EXCHANGE",str9
           endif
           reset      str9
           append     str9,mailbody
           append     "</b></td>",mailbody
           append     "<td width=#"33%#" valign=#"top#"><b> ",mailbody
           append     Desc001,mailbody
           append     "</b></td>",mailbody
           append     "<td width=#"33%#" valign=#"top#"><b> ",mailbody
          move    C0,N9
          move    OQTY,N9
          if (OELCODE = "1" OR OELCODE = "3")
                    if (N9 > 0)
                              move    EditMask,str11
                              edit    N9,str11
                              call      Trim using str11
                              pack      EditQuan,str11,"/ALL"
                    else
                              move    "All",EditQuan
                    endif
          else
                    move    EditMask,EditQuan
                    edit    N9,EditQuan
          endif
        clear   str9
        if (ORENT = "1")
                if (OELCODE = "2" OR OELCODE = "3")
                        append  "RENT/EXC",str9
                else
                        append  "RENTAL",str9
                endif
        else
                append  "EXCHANGE",str9
        endif
        reset   str9
................
..Note:  The Box holding "Answer" is printed after the XSTAT
..       This is done so that complete box is printed.  Following vars must, therefore, must remain.
        call    PrintSpecialInstructions

        append     " </b> </td>",mailbody

        return

PrintSpecialInstructions
        div     C2,eightlpi,N9
        add     N9,row
        add     eightlpi,row

        call    TRIM using NSEL2NAME
        if (NSEL2NAME <> "")
                pack    DESC003,"Select:  ",NSEL2NAME,".  ",DESC002
        else
                pack    DESC003,DESC002
        endif
        call    TRIM using DESC003
        if (DESC003 <> "")
                pack    str2,carr,B1
                rep     str2,DESC003
                move    C0,howmany
                move    NO,str1
                loop
                        call    PARSITUP using line1,DESC003,C1
                        call    Trim using line1
                        if (line1 <> carr AND line1 <> "")
                                move    YES,str1
                                append      Line1,mailbody
                                append      b1,mailbody
                        endif
                        add     C1,howmany
                        until   (howmany >= 7)
                repeat
        endif
        add     N9,row
           append     "</tr>",mailbody
           append     "</tbody>",mailbody
           append     "</table>",mailbody
           append   "<br>",mailbody
           append   "<br>",mailbody
        return


EmailUserHTML
.
                    
          pack      Mailto,userlogn,"@nincal.com"
          pack      Mailfrom,userlogn,"@nincal.com"
          move      "Here is your Galley Report ",Mailsubjct
          append     "</body> </html>",mailbody
          reset     Mailbody
          pack      mailbcc,"ComputerRequest@nincal.com"
          
.
            move      c1,MailType         .force e-mail body to HTML message
.
.code for debuging mailbody issues
                move    user,userlogn
                call    Trim using userlogn
.
                if (userlogn <> "")
                pack  taskname from "c:\work\",userlogn,timestamp,".dat"      
                call  trim using taskname
                prepare  debugfile,taskname
                write debugfile,seq;*ll,mailbody
                weof   debugfile,seq
                close debugfile
                endif

          call      SendMail
.
           Clear      Mailbody

          return
.end patch 3.20





          
......................................................
getExstat
           bump       desc001,5
           MOVEFPTR   desc001,n8
bump1
           bump       desc001,1
           cmatch     b1,desc001
           goto       bump1 if not equal
           MOVEFPTR   desc001,n9
           sub        c1 from n9
           reset      desc001,n8
           SETLPTR    desc001,n9
           MOve       desc001,str20
           return
......................................................


.Include IO file
        include nordio.inc
                              include   compio.inc
                              include   cntio.inc
        include nxrfio.inc
        include ndatio.inc
        include nownio.inc
        include npndio.inc
        include nord5io.inc
        include nspeio.inc
        include nsmpio.inc
        include nofrio.inc
        include ncntio.inc
          INCLUDE   NSEL2IO.INC
        include comlogic.inc