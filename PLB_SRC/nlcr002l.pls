PC        EQU       0

          INCLUDE   COMMON.INC
          INCLUDE   CONS.INC
          INCLUDE   NORDDD.INC
          INCLUDE   COMPDD.inc
          INCLUDE   CNTDD.inc
          INCLUDE   NCNTDD.INC
          INCLUDE   NOWNDD.INC
          include   hp.inc
          INCLUDE   NUSEDD.INC
          include   nspedd.inc
          include   nmdldd.inc
          INCLUDE   NOFRDD.INC
          include   nord5dd.inc
          INCLUDE   NSEL2DD.INC
        include winapi.inc
         INCLUDE   nsmpdd.inc
           include    npnddd.inc
Release   Init    "2.92"     DLH  HTML cleanup
REldate   Init      "2016 February 24"    
.release   init    "2.91"     DLH  break every record, print sample description on form
.REldate   Init      "2015 December 3"    
.see release 2.90 for previous code
.RELEASE   INIT      "1.0"         1999 AUG 08  DLH.
.begin patch 2.92
HexCRLF   INIT 0x0D, 0x0A
.end patch 2.92

.EXTERNAL ROUTINES FROM       NORDTEST.PLC
.this can probably go away
CleanUpLCRFaxFiles external "NORDTEST;CleanUpLCRFaxFiles"

. *****************************************************************************
. **** LIST  CLEARANCE  PRINT  PROGRAM  08/18/99
. *****************************************************************************
.
line1     dim       55              .Used for Printing Special Instructions
M01       INIT      "January"
M02       INIT      "February"
M03       INIT      "March"
M04       INIT      "April"
M05       INIT      "May"
M06       INIT      "June"
M07       INIT      "July"
M08       INIT      "August"
M09       INIT      "September"
M010      INIT      "October"
M011      INIT      "November"
M012      INIT      "December"
REPLN1    DIM       30
QTYMASK   INIT      "ZZZ,ZZZ,ZZ9"
QTYPRNT   DIM       11
QTYPRNT2 DIM        45
smpflag   form      1         *1=there is a file
.
. FILE DEFINITIONS
.
ORDPRINT FILE       FIX=696   DAILY PRINT FILE. 434 BYTES (INCLUDES 00/99/98 TEXT)
pict1     pict
prfile    pfile
FMESG     DIM       22
STATUS    DIM       15
newdate1 DIM        10
bigdate   dim       25
owncnt    form      3         .counts number of lcr's for a particular owner.
ownscnt   form      3         .counts number of samples for a particular owner.
owncnta   form      3         .for cover sheet
ownscnta form       3         .for cover sheet
count     form      4
EXT       DIM       3
ARCD      DIM       3
PHONE     DIM       4
faxsal    dim       5         fax salutation
faxname   dim       25
faxattn   dim       25
faxflag   form      1         .1=no, 2=yes.
LPTCNT    FORM      4         .LENGTH OF ATTCHLST
LONGDIST DIM        1
DCX       INIT      ".PDF"
intrnet   dim       50        .print contact's internet address
DCX2      dim       30
DCXFile   dim       120
SPOOLF    dim       120
FilePath DIM        45
          PACK      FILEPATH,NTWKPATH1,"SAMPLES\"                .
." just here to fix display of vars :)  
SMPArray dim        12(50)
SMPIndex form       2
EmailFlag DIm       1
FileCheck FIle
trapcount form      4
EmailAddr Dim       100
dmFileName          dim 80
StoredOLRN dim      1000
.Begin patch 2.91
SAMPLE2   DIM       26        *USED FOR ORDER PRINT
MlrNameHold         dim       75
MlrMNameHold        dim       75

.substituting report at faxform generates and Object error at load time under 8.6
rptcan    dim       1
ProgFlag form       "0"
DimPtr    dim       ^
FrmPtr    form      ^
FrmPtr1   form      ^
font1   font
font2   font
font3   font
font4   font
font5   font
font6   font
FontO7              font
FontO18B  font
NINLogo   PICT
EditMask init   "ZZZ,ZZZ,ZZZ"
EditQuan dim    20
Carr    init    0x7f
DESC003 dim     725     .DESC002 + O2DES

x         plform    report
        formload X
...........................................................
.
          move      "NLCR002L",PROGRAM
          move      "Names in the News",COMPNME
          move      "Produce LCR's ",STITLE
          move      "EXIT",PF5
          clock     DATE,TODAY
          call      PAINT
          call      FUNCDISP
          trap      NOFILE IF IO
          trap      EXIT IF F5


          display   *P1:24,*el,"OPENING FILES";
          move      "NIN PRINT FILE       ",FMESG
.testing
          pack      STR35,NTWKPATH1,"lcrprint.lcr"
.          pack      STR35,NTWKPATH1,"diskin05.dat"
          open      ordprint,STR35
          display   *EL,*P01:24;
          trapclr   IO
.
READ1
.Initialize a bunch of flags
          move      C0,count
          move      C0,owncnt
          move      C0,ownscnt
          move      C0,FaxFlag
          loop
                    read      ORDPRINT,SEQ;ORDVARS
                    until over
                    if (OHIST <> "t" & OHIST <> "E")
                    add       C1,count
                    display   *p10:12,count,b1,OLRN,b1,OLON;
                    call      Trim using OLRN
                    call      Trim using OLON
                    pack      MKEY,OMLRNUM,z3
                    move      "Read1-NMLRKEY",Location
                    call      NMLRKEY
                               if           (OSTAT = "z")
                               move      "** CANCELLED **",STATUS
                              else
                               move      "               ",STATUS
                               endif
                    call      Print
                    call       sendit
                    endif
          repeat
          display   *P1:24,*el,"Done, Bye",*w4;

          shutdown

Print
          move      OODTEM,mm
          move      OODTED,dd
          move      OODTEC,cc
          move      OODTEY,yy
          move      MM,N2
          load      STR9 USING N2 FROM M01,m02,m03,m04,m05,m06:
                    m07,m08,m09,m010,m011,m012
          move      DD,STR2
          reset     STR2,1
          setlptr   STR2,1
          rep       "0 ",STR2
          setlptr   STR2
          clear     bigDATE
          append    STR9,bigDATE
          append    B1,bigDATE
          append    STR2,bigDATE
          append    B1,bigDATE
          append    ",",bigDATE
          append    cc,bigDATE
          append    YY,bigDATE
          reset     bigDATE
          move      QTYMASK,QTYPRNT
          move      C0,result
          move      OQTY,result
          if (OELCODE = "1" OR OELCODE = "3")
                    if (result > 0 & (OCO2CODE = "" | OCO2CODE = "  "))
                              edit      result,QTYPRNT
                              call      Trim using QTYPRNT
                              pack      QTYPRNT2,"ALL/ ",QTYPRNT," Please advise actual quantity."
                    else
                              move      "    ALL",QTYPRNT2
                    endif
          else
                    edit      result,QTYPRNT
                    move      QTYPRNT,QTYPRNT2
          endif
          move      OLNUM,NMDLFLD
          rep       zfill,NMDLFLD
          clear     mdlcall
          move      "Print-NMDLKEY",Location
          pack      KeyLocation,"Key: ",NMDLFLD
          CALL      NMDLKEY
..............
. DETERMINE  REP NAME.
.
SVCREP
          clear     intrnet
          pack      REPLN1,B10,B10,B10
          pack      NCNTFLD,OCOCODE
          move      "SVCREP-NCNTKEY",Location
          pack      KeyLocation,"Key: ",NCNTFLD
          call      NCNTKEY
          if not over
SVCREP2
                    move      "NIN Contact,",REPLN1
                    move      CNTNAME,str55
                    scan      "BILLING",str55
                    if not equal
                              call      RemoveChar using str55,B1
                              call      Trim using str55
                              if (str55 <> "")
                                        pack      intrnet,str55,"@nincal.com"
                              endif
                    endif
          endif
          if (ProgFlag = 1)   .Single LCR Report called from other Program
                    move      OLON,NOWNFLD
                    rep       zfill,NOWNFLD
                    call      NOWNKEY
                    pack      MKEY,OMLRNUM,z3
                    move      "SVCREP2-NMLRKEY",Location
                    call      NMLRKEY
                    call      DISNIN
                    return
          endif
          call      Process1
          return


sendit
          scan      "@",emailaddr         
          if        not equal
          Move      intrnet,Mailto
          else
          reset     Emailaddr  
          move      emailaddr to mailto
          endif
          Move      intrnet,Mailfrom          

          MOve      "ComputerRequest@nincal.com",MailBCC                      .keep copy just incase .. rule on that inbox to move
                    Move      "30",MailTimer
                    pause     "5"
                    move      c1,MailType         .force e-mail body to HTML message
.............testing  
.                    move         "davidherrick@nincal.com",mailto
.                    clear        mailbcc
.............testing  

                    call      SendMail
          return


..........................................................................................................................



Process1
          move      OLON,NOWNFLD
          rep       zfill in NOWNFLD
          move      "PrepOpen-NOWNKEY",Location
          pack      KeyLocation,"Key: ",NOWNFLD
          call      NOWNKEY             .make sure we have last owners info
          if        not over 
.lets check for email first
          Clear      Emailaddr
          call      Trim using OwnEmail
                    if        (Ownemail <> "")               .we have something
                              scan      "@",Ownemail
                              if        equal                         .presuming valid
                                       reset     Ownemail
                                       
                                       move      Ownemail,Emailaddr
                                       move      c2,Faxflag                    .send it
.
                                       move       Yes,EmailFlag              .used for cover sheet
.
                              endif
                                        move      c2,faxflag        .do fax.

                    Goto      ContOwn                  
                    Endif
.did not have email check for fax
                    count     N2,OWNFAX
                    compare   C10,N2
                              if equal
                                        move      C1,LONGDIST
                                        unpack    OWNFAX,STR3,STR7
                                        match     "510",STR3           .LOCAL ?
                                        if equal
                                                  move      STR7,OWNFAX
                                                  clear     LONGDIST
                                        else
                                                  match     B3,STR3           .LOCAL ?
                                                  if equal
                                                            move      STR7,OWNFAX
                                                            clear     LONGDIST
                                                  endif
                                        endif
                              pack      EmailAddr from "+",longdist,OWNFAX,"@fax.nincal.com"
                              move       No,EmailFlag              .used for cover sheet
                              endif
                    type      OWNFAX                             .valid fax #?
                              if not equal
.printing not faxing - no or invalid fax number
                                          move      b55,emailaddr  
                                          clear     emailaddr   
                                        move      c1,faxflag        .
                              elseif (OWNFAX = "0000000000")
                                          move      b55,emailaddr  
                                          clear     emailaddr   
                                        move      c1,faxflag
                              else
                                        move      c2,faxflag        .do fax.
                              endif
          else
.email back to sender no email or fax
                    display   *p1:24,*el,"invalid owner number"
                    move      c1,faxflag        .
                    move      b55,emailaddr  
                    clear     emailaddr   
          endif
ContOwn


          pack      MKEY,OMLRNUM,z3
          move      "process1-NMLRKEY",Location
          call      NMLRKEY
.
DISNIN
          move      OLRN,NSPEFLD
          move      "DISNIN-NSPEKEY",Location
          call      NSPEKEY
.Rental or Exchange
          clear     str18
          if (ORENT = "1")
                    if (OELCODE = "2" OR OELCODE = "3")
                              append    "RENT/EXC",str18
                    else
                              append    "RENTAL",str18
                    endif
          else
                    append    "EXCHANGE",str18
          endif
          reset     str18
.Offer
          reset     OODNUM
          bump      OODNUM,4
          move      OODNUM,str4
          pack      NOFRFLD,OMLRNUM,str4
          rep       zfill,NOFRFLD
          move      "O.LoadOffer-NOFRKEY",Location
          call      NOFRKEY
.LCR Sub-Status
.
          clear     ARCD
          clear     EXT
          clear     PHONE
          clear     FAXSAL
          unpack    ownfax,ARCD,EXT,PHONE
           move      "Here is your List Clearance Request.",MailSubjct
           pack       MailSubjct using "REQUEST FOR LIST APPROVAL ## ",olrn    
           Clear      Mailbody
.begin patch 2.92
           append     "<!DOCTYPE html PUBLIC #"-//W3C//DTD HTML 4.01 Transitional//EN#"#"http://www.w3.org/TR/html4/loose.dtd#">",Mailbody   
           append     HexCRLF,mailbody
           append     "<html><Head>",Mailbody
           append     "<meta name=#"generator#" content=#"HTML Tidy for Linux (vers 25 March 2009), see www.w3.org#">",mailbody
.           append     "<!DOCTYPE HTML PUBLIC #"-//W3C//DTD HTML 4.01 Transitional//EN#">",Mailbody   ."
.           append     "<html>",Mailbody
.           append     "<head>",Mailbody
..           append     "<meta http-equiv=#"content-type#" content=#"text/html; charset=UTF-8#">",Mailbody         
.end patch 2.92
           append     "<title>REQUEST FOR LIST APPROVAL</title>",Mailbody
           append     "</head>",Mailbody
           append     "<body>",Mailbody
           append     "<h3 align=#"center#">REQUEST FOR LIST APPROVAL</h3>",Mailbody
           append     "<br>",mailbody
           append     OWNlONM,mailbody
           append     "<br>",mailbody
           append     OWNOCPY,mailbody
           append     "<br>",mailbody
           append     "<h4>",mailbody
           append     "List: ",mailbody
           append     O1DES,mailbody
           append     "</h4><br>",mailbody
.begin patch 2.92
           append     HexCRLF,mailbody
.end patch 2.92
          packkey   NSEL2FLD,"1",OLRN
          move      "NSEL2KEY",Location
          pack      KeyLocation,"Key: ",NSEL2FLD
          call      NSEL2KEY
          if over
                    move      O2DES,NSEL2NAME
          endif
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
          
           append     "<table width=#"100%#" border=#"1#" cellpadding=#"2#" cellspacing=#"2#" >",mailbody
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
           append     "<td width=#"25%#" valign=#"top#"><b>",mailbody      
           append     BigDate,Mailbody
           append     "<div style=#"width:30px;height:10px;border:1px solid ##000;#">    </div>",mailbody
           append     "</b></td>",mailbody
           append     "</tr>",mailbody
           append     "<tr>",mailbody
           append     "<td valign=#"top#"><b>Mailer: ",mailbody
           append     MCOMP,mailbody
           append     "</b></td>",mailbody
           append     "<td valign=#"top#"><b>Offer: ",mailbody
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
           move      "SAM1-COMPKEY",Location
           pack      COMPFLD,compnum
           pack      KeyLocation,"Key: ",COMPFLD
           call      COMPKEY
           pack      NSMPFLD,COMPNUM,OSAMCDE
           rep       zfill,NSMPFLD
           move      "SAM1-NSMPKEY",Location
           pack      KeyLocation,"Key: ",NSMPFLD
           if         (oscode = "1")                   .sample enclosed
           call      NSMPKEY
                      if        Not Over                          .triple check
                      call   trim using compcomp
                      call   trim using nsmpdes1
                      clear   taskname
                      pack    Taskname from "<a href=#"http://www.nincal.com/data/samples/S",NSmpfld,".pdf","#">",nsmpdes1,"</a>"
                      append  Taskname,MailBody
                      endif           
           elseif     (oscode = "2")                              
                      append     "Sample to Follow.",mailbody
           elseif     (oscode = "3")                              
                      append     "Sample previously approved.",mailbody
           endif
           append     "</b></td>",mailbody
           append     "</tr>",mailbody
           append     "</tbody>",mailbody
           append     "</table>",mailbody
.begin patch 2.92
           append     HexCRLF,mailbody
.end patch 2.92
           append     "<td width=#"33%#" valign=#"top#"><b> ",mailbody
           append     "</b></td>",mailbody
           append     "<td width=#"33%#" valign=#"top#"><b> ",mailbody
           append     "</b></td>",mailbody
           append     "</tr>",mailbody
           append     "</tbody>",mailbody
           append     "</table>",mailbody

           append     "<table width=#"100%#" border=#"1#" cellpadding=#"2#" cellspacing=#"2#" >",mailbody
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
.begin patch 2.92
           append     HexCRLF,mailbody
.end patch 2.92
           append   "<br>",mailbody

           append     "<h4 align=#"center#">CLEARANCE APPROVAL</h43>",Mailbody
           append     "<br><br>",mailbody
           append     "<U>Please provide count for segment being cleared.</U>",mailbody
           append     "<p style=#"text-align:center#">Please fill in below and reply to this email or fax to (510)302-4632.",mailbody
           append     "<br><br>",mailbody
.begin patch 2.92
           append     HexCRLF,mailbody
.end patch 2.92
           append     "____Approved                    ____Rental            ____Exchange",mailbody
           append     "<br><br>",mailbody
           append     "____ Not Approved and Reason: ______________________________________",mailbody
           append     "<br><br>",mailbody
           append     "Signature: ________________________________________________________ </p>",mailbody
           append     "<br>",mailbody
           append     "<p style=#"text-align:Left#">Thank you,",mailbody
           append     "<br>",mailbody
           append     cntname,mailbody
           append     "<br>",mailbody
           append     cntphone,mailbody
           append     "<br>",mailbody
           append     intrnet,mailbody           
           append     "</p><br>",mailbody

           reset      mailbody
                  
.
          if (ProgFlag = 1)
                    return
          endif
          DISPLAY   *P01:24,*EL
WIPEVARS
          CLEAR     OMLRNUM
          CLEAR     OCOBN
          CLEAR     OLNUM
          CLEAR     O2DES
          CLEAR     OELCODE
          CLEAR     OQTY
          CLEAR     OODTEM
          CLEAR     OODTED
          CLEAR     OODTEC
          CLEAR     OODTEY
          CLEAR     OMDTEM
          CLEAR     OMDTED
          CLEAR     OMDTEC
          CLEAR     OMDTEY
          CLEAR     OSAMCDE
          clear     OODNUM
          clear     O1DES
          clear     DESC001
          clear     DESC002
          return

......................................
NOFILE
          DISPLAY   *P12:20,FMESG,"NOT ON-LINE ",error;
          TRAPCLR   IO
          KEYIN     *P12:21,str1;
          CMATCH    "Q",str1
          GOTO CONTACT IF EQUAL
          GOTO NOFILE
.
CONTACT
          KEYIN     *P1:23,"PLEASE LEAVE THIS INFORMATION ON THE SCREEN ":
                    "AND INFORM THE COMPUTER PERSONEL. ",str1;
.
          CMATCH    "Q",str1
          GOTO CONTACT IF EOS
          GOTO CONTACT IF NOT EQUAL
          SHUTDOWN  "CLS"
          STOP
DREWBREAK
          NORETURN
          TRAP      zero giving error if io
          SCAN      "I03" IN ERROR
          GOTO ZERO
ZERO
          DISPLAY   *P1:24,"UNDEFINED FILE ERROR",*W2;
          SHUTDOWN "CLS"
.
EXIT
          DISPLAY   *P1:24,*EL,*HON,"Exit Subroutine. SHUTTING DOWN TO CONTINUE":
                    " CHAIN",*W8;
          SHUTDOWN "CLS"

PrintSingleLCR Routine DimPtr,FrmPtr,FrmPtr1
.DimPtr  = LCR Number
.FrmPtr  = Printer
.FrmPtr1 = PORTN
          move      FrmPtr1,PORTN
          move      C1,NORDPATH
          move      C1,ProgFlag
          call      Trim using DimPtr
          if (DimPtr <> "")
                    pack      NORDFLD,DimPtr
                    move      "NORDKEY",Location
                    pack      KeyLocation,"Key: ",NORDFLD
                    call      NORDKEY
                    if not over
                              call      Print
                                                  move      C0,NUSEFLD
                                                  move      C1,NUSEPATH
                                                  move      PORTN,NUSEFLD
                                                  rep       zfill,NUSEFLD
                                                  call      NUSEKEY
                                                  call      Trim using NUSEUSER
                                                  scan      "BILLING",NUSEUSER
                                                  if not equal
                                                            move      NUSEUSER,str1
                                                            loop
                                                                      bump      NUSEUSER,1
                                                                      cmatch    B1,NUSEUSER
                                                                      until equal
                                                                      until eos
                                                            repeat
                                                            if not eos
                                                                      bump      NUSEUSER,1
                                                                      move      NUSEUSER,str6
                                                                      clear     str7
                                                                      pack      str7,str1,str6
                                                            endif
                                                  endif
.
........................................................
.Mail it back to them
                                        pack      str45,str7,"@nincal.com"
.
.
                                                  move      "Here is your List Clearance Request.",MailSubjct
.                                                  pack      MailBOdy,taskname
                                                  reset     Mailbody           
                                                  move      str45,MailFrom
                                                  move      str45,MailTO                                                .User name
                                                  move      c1,MailType         .The e-mail body is a HTML message 
.............testing  
.                    move         "davidherrick@nincal.com",mailto
.                    clear        mailbcc
.............testing  
                                                  call      SendMail
                                                  call      wipevars
                    endif
          endif
          return

.
         
.

          INCLUDE   NORDIO.INC
          INCLUDE   COMPIO.inc
          INCLUDE   CNTIO.inc
          include   nmdlio.inc
          INCLUDE   NOWNIO.INC
          include   nuseio.inc
          include   nspeio.inc
          INCLUDE   NOFRIO.INC
          include   nord5io.inc
          include   hpio.inc
          INCLUDE   NCNTIO.INC
          INCLUDE   NSEL2IO.INC
          INCLUDE   nsmpio.inc
           include    npndio.inc
          INCLUDE   COMLOGIC.INC
