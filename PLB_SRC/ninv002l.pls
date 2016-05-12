................................................................................
.NINV002L - DATABUS VERSION OF RPG PROGRAM NINP21C
...............................................................................
.
PC             EQU            1
               INC            COMMON.INC
               INC            CONS.INC
               INC            CONSACCT.inc
.
.begin patch 10.0
               INCLUDE        ninvdd.inc
               Include        NInvAcdDD.inc
                              include   compdd.inc
                              include   cntdd.inc
               Include        Nsel2dd.inc
.end patch 10.0
.               INCLUDE        NMLRDD.INC
               INCLUDE        NBILDD.INC
               INCLUDE        NOWNDD.INC
               INCLUDE        NRTXDD.INC
               INC            NMTXDD.INC
               INCLUDE        NACDDD.inc
               include        nshpdd.INC
               inc            npaydd.INC
               INCLUDE        NORDDD.INC
.               include        nbrkdd.INC
               include        nmrgdd.INC
               include        nrtndd.INC
               include        ndatdd.INC
               include        ndat3dd.INC
               include        nofrdd.inc
               INCLUDE        HP.INC
               INCLUDE        ncntdd.inc
               include        nmlddd.inc
          include   winapi.inc
release   init    "11.33"     DLH  .527 Tax code
REldate   Init      "2015 Dec 1"      
.release   init    "11.32"     DLH  .was skipping broker guaranty if return to was infogroup????? turned off
.REldate   Init      "2015 June 4"      
.release   init    "11.31"     DLH  .LM Net change
.REldate   Init      "2015 April 30"      
.release   init    "11.3"         DLH .change from faxsys to faxcore printer
.Reldate   Init    "2015 April 14"
.release   init    "11.2"         DLH .if present and not bill direct print broker/consultant contact name
.Reldate   Init    "2014 November 3"
.release   init    "11.1"         DLH .Convert to Sunbelt PDF
.Reldate   Init    "2003 April 16"
.release   init    "11.0"         DLH .code to find pdf995 directory on 64 bit OS machines
.Reldate   Init    "08 April 2011"
.release   init    "10.9"         DLH .turn off LW Robbins Extras
.Reldate   Init    "17 November 2010"
.release   init    "10.8"         DLH .turn off Beth Foster Extras
.Reldate   Init    "26 February 2010"
.release   init    "10.7"         DLH .turn of PLI logo
.Reldate   Init    "02 February 2010"
.release   init    "10.6"         DLH fix inordinate wait for pdf printing
.Reldate   Init    "05 October 2008"
.release init    "10.55"                    JD       Cleanup more  printing
.Reldate        Init           "12 May 2008"
.release init    "10.54"         DLH 11Oct07     Cleanup more  printing
.Reldate        Init           "11 oct 2007"
.release init    "10.53"         DLH 28Sep07     Cleanup hot print pdf (tries to send LO copy even if not one)
.Reldate        Init           "28 Sep 2007"
.release init    "10.52"         DLH 26Sep07     PLi
.Reldate        Init           "26 Sep 2007"
.release init    "10.51"         DLH 17Sep07     New merge deduct nmrgcnr
.release init    "10.5"          DLH 12Mar07     Pacific Lists - brokerage side
.Reldate        Init           "17 Sep 2007"

.release init    "10.4"                 DLH         06Dec2006  SendMail
.release init    "10.37"      JD          05JUL2006  2 copies for Beth Foster clients
.release init    "10.36"      ASH 16MAY2006 RECONCILIATION OF LANGUAGE BETWEEN:  NCSH002A, NORD002L, NINV002L, NADJ002L, NORD0024, NORD024B
.RELEASE   INIT                 "10.35 "    JD04Apr06  No longer printing Remittance copies./Turned on.
.RELEASE   INIT                 "10.35 "      JD  27FEB06  No longer printing Remittance copies./Not implemented yet.
.RELEASE   INIT                 "10.34 "     JD  08DEC05  Added new Ship-to number for Donnelley/InfoUSA
.RELEASE   INIT                 "10.33 "     JD  18NOV05  Replaced Netline note with  CV note for disaster rejects.
.RELEASE   INIT                 "10.32 "    ASH 10NOV05  Added new Ship-to number for Donnelley/InfoUSA
.RELEASE   INIT                 "10.31 "    JD18oct2005  saving nmrgnet, getting overwritten by compute.
.Release        Init           "10.3"      JDOct062005 Patch for website
.Release        Init           "10.2"     Sep192005 Patch for website
.Release        Init           "10.1"     Sep142005 JD Trim loinvn before scan
.Release        Init           "10.0"    Aug312005 JD Went through and applied missing pathes.
.Release        Init           "10.0"   February 2004 DLH new additional structure, revised compute
.                                      New Invoice structure
.Reldate        Init           "25 February 2004"
...........................................
.CLOCK    FUNCTION
........................
DATE     DIM       6
SYSMO    DIM       2
SYSDY    DIM       2
SYSYR    DIM       2
.
.begin patch 10.53
PrtOwnOK  DIm       1         ."Y" if printing Lo copy
.end patch 10.53
.;Begin Patch 9.34
OLDBRKNEWCOMP external "COMP001A;OldBrktoNewComp"
OLDBRKNEWCONTACT external "COMP001A;OldBrktoNewContact"
HOLD      DIM       500
HOLD2     DIM       304
.;End Patch 9.34
.begin patch 11.2
ATTN       Dim        52                    .broker/consult attn
.end patch 11.2
.FILES.
...............................................................................
.
.
PINVOICE       FILE          FIXED=412
.
. WORK VARIABLES
...............................................................................
.
.Begin patch 10.6
FileCheck           FIle
trapcount           form      4
.end patch 10.6
MO       DIM       2
DY       DIM       2
YR       DIM       2
TYPIST   DIM       3
PAYKEY   DIM        5
AP2SW    DIM       1
PAYTOO   DIM       45
DIM1     DIM        1
reprint  dim        7
reprtdte dim        8
MRGSW    DIM       1    "Y" IF MERGE INFO
shipsw   dim       1
.
FORM22   FORM      2.2
FORM9    FORM      9
FORM52   FORM      5.2
FORM11   FORM      11
CVTFLD   DIM       10             WORK FIELD USED FOR MP CONVERSION.
COUNT    FORM      5
CO       FORM      1
.
.
.PRINT MASK VARIABLES
.
MASK22   INIT      "ZZ.ZZ"
MASK42   INIT      "Z,ZZZ.ZZ-"
MASK72   INIT      "Z,ZZZ,ZZZ.ZZ-"
MASK52   INIT      "ZZ,ZZZ.ZZ-"
MASK9    INIT      "ZZZ,ZZZ,ZZZ"
.
M$RTAX   DIM       5      *RETURN-TO TAX PERCENT
M$AR     DIM       15
M$PPM    DIM       6
M$PPMx   DIM       6
.begin patch 10.33
M$inpQTY DIM       11
.end patch 10.33
M$netQTY DIM       11
M$QTY    DIM       11
M$AddQTY    DIM       11
M$QTYx   DIM       11
M$AP1    DIM       15
M$AP2    DIM       15
M$STAX   DIM       10
M$CTAX   DIM       8
M$POST   DIM       6
M$LRINC  DIM       15
M$NINC   DIM       15
M$GROSS  DIM       15
M$GROSSbase  DIM       15      base  cost before net
M$netsavins  DIM       15      $ saved by having net
M$netsavfee  DIM       15      Fee to NINCa for negotiating savings
.begin patch 10.0
.Reset995Flag   Dim            1                             ;'Y' means we played with pdf99.ini and need to restore
.end patch 10.0

.
GUARPRT  DIM       60      *GUARANTY PRINT LINE.
guardate dim       8
SHORT    DIM       59      *NET/SHORT INSTRUCTIONS.
TAXLINE  DIM       24      *TAX INFO IF APPLICABLE.
NETLINE  INIT      "Payment must be accompanied by computer":
                       " verification."
.begin patch 10.33
CVLINE  INIT      "Disaster names have been suppressed."
.begin patch 10.33
.begin patch 10.51
CVLINE2  INIT      "CNR names have been suppressed."
.begin patch 10.51
.
userlogn dim        7
.timestamp1 dim      16
.timestamp2 dim      16
.time1     form      16
.time2     form      16
.time3     form      16
.hexeight integer 4,"4294967295"
LOCAL    INIT      "LOCAL"
HOTFLAG  FORM       1                 "1=daily print, 2=hot print"
FAXFLAG  FORM      1      2=FAXIT
apflag   form      1
mdate    form       5
idate    form       5
CareOf   dim        3
TOTNCOA  FORM      8
TOTBILL  FORM      8
ODATE    DIM       10
PRTFLAG  DIM       1
PERCENT  FORM      4.2
CALCmPER FORM      7.4 commented out 1/2/02
TEMP     FORM      8.4
RTNTAB   FORM      3
paychk   form      1
cvflag   form      1
REP      DIM       1
febdat  form      5
feb      dim       1
latelo   init     "      NONEGS      NONEGB     NOLOINV"
COPY     FORM       "1"
.BEGIN PATCH 10.31
mrgnetsv form      8
.end PATCH 10.31
Laser               PFILE
VerticalPos         Form           5
GreyFIll       Color
NoFIll         Color                   .White
colornum form       24
font1                  font
Font4                  font
font5                  font
Font08                 font
font09                 font
Font09I                font
Font09B                font
Font09BI               font
Font010                font
Font010B               font
Font012B               font
Font014                font
Font014B               font
Font014BI              font
Font018I               font
Font07                 font
Font07dot5             font
Font07dot5B            font
Font07dot5I            font
Font07dot5BI           font
Font018B               font
Font018BI              font
PRTPG24B               font
PRTPG24I               font
PRTPG10                font
.Begin Patch 10.5
FontO7              font
FontO18B  font
.end patch 10.5
sevenfive              form               "7.5"
str500        dim 500
NINLogo   PICT
.begin patch 9.5
//
// 22 DEC 2004 BJACKSON --> added lrNumber and externalMode for use with new
//                           webGenerate external
//
lrNumber                dim         6
externalmode            integer     1
//
// 22 DEC 2004 BJACKSON --> end of changes
//

// 22 DEC 2004 BJACKSON --> added start label and condtional create to support new
//                           webGenerate external
. goto webgenerate
start
    if (externalMode)
.Begin Patch 10.5
          create    fontO7,"Times New Roman",size=7
          create    fontO18B,"Times New Roman",size=18,Bold
.End Patch 10.5
        CREATE  NINLogo=3:13:30:50,"..\images\NIN logo black outline.jpg"
    else
.Begin Patch 10.5
          create    fontO7,"Times New Roman",size=7
          create    fontO18B,"Times New Roman",size=18,Bold
.End Patch 10.5
        CREATE  NINLogo=3:13:30:50:
            "\\nins1\e\netutils\NIN logo black outline.jpg"
    endif
//
// 22 DEC 2004 BJACKSON --> end of changes
//
.         CREATE    NINLogo=3:13:30:50:
.                   "\\NinS1\c\netutils\NIN logo black outline.jpg"
.end patch 9.5
.               "\\NinS1\c\netutils\NewNinLogo.gif"
mss1           plform  Error
..Create fonts to be used
               create               font1,"Helvetica",size=14,bold
               create               Font08,"Helvetica",size=8
               create               font5,"Helvetica",size=11
               create               font09,"Helvetica",size=9
               create               Font09I,"Helvetica",size=9,Italic
               create               Font09B,"Helvetica",size=9,Bold
               create               Font09BI,"Helvetica",size=9,Bold,Italic
               create               Font010,"Helvetica",size=10
               create               Font010B,"Helvetica",size=10,Bold
               create               Font012B,"Helvetica",size=12,Bold
               create               Font014,"Helvetica",size=14
               create               Font014B,"Helvetica",size=14,Bold
               create               Font014BI,"Helvetica",size=14,Bold,Italic
               create               Font018I,"Helvetica",size=18,Italic
               create               Font07,"Helvetica",size=7
               create               Font07dot5,"Helvetica",size=sevenfive
               create               Font07dot5I,"Helvetica",size=sevenfive,Italic
               create               Font07dot5b,"Helvetica",size=sevenfive,Bold
               create               Font07dot5bI,"Helvetica",size=sevenfive,Bold,Italic
               create               Font018B,"Helvetica",size=18,Bold
               create               Font018BI,"Helvetica",size=18,Bold,Italic
               create               PRTpg24B,"Helvetica",size=24,Bold
               create               PRTpg24I,"Helvetica",size=24,Italic
               create               PRTpg10,"Helvetica",size=10
               Create         GreyFill=224:224:224
               Create         NoFill=255:255:255
               formload       mss1

.Find out system information

               CALL           GETWINVER
.temp
.         PRtOPen   Laser,"",""
.         PRTOPEN       Laser,str500,"test fax"
.               goto           testprt
.
          MATCH     "NINV002L" TO PROGRAM
          IF         NOT EQUAL
                    MOVE      "NINV002L" TO PROGRAM
                    MOVE      "Names in the News" TO COMPNME
                    MOVE      "PRINT INVOICES" TO STITLE
                    MOVE      C1 TO hotflag
                    move      c1 to faxflag
                    move      c1 to NINVfrmflag
                    move      c1 to ninvoutflag              .print add charges flag`
                    MOVE      "NINVPRT1" TO INPNAME
                    move      c4 to cntprint
                    clear     reprint
                    clear     reprtdte
          ELSE
                    unpack    INPNAME,NINVFLD,PRTFLAG,typist
                    move      inpname,str25
                    move      user,userlogn
                    MOVE      C2 TO hotflag
                    move      "Reprint" to reprint
                    clock     date to reprtdte
                    move      c1 to faxflag
                    move      c1 to NINVFRMFLAG
                    move      c1 to ninvoutflag
                    move      func to faxflag
                    MOVE      "HOT INVOICE PRINT" TO STITLE
                    MOVE      C1 TO NINVPATH
                    MOVE      NINVNAME TO INPNAME
         ENDIF
         move      c1 to nbrkpath       .set access to isi by primary key.
           MOVE      C1 TO NORDPATH       .SET ACCESS TO ISI by primary key.
         MOVE       "02" TO MM
         MOVE       "09" TO DD
         MOVE       "96" TO YY
         CALL      CVTJUL
         move      juldays to febdat
         MOVE      DATE TO TODAY
         CALL      PAINT
          MOVE      "Exit" TO PF5
         TRAP      END IF F5
         CALL      FUNCDISP
         compare   c4 to cntprint
         goto      flagchck if equal
         move      c3 to ncntpath
.Find Printer Aread
.
        clock      port to str3           .plb note
.bytes 1-2 are 0-99
.byte  3   is the hundreds field
        unpack     str3 into str2,str1
        pack       str3 from str1,str2
        move       str3 to portN
.
         move      portn to ncntfld1
         rep       zfill in ncntfld1
         call      ncntkey
flagchck
         BRANCH    hotflag OF DLYPRT,HOTPRT
.
DLYPRT   PACK      STR35,NTWKPATH1,"NINV2.lst"
               call           PRTOPENPrep
         GOTO       PREPIT
HOTPRT   pack       prtfile from pdrive,prtname
         rep        lowup in prtfile
         display    *p15:07,prtfile;
         compare    c2 to faxflag
         if         not equal
               Pack           str45,"c:\work\",prtname
               call           PRTOPENPrep
                move          c2 to hotflag

         else
               Pack           str45,"C:\WORK\FAXFILE.PRN"
               erase          str45
.               call           PRTOPENPrep
.works great for xp prob need more code for others.
.               prtopen        Laser,"FACSys Fax Printer",str45
.will it work??????????????
               prtopen        Laser,"Faxcore IP Printer",str45
         endif
.
PREPIT   DISPLAY   *P01:06,"Input File  : ":
                   *P01:07,"Print File  : ":
                   *P01:08,"Input Count : ":
                   *P01:09,"Today's Date : "
.
INPGET         TRAP           IOMssg Giving Error IF IO
               append         "OpenInp - ",KeyLocation
               Move           INpname to Filename
               BRANCH         hotflag OF OPENINP,READLIVE
OPENINP        OPEN           Pinvoice,INPNAME
               Clear          Filename
               Clear          Location
               GOTO           TESTQUES
FILENG   NORETURN
         KEYIN     *P01:24,*EL,"The Input file is not on-line. ":
                   *P15:06,INPNAME
         GOTO      INPGET
.
TESTQUES goto      testprt
.
INPUT          READ           Pinvoice,SEQ;INVVARS:
                              TYPIST
           GOTO      EOJ IF OVER
.begin patchy
.Note code added to Ninv0004 to prevent this from happening DH 22March2012
          Call      trim using lrn
          call      trim using invnum
          if        (lrn = "")
.call subroutine send email
          goto      input
          endif
          if        (invnum = "")
.call subroutine send email
          goto      input
          endif
.end patchy



           match     "495532" to lrn
           call      debug if equal
           move      c1 to copy
           GOTO      PROCESS
READLIVE       REP       ZFILL IN NINVFLD
               append         "NInvFld - ",KeyLocation
               append         ninvfld,KeyLocation
               reset          Keylocation
               Move           "Ninvkey" to Filename
               call      ninvkey
.
PROCESS
           ADD       c1 TO COUNT
           DISPLAY   *P15:08,COUNT,b1,lrn
           MOVE      MASK9 TO M$QTY
           EDIT      qtybild TO M$QTY
.
           MOVE      PPM TO CMPT92
           MOVE      MASK32 TO M$PPM
           MOVE      CMPT92 TO FORM32
           EDIT      FORM32 TO M$PPM
.
           move      c0 to form9
           call               trim using irexqty
           MOVE      irexqty TO FORM9               split portion.
           MOVE      MASK9 TO M$QTYX
           EDIT      FORM9 TO M$QTYx
.
           move      c0 to CMPT92
           MOVE      iexPPM TO CMPT92
           MOVE      MASK32 TO M$PPMX
           MOVE      CMPT92 TO FORM32
           EDIT      FORM32 TO M$PPMX
.
           MOVE      LRN TO NORDFLD
               append         "NordFld - ",KeyLocation
               append         nordfld,KeyLocation
               reset          Keylocation
               Move           "Nordkey" to Filename
           CALL      NORDKEY
.
               move           oodnum to nofrfld
               append         "Nofrfld - ",KeyLocation
               append         nofrfld,KeyLocation
               reset          Keylocation
               Move           "Nofrkey" to Filename
               call           nofrkey
           move      "01" to mm
           move      "01" to dd
           move      "19" to cc
           move      "95" to yy
           call      cvtjul
           move      juldays to str5
           move      invdtem to mm
           move      invdted to dd
           move      invdtey to yy
           move      invdtec to cc
           call      cvtjul
           move      str5 to n5
           sub       juldays from n5
           goto      bilread if not less
           MATCH     "7364" TO MLRN
           GOTO      BILREAD IF EQUAL
           CLEAR     BRCOMP
           CLEAR     BRaddr
           CLEAR     BRcity
           CLEAR     BRstate
           CLEAR     BRzip
           CLEAR     NBRKFLD
           move      b3 to careof
.begin patch 11.2
              Clear   Attn
              Clear   Hold2
.end patch 11.2
           PACK      NBRKFLD FROM IBRKNUM,IBRKCNT
           CMATCH    B1 TO NBRKFLD
           goto      GOON IF EOS
.               append         "NbrkFld - ",KeyLocation
.               append         nbrkfld,KeyLocation
.               reset          Keylocation
.               Move           "NBrkkey" to Filename
.           call      nbrkkey
.           goto      goon if over
.           rep       zfill in cobn
.           pack      mkey from mlrn,z3
.               append         "NMlrFld - ",KeyLocation
.               append         Mkey,KeyLocation
.               reset          Keylocation
.               Move           "NMlrkey" to Filename
.               call           nmlrkey
.Begin Patch 9.34
.           call      nbrkkey
.           goto      goon if over
                                        call      OLDBRKNEWCOMP using NBRKFLD,HOLD
                                        goto      goon if (hold = "")
.begin patch 11.2
                                                       Clear      Attn
                                                       clear      Hold2
                                        call      OLDBRKNEWCONTACT using NBRKFLD,HOLD2
                                        goto      goon if (hold2 = "")
              call debug
                                        unpack           hold2 into str9,str45
                                        call           trim using str45
                                            if         (str45 <> "")
                                            pack       attn from "Attn: ",str45
                                            else
                                            clear attn
                                            endif

.end patch 11.2
.EndPatch 9.34
           rep       zfill in cobn
           pack      mkey from mlrn,z3
.Begin Patch 9.34
.           call      nmlrkey
                                        pack                compfld3 from mkey
                                        call                COMPKEY3
.End Patch 9.34
.START PATCH 9.4.4 ADDED LOGIC
                                        if (CNCTINACTIVE = "T")
                                                  clear     cnctfname
                                        endif
.END PATCH 9.4.4 ADDED LOGIC
           move      c10 to mm          .oct 25
           move      "24" to dd         . last day
           move      "95" to yy         . be
           move      "19" to cc
           call      cvtjul                .fore
           move      juldays to n6         .new obildrct
           move      oodtem to mm           .code
           move      oodted to dd           .check
           move      oodtey to yy           .it
           move      oodtec to cc
           call      cvtjul                .out
           sub       juldays from n6       .what is diff?
           compare   c0 to n6
           if        less              .do new way
           cmatch    yes to obildrct            .bill direct?  03/19/98 dlh
           goto      brker if not equal          .changed 3/19/98  .yes
           endif
           rep       zfill in cobn
           pack      mkey from mlrn,z3
.               append         "NMlrFld - ",KeyLocation
.               append         Mkey,KeyLocation
.               reset          Keylocation
.               Move           "NMlrkey" to Filename
.Begin Patch 9.34
.           call      nmlrkey
                                        pack                compfld3 from mkey
                                        call                COMPKEY3
.End Patch 9.34
.START PATCH 9.4.4 ADDED LOGIC
                                        if (CNCTINACTIVE = "T")
                                                  clear     cnctfname
                                        endif
.END PATCH 9.4.4 ADDED LOGIC
           goto      goord
brker
.Begin Patch 9.34  Code Added
                                        MOVE COMPCOMP to CNCTFNAME
                                        reset hold to 7
                                        move hold to COMPCOMP
                                        reset hold to 62
                                        move hold to COMPADDR
                                        reset hold to 132
                                        move hold to COMPCITY
                                        reset hold to 162
                                        move hold to COMPSTATE
                                        reset hold to 164
                                        move hold to COMPZIP
                                        reset hold


.           move      mcomp to mname
.           move      BRCOMP to Mcomp
.           move      BRaddr to maddr
.           move      BRcity to mcity
.           move     BRstate to mstate
.           move     BRzip to mzip
.End Patch 9.34

           move     "C/O" to careof
           goto      goord
GOON
           call      readblto
.
goord
               MOVE      LRN TO NORDFLD
               append         "NOrdFld - ",KeyLocation
               append         Nordfld,KeyLocation
               reset          Keylocation
               Move           "NOrdkey" to Filename
               CALL      NORDKEY
           pack         NMLDFLD1,"01X",OLRN
           clear   str8
           pack         str8,"99999999"
          move      "NMLDAIM",Filename
          pack      KeyLocation,"Key: ",NMLDfld1
           call         NMLDAIM
           loop
                  until over
                  if (NMLDDATE < str8)
                           move         NMLDDATE,str8
                  endif
             move   "NMLDKG",Filename
             pack   KeyLocation,"Key: ",NMLDfld1
                  call         NMLDKG
         repeat
         if (str8 <> "99999999")
.Valid Hit - Use this Value as Earliest Date
         unpack     str8 into omdtec,omdtey,omdtem,omdted
         else
.Use current Mail Date
         endif
.end PATCH 8.02
.
           move      oodnum to nofrfld
           call      nofrkey
.
.............................................New JD JAn97
           PACK      STR2 FROM OSALES10,OSALES
           REP       ZFILL IN STR2
           MOVE      NO TO LSTMSW
.begin patch 10.52
          If        (str2 = "06" | str2 = "19" | str2 = "27" | str2 = "28")
           MOVE      yes TO LSTMSW
.           MATCH     "06" TO STR2
.           IF        EQUAL
.           MOVE      YES TO LSTMSW            *LIST MANAGEMENT.
.           ELSE
.              MATCH     "19" TO STR2
.              IF        EQUAL
.              MOVE      YES TO LSTMSW
.              endif
.end patch 10.52
           endif
.........................................................
         MOVE      OLNUM TO NDATFLD
         MOVE      C1 TO NDATPATH
          move      "NDatKey",Filename
          pack      KeyLocation,"Key: ",NDatfld
         CALL      NDATKEY
.
.begin patch 10.0
          packkey   NSEL2FLD,"1",OLRN
          move      "NSEL2KEY",Filename
          pack      KeyLocation,"Key: ",NSEL2FLD
          call      NSEL2KEY
          if             over
          move      O2DES,NSEL2NAME
          unpack    OPPM,str3,str2
          pack      str6,str3,".",str2
          rep       zfill,str6
          move      str6,NSEL2PRICE
          endif

.end patch 10.0
.
         cmatch    "F" to onetfm
         if        equal
           MOVE      NORDFLD to nshpfld
           REP       ZFILL IN NshpFLD
          move      "NShpKey",Filename
          pack      KeyLocation,"Key: ",NShpfld
               CALL           NshpKEY
                    if        not over
                    move      squant to nmrgiqty
                      MOVE      squant TO FORM9
                      MOVE      MASK9 TO M$netQTY
                      EDIT      FORM9 TO M$netQTY
          else
                    bump      oqty
                    move      oqty to nmrgiqty
                    reset     oqty
                      MOVE      oqty TO FORM9
                      MOVE      MASK9 TO M$netQTY
                      EDIT      FORM9 TO M$netQTY
                    endif
         endif
....................................................................
         cmatch    yes to lstmsw                    .Lstmgmt its ok! 1/29/97.
         if        equal
         move      c0 to n2
         move      irnetper to n2
         compare   c0 to n2                    .net name order?
                        if        not equal
                          MOVE      NORDFLD to nshpfld
                          REP       ZFILL IN NshpFLD
                     move     "NSHPKey",Filename
                     pack     KeyLocation,"Key: ",NShpfld
                          CALL        NshpKEY
                                        if        not over
                                        move      squant to nmrgiqty
                                        MOVE      squant TO FORM9
                                        MOVE      MASK9 TO M$netQTY
                                        EDIT      FORM9 TO M$netQTY
                              else
                                        bump      oqty
                                        move      oqty to nmrgiqty
                                        reset     oqty
                                        MOVE      oqty TO FORM9
                                        MOVE      MASK9 TO M$netQTY
                                        EDIT      FORM9 TO M$netQTY
                                        endif
                          endif
         endif
.end........................................................
.
           MOVE      NORDFLD to nmrgfld
           REP       ZFILL IN NMRGFLD
.begin patch 10.31
                              move      c0 to mrgnetsv
.end patch 10.31
           move      no to mrgsw
           move      c1 to cvflag
          move      "NMRgKey",Filename
          pack      KeyLocation,"Key: ",NMRgFld
           CALL        NMRGKEY
           if        over
           display   *p1:24,*el,"No Cv",*p1:24,*el;
           else
           move      c2 to cvflag
           move      yes to mrgsw
           MOVE      nmrgrqty TO FORM9
.begin patch 10.33
                                sub       nmrgdisa from form9
.end patch 10.33
.begin patch 10.51
                                sub       nmrgcnr from form9
.end patch 10.51
           MOVE      MASK9 TO M$netQTY
           EDIT      FORM9 TO M$netQTY
.begin patch 10.33
.           MOVE      nmrgrqty TO FORM9
.           MOVE      MASK9 TO M$inpQTY
.           EDIT      FORM9 TO M$inpQTY
.end patch 10.33
.begin patch 10.31
                              move      nmrgnet to mrgnetsv
.end patch 10.31
           endif
.begin patch 10.0
               Move           c0 to n2
               Move           irnetper to n2
.               if             (n2 > c0)
               if             (n2 > c0 & nmrgdisa > c0)
.begin patch 10.33
               pack           SHORT from nmrgdisa,b1,cvline
.END patch 10.33
              else
.begin patch 10.51
.NEW CODE
                  if             (n2 > c0 & nmrgCNR > c0)
               pack           SHORT from nmrgCNR,b1,cvline2
               ELSE
               Clear          Short
               endif
                  ENDIF
.END patch 10.51
.end patch 10.0
.
.
.begin patch 10.0
.CHKNET
.....
.           MATCH     "032" TO OSPI            *NET NAME INSTRUCTIONS?
.           GOTO      CHKRTN IF EQUAL          *YES
.           MATCH     "033" TO OSPI            *NET NAME INSTRUCTIONS?
.           GOTO      CHKRTN IF EQUAL          *YES
.           BUMP      OSPI BY 3             *MOVE FP TO NEXT INSTRUCTION
.           GOTO      CHKGUAR IF EOS         *IF DONE, MOVE ON.
.           GOTO      CHKNET
.begin patch 10.0
.
.patch xxx
CHKRTN
.CHKRTN     MATCH     "0040" TO ORTNNUM    *TRIPLEX?
.           GOTO      CHKGUAR IF EQUAL     *YES.
.           MATCH     "5224" TO ORTNNUM    *TRIPLEX?
.           GOTO      CHKGUAR IF EQUAL     *YES.
..START PATCH 10.32 ADDED LOGIC
.           MATCH     "5318" TO ORTNNUM    *TRIPLEX?
.           GOTO      CHKGUAR IF EQUAL     *YES.
..END PATCH 10.34 ADDED LOGIC
..START PATCH 10.32 ADDED LOGIC
.           MATCH     "5316" TO ORTNNUM    *TRIPLEX?
.           GOTO      CHKGUAR IF EQUAL     *YES.
..END PATCH 10.34 ADDED LOGIC
..START PATCH 10.34 ADDED LOGIC
.           MATCH     "5319" TO ORTNNUM    *TRIPLEX?
.           GOTO      CHKGUAR IF EQUAL     *YES.
..END PATCH 10.34 ADDED LOGIC
.end patch xxx
.
BrkGuar
               Clear          Guarprt
               if             (OBRKGUAR = "1" | OBRKGUAR = "2" | OBRKGUAR = "3" | OBRKGUAR = "4" )
               Move           "You Guaranteed Payment, Please pay Promptly" to Guarprt
               goto           Ownprep
               endif
CHKGUAR  CLEAR     GUARPRT                 *CLEAR GUARANTY PRINT LINE.
         CLEAR     guardate                *clear guaranty date.
           MOVE      c0 TO n2         *CLEAR BRANCH VAR.
           MOVE      GUARCODE TO n2
           BRANCH    n2 OF GUAR1,GUAR1,GUAR1,GUAR2
           GOTO      OWNPREP
GUAR1    MOVE      "Payment Guaranteed. Please Pay By:" TO GUARPRT
         move      omdtem to mm
         move      omdted to dd
         move      omdtey to yy
         call      cvtjul
           MOVE      GUARCODE TO n2
         branch    N2 of day30,day45,day45
           GOTO      OWNPREP
day30    add       c30 to juldays
         call      cvtgreg
          pack      guardate from mm,slash,dd,slash,yy
          goto     ownprep
day45    add       c45 to juldays
         call      cvtgreg
          pack      guardate from mm,slash,dd,slash,yy
          goto     ownprep
day60    add       c60 to juldays
         call      cvtgreg
          pack      guardate from mm,slash,dd,slash,yy
          goto     ownprep
guar2     MOVE      "Payment Guaranteed. Please Pay By:" TO GUARPRT
.
OWNPREP        MOVE           OLON TO NOWNFLD
               REP            ZFILL IN NOWNFLD
               MOVE           PAYTN TO DIM1
               MOVE           PAYTN TO PAYCHK
               PackKey        Npayfld from NOWNFLD,DIM1
               REP            ZFILL IN Npayfld
         CLEAR     PCOMP                   *PCBUS DOES NOT CLEAR ON OVER.
         CLEAR      PNAME
         CLEAR      PSTREET
         CLEAR      PCITY
         CLEAR      PSTATE
         CLEAR      PZIP
          move      "NPayKey",Filename
          pack      KeyLocation,"Key: ",NPayfld
           CALL      NPAYKEY
          move      "NOwnKey",Filename
          pack      KeyLocation,"Key: ",NOwnfld
           CALL      NOWNKEY
.

MLRTX
           MOVE      OMLRNUM TO NMTXFLD
          move      "NMTXKey",Filename
          pack      KeyLocation,"Key: ",NMtxFld
           CALL      NMTXKEY
           GOTO      NOTAX IF OVER
.
           CMATCH    "T" TO MTXCODE     *TAX EXEMPT ?
           GOTO      TAXEXMPT IF EQUAL     *YES.
           CMATCH    "D" TO MTXCODE     *DIRECT PAYMENT PERMIT?
           GOTO      TAXDIRCT IF EQUAL     *YES.
           CMATCH    "R" TO MTXCODE     *RESALE CERT. ?
           GOTO      TAXRSALE IF EQUAL     *YES.
.Begin patch  11.33
           CMATCH    "P" TO MTXCODE     *527 Political org. ?
           GOTO      TAXP527 IF EQUAL     *YES.
.end patch  11.33
           GOTO      NOTAX
.
RTNTAX     APPEND    "TAX CODE: " TO TAXLINE
           APPEND    RTXCD TO TAXLINE
           APPEND    " " TO TAXLINE
           MOVE      C0 TO FORM52
           DIV       HUND INTO FORM52
           MOVE      C0 TO Form22
           ADD       FORM52 TO FORM22
           MOVE      MASK22 TO M$RTAX
           EDIT      FORM22 TO M$RTAX
           APPEND    M$RTAX TO TAXLINE
           APPEND    "%" TO TAXLINE
           GOTO      TAXDONE
.
TAXEXMPT MOVE      "TAX EXEMPT CERTIFICATE#:" TO TAXLINE
           GOTO      TAXDONE
.
TAXDIRCT MOVE      "DIRECT PAYMENT PERMIT#:" TO TAXLINE
           GOTO      TAXDONE
.
TAXRSALE MOVE      "RESALE CERTIFICATE#:" TO TAXLINE
           GOTO      TAXDONE
.Begin patch  11.33
TaxP527    MOVE      "527 Politcal Organization" TO TAXLINE
           GOTO      TAXDONE
.end patch  11.33
.
NOTAX    CLEAR     TAXLINE     *CLEAR PRINT LINE,
.
TAXDONE
.
.begin patch 10.00
               CLEAR          NInvAcdfld
               pack           NInvAcdFld from Invnum
               call           NInvAcdRecClear
          move      "NInvAcdTst",Filename
          pack      KeyLocation,"Key: ",NInvAcdFld
               call           NinvAcdTst
               Call           NInvAcdRecLoad
.end patch 10.00
               move           yes to subppsw
               CALL           COMPUTE
.
               if             (onetfm <> "F" & onetFM <> "M")
               move           c1 to netflag
               endif

               COMPARE        C0 TO FORMAP2
               IF             EQUAL
               MOVE           NO TO AP2SW
               ELSE
               MOVE     YES TO AP2SW
               ENDIF
.
APQUES
         MOVE      OWNOCPY TO PAYTOO
         CMATCH    B1 TO PCOMP
         IF        NOT EOS
         COMPARE   C0 TO PAYCHK
         IF        EQUAL
         MOVE      PCOMP TO PAYTOO
         MOVE      PCOMP TO OWNOCPY
         MOVE      PNAME TO OWNLONM
         MOVE      PSTREET TO OWNLOSA
         MOVE      PCITY TO OWNLOCTY
         MOVE      PSTATE TO OWNLOS
         MOVE      PZIP TO OWNLOZC
         ENDIF
         ENDIF
MASKER
....
           MOVE      MASK92 TO M$GROSS
           EDIT      GROSS TO M$GROSS
.
           MOVE      MASK92 TO M$AR
           EDIT      FORMAR TO M$AR
.
           MOVE      MASK92 TO M$AP1
           EDIT      AP TO M$AP1
.
           MOVE      MASK92 TO M$AP2
           EDIT      FORMAP2 TO M$AP2
.
           MOVE      MASK92 TO M$LRINC
           EDIT      LRINC TO M$LRINC
.
           MOVE      MASK92 TO M$NINC
           EDIT      NININC TO M$NINC
.
           MOVE      MASK52 TO M$STAX
           EDIT      TAXES TO M$STAX
.
           MOVE      C0 TO TAXES
           MOVE      MASK42 TO M$CTAX
           EDIT      TAXES TO M$CTAX
.
           MOVE      MASK32 TO M$POST
           EDIT      POST TO M$POST
           move      mask92 to m$grossbase
           edit      grossbase to m$grossbase
.
           move      mask92 to m$netsavfee
           edit      netsavfee to m$netsavfee
.
           move      mask92 to m$netsavins
           mult      seq by netsavins
           edit      netsavins to m$netsavins
.
           GOTO      PRINT
.
.
bilread
         bump      cobn by 2
         move      cobn to billtn
         reset     cobn
           PACK      NBILFLD FROM MLRN,COBN,BILLTN
           CALL      NBILKEY
         if        not over
         MOVE      B3 TO CAREOF
.         MOVE      BILNAME TO MNAME
.         move      bilcomp to mcomp
.           move    BILaddr to maddr
.           move    BILcity to mcity
.           move    BILState to mstate
.           move    BILzip to mzip
.         else
.           PACK      MKEY FROM MLRN,z3
.         move      "NMMLRKEY",Filename
.         pack      KeyLocation,"Key: ",MKey
.           CALL      NMLRKEY
.         endif
.Begin Patch 9.34
         MOVE      BILNAME TO CNCTFNAME
         move      bilcomp to COMPCOMP
           move    BILaddr to COMPADDR
           move    BILcity to COMPCITY
           move    BILState to COMPSTATE
           move    BILzip to COMPZIP
.         MOVE      BILNAME TO MNAME
.         move      bilcomp to mcomp
.           move    BILaddr to maddr
.           move    BILcity to mcity
.           move    BILState to mstate
.           move    BILzip to mzip
.End Patch 9.34
         else
           PACK      MKEY FROM MLRN,z3
.Begin Patch 9.34
.           call      nmlrkey
                                        pack                compfld3 from mkey
                                        call                COMPKEY3
.End Patch 9.34
.START PATCH 9.4.4 ADDED LOGIC
                                        if (CNCTINACTIVE = "T")
                                                  clear     cnctfname
                                        endif
.END PATCH 9.4.4 ADDED LOGIC
         endif

          goto      goord
.
READBLTO

           PACK      MKEY FROM MLRN,z3
.         move      "NMLRKEy",Filename
.         pack      KeyLocation,"Key: ",MKey
.           CALL      NMLRKEY

.Begin Patch 9.34 - DOn't think this is used but jic
.           call      nmlrkey
                                        pack                compfld3 from mkey
                                        call                COMPKEY3
.End Patch 9.34
.START PATCH 9.4.4 ADDED LOGIC
                                        if (CNCTINACTIVE = "T")
                                                  clear     cnctfname
                                        endif
.END PATCH 9.4.4 ADDED LOGIC
.Being Patch 9.34
         cmatch    "T" to COMPBDRCTFLG            .bill direct?  11/2 dlh
.         cmatch    yes to mbildrct            .bill direct?  11/2 dlh
.End Patch 9.34
         if         equal            .yes
         return
         endif
         return
.this code (above) needs to be cleaned up
.*......................................................................
..
PRINT
               compare        c1 to ninvfrmflag
               goto           nomlrcpy if not equal
               if             (obrknum = "0273")
               clear          taskname
               append         "!\\nins1\winbatch\butil job=HOTORD",TASKNAME
               append         " infile=",taskname
               append         lrn,taskname
               append         "M",taskname
               append         inits,taskname
               append         " F=default C=1",TASKNAME
               append         " B=",TASKNAME
               append         user,TASKNAME
.begin patch NOv 13 2007
.         APPEND    " PRIN=",TASKNAME
         APPEND    " PA=",TASKNAME
.end patch NOv 13 2007
               append         Cntprint,taskname
               reset          TASKNAME
               execute        TASKNAME
               endif
nomlrcpy
         RESET     RUNCODES
         SCAN      OLNUM IN RUNCODES
         if        equal
         clear     ofdesc
         endif
.begin 15 OCt 2010
          reset     EXFEELST
          scan      OLNUM IN EXFEELST
          if equal
                    clear     OODNUM
                    clear     OFDESC
          endif
.end 15 OCt 2010
         
        move      ap1 to saveap
        COMPARE   C0 TO FORMAP2
         IF        EQUAL
            compare   c0 to saveap
            if        not equal
            COMPARE   C0 TO PAYCHK
               IF         EQUAL
               move      c2 to apflag
               else
               move      c1 to apflag
                endif
            else
            move      c1 to apflag
            endif
            GOTO      PRINTBR
         ELSE
         MOVE      M$AP2 TO M$AP1
         move      c2 to apflag
         ENDIF
PRINTBR
FORMS    branch    NINVFRMFLAG of prtmlr,prtremit,prtown
prtmlr
               call           prtinvfrm
               call           prtmlrboxGui
               Call           PrintDetail
               Prtpage        Laser;*NewPage
               reset          exfeelst
               scan           olnum in exfeelst
               goto           WipeVars if equal

               if             (Copy = 2)
               clear      taskname
               append  "!\\Nins1\Winbatch\butil job=HOTORD",TASKNAME
               append  " infile=",taskname
               append  lrn,taskname
               append  "M",taskname
               append  inits,taskname
               append  " F=default C=1",TASKNAME
               append  " B=",TASKNAME
               append  user,TASKNAME
.begin patch NOv 13 2007
.         APPEND    " PRIN=",TASKNAME
         APPEND    " PA=",TASKNAME
.end patch NOv 13 2007
               append  Cntprint,taskname
               reset   TASKNAME
               execute TASKNAME
               execute        TASKNAME
               goto           WipeVars
               endif
.
.START PATCH 10.37 REPLACED LOGIC
.begin patch 10.8
.               if         ((obrknum = "0638" or  obrknum = "0619") & copy =1 )
.begin patch 10.9
.               if         (obrknum = "0638" & copy =1 )
..end patch 10.8
..               if         (obrknum = "0638" & copy =1 )
..END PATCH 10.37 REPLACED LOGIC
.               move       c2 to copy
.               goto       prtmlr
.               endif
.end patch 10.9
.               
.PATCH9.27 / 9.29 / 9.30 /9.33/9.36
.               if         ((obrknum = "0285" or MLRN = "9635" or MLRN = "1926" or MLRN = "5402") & copy =1)
.               if         ((obrknum = "0285" or MLRN = "9635" or MLRN = "1926" or MLRN = "5402" or MLRN = "7352" ) & copy =1)
.turn off ALL 9/23/11 DLH
.turn off baldwin 9/20/11 DLH
               if         ((MLRN = "000") & copy =1)
.               if         ((MLRN = "9635" or MLRN = "1926" or MLRN = "5402" or MLRN = "0276") & copy =1)
.               if         ((MLRN = "9635" or MLRN = "1926" or MLRN = "5402" or MLRN = "7352" or MLRN = "0276") & copy =1)
.               if         (obrknum = "0285" & copy =1 )  turned off   3/05 DB
.end PATCH9.27 / 9.29 / 9.30 /9.33/9.36
                         call           prtinvfrm
                         call           prtmlrboxGui
                         Call           PrintDetail
                         Prtpage        Laser;*NewPage
                         reset          exfeelst
                         scan           olnum in exfeelst
                         goto           WipeVars if equal
               endif
               goto       WipeVars

PrtRemit
               COMPARE        C2 TO FAXFLAG           fax?
               goto           wipevars IF EQUAL              .yes get out
.
.Begin Patch 10.35
               goto           WipeVars
.End   Patch 10.35
.
               call           prtinvfrm
               call           prtRemitboxGui
               Call           PrintDetail
               Prtpage        Laser;*NewPage
               reset          exfeelst
               scan           olnum in exfeelst
               goto           WipeVars if equal
.begin patch 10.9
.               if             (obrknum = "0638" & copy =1 )
.               move           c2 to copy
.               goto           prtRemit
.               endif
.end patch 10.9
               if             (obrknum = "0285" & copy =1 )
                         move           c2 to copy
                         goto           prtRemit
               endif
               goto           WipeVars
PrtOwn

               COMPARE        C2 TO FAXFLAG           fax?
               goto           wipevars IF EQUAL              .yes get out
.
               if             (count = c1)
               prtpage        Laser;*Newpage
               Prtpage        Laser;*Duplex=2
               endif

               match          "1883" to lon           .triplex ?
               goto           wipevars if equal       .yes skip
.
               branch    apflag of wipevars
.
               compare         c2 to hotflag
               if             not  equal
               reset          latelo
.Begin Patch 10.01
                                                  call           trim using loinvn
.End Patch 10.01
.begin patch 10.53
          MOVe      "N",PrtOwnOK
.end patch 10.53
               scan           loinvn in latelo
.begin patch 10.53
                    If        Equal
.               goto           printOwnOk if equal
                    MOVe      "Y",PrtOwnOK
                      goto           printOwnOk
                      endif
          endif                 
.end patch 10.53
.
               if             (hotflag <> c2 & Elstcde = "C" & (lstnum = "013260"))
.begin patch 10.53
          MOVe      "Y",PrtOwnOK
.end patch 10.53
          
               goto           PrintOwnok
               endif
.
.DH goes bad 12/11/09   turns of LO copies on hot prints
                    if (hotflag = c2)
                    Move      c3,ninvfrmflag
                    goto      Wipevars
                    endif
.DH goes bad 12/11/09
              compare       c2 to hotflag
              if            equal
              PACK          STR45,NTWKPATH1,"HOTLOINV.lst"
                              pack      str25,str25,"LO"
                              if (PRTFLAG = "p")
                                        move      "P",PRTFLAG
                              endif
               call           PRTOPENPrep
                              if (PRTFLAG <> "p")
                                        prtpage        Laser;*Newpage
                                        Prtpage        Laser;*Duplex=2
                              endif
.begin patch 10.53
          MOVe      "Y",PrtOwnOK
.end patch 10.53
               goto          printOwnok
               endif
               goto           WipeVars
.
PrintOwnOK
                             call           wipecvars
               call           NInvAcdRecClear
               CLEAR          NInvAcdfld
               packkey           NInvAcdFld from Invnum
.               call           NInvAcdRecClear
               call           NinvAcdTst
               Call           NInvAcdRecLoad
               call           compute
               call           prtinvfrm
               call           prtOwnerboxGui
               Call           PrintDetail
               call           PrintOwn2ndpage
               goto           WipeVars

PrintDetail
               Prtpage        Laser;*p=750:1762,*font=Font09Bi,reprint,b1,reprtdte
               Prtpage        Laser;*font=Font09b,*p=1500:2212,invdtem,"/",invdted,"/",invdtec,invdtey:
.begin patch 11.2  print broker/consultant contact name if present
                              *p=5000:2212,invnum,*p=6750:2212,Omlrpon
.                              *p=5000:2212,invnum,*p=6750:2212,Omlrpon:
.                              *p=1500:2400,Mlrn,"/",Cobn,"-",Billtn:
                      if         (obildrct = yes)            .bill direct?  
               Prtpage        Laser;*font=Font09b,*p=1500:2400,Mlrn,"/",Cobn
                      else
               Prtpage        Laser;*font=Font09b,*p=1500:2400,Mlrn,"/",Cobn,"-",Billtn,b2,Attn
                      endif
.                              *p=1500:2525,MName:
.                              *p=1150:2650,CareOf:
.                              *p=1500:2650,Mcomp:
.                              *p=6750:2712,LRN:
.                              *p=1500:2775,Maddr:
.                              *p=1500:2900,MCity,", ",Mstate," ",mzip
.Begin Patch 9.34
.                              *p=1500:2525,CNCTFNAME:
               Prtpage        Laser;*font=Font09b,*p=1500:2525,CNCTFNAME:
.end patch 11.2  print broker/consultant contact name if present
.                              *p=1500:2525,MName:
.End Patch 9.34
                              *p=1150:2650,CareOf:
.Begin Patch 9.34
                              *p=1500:2650,COMPCOMP:
                              *p=1500:2650,COMPCOMP:
.                              *p=1500:2650,Mcomp:
.End Patch 9.34
                              *p=6750:2712,LRN:
.Begin Patch 9.34
                              *p=1500:2775,COMPADDR:
.                              *p=1500:2775,Maddr:
                              *p=1500:2900,*ll,COMPCITY,", ",COMPSTATE," ",COMPZIP
.                              *p=1500:2900,MCity,", ",Mstate," ",mzip
.End Patch 9.34
               PrtPage        Laser;*p=6750:3212,OMDTEM,"/",OMDTED,"/",OMDTEC,OMDTEY:
                              *p=750:3212,Guarprt," ",Guardate:
.                              *p=750:3327,Guarprt," ",Guardate:
.                              *p=750:3452,Short:
                              *p=750:3327,Short:
                              *p=750:3212,Taxline," ",mtxexmpt:
.                              *p=750:2577,Taxline," ",mtxexmpt:
                              *p=1500:3662,ofdesc:
                              *p=1500:3912,Omlrky:                            
                              *p=1500:4162,O1des:
                              *p=1500:4287,NSEL2NAME
               call           trim using  OMlrLstCd
               if             (OMlrLStCd <> "")
               prtpage        Laser;*p=5000:4162,"List Code: ",OMlrLstCd
               endif

               Move           "4712" to VerticalPos
.
               branch         netflag to prtn1,prtn2,prtn2
prtn1
               Prtpage        Laser;*p=2500:4712,*ALIGNMENT=*Right,*overlayoff,M$Qty,*overlayon,*ALIGNMENT=*Left:
                              *p750:Verticalpos,*font=Font07Dot5I,"Quantity Addressed",*font=Font09B:
                              *p=5475:Verticalpos,*ALIGNMENT=*right,M$PPM,*ALIGNMENT=*Left:
                              *p=7500:Verticalpos,*ALIGNMENT=*right,M$Gross,*ALIGNMENT=*Left
              goto            prtnx
prtn2
.begin patch 10.31
                              move      mrgnetsv to nmrgnet
.end patch 10.31
         move      c0 to calcmper
         MOVE      NMRGNET TO CMPT94
         MOVE      NMRGIQTY TO N7
         DIVIDE    N7 INTO CMPT94
         MULT      "100" BY CMPT94
         MOVE      C0 TO PERCENT
         ADD       CMPT94 TO PERCENT
         move      c0 to temp
         move      irnetper to temp
         compare   c3 to netflag            .flat???
         if        equal
         move      irnetper to percent         .yes print stated flat rate
         else                              .no print calced rate
         compare   temp to percent           .use bigger %
         if        less
         move      temp to percent
         endif
         endif

         compare   c2 to netflag
         if        equal                                  ;net order
                reset     discmlrs
                scan      mlrn in discmlrs
                if        equal                     ;.discounted
.               Prtpage        Laser;*p=2500:Verticalpos,*ALIGNMENT=*right,M$NetQty,*ALIGNMENT=*Left:
.                              *p750:Verticalpos,*font=Font07Dot5I,"Quantity Addressed",*font=Font09B:
.                              *p=2500:VerticalPos,*ALIGNMENT=*right,*overlayoff,M$Qty,*overlayon,*ALIGNMENT=*Left:
.               Prtpage        Laser;*p=2500:Verticalpos,*overlayoff,*ALIGNMENT=*right,M$NetQty,*ALIGNMENT=*Left:
               Prtpage        Laser;*p=2500:Verticalpos,*ALIGNMENT=*Left:
                               *p750:Verticalpos,*font=Font07Dot5I,"Quantity Addressed",*font=Font09B:
                              *p=2500:VerticalPos,*ALIGNMENT=*right,*overlayoff,M$Qty,*overlayon,*ALIGNMENT=*Left:
.                              *p=2500:VerticalPos,*ALIGNMENT=*right,M$Qty,*ALIGNMENT=*Left:
                              *p=3000:Verticalpos,*ALIGNMENT=*right,percent,*ALIGNMENT=*Left:
                              *p=3020:Verticalpos,"% of":
                              *p=3750:Verticalpos,*ALIGNMENT=*right,m$netqty,*ALIGNMENT=*Left:
                              *p=5125:Verticalpos,*ALIGNMENT=*right,M$PPM,*ALIGNMENT=*Left:
                              *p=7500:Verticalpos,*ALIGNMENT=*right,M$Gross,*ALIGNMENT=*Left:
                              *p=4500:3912,Irnetper,"% Net Name Arrangement, Run charge @",inetrc,slash,"M"
.                              *p=4575:3912,Irnetper,"% Net Name Arrangement, Run charge @",inetrc,slash,"M"
.                              *p=3785:4287,Irnetper,"% Net Name Arrangement, Run charge @",inetrc,slash,"M"

                else                               ; Not Discounted
               Prtpage        Laser;*p=2500:Verticalpos,*ALIGNMENT=*right,m$netqty,*ALIGNMENT=*Left:
.begin patch 10.33
.               Prtpage        Laser;*p=2500:Verticalpos,*ALIGNMENT=*right,m$inpqty,*ALIGNMENT=*Left:
.end patch 10.33
                              *p750:Verticalpos,*font=Font07Dot5I,"Quantity Input",*font=Font09B:
                              *p=5475:Verticalpos,*ALIGNMENT=*right,M$PPM,*ALIGNMENT=*Left:
                              *p=7500:Verticalpos,*ALIGNMENT=*right,M$Grossbase,*ALIGNMENT=*Left
               add            "125" to VerticalPos
               Prtpage        Laser;*p=750:VerticalPos,"Net Savings":
                              *p=7500:VerticalPos,*ALIGNMENT=*right,M$NetSavins,*ALIGNMENT=*Left
               add            "125" to VerticalPos
               Prtpage        Laser;*p=750:VerticalPos,"Commission for negotiated savings":
                              *p=7500:VerticalPos,*ALIGNMENT=*right,M$NetSavfee,*ALIGNMENT=*Left
               add            "125" to VerticalPos
               Prtpage        Laser;*p750:VerticalPos,"Quantity Addressed":
.3/3/06                       *p=2500:VerticalPos,*ALIGNMENT=*right,*overlayoff,M$Qty,*overlayon,*ALIGNMENT=*Left:
                              *p=2500:VerticalPos,*ALIGNMENT=*right,M$Qty,*ALIGNMENT=*Left:
                              *p=3000:VerticalPos,*ALIGNMENT=*right,percent,*ALIGNMENT=*Left:
                              *p=3020:VerticalPos,"% of":
                              *p=3750:VerticalPos,*ALIGNMENT=*right,m$netqty,*ALIGNMENT=*Left:
                              *p=5475:Verticalpos,*ALIGNMENT=*right,M$PPM,*ALIGNMENT=*Left:
                              *p=7500:VerticalPos,*ALIGNMENT=*right,M$Gross,*ALIGNMENT=*Left:
                              *p=4500:3912,Irnetper,"% Net Name Arrangement, Run charge @",inetrc,slash,"M"
.                              *p=4575:3912,Irnetper,"% Net Name Arrangement, Run charge @",inetrc,slash,"M"
.                              *p=3785:4287,Irnetper,"% Net Name Arrangement, Run charge @",inetrc,slash,"M"

                endif

         goto      prtnx
         endif
..........................................................................................................................
         compare   c3 to netflag
         if        equal                                            ;flat of volume discount
.. check if brokerage, and if discounted
          cmatch    no to lstmsw
                if equal               .its a flat discounted brokerage client
                              reset     discmlrs
                              scan      mlrn in discmlrs
                              if equal                    ;.discounted mailer
                                        Prtpage   Laser;*p=2500:VerticalPos,*ALIGNMENT=*right,m$netqty,*ALIGNMENT=*Left:
                                                  *p750:VerticalPos,*font=Font07Dot5I,"Quantity Addressed",*font=Font09B:
.               *p=2500:VerticalPos,*ALIGNMENT=*right,M$Qty,*ALIGNMENT=*Left:
                                                  *p=2500:VerticalPos,*ALIGNMENT=*right,*overlayoff,M$Qty,*overlayon,*ALIGNMENT=*Left:
                                                  *p=3000:VerticalPos,*ALIGNMENT=*right,percent,*ALIGNMENT=*Left:
                                                  *p=3020:VerticalPos,"% of":
                                                  *p=3750:VerticalPos,*ALIGNMENT=*right,m$netqty,*ALIGNMENT=*Left:
                                                  *p5000:VerticalPos,"@":
                                                  *p=5475:VerticalPos,*ALIGNMENT=*right,M$PPM,*ALIGNMENT=*Left:
                                                  *p=7500:VerticalPos,*ALIGNMENT=*right,M$Gross,*ALIGNMENT=*Left:
                                                  *p=4500:3912,Irnetper,"% Net Name Arrangement, Run charge @",inetrc,slash,"M"
.                                                 *p=4575:3912,Irnetper,"% Net Name Arrangement, Run charge @",inetrc,slash,"M"
.                                                 *p=3785:4287,Irnetper,"% Net Name Arrangement, Run charge @",inetrc,slash,"M"
                              else                                ;Not discounted mailer
                                        Prtpage   Laser;*p=2500:VerticalPos,*ALIGNMENT=*right,m$netqty,*ALIGNMENT=*Left:
                                                  *p750:VerticalPos,*font=Font07Dot5I,"Quantity Input",*font=Font09B:
                                                  *p5000:VerticalPos,"@":
                                                  *p=5475:VerticalPos,*ALIGNMENT=*right,M$PPM,*ALIGNMENT=*Left:
                                                  *p=7500:VerticalPos,*ALIGNMENT=*right,M$Grossbase,*ALIGNMENT=*Left
                                        Add       "125" to VerticalPos
                                        Prtpage   Laser;*p=750:VerticalPos,"Net Savings":
                                                  *p=7500:VerticalPos,*ALIGNMENT=*right,M$NetSavins,*ALIGNMENT=*Left
                                        add       "125" to VerticalPos
                                        Prtpage   Laser;*p=750:VerticalPos,"Commission for negotiated savings":
                                                  *p=7500:VerticalPos,*ALIGNMENT=*right,M$NetSavfee,*ALIGNMENT=*Left
                                        Add       "125" to VerticalPos
                                        Prtpage   Laser;*p750:VerticalPos,"Quantity Addressed":
                                                  *p=2500:VerticalPos,*ALIGNMENT=*right,*overlayoff,M$Qty,*overlayon,*ALIGNMENT=*Left:
                                                  *p=3000:VerticalPos,*ALIGNMENT=*right,percent,*ALIGNMENT=*Left:
                                                  *p=3020:VerticalPos,"% of":
                                                  *p=3750:VerticalPos,*ALIGNMENT=*right,m$netqty,*ALIGNMENT=*Left:
                                                  *p=5475:Verticalpos,*ALIGNMENT=*right,M$PPM,*ALIGNMENT=*Left:
                                                  *p=7500:VerticalPos,*ALIGNMENT=*right,M$Gross,*ALIGNMENT=*Left:
                                                  *p=4500:3912,Irnetper,"% Net Name Arrangement, Run charge @",inetrc,slash,"M"
.                                                 *p=4575:3912,Irnetper,"% Net Name Arrangement, Run charge @",inetrc,slash,"M"
.                                                 *p=3785:4287,Irnetper,"% Net Name Arrangement, Run charge @",inetrc,slash,"M"
                              endif
.else its list management
                    else
.begin patch 11.31  . net logic
                      pack       str8 from oodtec,oodtey,oodtem,oodted
                      move       str8,n8
                      if         (n8 < "20150501")
                              Prtpage   Laser;*p=2500:VerticalPos,*ALIGNMENT=*right,*overlayoff,M$Qty,*overlayon,*ALIGNMENT=*Left:
                                        *p750:VerticalPos,*font=Font07Dot5I,"Quantity Addressed",*font=Font09B:
                                        *p=3000:VerticalPos,*ALIGNMENT=*right,percent,*ALIGNMENT=*Left:
                                        *p=3020:VerticalPos,"% of":
                                        *p=3750:VerticalPos,*ALIGNMENT=*right,m$netqty,*ALIGNMENT=*Left:
                                        *p5000:VerticalPos,"@":
                                        *p=5475:VerticalPos,*ALIGNMENT=*right,M$PPM,*ALIGNMENT=*Left:
                                        *p=7500:VerticalPos,*ALIGNMENT=*right,M$Gross,*ALIGNMENT=*Left:
.START PATCH 10.36 REPLACED LOGIC
.                                       *p=4500:4162,"Volume Discount - No Deducts, No CV required.":
.                                       *p=4500:3912,Irnetper,"% Vol Name Arrangement, Run charge @",inetrc,slash,"M"
                                        *p=4500:4162,"No Deducts, No CV required.":
                                        *p=4500:3912,Irnetper,"% Net Arrangement, Run charge @",inetrc,slash,"M"
.                      Else
                      Else
                                          move      c0 to n2
                                          move      irnetper to n2
                      
                              if            (n2 > 0)
                              Prtpage   Laser;*p=2500:VerticalPos,*ALIGNMENT=*right,*overlayoff,M$Qty,*overlayon,*ALIGNMENT=*Left:
                                        *p750:VerticalPos,*font=Font07Dot5I,"Quantity Addressed",*font=Font09B:
                                        *p=3000:VerticalPos,*ALIGNMENT=*right,percent,*ALIGNMENT=*Left:
                                        *p=3020:VerticalPos,"% of":
                                        *p=3750:VerticalPos,*ALIGNMENT=*right,m$netqty,*ALIGNMENT=*Left:
                                        *p5000:VerticalPos,"@":
                                        *p=5475:VerticalPos,*ALIGNMENT=*right,M$PPM,*ALIGNMENT=*Left:
                                        *p=7500:VerticalPos,*ALIGNMENT=*right,M$Gross,*ALIGNMENT=*Left:
                                        *p=4500:4162,"CV required to take deductions.":
                                        *p=4500:3912,Irnetper,"% Net Arrangement, Run charge @",inetrc,slash,"M"
                             Else    
                              Prtpage   Laser;*p=2500:VerticalPos,*ALIGNMENT=*right,*overlayoff,M$Qty,*overlayon,*ALIGNMENT=*Left:
                                        *p750:VerticalPos,*font=Font07Dot5I,"Quantity Addressed",*font=Font09B:
                                        *p=5475:VerticalPos,*ALIGNMENT=*right,M$PPM,*ALIGNMENT=*Left:
                                        *p=7500:VerticalPos,*ALIGNMENT=*right,M$Gross,*ALIGNMENT=*Left
                             endif           
                      endif
.end patch 11.31
.END PATCH 10.36 REPLACED LOGIC
.                                       *p=4585:4162,"Volume Discount - No Deducts, No CV required.":
.                                       *p=4575:3912,Irnetper,"% Vol Name Arrangement, Run charge @",inetrc,slash,"M"
                    endif
          endif
prtnx
          move      c0 to n7
          call      trim using irexqty
          move      irexqty to n7
          compare   c0 to n7
          if not equal
                    add       "125" to VerticalPos
                    Prtpage   Laser;*p=2500:VerticalPos,*ALIGNMENT=*right,M$Qtyx,*ALIGNMENT=*Left:
.                             *p5000:VerticalPos,"@":
                              *p4850:VerticalPos,"@":
                              *p=5475:VerticalPos,*ALIGNMENT=*right,M$PPMx,*ALIGNMENT=*Left
          endif
               FOR           AcdRecCount,"1","15"
                              MOve   NInvAcdRec(AcdRecCount).NinvAcdNumRec,NinvAcdNum
                              MOve   NInvAcdRec(AcdRecCount).NinvAcdCodeRec,NinvAcdCode
                              MOve   NInvAcdRec(AcdRecCount).NinvAcdRateRec,NinvAcdRate
                              MOve   NInvAcdRec(AcdRecCount).NInvAcdPercRec,NInvAcdPerc
                              MOve   NInvAcdRec(AcdRecCount).NINVAcdANINCDRec,NINVAcdANINCD
                              MOve   NInvAcdRec(AcdRecCount).NINvAcdqtyRec,str9
                              MOve   NInvAcdRec(AcdRecCount).NINvAcdTotalRec,str15
                              MOve   NInvAcdRec(AcdRecCount).NinvAcdAextcdRec,NinvAcdAextcd
                              MOve   NInvAcdRec(AcdRecCount).NinvAcdRateTRec,NinvAcdRateT
                              MOve   NInvAcdRec(AcdRecCount).NINvAcdDescRec,nacdtext

               move           NINVAcdANINCD to str3
               call           trim using str3
               move           c0 to anincd
               move           str3 to Anincd

               If             (NinvacdNum = "")
               Break
               endif
                              if             (ninvoutflag = 1 & Ninvfrmflag = c3 & NInvAcdCode = "041")
                              Goto           DontPrintDet
                              endif                           *
..
                              if        (ninvoutflag = 1 & Ninvfrmflag = c3 & NInvAcdCode = "085")
                              Goto           DontPrintDet
                              endif                           *

                              if        (ninvoutflag = 1 & (ANINCD = c3 or ANINCD = c4))
                              Goto           DontPrintDet
                              endif                           *

                              if        (ninvoutflag = 1 & NinvFrmflag = c1 & (ANINCD = c1 or ANINCD = c3 or ANINCD = c4 ))
                              Goto           DontPrintDet               ;mailer copies do not print codes 1,3, or 4
                              endif                           *
                              if        (ninvoutflag = 1 & NinvFrmflag = c2 & (ANINCD = c1 or ANINCD = c3 or ANINCD = c4 ))
                              Goto           DontPrintDet               ;mailer remit copies do not print codes 1,3, or 4
                              endif                           *

                              if        (ninvoutflag = 1 & Elstcde = "C" & NinvFrmflag = c3 & NInvAcdCode = "003")
                              Goto           DontPrintDet
                              endif                           *

                clear          str10
                              If             (NinvAcdRate > 0)
                                             move           mask72 to str12
                                             edit           NinvAcdRate to str12
                                             Move           "@" to Str1
                                             else
                                             Clear          str1
                                             clear          str12
                              endif
                              move           "ZZZ.9999" to str8
                              edit           NInvAcdPerc to str8

                              cLEAR          m$ADDQTY
                                             If             (str9 <> "")
                                             MOVE           MASK9 TO M$AddQTY
                                             MOVE           str9 TO N9
                                             Edit           N9 to M$AddQTY
                                             ENDIF
                              add            "150" to VerticalPos
.                              prtpage        Laser;*p800:VerticalPos,*font=Font09B,nacdtext,*p5000:VerticalPos,"@":
.                              prtpage        Laser;*p800:VerticalPos,*font=Font09B,nacdtext,*p5000:VerticalPos,str1:
                              prtpage        Laser;*p800:VerticalPos,*font=Font09B,nacdtext,*p4850:VerticalPos,str1:
                                             *p=4500:VerticalPos,*ALIGNMENT=*right,M$AddQty,*ALIGNMENT=*Left:
.                                             *p=5475:VerticalPos,*ALIGNMENT=*right,NInvAcdRate,*ALIGNMENT=*Left:
                                             *p=5475:VerticalPos,*ALIGNMENT=*right,str12,*ALIGNMENT=*Left:
                                             *p=7500:VerticalPos,*ALIGNMENT=*right,str15,*ALIGNMENT=*Left

DontPrintDet
               repeat
               if             (NINVFrmFlag = c3)
               Prtpage        Laser;*ALIGNMENT=*Left,*p=7600:10166,Typist
               else
               Prtpage        Laser;*p=7500:10166,*ALIGNMENT=*right,M$AR,*ALIGNMENT=*Left,*p=7600:10166,Typist
               endif

               If             (Oelcode <> "2" & OelCode <> "3")   ;not an exchange order
               call           PrtPaymentReq
               endif
.
..
               Return
..........................................................................................................................
.
PrintOwn2ndpage
                    Prtpage   Laser;*Newpage:
                              *p750:2400,*font=Font09Bi,"Owner ##":
                              *p=1500:2400,*font=Font09B,ownlon:
                              *p=1500:2525,ownlonm:
                              *p=1500:2650,ownocpy:
                              *p=1500:2775,ownlosa:
                              *p=1500:2900,ownlocty," ",ownlos," ",ownlozc
               branch    cvflag of cvexit
               MOVE      ORTNNUM TO NRTNFLD
               REP       ZFILL IN NRTNFLD
               CALL      NRTNKEY
               CALL      CNTRTN
               PACK      MKEY  FROM OMLRNUM,z3
.               CALL      NMLRKEY
.Begin Patch 9.34
.           call      nmlrkey
                                                  pack                compfld3 from mkey
                                                  call                COMPKEY3
.End Patch 9.34
.START PATCH 9.4.4 ADDED LOGIC
                                                  if (CNCTINACTIVE = "T")
                                                            clear     cnctfname
                                                  endif
.END PATCH 9.4.4 ADDED LOGIC
                compare    c0 to nmrgnnet
               if         not equal
               move       nmrgnnet to nmrgnet
               endif
               PACK      ODATE FROM OODTEM,SLASH,OODTED,SLASH,OODTEC,OODTEY
               MOVE      NMRGNET TO CALCmPER
               MOVE      NMRGIQTY TO N7
               DIVIDE    N7 INTO CALCmPER
               MULT      "100" BY CALCmPER
               MOVE      C0 TO PERCENT
               ADD       CALCmPER TO PERCENT
               ADD        NMRGID TO TOTREJ
               ADD        NMRGERR TO TOTREJ
               ADD        NMRGDISF TO TOTREJ
               ADD        NMRGNPER TO TOTREJ
               ADD        NMRGDMA TO TOTREJ
               ADD        NMRGZ4 TO TOTREJ
               ADD        NMRGPRIS TO TOTREJ
               ADD        NCOAMNF TO TOTREJ
               MOVE      invdtem TO MM
               MOVE      invdted TO DD
               MOVE      invdtey TO YY
               CALL      CVTJUL
               compare   juldays to febdat
               goto      nocust if equal
               if        not less
               move      yes to feb
               ADD        NMRGCUST TO TOTREJ  *turned off if billed after 2/08/96
               endif
nocust
.Begin Patch 9.35
               ADD        NMRGDPV TO TOTREJ       .DPV DROPS - PIDI 6/24/04
.End Patch 9.35
.Begin Patch 10.31
               ADD        NMRGDisa TO TOTREJ       .Disaster DROPS
.End Patch 10.31
               add        nmrgconv to totrej
.Begin Patch 10.51
               ADD        NMRGCNR TO TOTREJ       .cnr MATCHES Smile Train
.End Patch 10.51

               MOVE       NMRGRQTY TO TOTBILL
               SUB        TOTREJ FROM TOTBILL
               add        ncoanix1 to totncoa
               add        ncoanix2 to totncoa
               add        ncoanix3 to totncoa
               PrtPage        Laser;*p=1500:3275,"COMPUTER VERIFICATION":
                              *p=rtntab:3400,Rtcomp," MERGE":
.Begin Patch 9.34
                              *p=50:3525,"Mailer: ",COMPCOMP,*p=3000:3525,"Order Date: ",odate:
.                              *p=50:3525,"Mailer: ",COMPCOMP,*p=2600:3525,"Order Date: ",odate:
.                              *p=50:3525,"Mailer: ",mcomp,*p=2600:3525,"Order Date: ",odate:
.eND Patch 9.34
                              *p=50:3650,"List:   ",O1des,*p=3000:3650,"LR ##     : ",olrn:
                              *p=50:3775,"Input Qty: ",*p=2250:3775,*alignment=*right,NMrgrQty,*alignment=*left,*p=3000:3775,"M/P ## ",oMlrKy:
                              *p=50:3900,"NET OUTPUT: ",*p=2250:3900,NMRGNET,*alignment=*left,*p=3000:3900,"% NAMES MAILED ",PERCENT:
                              *p=50:4025,"TOTAL BILLABLE NAMES: ",*p=3000:4025,*alignment=*right,TOTBILL,*alignment=*left
.                              *p=50:3650,"List:   ",O1des,*p=2600:3650,"LR ##     : ",olrn:
.                              *p=50:3775,"Input Qty: ",*p=2250:3775,*alignment=*right,NMrgrQty,*alignment=*left,*p=2600:3775,"M/P ## ",oMlrKy:
.                              *p=50:3900,"NET OUTPUT: ",*p=2250:3900,NMRGNET,*alignment=*left,*p=3000:3900,"% NAMES MAILED ",PERCENT:
.                              *p=50:4025,"TOTAL BILLABLE NAMES: ",*p=3000:4025,*alignment=*right,TOTBILL,*alignment=*left
               mOVE           "4150" To VerticalPos
         COMPARE    C0 TO NMRGID
         IF         NOT EQUAL
               Add            "125" to VerticalPos
               PrtPage        Laser;*p=50:VerticalPos,"INTRA DUPES: ",*p=3000:VerticalPos,*alignment=*right,NMRGID,*alignment=*left
         ENDIF
         COMPARE    C0 TO NMRGERR
         IF         NOT EQUAL
               Add            "125" to VerticalPos
               PrtPage        Laser;*p=50:VerticalPos,"ERROR REJECTS: ",*p=3000:VerticalPos,*alignment=*right,NMRGerr,*alignment=*left
         ENDIF
         COMPARE    C0 TO NMRGdisf
         IF         NOT EQUAL
               Add            "125" to VerticalPos
               PrtPage        Laser;*p=50:VerticalPos,"DECEASED REJECTS: ",*p=3000:VerticalPos,*alignment=*right,NMRGDISF,*alignment=*left
         ENDIF
         COMPARE    C0 TO NMRGNPER
         IF         NOT EQUAL
               Add            "125" to VerticalPos
               PrtPage        Laser;*p=50:VerticalPos,"NONPERSONAL REJECTS: ",*p=3000:VerticalPos,*alignment=*right,NMRGNPER,*alignment=*left
         ENDIF
         COMPARE    C0 TO NMRGDMA
         IF         NOT EQUAL
               Add            "125" to VerticalPos
               PrtPage        Laser;*p=50:VerticalPos,"DMA REJECTS: ",*p=3000:VerticalPos,*alignment=*right,NMRGDMA,*alignment=*left
         ENDIF
         COMPARE    C0 TO NMRGZ4
         IF         NOT EQUAL
               Add            "125" to VerticalPos
               PrtPage        Laser;*p=50:VerticalPos,"ZIP+4 CRT REJECTS: ",*p=3000:VerticalPos,*alignment=*right,NMRGZ4,*alignment=*left
         ENDIF
         COMPARE    C0 TO NMRGPRIS
         IF         NOT EQUAL
               Add            "125" to VerticalPos
               PrtPage        Laser;*p=50:VerticalPos,"PRISON REJECTS: ",*p=3000:VerticalPos,*alignment=*right,NMRGPRIS,*alignment=*left
         ENDIF
         COMPARE    C0 TO NCOAMNF
         IF         NOT EQUAL
               Add            "125" to VerticalPos
               PrtPage        Laser;*p=50:VerticalPos,"NCOA MTCH NON FWD: ",*p=3000:VerticalPos,*alignment=*right,NCOAMNF,*alignment=*left
         ENDIF
         cmatch     yes to feb
         if         equal
         COMPARE    C0 TO NMRGCUST
         IF         NOT EQUAL
               Add            "125" to VerticalPos
               PrtPage        Laser;*p=50:VerticalPos,"CUSTOMER REJECTS: ",*p=3000:VerticalPos,*alignment=*right,NMRGCUST,*alignment=*left
         ENDIF
         endif
         COMPARE    C0 TO NMRGconv
         IF         NOT EQUAL
               Add            "125" to VerticalPos
               PrtPage        Laser;*p=50:VerticalPos,"CONVERSION REJECTS: ",*p=3000:VerticalPos,*alignment=*right,NMRGConv,*alignment=*left
         ENDIF
.Begin Patch 9.35
         COMPARE    C0 TO NMRGdpv
         IF         NOT EQUAL
               Add            "125" to VerticalPos
               PrtPage        Laser;*p=50:VerticalPos,"DPV DROPS: ",*p=3000:VerticalPos,*alignment=*right,NMRGDPV,*alignment=*left
         ENDIF
.Begin Patch 9.35
.Begin Patch 10.3
         COMPARE    C0 TO NMRGdisa
         IF         NOT EQUAL
               Add            "125" to VerticalPos
               PrtPage        Laser;*p=50:VerticalPos,"DISASTER DROPS: ",*p=3000:VerticalPos,*alignment=*right,NMRGDisa,*alignment=*left
         ENDIF
.Begin Patch 10.3
.Begin Patch 10.51
         COMPARE    C0 TO NMRGCNR
         IF         NOT EQUAL
               Add            "125" to VerticalPos
               PrtPage        Laser;*p=50:VerticalPos,"CNR MATCHES: ",*p=3000:VerticalPos,*alignment=*right,NMRGCNR,*alignment=*left
         ENDIF
.Begin Patch 10.51
         COMPARE    C0 TO NMRGrep
         IF         NOT EQUAL
         move       yes to rep
         endif
         COMPARE    C0 TO NMRGelmx
         if         not equal
         goto       printrep
         else
         cmatch     yes to rep
                         if         equal
printrep
                         Add            "125" to VerticalPos
                        PrtPage        Laser;*p=400:VerticalPos,"CNR REJECTS: ",*p=650:VerticalPos,NMRGElmx
                        Add            "250" to VerticalPos
                        PrtPage        Laser;*p=50:VerticalPos,"NOTE: INPUT QTY MINUS TOTAL REJECTS,"
                        Add            "125" to VerticalPos
                        PrtPage        Laser;*p=50:VerticalPos,"ELIMINATOR, TDMC, NCOA TOTAL , MAILDROP,"
                        Add            "125" to VerticalPos
                        PrtPage        Laser;*p=50:VerticalPos,"UNUSED DUPES, NIXIE, FAMILY, HOUSE HITS,"
                        Add            "125" to VerticalPos
                        PrtPage        Laser;*p=50:VerticalPos,"INDEPENDENT REJECTS, CNR REJECTS,"
                        Add            "125" to VerticalPos
                        goto           CVFInish
                        endif
         endif
PRINTREG
               Add            "250" to VerticalPos
               PrtPage        Laser;*p=50:VerticalPos,"NOTE: INPUT QTY MINUS TOTAL REJECTS,"
               Add            "125" to VerticalPos
               PrtPage        Laser;*p=50:VerticalPos,"ELIMINATOR, TDMC, NCOA TOTAL , MAILDROP,"
               Add            "125" to VerticalPos
               PrtPage        Laser;*p=50:VerticalPos,"UNUSED DUPES, NIXIE, FAMILY, HOUSE HITS,"
CVFinish
         cmatch     yes to feb
         if         not equal
               Add            "125" to VerticalPos
               PrtPage        Laser;*p=50:VerticalPos,"CUSTOMER SUPPRESS, CUSTOMER REJECTS = NET OUTPUT"
         else
               Add            "125" to VerticalPos
               PrtPage        Laser;*p=50:VerticalPos,"CUSTOMER SUPPRESS = NET OUTPUT"
         endif
               Add            "375" to VerticalPos
               PrtPage        Laser;*p=20:VerticalPos,str2
.              print      *f,hpprop,*flush
cvexit
               PrtPage        Laser;*NewPage
               Return
.
CNTRTN   SETLPTR   RTCOMP
         ENDSET    RTCOMP
CHKRHEAD CMATCH    B1 TO RTCOMP
         GOTO      SETRHEAD IF NOT EQUAL
         BUMP      RTCOMP BY -1
         GOTO      CHKRHEAD IF NOT EOS
SETRHEAD MOVEFPTR  RTCOMP TO N3
         MOVE      "80" TO RTNTAB
         SUBTRACT  N3 FROM RTNTAB
         DIVIDE    C2 INTO RTNTAB
         Mult      "10" by RTNTab
         RESET     RTCOMP
         SETLPTR   RTCOMP
         RETURN
...............................................................................
WIPEVARS
          branch    hotflag of wipevar1
          compare   c2 to faxflag
          if not equal
                    prtclose  laser
.START PATCH 10.2 ADDED LOGIC
                    if (!externalMode)
.END PATCH 10.2 ADDED LOGIC
.Give the email a chance of rendering itself before updating the INI file.
                              if (PRTFLAG = "P" or PRTFLAG = "p")
.                                        pack      APIFileName,"c:\progra~1\pdf995\flag.dat",hexzero
.                                        loop
.                                                  call      FindFirstFile
.                                                  until (APIResult = 0 | APIResult = hexeight)
.                                                  pause     "1"
.                                        repeat
.                                        pause     "2"
.                                        erase     "c:\progra~1\pdf995\flag.dat"
                              endif
.START PATCH 10.2 ADDED LOGIC
                    endif
.END PATCH 10.2 ADDED LOGIC
.begin patch 9.5
.                             call      CreatePDFFile
//            call    CreatePDFFile
//
// 22 DEC 2004 BJACKSON --> if we're using the webgenerate external, we don't need to
//                           worry about emailing anything, just return to where we
//                           came from
//
    if (externalMode)
    else
        call createPDFFile
    endif
//
// 22 DEC 2004 BJACKSON --> end of changes
//
.end patch 9.5
                    endif
.          endif
wipevar1
           CLEAR     SHORT
           CLEAR     GUARPRT
           CLEAR     GUARDATE
           CLEAR     MTXEXMPT
         MOVE        C0 TO TOTREJ
         MOVE        C0 TO TOTNCOA
         move        no to feb
         move        no to rep
         move      c0 to irnetper
         move      c0 to nmrgrqty
         call      wipecvars
...

         BRANCH    hotflag TO INPUT,EOJ
           GOTO      INPUT
testprt
         call      prtinvfrm
         prtpage              Laser;*newpage
.temp
.               stop
          goto     input
.
EOJ
         BRANCH    NINVFRMFLAG TO MLREOJ,remiteoj
done
               prtclose    laser
.START PATCH 10.2 ADDED LOGIC
                    if (!externalMode)
.END PATCH 10.2 ADDED LOGIC
.Give the email a chance of rendering itself before updating the INI file.
                              if (PRTFLAG = "P" or PRTFLAG = "p")
.                                        pack      APIFileName,"c:\progra~1\pdf995\flag.dat",hexzero
.                                        loop
.                                                  call      FindFirstFile
.                                                  until (APIResult = 0 | APIResult = hexeight)
.                                                  pause     "1"
.                                        repeat
.                                        pause     "2"
.                                        erase     "c:\progra~1\pdf995\flag.dat"
                              endif

                              if (PRTFLAG = "p")            .PDF
                                        call      CreatePDFFile
                              endif
.START PATCH 10.2 ADDED LOGIC
                    endif
.END PATCH 10.2 ADDED LOGIC

.begin patch 9.5
//
// 22 DEC 2004 BJACKSON --> added code to return to our external hook if the
//  program was started as an external.
//
    if (externalmode)
        return
    else
               shutdown  "cls"
    endif
//
// 22 DEC 2004 BJACKSON --> end of changes
//

.               shutdown  "cls"
               STOP
.
MLREOJ
         move       C2 TO NINVFRMFLAG
         display   *p10:14,"Mailer remittance COPY"
         compare   c2 to hotflag
         if        not equal
                    CLOSE     Pinvoice
                    prtclose    laser
.START PATCH 10.2 ADDED LOGIC
                    if (!externalMode)
.END PATCH 10.2 ADDED LOGIC
.Give the email a chance of rendering itself before updating the INI file.
                              if (PRTFLAG = "P" or PRTFLAG = "p")
.                                        pack      APIFileName,"c:\progra~1\pdf995\flag.dat",hexzero
.                                        loop
.                                                  call      FindFirstFile
.                                                  until (APIResult = 0 | APIResult = hexeight)
.                                                  pause     "1"
.                                        repeat
.                                        pause     "2"
.                                        erase     "c:\progra~1\pdf995\flag.dat"
                              endif

                              if (PRTFLAG = "p")            .PDF
                                        call      CreatePDFFile
                              endif
.START PATCH 10.2 ADDED LOGIC
                    endif
.END PATCH 10.2 ADDED LOGIC
         PACK      STR45,NTWKPATH1,"NINV2RMT.lst"
               call           PRTOPENPrep
         OPEN      Pinvoice,inpname
         move      c0 to count
         goto      input
         else
         goto      remiteoj
         endif
.
remitEOJ move       C3 TO NINVFRMFLAG
         display   *p10:14,"OWNer COPY"
         compare   c2 to hotflag
         if        not equal
                    CLOSE     Pinvoice
                    prtclose    laser
.START PATCH 10.2 ADDED LOGIC
                    if (!externalMode)
.END PATCH 10.2 ADDED LOGIC
.Give the email a chance of rendering itself before updating the INI file.
                              if (PRTFLAG = "P" or PRTFLAG = "p")
.                                        pack      APIFileName,"c:\progra~1\pdf995\flag.dat",hexzero
.                                        loop
.                                                  call      FindFirstFile
.                                                  until (APIResult = 0 | APIResult = hexeight)
.                                                  pause     "1"
.                                        repeat
.                                        pause     "2"
.                                        erase     "c:\progra~1\pdf995\flag.dat"
                              endif
                              if (PRTFLAG = "p")            .PDF
.                             call      CreatePDFFile
.                   endif
.begin patch 9.5
//**
                                        call    CreatePDFFile if (!externalMode)
                              endif
.START PATCH 10.2 ADDED LOGIC
                    endif
.END PATCH 10.2 ADDED LOGIC
.                             call      CreatePDFFile
.                   endif
.end patch 9.5

         PACK      STR45,NTWKPATH1,"NINV2OWN.lst"
               call           PRTOPENPrep
         OPEN      Pinvoice,"Ninvprt.OWN"
         move      c0 to count
         goto      input
         else
               prtclose    laser
.START PATCH 10.2 ADDED LOGIC
                    if (!externalMode)
.END PATCH 10.2 ADDED LOGIC
                              if (PRTFLAG = "P" or PRTFLAG = "p")
.Give the email a chance of rendering itself before updating the INI file.
.                                        pack      APIFileName,"c:\progra~1\pdf995\flag.dat",hexzero
.                                        loop
.                                                  call      FindFirstFile
.                                                  until (APIResult = 0 | APIResult = hexeight)
.                                                  pause     "1"
.                                        repeat
.                                        pause     "2"
.                                        erase     "c:\progra~1\pdf995\flag.dat"
                              endif
                              if (PRTFLAG = "p")            .PDF
.begin patch 9.5
//**
                                        call    CreatePDFFile if (!externalMode)
                              endif
.START PATCH 10.2 ADDED LOGIC
                    endif
.END PATCH 10.2 ADDED LOGIC

.                             call      CreatePDFFile
.                   endif
//
// 22 DEC 2004 BJACKSON --> added code to return to our external hook if the
//  program was started as an external.
//
    if (externalmode)
        return
    endif
//
// 22 DEC 2004 BJACKSON --> end of changes
//;end patch 9.5
      goto      process
         endif
................................................................
prtinvfrm
..hi English 1000 units to an inch
..               prtpage               Laser;"D 14154337796   NAndrew Harkins SDavid Herrick  (510) 302-4660  !^]":
               prtpage               Laser;*units=*HIENGLISH,*ROWSPACE=0,*COLSPACE=0,*Overlayon
.                              *p=3250:125,*font=FontLogob,"Names":
.                              *font=Font018I,"in the News":
.                              *PENSIZE=10,*p=2652:400,*Line=5652:400:
.                              *p=3250:450,*font=Font010,"C  A  L  I  F  O  R  N  I  A     I  N  C .":
.                              *p=3207:693,*font=FontLogo,"1300 Clay Street, 11th Floor, Oakland, CA 94612-1429":
.                              *p=3207:693,*font=Font07,"1300 Clay Street, 11th Floor, Oakland, CA 94612-1429":
.                              *p=3489:793,"415-989-3350 ","·"," Fax 415-433-7796":
.begin patch 10.54
.
.begin patch 10.5.
.         IF        (OCompID = "P")
.         call      debug
..begin patch 10.51
.         prtpage   Laser;*p=93:25,*font=fontO18b,"Pacific Lists, Inc.":
.                   *p=500:343,*font=font07,"1300 Clay St. 11th Floor":
.                   *p=400:443,"Oakland, CA 94612-1429":
.                   *p=335:543,"415-945-9450 ","·"," Fax 415-945-9451":
.                   *p=335:643,"A Division of Names in the News"
..        prtpage   Laser;*p=93:25,*font=fontO18b,"Pacific Lists, Inc.":
..                  *p=500:343,*font=font07,"100 Tamal Plaza, Suite 50":
..                  *p=400:443,"Corte Madera, CA 94925-1182":
..                  *p=335:543,"415-945-9450 ","·"," Fax 415-945-9451":
..                  *p=335:643,"A Division of Names in the News"
..end patch 10.51
.         Else
.               prtpage               Laser;*Pictrect=*off,*PICT=0:1000:375:3375:NINLogo
.         endif
.                              *Pictrect=*off,*PICT=0:1000:375:3375:NINLogo:
.end patch 10.5
.end patch 10.54

               prtpage               Laser;*RECT=1115:1340:3750:4500:
                              *p=3785:1125,*font=Font014BI,"Invoice":
                              *p=500:1962,*font=Font08,*line=7580:1962:                      ;top Hori line
                              *p=500:1962,*line=500:3462:                          ;left side (top)
                              *p=7580:1962,*line=7580:3462:                        ;right side (top)
                              *p=500:3522,*line=7580:3522:                      ;Middle Hori line
                              *p=500:3522,*line=500:4522:                          ;left side (Middle)
                              *p=7580:3522,*line=7580:4522:                        ;right side (Middle)
                              *RECT=4582:10582:500:7580:                           ;bottom section drawn as rectangle
                              *p515:10400,*font=Font09Bi,"Payment due upon receipt. Please return copy with payment.":
                              *p5500:10400,*font=Font09i,"Member Direct Marketing Association"
.                              *p5560:10400,*font=Font09i,"Member Direct Marketing Association"
               prtpage               Laser;*units=*HIENGLISH,*ROWSPACE=0,*COLSPACE=0,*Overlayon:
                              *p750:2212,*font=Font07Dot5I,"Date:":
                              *p4250:2212,"Invoice##":
                              *p5750:2212,"Mailer's P.O.":
                              *p750:2400,"Client##":
                              *p5750:2712,"NIN LR ##":
                              *p5750:3212,"Mail Date:":
                              *p750:3662,"Mailer's Offer:":
                              *p750:3912,"Key:":
                              *p750:4162,"List:":
                              *p=4250:4712,"$ Per M":
                              *p=5750:4712,"Amount Due"
               if             (NINVFrmFlag <> c3)    ;not list owner copy
               prtpage               Laser;*units=*HIENGLISH,*ROWSPACE=0,*COLSPACE=0,*Overlayon:
                              *p=5750:10166,*font=Font09B,"Total Due:":
                              *font=Font07Dot5I
               endif
               return
.begin patch 10.54
PRtNINLogo
              prtpage               Laser;*Pictrect=*off,*PICT=0:1000:375:3375:NINLogo
          return
PRtPLILogo
          prtpage   Laser;*p=93:25,*font=fontO18b,"Pacific Lists, Inc.":
                    *p=500:343,*font=font07,"180 Grand Avenue, Suite 1365":
                    *p=400:443,"Oakland, CA 94612-3716":
                    *p=335:543,"415-945-9450 ","·"," Fax 415-945-9451":
                    *p=335:643,"A Division of Names in the News"
          return
.end patch 10.54
PrtPaymentReq
               prtpage        Laser;*p515:10166,*font=font09,"Note: List owner requires payment of this invoice before":
                              *p515:10283,*font=font09,"additional orders for this mailer can be processed."
               Return

..prtmlrboxGui Routine Laser
prtmlrboxGui
.begin patch 10.54
          clear     str2
          pack      str2 from osales10,osales
.begin patch 10.55
.         IF        (Ocompid = "P")
.         IF        (Ocompid = "P" & (Ocompid2 <> "N" & str2 <> "06"))
.begin patch 10.7
.          IF        (Ocompid = "P" & str2 <> "06")
.          call      PrtPLILOgo
..         Elseif    (Ocompid2 = "P" & (str2 = "27" | str2 = "28"))
.          Elseif    (Ocompid2 = "P" & (str2 = "27" | str2 = "28"| str2 = "06"))
..end patch 10.55
.          call      PrtPLILOgo
.          else
          call      PrtNINLOgo
.          endif
.end patch 10.54
.end patch 10.7
               prtpage        Laser;*units=*HIENGLISH,*ROWSPACE=0,*COLSPACE=0,*Overlayon:
                              *p=3650:1625,*font=Font014b,"Client Copy"
               return
.
..prtRemitboxGui Routine Laser
prtRemitboxGui
.begin patch 10.54
          clear     str2
          pack      str2 from osales10,osales
.begin patch 10.7
.          IF        (Ocompid = "P")
.          call      PrtPLILOgo
.          Elseif    (Ocompid2 = "P" & (str2 = "27" | str2 = "28"))
.          call      PrtPLILOgo
.          else
          call      PrtNINLOgo
.          endif
.end patch 10.7
.end patch 10.54
               prtpage        Laser;*units=*HIENGLISH,*ROWSPACE=0,*COLSPACE=0,*Overlayon:
                              *p=3400:1625,*font=Font014b,"Remittance Copy"
               return
.
..prtownerboxGui Routine Laser
prtownerboxGui
.begin patch 10.54
.begin patch 10.7
.          IF        (Ocompid = "P" & OCompid2  <> "N")
.          call      PrtPLILOgo
.          ElseIF    (Ocompid2 = "P")
.          call      PrtPLILOgo
.          else
          call      PrtNINLOgo
.          endif
.end patch 10.7
.end patch 10.54
               getitem        GreyFill,0,colornum
               prtpage         Laser;*units=*HIENGLISH,*ROWSPACE=0,*COLSPACE=0,*Overlayon:
                               *Fill=*On:
                               *Bgcolor=Colornum:
                              *RECT=1527:1902:2652:5652:
                              *Fill=*Off
               getitem        NoFill,0,colornum
               prtpage        Laser;*Bgcolor=Colornum:
                              *p2750:1600,"Amount Due: ",Paytoo:
                              *p5500:1750,*Alignment=*right,M$Ap1,*Alignment=*left


               return
...............................................................................
.PRTOPENPREP - open print destination
PRTOPENPREP
               Branch          hotflag of ToPrinter,ToFIle
.ToPRinter - send directly to a printer
ToPRinter
               if             (OSFLAG = "1" or OSFLAG = "5" or OSFLAG = "6" or OSFLAG = "8" or OSFLAG = "9")  .NT4,NT5,XP,vista, win7
.               PRTOPEN        Laser,"\\NINS2\laser8",str45
.               PRTOPEN        Laser,"\\NINS2\laser8",str45
               PRTOPEN        Laser,"\\NINs2\laser2",str45
                              ELSEIF (OSFLAG = "3" or OSFLAG = "4")             .95/98
.                              PRTOPEN       Laser,"Laser8",str45
                              PRTOPEN       Laser,"Laser2",str45
                              else   .(osflag = c0)         .Don't know prompt for printer
                         PRTOPEN       Laser,"-",str45
               endif
               Return
.ToFile - Spool to file
ToFile
..........................................................
.
          if (PRTFLAG = "P")            .PDF
.>Patch 3.38 Logic Addition for PDF Quality Control
.START PATCH 10.2 ADDED LOGIC
                    if (!externalMode)
.END PATCH 10.2 ADDED LOGIC
.begin patch 11.1
.                              call PDF995Auto
.end patch 11.1

.                              call      GetPdfPath
.                              pack      str55 from PdFpath,"\res\pdf995.ini"
.                              call      "GU$INI;WRITE_TO_INI" USING str55:
.
./                              call      "GU$INI;WRITE_TO_INI" USING "c:\progra~1\pdf995\res\pdf995.ini":
.                                        "Parameters":
.                                        "ProcessPDF":
.                                        "\\nins1\e\apps\plb\code\pdftest.bat":
.                                        result
.                              if (result = C0)
..Prepare Flag file
..                                        prep      tempfile,"c:\progra~1\pdf995\flag.dat"
..                                        write     tempfile,SEQ;"flag set"
..                                        close     tempfile
.                              endif
..First check 995 autolaunch settings
.                              move      "PDF995 Copy",Filename
.                              pack      KeyLocation,"Prepping FIle "
.                              Open           TestFile,"c:\progra~1\pdf995\res\pdf995.ini"
.                              Prepare        TempFile,"c:\progra~1\pdf995\res\pdf995.out"
.                              Loop
.                                        move      "PDF995 Copy",Filename
.                                        pack      KeyLocation,"Reading FIle "
.                                        Read           TestFile,seq;taskname
.                                        Until          Over
.                                        Scan           "Autolaunch=1" in taskname
.                                        If             equal
.                                                  move           Yes to Reset995flag
.                                                  reset          taskname
.                                                  clear          taskname
.                                                  Move           "Autolaunch=0" to taskname
.                                        endif
.                                        move      "PDF995 Copy",Filename
.                                        pack      KeyLocation,"Writing FIle "
.                                        write          tempfile,Seq;taskname
.                              Repeat
.                              move      "PDF995 Copy",Filename
.                              pack      KeyLocation,"WEOF FIle "
.                              weof           tempfile,seq
.                              move      "PDF995 Copy",Filename
.                              pack      KeyLocation,"Closing Output FIle "
.                              close          tempfile
.                              move      "PDF995 Copy",Filename
.                              pack      KeyLocation,"Closing Input FIle "
.                              close          testfile
.                              move      "PDF995 Copy",Filename
.                              pack      KeyLocation,"Erasing Save FIle "
.                              Erase          "c:\progra~1\pdf995\res\pdf995.Sav"
.                              move      "PDF995 Rename",Filename
.                              pack      KeyLocation,"Ren Ini to Sav "
.                              Rename         "c:\progra~1\pdf995\res\pdf995.ini","c:\progra~1\pdf995\res\pdf995.Sav"
.                              move      "PDF995 Rename",Filename
.                              pack      KeyLocation,"Ren out to Ini"
.                              Rename         "c:\progra~1\pdf995\res\pdf995.out","c:\progra~1\pdf995\res\pdf995.ini"
.
.                              else
.
.                              call      "GU$INI;WRITE_TO_INI" USING "c:\progra~2\pdf995\res\pdf995.ini":
.                                        "Parameters":
.                                        "ProcessPDF":
.                                        "\\nins1\e\apps\plb\code\pdftest.bat":
.                                        result
.                              if (result = C0)
..Prepare Flag file
..                                        prep      tempfile,"c:\progra~2\pdf995\flag.dat"
..                                        write     tempfile,SEQ;"flag set"
..                                        close     tempfile
.                              endif
..First check 995 autolaunch settings
.                              move      "PDF995 Copy",Filename
.                              pack      KeyLocation,"Prepping FIle "
.                              Open           TestFile,"c:\progra~2\pdf995\res\pdf995.ini"
.                              Prepare        TempFile,"c:\progra~2\pdf995\res\pdf995.out"
.                              Loop
.                                        move      "PDF995 Copy",Filename
.                                        pack      KeyLocation,"Reading FIle "
.                                        Read           TestFile,seq;taskname
.                                        Until          Over
.                                        Scan           "Autolaunch=1" in taskname
.                                        If             equal
.                                                  move           Yes to Reset995flag
.                                                  reset          taskname
.                                                  clear          taskname
.                                                  Move           "Autolaunch=0" to taskname
.                                        endif
.                                        move      "PDF995 Copy",Filename
.                                        pack      KeyLocation,"Writing FIle "
.                                        write          tempfile,Seq;taskname
.                              Repeat
.                              move      "PDF995 Copy",Filename
.                              pack      KeyLocation,"WEOF FIle "
.                              weof           tempfile,seq
.                              move      "PDF995 Copy",Filename
.                              pack      KeyLocation,"Closing Output FIle "
.                              close          tempfile
.                              move      "PDF995 Copy",Filename
.                              pack      KeyLocation,"Closing Input FIle "
.                              close          testfile
.                              move      "PDF995 Copy",Filename
.                              pack      KeyLocation,"Erasing Save FIle "
.                              Erase          "c:\progra~2\pdf995\res\pdf995.Sav"
.                              move      "PDF995 Rename",Filename
.                              pack      KeyLocation,"Ren Ini to Sav "
.                              Rename         "c:\progra~2\pdf995\res\pdf995.ini","c:\progra~2\pdf995\res\pdf995.Sav"
.                              move      "PDF995 Rename",Filename
.                              pack      KeyLocation,"Ren out to Ini"
.                              Rename         "c:\progra~2\pdf995\res\pdf995.out","c:\progra~2\pdf995\res\pdf995.ini"
.                              endif
.START PATCH 10.2 ADDED LOGIC
                    endif
.END PATCH 10.2 ADDED LOGIC
.;                            PRTOPEN Laser,"Acrobat Distiller",str25
.                    PRTOPEN Laser,"PDF995",str25
.begin patch 11.1
                    pack    str55,"c:\work\pdf\",str25,".pdf"
                    PRTOPEN Laser,"PDF:",str55
.end patch 11.1
.                    pack    str55,str25,".pdf"
                    move      "p",PRTFLAG
          elseif (PRTFLAG = "2")        .Sales
.                   if (PRTFLAG = "2")  .Sales
                    if (OSFLAG = "1" or OSFLAG = "5" or OSFLAG = "6")  .NT4,NT5,XP
                              PRTOPEN   Laser,"\\NINS2\laser3 Blankstock",str45
                    elseif (OSFLAG = "3" or OSFLAG = "4")             .95/98
                              PRTOPEN   Laser,"Laser3 Blankstock",str45
                    else   .(osflag = c0)         .Don't know prompt for printer
                              PRTOPEN   Laser,"-",str45
                    endif
          else                          .All others
                    if (OSFLAG = "1" or OSFLAG = "5" or OSFLAG = "6")  .NT4,NT5,XP
                              PRTOPEN   Laser,"\\NINS2\laser2",str45
                    elseif (OSFLAG = "3" or OSFLAG = "4")             .95/98
                              PRTOPEN   Laser,"Laser2",str45
                    else   .(osflag = c0)         .Don't know prompt for printer
                              PRTOPEN   Laser,"-",str45
                    endif
          endif
       Return
CreatePDFFile
.It takes some time for the file to be created, so we must check
.         move    C0,N9
.         move    "                                        ",APIFileName
.         MOVe      "NXCH0006.pdf" to str35
.         clear   APIFileName
.         pack    APIFileName,"C:\WORK\PDF\",str55,hexzero
.         Pack      STR35,"C:\WORK\PDF\",str55
.         clock   timestamp,timestamp1
.;        move    timestamp1,time1
.;        Move      str35 to str55
.         loop
.;                  clock   timestamp,timestamp2
.;                  move    timestamp2,time2
.;                  sub     time1,time2,time3
.;                  if (time3 > 1000) .10 Seconds Maximum
.         Trap      notready if IO
.         Open      tempfile,str35,exclusive
.         Close     Tempfile
.                             break
.         goto      Ready
.Begin  patch 10.6
.          loop
.          call      FindFirstFile
.          if (APIResult =     0 | APIResult =     hexeight)      .file is found
..APILastWriteTime  -- last written to we want to save this wait abit and check again
.          move      APILastWriteTime,time1
..If file not ready then do not attempt
.          Pause     "3"
.          call      FindFirstFile
.          move      APILastWriteTime,time2
.          endif
.          until     (time1 = time2)
.          repeat
.end  patch 10.6

Notready
.         trapclr   IO
.         Noreturn
.                   endif
.         repeat
ready
.START PATCH 10.2 ADDED LOGIC
          if (!externalMode)
.END PATCH 10.2 ADDED LOGIC
.begin patch 10.4
.begin patch 10.53
          if        (str55 <> "" & str55 <> " ")

          Clear     Mailbody
          append    "This is a message from       the Invoice Print Program",Mailbody
          Append    CRLF,MailBody                                               
          Append    "Your PDF file was created!",Mailbody
          Append    CRLF,MailBody                                               
          append    "Location:  c:\work\pdf\ ",mailbody
          Append    CRLF,MailBody                                               
          append    "filename:  ",mailbody
          append    str55,mailbody
          Append    CRLF,MailBody                                               
          Reset     MailBody
                    
.begin patch 11.1
.          pack      mailattach,"c:\work\pdf\",str55         ."
          pack      mailattach,str55         ."
.end patch 11.1
.Begin  patch 10.6
CheckFile
          trap      WaitForEnd giving error if IO
          open      FileCheck,Mailattach,Exclusive     
          Close     FIleCHeck
          trap      IOMssg GIVING ERROR IF IO
.          call      SendMail
.end patch 10.6
          pack      Mailto,userlogn,"@nincal.com"
          pack      Mailfrom,userlogn,"@nincal.com"
          move      "Here is your PDF File",Mailsubjct
.          PAUSE     "10"
.begin patch 11.1
.          pack       APIFileName,"c:\work\pdf\",str55,HexZero            ."
.end patch 11.1
          call      SendMail
          endif     
.end patch 10.53
          


.         move      "Here is your PDF File",SmtpSubject Subject
.   Set the text message that is send with the attachments
.         move      str55,SmtpTextMessage(1)   Array <Text message >
.         move      "1",SmtpTextIndexLast                               Index to last entry in TextMessage array
.         move      "NTS4",SmtpEmailServer                   Address of email serverc
.         clear     SmtpUserName
.               Move           Userlogn to SmtpUserFullName
.         clear     smtpemailaddress
.         append    userlogn,SmtpEmailAddress
.         append    "@nincal.com",SmtpEmailAddress
.         reset     smtpemailaddress
.         move      userlogn,SmtpUserName                                User name
.   Set the destinations of the email. Max 100 (Mime spec)
.         move      smtpemailaddress,SmtpDestinations(1,1)
.         move      userlogn,SmtpDestinations(1,2)
..        move      "1",SmtpDestIndexLast                          originators UserName
.         move      str55,SmtpAttachments(1,1)                     Attached file name
.         move      "C:\WORK\PDF",SmtpAttachments(1,2)           Path to attached file name
.         move      "1",SmtpAttIndexLast                                Index to last entry - Only 1 entry
.         move      "C:\work\eMail.Log",SmtpLogFile          Path/filename to Log all socket read/writes
.         clear     SmtpLogFile                                         'Clear' disables the LogFile
.         move      "1",SmtpProgress                                    Enable progress bars
.         call      SmtpSend   ( 'Send' is in Smtp.Pri which is included in TestSmtp.Dbs )
.         if not equal
.                   pack      Mess,"Result Code ",SmtpResult," - ",SmtpResultText,NewLine:
.                             "Status Code ",SmtpStatus," - ",SmtpStatusText
.                   move      "PDF File not found",SmtpSubject Subject
.                   move      "0",SmtpAttIndexLast                                Index to last entry - Only 1 entry
.                   call      SmtpSend   ( 'Send' is in Smtp.Pri which is included in TestSmtp.Dbs )
..                  Alert  Stop,Mess,F1,"PL/B emailer - ERROR"
.         else
..                  Alert  Stop,"eMail transmitted OK",F1,"PL/B emailer"
.         endif
.end patch 10.4
.Clean up afterwards
.begin patch 10.0
.begin patch 11.1
.               if             (Reset995Flag = yes)
.               call           PDF995Auto0

..               move "PDF995 Restore",Filename
..               pack KeyLocation,"Erase temp ini FIle "
..               Erase          "c:\progra~1\pdf995\res\pdf995.ini"
..               move "PDF995 Restore",Filename
..               pack KeyLocation,"Ren ini  to Save "
..               Rename         "c:\progra~1\pdf995\res\pdf995.Sav","c:\progra~1\pdf995\res\pdf995.ini"
.               endif
.end patch 11.1
.end patch 10.0
.moved to Ninv0001.pls   DLH  20May 2004
.START PATCH 10.2 ADDED LOGIC
          endif
.END PATCH 10.2 ADDED LOGIC
          move      "P",PRTFLAG
          return
.begin patch 9.5
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
webGenerate routine lrNumber

    set externalmode
    move "NINV002L" to program
    pack INPNAME from lrNumber,"P","WEB"
    move "2" to hotflag
    call start

    return
///////////////////////////////////////////////////////////////////////////////
.end patch 9.5
.Begin patch 10.6
WaitForEnd
                    TrapClr   IO
.check the error if file does not exist just get out
                    add       C1,Trapcount
                    pause     c5
                    noreturn
.                   if        (trapcount > 240)   . 20 min are you kidding me
                   if        (trapcount > 60)   . 5 min are you kidding me
.                    if        (trapcount > 36)   . 3 min are you kidding me
                    Pack       MailSubjct,"Invoice PDF - ",str25
                    Move      "CReques@nincal.com",MailTO
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
.end patch 10.6
...............................................................................
           INCLUDE   NMTXIO.INC
           INCLUDE   NRTXIO.INC
.           INCLUDE   NMLRIO.INC
           INCLUDE   NBILIO.INC
           INCLUDE   NOWNIO.INC
           INCLUDE   NORDIO.INC
           INCLUDE   NACDIO.INC
         include     nofrio.inc
         include   npayio.INC
.         include   nbrkio.INC
.begin patch 10.0
               include        compute.inc
               INCLUDE        ninvio.inc
               Include        NInvAcdIO.inc
                              include   compio.inc
                              include   cntio.inc
               Include        Nsel2io.inc
.end patch 10.0
         include   nmrgio.INC
         include   nrtnio.INC
         include   ndatio.INC
         include   nshpio.INC
         include   ndat3io.inc
         include   ncntio.inc
         include   nmldio.inc

           INC     COMLOGIC.INC
