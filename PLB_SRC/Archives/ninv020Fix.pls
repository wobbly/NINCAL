PC       	EQU       0
         	INCLUDE   COMMON.INC
         	INCLUDE   CONS.INC
	include   consacct.inc
         	include   xMGtdd.inc
	include	compdd.inc
	include	cntdd.inc

         	INCLUDE   NORDDD.INC
         	INCLUDE   NOWNDD.INC
         	INCLUDE   NXRFDD.INC
         	INCLUDE   NMTXDD.INC
               include        ninvdd.inc
         	include   gnxtdd.inc
         	include   ndatdd.inc
         	inc       hp.inc
Release     Init        "2.47"       DLH internal index
REldate	INit	"23 April 2008"
ORDPRINT IFILE     KEYLEN=6,FIXED=696,Name="NINPRINT.isi|10.10.30.103:502"
TEMP     FILE                           .TEMP WORK FILE
input    file                           .additional handle for master file
REcsin   form      5                    .Records in
recsslct form      5                    .Records Selected
billed   form      5
daternge form      5                    .Date of records wanted Julian
Perm     dim       6
.START PATCH 1.6 - REPLACED LOGIC
.Following var no longer used - replaced with form92
.form62   form      6.2
.form74   form       7.4
.form94   form       9.4
.END PATCH 1.6 - REPLACED LOGIC
.form92   form      9.2
permask  init      "$$9.99"
.START PATCH 1.6 - REPLACED LOGIC
.EXAR     dim       10
.ARMask   init      "$$$,$$9.99"
.TOtar    form      6.2
.totqty   form      7
.qtymask  init      "Z,ZZZ,Z99"
.qtyout   dim       9
.GTOtar   form      7.2
.EXGAR     dim       12
.ARGMask   init      "$,$$$,$$9.99"
.Gtotqty  form      8
.Gqtymask  init      "ZZ,ZZZ,Z99"
.Gqtyout   dim       10
EXAR     dim       13
ARMask   init      "$$,$$$,$$9.99"
TOtar    form      8.2
totqty   form      9
qtymask  init      "ZZZ,ZZZ,Z99"
qtyout   dim       11
qtyout2   dim       11
GTOtar   form      9.2
EXGAR     dim       14
ARGMask   init      "$$$,$$$,$$9.99"
Gtotqty  form      10
Gqtymask  init      "Z,ZZZ,ZZZ,Z99"
Gqtyout   dim       13
.END PATCH 1.6 - REPLACED LOGIC
SPCL7    DIM       2
SPCL8    DIM       2
SPCL9    DIM       2
DESC0L1  DIM       47                    .'SPEC INSTRUC-DESCRIP'
DESC0L2  DIM       47                    .SPEC INSTRUC-DESCRIP
DESC991  DIM       47                    .SPEC INSTRUC-DESCRIP
DESC992  DIM       47                    .SPEC INSTRUC-DESCRIP
DESC981  DIM       47                    .SPEC INSTRUCTION DESc
DESC982  DIM       47                    .SPEC INSTRUCTION DESC
Lines    form      2
Hown     dim       4
Holdlist dim       6
holdlr   dim       6
holdrate form      3.2
Jan      init      "January"
feb      init      "February"
mar      init      "March"
Apr      init      "April"
may      init       "May"
Jun      init       "June"
Jul      init       "July"
Aug      init       "August"
Sep      init       "September"
oct      init        "October"
Nov      init       "November"
Dec      init       "December"
Month    dim        9
minqty   form       "000003750"
maxqty   form       "000100000"
xmgtbill form       9
.begin patch 2.01
.ratetwo  init       "011507-002303-002700"
.ratetwo  init       "011507-002303-002700-020411-012594"
.ratetwo  init       "011507-002700-012594"
.begin patch 2.4
.ratetwo  init       "011507-002700"
.end patch 2.4
.ratetwo  init       "002700"
.end patch 2.42
ratetwo  init       " "
.end patch 2.01
.end patch 2.42
rate3    init       "021666"
.end patch 2.42
.start patch 2.43
rate5    init       "000995"
.end patch 2.43
.begin patch 2.0
cap      init       "N"
capown   init       "N"
.end patch 2.0
................................................................................................
.Main
         move      "Ninv0020" to program
         MOVE      "EXCHANGE Management Fee" TO STITLE
         MOVE      "Names in the News CA" TO COMPNME
         MOVE      C1 TO NDATPATH         .SET ACCESS TO ISAM KEY List number
         MOVE      C1 TO NORDPATH         .SET ACCESS TO ISAM KEY
         move      c2 to ninvpath         .SET ACCESS TO ISAM KEY INVoice number
.set up input file, verify date being run
         clock     date to today
         unpack    today into mm,str1,dd,str1,yy
         clock     timestamp to timestamp
         unpack    timestamp into cc
         call      paint
         call      funcdisp
.         move      c1 to dd
         call       cvtjul
         move      juldays to daternge
.begin patch 1.3
         sub       c3 from daternge
.end patch 1.3
Datcheck
           open       input,"c:\work\exchmgnt04.8"
.*&*&*&*&*&*&*&*&*&*&*&*&*&*&*&*&*&*&*&*&
.Temporary - open fake main files
.        Order, Invoice, Order Print, INvoice Print, Gnxt


.begin patch 2.2
.         OPEN       PINVOICE,"NINVOICE"          .Print file for NINca invoices
.end patch 2.2
         PACK      STR35,NTWKPATH1,"ExManFee.lst"
         PACK      STR45,NTWKPATH1,"exchTemp.dat"
         splopen   STR35
         prepare   temp,STR45
.END PATCH 1.7 REPLACED LOGIC
................................................................................................
.MainLoop - read file till done

MainLoop read      input,seq;xmgtvars
         goto      eoj if over
         add       c1 to recsin
         display   *p10:10,"Records in       : ",recsin,b2,xmgtlr
         type      xmgtord
         goto      mainloop if Not equal            .blank skip iot

takeit   display    *p1:20,*el," ok"
         add       c1 to recsslct
         display   *p10:11,"Records Selected : ",recsslct,b2,xmgtlr

	packkey	xmgtfld from Xmgtlr
	call	xmgttst
	if	over
	call	xmgtwrt
	else	
	call	xmgtupd
	endif
	goto 	mainloop	



.
...............................................................................................
.Detail: print Details
Detail   COMPARE    "50" TO LINES
         CALL       HEADER IF NOT LESS
			move       no to cap
         move       permask to perm
         edit       xmgtrate to perm
         move       xmgtrate to holdrate      .if this is the last record make sure we don't lose rate
         move       armask to EXar
.START PATCH 1.6 - REPLACED LOGIC
.         move       c0 to lform74
.         move       xmgtqty to form74
.         divide     "1000" into form74
.         mult      xmgtrate by form74
.         move      c0 to form62
.         add       form74 to form62
.         edit      form62 to exar
.         add       form62 to TOTAR
         move       c0 to form94
         move       xmgtqty to form94
         divide     "1000" into form94
         mult      xmgtrate by form94
.
.begin patch 2.0
			move           c0 to xmgtbill
			add            xmgtqty to xmgtbill
.end patch 2.0
.
         RESET      RATETEN
         SCAN       OLNUM IN RATETEN
			goto       skipchk if equal

.begin patch 2.0
         RESET      RATETWO
         SCAN       OLNUM IN RATETWO
			goto       skipchk if equal
.
.begin patch 2.42
         RESET      RATE3
         SCAN       OLNUM IN RATE3
			goto       skipchk if equal
.End patch 2.42
.

.begin patch 2.43
         RESET      RATE5
         SCAN       OLNUM IN RATE5
			goto       skipchk if equal
.End patch 2.43
.
               If             (form94 < 15)
               Move           "15.00" to form94
					move           c0 to xmgtbill
					add            minqty to xmgtbill
               endif
.
               If             (XmgtRate <> 10 & form94 > 400)        ;if its not List Management only check for $400 cap
               Move           "400.00" to form94
					move           c0 to xmgtbill
					move           maxqty to xmgtbill
					move           yes to cap
					move           yes to capown
               endif
skipchk
         move      c0 to form92
         add       form94 to form92
         edit      form92 to exar
         add       form92 to TOTAR
.END PATCH 1.6 - REPLACED LOGIC
.         add       Xmgtqty to TotQTY
         add       Xmgtbill to TotQTY
         move      qtymask to qtyout
         move      qtymask to qtyout2
         edit      xmgtqty to qtyout
.			call      trim using qtyout
         edit      xmgtbill to qtyout2
			call      trim using qtyout2
         pack      mkey from omlrnum,z3
         call      nmlrkey
.         PRINT     *1,hpdtch10,oLRn,hpt175,MCOMP,hpfixed,hpt375,QTYout:
.                   hpt550,ORTNDTEM,"/",ORTNDTEd,"/",ORTNDTEy,hpdtch10
         if        (cap = yes)
         PRINT     *1,hpdtch10,oLRn,"*",hpt125,MCOMP,hpfixed,hpt400,QTYout:
.                   hpt550,ORTNDTEM,"/",ORTNDTEd,"/",ORTNDTEy,hpdtch10
                   slash,qtyout2,hpt700,ORTNDTEM,"/",ORTNDTEd,"/",ORTNDTEy,hpdtch10
			else
         PRINT     *1,hpdtch10,oLRn,hpt125,MCOMP,hpfixed,hpt400,QTYout:
.                   hpt550,ORTNDTEM,"/",ORTNDTEd,"/",ORTNDTEy,hpdtch10
                   slash,qtyout2,hpt700,ORTNDTEM,"/",ORTNDTEd,"/",ORTNDTEy,hpdtch10
			endif
.end patch 2.0
         ADD       C1 TO LINES
.temp file at order/invoice creation read it and update master with Dummy lr and Invoice number
. and create a new temp file.
         write      temp,seq;xmgtlr
         return
.End of Subroutine Detail
..............................................................................................
...............................................................................................
.Header:
Header
.GOING TO PRINT BOTH INHOUSE AND CLIENT COPIES THE SAME
.
         MOVE      HOWN TO NOWNFLD
         rep       zfill in nownfld
         CALL      NOWNKEY
.START PATCH 1.83
         compare    c1 to recsslct           .1st good record?
        if         equal
         PRINT     032,hpreset,hp17ptch,hpdtch10,*F,*1,"CONFIDENTIAL",hpt700,Today:
.begin patch 2.46
.                   *l,*L,hpt150,hpbon,hpunon,"*** NINCAL EXCHANGE MANAGEMENT CHARGES REPORT ***":
                    *l,*L,hpt150,hpbon,hpunon,"     ***  EXCHANGE MANAGEMENT CHARGES REPORT ***":
                   hpboff,hpunoff,*L,*L,hpt300,"Owner##: ",HOWN,"  ",OWNOCPY
			else
         PRINT     	hp17ptch,hpdtch10,*F,*1,"CONFIDENTIAL",hpt700,Today:
.                	*l,*L,hpt150,hpbon,hpunon,"*** NINCAL EXCHANGE MANAGEMENT CHARGES REPORT ***":
                        *l,*L,hpt150,hpbon,hpunon,"     ***  EXCHANGE MANAGEMENT CHARGES REPORT ***":
                   hpboff,hpunoff,*L,*L,hpt300,"Owner##: ",HOWN,"  ",OWNOCPY
			endif
.end patch 2.46
.START PATCH 1.83
         MOVE      olnum TO NDATFLD
         rep       zfill in ndatfld
         CALL      NDATKEY
           IF          OVER
           MOVE        "No List Found!!!!!!!!" TO OLSTNAME
           ENDIF
         CLEAR     NXRFMLR             *CLEAR VARIABLE IN CASE OVER.
         MOVE        Ndatfld TO NXRFFLD
         rep       zfill in nxrffld
         MOVE      C1 TO NXRFPATH
         CALL      NXRFKEY
         PRINT   *L,*1,"List##: ",ndatfld,"  ",OLSTNAME:
                   *L,*1,"Client##: ",NXRFMLR
         PRINT     *l,hpbon,hpt475,"QTY",hpt700,"Return":
                   *L,*1,"LR",hpt125,"Mailer",hpt450,"  Ordered/Billed":
                   hpt700,"  Date",hpboff

         MOVE      "10",LINES

         return
.End of Subroutine Header
.START PATCH 1.82
Headerbrk
.
         MOVE      HOWN TO NOWNFLD
         rep       zfill in nownfld
         CALL      NOWNKEY
         PRINT     hp17ptch,hpdtch10,*F,*1,"CONFIDENTIAL",hpt700,Today:
                   *l,*L,hpt150,hpbon,hpunon,"*** NINCAL EXCHANGE MANAGEMENT CHARGES REPORT ***":
                   hpboff,hpunoff,*L,*L,hpt300,"Owner##: ",HOWN,"  ",OWNOCPY
         MOVE      holdlist TO NDATFLD
         rep       zfill in ndatfld
         CALL      NDATKEY
           IF          OVER
           MOVE        "No List Found!!!!!!!!" TO OLSTNAME
           ENDIF
         CLEAR     NXRFMLR             *CLEAR VARIABLE IN CASE OVER.
         MOVE        Ndatfld TO NXRFFLD
         rep       zfill in nxrffld
         MOVE      C1 TO NXRFPATH
         CALL      NXRFKEY
         PRINT   *L,*1,"List##: ",ndatfld,"  ",OLSTNAME:
                   *L,*1,"Client##: ",NXRFMLR
         PRINT     *l,hpbon,hpt475,"QTY",hpt700,"Return":
                   *L,*1,"LR",hpt125,"Mailer",hpt450,"  Ordered/Billed":
                   hpt700,"  Date",hpboff
         MOVE      "10",LINES

         return
.End of Subroutine HeaderBRk
.end PATCH 1.82 -
..............................................................................................
.Total
Total
         COMPARE   "50",LINES
         CALL      headerbrk IF NOT LESS
         move      qtymask to qtyout
         edit      totqty to qtyout
.begin patch 1.5
.START PATCH 1.6 - REPLACED LOGIC
.         move       c0 to form74
.         move       totqty to form74
.         mult      holdrate by form74
.         divide     "1000" into form74
.         move      c0 to totar
.         add       form74 to totar
...
         move       c0 to form94
         move       totqty to form94
         mult      holdrate by form94
         divide     "1000" into form94
         move      c0 to totar
         add       form94 to totar
.END PATCH 1.6 - REPLACED LOGIC
.end patch 1.5
         move      armask to exar
         edit      totar to exar
.         PRINT *L,*L,*L,*3:
.               *L,*1,"Totals",hpfixed,hpt100,hpunon,exAR,hpunoff,hpt375,hpunon,qtyout,hpunoff,hpdtch10
         PRINT *L,*L,*L,*3:
               *L,*1,"Totals",hpfixed,hpt425,hpunon,qtyout,hpunoff," @ ",perm,"/m = ",hpunon,exAR,hpunoff,hpdtch10
         add       TOTar to Gtotar
         add       TOTQty to Gtotqty
.
.Create order and invoice here and print on report!!!!!
.order goodies

         move     "018710" to ndatfld      .pull correct description
         call     ndatkey
.START PATCH 2.21 REPLACED LOGIC - TEMPORARY PATCH
.         move     NXRFMLR to omlrnum
	move	NXRFMLR,COMPFLD
	move	"Total-COMPKEY",Location
	pack	KeyLocation,"Key: ",COMPFLD
	call	COMPKEY
	move	COMPOLDMLR,omlrnum
.END PATCH 2.21 REPLACED LOGIC - TEMPORARY PATCH
         clear    mkey
         pack     mkey from omlrnum,z3
         clear    mbrknum
         move       c0 to osales10
         move       c0 to osales
         clear     obildrct
         call     nmlrkey
.Begin patch 1.2
         match    yes to mbildrct
         if       equal
         move     yes to obildrct
         endif
.End patch 1.2
         UNPACK    MSLSPER INTO OSALES10,OSALES         .DLH 29sep98
         match    "0000" to mbrknum
         goto     skipbrk if equal
         type     mbrknum
         if       equal
         pack     nbrkfld from mbrknum
         call     nbrkkey
         UNPACK    BRSALES INTO OSALES10,OSALES        .IF EXISTS USE IT.
         else
         UNPACK    MSLSPER INTO OSALES10,OSALES
         endif
skipbrk  move     holdlist to ndatfld         .restore key
         move     z3 to ocobn
.begin patch 2.46
         	clear    OMLRPON
.	if	(CompID = "P")
	if	(CompExcl = "P")
         	move     	"024593" to olnum
         	MOve	"P",OcompID      	
         	move     	"1490" to olon
	Else
.end patch 2.46
	move     "018710" to olnum
         	MOve	"N",OcompID      	
.         clear    OMLRPON
         	move     "0033" to olon
.begin patch 2.46
	Endif
.enD patch 2.46

         move     "S" to orcode
         move     "B" to ostat
.Start patch #1.1 - increase string to fill increased OQTY
.         move      "      0" to oqty
         move      "        0" to oqty
.End patch #1.1 - increase string to fill increased OQTY
         clear     OMLRKY
         move      "   00" to oppm
.Start patch #1.1 - increase string to fill increased VAR
.         move      b1 to ofocode
         move      b2 to ofocode
.End patch #1.1 - increase string to fill increased VAR
         clear     ortndtem
         clear     ortndted
         clear     ortndtey
.Start patch #1.1 - clear new var
         clear     ortndtec
.         move      "00",omdtec
         clear     oodtecoc
.End patch #1.1 - clear new var
.         clear     ortndtem
.         clear     ortndted
.         clear     ortndtey
.Start patch #1.1 - clear new var
.         clear     ortndtec
.         move      "00",omdtec
.         clear     oodtecoc
.End patch #1.1 - clear new var
.          move      "00",omdtem
.         move      "00",omdted
.         move      "00",omdtey
.Start patch #1.80
         unpack     today into omdtem,str1,omdted,str1,omdtey
         move       cc,omdtec
.End patch #1.80
         clear     otocode
         clear     osotcode
         clear     occode
         clear     olrnco
         clear     oodtecom
         clear     oodtecod
         clear     oodtecoy
         clear     oqtyco
         clear     ospi
         clear     obrkguar
         clear     oelcode
         clear     oodnum
         clear     ohist
         clear     oclrinit
         clear     orent
         clear     OBRKRPT
         clear     OCLRDTEC
         clear     OCLRDTEY
         clear     OCLRDTEM
         clear     OCLRDTED
.Start patch #1.71 - clear new varS
          CLEAR     OCAMP
         CLEAR     OCLRINIT
         CLEAR     OCLRSTAT
.END patch #1.71 - clear new varS
.offer ????what to do
         append    omlrnum to oodnum
         append    "001" to oodnum
         reset      oodnum
         clear      oodes        .offer desc
         move      "0269" to ortnnum
         clear      otaperet
         clear      ouqty
.Start patch #1.1 - increase string to clear increased var
.         move       b1 to ococode
         move       b2 to ococode
.End patch #1.1 - increase string to clear increased var
         unpack     today into oodtem,str1,oodted,str1,oodtey
.Start Patch #1.1 - pack century
         move       cc,oodtec
.End Patch #1.1 - pack century
         clear      oscode
         clear      ocomslct
         clear      oshp
         move       olstname to o1des
         clear      o2des
         clear      oreuse
         move       "AMB" to odowj
         clear      oexqty
         clear      guarcode
         clear      obrknum
         clear      obrkcnt
         unpack     mbrknum into obrknum,obrkcnt
         clear      osamcde
         clear      onetper
         move       c0 to onetrc
         clear      onetfm
         move       c0 to onetmin
         clear      spcl7
         clear      spcl8
         clear      spcl9
         clear      DESC0L1
         clear      DESC0L2
         clear      desc991
         clear      desc992
         clear      desc981
         clear      desc982
.Start patch #2.85
	clear       OcompID
	Move	CompExcl,OCompID
.begin patch 2.46
	if	(OCompID = "" or OCompID = " ")
		if	(ELSTCDE = "P")
		move	"P",OCompid
		Elseif	(ELSTCDE = "C")
		move	"N",OCompid
		endif
	Endif	
.End patch 2.46
		
.End patch #2.85

         MOVE     "NORDNXT" TO GNXTFLD
         CALL     GNXTKEY
         MOVE     GNXTNUM TO N6
addord   ADD      C1,N6
         MOVE      N6 TO GNXTNUM
         REP       ZFILL IN GNXTNUM
.         CALL      GNXTUPD
         move      n6 to nordfld
         move      n6 to olrn
         call      nordtst
         goto      addord if not over
.make sure we pass data for all keys!!!!!
.         call      nordwrt
.         FILEPI    1;ORDPRINT
.         WRITE     ORDPRINT,olrn;ordvars:
.                  SPCL7,DESC0L1,DESC0L2:
.                  SPCL8,DESC991,DESC992,SPCL9,DESC981,DESC982
*
.invoice goodies go here
*
         move      "F" to CODE
         move      c0 to STATB
         move      omlrnum to MLRN
         move      olrn to LRN
         move      c0 to BILLTN
         move      c0 to PAYTN
         CLEAR     LOINVN
         MOVE      C0 TO AP1
         MOVE      C0 TO COMMPCT
         MOVE      C4 TO PAYCODE                 .all to lr income
.begin patch 1.4
.         MOVE      TOTQTY TO QTYSHP
         MOVE      TOTQTY TO QTYin
         MOVE      TOTQTY TO QTYbild
.end patch 1.4
.START PATCH 1.6 - REPLACED LOGIC
.         move      c0 to form62
..         move      holdrate to form62            .02APR98
..         move      xmgtrate to form62
..         mult      "100" by form62
.         move      c0 to n5
.         move      holdrate to ppm            .14aug99
..         add       form62 to n5
..         MOVE      n5 TO PPM
.................................
         move      c0 to form92
.         move      holdrate to form92            .02APR98
.         move      xmgtrate to form92
.         mult      "100" by form92
         move      c0 to n5
         move      holdrate to ppm            .14aug99
.         add       form92 to n5
.         MOVE      n5 TO PPM
.end PATCH 1.6 - REPLACED LOGIC
         MOVE      "000" TO COBN
         clear     INCC
.begin patch 2.46
.         move       "0033" to LON
	Move	Olon,Lon
.begin patch 2.46
         CLEAR      CHKN2
         clear      WSJPC
         CLEAR      ADJC
         CLEAR      CHKN1
.begin patch 1.4
.         CLEAR      CHKDTEM
.         CLEAR      CHKDTED
.         cLEAR      CHKDTEY
         CLEAR      CHKN2
         CLEAR      CHKN3
         CLEAR      CHK1DTEM
         CLEAR      CHK1DTED
         cLEAR      CHK1DTEY
         CLEAR      CHK2DTEM
         CLEAR      CHK2DTED
         cLEAR      CHK2DTEY
         CLEAR      CHK3DTEM
         CLEAR      CHK3DTED
         cLEAR      CHK3DTEY
.end patch 1.4
         CLEAR      LET90D
         CLEAR      MLRPAYR
         CLEAR      MLRPAYD
         CLEAR      INVNUM
         unpack     today into invdtem,str1,invdted,str1,invdtey
.begin patch 1.4
         move      cc to invdtec
.end patch 1.4
.begin patch 2.2
.         CLEAR      ADDCHG1
.         CLEAR      ADDCHG2 
.         CLEAR      ADDCHG3 
.         CLEAR      ADDCHG4 
.         CLEAR      ADDCHG5 
.         CLEAR      ADDCHG6 
.         CLEAR      ADDCHG7 
.         CLEAR      ADDCHG8 
.         CLEAR      ADDCHG9 
.         CLEAR      ADDCHG10
.end patch 2.2
         CLEAR      GUARPAY
         CLEAR      AR
         move       c0 to form92
.begin patch 1.4
.         add        totar to form92
.         MULT       "100" BY form92
.         move       c0 to n9
.         add        form92 to n9
.         MOVE       n9 TO AR
         move       totar to ar
.         CLEAR      SALES
         CLEAR      invSALES
.end patch 1.4
         MOVE        C0 TO AP2
         MOVE       OBRKNUM TO IBRKNUM
         MOVE       OBRKCNT TO IBRKCNT
         CLEAR      IMLRCHK
         MOVE       C0 TO IRCQTY
         MOVE       C0 TO IREXQTY
         MOVE       C0 TO iexPPM
         MOVE       C0 TO irnetper
         MOVE       C0 TO inetrc
         MOVE      "NINVNXT" TO GNXTFLD
         CALL      GNXTKEY
         MOVE      GNXTNUM TO n6
NINADD   MOVE      N6,NINVFLD
         ADD       "1",N6
         MOVE      C2 TO NINVPATH
         CALL      NINVTST
         GOTO      NINADD IF NOT OVER
         MOVE     ninvfld TO GNXTNUM
.         CALL     GNXTUPD
         MOVE      NINVFLD TO INVNUM
         MOVE      C1 TO NINVPATH
         MOVE      OLRN TO NINVFLD
         REP       ZFILL IN INVNUM
         REP       ZFILL IN NINVFLD
.         CALL       NINVWRT
         Move     "AMB" to Inits
.begin patch 2.2
.          Call           PInvWrt
.         Filepi    1;PINVOICE
.         WRITE     PINVOICE,NINVFLD;invvars,inits
.end patch 2.2
totcap
.begin patch 2.0
         COMPARE   "50",LINES
         CALL      header IF NOT LESS
         print     *L,*1,hpbon,"The above are detail of orders billed on Invoice ## ",invnum,hpboff:
                   *l,*1,"These reflect the exchange orders run at your Service Bureau during ",Month
			if        (capown = yes)
         COMPARE   "50",LINES
         CALL      header IF NOT LESS
         print     *L,*1,"* Management exchange fee capped on orders 100m+. "
			move      no to capown
			endif
.end patch 2.0
         weof      temp,seq
         close     temp
.START PATCH 1.7 REPLACED LOGIC
.         open      temp,"g:\data\EXCHTEmp.dat"
         PACK      STR35,NTWKPATH1,"EXCHTEMP.DAT"
         open      temp,STR35
.END PATCH 1.7 REPLACED LOGIC
.update - sucessfully created order and invoice lets update input records to reflect bill done
update   read      temp,seq;xmgtfld
         goto      endupd if over
         rep       zfill in xmgtfld
         call      xmgtkey
         move      nordfld to xmgtOrd
         move      invnum to xmgtInv
         call      xmgtupd
         add       c1 to billed
         display   *p10:16,"Billed           : ",billed
         goto      update

endupd   close     temp,delete
.START PATCH 1.7 REPLACED LOGIC
.         prepare   temp,"g:\data\exchTemp.dat"
         PACK      STR35,NTWKPATH1,"ExCHTEMP.DAT"
         prepare   temp,STR35
.END PATCH 1.7 REPLACED LOGIC
         move      c0 to totAr
         move      c0 to totqty
			move      c0 to xmgtbill
         return
.End of Subroutine Total
..............................................................................................
.EoJ
eoj     move       holdrate to xmgtrate
        call       total
.begin patch 2.46 ??????????????????????????????????????
        move       "018710" to ndatfld
        move       "0033" to hown
        call       header
        move       arGmask to exGar
        edit       gtotar to exGar
         move      Gqtymask to Gqtyout
         edit      gtotqty to Gqtyout
         PRINT *L,*L,*L,*3:
               *L,*1,"Totals",hpfixed,hpt375,hpunon,Gqtyout,hpt550,exGAR,hpunoff
        splclose
        release
        stop
..............................................................................................
.End of Subroutine EOJ
..............................................................................................

         include   xmgtio.inc
         INCLUDE   NOWNIO.inc
.Patch1.84
			include	compio.inc
			include	cntio.inc
.         INCLUDE   NMLRIO.inc
.Patch1.84
         INCLUDE   NDATIO.inc
         INCLUDE   NORDIO.inc
         INCLUDE   NXRFIO.inc
         INCLUDE   NMTXIO.inc
.begin patch 2.2
.         include   ninvio.inc
         include   ninvio.inc
.end patch 2.2
         include   gnxtio.inc
.Patch1.84
.         include   nbrkio.inc
.Patch1.84
         INCLUDE   COMLOGIC.inc

