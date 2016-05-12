.THIS PROGRAM HAS NOT BEEN CONVERTED WITH NEW DATACARD FORMAT.  ONLY PROGRAMS WHICH ARE STILL USING IT:  NINV0001 & NMRG0001
.WILL UNDERGO CONVERSION AND THIS INCLUDE WILL BE OBSOLETE.
;    Last change:  JD   13 Apr 2000    3:41 pm
.160502 JD DISPLAY nfulcomp instead of ownctn.
.13Apr99 JD moved display of order info from 73 to 70.
.01Apr99 DLH change var ship TO shipdesc
..22oct98 DLH ADD paths AND lock flags TO mlr rtn AND brk calls TO help insure ALL ok
.UPDATED TO NEW MEDIA TABLE
..29APR94 DLH REQUIRES MEDIA.INC & shipping.inc
..16Feb99 ASH NINORD Y2K, File expansion patch #1.1
..11may99 ASH NINSPE Conversion/expansion patch 1.2
..16MAR2000 ASH REPLACED REFERENCES TO CONTACT1.INC WITH NCNTDD.INC PATCH 1.3
..01JUN2000 ASH Increased Media.inc PATCH 1.4
.
.START PATCH 1.2 - ADDED VARS UNTIL ALL PROGRAMS USE GUI INTERFACE
#holdstr  dim      752
#line1    dim      49
#CARR     INIT     0x7f
#carrfill dim      2
.END PATCH 1.2 - ADDED VARS UNTIL ALL PROGRAMS USE GUI INTERFACE
.START PATCH 1.2 - REMMED VARS - NO LONGER USED
.DISSCRNB CLEAR      SPCL7
.         CLEAR      SPCL8
.         CLEAR      SPCL9
.         CLEAR      DESC0L1
.         CLEAR      DESC0L2
.         CLEAR      DESC991
.         CLEAR      DESC992
.         CLEAR      DESC981
.         CLEAR      DESC982
DISSCRNB
.END PATCH 1.2 - REMMED VARS - NO LONGER USED
         CLEAR      ODOWJ
         MOVE      B1,OTOCODE
         CALL      DISS
         GOTO      MATCHF3
DISS     
.START PATCH #1.1 - REPLACED LOGIC
.         DISPLAY   *ES,"1)Mlr:____ Cnt:___ (2)PO:____________3)Rtn-":
.                   *P44:1,"To##:____(5)List##:______(6)Owner##:____":
.                   *P01:07,"(4)Ofr##:___ __________________________________":
.                   "______ (7)NIN Guar. :_",*P1:8,"(26)BRK Guar.:_":
.                   *P35:08,"   List:___________________________________":
.                   *P01:09,"(9)Continuation?:_ LR:______       (8)Sel:____":
.                   "_______________________________":
.                   *P4:10,"Dat:__/__/__ Qty:______",*P34:10,"(20)Special ":
.                   "Instructions: __ __ __ __ __ __":
.                   *P01:12,"(10)Test?:_ Entire?:_ Exch?:_":
.                   *P01:13,"(11)Xsec?:_ Nth Nm?:_ Othr?:_":
.                   *P01:14,"(12)Order Qty:_______":
.                   *P01:15,"(13)Price /M :___.__":
.                   *P01:16,"(14)Key Info :________":
.                   *P01:17,"(24)Comselect?:_":
.                   *P01:18,"(15)Rtn:__/__/__ MD:__/__/__":
.                   *P01:19,"(23)Salesmn:_":
.                   *P01:20,"(16)Media  :_":
.                   *P01:21,"(25)Rt-tape:_":
.                   *P01:22,"(17)Sample :_":
.                   *P01:23,"(18)Ship   :_",*P1:24,"(19)Contact:_";
.
         DISPLAY   *ES,"1)Mlr:____ Cnt:___ (2)PO:____________3)Rtn-":
                   *P44:1,"To##:____(5)List##:______(6)Owner##:____":
                   *P01:07,"(4)Ofr##:___ __________________________________":
                   "______ (7)NIN Guar. :_",*P1:8,"(26)BRK Guar.:_":
                   *P35:08,"   List:___________________________________":
                   *P01:09,"(9)Continuation?:_ LR:______       (8)Sel:____":
                   "_______________________________":
                   *P4:10,"Dat:__/__/____ Qty:_________",*P34:10,"(20)Special ":
                   "Instructions: __ __ __ __ __ __":
                   *P01:12,"(10)Test?:_ Entire?:_ Exch?:_":
                   *P01:13,"(11)Xsec?:_ Nth Nm?:_ Othr?:_":
                   *P01:14,"(12)Order Qty:_________":
                   *P01:15,"(13)Price /M :___.__":
                   *P01:16,"(14)Key Info :________":
                   *P01:17,"(24)Comselect?:_":
                   *P01:18,"(15)Rtn:__/__/____ MD:__/__/____":
                   *P01:19,"(23)Salesmn:_":
                   *P01:20,"(16)Media  :__":
                   *P01:21,"(25)Rt-tape:_":
                   *P01:22,"(17)Sample :_":
                   *P01:23,"(18)Ship   :__",*P1:24,"(19)Contact:_";
.END PATCH #1.1 - REPLACED LOGIC
         RETURN
MATCHF3  DISPLAY   *P7:1,OMLRNUM,*P16:1,OCOBN,*P26:1,OMLRPON,*P48:1,ORTNNUM;
.
         type      obrknum
         goto      brk if equal
         goto      nmlr
brk      PACK      NBRKFLD FROM OBRKNUM,OBRKCNT
         PACK      MKEY FROM OMLRNUM,OCOBN
.dlh 22oct98
         MOVE      c1 to nmlrpath
         MOVE      c1 to nbrkpath
         MOVE      c3 to nmlrlock
         MOVE      c3 to nbrklock
.end dlh 22oct98
         CALL      NMLRKEY
         CALL      NBRKKEY
         GOTO     ZEROBROK IF NOT OVER
.dlh 22oct98
         MOVE      c1 to nmlrlock           .restore defaults
         MOVE      c1 to nbrklock
.end dlh 22oct98
         DISPLAY   *P1:3,"BROKER/CONSULTANT RECORD NOT FOUND";
         CMATCH    NO,CORP
         GOTO      RDRTN IF EQUAL
         GOTO      CM1
NMLR     PACK      MKEY FROM OMLRNUM,OCOBN
CM1      REP       ZFILL IN MKEY
         CALL      NMLRKEY
         GOTO       DISMAIL IF NOT OVER
         DISPLAY   *P1:3,"MAILER RECORD NOT FOUND";
         CMATCH    NO,CORP
         GOTO      RDRTN IF EQUAL
.
ZEROBROK
.dlh 22oct98
         MOVE      c1 to nmlrlock          .restore defaults
         MOVE      c1 to nbrklock
.end dlh 22oct98
         BUMP      OODNUM BY 4
         DISPLAY   *P14:1,"Brk:",obrknum:
                   *P1:2,MCOMP,*P1:3,*HON,"C/O",*HOFF:
                   BRCOMP:
                   *P9:7,STR3,B1,OODES,*P61:1,OLNUM,*P77:1,OLON;
        cmatch     yes to mcopies               .regional?
        if         equal                        .yes
        display    *p1:2,*yellow,mcomp,*white
        endif
         cmatch    no to onetfm
         if        equal
         display   *p1:5,*hon,"Gross Billing No Deducts",*hoff
         goto      rdrtn
         endif
         cmatch     "F" to onetfm
         if         not equal
         display   *p1:5,"28)Net: ",onetper,"%",b1,onetrc,"/m RC":
                   *p1:6,onetmin," minimum"
         else
         display   *p1:5,"28)Net: ",onetper,"% Flat Discount":
                   *p1:6,onetmin," minimum"
         endif
         GOTO      RDRTN
.
.         GOTO      NMLR IF NOT EQUAL
.         PACK      MKEY FROM OMLRNUM,C0,C0,C0
.         GOTO      CM1
DISMAIL  UNPACK    OODNUM INTO STR4,STR3      GET  OFFER #.
         DISPLAY   *P1:2,MNAME,*P1:3,MCOMP:
                   *P9:7,STR3,B1,OODES,*P61:1,OLNUM,*P77:1,OLON;
        cmatch     yes to mcopies               .regional?
        if         equal                        .yes
        display    *p1:3,*yellow,mcomp,*white
        endif
         cmatch    no to onetfm
         if        equal
         display   *p1:5,*hon,"Gross Billing No Deducts",*hoff
         goto      rdrtn
         endif
         cmatch     "F" to onetfm
         if         not equal
         display   *p1:5,"28)Net: ",onetper,"%",b1,onetrc,"/m RC":
                   *p1:6,onetmin," minimum"
         else
         display   *p1:5,"28)Net: ",onetper,"% Flat Discount":
                   *p1:6,onetmin," minimum"
         endif
         GOTO      RDRTN
.
         CLEAR     STR4
         CLEAR     STR3
RDRTN    MOVE      ORTNNUM TO NRTNFLD
.dlh 22oct98
         MOVE      c1 to nrtnpath
         MOVE      c3 to nrtnlock
.end dlh 22oct98
         CALL      NRTNKEY
         GOTO      DISRTN IF NOT OVER
.dlh 22oct98
         MOVE      c1 to nrtnlock
.end dlh 22oct98
         DISPLAY   *P28:3,"RETURN-TO RECORD NOT FOUND";
         GOTO      DISRTNX     .EXIT
DISRTN
.dlh 22oct98
         MOVE      c1 to nrtnlock
.end dlh 22oct98
         MATCH     "0677" TO OMLRNUM
         GOTO      CMSKEN IF EQUAL
         MATCH     "0210" TO OMLRNUM
         GOTO      CMSKEN IF EQUAL
         MATCH     "1361" TO OMLRNUM         "CUSTOM LISTS"
         GOTO      CMSKEN IF EQUAL
         MATCH     "0053" TO OMLRNUM         "ANACAPA
         GOTO      CMSKEN IF EQUAL
         MATCH     "0702" TO OMLRNUM
         GOTO      CMSKEN IF EQUAL
         MATCH     "0965" TO OMLRNUM         "
         GOTO      CMSKEN IF EQUAL
         MATCH     MCOMP TO RTCOMP
         GOTO      DISRTN1 IF EQUAL
         GOTO      DISRTN2
. CMSKEN - FOR MAILERS CRAVER,MATHEWS,SMITH AND KEN COPLON OFFER APPEARS ON
.          RETURN TO CONTACT LINE.
.AND       CUSTOM LISTS.
CMSKEN   MOVE      OODES TO MNAME
         DISPLAY   *P28:2,MNAME,*P28:3,RTCOMP,*P28:4,RTADDR,*P28:5,RTCITY:
                   B1,RTSTATE,RTZIP;
         GOTO      DISRTNX
. DISRTN1 - MAILER & RETURN TO COMPANY NAMES THE SAME. DISPLAY RETURN-TO CNTCT.
DISRTN1
         DISPLAY   *P28:2,RTCNTCT,*P28:3,RTCOMP,*P28:4,RTADDR,*P28:5,RTCITY:
                   B1,RTSTATE,RTZIP;
         GOTO      DISRTNX
. DISRTN2 - MAILER & RETURN-TO COMPANY NAMES DIFFERENT. DISPLAY MLR AS CANTACT.
DISRTN2
         DISPLAY   *P28:2,MCOMP,*P28:3,RTCOMP,*P28:4,RTADDR,*P28:5,RTCITY:
                   B1,RTSTATE,RTZIP,*P25:3,*HON,"C/O",*HOFF;
DISRTNX
         MATCH     "0001",ORTNNUM
         GOTO      RDOWN IF NOT EQUAL
         DISPLAY   *P43:4,OREUSE;
.
RDOWN
 	call	Trim using NFULCOMP
          DISPLAY   *P56:2,OWNLONM,*P56:3,OWNOCPY,*P56:4,OWNLOSA,*P56:5:
.                   OWNLOCTY,B1,OWNLOS,OWNLOZC,*P58:6,OWNCTN;
                   OWNLOCTY,B1,OWNLOS,OWNLOZC,*P58:6,nfulcomp;
.
         DISPLAY   *P68:7,GUARCODE,*P43:8,O1DES,*P43:9,O2DES,*P18:9,OCCODE:
                   *P70:07,"        ",*p15:8,obrkguar;
         MOVE      C0 TO N1
         MOVE      GUARCODE TO N1
         BRANCH    N1,DISGU1,DISGU2,DISGU3,DISGU4,DISGU5,DISGU6:
                   DISGU7,DISGU8,DISGU9
         GOTO      DISOWN3
DISGU1   DISPLAY   *P70:7,"30 - DAY"
         GOTO      DISOWN3
DISGU2   DISPLAY   *P70:7,"45 - DAY"
         GOTO      DISOWN3
DISGU3   DISPLAY   *P70:7,"60 - DAY"
         GOTO      DISOWN3
DISGU4   DISPLAY   *P70:7,"NO DATE"
         GOTO      DISOWN3
DISGU5   DISPLAY   *P70:7,"       "
         GOTO      DISOWN3
DISGU6   DISPLAY   *P70:7,*HON,"PREPAID",*HOFF
         GOTO      DISOWN3
DISGU7   DISPLAY   *P70:7,*HON,"PREPAID 30-DAY",*HOFF
         GOTO      DISOWN3
DISGU8   DISPLAY   *P70:7,*HON,"PREPAID 45-DAY",*HOFF
         GOTO      DISOWN3
DISGU9   DISPLAY   *P70:7,*HON,"PREPAID 60-DAY",*HOFF
         GOTO      DISOWN3
DISOWN3
         MOVE      C0 TO N1
         MOVE      OBRKGUAR TO N1
         BRANCH    N1,DISBGU1,DISBGU2,DISBGU3,DISBGU4
         GOTO      DISOWNx
DISBGU1   DISPLAY   *P17:8,"30 - DAY"
         GOTO      DISOWNx
DISBGU2   DISPLAY   *P17:8,"45 - DAY"
         GOTO      DISOWNx
DISBGU3   DISPLAY   *P17:8,"60 - DAY"
         GOTO      DISOWNx
DISBGU4   DISPLAY   *P17:8,"NO DATE"
         GOTO      DISOWNx
DISOWNX  CMATCH    B1 TO OCCODE
         GOTO      NOCONT IF EQUAL
         MOVE      OCCODE TO N1
         BRANCH    N1 OF CNTOMIT,CNTNOMIT
         GOTO      NOCONT
CNTOMIT  DISPLAY   *P18:9,YES,*P23:9,OLRNCO;
.         *FOR DATE DISPLAY;
.START PATCH #1.1 - REPLACED LOGIC
.         DISPLAY   *P8:10,OODTECOM,*P11:10,OODTECOD,*P14:10,OODTECOY:
.                   *P21:10,OQTYCO;
         DISPLAY   *P8:10,OODTECOM,*P11:10,OODTECOD,*P14:10,OODTECOC,OODTECOY:
                   *P23:10,OQTYCO;
.END PATCH #1.1 - REPLACED LOGIC
         GOTO      BOTHDIS
CNTNOMIT DISPLAY   *P18:9,YES,*P20:9,"NO OMIT   ",*P4:10:
                   "                       ";
         GOTO      BOTHDIS
NOCONT   DISPLAY   *P18:9,"N          ",*P4:10,"                       ";
BOTHDIS  CMATCH    B1 TO OTOCODE                    TEST
         GOTO      DISNO IF EQUAL
         DISPLAY   *P11:12,YES;
         GOTO      MATCH16
DISNO    DISPLAY   *P11:12,NO;
MATCH16  CMATCH    B1 TO  OELCODE
         GOTO      DISNO2 IF EQUAL
         MOVE      OELCODE,N1
         BRANCH    N1 OF DISENT,DISEX,DISNTEX
DISENT   DISPLAY   *P21:12,YES,*P29:12,NO;
         GOTO      BRIXO
DISEX    DISPLAY   *P21:12,NO,*P29:12,YES;
         GOTO      BRIXO
DISNTEX  DISPLAY   *P21:12,YES,*P29:12,YES;
         GOTO      BRIXO
DISNO2   DISPLAY   *P21:12,NO,*P29:12,NO;
BRIXO    SUB       N1,N1
         MOVE      OSOTCODE,N1
         BRANCH    N1 OF DISXSEC,DISNTH,DISOTHR
         GOTO      DISQTY
DISXSEC  DISPLAY   *P11:13,YES,*P21:13,B1,*P29:13,B1;
         GOTO      DISQTY
DISNTH   DISPLAY   *P21:13,YES,*P11:13,B1,*P29:13,B1;
         GOTO      DISQTY
DISOTHR  DISPLAY   *P29:13,YES,*P11:13,B1,*P21:13,B1;
DISQTY
.         MOVE      OPPM,STR3
.         BUMP      OPPM BY 3
.         MOVE      OPPM,STR2
.         RESET     OPPM
	packkey	NSEL2FLD,"1",OLRN
	move	"NSEL2KEY",Location
	pack	KeyLocation,"Key: ",NSEL2FLD
	call	NSEL2KEY
	if over
		move	O2DES,NSEL2NAME
		unpack	OPPM,str3,str2
		pack	str6,str3,".",str2
		rep	zfill,str6
		move	str6,NSEL2PRICE
		move	"/M",NMODDESC
	else
		pack	NMODFLD,NSEL2DESC
		rep	zfill,NMODFLD
		move	"NMODKEY",Location
		pack	KeyLocation,"Key: ",NMODFLD
		call	NMODKEY
		if over
			move	"/M",NMODDESC
		else
			call	Trim using NMODDESC
		endif
	endif

.START PATCH #1.1 - REPLACED LOGIC
.         DISPLAY   *P15:14,OQTY,*P15:15,STR3,".",STR2,*P15:16,OMLRKY:
.                   *P22:14,"\",OEXQTY;
..
.         DISPLAY   *P9:18,ORTNDTEM,"/",ORTNDTED,"/",ORTNDTEY    (RETURN-DATE);
..
.         DISPLAY   *P21:18,OMDTEM,"/",OMDTED,"/",OMDTEY:     (MAIL-DATE);
.                   *P13:19,OSALES10,OSALES:
.                   *P13:20,OFOCODE
...
;         DISPLAY   *P15:14,OQTY,*P15:15,STR3,".",STR2,*P15:16,OMLRKY:
         DISPLAY   *P15:14,OQTY,*P15:15,nsel2price,*P15:16,OMLRKY:
                   *P24:14,"\",OEXQTY;
.
         DISPLAY   *P9:18,ORTNDTEM,"/",ORTNDTED,"/",ORTNDTEC,ORTNDTEY    (RETURN-DATE);
.
         DISPLAY   *P21:18,OMDTEM,"/",OMDTED,"/",OMDTEC,OMDTEY:     (MAIL-DATE);
                   *P13:19,OSALES10,OSALES:
                   *P13:20,OFOCODE
.END PATCH #1.1 - REPLACED LOGIC
SELECT
         CMATCH    "C",OCOMSLCT
         GOTO      SELECT1 IF NOT EQUAL
         DISPLAY   *P16:17,YES;
         GOTO      SALE
SELECT1  DISPLAY   *P16:17,NO;
.
SALE     CMATCH    B1,OSALES
         GOTO      NOSALE IF EQUAL
         GOTO      NOSALE IF EOS
         PACK      STR2 FROM OSALES10,OSALES
         MOVE      STR2 TO N2
         MOVE      OSLS0 TO CONTACT
         LOAD      CONTACT FROM N2 OF OSLS1,OSLS2,OSLS3,OSLS4,OSLS5:
                   OSLS6,OSLS7,OSLS8,OSLS9,OSLS10,OSLS11,OSLS12,OSLS13:
                   OSLS14,OSLS15,OSLS16,OSLS17,OSLS18,OSLS19,OSLS20,OSLS21:
                   OSLS22
         DISPLAY   *P16:19,*EL,CONTACT
         GOTO      DISmed2
NOSALE   DISPLAY   *P16:19,*EL,"NOTHING        ";
.
.START PATCH #1.1 - REPLACED LOGIC
.         CMATCH    B1,OFOCODE
         MATCH     B2,OFOCODE
.END PATCH #1.1 - REPLACED LOGIC
         GOTO      DISMED2 IF NOT EQUAL
         GOTO      DISF28
DISMED2  MOVE      C0 TO N2
         TYPE      OFOCODE
         MOVE      OFOCODE TO SAVE         *SAVE VARIABLE
         GOTO      MED10 IF NOT EQUAL
         MOVE      OFOCODE TO N2
         GOTO      DIS27
MED10    REP       "A0B1C2D3E4F5G6H7I8J9" IN OFOCODE
         TYPE      OFOCODE
         GOTO      MED20 IF NOT EQUAL
         MOVE      OFOCODE TO N2
         ADD       C10 TO N2
         GOTO      DIS27
MED20    REP       "K0L1M2N3O4P5Q6R7S8T9" IN OFOCODE
         TYPE      OFOCODE
         GOTO      MED30 IF NOT EQUAL
         MOVE      OFOCODE TO N2
         ADD       "20" TO N2
         GOTO      DIS27
MED30    REP       "U0V1X2Y3Z4" IN OFOCODE
         MOVE      OFOCODE TO N2
         ADD       "30" TO N2
DIS27    MOVE      MED0 TO MEDIA
.START PATCH 1.4 REPLACED LOGIC
.         LOAD      MEDIA FROM N2 OF MED1,MED2,MED3,MED4,MED5:
.                   MED6,MED7,MED8,MED9,MED10,MED11,MED12,MED13,MED14:
.                   MED15,MED16,MED17,MED18,MED19,MED20,MED21,MED22:
.                   MED23,MED24,MED25
         LOAD      MEDIA FROM N2 OF MED1,MED2,MED3,MED4,MED5:
                   MED6,MED7,MED8,MED9,MED10,MED11,MED12,MED13,MED14:
                   MED15,MED16,MED17,MED18,MED19,MED20,MED21,MED22:
                   MED23,MED24,MED25,MED26,MED27,MED28,MED29
.END PATCH 1.4 REPLACED LOGIC
         DISPLAY   *P15:20,*EL,MEDIA
         MOVE      SAVE TO OFOCODE        *RESTORE VARIABLE.
DISF28   DISPLAY   *P13:22,OSCODE;
.TAPE RETURN
         DISPLAY   *P15:21,*EL,OTAPERET;
         CMATCH    B1,OSCODE
         GOTO      SAM0 IF EQUAL
         MOVE      OSCODE,N2
         BRANCH    N2 OF SAM1,SAM2,SAM3
SAM0     DISPLAY   *P15:22,*EL,"NOTHING";
         GOTO      DISF29
SAM1     DISPLAY   *P15:22,*EL," SAMPLE ENCLOSED";
         GOTO      DISF29
SAM2     DISPLAY   *P15:22,"SAMPLE TO FOLLOW";
         GOTO      DISF29
SAM3     DISPLAY   *P15:22,"SAMP.PREV.CLEARED";
DISF29   DISPLAY   *P13:23,OSHP;
.START PATCH #1.1 - REPLACED LOGIC
.         CMATCH    B1,OSHP
         MATCH     B2,OSHP
.END PATCH #1.1 - REPLACED LOGIC
         GOTO      SHIPBLNK IF EQUAL
         MOVE      OSHP TO N2
         move      ship0 to shipdesc
         load      shipdesc from n2 OF SHIP1,SHIP2,SHIP3,SHIP4,SHIP5,SHIP6:
                            SHIP7,SHIP8,SHIP9
         goto     ship0
SHIPBLNK DISPLAY   *P15:23,*EL,"Nothing";
         GOTO      DISCONTC
SHIP0    DISPLAY   *P15:23,*EL,shipdesc;
         GOTO      DISCONTC
.
DISCONTC DISPLAY   *P13:24,OCOCODE;
.START PATCH 1.3 REPLACED LOGIC
..START PATCH #1.1 - REPLACED LOGIC
..         CMATCH    "0",OCOCODE
..         GOTO      DISSPCL IF EQUAL
..         CMATCH    B1,OCOCODE
..
.         MATCH     "00",OCOCODE
.         GOTO      DISSPCL IF EQUAL
.         MATCH     B2,OCOCODE
..END PATCH #1.1 - REPLACED LOGIC
.         GOTO      DISCON2 IF NOT EQUAL
.         GOTO      DISNIN
.DISCON2  MOVE      C0 TO N2
.         TYPE      OCOCODE
.         MOVE      OCOCODE TO SAVE         *SAVE VARIABLE
.         GOTO      CON10 IF NOT EQUAL
.         MOVE      OCOCODE TO N2
.         GOTO      DISNIN
.CON10    REP       "A0B1C2D3E4F5G6H7I8J9" IN OCOCODE
.         TYPE      OCOCODE
.         GOTO      CON20 IF NOT EQUAL
.         MOVE      OCOCODE TO N2
.         ADD       C10 TO N2
.         GOTO      DISNIN
.CON20    REP       "K0L1M2N3O4P5Q6R7S8T9" IN OCOCODE
.         TYPE      OCOCODE
.         GOTO      CON30 IF NOT EQUAL
.         MOVE      OCOCODE TO N2
.         ADD       "20" TO N2
.         GOTO      DISNIN
.CON30    REP       "U0V1X2Y3Z4" IN OCOCODE
.         MOVE      OCOCODE TO N2
.         ADD       "30" TO N2
.DISNIN   MOVE      OCNT0 TO CONTACT
.        LOAD      CONTACT FROM N2 OF OCNT1,OCNT2,OCNT3,OCNT4,OCNT5,OCNT6:
.                   OCNT7,OCNT8,OCNT9,OCNT10,OCNT11,OCNT12,OCNT13,OCNT14,OCNT15
.         DISPLAY   *P15:24,CONTACT;
.         MOVE      SAVE TO OCOCODE        *RESTORE VARIABLE.
...............
        pack    NCNTFLD,OCOCODE
        move    "DISCONTC-NCNTKEY",Location
        pack    KeyLocation,"Key: ",NCNTFLD
        call    NCNTKEY
        if over
                clear   CNTNAME
        endif
        display *P15:24,CNTNAME;
.END PATCH 1.3 REPLACED LOGIC
DISSPCL  
.START PATCH 1.2 - REPLACED LOGIC
..START PATCH #1.1 - REPLACED LOGIC
..         UNPACK    OSPI INTO SPCL1,SPCL2,SPCL3,SPCL4,SPCL5,SPCL6
.         UNPACK    OSPI INTO STR1,SPCL1,STR1,SPCL2,STR1,SPCL3,STR1,SPCL4,STR1,SPCL5,STR1,SPCL6
..END PATCH #1.1 - REPLACED LOGIC
.         DISPLAY   *P60:10,SPCL1,B1,SPCL2,B1,SPCL3,B1,SPCL4:
.                   B1,SPCL5,B1,SPCL6;
.         MOVE      NORDFLD TO NSPEFLD
.         CALL      NSPEKEY
..         READ      OUTSP,KEYMD;WORK06,DESC0L1,DESC0L2,DESC991,DESC992:
..                   DESC981,DESC982
..
.         GOTO      NO0099 IF OVER
.         GOTO      DISREG0
.NO0099   MOVE      "NO SPECIAL INSTRUCTION FOUND!" TO DESC0L1
.         MOVE      "NO SPECIAL INSTRUCTION FOUND!" TO DESC981
.         MOVE      "NO SPECIAL INSTRUCTION FOUND!" TO DESC992
.DISREG0  MOVE      SPCL1 TO NSPIFLD
.         MOVE      "11" TO V1
.         MOVE      "12" TO V2
.         CALL      SPCLNSTR                           SPEC INSTRUC ROUTINE
.         MOVE      SPCL2 TO NSPIFLD
.         MOVE      "13" TO V1
.         MOVE      "14" TO V2
.         CALL      SPCLNSTR
.         MOVE      SPCL3 TO NSPIFLD
.         MOVE      "15" TO V1
.         MOVE      "16" TO V2
.         CALL      SPCLNSTR
.         MOVE      "17" TO V1
.         MOVE      "18" TO V2
.         MOVE      SPCL4 TO NSPIFLD
.         CALL      SPCLNSTR
.         MOVE      SPCL5 TO NSPIFLD
.         MOVE      "19" TO V1
.         MOVE      "20" TO V2
.         CALL      SPCLNSTR
.         MOVE      SPCL6 TO NSPIFLD
.         MOVE      "21" TO V1
.         MOVE      "22" TO V2
.         CALL      SPCLNSTR
.         GOTO      DISLR
..
.. ROUTINE FOR SPECIAL INSTRUCTION DISPLAY;
..
.SPCLNSTR DISPLAY   *P31:V1,*EL,NSPIFLD;
.         MATCH     "00" TO NSPIFLD
.         GOTO      CMPSP1 IF NOT EQUAL
.         MOVE      "00",SPCL7
.         DISPLAY   *P34:V1,DESC0L1,*P34:V2,DESC0L2;
.         RETURN
.CMPSP1   MATCH     "99" TO NSPIFLD
.         GOTO      CMPSP2 IF NOT EQUAL
.         MOVE      "99",SPCL8
.         DISPLAY   *P34:V1,DESC991,*P34:V2,DESC992;
.         RETURN
.CMPSP2   MATCH     "98" TO NSPIFLD
.         GOTO      LUKUP1 IF NOT EQUAL
.         MOVE      "98",SPCL9
.         DISPLAY   *P34:V1,DESC981,*P34:V2,DESC982;
.         RETURN
.LUKUP1   CLEAR     INST1
.         CLEAR     INST2
.         MATCH     "  ",NSPIFLD
.         GOTO      RETN IF EQUAL
.         REP       ZFILL IN NSPIFLD
.         CALL      NSPIKEY
..         READ      SPIMST,NSPIFLD;DAY,INST1,INST2
.         GOTO      NODES IF OVER                      DISP REC NO FND IN SPC F
.         DISPLAY   *P34:V1,INST1,*P34:V2,*EL,INST2;
.         RETURN
..
.NODES    DISPLAY   *P34:V1,*B,"Description of special instruction ",NSPIFLD:
.                   " cannot be found.",*W;
.RETN     RETURN
...........................
         clear     #holdstr
         clear     #line1
         MOVE      NORDFLD TO NSPEFLD
.Following used for testing purposes only - ASH
.         FILEPI  1;TESTER
.         READ    TESTER,NSPEFLD;STR6,#holdSTR
         CALL      NSPEKEY
         pack      #holdstr,DESC001,DESC002
.Throw away carriage return characters.  Do not need them for this situation.
         append    #carr,#carrfill
         append    " ",#carrfill
         reset     #carrfill
         rep       #carrfill,#holdstr
         move      C1,N1
         move      "11",N2
         loop
                   call      PARSITUP using #line1,#holdstr,C1
                   call      Trim using #line1
                   if (#line1 <> "")
                             DISPLAY   *P31:N2,*EL,#line1;
                             add       C1,N2
                   endif
                   until (N1 >= 9)
                   add       C1,N1
         repeat
.END PATCH 1.2 - REPLACED LOGIC
.
.
. DISPLAY LR# AND ORDER DATE;
.
DISLR    DISPLAY   *P35:21,*EL,"L.R.#: ",NORDFLD;
.
.      *FOR (ORDER-DATE) DISPLAY;
.
.START PATCH #1.1 - REPLACED LOGIC
.         DISPLAY   *P49:21,"Order Date:  ",OODTEM,"/",OODTED,"/",OODTEY:
.                   *P71:21,"DJ#: ",ODOWJ;
         DISPLAY   *P49:21,"Order Date:  ",OODTEM,"/",OODTED,"/",OODTEC,OODTEY:
                   *P70:21,"DJ#: ",ODOWJ;
.END PATCH #1.1 - REPLACED LOGIC
         keyin     str1
         RETURN 

. END OF DISPLAY, 

