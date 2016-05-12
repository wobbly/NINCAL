PC             EQU       0
               INC       COMMON.INC
               INC       CONS.INC
Release        Init           "NO"
               INC       CONSACCT.inc
.
               INCLUDE   NINVDD.inc
               INCLUDE   NINVACDDD.inc
               Include            Nmrgdd.inc
               include            nShpdd.inc
SHIPSW         DIM       1    "Y" IF SHIPPING INFO
MRGSW          DIM       1    "Y" IF MERGE INFO
count          Form           6
writes         form           6
str1a          dim            1
PINVFILE        IFILE          KEYLEN=6,FIX=303                        .Print file Duplicates master + 3 bytes typist info
NinInvDev      Ifile          Keylen=6,fixed=300
;
           Move               C1 to Ninvpath
           open               NINvfile,NINVNAME,Read
;           open               NINvfile,NINVNAME
           move               c1 to ninvflag
;           Prepare            NINvAcdFile,"\\nts1\e\data\text\NINInvAcd.dat","\\nts1\e\data\index\NINInvAcd.isi","9","49",exclusive
           Prepare            NINvAcdFile,"c:\work\NINInvAcd.dat","c:\work\NINInvAcd.isi","9","49",exclusive
           Prepare            NININVDEV,"c:\work\NINInvDev.dat","c:\work\NINInvDEv.isi","6","300",exclusive
           Prepare            PInvFile,"\\nts1\e\data\NINVoiceDev.dat","\\nts1\e\data\index\NINvoiceDEv.isi","6","303"
           move               c1 to NInvAcdflag
Looper         CAll           Ninvks
               goto           eoj if over
breakit        add            c1 to count
               display        *p10:10,"Records read ",count,b1,invnum
               if             (InvDtey = "05" and INvdtem = "10")
               goto	looper
               endif
               pack           str6 from invdtec,invdtey,invdtem
               rep            zfill in str4
               move           str6 to n6
;               if             (n4 = 200503)
;               goto           looper
;               endif
               Packkey        Nmrgfld from lrn
;               move           lrn to nmrgfld
               move           c0 to nmrgrqty
               move           c0 to nmrgiqty
               move           c0 to nmrgnet
               move           no to mrgsw
               move           no to SHIPsw
               call           nmrgkey
               if             not over
               move           yes to mrgsw
               endif
               packkey        Nshpfld from lrn
;               move           lrn to nshpfld
               call           nshpkey
               if             not over
               move           yes to shipsw
               endif

               For            Index of "1" to "10"
               LOAD           STR15 FROM INDEX OF ADDCHG1,ADDCHG2,ADDCHG3,ADDCHG4,ADDCHG5:
                              ADDCHG6,ADDCHG7,ADDCHG8,ADDCHG9,ADDCHG10
..................................................................................
.  1-3        ADD. CHG. CODE USED TO ACCESS NINACD/TEXT.
.  4-10       AMOUNT IF FLAT FEE, OR RATE PER M. BASED ON ADD. CHG. CODE.
;10Sep2003 dlh - for Nadj0001 release 1.4
. 11-11      AEXTCD  ---  if 1=use & an adjustment call go calc use original qtybild for calc, else use adj qty
. 12-14      PERCENTAGE OF ADD. CHG. TO LR INCOME, REMAINDER TO A/P.
. 15-15      '1'= DON'T PRINT CHARGE ON INVOICE ie: RUNNING CHARGES
.            ADD TO A/R AND A/P1 *
.           ' '= STRAIGHT BILLING
.                INTO A/R AND A/P1 *
.               UNLESS 2 A/P'S THEN TO A/R & A/P2.
.        OR     INCOME/PAYCODE = '3' THEN TO A/R & LR.
.           '2'= INTO A/R AND A/P1 *
.
.         * ALSO PERCENTAGE MAY BE COMBINED WITH CODES 1 & 2 TO SPLIT BETWEEN
.           LRINC & A/P.
...............................................................................

               UNpack         str15 into NinvAcdCode,str7,aextcd,str3,str1a
               if             (NinvAcdCode = "   " or NinvAcdCode = "")
               break          
               endif
               if             (NinvAcdCode = "00A")
               Move           "100" to NinvAcdCode
               elseif         (NinvAcdCode = "00B")
               Move           "101" to NinvAcdCode
               endif
               move           c0 to n2
               move           irnetper to n2

;               if             (lrn = "498057")
;               call           debug
;               endif
               pack           Ninvacdnum from c0,index
;               Move           INdex to NinvAcdNum
               MOve           Invnum to NinvAcdINV
               rep            Zfill in Ninvacdnum
               rep            Zfill in NinvacdCode
               rep            Zfill in NinvacdInv
               packkey        NinvAcdFld from NinvAcdINv,NinvAcdNum
               call           NinvAcdTst
               IF             not over
               Break
;               goto           looper
               endif
               move           str7 to cmpt92
               DIVIDE         HUND BY CMPT92      ;/
               MOVE           CMPT92 TO NInvAcdRate
               MOVE           STR3 TO FORM32
               DIVIDE         HUND BY FORM32      
               MOVE           C0 TO NinvAcdPErc
               MOVE           FORM32 TO NinvAcdPErc
;               MOVE           STR1a TO NInvAcdFlag            .anincd
               pack           Ninvacdanincd from b1,b1,str1a
;               MOVE           STR1a TO NINVAcdANINCD           .anincd
               Move           AExtCd to NinvAcdAextcd
               Move           c0 to N3
;maybe we should lets goto new codes.
         	RESET     DISCMLRS                  .DISCOUNTED MLR?
	scan	mlrn in discmlrs
	if	not equal
               	If             (ninvacdcode = "081")             Comm recapture (leave for now)
;              	 Move           "048" to Ninvacdcode
               	elseif         (ninvacdcode = "085")             Broker COmm  (leave for now)
;              	 Move           "041" to Ninvacdcode
               	elseif         (ninvacdcode = "090")              Business Select    (leave for now)
;              	 Move           "033" to Ninvacdcode
               	elseif         (ninvacdcode = "069")             Cleaning Fee   (leave for now)
;              	 Move           "029" to Ninvacdcode
               	elseif         (ninvacdcode = "067")
               	move	qtyin to cmpt94
               	div            hund into cmpt94
               	mult	"10" by cmpt94
               		IF	(cmpt94 = Ninvacdrate)
               		Move           "045" to Ninvacdcode
               		Move	"10.00" to NInvAcdRate
               		endif
               	elseif         (ninvacdcode = "068")	  Hot line (leave for now)
;              	 Move           "026" to Ninvacdcode
               	elseif         (ninvacdcode = "086")
;              	 Move           "042" to Ninvacdcode
               	elseif         (ninvacdcode = "091")
;              	 Move           "035" to Ninvacdcode
               	elseif         (ninvacdcode = "066")
;              	 Move           "013" to Ninvacdcode
               	elseif         (ninvacdcode = "061")
;              	 Move           "020" to Ninvacdcode
               	elseif         (ninvacdcode = "087")
;              	 Move           "044" to Ninvacdcode
               	elseif         (ninvacdcode = "056")
;              	 Move           "018" to Ninvacdcode
               	elseif         (ninvacdcode = "062")       ;MMMMM  compute has this as shipping even though desc is zip charge?????
;              	 Move           "043" to Ninvacdcode
               	endif
               Else
               	if         (ninvacdcode = "067")
               	Move           "045" to Ninvacdcode
               	Move	"5.00" to NInvAcdRate
               	endif
               endif	
               Move           NinvAcdCode to N3
               If             (N3 < 12)
               Move           b1 to NINVacdRateT
               elseif         (N3 > 11 and N3 < 51)
               Move           "m" to NINVacdRateT
               elseif         (N3 > 50)
               Move           "f" to NINVacdRateT
               endif
               If	(Ninvacdcode = "153" or ninvacdcode = "151" or ninvacdcode = "155")
               Move           "m" to NINVacdRateT
	 endif               
;               If             (ninvacdcode = "100" or ninvacdcode = "153" or ninvacdcode = "154")
               If             (ninvacdcode = "100"  or ninvacdcode = "154")
               Move           b1 to NINVacdRateT
               endif
               If             (NinvAcdRateT = "m")
                              if             (mrgsw = Yes | n2 != 0)
                              move           qtyin to NINvAcdQty
;take care of minimums/old invoices that have 0 qtyin
                                       if        (qtyin < 5000)
                                       MOVE      QTYbild TO NINvAcdQty
                                       endif
                              else
                              move           QTYbild  to NINvAcdQty
                              endif
;
                              if        (ninvacdcode = "041" | ninvacdcode = "035")
                              MOVE      QTYbild TO NINvAcdQty
                              endif
                              IF	(NInvAcdCode = "045" & NInvAcdQty < 5000)
                              Move	"5000" to Ninvacdqty                         .minumum
;                              aLERT          cAUTION,"Doing Minimum thing",result
                              endif
                              
               Else
               MOve           c0 to NINvAcdQty
               endif
               Call           NInvAcdwrt
               repeat
               add            c1 to Writes
               display        *p10:12,"Invoices Written ",writes
               clear          str15
               move           Imlrchk to str15
               move           c0 to n3
               MOVE      c0 TO FORM32
		MOVE      COMMPCT TO FORM32

               write          NinINvDev,lrn;CODE:
                              STATB:
                              B2,MLRN,b3,LRN,BILLTN,PAYTN,LOINVN,AR,AP1,AP2,AP3:
                              form32,PAYCODE,n3,QTYIN,n3,QTYBILD,PPM,b3,INVNUM,INVDTEc,INVDTEY,INVDTEM,INVDTED:
;                              COMMPCT,PAYCODE,n3,QTYIN,n3,QTYBILD,PPM,b3,INVNUM,INVDTEc,INVDTEY,INVDTEM,INVDTED:
;                              ADDCHG1,ADDCHG2,ADDCHG3,ADDCHG4,ADDCHG5,ADDCHG6,ADDCHG7,ADDCHG8,ADDCHG9,ADDCHG10:
                              COBN,invsales,INCC,B2,LON,b2,lon1,b2,lon2,WSJPC,b2,IBRKNUM,IBRKCNT,IRCQTY,n3,IREXQTY,iexPPM:
                              irnetper,inetrc,ADJC,statpay,CHKN1,CHK1DTEc,CHK1DTEY,CHK1DTEM,CHK1DTED,CHKN2,CHK2DTEc,CHK2DTEY,CHK2DTEM,CHK2DTED:
                              CHKN3,CHK3DTEc,CHK3DTEY,CHK3DTEM,CHK3DTED,LET90D,MLRPAYR,MLRPAYD,GUARPAY,str15

               Goto           Looper
Eoj
               stop
           Include   Comlogic.inc
           Include            Nmrgio.inc
           include            nShpio.inc
           INCLUDE   NINVio.inc
           INCLUDE   NINVACDio.inc

