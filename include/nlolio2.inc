.RELEASE 1.4 ASH 14MAR2005 Exchange File conversion
.RELEASE 1.3 ASH 28OCT2004 Added call to NMLRKEY in order to verify Mailer Name
.RELEASE 1.2 ASH 16APR2004 Extended size of:  NLOLUNIVERSE, NLOLPRICE
.START PATCH 1.1 ADDED VAR
USAGE1HOLD	FORM	10
USAGE2HOLD	FORM	10
.END PATCH 1.1 ADDED VAR
OrderTestXSTAT
.Called by:  OrderLoadLOLScreen,OrderLoadLOLScreen2,Order1StatExchangeMssg
.OLNUM & OMLRNUM need to be established beforehand
.returns taskname with appropriate XSTAT/NULL
        clear   taskname
        move    C0,EFLAG
        move    OLNUM,NXRFFLD
        clear   NXRFMLR
        move    C1,NXRFPATH
        move    "O.TestXStat-NXRFKEY",Location
        pack    KeyLocation,"Key: ",NXRFFLD
        call    NXRFKEY
        if not over
                clear   ACKEY
                clear   NXNGFLD1
                clear   NXNGFLD2
.START PATCH 1.4 REPLACED LOGIC - TEMPORARY PATCH
.                pack    ACKEY,OMLRNUM,NXRFMLR
.                pack    NXNGFLD1,AKey1A,OMLRNUM
.                pack    NXNGFLD2,AKey2A,NXRFMLR
		pack	COMPFLD3,OMLRNUM
		move	"COMPKEY3",Location
		pack	KeyLocation,"Key: ",COMPFLD3
		call	COMPKEY3
		move	COMPNUM,str6
.
		pack	COMPFLD,NXRFMLR
		move	"2-COMPKEY",Location
		pack	KeyLocation,"Key: ",COMPFLD
		call	COMPKEY
.
                pack    ACKEY,str6,COMPNUM
		pack    NXNGFLD1,AKey1A,str6
		pack    NXNGFLD2,AKey2A,COMPNUM
.END PATCH 1.4 REPLACED LOGIC - TEMPORARY PATCH
                rep     ZFILL,ACKEY
                move    "O.TestXStat-NXNGAIM",Location
                pack    KeyLocation,"Key: ",NXNGFLD1,COMMA,NXNGFLD2
                call    NXNGAIM
                if over
.Check to see if Mlr/List Mlr have been reversed
                        clear   ACKEY
                        clear   NXNGFLD2
                        clear   NXNGFLD1
.START PATCH 1.4 REPLACED LOGIC - TEMPORARY PATCH
.                        pack    ACKEY,NXRFMLR,OMLRNUM
.                        pack    NXNGFLD2,AKey2A,OMLRNUM
.                        pack    NXNGFLD1,AKey1A,NXRFMLR
............................
			pack    ACKEY,COMPNUM,str6
			pack    NXNGFLD2,AKey2A,str6
			pack    NXNGFLD1,AKey1A,COMPNUM
.END PATCH 1.4 REPLACED LOGIC - TEMPORARY PATCH
                        rep     ZFILL,ACKEY
                        move    "O.TestXStat-NXNGAIM",Location
                        pack    KeyLocation,"Key: ",NXNGFLD1,COMMA,NXNGFLD2
                        call    NXNGAIM
                        if over
                                move    "No Exchange History",taskname
                                return
                        endif
                        match   ACKEY,ACCKEY
                        if not equal
                                move    "No Exchange History",taskname
                                return
                        endif
                        move    C2,EFLAG
                else
                        match   ACKEY,ACCKEY
                        if not equal
                                move    "No Exchange History",taskname
                                return
                        endif
                        move    C1,EFLAG
                endif
.
                move    C1,NXCHPATH
                pack    NXCHFLD1,ACKEY,ENTRY
                rep     ZFILL,NXCHFLD1
                move    "ExAcLoop-NXCHKEY",Location
                pack    KeyLocation,"Key: ",NXCHFLD1
                call    NXCHKEY
                if over
                        move    C0,EFLAG
                endif
        endif
        if (EFLAG <> "0")
.START PATCH 1.1 REPLACED LOGIC
.                compare USAGE1,USAGE2
.                if less
.                        sub     USAGE2,USAGE1
.                        if (EFLAG = "1")
.                                call    Trim using MCOMP
.                                append  MCOMP,taskname
.                                append  " owes ",taskname
.                                move    USAGE1,str10
.                                call    FormatNumeric using str10,str13
.                                append  str13,taskname
.                                append  " names.",taskname
.                        else
.                                call    Trim using OLSTNAME
.                                append  OLSTNAME,taskname
.                                append  " owes ",taskname
.                                move    USAGE1,str10
.                                call    FormatNumeric using str10,str13
.                                append  str13,taskname
.                                append  " names.",taskname
.                        endif
.                elseif not equal       .Greater
.                        sub     USAGE1,USAGE2
.                        if (EFLAG = "1")
.                                call    Trim using OLSTNAME
.                                append  OLSTNAME,taskname
.                                append  " owes ",taskname
.                                move    USAGE2,str10
.                                call    FormatNumeric using str10,str13
.                                append  str13,taskname
.                                append  " names.",taskname
.                        else
.                                call    Trim using MCOMP
.                                append  MCOMP,taskname
.                                append  " owes ",taskname
.                                move    USAGE2,str10
.                                call    FormatNumeric using str10,str13
.                                append  str13,taskname
.                                append  " names.",taskname
.                        endif
...............................
.START PATCH 1.3 ADDED LOGIC
		pack	MKEY,OMLRNUM,"000"
		move	"O.TestXStat-NMLRKEY",Location
		pack	KeyLocation,"Key: ",MKEY
		call	NMLRKEY
.END PATCH 1.3 ADDED LOGIC
		move	C0,USAGE1HOLD
		move	C0,USAGE2HOLD
		move	USAGE1,USAGE1HOLD
		move	USAGE2,USAGE2HOLD
                compare USAGE1HOLD,USAGE2HOLD
                if less
                        sub     USAGE2HOLD,USAGE1HOLD
                        if (EFLAG = "1")
                                call    Trim using MCOMP
                                append  MCOMP,taskname
                                append  " owes ",taskname
                                move    USAGE1HOLD,str10
                                call    FormatNumeric using str10,str13
                                append  str13,taskname
                                append  " names.",taskname
                        else
                                call    Trim using OLSTNAME
                                append  OLSTNAME,taskname
                                append  " owes ",taskname
                                move    USAGE1HOLD,str10
                                call    FormatNumeric using str10,str13
                                append  str13,taskname
                                append  " names.",taskname
                        endif
                elseif not equal       .Greater
                        sub     USAGE1HOLD,USAGE2HOLD
                        if (EFLAG = "1")
                                call    Trim using OLSTNAME
                                append  OLSTNAME,taskname
                                append  " owes ",taskname
                                move    USAGE2HOLD,str10
                                call    FormatNumeric using str10,str13
                                append  str13,taskname
                                append  " names.",taskname
                        else
                                call    Trim using MCOMP
                                append  MCOMP,taskname
                                append  " owes ",taskname
                                move    USAGE2HOLD,str10
                                call    FormatNumeric using str10,str13
                                append  str13,taskname
                                append  " names.",taskname
                        endif
.END PATCH 1.1 REPLACED LOGIC
                else
                        append  "Exchange Status is Even",taskname
                endif
                reset   taskname
        else
                move    "No Exchange History",taskname
        endif
        return

OrderLoadLOLPackLOL
        move    B55,NLOLCode
        move    B55,NLOLStat
        move    B55,NLOLLOL
        move    OCAMP,NLOLCNum
        move    OLNUM,NLOLList
        move    B55,NLOLLCR
        move    OLON,NLOLOwner
.START PATCH 3.72 REPLACED LOGIC
.This patch was instigated from patch applied to NORDTEST.PLS!!!
.Uses same patch number found in NORDTEST.PLS for better reference
.        move    O2DES,NLOLSelect
.        move    OUQTY,NLOLUniverse
	packkey	NSEL2FLD,"1",OLRN
	move	"O.LoadLOLP.LOL-NSEL2KEY",Location
	pack	KeyLocation,"Key: ",NSEL2FLD
	call	NSEL2KEY
	if over
	        move    O2DES,NLOLSelect
        	move    OUQTY,NLOLUniverse
		unpack	OPPM,str3,str2
		pack	str6,str3,".",str2
		rep	zfill,str6
.START PATCH 1.2 ADDED LOGIC
		move	C0,NSEL2PRICE
.END PATCH 1.2 ADDED LOGIC
		move	str6,NSEL2PRICE		.Will eventually be defunct!
	else
	        move    NSEL2NAME,NLOLSelect
.START PATCH 1.2 REPLACED LOGIC
.		move	NSEL2QTY,N9
.		move	N9,NLOLUNIVERSE
		move	NSEL2QTY,N10
		move	N10,NLOLUNIVERSE
.END PATCH 1.2 REPLACED LOGIC
		rep	zfill,NLOLUNIVERSE
	endif
.END PATCH 3.72 REPLACED LOGIC
        pack    NLOLDate,OODTEC,OODTEY,OODTEM,OODTED
        move    B55,NLOLModDate
        move    B55,NLOLInits
        move    OQTY,NLOLQty
        move    ONETQTY,NLOLNetQty
        move    C0,N9
        move    C0,N10
        move    NLOLQty,N10
        move    NLOLNetQty,N9
        if (N9 <> C0 AND N10 <> C0)
                move    C0,CALCPER
                move    C0,N32
                div     N10,N9,CALCPER
                mult    "100",CALCPER
                move    CALCPER,N32
                move    N32,NLOLNET
        else
                clear   NLOLNET
        endif
        move    B55,NLOLTest
........................
..        if (OTOCODE = "1")
..                if (OCCODE = "1")
..                        move    "3",NLOLTEST
..                else
..                        move    "1",NLOLTEST
..                endif
..        elseif (OCCODE = "1")
..                move    "2",NLOLTEST
..        endif
.        if (OCCODE = "1")       .CONTINUATION
.                move    "0",NLOLTEST
.        elseif (OTOCODE = "1")  .TEST
.                move    "1",NLOLTEST
.        endif

	if (OLRN = "610360" | OLRN = "605366")
		CALL	BREAKER
	endif
.begin patch xxx DLH 2014 October 23  if a continuation and retest display as retest
.        if (OCCODE = "1")       .CONTINUATION
        if (OCCODE = "1" & OTOCODE <> "2" )       .CONTINUATION and not retest
.end patch xxx DLH 2014 October 23  if a continuation and retest display as retest
                move    "0",NLOLTEST
        elseif (OTOCODE = "1")  .TEST
                move    "1",NLOLTEST
        elseif (OTOCODE = "2")  .RETEST
                move    "2",NLOLTEST
        endif
.......................
        if (OELCODE = "2" | OELCODE = "3")
                if (OSTAT = "l" | OSTAT = "p" | OSTAT = "x" | OSTAT = "z")
                        if (ORENT = "1")  .LCR Rental
                                move    "3",NLOLRent
                       else
                                move    "1",NLOLRent
                        endif
                else    .Orders
                        move    C0,N10
                        move    OEXQTY,N10
                        if (N10 > C0)
                                move    "3",NLOLRent
                        else
                                move    "1",NLOLRent
                        endif
                endif
        else
                move    "2",NLOLRent
        endif
.START PATCH 3.72 REPLACED LOGIC
.        pack    NLOLPrice,B1,OPPM
	move	NSEL2PRICE,str8
	call	RemoveChar using str8,PERIOD
.START PATCH 1.2 REMOVED LOGIC
.	call	Trim using str8
.END PATCH 1.2 REMOVED LOGIC
        pack    NLOLPrice,str8
.END PATCH 3.72 REPLACED LOGIC
        move    B55,NLOLRate
        move    B55,NLOLGift
        move    B55,NLOLNetReq
        move    B55,NLOLNetApp
        move    B55,NLOLRegional
        unpack  OODNUM,str4,NLOLOffer
        move    OSAMCDE,NLOLSample
        move    B55,NLOLPackage
        pack    NLOLMDate,OMDTEC,OMDTEY,OMDTEM,OMDTED
        pack    NLOLComment,B55,B55,B55
        pack    NLOLComment1,B55,B55,B55
        move    B55,NLOLLR
        move    B55,NLOLFiller
        return

BREAKER
	MOVE	STR1,STR1
	RETURN