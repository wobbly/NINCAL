	include	common.inc
	include	cons.inc
	include	norddd.inc
.	include	nord4dd.inc
.	include	nord5dd.inc
.	include	nord6dd.inc
	include	gnxtdd.inc

release	init	"temp"
tempfile2	file
ORDVARS2	LIST
ORCODE2   DIM       1      001-001      ORDER CODE, "S"
OSTAT2    DIM       1      002-002      STATUS, 0,B,Q,X,p,x,l,z
OMLRNUM2  DIM       4      003-006      MAILER NUMBER, AIM KEY 1
OLRN2     DIM       6      007-012     LR NUMBER, KEY
OCOBN2    DIM       3      013-015     CONTACT # (NIN) OR BROKER # (CMP)
OLNUM2    DIM       6      016-021      LIST NUMBER, AIM KEY 2
OLON2     DIM       4      022-025     LIST OWNER NUMBER
OMLRPON2  DIM       12     026-037     MAILER PURCHASE ORDER NUMBER was 7
OQTY2     DIM       9      038-046     QUANTITY,  X,XXX,XXX
OPPM2     DIM       5      047-051     PRICE PER THOUSAND, XXX.XX
OMLRKY2   DIM       12     052-063     MAILER'S KEY
OFOCODE2  DIM       2      064-065     FURNISHED-ON CODE, 0,1,2,3,4,5,6,7,8,OR 9
ORTNDTEC2 DIM       2      066-067     RETURN DATE (CENTURY)
ORTNDTEY2 DIM       2      068-069     RETURN DATE (YEAR)
ORTNDTEM2 DIM       2      070-071     RETURN DATE (MONTH)
ORTNDTED2 DIM       2      072-073     RETURN DATE (DAY)
OMDTEC2   DIM       2      074-075      MAIL DATE (CENTURY)
OMDTEY2   DIM       2      076-077       MAIL DATE (YEAR)
OMDTEM2   DIM       2      078-079      MAIL DATE (MONTH)
OMDTED2   DIM       2      080-081      MAIL DATE (DAY)
OTOCODE2  DIM       1      082-082      TEST ORDER CODE, "1"
OSOTCODE2 DIM       1      083-083      SELECTION ON TEST CODE, 1,2 OR 3
OCCODE2   DIM       1      084-084      CONTINUATION CODE, "1"
OLRNCO2   DIM       6      085-090      LR NUMBER OF CONTINUATION CODE
OODTECOC2 DIM       2      091-092      ORDER DATE OF CONTINUATION CODE (CENTURY)
OODTECOY2 DIM       2      093-094       ORDER DATE OF CONTINUATION CODE (YEAR)
OODTECOM2 DIM       2      095-096      ORDER DATE OF CONTINUATION CODE (MONTH)
OODTECOD2 DIM       2      097-098      ORDER DATE OF CONTINUATION CODE (DAY)
OQTYCO2   DIM       9      099-107      QUANTITY OF CONTINUATION ORDER, XXX,XXX,XXX
OSPI2     DIM       24     108-131      SPECIAL INSTRUCTION CODES, MAX.6 (2 DIGIT)
OBildrct2 DIM       1      132-132      Placed as bill direct? y=yes n=no ' '=no
OBRKGUAR2 DIM       1      133-133       BROKER GUARANTY, '1' = 30 DAY.
OELCODE2  DIM       1      134-134       ENTIRE LIST CODE
OODNUM2   DIM       7      135-141       OFFER DeSCRIPTION NUMBER
OODES2    DIM       5      142-146       OFFER DESCRIPTION
ONETQTY2  DIM       9      147-155       ORDER NET QUANTITY
OCAMP2    DIM       6      156-161       ASSOCIATED CAMPAIGN
OCLRSTAT2 DIM       1      162-162       CLEARANCE STATUS  1=EXCHANGE, 2=RENT, 3=EXC/SPLIT, 4=DENIED
OCLRINIT2 DIM       3      163-165       INITS OF PERSON WHO CLEARED LCR
OBRKRPT2  DIM       1      166-166       OUTSIDE BROKER NOTIFIED OF CLEARANCE STATUS?  1=YES, B1=NO
OCLRDTEC2 DIM       2      167-168       CLEAR DATE (CENTURY)
OCLRDTEY2 DIM       2      169-170       CLEAR DATE (YEAR)
OCLRDTEM2 DIM       2      171-172       CLEAR DATE (MONTH)
OCLRDTED2 DIM       2      173-174       CLEAR DATE (DAY)
ORENT2    DIM       1      175-175       LCR REQUEST FOR RENT "1" = YES
OHIST2    DIM       1      176-176       ORDER HISTORY "l"-LCR to Live, "L"-In-House LCR to Live, "p"-Pending to Live
OXPPM2    DIM       5      177-181       EXCHANGE PRICE PER THOUSAND, XXX.XX
ORTNNUM2  DIM       4      182-185       RETURN-TO NUMBER
OTAPERET2 DIM       1      186-186       TAPE-RETURNABLE ?
OUQTY2    DIM       9      187-195       UNIVERSE QUANTITY, XXX,XXX,XXX
OSALES102 DIM       1      196-196       TENS DIGIT OF SALESMAN #.
OSALES2   DIM       1      197-197       ONES DIGIT OF SALESMAN CODE
OCOCODE2  DIM       2      198-199       CONTACT CODE, 1,2,3, OR 4
OCO2CODE2 DIM       2      200-201       CONTACT CODE, 1,2,3, OR 4
OODTEC2   DIM       2      202-203       ORDER DATE (CENTURY)
OODTEY2   DIM       2      204-205       ORDER DATE (YEAR)
OODTEM2   DIM       2      206-207       ORDER DATE (MONTH)
OODTED2   DIM       2      208-209       ORDER DATE (DAY)
OSCODE2   DIM       1      210-210       SAMPLE CODE, 1,2, OR 3
OCOMSLCT2 DIM       1      211-211       overlay: COMSELECT='C', lifestyle="L"
OSHP2     DIM       2      212-213       SHIPPED VIA CODE, 0,1,2...9.
O1DES2    DIM       35     214-248       LINE #1 OF LIST DESCRIPTION, DATACARD
O2DES2    DIM       35     249-283       LINE #2 OF LIST DESCRIPTION, KEYIN
OREUSE2   DIM       6      284-289       RE-USE LR #, RTN-TO # WILL BE '0'
ODOWJ2    DIM       3      290-292       TYPISTS INITIALS was 2
OEXQTY2   DIM       9      293-301       EXCHANGE QTY ON SPLIT ORDER.
GUARCODE2 DIM       1      302-302       NIN GUARANTY CODE, '1' = 30 DAY.
OBRKNUM2  DIM       4      303-306        BROKER/CONSULTANT NUMBER.
OBRKCNT2  DIM       3      307-309        BROKER/CONSULTANT CONTACT NUMBER.
osamcde2  dim       3      310-312        sample numbers
onetper2  dim       2      313-314        net name percentage (NN = Net Name)
onetrc2   form      3.2    315-320        net name running charge
onetfm2   dim       1      321-321        net flag (F)lat'volume', per (M)'net', (N)o
onetmin2  form      7      322-328        net name minimum.
ofiller2  dim       80     329-408
	LISTEND

	CALL	PAINT
	ERASE	"C:\WORK\ORDERR3.DAT"
	prep	tempfile2,"C:\WORK\ORDERR3.DAT",exclusive
.
	move	C1,NORDPATH
	open	tempfile,"\\NTS1\E\DATA\NINPRINTL2.DAT",exclusive
	loop
		read	tempfile,SEQ;ORDVARS2
		until	over
		add	C1,howmany
		display	*p10:10,"records ",howmany
		pack	NORDFLD,OLRN2
		CALL	NORDKEY
		if over
			WRITE	TEMPFILE2,SEQ;"over",NORDFLD
		else
			move	C0,N4
			if (OCOCODE2 <> OCOCODE)
				add	"1",N4
			endif
			if (OLNUM2 <> OLNUM)
				add	"2",N4
			endif
			if (OCO2CODE2 <> OCO2CODE)
				add	"4",N4
			endif
			if (OHIST2 <> OHIST)
				add	"8",N4
				move	OHIST2,OHIST
			endif
			if (OSTAT2 <> OSTAT)
				add	"16",N4
			endif
			pack	str8,OCLRDTEC2,OCLRDTEY2,OCLRDTEM2,OCLRDTED2
			pack	str9,OCLRDTEC,OCLRDTEY,OCLRDTEM,OCLRDTED
			if (OCLRSTAT2 <> OCLRSTAT)
				add	"32",N4
				move	OCLRSTAT2,OCLRSTAT
			endif
			if (OCLRINIT2 <> OCLRINIT)
				add	"64",N4
				move	OCLRINIT2,OCLRINIT
			endif
			if (str8 <> str9)
				add	"128",N4
				move	OCLRDTEC2,OCLRDTEC
				move	OCLRDTEY2,OCLRDTEY
				move	OCLRDTEM2,OCLRDTEM
				move	OCLRDTED2,OCLRDTED
			endif
			if (N4 > C0)
				//Write to error file for hand viewing!!
				WRITE	TEMPFILE2,SEQ;N4," ",ORDVARS2
				if (N4 = "8" | n4 > "16")
					//call	NORDUPD
				endif
			endif
		endif
	repeat
	CLOSE	TEMPFILE
	shutdown

	include	gnxtio.inc
	include	nordio.inc
.	include	nordio4.inc
.	include	nordio5.inc
.	include	nordio6.inc
	include	comlogic.inc