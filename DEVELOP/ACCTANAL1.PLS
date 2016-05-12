.value analysis.
.created May 2001 - DLH
PC            equ             0
              include         common.inc
              include         cons.inc
              include         norddd.inc
              include         nmlrdd.inc
              include         nbrkdd.inc
              include         oslspern.inc
              include   	  winapi.inc
              include   	  ninvdd.inc
.begin beta 2
              include             consacct.inc
.begin beta 3
.              include             njstdd.inc
              include             nadjdd.inc
.end beta 3
              include             nacddd.inc
              include             nowndd.inc
              include             nmrgdd.inc
              include             nshpdd.inc
              include             ndatdd.inc
              include             ndat3dd.inc
.end beta 2
.
OSTM0    INIT      "NO SALESPERSON "
OSTM1    INIT      "JEANETTE'S TEAM "
OSTM2    INIT      "               "
OSTM3    INIT      "SUSAN'S TEAM"
OSTM4    INIT      "            "
OSTM5    INIT      "               "
OSTM6    INIT      "LIST MANAGEMENT "
OSTM7    INIT      "SUZIE'S TEAM "
OSTM8    INIT      "             "
OSTM9    INIT      "           "
OSTM10   INIT      "               "
OSTM11   INIT      "   "
OSTM12   INIT      "  "
OSTM13   INIT      " "
OSTM14   INIT      " "
OSTM15   INIT      " "
OSTM16   INIT      " "
OSTM17   INIT      " "
OSTM18   INIT      " "
OSTM19   INIT      " "
OSTM20   INIT      " "
................................................................................................
FIle          Ifile           key=10                    .mailer/broker/slsperson
File2         file
.
.output file vars.
Avars         List
Amlr          dim            4     1-4          .key \
ABRK          dim            4     5-8          .key  >
ASLSpers      dim            2     9-10         .key /
AINVcount     form           5    11-15          .number of invoices to calc with
ADaystopay    form           5    16-20          .number of 'days to pay' to calc with
Aorders       form           5    21-25          .number of orders in period.
AordersQ      form           9    26-34          .number of ordered Names in period.
Ainvoices     form           5    35-39          .number of orders/Invoices in period for aver pay calc
Acancld       form           5    40-44          .number of cancelled orders
AcancldQ      form           9    45-53          .number or cancelled orders QTY
ALCRs         form           5    54-58          .number of LCRs
ALCRsQ        form           9    59-67          .number of LCRs  QTY
ACAncLcr      form           5    68-72          .number of Cancelled LCRs
ACAncLcrQ     form           9    73-81          .number of Cancelled LCRs QTY
Apending      form           5    82-86          .number of pending orders
ApendingQ     form           9    87-95          .number of pending orders  QTY
AcancPnd      form           5    96-100          .cancelled pending
AcancPndQ     form           9   101-109          .cancelled pending  QTY
ALRINC        Form           9.2 110-121        .lrinc in period
ANinInc       form           9.2 122-133
              ListEnd
.

release        init           "Beta 3"          .add invoice and adjustment pass
RelDate       init           "June 2001"

.release       init           "Pre"
.RelDate       init           "May 2001"
.
count         form           6          .number of orders in
counti        form           6          .number of invoices in
counta        form           6          .number of adjustments in
startdate     form           5
enddate       form           5
outkey        dim            8
str10a        dim            10
holdmdate     form           5
countinv      dim            1
orderflag     dim            1
Days     dim       5
NDays    form      5
.begin patch beta2
. 385477 FIRST INV# OF 2001
shipsw        dim       1
mrgsw         dim       1
.end patch beta2
.
              move           c1 to nordpath
              move           c1 to ninvpath
              move           c3 to nordlock
              move           c1 to ndatpath
              move           c1 to nownpath
              call           paint
.              goto           output
.              open           file,"c:\work\analysis.isi"
              prepare        File,"c:\work\analysis.dat","C:\work\analysis.isi","10","133",exclusive

              move           "07" to mm
              move            "01" to dd
              move            "01" to yy
              call            cvtjul
              move            juldays to startdate
.
              move            "09" to mm
              move            "30" to dd
              move            "01" to yy
              call            cvtjul
              move            juldays to enddate
              display        *p10:10,"Rec's In:"
              packkey 	  nordfld,"350000"
              call            nordtst
.
MAIN
              call            Nordks
              goto            output if over
.
              add            c1 to count
              display        *p25:10,count
.
              if             (oodtey <> "01")
              goto           main
              endif
              if             (omlrnum = "7770")
              call           debug
              endif
.
              packkey         outkey from omlrnum,obrknum,osales10,osales
.
              move            oodtem to mm
              move            oodted to dd
              move            oodtey to yy
              call            cvtjul
              if              (startdate <= juldays and juldays <= enddate)
              Move	  yes to orderflag           .order within range
              else
              move            no to orderflag
              goto            main
              endif
.
              reset           runcodes
              scan            olnum in runcodes
              goto            main if equal

 	if       	  (olnum = "014477" or olnum = "017889" or olnum = "009766" or olnum = "005051" or olnum = "016909" or olnum = "016898")            .NON List Management Related
              move            No to OrderFlag
              GOTO            MAIN
              endif

              move            c0 to n9
              move            oqty to n9
              if              (n9 <= 0)
              goto            main
              endif
.
              call            calcpayhist
              IF              (countinv = NO AND ORDERFLAG = NO)
              GOTO            MAIN
              ENDIF

              read            file,outkey;Avars
              goto            writeout if over
.
              if              (COUNTINV = Yes)
              add             Ndays to ADaystopay
              add             c1 to AINVcount
              endif
              if              (orderflag = Yes)
                              If        (ostat = "Q" or OSTAT = "X")
                              Add         c1 to ACancld
			      move        c0 to n9
 			      move        oqty to n9
			      add         n9 to ACancldQ
                              elseif     (ostat = "z")
                              Add         c1 to ACancLCR
			      move        c0 to n9
 			      move        oqty to n9
			      add         n9 to ACancLcrQ
                              elseif     (ostat = "x")
                              Add         c1 to ACancPND
			      move        c0 to n9
 			      move        oqty to n9
			      add         n9 to ACancPndQ
                              elseif     (ostat = "p")
                              Add         c1 to APeNDing
			      move        c0 to n9
 			      move        oqty to n9
			      add         n9 to APendingQ
                              elseif     (ostat = "l")
                              Add         c1 to ALCRs
			      move        c0 to n9
 			      move        oqty to n9
			      add         n9 to ALcrsQ
                              elseif      (ostat = "0" or ostat = "B")
drew
		  Add         c1 to Aorders
	                move        c0 to n9
              	  move        oqty to n9
                  	  add         n9 to AordersQ
              endif
              endif

              IF              (countinv = NO AND ORDERFLAG = NO)
              GOTO            MAIN
.begin patch beta 3
              else
              move           c0 to lrinc
              move           c0 to nininc
              move           c0 to aslrinc
              move           c0 to asNINinc
              CALL           INVGOODIES
.enD patch beta 3
              ENDIF
.
              add             lrinc to Alrinc
              add             NININC to Anininc
              add             aslrinc to Alrinc
              add             asNININC to Anininc
              Update         file;Avars
              call           wipeavars
              goto            main
writeout

              if              (COUNTINV = Yes)
              move             Ndays to ADaystopay
              move             c1 to AINVcount
              endif
              move            obrknum to Abrk
              move            omlrnum to Amlr
              pack            ASLSPERS from Osales10,osales

              if              (orderflag = Yes)
                              If        (ostat = "Q" or OSTAT = "X")
                              move        c1 to ACancld
			        move        c0 to n9
			        move        oqty to n9
			        add         n9 to ACancldQ
                              elseif     (ostat = "z")
                              move        c1 to ACancLCR
			        move        c0 to n9
			        move        oqty to n9
			        add         n9 to ACancLcrQ
                              elseif     (ostat = "x")
                              move        c1 to ACancPND
			        move        c0 to n9
			        move        oqty to n9
			        add         n9 to ACancPndQ
                              elseif     (ostat = "p")
                              move        c1 to APeNDing
			      move        c0 to n9
			        move        oqty to n9
			        add         n9 to APendingQ
                              elseif     (ostat = "l")
                              move        c1 to ALCRs
			      move        c0 to n9
			      move        oqty to n9
			      add         n9 to ALcrsQ
                              elseif      (ostat = "0" or ostat = "B")
	 	move            c1 to Aorders
	              move        c0 to n9
	              move        oqty to n9
	              add         n9 to AordersQ
	                endif
              endif
.begin patch beta 3
              move           c0 to lrinc
              move           c0 to nininc
              move           c0 to aslrinc
              move           c0 to asNINinc
              CALL            INVGOODIES
.enD patch beta 3

              MOVE            lrinc to Alrinc
              MOVE            NININC to Anininc
              add             aslrinc to Alrinc
              add             asNININC to Anininc

              write            file,outkey;Avars

              call           wipeavars

              goto            main
.
.calcpayhist
calcpayhist
              move            No to countInv
              move            c0 to ndays
.
	if 	(OSTAT = "X" OR OSTAT = "z" OR OSTAT = "x" or Ostat = "l")
	Return
              endif
.
.       move      no to ManFeeFlag
.       if       (olnum = "014477" or olnum = "017889" or olnum = "009766" or olnum = "005051" or olnum = "016909" or olnum = "016898")            .NON List Management Related
.       move      Yes to ManFeeFlag
.       endif
        packkey    ninvfld from olrn
        call       ninvkey
        return     if over

         clear     str10a
         if        (omdtem <> "00" and omdtem <> "  ")
         pack      str10a from omdtem,slash,omdted,slash,omdtec,omdtey
         move      omdtem to mm
         move      omdted to dd
         move      omdtey to yy
         else
         pack      str10a from invdtem,slash,invdted,slash,invdtec,invdtey
         move      invdtem to mm
         move      invdted to dd
         move      invdtey to yy
         endif
         call      cvtjul
         move      juldays to holdmdate
        cmatch     "P" to statb
        if         equal
        unpack     mlrpayd into str2,yy,mm,dd
        else
        clock      date to today
        unpack     today into mm,str1,dd,str1,yy
        endif
        call      cvtjul
.
        sub       holdmdate from juldays
        move      c0 to days
        move      juldays to days
        move      days to Ndays
        If        (NDAYS < 0)
        move      c0 to Ndays
        endif
        MOVE      yes to countinv
        return
.................................................................................................
output
              Close          File
              open           File,"c:\work\analysis",read
              prepare        File2,"c:\work\analysis.csv",exclusive
              write          file2,seqeof;*cdfon,b1
              write          file2,seqeof;*cdfon,b1
              write          file2,seqeof;*cdfon,"TEAM","CLIENT","Mlr","Brk","Sales","Invoice","Days","Order","Order":
              	 "Inv","Orders","CNC Order","Lcr","LCR","Lcr","LCR","Pending","Pending","Pending","Pending","LR","NIN","Broker"
              write          file2,seqeof;*cdfon,"Name","Name","Number","Number","number","Count","To Pay","Count","Qty":
              	 "clc qty","CnCld","Qty","Count","QTY","CNC","Cnc qty","Count","Qty","CNC","Cnc qty","Income","Income","Name"
              write          file2,seqeof;*cdfon,b1
              write          file2,seqeof;*cdfon,b1
              move           c1 to nmlrpath
              move           c3 to nmlrlock
              move           c1 to nbrkpath
              move           c3 to nbrklock
Process       read           File,seq;Avars
              goto           eoj if over
              packkey        mkey from Amlr,z3
              call           nmlrkey
              packkey        Nbrkfld from Abrk,z3
              call           nbrkkey
              move           c0 to n2
	move 	 ASLSPERS to n2
              clear          str25
LOADOK        LOAD      str25  FROM N2 OF OSTM1:
                                            OSTM5:     2
                                            OSTM3:     3
                                            OSTM9:     4
                                            OSTM4:     5
                                            OSTM6:     6
                                            OSTM3:     7
                                            OSTM1:     8
                                            OSTM8:     9
                                            OSTM7:    10
                                            OSTM1:    11
                                            OSTM1:    12
                                            OSTM7:    13
                                            OSTM7:    14
                                            OSTM1:    15
                                            OSTM2:    16
                                            OSTM7:    17
                                            OSTM1:    18
                                            OSTM6:    19
                                            OSTM5:    20
                                            OSTM3:    21
                                            OSTM3    .'22

.
              dIVIDE         aINVCOUNT INTO ADAYSTOPAY
              write          file2,seqeof;*cdfon,str25,mcomp,avars,brcomp
              goto           process
.......................................................................................................
.BEGIN BETA 3
invGOODIES
              move           olrn to ninvfld
              call           ninvkey
	              if             not over
	              add            c1 to countI
	              display        *p25:11,countI
	              call      wipecvars
              	move           lrn to nordfld
              	call           nordkey
		move      c1 to ndatpath
              	move      olnum to ndatfld
         		call      ndatkey
         		move      lrn to nshpfld
         		move      no to shipsw
         		move      c1 to nownpath
         		move      olon to nownfld
         		call      nownkey
         		move      c2 to tdmcflag
           		MOVE      NORDFLD to nmrgfld
           		REP       ZFILL IN NMRGFLD
         		move      c0 to nmrgrqty
         		move      c0 to nmrgiqty
         		move      c0 to nmrgnet
         		move      no to mrgsw
           		CALL        NMRGKEY
	           		if      not over
	           		move    yes to mrgsw
	           		endif

         		call      nshpkey
	         		if        not over
	         		move      yes to shipsw
	         		endif

	              CALL      COMPUTE

	              move           olrn to nADJfld
	              call           nADJkey
	                            IF             NOT OVER
		              add            c1 to counta
	              	display        *p25:12,counta
              	              ENDIF

                            endif
INVEXIT
	RETURN
.END BETA 3
wipeavars
	clear	Amlr
	clear   	ABRK
	clear   	ASLSpers
	move	c0,AINVcount
	move	c0,ADaystopay
	move	c0,Aorders
	move	c0,AordersQ
	move	c0,Ainvoices
	move	c0,Acancld
	move	c0,AcancldQ
	move	c0,ALCRs
	move	c0,ALCRsQ
	move	c0,ACAncLcr
	move	c0,ACAncLcrQ
	move	c0,Apending
	move	c0,ApendingQ
	move	c0,AcancPnd
	move	c0,AcancPndQ
	move	c0,ALRINC
	move	c0,ANinInc
              return
debug
              return
.............................................................................
eoj           weof           file2,seqeof
              close          file
              close          file2
              stop

              include         nordio.inc
              include         nmlrio.inc
              include         nbrkio.inc
              include   	  ninvio.inc
.begin beta 2
.begin beta 3
.              include             njstio.inc
              include             nadjio.inc
.end beta 3
              include             nacdio.inc
              include             nownio.inc
              include             ndatio.inc
              include             ndat3io.inc
              include             compute.inc
              include             nmrgio.inc
              include             nshpio.inc
.end beta 2
	      include         comlogic.inc
