..............................................................................
.
. NORD2DD INCLUSION
. NIN ORDER FILE DEFINITION (CREATED FROM THE ORDERPIC PROGRAM)
.
. FILE NAME : ?   (SUPPLIED BY USER)
. REC LENGTH: 498 VAR
. WAS REC LENGTH: 384 VAR       PATCH #6
. WAS REC LENGTH: 344 VAR
. WAS REC LENGTH: 319 VAR
. patch 22 order file conversion mailer et al
. Patch 9.1 DLH 09Jan2007 add OCompID
. Patch 9 DLH 12September2006 add OFulFIll 6 byte Fulfillment company#
. PATCH 8 ASH 19MAR01 NINORD ADDED TO FILE MANAGER
. PATCH 7 ASH 04May99 Added var for Exchange Price
. PATCH 6 ASH 08JAN99 NINORD Y2K, File expansion
. patch 5 DLH 28Sep98 add new ostat & cancodes for Pending orders
..13Aug98 ASH increased record size to account for increase in var size w/ Broker Y2K
..17oct94 DLh proposed change adding net info expand record to 344.
..............................................................................
.
NORDFILE FILE      VAR=498,COMP
.START PATCH 8 REPLACED LOGIC
.NORDNAME INIT      "              "
NORDNAME INIT      "                            "
.END PATCH 8 REPLACED LOGIC
NORDFLAG FORM      1
.
ORD2VARS  List
ORCODE    DIM       1          001-001      ORDER CODE, "S"
OSTAT     DIM       1          002-002      STATUS, 0,B,Q,X,p,x,l,z
.                                      0-Live order
.                                      B-Billed order
.                                      Q-Cancelled/Billed order
.                                      X-Cancelled order
.                                      e-Live Order with Estimated Invoice uses "X" if cancelled    
.patch 5    New values for OSTAT  p Lower case = Pending order (awaiting LO/manager Approval)
.                                 x Lower case = Cancellation of above (never approved)
.                                 l Lower case = LCR
.                                 z Lower case = Cancellation of LCR
..................................................
.WILL NEED TO BE UPDATED WHEN CLIENT FILE IS FINISHED
.begin patch 22
Ofill1    Dim       2         003-004     for company # (mailer conversion DOne)                    ** New              
.end patch 22
OMLRNUM   DIM       4         005-008      MAILER NUMBER, AIM KEY 1
..................................................
OLRN      DIM       6         009-014     LR NUMBER, KEY
OCOBN     DIM       3         015-017     CONTACT # (NIN) OR BROKER # (CMP)
OLNUM     DIM       6         018-023      LIST NUMBER, AIM KEY 2
..................................................
.WILL NEED TO BE UPDATED WHEN CLIENT FILE IS FINISHED
.begin patch 22
Ofill2    Dim       2         024-025     for company # (Owner conversion DOne)           ** New
OLON      DIM       4         026-029     LIST OWNER NUMBER
OwnCtn    Dim       3         030-032     Owner COntact                                   ** New
.end patch 22
..................................................
OMLRPON   DIM       12        033-044     MAILER PURCHASE ORDER NUMBER was 7
OQTY      DIM       9         045-053     QUANTITY,  X,XXX,XXX
OPPM      DIM       5         054-058     PRICE PER THOUSAND, XXX.XX
OMLRKY    DIM       12        059-070     MAILER'S KEY
OFOCODE   DIM       2         071-072     FURNISHED-ON CODE, 0,1,2,3,4,5,6,7,8,OR 9
ORTNDTEC  DIM       2         073-074     RETURN DATE (CENTURY)
ORTNDTEY  DIM       2         075-076     RETURN DATE (YEAR)
ORTNDTEM  DIM       2         077-078     RETURN DATE (MONTH)
ORTNDTED  DIM       2         079-080     RETURN DATE (DAY)
OMDTEC    DIM       2         081-082      MAIL DATE (CENTURY)
OMDTEY    DIM       2         083-084       MAIL DATE (YEAR)
OMDTEM    DIM       2         085-086      MAIL DATE (MONTH)
OMDTED    DIM       2         087-088      MAIL DATE (DAY)
OTOCODE   DIM       1         089-089      TEST ORDER CODE, "1"
OSOTCODE  DIM       1         090-090      SELECTION ON TEST CODE, 1,2 OR 3
OCCODE    DIM       1         091-091      CONTINUATION CODE, "1"
OLRNCO    DIM       6         092-097      LR NUMBER OF CONTINUATION CODE
OODTECOC  DIM       2         098-099      ORDER DATE OF CONTINUATION CODE (CENTURY)
OODTECOY  DIM       2         100-101       ORDER DATE OF CONTINUATION CODE (YEAR)
OODTECOM  DIM       2         102-103      ORDER DATE OF CONTINUATION CODE (MONTH)
OODTECOD  DIM       2         104-105      ORDER DATE OF CONTINUATION CODE (DAY)
OQTYCO    DIM       9         106-114      QUANTITY OF CONTINUATION ORDER, XXX,XXX,XXX
OSPI      DIM       24        115-138      SPECIAL INSTRUCTION CODES, MAX.6 (2 DIGIT)
OBildrct  DIM       1         139-139      Placed as bill direct? y=yes n=no ' '=no
OBRKGUAR  DIM       1         140-140       BROKER GUARANTY, '1' = 30 DAY.
.                                       '2' =45 DAY, '3' =60 DAY, '4' = NO DATE
OELCODE   DIM       1         141-141       ENTIRE LIST CODE
.                                        1=RENT/ENTIRE,2=EXCH,3=EXCHANGE/ENTIRE
OODNUM    DIM       7         142-148       OFFER DeSCRIPTION NUMBER
OODES     DIM       5         149-153       OFFER DESCRIPTION
ONETQTY   DIM       9         154-162       ORDER NET QUANTITY
OCAMP     DIM       6         163-168       ASSOCIATED CAMPAIGN
OCLRSTAT  DIM       1         169-169       CLEARANCE STATUS  1=EXCHANGE, 2=RENT, 3=EXC/SPLIT, 4=DENIED
OCLRINIT  DIM       3         170-172       INITS OF PERSON WHO CLEARED LCR
OBRKRPT   DIM       1         173-173       OUTSIDE BROKER NOTIFIED OF CLEARANCE STATUS?  1=YES, B1=NO
OCLRDTEC  DIM       2         174-175       CLEAR DATE (CENTURY)
OCLRDTEY  DIM       2         176-177       CLEAR DATE (YEAR)
OCLRDTEM  DIM       2         178-179       CLEAR DATE (MONTH)
OCLRDTED  DIM       2         180-181      CLEAR DATE (DAY)
ORENT     DIM       1         182-182       LCR REQUEST FOR RENT "1" = YES
OHIST     DIM       1         183-183       ORDER HISTORY "l"-LCR to Live, "L"-In-House LCR to Live, "p"-Pending to Live
.                                       IN-HOUSE LCR's:  "e"-Waiting for Clearance
.                                                        "E"-Cleared
.                                                        "*"-Faxed to Owner
.                                                        "z"-Denied
OXPPM     DIM       5         184-188       EXCHANGE PRICE PER THOUSAND, XXX.XX
.begin patch 22
oFILL3    dIM       2         189-190     FOR CONVERSION OF COMP-RETURN-TO                ** New
.enD patch 22
ORTNNUM   DIM       4         191-194       RETURN-TO NUMBER
..................................................
OTAPERET  DIM       1         195-195       TAPE-RETURNABLE ?
OUQTY     DIM       9         196-204       UNIVERSE QUANTITY, XXX,XXX,XXX
OSALES10  DIM       1         205-205       TENS DIGIT OF SALESMAN #.
OSALES    DIM       1         206-206       ONES DIGIT OF SALESMAN CODE
OCOCODE   DIM       2         207-208       CONTACT CODE, 1,2,3, OR 4
OCO2CODE  DIM       2         209-210       CONTACT CODE, 1,2,3, OR 4
OODTEC    DIM       2         211-212       ORDER DATE (CENTURY)
OODTEY    DIM       2         213-214       ORDER DATE (YEAR)
OODTEM    DIM       2         215-216       ORDER DATE (MONTH)
OODTED    DIM       2         217-218       ORDER DATE (DAY)
OSCODE    DIM       1         219-219       SAMPLE CODE, 1=sample enclosed,2=sample to follow, OR 3=sample prev approved
OCOMSLCT  DIM       1         220-220       overlay: COMSELECT='C', lifestyle="L"
.                                                     (LSVCS)
OSHP      DIM       2         221-222       SHIPPED VIA CODE, 0,1,2...9.
O1DES     DIM       35        223-257       LINE #1 OF LIST DESCRIPTION, DATACARD
O2DES     DIM       35        258-292       LINE #2 OF LIST DESCRIPTION, KEYIN
OREUSE    DIM       6         293-298       RE-USE LR #, RTN-TO # WILL BE '0'
ODOWJ     DIM       3         299-301       TYPISTS INITIALS was 2
OEXQTY    DIM       9         302-310       EXCHANGE QTY ON SPLIT ORDER.
GUARCODE  DIM       1         311-311       NIN GUARANTY CODE, '1' = 30 DAY.
.                                                 '2' =45 DAY, '3' =60 DAY, '4' = NO DATE
.                                                 '5' =NOT USED, '6'=REC'D PREPAY,
.                                                 '7' REC'D PRE 30-DAY, '8' REC'D PRE 60,
.                                                 '9' =REC'D PRE 60 DAY.
..................................................
.WILL NEED TO BE UPDATED WHEN CLIENT FILE IS FINISHED
.begin patch 22
oFILL4    dIM       2         312-313     .FOR CONVERSION OF BRK/CONSULT
.enD patch 22
OBRKNUM   DIM       4         314-317        BROKER/CONSULTANT NUMBER.
OBRKCNT   DIM       3         318-320        BROKER/CONSULTANT CONTACT NUMBER.
..................................................
osamcde   dim       3         321-323        sample numbers
onetper   dim       2         324-325        net name percentage (NN = Net Name)
onetrc    form      3.2       326-331        net name running charge
onetfm    dim       1         332-332        net flag (F)lat'volume', per (M)'net', (N)o
.                                             deducts allowed by Lo.
.                                        
onetmin   form      7         333-339        net name minimum.
OFullFil  Dim       6         340-345       Fullfilment number
OCompID   Dim       1         346-346       Brokerage Company ID 'N' or ' '= NIN 'P' = Pacific lists
OCompID2  Dim       1         347-347       Management Company ID 'N' or ' '= NIN 'P' = Pacific lists
.begin patch 22
.ofiller            dim       72 337-408        
ofiller   dim       61        348-408     
ORDmNAME DIM       45     409-453   Broker NAME (ELSE blank)
ORDcNAME DIM       45     454-498   MAILER NAME (ALWAYS)
* .......................................................................
          Listend

