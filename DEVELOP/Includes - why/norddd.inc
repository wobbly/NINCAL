..............................................................................
.
. NORDDD INCLUSION
. NIN ORDER FILE DEFINITION
.
. FILE NAME : NINORD
. REC LENGTH: 408 FIXED         .WAS 294
. INDEX KEY : (1)  7-12       (LR#)
.             (2)303-306,7-12 broker/consultant#+lr#  
.AIMDEX KEYS: (1)  3-6        (MAILER#)
.             (2)  16-21      (LIST#)
.             (3)  26-37      (PO#)
.             (4) 303-306     (broker/consultant#)  
.START PATCH 19 ADDED LOGIC
.             (5)  22-25      (LIST OWNER)
.             (6)  52-63      (MAILER KEY)
.             (7) 198-199     (CONTACT)
.END PATCH 19 ADDED LOGIC
.............................................................................
.######################################  NOTE ordprint uses these vars and is still hardwired in nordtest.pls
.######################################
.######################################
.######################################
.######################################



.#####DLH added nordfld5 aim key by lr for ninprint - AH stuff allow delete by lr
.misc notes regarding future record format changes:
.add a second contact (nin) field (shipping contact) not a required field
.allow for larger client nane  25-> 45
.expand shipping code from 1 to 2 bytes  & add in house transfer (change to file?)
.Y2  in ccyymmdd format for all dates
.allow for Pending order status and be able to track NEVER Approved.
.allow for second contact shipping Info Contact (not required)
.expand media to 2 bytes
.expand quantity fields to 9 bytes
.review other fields for expansion
........................................................................
.
. Patch 21.2 DLH 20130514 New Ostat 't' tentative approval
. Patch 21.1 DLH 09Jan2006 Company code  "P" for Pacific Lists
. Patch 21 DLH 12September2006 add OFulFIll 6 byte Fulfillment company#
. PATCH 20 DMBASH 18JUN2005 Change IP Address of File Manager
. PATCH 19 ASH 02MAY2003 ADDED NEW AAMDICES FOR NEW SHIPPING PROGRAM
. PATCH 18 ASH 19MAR01 MOVED NINORD TO FILE MANAGER
. PATCH 17 ASH 15SEP00 Added Net Quantity
. PATCH 16 ASH 11MAY00 Added new key value - associated Campaign
. PATCH 15 ASH 19APR00 Added new var - associated Campaign
. PATCH 13 ASH 29Nov99 Added new var to determine if Rental
. PATCH 12 ASH ??????? Added new var
. PATCH 11 ASH 04May99 Added Exchange Price var
. patch 10 ASH 23Dec98 file conversion with Y2K expansion
. patch 5 DLH 28Sep98 add new ostat & cancodes for Pending orders
.30Mar98 DLH add code for Multiple locking modes
.23may95 DLH change unused obrkcode to obildrct - to track direct billing
.17oct94 DLh proposed change adding net info expand record to 294.
.15jul93 dlh add sample code, expand mlr PO from 7 to 12, put sales numbers.
. together were   165 & 246   now 170-171 expand typist inits to 3.
.new record size = 278
.23APR93 DLH PREP WORK ADD BROKER/CONSULTANT FIELDS, RECORD SIZE WILL BE 269.
.21OCT92 DLH ELIMINATE OBLANK ADDED OBRKCODE, OBRKGUAR.
..............................................................................
.
NORDNAME DIM       65
.START PATCH 18 REPLACED LOGIC
.NORDNME1 INIT      "NINORD"
.NORDNME2 INIT      "NINORD"
.nordnme3 init      "NINORDB"
..START PATCH 16 ADDED LOGIC
.nordnme4 init      "NINORDC"
..END PATCH 16 ADDED LOGIC
.>START PATCH 20 CHANGED IP ADDRESS
.NORDNME1 INIT      "NINORD.ISI|20.20.30.103:502"
NORDNME1 INIT      "NINORD.ISI|NINS1:502"
.NORDNME2 INIT      "NINORD.AAM|20.20.30.103:502"
NORDNME2 INIT      "NINORD.AAM|NINS1:502"
nordnme3 init      "NINORDB"
.nordnme4 init      "NINORDC.ISI|20.20.30.103:502"
nordnme4 init      "NINORDC.ISI|NINS1:502"
.>END PATCH 20 CHANGED IP ADDRESS
.END PATCH 18 REPLACED LOGIC
NORDFILE IFILE     KEYLEN=6,FIXED=408,NODUPLICATES
         IFNZ      PC
NORDFLE2 AFILE     FIXED=408
         XIF
         IFZ       PC
.NORDFLE2 AFILE     17,4,,FIXED=294    .pcbus
NORDFLE2 AFILE     FIXED=408           .plb
         XIF
.NORDFLE3 IFILE     KEYLEN=10,FIXED=294
.START PATCH 16 ADDED LOGIC
NORDFLE4 IFILE     KEYLEN=6,FIXED=408
.END PATCH 16 ADDED LOGIC
NORDFLD  DIM       6
NORDFLD1 DIM       7      nordfle2 AIM KEY 1 MLR
NORDFLD2 DIM       9      nordfle2 AIM KEY 2 LIST
.NORDFLD3 DIM       11    nordfle2 AIM KEY 3 MLRPO
NORDFLD3 DIM       15     nordfle2 AIM KEY 3 MLRPO
nordfld4 dim       7      nordfle2 aim key 4 broker## 
nordfld5 DIM       9      .order print file secondary aim key lr
.START PATCH 19 ADDED LOGIC
NORDFLD6 DIM       7      nordfle2 AIM KEY 5 LIST OWNER
NORDFLD7 DIM       15     nordfle2 AIM KEY 6 MAILER KEY
NORDFLD8 DIM       5      nordfle2 AIM KEY 7 CONTACT
.test DLH june 09
NORDFLD9 DIM       5      nordfle2 AIM KEY 7 sales
.test DLH june 09
.END PATCH 19 ADDED LOGIC
.START PATCH 16 ADDED LOGIC
nordfldC DIM       6      .NORDFLE4 ISAM KEY BASED ON CAMPAIGN NUMBER
.END PATCH 16 ADDED LOGIC
NORDFLAG FORM      1
NORDFLG2 FORM      1
.NORDFLG3 FORM      1
.START PATCH 16 ADDED LOGIC
NORDFLG4 FORM      1
.END PATCH 16 ADDED LOGIC
NORDPATH FORM      1
NORDLOCK FORM      1         0 & 1=FILEPI, 2=REcord lock, 3=no lock
.
ORCODE   DIM       1      001-001      ORDER CODE, "S"
OSTAT    DIM       1      002-002      STATUS, 0,B,Q,X,p,x,l,z
.                                      0-Live order
.                                      B-Billed order
.                                      Q-Cancelled/Billed order
.                                      X-Cancelled order
.                                      e-Live Order with Estimated Invoice uses "X" if cancelled    
.patch 5    New values for OSTAT  p Lower case = Pending order (awaiting LO/manager Approval)
.                                 x Lower case = Cancellation of above (never approved)
.                                 l Lower case = LCR
.                                 z Lower case = Cancellation of LCR
.                                 t Lower case = tentative approval of LCR
..................................................
.WILL NEED TO BE UPDATED WHEN CLIENT FILE IS FINISHED
OMLRNUM  DIM       4      003-006      MAILER NUMBER, AIM KEY 1
..................................................
OLRN     DIM       6      007-012     LR NUMBER, KEY
OCOBN    DIM       3      013-015     CONTACT # (NIN) OR BROKER # (CMP)
OLNUM    DIM       6      016-021      LIST NUMBER, AIM KEY 2
..................................................
.WILL NEED TO BE UPDATED WHEN CLIENT FILE IS FINISHED
OLON     DIM       4      022-025     LIST OWNER NUMBER
..................................................
OMLRPON  DIM       12     026-037     MAILER PURCHASE ORDER NUMBER was 7
.Start Patch 10 - expand var
.OQTY     DIM       7      038-044     QUANTITY,  X,XXX,XXX
OQTY     DIM       9      038-046     QUANTITY,  X,XXX,XXX
.End Patch 10 - expand var
OPPM     DIM       5      047-051     PRICE PER THOUSAND, XXX.XX
OMLRKY   DIM       12     052-063     MAILER'S KEY
.Start Patch 10 - expand var
.OFOCODE  DIM       1      062-062     FURNISHED-ON CODE, 0,1,2,3,4,5,6,7,8,OR 9
OFOCODE  DIM       2      064-065     FURNISHED-ON CODE, 0,1,2,3,4,5,6,7,8,OR 9
.end Patch 10 - expand var
.Start Patch 10 - add var and rearrange other date fields
.ORTNDTEM DIM       2      063-064     RETURN DATE (MONTH)
.ORTNDTED DIM       2      065-066     RETURN DATE (DAY)
.ORTNDTEY DIM       2      067-068     RETURN DATE (YEAR)
.OMDTEM   DIM       2      069-070      MAIL DATE (MONTH)
.OMDTED   DIM       2      071-072      MAIL DATE (DAY)
.OMDTEY   DIM       2      073-074       MAIL DATE (YEAR)
ORTNDTEC DIM       2      066-067     RETURN DATE (CENTURY)
ORTNDTEY DIM       2      068-069     RETURN DATE (YEAR)
ORTNDTEM DIM       2      070-071     RETURN DATE (MONTH)
ORTNDTED DIM       2      072-073     RETURN DATE (DAY)
OMDTEC   DIM       2      074-075      MAIL DATE (CENTURY)
OMDTEY   DIM       2      076-077       MAIL DATE (YEAR)
OMDTEM   DIM       2      078-079      MAIL DATE (MONTH)
OMDTED   DIM       2      080-081      MAIL DATE (DAY)
.End Patch 10 - add var and rearrange other date fields
OTOCODE  DIM       1      082-082      TEST ORDER CODE, "1"
OSOTCODE DIM       1      083-083      SELECTION ON TEST CODE, 1,2 OR 3
OCCODE   DIM       1      084-084      CONTINUATION CODE, "1"
OLRNCO   DIM       6      085-090      LR NUMBER OF CONTINUATION CODE
.Start Patch 10 - add var and rearrange other date fields
.OODTECOM DIM       2      084-085      ORDER DATE OF CONTINUATION CODE (MONTH)
.OODTECOD DIM       2      086-087      ORDER DATE OF CONTINUATION CODE (DAY)
.OODTECOY DIM       2      088-089       ORDER DATE OF CONTINUATION CODE (YEAR)
OODTECOC DIM       2      091-092      ORDER DATE OF CONTINUATION CODE (CENTURY)
OODTECOY DIM       2      093-094       ORDER DATE OF CONTINUATION CODE (YEAR)
OODTECOM DIM       2      095-096      ORDER DATE OF CONTINUATION CODE (MONTH)
OODTECOD DIM       2      097-098      ORDER DATE OF CONTINUATION CODE (DAY)
.End Patch 10 - add var and rearrange other date fields
.Start Patch 10 - expand var
.OQTYCO   DIM       6      090-095      QUANTITY OF CONTINUATION ORDER, XXX,XXX
.OSPI     DIM       12     96-107      SPECIAL INSTRUCTION CODES, MAX.6 (2 DIGIT)
OQTYCO   DIM       9      099-107      QUANTITY OF CONTINUATION ORDER, XXX,XXX,XXX
OSPI     DIM       24     108-131      SPECIAL INSTRUCTION CODES, MAX.6 (2 DIGIT)
.End Patch 10 - expand var
OBildrct DIM       1      132-132      Placed as bill direct? y=yes n=no ' '=no
OBRKGUAR DIM       1      133-133       BROKER GUARANTY, '1' = 30 DAY.
.                                       '2' =45 DAY, '3' =60 DAY, '4' = NO DATE
OELCODE  DIM       1      134-134       ENTIRE LIST CODE
.                                       1=RENT/ENTIRE,2=EXCH,3=EXCHANGE/ENTIRE
OODNUM   DIM       7      135-141       OFFER DeSCRIPTION NUMBER
.START PATCH #11 - REPLACED LOGIC WITH ADDED VAR
.OODES    DIM       40     142-181       OFFER DESCRIPTION
.START PATCH 15 NEW VAR
.OODES    DIM       20     142-161       OFFER DESCRIPTION
.START PATCH 17 NEW VAR
.OODES    DIM       14     142-155       OFFER DESCRIPTION
OODES    DIM       5      142-146       OFFER DESCRIPTION
ONETQTY  DIM       9      147-155       ORDER NET QUANTITY
.END PATCH 17 NEW VAR
OCAMP    DIM       6      156-161       ASSOCIATED CAMPAIGN
.END PATCH 15 NEW VAR
.Start Patch #14 - added var
OCLRSTAT DIM       1      162-162       CLEARANCE STATUS  1=EXCHANGE, 2=RENT, 3=EXC/SPLIT, 4=DENIED
OCLRINIT DIM       3      163-165       INITS OF PERSON WHO CLEARED LCR
OBRKRPT  DIM       1      166-166       OUTSIDE BROKER NOTIFIED OF CLEARANCE STATUS?  1=YES, B1=NO
OCLRDTEC DIM       2      167-168       CLEAR DATE (CENTURY)
OCLRDTEY DIM       2      169-170       CLEAR DATE (YEAR)
OCLRDTEM DIM       2      171-172       CLEAR DATE (MONTH)
OCLRDTED DIM       2      173-174       CLEAR DATE (DAY)
.End Patch #14 - added var
.Start Patch #13 - added var
ORENT    DIM       1      175-175       LCR REQUEST FOR RENT "1" = YES
.End Patch #13 - added var
.Start Patch #12 - added var
OHIST    DIM       1      176-176       ORDER HISTORY "l"-LCR to Live, "L"-In-House LCR to Live, "p"-Pending to Live
.                                       IN-HOUSE LCR's:  "e"-Waiting for Clearance
.                                                        "E"-Cleared
.                                                        "*"-Faxed to Owner
.                                                        "z"-Denied
.							 "t"-Tentatively Approved
.End Patch #12 - added var
OXPPM    DIM       5      177-181       EXCHANGE PRICE PER THOUSAND, XXX.XX
.END PATCH #11 - REPLACED LOGIC WITH ADDED VAR
..................................................
.WILL NEED TO BE UPDATED WHEN CLIENT FILE IS FINISHED
ORTNNUM  DIM       4      182-185       RETURN-TO NUMBER
..................................................
OTAPERET DIM       1      186-186       TAPE-RETURNABLE ?
.Start Patch 10 - expand var
.OUQTY    DIM       7      163-169       UNIVERSE QUANTITY, X,XXX,XXX
OUQTY    DIM       9      187-195       UNIVERSE QUANTITY, XXX,XXX,XXX
.End Patch 10 - expand var
OSALES10 DIM       1      196-196       TENS DIGIT OF SALESMAN #.
OSALES   DIM       1      197-197       ONES DIGIT OF SALESMAN CODE
.Start Patch 10 - expand var and add var
.OCOCODE  DIM       1      172-172       CONTACT CODE, 1,2,3, OR 4
OCOCODE  DIM       2      198-199       CONTACT CODE, 1,2,3, OR 4
OCO2CODE DIM       2      200-201       CONTACT CODE, 1,2,3, OR 4
.End Patch 10 - expand var and add var
.Start Patch 10 - add var and rearrange other date fields
.OODTEM   DIM       2      173-174       ORDER DATE (MONTH)
.OODTED   DIM       2      175-176       ORDER DATE (DAY)
.OODTEY   DIM       2      177-178       ORDER DATE (YEAR)
OODTEC   DIM       2      202-203       ORDER DATE (CENTURY)
OODTEY   DIM       2      204-205       ORDER DATE (YEAR)
OODTEM   DIM       2      206-207       ORDER DATE (MONTH)
OODTED   DIM       2      208-209       ORDER DATE (DAY)
.End Patch 10    - add var and rearrange other date fields
OSCODE   DIM       1      210-210       SAMPLE CODE, 1=sample enclosed,2=sample to follow, OR 3=sample prev approved
OCOMSLCT DIM       1      211-211       overlay: COMSELECT='C', lifestyle="L"
.                                       (LSVCS)
.Start Patch 10 - expand var
.OSHP     DIM       1      181-181       SHIPPED VIA CODE, 0,1,2...9.
OSHP     DIM       2      212-213       SHIPPED VIA CODE, 0,1,2...9.
.End Patch 10 - expand var
O1DES    DIM       35     214-248       LINE #1 OF LIST DESCRIPTION, DATACARD
O2DES    DIM       35     249-283       LINE #2 OF LIST DESCRIPTION, KEYIN
OREUSE   DIM       6      284-289       RE-USE LR #, RTN-TO # WILL BE '0'
ODOWJ    DIM       3      290-292       TYPISTS INITIALS was 2
.Start Patch 10 - expand var
.OEXQTY   DIM       7      261-267       EXCHANGE QTY ON SPLIT ORDER.
OEXQTY   DIM       9      293-301       EXCHANGE QTY ON SPLIT ORDER.
.End Patch 10 - expand var
GUARCODE DIM       1      302-302       NIN GUARANTY CODE, '1' = 30 DAY.
.                                       '2' =45 DAY, '3' =60 DAY, '4' = NO DATE
.                                       '5' =NOT USED, '6'=REC'D PREPAY,
.                                       '7' REC'D PRE 30-DAY, '8' REC'D PRE 60,
.                                       '9' =REC'D PRE 60 DAY.
..................................................
.WILL NEED TO BE UPDATED WHEN CLIENT FILE IS FINISHED
OBRKNUM  DIM       4      303-306        BROKER/CONSULTANT NUMBER.
OBRKCNT  DIM       3      307-309        BROKER/CONSULTANT CONTACT NUMBER.
..................................................
osamcde  dim       3      310-312        sample numbers
onetper  dim       2      313-314        net name percentage (NN = Net Name)
onetrc   form      3.2    315-320        net name running charge
onetfm   dim       1      321-321        net flag (F)lat'volume', per (M)'net', (N)o
.                                         deducts allowed by Lo.
.                                        
onetmin  form      7      322-328        net name minimum.
.begin patch 20
OFullFil  Dim       6  329-334       Fullfilment number
.begin patch 21.1
OCompID   Dim       1  335-335       Brokerage Company ID 'N' or ' '= NIN 'P' = Pacific lists
OCompID2  Dim       1  336-336       Management Company ID 'N' or ' '= NIN 'P' = Pacific lists
ofiller   dim       72 337-408        
.ofiller            dim       73 336-408        
.ofiller            dim       74 335-408        
.end patch 21.1
.ofiller  dim       80     329-408        
.end patch 20
* .......................................................................
ORDVARS  VARLIST   ORCODE:
                        OSTAT:
                        OMLRNUM:
                        OLRN:   
                        OCOBN:  
                        OLNUM:  
                        OLON:   
                        OMLRPON:
                        OQTY:   
                        OPPM:   
                        OMLRKY: 
                        OFOCODE:
                        ORTNDTEC:
                        ORTNDTEY:
                        ORTNDTEM:
                        ORTNDTED:
                        OMDTEC:
                        OMDTEY:
                        OMDTEM:  
                        OMDTED:    
                        OTOCODE: 
                        OSOTCODE:
                        OCCODE:  
                        OLRNCO:  
                        OODTECOC:
                        OODTECOY:
                        OODTECOM:
                        OODTECOD:
                        OQTYCO:  
                        OSPI:    
                        OBildrct:
                        OBRKGUAR:
                        OELCODE: 
                        OODNUM:  
                        OODES:
                        ONETQTY:
                        OCAMP:
                        OCLRSTAT:
                        OCLRINIT:
                        OBRKRPT:
                        OCLRDTEC:
                        OCLRDTEY:
                        OCLRDTEM:
                        OCLRDTED:
                        ORENT:
                        OHIST:
                        OXPPM:   
                        ORTNNUM: 
                        OTAPERET:
                        OUQTY:   
                        OSALES10:
                        osales:
                        OCOCODE:
                        OCO2CODE: 
                        OODTEC:
                        OODTEY:
                        OODTEM:  
                        OODTED:    
                        OSCODE:  
                        OCOMSLCT:
                        OSHP:    
                        O1DES:   
                        O2DES:   
                        OREUSE:  
                        ODOWJ:   
                        OEXQTY:  
                        GUARCODE:
                        OBRKNUM:
                        OBRKCNT:
                        OSAMCDE:
                        onetper:
                        onetrc:
                        onetfm:
                        onetmin:
                    OFullFil:
.begin patch 21.1
                    OCompID:
                    OCompID2:
.end patch 21.1
                    ofiller

