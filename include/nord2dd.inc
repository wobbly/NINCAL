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
. Patch 9.2 DLH 03Mar2010 add lock
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
NORDNAME  Dim       65
.END PATCH 8 REPLACED LOGIC
NORDFLAG FORM      1
NORDLOCK FORM      1
.
ORCODE   DIM       1        1-1      ORDER CODE, "S"
OSTAT    DIM       1      002-002      STATUS, 0,B,Q,X,p,x
.                                      0-Live order
.                                      B-Billed order
.                                      Q-Cancelled/Billed order
.                                      X-Cancelled order
.patch 5    New values for OSTAT  p Lower case = Pending order (awaiting LO/manager Approval)
.                                 x Lower case = Cancellation of above (never approved)
.                                 l Lower case = LCR
.                                 z Lower case = Cancellation of LCR
.                                 t Lower case = tentative approval of LCR
OMLRNUM  DIM       4        3-6      MAILER NUMBER, AIM KEY 1
OLRN     DIM       6        7-12     LR NUMBER, KEY
OCOBN    DIM       3       13-15     CONTACT # (NIN) OR BROKER # (CMP)
OLNUM    DIM       6       16-21      LIST NUMBER, AIM KEY 2
OLON     DIM       4       22-25     LIST OWNER NUMBER
OMLRPON  DIM       12      26-37     MAILER PURCHASE ORDER NUMBER
.Start Patch #6 - increased var
.OQTY     DIM       7       38-44     QUANTITY,  X,XXX,XXX
OQTY     DIM       9       38-46     QUANTITY,  X,XXX,XXX
.End Patch #6 - increased var
OPPM     DIM       5       47-51     PRICE PER THOUSAND, XXX.XX
OMLRKY   DIM       12      52-63     MAILER'S KEY
.Start Patch #6 - increased var
.OFOCODE  DIM       1       62-62     FURNISHED-ON CODE, 0,1,2,3,4,5,6,7,8,OR 9
.ORTNDTEM DIM       2       63-64     RETURN DATE (MONTH)
.ORTNDTED DIM       2       65-66     RETURN DATE (DAY)
.ORTNDTEY DIM       2       67-68     RETURN DATE (YEAR)
.OMDTEM   DIM       2       69-70      MAIL DATE (MONTH)
.OMDTED   DIM       2       71-72      MAIL DATE (DAY)
.OMDTEY   DIM       2       73-74       MAIL DATE (YEAR)
OFOCODE  DIM       2       64-65     FURNISHED-ON CODE, 0,1,2,3,4,5,6,7,8,OR 9
ORTNDTEC DIM       2       66-67
ORTNDTEY DIM       2       68-69     RETURN DATE (YEAR)
ORTNDTEM DIM       2       70-71     RETURN DATE (MONTH)
ORTNDTED DIM       2       72-73     RETURN DATE (DAY)
OMDTEC   DIM       2       74-75
OMDTEY   DIM       2       76-77       MAIL DATE (YEAR)
OMDTEM   DIM       2       78-79      MAIL DATE (MONTH)
OMDTED   DIM       2       80-81      MAIL DATE (DAY)
.End Patch #6 - increased var
OTOCODE  DIM       1       82-82      TEST ORDER CODE, "1"
OSOTCODE DIM       1       83-83      SELECTION ON TEST CODE, 1,2 OR 3
OCCODE   DIM       1       84-84      CONTINUATION CODE, "1"
OLRNCO   DIM       6       85-90      LR NUMBER OF CONTINUATION CODE
.Start Patch #6 - increased var
.OODTECOM DIM       2       84-85      ORDER DATE OF CONTINUATION CODE (MONTH)
.OODTECOD DIM       2       86-87      ORDER DATE OF CONTINUATION CODE (DAY)
.OODTECOY DIM       2       88-89       ORDER DATE OF CONTINUATION CODE (YEAR)
.OQTYCO   DIM       6       90-95      QUANTITY OF CONTINUATION ORDER, XXX,XXX
.OSPI     DIM       12     108-119    SPECIAL INSTRUCTION CODES, MAX.6 (2 DIGIT)
OODTECOC DIM       2       91-92
OODTECOY DIM       2       93-94       ORDER DATE OF CONTINUATION CODE (YEAR)
OODTECOM DIM       2       95-96      ORDER DATE OF CONTINUATION CODE (MONTH)
OODTECOD DIM       2       97-98      ORDER DATE OF CONTINUATION CODE (DAY)
OQTYCO   DIM       9       99-107     QUANTITY OF CONTINUATION ORDER, XXX,XXX
OSPI     DIM       24     108-131    SPECIAL INSTRUCTION CODES, MAX.6 (2 DIGIT)
.END Patch #6 - increased var
OBLANK   DIM       2      132-133       NOT USED 103-103 USED FOR BROKER FLAG '
.BYTE 103 RESERVED FOR BROKER FLAG '*'.
.OBRKCODE DIM       1      110-110     "*" = BROKER ORDER
.OBLANK   DIM       !      104-104       NOT USED
OELCODE  DIM       1      134-134       ENTIRE LIST CODE
.                                       1=RENT/ENTIRE,2=EXCH,3=EXCHANGE/ENTIRE
OODNUM   DIM       7      135-141       OFFER DISCRIPTION NUMBER
.START PATCH #7 - REPLACED LOGIC WITH ADDED VAR
.OODES    DIM       40     142-181       OFFER DESCRIPTION
OODES    DIM       34     142-175       OFFER DESCRIPTION
OHIST    DIM       1      176-176       ORDER HISTORY 1=LIVE, 2=LCR TO LIVE, 3=PENDING TO LIVE
OXPPM    DIM       5      177-181        EXCHANGE PRICE PER THOUSAND, XXX.XX
.END PATCH #7 - REPLACED LOGIC WITH ADDED VAR
ORTNNUM  DIM       4      182-185       RETURN-TO NUMBER
OTAPERET DIM       1      186-186       TAPE-RETURNABLE ?
.Start Patch #6 - increased var
.OUQTY    DIM       7      163-169       UNIVERSE QUANTITY, X,XXX,XXX
OUQTY    DIM       9      187-195       UNIVERSE QUANTITY, X,XXX,XXX
.End Patch #6 - increased var
OSALES10 DIM       1      196-196       TENS DIGIT OF SALESMAN #.
OSALES   DIM       1      197-197       ONES DIGIT OF SALESMAN #.
.Start Patch #6 - increased var
.OCOCODE  DIM       1      171-171       CONTACT CODE, 1,2,3, OR 4
.OODTEM   DIM       2      172-173       ORDER DATE (MONTH)
.OODTED   DIM       2      174-175       ORDER DATE (DAY)
.OODTEY   DIM       2      176-177       ORDER DATE (YEAR)
OCOCODE  DIM       2      198-199       CONTACT CODE, 1,2,3, OR 4
OCO2CODE DIM       2      200-201       CONTACT CODE, 1,2,3, OR 4
OODTEC   DIM       2      202-203
OODTEY   DIM       2      204-205       ORDER DATE (YEAR)
OODTEM   DIM       2      206-207       ORDER DATE (MONTH)
OODTED   DIM       2      208-209       ORDER DATE (DAY)
.End Patch #6 - increased var
OSCODE   DIM       1      210-210       SAMPLE CODE, 1,2, OR 3
OCOMSLCT DIM       1      211-211       COMSELECT CODE, C
.Start Patch #6 - increased var
.OSHP     DIM       1      180-180       SHIPPED VIA CODE, 0,1,2...9.
OSHP     DIM       2      212-213       SHIPPED VIA CODE, 0,1,2...9.
.End Patch #6 - increased var
O1DES    DIM       35     214-248       LINE #1 OF LIST DESCRIPTION, DATACARD
O2DES    DIM       35     249-283       LINE #2 OF LIST DESCRIPTION, KEYIN
OREUSE   DIM       6      284-289       RE-USE LR #, RTN-TO # WILL BE '0'
ODOWJ    DIM       3      290-202       TYPISTS INITIALS
.Start Patch #6 - increased var
.OEXQTY   DIM       7      261-267       EXCHANGE QTY ON SPLIT ORDER.
OEXQTY   DIM       9      293-301       EXCHANGE QTY ON SPLIT ORDER.
.END Patch #6 - increased var
GUARCODE DIM       1      302-302       GUARANTY CODE, '1' = 30 DAY.
.                                       '2' =45 DAY, '3' =60 DAY, '4' = NO DATE
OBRKNUM  DIM       4      303-306        BROKER/CONSULTANT NUMBER.
OBRKCNT  DIM       3      307-309        BROKER/CONSULTANT CONTACT NUMBER.
OSAMCDE  DIM       3      310-312
onetper  dim       2      313-314        net name percentage
onetrc   form      3.2    315-320        net name running charge
onetfm   dim       1      321-321        net flag flat/per m.
onetmin  form      7      322-328        net name minimum.
.begin patch 8
OFullFil  Dim       6  329-334
.begin patch 9.1
OCOmpID   Dim       1  335-335
OCOmpID2  Dim       1  336-336
.begin patch 21.4
OComPrc    form       3.2        337-342        overlay price IE Comselect
OComMod    Dim        1          343-343        Overlay price Modifier  ' ',F, or M     
OMLRLstCd  dim        12         344-355
Ofiller    dim        53         356-408
.Ofiller    dim        65         344-408
.ofiller   dim       72 337-408        
.ofiller            dim       73 336-408        
.ofiller            dim       74 335-408        
.End Patch 9.1
.ofiller  dim       80     329-408        
.end patch 8
.Fields increased w/ Broker & Mailer Y2K
.ORDmNAME DIM       25     306-330   Broker NAME (ELSE blank)
.ORDcNAME DIM       25     331-355   MAILER NAME (ALWAYS)
ORDmNAME DIM       45     409-453   Broker NAME (ELSE blank)
ORDcNAME DIM       45     454-498   MAILER NAME (ALWAYS)
* .......................................................................
ORD2VARS  VARLIST   ORCODE:
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
                        OBLANK:  
                        OELCODE: 
                        OODNUM:                       
                        OODES:
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
                    OCompID:
                    OCompID2:
.begin patch 21.4
                      OComPrc:
                      OComMod:
.end patch 21.4
                        OMLRLstCd:
                        ofiller:
                        ORDMNAME:    295-319   broker NAME (IF BROKER ELSE BLANK) 295-339
                        ORDCNAME     320-324   MAILER NAME (always) 340-364
..............................................................................

