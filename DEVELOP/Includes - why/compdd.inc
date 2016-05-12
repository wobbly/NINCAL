.----------------------------------------------------------------
.
.   COMPDD.INC -
.
.   Copyright(c) Names in the News Ca., Inc. 2002
.
.   Author: David L Herrick
.----------------------------------------------------------------
.Release            2.1          Expand so old 4 byte vars can be pushed to 6, total record from 650 to 660
.Release            2.0          Create needed Return-to vars
.Release            1.9          add accounting email address and more room from 500 to 650 bytes
.Release            1.8          Needs to have room for ninRTN 4 byte field or can we reuse the old Fullfill space?
.Release            1.8          2008 September 05          DLH - COMPShpFlag
.Release            1.7          2008 January 30  DLH - COMPConFlag
.   release         1.6                2006 December 13         DLH - Added Exclusive ID field
.   release         1.5                2006 June 20         ASH - Added New AAMFile for Associated Service Bureaus
.   release         1.4                2005 June 18         DMB - Changed IP of File Manager
.   release         1.3                2005 June 10         ASH - Added Field for Owners - Flag if Samples not accepted!
.   release         1.2                2005 April 21        ASH - Added New AAMFile for Associated Consultant/Brokers
.   release         1.1                2005 April 06        ASH - Added Client Activate Date
.   release         1.02               2003 July            Added Seq File Read
.   release         1.01               2003 April
.   Release         1.0                2002June
.   Master Company file.
.   Related files:  CNTDD      =  Master Contact info.
.             CmpNotesdd = Comments, info pertinant to only a particular Company.
.             CMPScfsdd  = Scfs that are always applicable to a regional mailer. - Format as yet not set
.             CMPSubsdd  = cross reference of related companies
.             May need an additional file for discount / deal info.
.             NOTE for transition additional indices on the "old" numbers can be used to facilitate conversion
...............................................
.
            include  nmlrdd.inc
          include   nbrkdd.inc
.>Patch 1.4 Begin
COMPFLIST     Filelist
.COMPFILE      IFILE           keylen=6,fix=500,uncomp,Name="COMPANY.ISI|NINS1:502"
.begin patch 2.1
COMPFILE      IFILE           keylen=6,fix=660,uncomp,Name="COMPANY.ISI|NINS1:502"
COMPFILE2     AFILE           fix=660,uncomp,Name="COMPANY.AAM|NINS1:502"
.old mailernumber
COMPFILE3     IFILE           fix=660,uncomp,Name="COMPANYM.isi|NINS1:502"
.old brknumber
COMPFILE4     IFILE           fix=660,uncomp,Name="COMPANYB.isi|NINS1:502"
.old Ownernumber
COMPFILE5     IFILE           fix=660,uncomp,Name="COMPANYO.isi|NINS1:502"
.old Fulfillment number
COMPFILE6     IFILE           fix=660,uncomp,Name="COMPANYR.isi|NINS1:502"
.START PATCH 1.2 ADDED LOGIC
.Associated Consultants/Brokers
COMPFILE8     AFILE           fix=660,uncomp,Name="COMPANY2.AAM|NINS1:502"
.END PATCH 1.2 ADDED LOGIC
.begin patch 2.0
COMPFILE9     IFILE           fix=660,uncomp,Name="COMPANY9.isi|NINS1:502"
.end patch 2.0
              FileListEnd
.>Patch 1.4 End
COMPFILE7     FILE            fix=660
.end patch 2.1
COMPNAME      INIT            "COMPANY|NINS1:502       "
.Patch1.02
COMPNAM2      INIT            "COMPANY|NINS1:502"
COMPNAM3      INIT            "COMPANY|NINS1:502M"
COMPNAM4      INIT            "COMPANYB|NINS1:502"
COMPNAM5      INIT            "COMPANYO|NINS1:502"
COMPNAM6      INIT            "COMPANYR|NINS1:502"
.START PATCH 1.2 ADDED LOGIC
COMPNAM8      INIT            "COMPANY2|NINS1:502"
.END PATCH 1.2 ADDED LOGIC
.begin patch 2.0
.old rtn -to
COMPNAM9      INIT            "COMPANY9|NINS1:502"
.end patch 2.0
.Patch1.02
COMPPATH      FORM            1
COMPFLAG      FORM            1
.START PATCH 1.5 ADDED LOGIC
COMPFLGSEQ    FORM            1
.END PATCH 1.5 ADDED LOGIC
COMPFLD       DIM             6
COMPFLD2      DIM             58
.Patch1.02
.begin patch 2.1
COMPFLD3      DIM             6
.COMPFLD3      DIM             4
.end patch 2.1
COMPFLD4      DIM             4
COMPFLD5      DIM             4
COMPFLD6      DIM             4
.For Company Flags
COMPFLD7      DIM             4
COMPFLD8      DIM             4
.old rtn-to
COMPFLD9      DIM             4
COMPFLD10     DIM             4
.START PATCH 1.2 ADDED LOGIC
.Associated Consultants/Brokers
COMPFLD11     DIM             9              .COMPCONSULT   255-260
COMPFLD12     DIM             9              .COMPBROKER    282-287
.END PATCH 1.2 ADDED LOGIC
.Start Patch 1.5 Added Logic
COMPFLD13     DIM             4              .COMPSVBFLG    230    ;Service Bureau Flag
.EndPatch 1.5
COMPCONVFLAG  FORM 1                              .2=brk,3=mlr
.
.DH 22Jan09
CompDntCare         Dim       1                . if "T" don't care about mlrflg,brkflg,clrflg
.DH 22Jan09

.Patch1.02
COMPLOCK      FORM            1

COMPVARS      LIST
COMPNUM        DIM            6       1-6     ;COMPANY NUMBER. **KEY**
COMPCOMP      DIM             55      7-61    ;*******Required*******
COMPADDR      DIM             35     62-96
COMPADDR2     DIM             35     97-131
COMPCITY      DIM             30    132-161   ;CITY.
COMPSTATE     DIM             2     162-163   ;STATE..MAY NEED TO INCREASE  FOR USE IN FOREIGN COUNTRY STATES
COMPZIP        DIM            10    164-173   ;ZIP..MAY NEED TO INCREASE FOR USE IN FOREIGN COUNTRY
COMPCNTRY     DIM             15    174-188    .MAY NEED TO INCREASE FOR USE IN FOREIGN COUNTRY
.MANY OF THE NEWER POST SOVIET COUNTRIES ARE IN THE 99* AREA, SO GO TO 4 BYTES
COMPCNTRYCDE  DIM             4     189-192   ;country code for phone/fax  default = 1
.MAY NEED TO INCREASE FOLLOWING 3 FIELDS FOR USE IN FOREIGN COUNTRY
.Do other countries have longer phone number lengths?
COMPPHONE     DIM             10    193-202   ;Phone Number
COMPFAX        DIM            10    203-212   ;Fax Number  **********Required*************
COMPACCTFAX   DIM             10    213-222   ;Accounting fax Number
........................
COMPREGCDE    DIM             1     223-223   ;'T' if regional
COMPBILLCDE   DIM             1     224-224   ;'B' if Batch bill 'A' if batch and adjust
COMPCONTACT   DIM             2     225-226   ;Code for our inhouse account person
.Flags set to true if record moved from old file and has ##
. and for new records as applicable
COMPMLRFLG    DIM             1     227-227   ;T = Mailer flag
COMPBRKFLG    DIM             1     228-228   ;T = Broker
COMPOWNFLG    DIM             1     229-229   ;T = Owner
COMPSVBFLG    DIM             1     230-230   ;T = Service B
COMPCLRFLG    DIM             1     231-231   ;T = Consultant
COMPMNGFLG    DIM             1     232-232   ;T = Manager Flag
.begin patch 2.1
COMPOLDMLR    DIM             6     233-238   ;Old Mailer Number                 ;= key for COMPfile3
CompFiller1   Dim		2     239-240   ;expansion room for old broker	
COMPOLDBRK    DIM             4     241-244   ;Old Broker/consultant #           ;= key for COMPfile4
CompFiller2   Dim		2     245-246   ;expansion room for old OWn	
COMPOLDOWN    DIM             4     247-250   ;Old Owner Number                  ;= key for COMPfile5
CompFiller3   Dim		2     251-252   ;expansion room for old SB
COMPOLDSVB    DIM             4     253-256   ;Old (service b) Number from NinFull ;= key for COMPfile6
.COMPOLDMLR    DIM             4     233-236   ;Old Mailer Number                 ;= key for COMPfile3
.COMPOLDBRK    DIM             4     237-240   ;Old Broker/consultant #           ;= key for COMPfile4
.COMPOLDOWN    DIM             4     241-244   ;Old Owner Number                  ;= key for COMPfile5
.COMPOLDSVB    DIM             4     245-248   ;Old (service b) Number from NinFull ;= key for COMPfile6
.end patch 2.1
COMPMUSAGE    DIM             1     257-257   ; 'F' If we cannot share Mailers list usage info
CompExcl      Dim             1     258-258   .'N' = NIN, 'P' = Pacific Lists -id our mailers by company
COMPBLANK1    DIM             2     259-260
COMPCREDIT    DIM             1     261-261   ;CREDIT STATUS
.              ' '=OK,
.             "*" = ON HOLD.
.             "I" = INACTIVE,
.             "B" = CREDIT RISK.  -      reset nightly if released
.             "N" =   NEW MAILER.
.             "P" = POLITICAL MAILER.  - reset nightly if released
.             "W" = Warning - read note                     ; 21Dec2000
.             "M" = Must Prepay                                            ; 05Mar2002
.             "9" = On hold until over 90s paid             ; 05Mar2002
.             "G" = Guarantees are always required          ; 05Mar2002
.             "g" = Guarantees No longer accepted           ; 06Apr2004
.
COMPBDRCTFLG  DIM             1     262-262   ;'T'  If Mailer and Billed Direct
COMPCONSULT   DIM             6     263-268   ;For Mailer - Code of Consultant used
COMPCONSULT1  DIM             3     269-271   ;For Mailer - Contact Code of Consultant used
COMPMANAGER   DIM             6     272-277   ;For List Owner - Code of Manager used
COMPMANAGER1  DIM             3     278-280   ;For List Owner - Contact Code of Manager used
.Following 2 fields may go bye-bye.
COMPSRVB      DIM             6     281-286   ;For List Owner - Code of Service B used
COMPSRVB1     DIM             3     287-289   ;For List Owner - Contact Code of Service B used
COMPBROKER    DIM             6     290-295   ;For Mailer - Code of Broker used
COMPBROKER1   DIM             3     296-298   ;For Mailer - Contact Code of Broker used
COMPEMAIL     DIM             50    299-348   ;Corporate Email Address (Generic)
COMPPBRKFLG   DIM             1     349-349   ;'1,2,3' = Broker Rank - BLO
COMPACCEPT    DIM             1     350-350   ;'F' = We do not accept their Guars.
COMPUSER      DIM             7     351-357   ;Record Created by
COMPDTE        DIM            8     358-365   ;Date record Added
COMPRUSER     DIM             8     366-373   ;Added/Modified by
COMPRDTE      DIM             8     374-381   ;Date Record Modified
COMPMAIN      DIM             6     382-387   ;if this is a satelite or subsidiary office it holds id # of main (parent) corp record
COMPINACTIVE  DIM             1     388-388   ;'T' if this company is inactive -see notes for reason
COMPBLANK2    DIM             20    389-408   ;available space
COMPFTP        DIM            50    409-458   ;Companies FTP Site
COMPACCTM     DIM             1     459-459   ;Acct Method "C"ash "A"ccrual
COMPACCTD     DIM             4     460-463   ;start date of accounting year MMDD  ie 0101 or 0701
COMPDISCFLG   DIM             1     464-464   ;"T" if discount associated with this company
COMPINVSGO    DIM             6     465-470   ;for those Mailers who's Invoices's go to a diff place
COMPINVSGO1   DIM             3     471-473   ;for those Mailers who's Invoices's go to a diff place
COMPFAXFLAG1  DIM             1     474-474   ;T = fax order confirmations to mailer
COMPFAXFLAG2  DIM             1     475-475   ;T = fax order confirmations to Broker/Consultant
COMPEXCHANGE  DIM             1     476-476   ;T = Exchanges Allowed
COMPGALLEY    DIM             1     477-477   ;T = List Owner prefers Galley Listing
COMPTAXID     DIM             15    478-492   ;TEFRA TAX ID CODE - pulled from Owner file
.begin patch 1.01
CompTransmitFlag Dim          1     493-493   ;-Send Fulfilment info via: ' '=fax or print based on  presence of fax#
.                                             ; 1=FTP, 2= Email.......
.begin patch 1.02                ;Does Vendor Receive Statement
COMPSTATEMENT DIM 1
.patch1.02
.START PATCH 1.1 REPLACED LOGIC
.COMPFILLER   DIM             14    486-500   ;not used
COMPCNTDATE   DIM             8     494-501   ;Client Contract Date - Used for revenue purposes
.START PATCH 1.3 REPLACED LOGIC
.COMPFILLER   DIM             6     494-500   ;not used
COMPOSamp     dim             1     502-502   ;for Owners - Flag to indicate No Samples Allowed
COMPODRCTFLG  DIM             1     503-503   ;'T'  If Mailer and Orders go Direct
COMPBRKRPT    DIM             1     504-504   ;
COMPConFlag   DIM             1     505-505   ;'T'  If Mailer wants individual Orders confirmations (mlr copy of order)      30Jan08
.begin patch 1.8
CompShpFlag   Dim             1     506-506   . 'T' If we want to suppress auto shipping info requests (SB's only) 
.begin patch 1.9
.COMPFILLER    DIM             1     499-500   ;not used      30Jan08
.begin patch 2.0
COMPRTNFLG    DIM             1     507-507   ;T = Return-to Flag
CompFiller4   Dim		2     508-509    :old (Return-to) filler 
COMPOLDRTN    DIM             4     510-513   ;Old (Return-to) Number from Ninrtn ;= key for COMPfile9
CompActEmailAR        Dim    50     514-563      .Accounting Email
COMPFILLER    DIM             46    564-609   ;not used      
.COMPFILLER    DIM             96   504-600   ;not used      

.COMPFILLER    DIM             101     499-600   ;not used      Jul 11
.end patch 2.0
CompActEmail        Dim       50    610-659      .Accounting Email
.end patch 2.1  mmm position counting is off somewhere
.end patch 1.9

.COMPFILLER    DIM             2     498-500   ;not used      30Jan08
.end patch 1.8
.COMPFILLER    DIM             3     497-500   ;not used
.COMPFILLER   DIM             5     495-500   ;not used
.END PATCH 1.3 REPLACED LOGIC
.END PATCH 1.1 REPLACED LOGIC
.COMPFILLER   DIM             16    485-500   ;not used
.end patch 1.01
               LISTEND