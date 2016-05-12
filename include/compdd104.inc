.----------------------------------------------------------------
.
.   COMPDD.INC -
.
.   Copyright(c) Names in the News Ca., Inc. 2002
.
.   Author: David L Herrick
.----------------------------------------------------------------
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
.                   CMPScfsdd  = Scfs that are always applicable to a regional mailer. - Format as yet not set
.             CMPSubsdd  = cross reference of related companies
.             May need an additional file for discount / deal info.
.             NOTE for transition additional indices on the "old" numbers can be used to facilitate conversion
...............................................
.
            include         nmlrdd.inc
	include	nbrkdd.inc
.>Patch 1.4 Begin
COMPFLIST     Filelist
.COMPFILE     IFILE           keylen=6,fix=500,uncomp,Name="COMPANY.ISI|20.20.30.104:502"
COMPFILE      IFILE           keylen=6,fix=500,uncomp,Name="COMPANY.ISI|10.10.30.104:502"
.COMPFILE2    AFILE           fix=500,uncomp,Name="COMPANY.AAM|20.20.30.104:502"
COMPFILE2     AFILE           fix=500,uncomp,Name="COMPANY.AAM|10.10.30.104:502"
.old mailernumber
.COMPFILE3    IFILE           fix=500,uncomp,Name="COMPANYM.isi|20.20.30.104:502"
COMPFILE3     IFILE           fix=500,uncomp,Name="COMPANYM.isi|10.10.30.104:502"
.old brknumber
.COMPFILE4    IFILE           fix=500,uncomp,Name="COMPANYB.isi|20.20.30.104:502"
COMPFILE4     IFILE           fix=500,uncomp,Name="COMPANYB.isi|10.10.30.104:502"
.old Ownernumber
.COMPFILE5    IFILE           fix=500,uncomp,Name="COMPANYO.isi|20.20.30.104:502"
COMPFILE5     IFILE           fix=500,uncomp,Name="COMPANYO.isi|10.10.30.104:502"
.old Fulfillment number
.COMPFILE6    IFILE           fix=500,uncomp,Name="COMPANYR.isi|20.20.30.104:502"
COMPFILE6     IFILE           fix=500,uncomp,Name="COMPANYR.isi|10.10.30.104:502"
.START PATCH 1.2 ADDED LOGIC
.Associated Consultants/Brokers
.COMPFILE8    AFILE           fix=500,uncomp,Name="COMPANY2.AAM|20.20.30.104:502"
COMPFILE8     AFILE           fix=500,uncomp,Name="COMPANY2.AAM|10.10.30.104:502"
.END PATCH 1.2 ADDED LOGIC
              FileListEnd
.>Patch 1.4 End
COMPFILE7     FILE            fix=500
COMPNAME      INIT            "COMPANY       "
.Patch1.02
COMPNAM2      INIT            "COMPANY"
COMPNAM3      INIT            "COMPANYM"
COMPNAM4      INIT            "COMPANYB"
COMPNAM5      INIT            "COMPANYO"
COMPNAM6      INIT            "COMPANYR"
.START PATCH 1.2 ADDED LOGIC
COMPNAM8      INIT            "COMPANY2"
.END PATCH 1.2 ADDED LOGIC
.Patch1.02
COMPPATH      FORM            1
COMPFLAG      FORM            1
.START PATCH 1.5 ADDED LOGIC
COMPFLGSEQ    FORM            1
.END PATCH 1.5 ADDED LOGIC
COMPFLD       DIM             6
COMPFLD2      DIM             58
.Patch1.02
COMPFLD3      DIM             4
COMPFLD4      DIM             4
COMPFLD5      DIM             4
COMPFLD6      DIM             4
.For Company Flags
COMPFLD7      DIM             4
COMPFLD8      DIM             4
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
COMPCONVFLAG  FORM 1
.
.Patch1.02
COMPLOCK      FORM            1

COMPVARS      LIST
COMPNUM        DIM            6       1-6     ;COMPANY NUMBER. **KEY**
COMPCOMP      DIM             55      7-61    ;*******Required*******
COMPADDR      DIM             35     62-96
COMPADDR2     DIM             35     97-131
COMPCITY      DIM             30    132-161   ;CITY.
.MAY NEED TO INCREASE FOLLOWING FOR USE IN FOREIGN COUNTRY STATES
COMPSTATE     DIM             2     162-163   ;STATE.
.MAY NEED TO INCREASE FOLLOWIN FOR USE IN FOREIGN COUNTRY
COMPZIP        DIM            10    164-173   ;ZIP.
.MAY NEED TO INCREASE FOLLOWING FOR USE IN FOREIGN COUNTRY
COMPCNTRY     DIM             15    174-188
.MAY NEED TO INCREASE FOLLOWING FOR USE IN FOREIGN COUNTRY - IS COUNTRY CODE ALWAYS ONE BYTE
.PRELIMINARY RESEARCH AT ATT.COM INDICATES MANY COUNTRIES HAVE 3 BYTE COUNTRY CODE
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
COMPOLDMLR    DIM             4     233-236   ;Old Mailer Number                 ;= key for COMPfile3
COMPOLDBRK    DIM             4     237-240   ;Old Broker/consultant #           ;= key for COMPfile4
COMPOLDOWN    DIM             4     241-244   ;Old Owner Number                  ;= key for COMPfile5
COMPOLDSVB    DIM             4     245-248   ;Old return-To (service b) Number  ;= key for COMPfile6
COMPMUSAGE    DIM             1     249-249   ; 'F' If we cannot share Mailers list usage info
CompExcl	  Dim	      1     250-250   .'N' = NIN, 'P' = Pacific Lists -id our mailers by company
COMPBLANK1    DIM             2     251-252
.COMPBLANK1    DIM             3     250-252
COMPCREDIT    DIM             1     253-253   ;CREDIT STATUS
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
COMPBDRCTFLG  DIM             1     254-254   ;'T'  If Mailer and Billed Direct
COMPCONSULT   DIM             6     255-260   ;For Mailer - Code of Consultant used
COMPCONSULT1  DIM             3     261-263   ;For Mailer - Contact Code of Consultant used
COMPMANAGER   DIM             6     264-269   ;For List Owner - Code of Manager used
COMPMANAGER1  DIM             3     270-272   ;For List Owner - Contact Code of Manager used
.Following 2 fields may go bye-bye.
COMPSRVB      DIM             6     273-278   ;For List Owner - Code of Service B used
COMPSRVB1     DIM             3     279-281   ;For List Owner - Contact Code of Service B used
COMPBROKER    DIM             6     282-287   ;For Mailer - Code of Broker used
COMPBROKER1   DIM             3     288-290   ;For Mailer - Contact Code of Broker used
COMPEMAIL     DIM             50    291-340   ;Corporate Email Address (Generic)
COMPPBRKFLG   DIM             1     341-341   ;'1,2,3' = Broker Rank - BLO
COMPACCEPT    DIM             1     342-342   ;'F' = We do not accept their Guars.
COMPUSER      DIM             7     343-349   ;Record Created by
COMPDTE        DIM            8     350-357   ;Date record Added
COMPRUSER     DIM             8     358-365   ;Added/Modified by
COMPRDTE      DIM             8     366-373   ;Date Record Modified
COMPMAIN      DIM             6     374-379   ;if this is a satelite or subsidiary office it holds id # of main (parent) corp record
COMPINACTIVE  DIM             1     380-380   ;'T' if this company is inactive -see notes for reason
COMPBLANK2    DIM             20    381-400   ;available space
COMPFTP        DIM            50    401-450   ;Companies FTP Site
COMPACCTM     DIM             1     451-451   ;Acct Method "C"ash "A"ccrual
COMPACCTD     DIM             4     452-455   ;start date of accounting year MMDD  ie 0101 or 0701
COMPDISCFLG   DIM             1     456-456   ;"T" if discount associated with this company
.  Note 1
.  I have changed my mind and think this should be tracked by List
.COMPLcrsGo   DIM             5     433-437  ;for those owners who's Lcr's go to a diff place
.COMPLcrsGo1    DIM           3     438-440  ;for those Owners who's Lcr's go to a diff place
.COMPOrdsGo   DIM             5     441-445  ;for those Owners who's Order's go to a diff place
.COMPOrdsGo1  DIM             3     446-448  ;for those Owners who's Order's go to a diff place
. end Note 1
COMPINVSGO    DIM             6     457-462   ;for those Mailers who's Invoices's go to a diff place
COMPINVSGO1   DIM             3     463-465   ;for those Mailers who's Invoices's go to a diff place
COMPFAXFLAG1  DIM             1     466-466   ;T = fax order confirmations to mailer
COMPFAXFLAG2  DIM             1     467-467   ;T = fax order confirmations to Broker/Consultant
COMPEXCHANGE  DIM             1     468-468   ;T = Exchanges Allowed
COMPGALLEY    DIM             1     469-469   ;T = List Owner prefers Galley Listing
COMPTAXID     DIM             15    470-484   ;TEFRA TAX ID CODE - pulled from Owner file
.begin patch 1.01
CompTransmitFlag Dim          1     485-485   ;-Send Fulfilment info via: ' '=fax or print based on  presence of fax#
.                                             ; 1=FTP, 2= Email.......
.begin patch 1.02                ;Does Vendor Receive Statement
COMPSTATEMENT DIM 1
.patch1.02
.START PATCH 1.1 REPLACED LOGIC
.COMPFILLER   DIM             14    486-500   ;not used
COMPCNTDATE   DIM             8     486-493   ;Client Contract Date - Used for revenue purposes
.START PATCH 1.3 REPLACED LOGIC
.COMPFILLER   DIM             6     494-500   ;not used
COMPOSamp     dim             1     494-494   ;for Owners - Flag to indicate No Samples Allowed
COMPODRCTFLG  DIM             1     495-495   ;'T'  If Mailer and Orders go Direct
COMPBRKRPT    DIM             1     496-496   ;
COMPFILLER    DIM             3     497-500   ;not used
.COMPFILLER   DIM             5     495-500   ;not used
.END PATCH 1.3 REPLACED LOGIC
.END PATCH 1.1 REPLACED LOGIC
.COMPFILLER   DIM             16    485-500   ;not used
.end patch 1.01
               LISTEND