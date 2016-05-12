.----------------------------------------------------------------
.;
.;   CNTDD.INC - 
.;
.;   Copyright(c) Names in the News Ca., Inc. 2000
.;
.;   Author: David L Herrick
.    Release        1.2                 DLH 07 Jan 2010  3 New list flags
.;   Release         1.1                2005 June 18 Changed IP of File Manager
.;   Release         1.0                2002June
.;   Master Contact file. See CMPDD for Master Company info.
.;   Related file  CNTNotes = Comments, info pertinant to only a particular Contact.
.;----------------------------------------------------------------
.>Patch 1.1 Begin
CNCTFLIST Filelist
.CNCTFILE IFILE     keylen=9,fix=304,uncomp,Name="CONTACTS.ISI|20.20.30.103:502"
.CNCTFILE2          AFILE     fix=304,uncomp,Name="CONTACTS.AAM|20.20.30.103:502"
.CNCTFILE3          IFILE     KEYLEN=7,fix=304,uncomp,Name="CONTACT2.ISI|20.20.30.103:502"
CNCTFILE  IFILE     keylen=9,fix=304,uncomp,Name="CONTACTS.ISI|NINS1:502"
CNCTFILE2 AFILE     fix=304,uncomp,Name="CONTACTS.AAM|NINS1:502"
CNCTFILE3 IFILE     KEYLEN=7,fix=304,uncomp,Name="CONTACT2.ISI|NINS1:502"
                    FileListEnd
.>Patch 1.1 Patch End                   
CNCTFILE4 FILE     fix=304

CNCTNAME  INIT      "CONTACTS"
CNCTNME2  INIT      "CONTACT2"
CNCTFLAG  FORM      1
CNCTFLD   DIM       9         .company+contact  Isam Key   
CNCTFLD2  DIM       9         .Company Number  Aim Key
CNCTFLD3  DIM       48        .Contact Name    Aim Key
cnctfld4        DIM     7     .  old broker(mlr)/contact   Isam
CNCTLOCK  FORM      1
.;
CNCTVARS  LIST
CNCTCODE  DIM       6        1-6    ;COMPANY NUMBER - KEY
CNCTID              DIM       3        7-9    ;CONTACT NUMBER - KEY
CNCTFNAME DIM       45      10-54   ;Full Name
CNCTLNAME DIM       25      55-79   ;Last Name
CNCTTITLE DIM       33      80-112  ;Job Title
CNCTSAL   DIM       30     113-142  ;Salutation
CNCTEMAIL DIM       50     143-192  ;Email
CNCTCNTRY DIM       4      193-196  ;Phone/Fax Country Code
CNCTPHONE DIM       10     197-206  ;Contact Phone
CNCTPHONE1          DIM       10     207-216  ;Contact Phone
CNCTFAX             DIM       10     217-226  ;Contact Fax
CNCTINACTIVE        DIM       1      227-227  ;'T' if this contact is no longer active - historical purposes
CNCTPWKLYFLG        DIM       1      228-228  ;'T' = Member of Weekley Promo List    
CNCTPROMOFLG        DIM       1      229-229  ;'T' = Member of General Promo List
CNCTPBRKFLG         DIM       1      230-230  ;'T' = Member of Broker Promo List         
CNCTPCLNTFLG        DIM       1      231-231  ;'T' = Member of Client/Prospect Promo List
CNCTPPRTYFLG        DIM       1      232-232  ;'T' = Member of Party Promo List          
CNCTPDMAFLG         DIM       1      233-233  ;'T' = Member of DMA Promo List            
CNCTPDTAFLG         DIM       1      234-234  ;'T' = member of datacard list             
CNCTHLYDYFLG        DIM       1      235-235  ;'T' = Member of Holiday list              
CNCTMRKTNEWS        DIM       1      236-236  ;'T' = Member of Newsletter list           
.begin patch 1.2
CNCTACTFLG          DIM       1      237-237  ;'T' = Member of Active client list ($20,000 and Up volune)
CNCTPRosFLG         DIM       1      238-238  ;'T' = Member of Prospective client list ($30,000 and Up volune)
CNCTVndrFLG         DIM       1      239-239  ;'T' = Member of Vendor list           
CNCTFILL            DIM       7      240-246  ;unused
.CNCTFILL  DIM       10     237-246  ;unused
.end patch 1.2
CNCTORDDISP         DIM       1      247-247  ;'F' = Non order related ie acct do not display in contact list for order selection
CNCTTYPE  DIM       1      248-248  ;Type of Contact
.; 1 = Mailer
.; 2 = Broker
.; 3 = List Owner
.; 4 = Service Bureau
.; 5 = Consultant
.; 6 = Manager
CNCTUSER  DIM       8      249-256  ;User who created the record
CNCTDATE  DIM       8      257-264  ;Date record was created - CCYYMMDD
CNCTUSER2 DIM       8      265-272  ;Last modified by
CNCTDATE2 DIM       8      273-280  ;Date last modified - CCYYMMDD
CNCTCNT   DIM       7      281-287      ;Original Broker/Contact NUMBER  KEY
CNCTSALES       DIM     2      288-289  ;salesperson's #
CNCTFILL2 DIM       15     290-304  ;Filler
          Listend
