Pc        Equ       0
          Include   Common.inc
          INclude   Cons.inc  
          include   srdsdd.inc
          include   srdsLOdd.inc
          include   SRDSSELdd.inc                             .Selects(segments) with counts
          include   SRDSSLtdd.inc                             .selects,addressing, etc
          include   SRDSxSLtdd.inc                             .xref selects,addressing, etc
          include   NREFdd.inc                          
.begin patch 1.2          
          include   SRDSTXTdd.inc                             .special pricing
.end patch 1.2          

Release   INit      "1.51"    DLH            .check for numeric counts thru date.
REldate   Init      "2014 March 13"
.Release   INit      "1.5"    DLH            .pull counts thru date for revdate
.REldate   Init      "2013 May 17"
.Release   INit      "1.4"    DLH            code to find additional rates
.REldate   Init      "03 January 2013"
.Release   INit      "1.3"    DLH            addition of more variable cleaning
.REldate   Init      "20 JUne 2012"
.Release   INit      "1.2"    DLH            addition of special pricing info
.REldate   Init      "14 September 2011"
.Release   INit      "1.1"    DLH            addition of list status OTM
.REldate   Init      "22 August 2011"
.Release   INit      "1.0"    DLH
.REldate   Init      "13 April 2011"
Str50a    dim       50
Str50b    dim       50
Str50c    dim       50
Str50d    dim       50
Str50e    dim       50


FieldNames          Ifile     keyl=40
Skey      Dim       40
Record     RECORD
Fieldname  Dim      30
          RECORDEND
.begin patch 1.4
SRDSTXTDELFLAG      Dim       1
.end patch 1.4

RecCount  FOrm      5  
PhoneCOunt          Form      1         1-4 phone numbers
INetCOunt           Form      1         1-2 Email Addresses
phoneType           Dim       1         (F)ax (P)hone
mgrflag   dim       1         Y=processing mgr data
SelFlag   Init      "N"       Y=Process selects with counts 
SelFlag1  Init      "Y"       Y= we have already processed 1st select
SelFlag2  Init      "N"       Y= we have additional selects without counts
SelFlag2a Init      "Y"       Y= we have already processed 1st select
BaseFlag  Init      "N"         yes= we have base price
UpDateFlag  Init      "N"       Y= update date
countsUpdateFlag  Init      "N"       Y= counts thru date
CountFlag Init      "N"       Y= counts thru date
OwnCOunt  Form      5
LstCOunt  Form      5
SRDSSELPRICEX FORM      5.2         SELECT CODE PRICE extRA
.I did the same thing some years ago.... getting XML TAGS (field names) and VALUES (field values) but instead of using XFILES, I used a TREEVIEW and processed the items from there. That was before Sunbelt released XFILE support.
.
.Doing it this way, I didn't care WHAT the structure was... if I could identify the node as a TAG (field name), then I knew the child node was its field value. Anything else, I ignored because it was just structure; not field names or field values. The only "extra" thing I had to do was create a data file of TAGS (field names). But you could do it differently; use an array, a long string in memory, etc. And, instead of my switch/endswitch, you could use STORE with an index, for instance. Or any other creative solution.
.
.The first thing I did was load the TREEVIEW with the XML file.
.Code: 

 
tvXfile     treeview                    // XML file contents
XMLfilename dim  260
nItem       form 9
nSiblingItem form 9
ndx         form 9
.Variables used for TreeView Object
TVI_ROOT integer    4,"0xFFFF0000"
TVI_1ST   integer   4,"0xFFFF0001"
TVI_LAST integer    4,"0xFFFF0002"
TVI_SORT integer    4,"0xFFFF0003"
.
ROOT      integer   1,"0x00"  TVGN_ROOT first child item of the root item.
NEXT      integer   1,"0x01"  TVGN_NEXT next sibling item.
PREVIOUS integer 1,"0x02"     TVGN_PREVIOUS       previous sibling item.
PARENT    integer   1,"0x03"  TVGN_PARENT         parent of the item
CHILD     integer   1,"0x04"  TVGN_CHILD          first child item.
.0x05     TVGN_FIRSTVISIBLE   first visible item.
.0x06     TVGN_NEXTVISIBLE    next visible item.
.0x07     TVGN_PREVIOUSVISIBLE          first visible item that precedes the given item.
.0x08     TVGN_DROPHILITE     item that is the target of a drag-and-drop operation.
.0x09     TVGN_CARET          currently selected item.
.                             Replace with Space Characters Less than a Space.                             and greater than a ~
REPBAD   INIT       0x00,0x20,0x01,0x20,0x02,0x20,0x03,0x20,0x04,0x20,0x05,0x20:
.                    0x06,0x20,0x07,0x20,0x08,0x20,0x09,0x20,0x0A,0x20,0x0B,0x20: .Keep Line feed
                    0x06,0x20,0x07,0x20,0x08,0x20,0x09,0x20,0x0B,0x20:
.                    0x0C,0x20,0x0D,0x20,0x0E,0x20,0x0F,0x20:             keep Carriage return
                    0x0C,0x20,0x0E,0x20,0x0F,0x20:
                    0x10,0x20,0x11,0x20,0x12,0x20,0x13,0x20,0x14,0x20,0x15,0x20:
                    0x16,0x20,0x17,0x20,0x18,0x20,0x19,0x20,0x1A,0x20,0x1B,0x20:
                    0x1C,0x20,0x1D,0x20,0x1E,0x20,0x1F,0x20:
                    0x7F,0x20:
                    0x80,0x20,0x81,0x20,0x82,0x20,0x83,0x20,0x84,0x20,0x85,0x20:
                    0x86,0x20,0x87,0x20,0x88,0x20,0x89,0x20,0x8A,0x20,0x8B,0x20:
                    0x8C,0x20,0x8D,0x20,0x8E,0x20,0x8F,0x20:
                    0x90,0x20,0x91,0x20,0x92,0x20,0x93,0x20,0x94,0x20,0x95,0x20:
                    0x96,0x20,0x97,0x20,0x98,0x20,0x99,0x20,0x9A,0x20,0x9B,0x20:
                    0x9C,0x20,0x9D,0x20,0x9E,0x20,0x9F,0x20:
                    0xA0,0x20,0xA1,0x20,0xA2,0x20,0xA3,0x20,0xA4,0x20,0xA5,0x20:
                    0xA6,0x20,0xA7,0x20,0xA8,0x20,0xA9,0x20,0xAA,0x20,0xAB,0x20:
                    0xAC,0x20,0xAD,0x20,0xAE,0x20,0xAF,0x20:
                    0xB0,0x20,0xB1,0x20,0xB2,0x20,0xB3,0x20,0xB4,0x20,0xB5,0x20:
                    0xB6,0x20,0xB7,0x20,0xB8,0x20,0xB9,0x20,0xBA,0x20,0xBB,0x20:
                    0xBC,0x20,0xBD,0x20,0xBE,0x20,0xBF,0x20:
                    0xC0,0x20,0xC1,0x20,0xC2,0x20,0xC3,0x20,0xC4,0x20,0xC5,0x20:
                    0xC6,0x20,0xC7,0x20,0xC8,0x20,0xC9,0x20,0xCA,0x20,0xCB,0x20:
                    0xCC,0x20,0xCD,0x20,0xCE,0x20,0xCF,0x20:
                    0xD0,0x20,0xD1,0x20,0xD2,0x20,0xD3,0x20,0xD4,0x20,0xD5,0x20:
                    0xD6,0x20,0xD7,0x20,0xD8,0x20,0xD9,0x20,0xDA,0x20,0xDB,0x20:
                    0xDC,0x20,0xDD,0x20,0xDE,0x20,0xDF,0x20:
                    0xE0,0x20,0xE1,0x20,0xE2,0x20,0xE3,0x20,0xE4,0x20,0xE5,0x20:
                    0xE6,0x20,0xE7,0x20,0xE8,0x20,0xE9,0x20,0xEA,0x20,0xEB,0x20:
                    0xEC,0x20,0xED,0x20,0xEE,0x20,0xEF,0x20:
                    0xF0,0x20,0xF1,0x20,0xF2,0x20,0xF3,0x20,0xF4,0x20,0xF5,0x20:
                    0xF6,0x20,0xF7,0x20,0xF8,0x20,0xF9,0x20,0xFA,0x20,0xFB,0x20:
                    0xFC,0x20,0xFD,0x20,0xFE,0x20,0xFF,0x20

fiTemp  file
sBig    dim  ^10
nSize   form 10
.
 
          Move      c0 to N5
          Display   *p1:1,*el,"Pre Processing SRDS ",*w2
          Move     "c:\work\srds\NewsCaliforniaDMLS.xml",XmlFileName

          TRAP      IOMssg Giving Error if IO
.          goto      restart
          move           "READ - c:\work\srds\NewsCaliforniaDMLS.xml",Location
          pack           KeyLocation,"seq: ",Skey

          Move      "0001",SRDSSELNUM
          findfile XMLfileName,filesize=nSize
          dmake sBig,nSize    
          open fiTemp,XMLfileName
          read fiTemp,seq;*ABSON,sBig    
          Close fiTemp    
          replace "¿-",Sbig    
          replace "ée",Sbig    
          replace "ñn",SBig
           replace "¢c",Sbig
           Replace  RepBad,SBig
.          move      " </",str4
.          move      "</",str3
          erase XMLfileName
          move           "PREP - c:\work\srds\NewsCaliforniaDMLS.xml",Location
          pack           KeyLocation,"seq: ",Skey
          prep fiTemp,XMLfileName
          move           "Write - c:\work\srds\NewsCaliforniaDMLS.xml",Location
          pack           KeyLocation,"seq: ",Skey
          write fiTemp,seq;sBig    
          close fiTemp    
          dfree sBig        
restart
.    create  tvXfile=1:2:3:4,visible=$OFF,autoredraw=$OFF    // for xml file contents
          Display   *p1:1,*el,"Loading SRDS Data into ListView"
    create  tvXfile=1:2:3:4

    tvXfile.LoadXMLFile GIVING Result USING *FileName=XMLFilename
    if ( Result = 0 )       // XML file load success
.    ....[[process data]]....
          Open      Fieldnames,"fieldnames|10.10.30.103:502",read
          Display   *p1:1,*el,"Processing SRDS Data"
    call  Process      
          Else
          Move      "DavidHerrick@nincal.com",Mailto
          Move      "DavidHerrick@nincal.com",MailFrom
          Move      "SRDS XML Load Failed",MailSubjct
          call      Sendmail
.           display   *p1:10,*el,"XML Load Failed!!!! ",*w10

    endif
          shutdown
          Stop 



...then, if the XML file loaded successfully, [[process the data]]:

Process 
.   *
.   * now, process each node in the treeview for fields....
.   *
    tvXfile.GetNextItem giving nItem using *Item=ndx:
                                            *Code=ROOT
    move    nItem to nSiblingItem         // remember this
    loop
    until ( nItem = 0 )
        call    AssignFieldValue using nItem,Result // is it a TAG?
        call    ProcessChildren using nItem    // any children to this?
        tvXfile.GetNextItem giving nItem using *Item=nSiblingItem:
                                                *Code=NEXT
    repeat
          call      Break

          Return



...AssignFieldValue would get the node value and determine whether the value was a field name (TAG) or a field value, or nothing of interest (structure only):

 
AssignFieldValue function   // parameters follow:
nItem      form    15                  // item handle
npBadSw    form    ^                   // bad result flag
    entry                   // local variables follow:
nChild     form    15
work40      dim     40                  // for tag name
.begin patch 1.2          
.work100     dim     100                 // for field value
work100     dim     250                 // for field value
.end patch 1.2          
.                           // instructions follow:
    tvXfile.GetItemText giving work40 using *Item=nItem
.   * 
.   * look up TREEVIEW branch "value" in data file; is it a field name (TAG) or a field value?
.   *
.check for listing as it indicates a new "record"
    packkey sKEY with work40
          call      trim using work40
          if        (work40 = "listing")
          call      Break
          endif
          move           "Read - Fieldnames",Location
          pack           KeyLocation,"Key: ",Skey
          read    FIELDNAMES,sKEY;record       // read by tag
          if  over                          // tag not found
                if        (mgrflag = yes)
                    Type      Work40
                    if        Not equal
                              if        (work40 <> "")
                              MOVe      "amp;",str4
                              MOve      b1,str1
                              call     replaceit using Work40,str4,str1

                              MOVe      "&  ",str4
                              MOve      "&",str1
                              call     replaceit using Work40,str4,str1
                              move      work40,SRDSCOMP
                              move      No,MgrFlag
                              endif
                    endif          
                    
          endif
        set     npBadSw                 // not a TAG (field name)
    else                                // got the TAG (field name)
        chop    record.fieldname
        if ( record.fieldname != "none" ) // field name is not 'none'
            tvXfile.GetNextItem giving nChild using *Item=nItem:
                                                     *Code=CHILD
            if ( nChild = 0 )
                set     npBadSw
            else
                tvXfile.GetItemText giving work100 using *Item=nChild
                call          Trim using work100

                    MOVe      "amp;",str4
                    MOve      b1,str1
                    call     replaceit using Work100,str4,str1

                    MOVe      "&  ",str4
                    MOve      "&",str1
                    call     replaceit using Work100,str4,str1


                chop    record.fieldname
                switch  record.fieldname
....................................................................................................................................

                case    "titleName"
                    move    work100 to SRDSMLSTNAME
.if srdslstnum <> "" then its ownernumber
                case    "smidID"
                    count     n2,SRDSLSTNUM
                    if        (n2 = c0)
                    move    work100 to SRDSLSTNUM
                    else
                    move    work100 to SRDSOWNNUM
                    move    work100 to SRDSONUM
                    move      Yes,MgrFlag
                    endif

                 case    "addrType"
                    move    work100 to SRDSLOType
                    
                case    "addrLine3"
                    move    work100 to SRDSADDR
                case    "addrCity"
                    move    work100 to SRDSCITY
                case    "addrState"
                    move    work100 to SRDSState
                case    "addrzip"
                    move    work100 to SRDSZip
                case    "priceAmt"
                    if        (baseFlag = No)          
                    move    work100 to SRDSPRice
                    move      yes,Baseflag
                    elseif    (SelFlag = Yes)                                    .we are working on a select
                    move    work100 to SRDSSELPRICE
                    move    "001"  to SRDSSELDESC                                   .default
                    endif
                Case    "extraAmt"
                    move    work100 to SRDSSELPRICEX                            .add on fee
                
                case    "totalNbr"
                    if        (selflag = no)
                    move    work100 to SRDSUNIVERSE
                    Elseif     (SelFlag = yes)
                    move    work100 to SRDSSELQTY
                    endif                              
                case    "phone"
                    add       C1,Phonecount
                case    "phoneType"
                    move    work100 to phoneType      
                Case          "commtobroker"
.                    call      debug
                    unpack    work100 into str2
                    pack      SRDSCOMMPER from b1,str2,".00"
                    
                case    "phoneAreaCode"
                    if        (PhoneCOunt = c1 or PhoneCOunt = c2)
                              if        (PhoneType = "F")
                              move    work100 to SRDSLOFAX.SRDSLOFAX1
                              elseif        (PhoneType = "P")
                              move    work100 to SRDSLOPhone.SRDSLOPhone1
                              endif
                    Elseif        (PhoneCOunt = c3 or PhoneCOunt = c4)
                              if        (PhoneType = "F")
                              move    work100 to SRDScFAX.SRDScFAX1
                              elseif        (PhoneType = "P")
                              move    work100 to SRDScPhone.SRDScPhone1
                              endif
                    endif                              
                case    "phoneExchange"
                    if        (PhoneCOunt = c1 or PhoneCOunt = c2)
                              if        (PhoneType = "F")
                              move    work100 to SRDSLOFAX.SRDSLOFAX2
                              elseif        (PhoneType = "P")
                              move    work100 to SRDSLOPhone.SRDSLOPhone2
                              endif
                    Elseif        (PhoneCOunt = c3 or PhoneCOunt = c4)
                              if        (PhoneType = "F")
                              move    work100 to SRDScFAX.SRDScFAX2
                              elseif        (PhoneType = "P")
                              move    work100 to SRDScPhone.SRDScPhone2
                              endif
                    endif                              
                case    "phoneSuffix"
                    if        (PhoneCOunt = c1 or PhoneCOunt = c2)
                              if        (PhoneType = "F")
                              move    work100 to SRDSLOFAX.SRDSLOFAX3
                              elseif        (PhoneType = "P")
                              move    work100 to SRDSLOPhone.SRDSLOPhone3
                              endif
                    Elseif        (PhoneCOunt = c3 or PhoneCOunt = c4)
                              if        (PhoneType = "F")
                              move    work100 to SRDScFAX.SRDScFAX3
                              elseif        (PhoneType = "P")
                              move    work100 to SRDScPhone.SRDScPhone3
                              endif
                    endif                              
                case    "inetAddr"                                
                    if        (INetCOunt = c1)
                              move    work100 to SRDSLOEMAIL
                              add       c1,InetCOunt
                    else
                              move    work100 to SRDSCEmail
                    endif
                case    "nameFirst"
                         append         work100,SRDSCntct     
                case    "nameMiddle"
                         append         b1,SRDSCntct          
                         append         work100,SRDSCntct     
                case    "nameLast"
                         append         b1,SRDSCntct          
                         append         work100,SRDSCntct     
                         reset          SRDSCntct          
                Case    "selWCntsMain"          
.begin patch 1.5
.new DH MAy 2013
                Case     "dateCountsThru"         
                          Move  yes,countsUpdateFlag
                         call      debug             
.end patch 1.5

                case          "dateUpdated"
                         call      debug
                         move  yes,UpDateFlag
                Case          "formatted"             
                        if              (UpDateFlag = yes)      
                        unpack          Work100 into str9,str3,str1,str2,str1,str1,str4
                        call            getmonth
                        move            str2,dd
                        unpack          str4 into cc,yy
.begin patch 1.5
                        Pack              SRDSNDATUPDDATE from cc,yy,mm,dd
.                        pack            SRDSREVDATE from cc,yy,mm,dd
.new DH MAy 2013
.end patch 1.5
                              Move        No,UpDateFlag
                        ElseIF          (CountFlag = yes)
.                              .use Nseldate??  SRDS DATA ONLY SEEMS TO HAVE MONTH AND YEAR??
                        ElseIf          (countsUpdateFlag = yes)    
                        unpack          Work100 into str13,str3,str1,str4
                        call            getmonth
.begin patch 1.51
                        unpack          str4 into cc,yy
.                        pack            SRDSREVDATE from cc,yy,mm,"01"
.                        move            No,countsUpdateFlag            
			Type		str4
				if	equal
                        	pack    SRDSREVDATE from cc,yy,mm,"01"
                        	else
                        	move	SRDSNDATUPDDATE,SRDSREVDATE
                        	endif
			move    No,countsUpdateFlag            

.end patch 1.51
                        endif
                case    "segTitleName"
                        IF    (WORK100 = "SELECTIONS WITH COUNTS")
                        MOVE  YES,SELFLAG
                        ElseIF    (WORK100 = "OTHER SELECTIONS")
                        MOVE  No,SELFLAG
                        MOVE  Yes,SELFLAG2
                        ENDIF
                case    "paraText"
                              Move      Work100,Taskname
                              rep       uplow in taskname
                              scan     "net name arrangement",taskname
                              if        equal
                              Move      "Net name arrangement ",str55
                              move      b1,str1
                              call      ReplaceIt using Work100,str55,str1

                              Move      "Net name arrangements ",str55
                              move      b1,str1
                              call      ReplaceIt using Work100,str55,str1
                              Move      work100 to SRDSNETINFO
                              goto      Endparatext
                              endif

                              Move      Work100,Taskname
                              rep       uplow in taskname
                              scan     "net name policy",taskname
                                        if        equal
          
                                        Move      "Net name policy ",str55
                                        move      b1,str1
                                        call      ReplaceIt using Work100,str55,str1
                                        Move      work100 to SRDSNETINFO

                              goto      Endparatext
                              endif
                              Scan      "(*) Exchange only",work100
                                        if        equal
                                        Move      c2,SRDSSELEXC
                                        else
.check for special pricing and write it out
.fundraisin, pub, etc
.                              call      debug
                                        Reset     Work100
                                        move      c1,srdstxtpath
                                        packkey   SRDSTXTFLD from SRDSLSTNUM
                                        Move      SRDSLSTNUM,str6
                                        move           "Read-SRDSTXTTsT",Location
                                        pack           KeyLocation,"Key: ",SRDSTXTFLD
                                        Loop
                                        call      SRDSTXTTST
                                        until     over
                                        move           "Read-SRDSTXTDEL",Location
                                        pack           KeyLocation,"Key: ",SRDSTXTFLD
                                        call      SRDSTXTDEL
                                        Move      Yes,SRDSTXTDELFLAG
                                        repeat
                                        move      srdslstnum,SRDSTXTLSTNUM
                                        packkey   SRDSTXTFLD from SRDSLSTNUM

                                        call      trim using work100
                                        unpack    work100 into str50a,str50b,str50c,str50d,str50e          
                                        call      trim using str50a
                                        cmatch    b1,str50a
                                        if        not eos
                                        move           "Read-SRDSTXTWRT",Location
                                        pack           KeyLocation,"Key: ",SRDSTXTFLD
                                        packkey   SRDSTXTFLD1 from SRDSLSTNUM,"01"
                                        move      "01",srdstxtnum
                                        move      str50a,srdstxt
                                        call      SRDSTXTWRT
                                        endif                    

                                        call      trim using str50b
                                        cmatch    b1,str50b
                                        if        not eos
                                        move           "Read-SRDSTXTWRT",Location
                                        pack           KeyLocation,"Key: ",SRDSTXTFLD
                                        packkey   SRDSTXTFLD1 from SRDSLSTNUM,"02"
                                        move      "02",srdstxtnum
                                        move      str50b,srdstxt
                                        call      SRDSTXTWRT
                                        endif                    

                                        call      trim using str50c
                                        cmatch    b1,str50c
                                        if        not eos
                                        move           "Read-SRDSTXTWRT",Location
                                        pack           KeyLocation,"Key: ",SRDSTXTFLD
                                        packkey   SRDSTXTFLD1 from SRDSLSTNUM,"03"
                                        move      "03",srdstxtnum
                                        move      str50c,srdstxt
                                        call      SRDSTXTWRT
                                        endif                    

                                        call      trim using str50d
                                        cmatch    b1,str50d
                                        if        not eos
                                        move           "Read-SRDSTXTWRT",Location
                                        pack           KeyLocation,"Key: ",SRDSTXTFLD
                                        packkey   SRDSTXTFLD1 from SRDSLSTNUM,"04"
                                        move      "04",srdstxtnum
                                        move      str50D,srdstxt
                                        call      SRDSTXTWRT
                                        endif                    
                                        call      trim using str50e
                                        cmatch    b1,str50e
                                        if        not eos
                                        move           "Read-SRDSTXTWRT",Location
                                        pack           KeyLocation,"Key: ",SRDSTXTFLD
                                        packkey   SRDSTXTFLD1 from SRDSLSTNUM,"05"
                                        move      "05",srdstxtnum
                                        move      str50E,srdstxt
                                        call      SRDSTXTWRT
                                        endif                    

                              endif
                              Reset     Work100

Endparatext
.if its exchange only process here as well. --- 
                    Case      "rowText"
                              if        (selflag = yes)
                                        if        (srdslstnum = "041957")
.                                        call      debug
                                        endif
                                        if        (SELflag1 = yes)      .FIrst one
                                        packkey   SRDSSELFLD from SRDSLSTNUM
                                        Move      SRDSLSTNUM,str6
                                        move           "Read-SRDSSELKey",Location
                                        pack           KeyLocation,"Key: ",SRDSSELFLD
                                        call      SRDSSELTST
                                                  if        Not over
                                                  move           "Rln-SRDSSELDEL",Location
                                                  pack           KeyLocation,"Key: ",SRDSSELFLD
                                                  call      SRDSSELDEL
                                                  endif          
                                        loop
                                        move           "Read-SRDSSELKs",Location
                                        pack           KeyLocation,"Key: ",SRDSSELFLD
                                        CALL      SRDSSELKS1
                                        until     OVer
                                                  if        (SRDSSELLIST = str6)
                                                  move           "Read-SRDSSElDel",Location
                                                  pack           KeyLocation,"Key: ",SRDSSELFLD
                                                  call      SRDSSELDEL
                                                  else
                                                  Break
                                                  endif
                                              REpeat

                                        Move      str6,SRDSLstNum               .restore var
                                        move      "0001",SRDSSELnum
                                        Move      work100,SRDSSELSNAME
                                        move      no,SElFlag1
                                        else                          .we have already cleaned the file

                                        packkey   SRDSSELFLD from SRDSLSTNUM
                                        Move      SRDSLSTNUM,SRDSSELLIST
                                        move           "write-SRDSSELwrt",Location
                                        pack           KeyLocation,"Key: ",SRDSSELFLD
                                        Add       SRDSSELPRICEX,SRDSSELPRICE                 .ADD ADDITIONAL $
                                        Sub       SRDSSELPRICEX,SRDSSELPRICEX                .Clear variable
VERFSEL1                                          packkey   SRDSSELFld2 from SRDSLSTNUM,srdsselnum
                                                  move        c2,SRDSSelPath
                                                  call        srdsseltst
                                                  if          over
                                                  call      SRDSSELWRT
                                                  else
                                                  Move      SRDSSELnum,n4
                                                  add       C1,n4
                                                  Move      n4,SRDSSELnum
                                                  rep       Zfill,SRDSSELNUM
                                                  goto        VerfSel1
                                                  ENDIF





                                        Move      SRDSSELnum,n4
                                        add       C1,n4
                                        Move      n4,SRDSSELnum
                                        rep       Zfill,SRDSSELNUM
                                        Move      work100,SRDSSELSNAME
                                        endif
                             endif           
                 Case         "standardDesc"             
.                              call      Debug
                              if        (selflag2 = yes)
                                        if        (SELflag2a = yes)      .FIrst one
                                        packkey   SRDSSLtFLD from SRDSLSTNUM
                                        Move      SRDSLSTNUM,STr6
                                        move           "Read-SRDSSLtKey",Location
                                        pack           KeyLocation,"Key: ",SRDSSLtFLD
                                        call      SRDSSLTTST
                                                  if        Not over
                                                  move           "Rln-SRDSSLTDEL",Location
                                                  pack           KeyLocation,"Key: ",SRDSSLTFLD
                                                  call      SRDSSLTDEL
                                                  endif          
                                        loop
                                        move           "Read-SRDSSLTKs",Location
                                        pack           KeyLocation,"Key: ",SRDSSLTFLD
                                        CALL      SRDSSLTKS1
                                        until     OVer
                                                  if        (SRDSSLTLIST = str6)
                                                  move           "Read-SRDSSLTKey",Location
                                                  pack           KeyLocation,"Key: ",SRDSSLTFLD
                                                  call      SRDSSLTDEL
                                                  else
                                                  Break
                                                  endif
                                        REpeat
                                        Move      str6,SRDSLstNum               .restore var
                                        Move      work100,SRDSSLTDES
                                        move      no,SElFlag2a

                                        else                          .we have already cleaned the file

                                        packkey   SRDSSLTFLD from SRDSLSTNUM
                                        Move      SRDSLSTNUM,SRDSSLTLIST

                                        call         SrdsSltXref
                                        
                                        move           "write-SRDSSLTwrt",Location
                                        pack           KeyLocation,"Key: ",SRDSSLTFLD
                                        call      SRDSSLTWRT
                                        Move      work100,SRDSSLTDES
                                        endif
                             endif           
                 Case         "perM"             
                                        Move      "001",SRDSSLTDESC
                 Case         "unitFlatExtra"
                                        Move      "002",SRDSSLTDESC
                 Case         "feeAmt"             
                                        Move      Work100,SRDSSLTPRICE 
                 Case         "mediaDesc"
                              if        (selflag2 = yes)
                                        if        (SELflag2a = yes)      .FIrst one
                                        packkey   SRDSSLtFLD from SRDSLSTNUM
                                        Move      SRDSLSTNUM,STr6
                                        move           "Read-SRDSSLtKey",Location
                                        pack           KeyLocation,"Key: ",SRDSSLtFLD
                                        call      SRDSSLTTST
                                                  if        Not over
                                                  move           "Rln-SRDSSLTDEL",Location
                                                  pack           KeyLocation,"Key: ",SRDSSLTFLD
                                                  call      SRDSSLTDEL
                                                  endif          
                                        loop
                                        move           "Read-SRDSSLTKs",Location
                                        pack           KeyLocation,"Key: ",SRDSSLTFLD
                                        CALL      SRDSSLTKS1
                                        until     OVer
                                                  if        (SRDSSLTLIST = str6)
                                                  move           "Read-SRDSSLTKey",Location
                                                  pack           KeyLocation,"Key: ",SRDSSLTFLD
                                                  call      SRDSSLTDEL
                                                  else
                                                  Break
                                                  endif
                                        REpeat
                                        Move      str6,SRDSLstNum               .restore var
                                        Move      work100,SRDSSLTDES
                                        move      no,SElFlag2a

                                        else                          .we have already cleaned the file

                                        packkey   SRDSSLTFLD from SRDSLSTNUM
                                        Move      SRDSLSTNUM,SRDSSLTLIST

                                                  call         SrdsSltXref

                                        move           "write-SRDSSLTwrt",Location
                                        pack           KeyLocation,"Key: ",SRDSSLTFLD
                                        call      SRDSSLTWRT
                                        Move      work100,SRDSSLTDES
                                        endif
                             endif           
                 Case         "otherDesc"
                              if        (selflag2 = yes)
                                        if        (SELflag2a = yes)      .FIrst one
                                        packkey   SRDSSLtFLD from SRDSLSTNUM
                                        Move      SRDSLSTNUM,STr6
                                        move           "Read-SRDSSLtKey",Location
                                        pack           KeyLocation,"Key: ",SRDSSLtFLD
                                        call      SRDSSLTtst
                                                  if        Not over
                                                  move           "Rln-SRDSSLTDEL",Location
                                                  pack           KeyLocation,"Key: ",SRDSSLTFLD
                                                  call      SRDSSLTDEL
                                                  endif          
                                        loop
                                        move           "Read-SRDSSLTKs",Location
                                        pack           KeyLocation,"Key: ",SRDSSLTFLD
                                        CALL      SRDSSLTKS1
                                        until     OVer
                                                  if        (SRDSSLTLIST = str6)
                                                  move           "Read-SRDSSLTKey",Location
                                                  pack           KeyLocation,"Key: ",SRDSSLTFLD
                                                  call      SRDSSLTDEL
                                                  else
                                                  Break
                                                  endif
                                        REpeat
                                        Move      str6,SRDSLstNum               .restore var
                                        Move      work100,SRDSSLTDES
                                        move      no,SElFlag2a

                                        else                          .we have already cleaned the file

                                        packkey   SRDSSLTFLD from SRDSLSTNUM
                                        Move      SRDSLSTNUM,SRDSSLTLIST

                                                  call         SrdsSltXref

                                        move           "write-SRDSSLTwrt",Location
                                        pack           KeyLocation,"Key: ",SRDSSLTFLD
                                        call      SRDSSLTWRT
                                        Move      work100,SRDSSLTDES
                                        endif
                             endif           
                endswitch
            endif
        endif
    endif
   functionend
 


....................................................................................................................................
...Lastly, the ProcessChildren function, ensures that all nodes are processed
.Code: 

 
.*
.* for a given treeview node, process all of its child nodes
.*
ProcessChildren function    // parameters follow:
nParentItem form   15
    entry                   // local variables follow:
nItem      form    15                  // item handle
nSiblingItem form  15                  // first sibling item handle
nBadSw     form    1
.                           // instructions follow:
    tvXfile.GetNextItem giving nItem using *Item=nParentItem:
                                            *Code=CHILD
    move    nItem to nSiblingItem             // remember this
    loop
    until ( nItem = 0 )

        call    AssignFieldValue using nItem,nBadSw   // is it a TAG?

        call    ProcessChildren using nItem    // any children to this?

        move    nItem to nSiblingItem             // remember this
        tvXfile.GetNextItem giving nItem using *Item=nSiblingItem:
                                            *Code=NEXT

    repeat
    functionend
.Notice that the ProcessChildren function is reentrant; it calls itself.
.........................................................................................................................................          
Break
          if        (recCount = c0)     .nothing to process yet
          add       c1,RecCount
          return
          endif
.else output and clear flags and variables
.begin patch 1.1
          scan      "OTM-",SRDSMLSTNAME
          if        equal
.          call      Debug
          move      "W",srdsStatus
          Reset     SRDSMLSTNAME
          Move      "OTM-",Str4
          call      REPLACEIT using SRDSMLSTNAME,STR4,B1
          call      Ltrim using SRDSMLSTNAME
.          call      Debug
          else      
          Reset     SRDSMLSTNAME
          Clear     SrdsStatus
          endif
.end patch 1.1
          
          add       c1,Lstcount
          display   *p1:10,*el,"List  count ",Lstcount,b1,srdsLSTNUM,b1,SRDSMLSTNAME
          move           "SRDStst",Location
          pack           KeyLocation,"Key: ",Srdsfld
          packkey   SRDSfld,SRDSLSTNUM
          call      SRDSTST
          if        over
          move           "SRDSWRT",Location
          pack           KeyLocation,"Key: ",Srdsfld
          call      SRDSWRT
          else
          move           "SRDSUPD",Location
          pack           KeyLocation,"Key: ",Srdsfld
          call      SRDSUpd
          ENDIF

          Type      srdsonum
          if        equal
          add       c1,Owncount
          display   *p1:12,*el,"owner count ",owncount,b1,srdsonum,b1,srdscomp
          packkey   SrdsLOfld,SRDSONUM
          move           "SRDSLOtst",Location
          pack           KeyLocation,"Key: ",SrdsLOfld
          call      SRDSLOTST
                    if        over
                    move           "SRDSLOwrt",Location
                    pack           KeyLocation,"Key: ",SrdsLOfld
                    call      SRDSLOWRT
                    else
                    move           "SRDSLOUPD",Location
                    pack           KeyLocation,"Key: ",SrdsLOfld
                    call      SRDSLOUpd
                    ENDIF
          endif

          add       c1,RecCount
.write out last select with counts
          packkey   SRDSSELFLD from SRDSLSTNUM
          Move      SRDSLSTNUM,SRDSSELLIST
          move      "write-SRDSSELwrt",Location
          pack      KeyLocation,"Key: ",SRDSSELFLD
          Add       SRDSSELPRICEX,SRDSSELPRICE                 .ADD ADDITIONAL $
          Sub       SRDSSELPRICEX,SRDSSELPRICEX                .Clear variable
VerfSel
.            call        Debug
            Packkey   SRDSSELFld2 from SRDSLSTNUM,srdsselnum
            move        c2,SRDSSelPath
            call        srdsseltst
            if          over
          call      SRDSSELWRT
            else
            Move      SRDSSELnum,n4
          add       C1,n4
          Move      n4,SRDSSELnum
          rep       Zfill,SRDSSELNUM
            goto        VerfSel
            endif
            move        c1,SRDSSelPath
.write out last select without counts
          packkey   SRDSSLTFLD from SRDSLSTNUM
          Move      SRDSLSTNUM,SRDSSLTLIST
          call      SrdsSltXref
          move      "write-SRDSSLTwrt",Location
          pack      KeyLocation,"Key: ",SRDSSLTFLD
          call      SRDSSLTWRT



          call      WipeVars
          return
........................................................................................................
GetMonth  if        (str3 = "Jan")
          move      "01",mm
          Elseif    (Str3 = "Feb")
          MOVe      "02",mm
          Elseif    (Str3 = "Mar")
          MOVe      "03",mm
          Elseif    (Str3 = "Apr")
          MOVe      "04",mm
          Elseif    (Str3 = "May")
          MOVe      "05",mm
          Elseif    (Str3 = "Jun")
          MOVe      "06",mm
          Elseif    (Str3 = "Jul")
          MOVe      "07",mm
          Elseif    (Str3 = "Aug")
          MOVe      "08",mm
          Elseif    (Str3 = "Sep")
          MOVe      "09",mm
          Elseif    (Str3 = "Oct")
          MOVe      "10",mm
          Elseif    (Str3 = "Nov")
          MOVe      "11",mm
          Elseif    (Str3 = "Dec")
          MOVe      "12",mm
          else
          clear     mm
          endif
          return

........................................................................................................
SrdsSltXref
          Clear     SRDSSLTTYPE
          Clear     SRDSSLTNUM
          move      c3,nrefpath
          move      "read-SRDSXSLT",Location
          pack      KeyLocation,"Key: ",SrdsXsltfld

          call      Trim using SRDSSLTDES
          Packkey   SRDSxSLTfld,SRDSSLTDES
          call      SRDSxSLTKey
          if        Not over
          call      Trim using SRDSxSLTDESc
          packkey   Nreffld4,SRDSxSLTDESC
          else
          packkey   Nreffld4,SRDSSLTDES
          endif
          move      "read-NrefKey",Location
          pack      KeyLocation,"Key: ",Nreffld4
          
          call      Nrefkey
          if        not over
          move      NREFCODE,SRDSSLTTYPE
          move      NREFNUM,SRDSSLTNUM
.add code here to track missing codes
          endif

          return
........................................................................................................

WipeVars
          Clear     SrdsOnum
          Clear     SRDSCOMP       
          Clear     SRDSADDR       
          Clear     SRDSADDR2      
          Clear     SRDSCITY       
          Clear     SRDSSTATE      
          CLear     SRDSZIP        
          CLear     SRDSCNTRY      
          Clear     SRDSCNTRYCDE   
          CLear     SRDSLOPHONE    
          Clear     SRDSLOFAX      
          Clear     SRDSLOACCTFAX  
          Clear     SRDSLOEMAIL    
          Clear     SRDSLOFTP      
          Clear     SRDSCntct      
          Clear     SRDSCphone     
          Clear     SRDSCFax       
          Clear     SRDSCEmail     
          Clear     SRDSOFIll      
.
          Clear     SRDSLSTNUM    
          Clear     SRDSOWNNUM    
          Clear     SRDSDATFUL    
          Clear     SRDSDATMAN    
          Sub       SRDSPRICE,SRDSPRICE
          Clear     SRDSNLSTCDE   
          Clear     SRDSHOTLINE   
          Clear     SRDSNEWDATE   
          Clear     SRDSREVDATE   
          Clear     SRDSPASSWORD  
          Clear     SRDSMLSTNAME  
          Clear     SRDSCLEANCDE  
          Clear     SRDSCLNINFO   
          Clear     SRDSNETNAME   
          Clear     SRDSNETINFO   
          Clear     SRDSDELCODE   
          Clear     SRDSSAMPLE    
          Clear     SRDSSEX       
          Clear     SRDSMIN       
          Clear     SRDSUNIVERSE  
          Clear     SRDSUNITDATA  
          Clear     SRDSNDATUPDDATE
          Clear     SRDSCOMMPER   
          Clear     SRDSNDATVerf  
          Clear     SRDSNdatMen   
          Clear     SRDSNdatFem   
          Clear     SRDSNDatFill  
          sub       Phonecount,Phonecount
          Clear     PhoneType
          Sub       INetcount,Inetcount
          Move      No,mgrflag
          Move      No,Baseflag
          Move      No,Selflag
          Move      Yes,Selflag1
          Move      "0001",SRDSSELNUM
          move      c0,n4
          Clear     SRDSSELEXC
          Move      No,Selflag2
          Move      Yes,Selflag2A
          cLEAR     SRDSSLTDES
          Clear     SRDSSLTDESC
          Clear     SRDSSLTLIST
          Clear     SRDSSLTType
          Clear     SRDSSLTNUM
          sub       SRDSSLTPRICE,SRDSSLTPRICE

          Clear     SRDSSELLIST  
          Clear     SRDSSELNUM   
          Clear     SRDSSELSNAME 
          Clear     SRDSSELQTY   
          Sub       SRDSSELPRICE,SRDSSELPRICE
          Sub       SRDSSELPRICEX,SRDSSELPRICEX
          Clear     SRDSSELPCOMM 
          Clear     SRDSSELDESC



          return
          
          include   srdsio.inc
          include   srdsLOio.inc
          include   SRDSSELio.inc
          include   SRDSSLtio.inc                             .selects,addressing, etc
          include   SRDSxSLtio.inc                             .xref selects,addressing, etc
          include   NREFio.inc                             
.begin patch 1.2          
          include   SRDSTXTio.inc                             .special pricing
.end patch 1.2          

          include   Comlogic.inc




