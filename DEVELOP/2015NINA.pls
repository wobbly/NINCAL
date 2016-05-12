.2015NIN - read file of old mailer numbers, backup, clean databases so the numbers can be reused
pc         equ        0
           include    common.inc
           include    cons.inc
           include    compdd.inc
           include    cntdd.inc
           include    Nsmpdd.inc
           include    norddd.inc
           Include    nspedd.inc
           include    NoNodd.inc
           include    ninvdd.inc
           include    ninvacddd.inc
           include    nadjdd.inc
           include    njstdd.inc
           include    Nxrfdd.inc
           include    Nofrdd.inc
           include    Nmoadd.inc
           include    Nmobdd.inc
           include    Nescdd.inc
           include    Nbildd.inc
           include    Ncaldd.inc
           include    nshpdd.inc
              Include CompNotesdd.inc           
.other files to be checked
.version A cleanup offers only
.version Aa cleanup sample files only
SetAttrib     PROFILE           kernel32,SetFileAttributesA,INT1,PDIM,INT4
Attributes    integer           4
nullTerm      init                0x00
return        integer           1
FILE_ATTRIBUTE_NORMAL     Init          0x00000080
FILE_ATTRIBUTE_READONLY   Init          0x00000001

Release    init       "NOT"
Reldate    init       "2015 June 30"
Input      FIle       .comp#, Old mlr#, comp name
Comp       Dim        6
Delmlr       dim        4
desc       dim        50
FileName1 dim       250
count      form       2
CHANGE   FORM      7.2         CHANGE TO BE APPLIED TO BALANCE.
RecsIN     form       5
.output files
Ord2015    Ifile
smp2015    Ifile
smpDelFailed    Ifile

.Read input
.read order file if present - copy & delete
.          read secondary order files special instructions, ninord3, ninord4
.          ordernotes
.          read invoice file copy delete including additional charges, adjustment files, and Tinv
.          read shipping file
.          read merge file
.          datacard xref
.copy delete contacts if present
.copy delete offers if present
.copy delete samples if present
.copy delete 501c if present
.copy delete the company record
.           Prepare    ord2015,"\\nins1\e\data\text\ord2015.dat","\\nins1\e\data\index\ord2015.isi|nins1:502","6","502",exclusive
.           Prepare    SMP2015,"\\nins1\e\data\text\SMP2015.dat","\\nins1\e\data\index\SMP2015.isi|nins1:502","6","110",exclusive
           Prepare    SMPDelFailed,"\\nins1\e\data\text\SMP2015.dat","\\nins1\e\data\index\SMP2015.isi|nins1:502","6","110",exclusive
.           open       input,"\\nins1\e\data\CleanMlrs.dat|nins1:502",exclusive
           open       input,"\\nins1\d\data\olddata\2015\smp2015.dat|nins1:502",exclusive

Primary    Loop
.           Read       input,seq;*CdfOn,Comp,DelMLr,Desc
           Read       input,seq;Comp,NSMPNUM,desc
           until      over
.check order file(s)
              add     c1,RecsIn
.              Display *p10:10,"Working on: ",COmp,b1,Delmlr,b1,Desc," ## "
              Display *p10:10,"Working on: ",COmp,b1,NSMPNUM,b1,Desc," ## "
              Display *p10:12,"Records in ## ",Recsin
.               move   c2,nordpath
.           packkey    NORDFLD1 with "01X",Delmlr
.           call       Nordaim
.           if         not over
.                      call       ordclean
.
.                      loop
.                      call       NordKG
.                      until      over
.                      match      Delmlr,omlrnum     .double check
.                      if         equal
.                                 call OrdClean
.                                 endif
.                      repeat           
.          endif            

.cleanup compfile, cnt file, etc sample contact 501c
.company file
.           packkey    COMPFLD using comp
.           call       comptst
.           call       compdel
..contact file
.           packkey    CNCTFLD2 using "01X",comp
.CNTREAD    call       CNCtaim
.           goto       smpread if over
.           packkey    CNCTFLD from CNCTCODE,CNCTID
.           call       CNCTtst
.           if         not over
.           call       CNCTDEL
.           endif
.           goto       CNTread
..company notes
.           packkey               COMPNOTEFLD,comp
.           call                  COMPNOTETST
.           if         Not over
.           call       COMPNOTEDel
.           endif
..Sample file(s)
..write a list of samples to be deleted from web server
.SMPREAD    packkey    NSMPFLD1,"01X",comp
.           call       Nsmpaim
.           if         not over
.           write      SMp2015,Comp;NSMPVARS
.           call       Nsmpdel
           pack      FileName1,"\\nins1\e\data\samples\s",Comp,NSMPNUM,".pdf"
                    clear     taskname
                    append    FileName1,taskname
                    append    nullterm,taskname  
                    reset     taskname
                    MOVe       FILE_ATTRIBUTE_NORMAL,Attributes
                    winapi     SetAttrib giving RETURN using FileName1,Attributes


           erase     FileName1
           if         over
           write      SMpDelFailed,Comp;Filename1
           endif
.           goto       smpread
.           endif
..501c file
.           rep        zfill,comp
.           pack      FileName1,"\\nins1\e\data\501c\",COMP,"cert.pdf"
.           erase     FileName1
..xref
.           packkey    NXRFFLD2 using comp
.xrefread   call       nxrftst
.           if         not over
.           call       nxrfdel
.           goto       xrefread
.           else
.           endif
.offer file
.           packkey    nofrfld1 using "01X",Comp
.           rep        zfill,nofrfld1
.           Loop
.           call       nofrAim
.           Until      over
.           call       nofrDel
.           repeat
.MOA
.           move       c1,nmoapath
.           packkey    Nmoafld,DelMLR,"000"
.Moa        call       NmoaKey
.           if         not over
.           call       Nmoadel
.           goto       MOA
.           endif
..Mob           
.           move       c4,nmoapath
.           packkey    NMOAFLD4 from DelMLR,NMOABRK
.           call       Nmobkey
.           if         not over
.           call       Nmobdel           
.              endif
..Escrow  indes is mailer broker we only have mailer
.EscRead    packkey    NESCFLD,comp
.           call       Nesckey
.           if         not over
.           match      comp,nescmlr
.                      if         equal
.                      packkey    NESCFLD,NescMlr,Nescbrk
.                      call       NescKey
.                      call       NescDel
.                      endif
.           goto       EscREad
.           endif
..Billto           
.BillRead   
.           for        N3 from "0" to "999" by c1
.           move       N3,str3
.              rep     zfill,str3
.                                 For        n1 from "0" to "9" by c1
.                                 move       n1,str1
.                                 rep        zfill,str1
.                                 packkey   Nbilfld,Delmlr,str3,str1
.                                 call       Nbilkey
.                                 if         not over
.                              call       NbilDel
.                                 else
.                                 break
.                                 endif
.                                 repeat
.              repeat

....
..caller    
.CallRead   Packkey    NCALFLD2,"01X",comp
.           call       NcalAim
.           if         not over
.           call       NcalDel
.           goto       CallRead
.           endif

            Repeat                      .go get next mailer          
.
           Shutdown   "CLS"

........................................................................
.Ordclean - take care of copying and deleting the order
ordclean
           write      ord2015,olrn;ordvars
               move   c1,nordpath

           packkey    Nordfld using OLRN
           call       NordDel
           call       checkspec
           Call       CheckINv
           call       checkshp
           return
.checkspec - take care of special instructions and notes
checkspec
.take care of specila inst and notes here      
           packkey    NSPEFLD,olrn
           call       NspeKey
                      if         Not over
                      call       nspedel
                      endif
           packkey    NoNofld,olrn
           Loop
           call       NoNokey
                      Until over
                      call       nonodel
           Repeat                                 
           return
CheckInv
           packkey    Ninvfld,olrn
           call       Ninvkey
           return     if over                        .yeah - nada
                      call       ninvdel
                     CLEAR          NInvAcdfld
                     pack           NInvAcdFld from Invnum
                     call        NinVAcdTst
                     return      if over
           for       AcdRecCount,"1","15"
           packkey   NInvAcdFld from Invnum,AcdRecCount
           call       NinvAcdkey
                      return     if over
                      call       NInvAcdDEL 
           repeat
           call       checkadj
           return
.master adjustment
CheckAdj
           packkey     NADJFLD,olrn
           call       nadjtst
           if         not over 
           call       nadjdel
           call       checkjst
           endif
           return
.Detail adjustments
Checkjst   
           move       ASAMNUM,N2      .number of adjustments
           for        Count from c1 to N2 by c1
           move       count,str2
           packkey    njstfld using ASINVNO,str2
           rep        zfill,njstfld
           call       njsttst
           call       njstdel
           repeat
           return
.Chipping
Checkshp
           packkey     NshpFLD,olrn
           call       nshptst
           if         not over 
           call       nshpdel
           endif
           return
;.............................................................................
;
; ENTRY POINT : NOFRAIM   
; REQUIRED    : NOFRFld1
; RETURNED    : OFFER RECORD
; DESCRIPTION : AIM OFFER FILE READ
;               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
;
NOFRAIM
           BRANCH     NOFRFLAG,NOFR9
           CALL       NOFROPEN
NOFR9
           TRAP       IOMssg Giving Error if IO
           BRANCH     NOFRLOCK,NOFR9L,NOFR9R,NOFR9N

NOFR9L
           FILEPI     1;NOFRFLE2
           READ       NOFRFLE2,NOFRFld1;OFRVARS
           TRAPCLR    IO
           RETURN
NOFR9R 
           READLK     NOFRFLE2,NOFRFld1;OFRVARS
           TRAPCLR    IO
           RETURN
NOFR9N
           READ       NOFRFLE2,NOFRFld1;OFRVARS
           TRAPCLR    IO
           RETURN
;.............................................................................
..............................................................................
.
. ENTRY POINT : NOFRDEL
. REQUIRED    : 'NOFRFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
NOFRDEL  BRANCH    NOFRFLAG TO NOFR7
         CALL      NOFROPEN
NOFR7    trap      IOMssg giving Error if IO
.START PATCH 1.1 REPLACED LOGIC
.         FILEPI    1;NOFRFILE
.         DELETE    NOFRFILE,NOFRFLD
         FILEPI    1;NOFRFLIST
         DELETE    NOFRFLIST
.END PATCH 1.1 REPLACED LOGIC
         trapclr   IO
         RETURN
...............................................................................
.
. ENTRY POINT : NOFROPEN
. REQUIRED    : 'NOFRFLAG'
. RETURNED    : 'NOFRFLAG' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN OFFER FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
NOFROPEN TRAP      NOFRGONE IF IO
.START PATCH 1.1 REPLACED LOGIC
.         OPEN      NOFRFILE,NOFRNAME
         OPEN      NOFRFLIST
.END PATCH 1.1 REPLACED LOGIC
         TRAPCLR   IO
         MOVE      C1 TO NOFRFLAG
         RETURN
.
NOFRGONE MOVE      NOFRNAME TO FILENAME
         CALL      FILEGONE
.
..............................................................................

           
           include    compio.inc
           include    cntio.inc
           include    Nsmpio.inc
           include    nordio.inc
           Include    nspeio.inc
           Include    nonoio.inc
           include    ninvio.inc
           include    ninvacdio.inc
           include    nadjio.inc
           include    njstio.inc
           include    Nxrfio.inc
.           include    Nofrio.inc
           include    Nmoaio.inc
           include    Nmobio.inc
           include    Nescio.inc
           include    Nbilio.inc
           include    Ncalio.inc
              Include CompNotesio.inc           
           include    nshpio.inc
           include    comlogic.inc
