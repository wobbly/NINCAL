PC            EQU             0

              INCLUDE         COMMON.inc
              INCLUDE         CONS.inc
              INCLUDE         NORDDD.inc
              Include         Compdd.inc
              include         cntdd.inc
              INCLUDE         NSHPDD.inc
              Include         CDXFDD.INC
              Include         Ndatdd.inc
              include         NSel2dd.inc
              Include         DMEXDD.inc
              include         Media.inc

.
...................................................................................................................
.               "DMX SHIP INFO" TO STITLE
...................................................................................................................
release       init            "1.06"           11May2006 DLH Clean Name replace " with '
;release       init            "1.05"           27Apr2006 DLH more changes in data DMExchange Data structure
;release       init            "1.04"           27Mar2006 DLH change in data DMExchange Data structure
;release       init            "1.03"           26JAN2006 DMB added code for data folder restructure
.release       init            "1.02"           21Nov2005 DLH add Clientlogic
;release      init            "1.01"           16Aug2005 DLH Small patch to prevent uploading miskeys on Millard orders
;release      init            "1.0"           18May2005 DLH New - Create DMEXCHANGE SHIPPING FILES
.
input         file
TIME          DIM             8
DATE          DIM             8              'MM/DD/YY'.
copycmd       init            "y:command.com /C copy /b/y "
qtymsk        dim             11
Logfile       file            .created at start of  job, delete if job finishes ok.
SHPFAX        FILE            .SUBSET OF ORDER FILE REPRESENTING NEW SHIPPING INFO ON
.                  LIST MANAGEMENT ORDERS.
Output1       File
Output2       FIle
Output3       File
;begin patch 1.02
Output4        FIle
count4        form            5
;end patch 1.02
HR            dim             2
Minute        Dim             2
Sec           Dim             2
.STR255        Dim             255
Count1        form            5
count2        form            5
count3        form            5
.Patch 1.03
.FPath1        Init            "\\nins1\e\data\DMEXchange\"
FPath1        Init            "\\nins1\e\storage\DMEXchange\"
.FPath2        Init            "\\nins1\e\data\DMEXchange\sent\"
FPath2        Init            "\\nins1\e\storage\DMEXchange\sent\"
.Patch 1.03
FName         Dim             150
;
...............................................................................
.
              MOVE            "NDMX001A" TO PROGRAM
              MOVE            "NAMES IN THE NEWS" TO COMPNME
              MOVE            "DMX SHIP INFO" TO STITLE
              MOVE            "EXIT" TO PF5
              TRAP            ABORT IF F5
              CALL            PAINT
              CALL            FUNCDISP
              move            c1 to nmlrpath
              move            c1 to nbrkpath
              call            GetWinVer
.Load some DMX Fields now!!
.NIN Contact
              Move            "List Management" to DMcnt
.NIN Phone Number
              Move            "(415)989-3350" to DMPhone
.NIN Email
              Move            "ListRequest@nincal.com" to DmEmail
.Transmission Date
              clock           timestamp to STR25
              unpack          STR25 into str4,mm,dd,hr,minute,sec
              Pack            DMTDate from mm,slash,dd,slash,str4,b1,hr,":",Minute,":",sec
.
              Pack            str8 from str4,mm,dd
              rep             zfill in str8
;need to create file names here - looking for dupes and increment the last 2 bytes if nec.
              Move            "01" to str2
              move            c0 to n2
File1
              add             c1 to n2
              Move            n2 to str2
              rep             zfill in str2
              Trap            File1aio if IO
              Pack            Fname from "nin_alc_",str8,"_shp_",str2,".dat"
              Pack            str255 from Fpath1,Fname
              Open            output1,str255
              close           output1                                ;oops it is here increment counter
              goto            file1
file1a
              Pack            str255 from Fpath2,Fname
              Open            output1,str255
              close           output1                                ;oops it is here increment counter
              goto            file1
File1b
              Pack            str255 from Fpath1,Fname
              Prepare         Output1,str255,exclusive
              Move            "01" to str2
              move            c0 to n2
File2
              add             c1 to n2
              Move            n2 to str2
              rep             zfill in str2
              Trap            File2aio if IO
              Pack            Fname from "nin_mgi_",str8,"_shp_",str2,".dat"
              Pack            str255 from Fpath1,Fname
              Open            output2,str255
              close           output2                                ;oops it is here increment counter
              goto            file2
file2a
              Pack            str255 from Fpath2,Fname
              Open            output2,str255
              close           output2                                ;oops it is here increment counter
              goto            file2
File2b
              Pack            str255 from Fpath1,Fname
              Prepare         Output2,Str255,exclusive
              move            c0 to n2
File3
              add             c1 to n2
              Move            n2 to str2
              rep             zfill in str2
              Trap            File3aio if IO
              Pack            Fname from "nin_dmi_",str8,"_shp_",str2,".dat"
              Pack            str255 from Fpath1,Fname
              Open            output3,str255
              close           output3                                ;oops it is here increment counter
              goto            file3
file3a
              Pack            str255 from Fpath2,Fname
              Open            output3,str255
              close           output3                                ;oops it is here increment counter
              goto            file3
              move            c1 to n2
;             pack            str255 from "\\nins1\e\data\DMexchange\nin_dmi_",str8,"_shp_",str2,".dat"
file3b
              Pack            str255 from Fpath1,Fname
              Prepare         Output3,STR255,exclusive
;begin patch 1.02
              trapclr         io
              Move            c0 to n2
File4
              add             c1 to n2
              Move            n2 to str2
              rep             zfill in str2
              Trap            File4aio if IO
              Pack            Fname from "nin_clg_",str8,"_shp_",str2,".dat"
              Pack            str255 from Fpath1,Fname
              Open            output4,str255
              close           output4                                ;oops it is here increment counter
              goto            file3
file4a
              Pack            str255 from Fpath2,Fname
              Open            output4,str255
              close           output4                                ;oops it is here increment counter
              goto            file4
              move            c1 to n2
file4b
              Pack            str255 from Fpath1,Fname
              Prepare         Output4,STR255,exclusive
              trapclr         io
;end patch 1.02

              PACK            STR35,NTWKPATH1,"shipfax.dat"
              open            input,STR35,read
              PACK            STR35,NTWKPATH1,"shipDMX.log"
              prepare         logfile,STR35
              write           logfile,seq;"If this file is here NDMX001A is running or bombed"
              weof            logfile,seq
              close           logfile
.
READ
              clear           olrn
              read            input,seq;ordvars
              GOTO EOJ IF OVER
              ADD             C1 TO N4
              DISPLAY         *P10:12,"NUMBER OF ORDERS READ : ",N4
              PACK            MKEY FROM OMLRNUM,Z3
              move            "NMLRKEY",Location
              pack            KeyLocation,"Key: ",MKEY
              CALL            NMLRKEY
              pack            nbrkfld from obrknum,obrkcnt
              move            "nbrkkey",Location
              pack            KeyLocation,"Key: ",nbrkfld
              call            nbrkkey
              PackKey         CDXFFLD from COMPNUM
              move            "CDXFKey",Location
              pack            KeyLocation,"Key: ",CDXFFLD
              Call            CDXFKey                     ;dmexchange vendor ?
.
              goto read if over                ;Nope
              if (CDXFXType <> 2)            ;dmexchange vendor ?
               goto           read                          ;nope
              endif
.
              call            Trim using CDXFComp
;OK sigh multiple companies for same vendor so for now    3 passes 1 ALC 2 MIllard 3 DM
              MOVE            OLRN TO NSHPFLD
              match           "      " to olrn
              goto read if equal
              match           "      " to olrn
              goto read if eos
              type            olrn
              goto read if not equal
              rep             zfill in nshpfld
              move            "NSHPKEY",Location
              pack            KeyLocation,"Key: ",nshpfld
              CALL            NSHPKEY
              GOTO error IF OVER
              packkey         NSEL2FLD,"1",OLRN
              move            "NSEL2KEY",Location
              pack            KeyLocation,"Key: ",NSEL2FLD
              call            NSEL2KEY
              if over
               move           O2DES,NSEL2NAME
              endif
              move            c1 to nDATpath
              PACKKEY         nDATFLD from olnum
              move            "Ndatkey",Location
              pack            KeyLocation,"Key: ",nDATFLD
              call            Ndatkey
.Load DMX Variables
.Record Type
              move            "SHP",DMREC
.List Name
              Clear           DMLname
              pack            DMLname from o1des
.begin patch 1.06
              rep             "#"'",DMLname
.end patch 1.06
              call            trim using dmlname
.Mailer Name
              clear           DMMname
              pack            DMmname from compcomp
              call            trim using dmmname
.Broker Order # - P.O.
              clear           DMPo
              pack            DMPO from omlrpon
;begin patch 1.01
              If              (CDXFCOMP = "MGI")
                              unpack         dmpo into str2,str50
                                             rep            "0O" in str2
                                             clear          dmpo
                                             pack           DMPO from str2,str50          
                               ;if the 1st two bytes are  T zero its wrong for millard should always be  T O "oh"
              endif                                         
;end patch 1.01
              call            trim using dmPO
.Shipped QTY
.             Move            Squant to DMqshp
              move            C0,N8
              rep             zfill,Squant
              move            Squant,N8
              Move            N8,DMqshp
              call            trim using DMQshp
.Shipped Via
              Clear           DMVIA
              pack            DMVIA from sinfo
              call            trim using dmVia
.Shipped Date
              UNpack          Sdate into str4,mm,dd
              pack            DMSdate  from mm,slash,dd,slash,str4
.Shipped Tracking Number - Airbill Number
              clear           DMAirb
              Pack            DMAirB from strack
              call            trim using dmairb
.Media
              clear           MEDIA
              call            Trim using OFOCODE
              if (OFOCODE <> "")
               move           C0,N2
               move           OFOCODE,N2
               move           MED0,MEDIA
               load           MEDIA FROM N2 OF MED1,MED2,MED3,MED4,MED5:
                              MED6,MED7,MED8,MED9,MED10,MED11,MED12,MED13,MED14:
                              MED15,MED16,MED17,MED18,MED19,MED20,MED21,MED22:
                              MED23,MED24,MED25,med26,med27,med28,med29
              endif
              Clear           DMMedia
              Pack            DMMedia from media
              call            trim using dmMedia
.Volume Serial #
              clear           DMSer
.Shipping Charge
              clear           DMChrg
.Mail Date
              clear           DMMdate
              call            Trim using omdtem
              if (omdtem <> "")
               pack           DMMdate from omdtem,slash,omdted,slash,omdtec,omdtey
              endif
.Select
              clear           DMSel
              pack            DMSEL from Nsel2name
              call            trim using dmSel
.Order Quantity
.             Move            Oqty to DMOqty
              move            C0,N8
              rep             zfill,OQTY
              move            OQTY,N8
              Move            N8,DMOqty
              call            trim using DMOQty
.NIN LR Number
              pack            DMLR from olrn
.Order Date
              clear           DMODate
              pack            DMOdate from oodtem,slash,oodted,slash,oodtec,oodtey
.
              IF              (CDXFComp = "ALC")
               goto           WRite1
              ElseIf          (CDXFCOMP = "MGI")
               goto           WRite2
              ElseIf          (CDXFCOMP = "DMI")
               goto           Write3
;begin patch 1.02
              ElseIf          (CDXFCOMP = "CLG")
               goto           Write3
;end patch 1.02
              endif
.
              goto            read
.
WRite1
;begin patch 1.04
;              Write           Output1,seq;*CDFON,*ll,Dmvars
              Write           Output1,seq;*ll,Dmrec,",":
;begin patch 1.05
;                              *CDFON,*ll,DmtDate:
;                              DMLname:
;                              DMMName:
;                              DMPO:
;                              DMQshp:        
;                              DMVia:         
;                              DMSdate:       
;                              DMAirB:        
;                              DMMedia:       
;                              DMSer:         
;                              DMChrg:        
;                              DMMdate:       
;                              DMSEl:         
;                              DMOqty:        
;                              DMLR:          
;                              DMODate:       
;                              DMcnt:         
;                              DMPhone:       
;                              DMEmail       
;end patch 1.04
                              DmtDate,",":
                              "#"",DMLname,"#",":
                              "#"",DMMName,"#",":
                              "#"",DMPO,"#",":
                              DMQshp,",":        
                              "#"",DMVia,"#",":
                              DMSdate,",":       
                              "#"",DMAirB,"#",":
                              "#"",DMMedia,"#",":
                              "#"",DMSer,"#",":
                              DMChrg,",":        
                              DMMdate,",":       
                              "#"",DMSEl,"#",":
                              DMOqty,",":        
                              "#"",DMLR,"#",":
                              DMODate,",":       
                              "#"",DMcnt,"#",":
                              "#"",DMPhone,"#",":
                              "#"",DMEmail,"#""       
;end patch 1.05                              
              Add             c1 to count1
              goto read
WRite2
;begin patch 1.04
;              Write           Output2,seq;*CDFON,*ll,Dmvars
              Write           Output2,seq;*ll,Dmrec,",":
;begin patch 1.05
;                              *CDFON,*ll,DmtDate:
;                              DMLname:
;                              DMMName:
;                              DMPO:
;                              DMQshp:        
;                              DMVia:         
;                              DMSdate:       
;                              DMAirB:        
;                              DMMedia:       
;                              DMSer:         
;                              DMChrg:        
;                              DMMdate:       
;                              DMSEl:         
;                              DMOqty:        
;                              DMLR:          
;                              DMODate:       
;                              DMcnt:         
;                              DMPhone:       
;                              DMEmail       
;end patch 1.04
                              DmtDate,",":
                              "#"",DMLname,"#",":
                              "#"",DMMName,"#",":
                              "#"",DMPO,"#",":
                              DMQshp,",":        
                              "#"",DMVia,"#",":
                              DMSdate,",":       
                              "#"",DMAirB,"#",":
                              "#"",DMMedia,"#",":
                              "#"",DMSer,"#",":
                              DMChrg,",":        
                              DMMdate,",":       
                              "#"",DMSEl,"#",":
                              DMOqty,",":        
                              "#"",DMLR,"#",":
                              DMODate,",":       
                              "#"",DMcnt,"#",":
                              "#"",DMPhone,"#",":
                              "#"",DMEmail,"#""       
;end patch 1.05                              
              Add             c1 to count2
.
              goto read
WRite3
;begin patch 1.04
;              Write           Output3,seq;*CDFON,*ll,Dmvars
              Write           Output3,seq;*ll,Dmrec,",":
;begin patch 1.05
;                              *CDFON,*ll,DmtDate:
;                              DMLname:
;                              DMMName:
;                              DMPO:
;                              DMQshp:        
;                              DMVia:         
;                              DMSdate:       
;                              DMAirB:        
;                              DMMedia:       
;                              DMSer:         
;                              DMChrg:        
;                              DMMdate:       
;                              DMSEl:         
;                              DMOqty:        
;                              DMLR:          
;                              DMODate:       
;                              DMcnt:         
;                              DMPhone:       
;                              DMEmail       
;end patch 1.04
                              DmtDate,",":
                              "#"",DMLname,"#",":
                              "#"",DMMName,"#",":
                              "#"",DMPO,"#",":
                              DMQshp,",":        
                              "#"",DMVia,"#",":
                              DMSdate,",":       
                              "#"",DMAirB,"#",":
                              "#"",DMMedia,"#",":
                              "#"",DMSer,"#",":
                              DMChrg,",":        
                              DMMdate,",":       
                              "#"",DMSEl,"#",":
                              DMOqty,",":        
                              "#"",DMLR,"#",":
                              DMODate,",":       
                              "#"",DMcnt,"#",":
                              "#"",DMPhone,"#",":
                              "#"",DMEmail,"#""       
;end patch 1.05                              
              Add             c1 to count3
              goto read
;begin patch 1.02
WRite4
;begin patch 1.04
;              Write           Output4,seq;*CDFON,*ll,Dmvars
              Write           Output4,seq;*ll,Dmrec,",":
;begin patch 1.05
;                              *CDFON,*ll,DmtDate:
;                              DMLname:
;                              DMMName:
;                              DMPO:
;                              DMQshp:        
;                              DMVia:         
;                              DMSdate:       
;                              DMAirB:        
;                              DMMedia:       
;                              DMSer:         
;                              DMChrg:        
;                              DMMdate:       
;                              DMSEl:         
;                              DMOqty:        
;                              DMLR:          
;                              DMODate:       
;                              DMcnt:         
;                              DMPhone:       
;                              DMEmail       
;end patch 1.04
                              DmtDate,",":
                              "#"",DMLname,"#",":
                              "#"",DMMName,"#",":
                              "#"",DMPO,"#",":
                              DMQshp,",":        
                              "#"",DMVia,"#",":
                              DMSdate,",":       
                              "#"",DMAirB,"#",":
                              "#"",DMMedia,"#",":
                              "#"",DMSer,"#",":
                              DMChrg,",":        
                              DMMdate,",":       
                              "#"",DMSEl,"#",":
                              DMOqty,",":        
                              "#"",DMLR,"#",":
                              DMODate,",":       
                              "#"",DMcnt,"#",":
                              "#"",DMPhone,"#",":
                              "#"",DMEmail,"#""       
;end patch 1.05                              
              Add             c1 to count4
              goto read
;end patch 1.02
              
;.....................................................................................................................................................

ABORT
              DISPLAY   *P1:24,*EL,*HON,"JOB ABORTED BY OPERATOR",*B,*W;
              shutdown
              stop
error
              DISPLAY   *P1:24,*EL,*HON,"can't find shipping record"
              goto read
.
EOJ
              If (Count1 >= C1)
               WEOF           output1,seq
              endif
              Close           Output1
              If (Count2 >= C1)
               WEOF           output2,seq
              endif
              Close           Output2
              If (Count3 >= C1)
               WEOF           output3,seq
              endif
              Close           Output3
.
;begin patch 1.02
              If (Count4 >= C1)
               WEOF           output4,seq
              endif
              Close           Output4
;end patch 1.02
              
             If (osflag = c3 | osflag = c4)
;              execute  "!c:\command.com /C f:\apps\winbatch\senderr.exe"
;              PACK     TASKNAME,"!c:\command.com /C copy ",NTWKPATH1,"shipfax.lst \\nts0\laser6"
;              execute  TASKNAME
              ElseIf (osflag = c1 | osflag = c5)
;              execute  "!c:\winnt\system32\cmd.exe /C f:\apps\winbatch\senderr.exe"
;              PACK     TASKNAME,"!c:\winnt\system32\cmd.exe /C copy ",NTWKPATH1,"shipfax.lst \\nts0\laser6"
;              execute  TASKNAME
              ElseIf (osflag = c6)
;              execute  "!c:\windows\system32\cmd.exe /C f:\apps\winbatch\senderr.exe"
;              PACK     TASKNAME,"!c:\windows\system32\cmd.exe /C copy ",NTWKPATH1,"shipfax.lst \\nts0\laser6"
;              execute  TASKNAME
              endif
              display    *p2:23,*el,"Job Done",*w3
              PACK       STR35,NTWKPATH1,"shipDMX.log"
              erase      str35
              shutdown
              stop

BADLR
              COMPARE         C1 TO N4   1ST REC?
              IF EQUAL
               MOVE           C0 TO N4
              ENDIF
.
              GOTO READ
File1aIO
              Trapclr         IO
              noreturn
              trap            file1bio if io
              goto File1a
File2aIO
              Trapclr         IO
              noreturn
              trap            file2bio if io
              goto File2a
File3aIO
              Trapclr         IO
              noreturn
              trap            file3bio if io
              goto File3a
File4aIO
              Trapclr         IO
              noreturn
              trap            file4bio if io
              goto            File4a
File1bIO
              Trapclr         IO
              noreturn
              goto            File1b
File2bIO
              Trapclr         IO
              noreturn
              goto File2b
File3bIO
              Trapclr         IO
              noreturn
              goto File3b
File4bIO
              Trapclr         IO
              noreturn
              goto            File4b

              INCLUDE         NORDIO.inc
              INCLUDE         NSHPIO.inc
              Include         CDXFIO.INC
              Include         Compio.inc
              include         cntio.inc
              Include         Ndatio.inc
              include         NSel2io.inc
              INCLUDE         COMLOGIC.inc
