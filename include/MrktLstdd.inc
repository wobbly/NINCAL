;----------------------------------------------------------------
;
;   Mrktkstdd.inc -
;
;   Copyright(c) Names in the News Ca., Inc. 2000
;
.   Release         1.4                2010 January  5      DLH more flags
.   Release         1.3                2009                 DLH added company #      
;   release         1.2                2006 September 20    DLH - add Email
;   release         1.1                2005 June 18         DMB - Changed IP of File Manager
;   Author: David L Herrick
;   Created: 3/11/2002 12:53:46 PM
;   Last change: DLH 3/19/2002 8:14:38 AM
;----------------------------------------------------------------

.Master marketing file definitions   temp
.Primary key   company name
.Secondary Key  Contact
.Terciary Key - Isam - ID num
.>Patch 1.1 Begin
MrktFlist       Filelist
.MrktFile          Afile          fixed=286,uncomp,Name="MrktList.aam|20.20.30.103:502"
.MrktFIle2         IFile          key=6,fixed=286,uncomp,nodup,Name="MrktList.isi|20.20.30.103:502"
.MrktFile          Afile          fixed=286,uncomp,Name="MrktList.aam|NINS1:502"
.MrktFIle2         IFile          key=6,fixed=286,uncomp,nodup,Name="MrktList.isi|NINS1:502"
MrktFile          Afile          fixed=400,uncomp,Name="MrktList.aam|NINS1:502"
MrktFIle2         IFile          key=6,fixed=400,uncomp,nodup,Name="MrktList.isi|NINS1:502"
                FileListEnd
.>Patch 1.1 End                
MrktName        Dim            25
MrktName1       Init           "MrktList"

MrktVars        List
MrktCoName      Dim            50      1-50
MrktCntName     Dim            50     51-100       Contact Name
MrktAddr1       dim            50    101-150
MrktAddr2       dim            50    151-200
MrktCity        Dim            50    201-250
MrktState       Dim            2     251-252
MrktZip         Dim            9     253-261
MrktIdNum       dim            6     262-267
MrktDtacrd      Dim            1     268-268       Y=member of datacard list
MrktPromo       Dim            1     269-269       Y=member of Promo list
MrktHlydy       Dim            1     270-270       Y=Member of Holiday list
MrktNews        Dim            1     271-271       Y=Member of Newsletter list
MrktParty       Dim            1     272-272       Y=Member of Party List
MrktCode        dim            1     273-273       A= ,N= ,D= ,C=
MrktSLS         Dim            2     274-275       Nin Contact inits
.begin patch 1.4
MrktActive      Dim           1      276-276      Y=Member of Active List    
MrktPrsct       Dim           1      277-277      Y=Member of Prospect List    
MrktVndr        Dim           1      278-278      Y=Member of Vendor List    
MrktFill        Dim            9     279-286       not used
.MrktFill        Dim            11    276-286       not used
.end patch 1.4
MrktEmail     Dim          75    287-361        
.begin patch
MrktPhone     Dim          10    362-371        
MrktFax       Dim          10    372-381        
MrktBrkr      Dim          1     382-382
MrktComp      Dim   6            383-388            .company #
MrktCnt       Dim   3            389-391             .contact #
MrktFill2     Dim          9     392-400
.MrktFill2              Dim          18    383-400
.MrktFill2              Dim          19    382-400
.MrktFill2              Dim          49    382-400
.end patch
               Listend
.

.
MrktAkey1          dim            53
MrktAkey2          Dim            53
MrktFld            Dim             6                -Isam key - MrktIdNum
MrktFlag           Form            1                - = 1 If open

