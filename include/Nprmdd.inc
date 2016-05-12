;----------------------------------------------------------------
;
;   Nprmdd.inc - 
;
;   Copyright(c) Names in the News Ca., Inc. 2000
;
;   Author: David L Herrick
;   Created: 3/11/2002 12:53:46 PM
;   Last change: DLH 3/13/2002 6:43:36 AM
;----------------------------------------------------------------

.Promo file definitions   temp
.Primary key   company name
.Secondary Key  Contact
.Terciary Key - Isam - ID num

PrmFlist       Filelist
PrmFile          Afile          fixed=125,uncomp,Name="FaxPromo.aam"
PrmFIle2         IFile          key=5,fixed=125,uncomp,Name="FaxPromo.isi"
                FileListEnd
PrmName        Dim            25
PrmName1       Init           "Promo"

PrmVars        List
prmPhNum       dim            20      1-20       phone in format Countrycode-areacode-xxx-xxxx   and 4 blanks
PRmCntName     Dim            35     21-55       Contact Name
PrmCoName      Dim            50     56-105
PrmIdNum       dim            6     106-111
PrmWeekly      Dim            1     112-112       Y=member of weekly list
PrmPromo       Dim            1     113-113       Y=member of Promo list
PrmBroker      dim            1     114-114       Y=Member of Broker List
PrmClient      Dim            1     115-115       Y=Member of Client/prospect list
PrmFill        Dim            10    116-125       not used
               Listend
.
PrmAkey1          dim            53
PrmAkey2          Dim            38
PrmFld            Dim             6                -Isam key - PrmIdNum
PrmFlag           Form            1                - = 1 If open

