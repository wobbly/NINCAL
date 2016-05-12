..............................................................................
.
. COMMON
.
. COMMON UDA VARIABLES
.
$ERROR   DIM       *12                 DATASHARE ERROR MESSAGE
$PORTN   DIM       *2                  SOFT PORT NUMBER
$TODAY   DIM       *8                  DATE IN mm/dd/yy FORMAT
$JULIAN  FORM      *5                  TODAY IN yyjjj FORMAT
.
$USER    DIM       *10                 USER ID
$USERNME DIM       *10                 USER NAME (FIRST INIT, LAST)
$PRIO    FORM      *3                  OVERALL PRIORITY LEVEL
$LEVELS  DIM       *36                 SECURITY LEVELS
$COMM    DIM       *1                  COMMUNICATION ALLOWED
.
$COMPANY FORM      *1                  COMPANY CODE (1-NIN,2-CMP,3-DECK,4-DATA)
$COMPNME DIM       *24                 COMPANY NAME TEXT
$MULTCOS FORM      *1                  MULTIPLE COMPANIES ALLOWED
$CURSYS  FORM      *1                  CURRENT SYSTEM
$CURLEVL FORM      *1                  CURRENT LEVEL WITHIN SYSTEM
$MULTSYS FORM      *1                  MULTIPLE SYSTEMS ALLOWED
$PROGRAM DIM       *8                  LAST PROGRAM NAME
$FUNC    FORM      *1
$COMMENT DIM       *20
.
..............................................................................
