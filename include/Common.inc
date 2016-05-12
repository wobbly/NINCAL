.    Last change: DLH 6/11/2002 10:23:04 AM
..............................................................................
.
. COMMON
.
. COMMON UDA VARIABLES
.
ERROR    DIM       *35                 DATASHARE ERROR MESSAGE
TODAY    DIM       *8                  DATE IN mm/dd/yy FORMAT
SECURITY FORM      *1
FUNC     DIM       *2
TYPINIT  DIM       *2                 TYPIST INITIALS
PORTN    FORM       *3                  SOFT PORT NUMBER
AGENDAID DIM       *6                  AGENDA ID NUMBER
JULIAN  FORM      *5                  TODAY IN yyjjj FORMAT
USER    DIM       *10                 USER ID
USERNME DIM       *10                 USER NAME (FIRST INIT, LAST)
PRIO    FORM      *3                  OVERALL PRIORITY LEVEL
LEVELS  DIM       *36                 SECURITY LEVELS
COMM    DIM       *1                  COMMUNICATION ALLOWED
.
COMPANY FORM      *1                  COMPANY CODE (0,1-NIN,2-PL (03/27/2007)
COMPNME DIM       *24                 COMPANY NAME TEXT
MULTCOS FORM      *1                  MULTIPLE COMPANIES ALLOWED
CURSYS  FORM      *1                  CURRENT SYSTEM
CURLEVL FORM      *1                  CURRENT LEVEL WITHIN SYSTEM
MULTSYS FORM      *1                  MULTIPLE SYSTEMS ALLOWED
PROGRAM DIM       *8                  LAST PROGRAM NAME
COMMENT DIM       *30
INITS    DIM       *3                 USER INITIALS
EXIT     FORM      *2                 EXIT ALLOWED
INPNAME  DIM       *25                INPUT FILE NAME
OUTNAME  DIM       *25                 OUTFILE NAME
PRTNAME  DIM       *25                 PRINT FILE NAME (/PRT ASSUMED)
.path    DIM      *45                   use Path name ie \\nts0\c\data\text
.Subject  dim      *30
.
..............................................................................

