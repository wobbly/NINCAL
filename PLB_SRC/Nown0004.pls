........................................................................
.output exclusive owners only to print labels via nown0002
.......................................................................
.
PC       EQU       0
         INCLUDE   COMMON.INC
         INCLUDE   CONS.INC
         INCLUDE   NOWNDD.INC
         INCLUDE   NDATDD.INC
         include   nusedd.inc
release  init      "1.1"        ASH 02OCT2000 NEW SERVER ADDED
.                                              REMOVED DUPE FROM CONS.INC
.release  init      "Not!"
.......................................................................
.
NEWMST   IFILE     keylen=6,var=3002,comp,dup  
FORMFILE FILE      UNCOMP
output   ifile     keylen=4
input    file     var=3002,comp
. ............................................................................
.
.FILENAME DIM       12        file name being openned
NEWNAME  DIM       18        file name of output file
SAVENAME DIM       18
recname  dim       50
A        INIT      "A"
R        INIT      "R"
CO       DIM       1         company N/C
PICK     DIM       1         keyed in pick-off choice
NUMPICK  FORM      1         numeric version of above
WITHDRAW FORM      1         withdrawn branch flag, 0=don't include them
SORT     FORM      1         SORT BRANCH FLAG 0=DEFAULT ALPHA CARD STYLE,
.                            1=NO SORT, 2=RH style only, 3=RH & Card style.
.                            4=BLANK STOCK SORTED, 5=BLANK NO SORT, 9-NO PRINT
CARD     INIT      ",DATA"
CHAIN    INIT      "CHAIN "
UTIL     INIT      "UTIL;"
IN       INIT      ",IN="
KILL     INIT      ",KILL=Y"
RH       INIT      ",DATARH"
.START PATCH 1.1 REMOVED DUPE VAR
.COMMA    INIT      ","
.END PATCH 1.1 REMOVED DUPE VAR
copy     dim       3        number of copies
H1       FORM      2         horizontal screen coordinate
H2       FORM      2                     "
H3       FORM      2                     "
H4       FORM      2                     "
H5       FORM      2                     "
FRLSNO   DIM       6         keyed in low list number range
TOLSNO   DIM       6         keyed in high list number range
CHECK1   FORM      6         numeric version of list number low range
CHECK2   FORM      6         numeric version of list number high number
FOUND    FORM      "    0"   records that matched criteria
TOTAL    FORM      5         total records read for a pick-off
ONAME    DIM       25        owner name from the owner file
OCOMP    DIM       25        owner company from the owner file
LISTOWN  DIM       70        chosen list owners seperated by a blank
LISTBRCH FORM      1         used for coordinate and string changes
LIST     DIM       96        current list range string being worked on
LIST1ST  DIM       96        list number ranges
LIST2ND  DIM       96               "
LIST3RD  DIM       96               "
LIST4TH  DIM       96               "
ANDOR    DIM       1         keyed in A or O for category pick-off
ANDORTXT DIM       3         for category pick-off contains 'and'/'or'
CATPICK  DIM       30        chosen categories
CATOMIT  DIM       15        chosen category omissions
OMITBRCH FORM      1         if category omits were requested,0=yes,1=no
CATMASK  INIT      "XXX-XXX-XXX-XXX-XXX-XXX-XXX-XXX-XXX-XXX"
CATS     DIM       39        above mask breaks up 'category' field for scan
DESC20   DIM       20        category description retrieved from catmst
EXCLCODE DIM       1         exclusive type requested N/C/B
REVPICK  DIM       112       chosen dates
REVPICK2 DIM       112       chosen dates only when mm/dd/yy is used
REVCOUNT FORM      2         screen line counter
YEAR     DIM       2         keyed in year and year from datmst
MO       DIM       2         keyed in month and month from datmst
MOYR     DIM       4         month & year from datmst
DAY      DIM       2         keyed in day and day from datmst
DATE     DIM       8         mm/dd/yy appended to chosen string
REVBRCH  FORM      1         used to determine if/how dates should
.                             be checked. 0=no,1=yy,2=mmyy,3=mmddyy
MLRBRCH  FORM      1         check mlr/offer omits,0=no,1=no,2=mlr,3=ofr
MLR      DIM       4         keyed in mailer number
MLROMIT  DIM       35        chosen mlr/offer omissions seperated by ' '
.                            maximum of seven mailers.
MLRPAGE  FORM      1         used during mlr omit, if more than 5 mlrs.
YR       DIM       2
LODATE   FORM      5         julian low order date   (yyjjj)
HIDATE   FORM      5         julian high order date  (yyjjj)
ORDJUL   FORM      5         julian order date from ordmst
.
.
DATA     INIT      "DATA"
.T        INIT      "T"
.SLASH    INIT      "/"
.ZIP3     INIT      "000"
.ZERO     FORM      "0"
.ONE      FORM      "1"
.N1       FORM      1
.
         IFNZ      PC
DR       INIT      ":PRINT"         FORCE OUTPUT FILES TO BE BUILD
TXT      INIT      "/TEXT"
         XIF
         IFZ       PC
.START PATCH 1.1 REPLACED LOGIC
.DR       INIT      "g:\DATA\"         FORCE OUTPUT FILES TO BE BUILD
DR       DIM       35
         MOVE      NTWKPATH1,DR         FORCE OUTPUT FILES TO BE BUILD
.END PATCH 1.1 REPLACED LOGIC
TXT      INIT      ".DAT"
         XIF
SHORTNME DIM       6
OKEY1    INIT      "01R"
OKEY2    INIT      "02R"
WITHCODE DIM       1
NEWMFLAG FORM      1               0=OUTPUT FILE CLOSED 1=OUTPUT FILE OPEN
repflag  form      1               report type
rhflag   form      1               1=no rh 2=yes rh as secondary rep.
repstyle dim       25
rep01    init      "Cardstock"
rep02    init      "RH Style listing"
rep03    init      "Blankstock"
rep04    init      "Fax Cards"
rep05    init      "Laser Cards"
rep06    init      "No Print"
faxtele  dim       10
faxname  dim       25
LONGDIST DIM       1
subject  dim       50
fline1   dim       50
fline2   dim       50
fline3   dim       50
fline4   dim       50
fline5   dim       50
fline6   dim       50
fline7   dim       50
fline8   dim       50
fline9   dim       50
fline10  dim       50
fline11  dim       50
fline12  dim       50
fline13  dim       50
fline14  dim       50
fline15  dim       50
fline16  dim       50
fline17  dim       50
fline18  dim       50
fline19  dim       50
fline20  dim       50
fline99  dim       50
blincnt form       1              blank line counter allow several in a row.
.PHONE VAR'S
telflag  form      1                  access number by 1=brk 2= mlr
ARCD     DIM       3                   AREA CODE
EXCH     DIM       3                   EXCHANGE
TELE     DIM       4                   TELEPHONE#
.for list help
wsw      dim       1              withdrawn flag
keycount form      2              number of keyed in characters for search
akey1    dim       3
ques     init      "??????"
formflag form      1              2=fax form file has been created
.
+
.START    KEYIN     *ES,*P11:1,"*** D A T A   C A R D   R E T R I E V A L   ":
.                   "P R O G R A M ***":
START    MOVE       "NDAT0999" TO PROGRAM
         MOVE       "NINCAL" TO COMPNME
         MOVE       "DATACARD lo  RETRIEVAL" TO STITLE
         CALL       PAINT
         CALL       FUNCDISP
         MOVE       C1 TO NDATPATH
.
.
CHOICE   CALL       PAINT
         KEYIN     *P15:6,"input file,",str10
         open      input,str10
.START PATCH 1.1 REPLACED LOGIC
.         prepare   output,"g:\data\exclowns","g:\data\exclowns","4","190"
         PACK   STR35,NTWKPATH1,"EXCLOWNS"
         PACK   STR45,NTWKPATH1,"EXCLOWNS"
         prepare   output,STR35,STR45,"4","190"
.END PATCH 1.1 REPLACED LOGIC
.
looper   read      input,seq;datvars
         goto      stop if over
         unpack	OWNNUM,str2,str4
         move       str4 to nownfld
         rep        zfill in nownfld
         read       output,nownfld;;
         goto       looper if not over
         call       nownkey
         write      output,nownfld;ownvars
         goto       looper
stop     stop
         INCLUDE   NOWNIO.inc
         INCLUDE   NDATIO.inc
         include   nuseio.inc
         INCLUDE   COMLOGIC.inc

