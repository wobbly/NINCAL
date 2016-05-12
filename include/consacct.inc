..............................................................................
.
. CONSacct INCLUSION
.
. DEFINES ALL CONSTANT AND FREQUENTLY USED VARIABLES.
. for accounting programs specifically compute.inc
.
.Patch 1.68     JD  08Apr08   added 12781 Oglala Lakota to Rateten.
.Patch 1.67     JD  01Mar07   added 17865 Interfaith Alliance to rateten.
.Patch 1.66    DLH  12Jul06   Zero out Dawson & Epsilon Mailer vars
.patch #1.65    JD  08Jul05   Removed Unicef/Now escrow mailers.
.patch #1.64    JD  07Oct04   added # 21716 & 21703  to  Rateten.
.patch #1.63    JD  21May04   added # 21558/20998 to  Rateten.
.patch #1.62    JD  29Apr04   Removed Pop Conn 6476 from Rateten.
.patch #1.61    JD  13Apr04   Marine Mammal added to RateTen
.patch #1.60    JD  11Mar04   Mass Demo added to RateTen
.patch #1.59    DLH 15Dec03   add PFLAG tp RateTen
.patch #1.58    JD  18jul03   removed DNC from escmlrs.
.patch #1.57    JD  25jun03   removed AHS from rateten.
.patch #1.56    JD  18apr03   added Girls Inc epsimlrs/added DNC/NOW/PPFA escrmlrs.
.patch #1.55    JD  18Nov02   added DNC rateten var.
.patch #1.52    JD  04mar02   added covemlr for rebate.
.patch #1.51    JD  11DEC01   added qtybildn USED best quantity on Net orders.
.patch #1.5    DLH  08Jul01   rateten moved here
.patch #1.4     JD  13JUL01   removed american humane escrow.
.patch #1.3     DLH 23Jun00   move vars here from ninv0001.pls
.patch #1.2     DLH 15Oct99   new vars to breakout $ savings and fee on nets
.patch #1.1     DLH 07Apr99 - NININV Y2K & new COMpute.inc
.Patch #1.0     ASH 29Dec98 - NINORD Y2K, File expansion:  vars added for increase in Quantities
. D. HERRICK 11/10/94  Created
.
.begin Patch 1.2
grossbase       form    9.2
netsavins       form    9.2
netsavfee       form    9.2
.end Patch 1.2
.begin patch ninv0001 version 10.0
GrossBaseSR       form    9.2                       gross base on rental portion   (split rental)
GrossBaseSE       form    9.2                       gross base on Exchange portion   (split Exchange)
.end patch ninv0001 version 10.0
.Start Patch #1.0 - increased vars
.SELECT   FORM      7.2
.TAXES    FORM      7.2
.FORMAP2  FORM      7.2
.SAVEAP   FORM      7.2
.LRINC    FORM      7.2
.NININC   FORM      7.2
.AMOUNT   FORM      7.4
.AMOUNTX  FORM      7.4
.CANUSE   FORM      7.4            CANADIAN USE TAX
.GROSS    FORM      7.2
.SHIP     FORM      7.2
.CMPCOM   FORM      7.2
.BRKCOM   FORM      7.2
.AP       FORM      7.2
.FLOAMT   FORM      7.2
.SVECOM   FORM      7.4
.PREPAY   FORM      7.2
.FORMAR   FORM      7.2
.SVEACR   FORM      7.2
.ACAMT    FORM      7.2
...
SELECT   FORM      9.2
TAXES    FORM      9.2
PRICE    FORM      3.2  -not changed
PRICEx   FORM      3.2  -not changed
FORMAP2  FORM      9.2
FORMAP3  FORM      9.2
SAVEAP   FORM      9.2
LRINCX   FORM      9.2      recovered commission on net orders
LRINC    FORM      9.2
NININC   FORM      9.2
XNININC   FORM      9.2
AMOUNTr  FORM      7.4  -not changed
AMOUNT   FORM      9.4
AMOUNTx  FORM      9.4
CANUSE   FORM      9.4            CANADIAN USE TAX
GROSS    FORM      9.2
argh94   FORM      9.4       .dlh 09Aug99  for commission on gross names on a net order
Grossar  FORM      9.2      .DLH 22Jun99 for commission on gross names
.                           on a net order.
SHIP     FORM      9.2
POST     FORM      3.2  -not changed
CMPCOM   FORM      9.2
BRKCOM   FORM      9.2
AP       FORM      9.2
FLOAMT   FORM      9.2
SVECOM   FORM      9.4
PREPAY   FORM      9.2
FORMAR   FORM      9.2
SVEACR   FORM      9.2
.End Patch #1.0 - increased vars
.Start patch #1.0 - new vars added
FORM92   FORM      9.2
FORM93   FORM      9.3
FormNineTwo1 FORM  9.2
FormNineTwo2 FORM  9.2
FORM94   FORM      9.4
.End patch #1.0 - new vars added
.Start patch #1.0 - new vars to replace current vars
.I LEFT REPLACED VARS INTACT IN CASE THEY NEED TO BE USED ELSEWHERE.
.HOWEVER, NOTE THAT COMPUTE.INC NO LONGER USES THEM BUT INSTEAD
.USES THE FOLLOWING REPLACEMENTS!!!!
CMPT92   FORM      9.2          .replaces FORM72
CMPT94   FORM      9.4          .replaces FORM74
CMPT94X  FORM      9.4          .replaces FORM74X
CMPT94A  FORM      9.4          .replaces FORM74A
NET94    FORM      9.4          .replaces NET74
NET94N   FORM      9.4          .replaces NET74N
.End patch #1.0 - new vars to replace current vars
.begin Invoice patch 10.001
Form34        Form            3.4
.end Invoice patch 10.001
FORM32   FORM      3.2
FORM72   FORM      7.2
FORM72x  FORM      7.2          .split
FORM73   FORM      7.3
net74    FORM      7.4           .for net name orders
net74n   FORM      7.4           .for net name orders save field.
form74a  form      7.4
FORM74r  FORM      7.4
FORM74   FORM      7.4
FORM74x  FORM      7.4          .split
FORM82   FORM      8.2

CALCRUN   FORM     9.2
RUNAR    FORM      9.2      TOTAL ADDITIONAL TDMC RUNNING CHARGES AR.
RUNLR    FORM      9.2      TOTAL COMMISION FROM RUNNING CHARGES.
RUNPASS  FORM      9.2      TOTAL PASSED THROUGH TO TDMC.               
RUNFLAT  FORM      9.2       ALL NON-RUN CHARGES. IE MAG TAPE.
RUNRAR   FORM      9.2      TOTAL ADDITIONAL TDMC RUNNING CHARGES AR.
RUNRLR   FORM      9.2      TOTAL COMMISION FROM RUNNING CHARGES.
RUNRPASS FORM      9.2      TOTAL PASSED THROUGH TO TDMC.               
RUNRFLAT FORM      9.2      ALL NON-RUN CHARGES. IE MAG TAPE.
RUNRCNT  FORM      5
RUNFLAG   DIM      1         HOLDS STAR IF TDMC RUNNING CHARGES INVOLVED
RUNCOUNT FORM      5
netflag  form      1         1=gross order 2=net
.begin patch 1.1
.CODENUM  FORM      2
.ADDKEY   DIM       2
.ADDCODE  DIM       2
CODENUM  FORM      3
ADDKEY   DIM       3
ADDCODE  DIM       3
nacd1flag form     1         0=normal, 1=addcode '001' was encounterd
.end patch 1.1
.STR14    DIM       14          turned offset 14mar2001 jd conflicted with cons.inc
.Start Patch #1.0 - increased vars
.ACAMT    FORM      7.2
ACAMT    FORM      9.2
.end Patch #1.0 - increased vars
.begin patch 1.1
ordteflag DIM       1          yes = ORDER DATE after 7/1/99
NINVFRMFLAG FORM      1      1=MAILER, 2=mlr remitance, 3=OWNER/manager
NINVOUTFLAG FORM     1      0= no detail 1=PRT detail from compute  2=Display Detail
TOTREJ   FORM      8
.ADDITIONAL CHARGE DESCRIPTION.
.
addcd1   dim       3      used in ninv0001 for display
addcd2   dim       3
addcd3   dim       3
addcd4   dim       3
addcd5   dim       3
addcd6   dim       3
addcd7   dim       3
addcd8   dim       3
addcd9   dim       3
addcd10  dim       3
addncd1   dim       3      used in ninv0001 for display
addncd2   dim       3
addncd3   dim       3
addncd4   dim       3
addncd5   dim       3
addncd6   dim       3
addncd7   dim       3
addncd8   dim       3
addncd9   dim       3
addncd10  dim       3
.begin patch nadj0001 1.4
addAext1       Dim            1
addAext2       Dim            1
addAext3       Dim            1
addAext4       Dim            1
addAext5       Dim            1
addAext6       Dim            1
addAext7       Dim            1
addAext8       Dim            1
addAext9       Dim            1
addAext10      Dim            1
.end patch nadj0001 1.4
.addP1    dim       3      used in ninv0001 for display
.addP2    dim       3
.addP3    dim       3
.addP4    dim       3
.addP5    dim       3
.addP6    dim       3
.addP7    dim       3
.addP8    dim       3
.addP9    dim       3
.addP10   dim       3
addP1    dim       4      used in ninv0001 for display
addP2    dim       4
addP3    dim       4
addP4    dim       4
addP5    dim       4
addP6    dim       4
addP7    dim       4
addP8    dim       4
addP9    dim       4
addP10   dim       4
ADDESC1  DIM       35
ADDESC2  DIM       35
ADDESC3  DIM       35
ADDESC4  DIM       35
ADDESC5  DIM       35
ADDESC6  DIM       35
ADDESC7  DIM       35
ADDESC8  DIM       35
ADDESC9  DIM       35
ADDESC10 DIM       35
.
ATPRT    DIM       1
AT1      DIM       1      ADDITION CHARGE PRINT LINE VAR.
AT2      DIM       1      ADDITION CHARGE PRINT LINE VAR.
AT3      DIM       1      ADDITION CHARGE PRINT LINE VAR.
AT4      DIM       1      ADDITION CHARGE PRINT LINE VAR.
AT5      DIM       1      ADDITION CHARGE PRINT LINE VAR.
AT6      DIM       1      ADDITION CHARGE PRINT LINE VAR.
AT7      DIM       1      ADDITION CHARGE PRINT LINE VAR.
AT8      DIM       1      ADDITION CHARGE PRINT LINE VAR.
AT9      DIM       1      ADDITION CHARGE PRINT LINE VAR.
AT10     DIM       1      ADDITION CHARGE PRINT LINE VAR.
.
.begin patch
.qty for charge
addqty   DIM       9
addqty1  DIM       9
addqty2  DIM       9
addqty3  DIM       9
addqty4  DIM       9
addqty5  DIM       9
addqty6  DIM       9
addqty7  DIM       9
addqty8  DIM       9
addqty9  DIM       9
addqty10 DIM       9
.end patch
.ADDITIONAL CHARGE RATE.
.
ADD$RTE  DIM       6
ADD$RT1  DIM       6
ADD$RT2  DIM       6
ADD$RT3  DIM       6
ADD$RT4  DIM       6
ADD$RT5  DIM       6
ADD$RT6  DIM       6
ADD$RT7  DIM       6
ADD$RT8  DIM       6
ADD$RT9  DIM       6
ADD$RT10 DIM       6
.
. TOTAL ADDITIONAL CHARGE
.
ADD$1    DIM       15
ADD$2    DIM       15
ADD$3    DIM       15
ADD$4    DIM       15
ADD$5    DIM       15
ADD$6    DIM       15
ADD$7    DIM       15
ADD$8    DIM       15
ADD$9    DIM       15
ADD$10   DIM       15
.end patch 1.1.
ACCMPR   FORM      3.2
NewACCMPR   FORM      3.4        for new compute    03 June 2003 DLH
AEXTCD   DIM       1                       ;10Sep2003 dlh - for Nadj0001 release 1.4  
.AEXTCD  ---  if 1=use & an adjustment call go calc use original qtybild for calc, else use adj qty
ANINCD   FORM      1
THOUS    FORM      "1000"
HUND     FORM      "100"
INDEX    FORM      2
ppsw     dim       1
PREPAYSW DIM       1            =Y IF PREPAY $
CMREFLAG FORM      1            0=NO 1=YES
CMREDOLR FORM      9.12
fulhouse init      "TDMC-ANACAPA-EPSILON-PERLOWIN-LPS"
subppsw  dim       1             -if yes handle prepays.
TDMCFLAG FORM      1            2=DO TDMC CALCS.
.begin patch 1.3
.dawsmlrs init      "0036"
dawsmlrs  init     "0000"
covemlr  init      "0396"
EpsiMLrs      INit            "0000"
.epsimlrs init      "2702-0904-1762-1604-0024-6319-3083-3852-0093-4417-9374-1403-5759"
.epsimlrs init      "2702-0904-1762-1604-0024-6319-3083-3852-0093-4417-9374-1403"
.escrmlrs init      "1498-0774-0308-5838-1921-2661"
.escrmlrs init      "1498-0774-5838-1921-2661"
.escrmlrs init      "1498-0774-1921-0073-0071-0076"
.escrmlrs init      "1498-0774-1921-0073-0076-0055"
.begin patch 1.65
escrmlrs init      "1498-0774-1921-0076"
.begin patch 1.65
rebate   FORM      9.4
.end patch 1.3
.begin patch 1.5 moved to consacct.inc 8/8/01
.begin patch 1.59
.RATETEN  INIT      "005172-016291-016533-018246-018678-019534-018575-006476-004493-011947" $10 EXCHANGE MAN FEE LISTS
.RATETEN  INIT      "005172-016291-016533-018246-018678-019534-018575-006476-004493-011947-014841" $10 EXCHANGE MAN FEE LISTS
.RATETEN  INIT      "005172-016291-016533-018246-018678-019534-018575-006476-004493-011947-014841-020887" $10 EXCHANGE MAN FEE LISTS
.RATETEN  INIT      "005172-016291-016533-018246-018678-019534-018575-006476-004493-011947-014841-020887-011278" $10 EXCHANGE MAN FEE LISTS
.RATETEN  INIT      "005172-016291-016533-018246-018678-019534-018575-004493-011947-014841-020887-011278-021558-020998" $10 EXCHANGE MAN FEE LISTS
.begin patch 1.64
.RATETEN  INIT      "005172-016291-016533-018246-018678-019534-018575-004493-011947-014841-020887-011278-021558-020998-021716-021703" $10 EXCHANGE MAN FEE LISTS
.end patch 1.64
.begin patch 1.67
;RATETEN  INIT      "005172-016291-016533-018246-018678-019534-018575-004493-011947-014841-020887-011278-021558-020998-021716-021703-017865" $10 EXCHANGE MAN FEE LISTS
.end patch 1.67
.begin patch 1.68
RATETEN  INIT      "005172-016291-016533-018246-018678-019534-018575-004493-011947-014841-020887-011278-021558-020998-021716-021703-017865-012781" $10 EXCHANGE MAN FEE LISTS
.end patch 1.68
.end patch 1.59
qtybildn FORM      9
.added dec 2002   --- for JOse
chgrqty form      9
CALCPER  FORM      9
PER85    FORM      "000.85"
CHGQTY1 FORM      8
CHGQTY2 FORM      9
.begin adj patch 1.4
RunChrgTot     form           9.2
HoldRunChrg     form           9.2
OrigQtyBild    Form           9
QtyAdjFlag     Dim            1              .if yes qty adjutment routine is calling compute
FrcCompFlag    Dim            1              .IF set to "Y" force Recompute of previously billed order
.end adj patch 1.4
.added KCET  #004493          08/08/2001
..RATETEN  INIT      "005172-016291-016533-018246-018678-019534-018575-006476-014943" $10 EXCHANGE MAN FEE LISTS
.added Amer Humane  #14943          04/11/2001
.RATETEN  INIT      "005172-016291-016533-018246-018678-019534-018575-006476" $10 EXCHANGE MAN FEE LISTS
..5172 planned parenthood
..16291 PP Voter fund
..16533 PP High Dollar
..18246 PP Action Fund
..18678  High dollar democrats master
..19534  La Raza
.added ZPG #18575 & 6476          02/02/2000
.added La Raza #19534 11/30/99
.removed UCS Sep 99 DLH
.RATETEN  INIT      "005172-016291-016533-018246-018678-005670" $10 EXCHANGE MAN FEE LISTS
.JUne 99 KCET rate reduced to $2 per SMM
.RATETEN  INIT      "004493-005172-016291-016533-018246-018678-005670" $10 EXCHANGE MAN FEE LISTS
.                   PP, KCET,  UCS 9/1/98
.ncjw #015445 removed from list 9/24/98 DLH per SMM
.end patch 1.5
