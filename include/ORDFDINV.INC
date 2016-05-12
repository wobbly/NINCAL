. *****************************************************************************
. 
.     FILE:   NININV/CMPINV
.   LENGTH:   256
. COMPRESS:   NONE
.     TYPE:   ISAM
.      KEY:   ILR
. 
. *****************************************************************************
. 
INVOICE  IFILE     KEYL=6
. 
IKEY     DIM       6
. 
ICD      DIM       1                   CODE
IST      DIM       1                   STATUS
IMN      DIM       4                   MAILER NUMBER
ILR      DIM       6                   LR NUMBER
IBT      DIM       1                   BILL-TO
IPT      DIM       1                   PAY-TO
ILI      DIM       6                   LIST OWNER INVOICE NUMBER
IP1      FORM      7                   LIST OWNER AP
ICP      FORM      2                   COMMISSION PERCENT
IPC      DIM       1                   PAYABLE CODE
IQT      FORM      7                   QUANTITY SHIPPED
IPR      FORM      5                   PROCE/M
ICT      DIM       3                   CONTACT NUMBER
IIC      DIM       1                   INCOME CODE
ILO      DIM       4                   LIST OWNER NUMBER
IN2      DIM       6                   CHECK NUMBER 2
IDJ      DIM       1                   WSJ PAYMENT CODE
IAC      DIM       1                   ADJUSTMENT CODE
IN1      DIM       6                   CHECK NUMBER 1
IKD      DIM       6                   CHECK DATE
I90      DIM       1                   OVER 90 DAY LETTER CODE
IMP      FORM      7                   MAILER PAYMENT RECEIVED
IDP      DIM       6                   DATE OF MAILER PAYMENT
IIN      DIM       6                   INVOICE NUMBER
IID      DIM       6                   INVOICE DATE
IC1      DIM       2                   ADD CHRG CODE 1
IA1      DIM       12                  
IC2      DIM       2
IA2      DIM       12
IC3      DIM       2
IA3      DIM       12
IC4      DIM       2
IA4      DIM       12
IC5      DIM       2
IA5      DIM       12
IC6      DIM       2
IA6      DIM       12
IC7      DIM       2
IA7      DIM       12
IC8      DIM       2
IA8      DIM       12
IC9      DIM       2
IA9      DIM       12
IC0      DIM       2
IA0      DIM       12
IGP      DIM       1                   GUARANTEED PAYMENT CODE
IAR      FORM      9                   ACCOUNTS RECEIVABLE
ISC      DIM       1                   SALESMAN CODE
IP2      FORM      9                   A/P 2
