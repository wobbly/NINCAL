.PURPOSE - PRINT CHECK RECONCILIATION STATEMENT AFTER EACH CHECK RUN.
...............................................................................
PC       EQU       0
         INCLUDE   COMMON.inc
         INC       CONS.inc
         inc       hp.inc
.
Release   init     "1.00"               DLH create UB Positive pay recon file from QB
Reldate   Init      "2015 October 8"
.
* *****************************************************************************
* NAMES IN THE NEWS MASTER RECONN1 FILE.
* *****************************************************************************
.
.    FILE:      RECONN1
.  LENGTH:
.COMPRESS:      SPACE
.    TYPE:      VAR/RANDOM
.     KEY:
...............................................................................
.
INPUT    FILE                               .QB CSV file
.create by using register FInd  check number range desired, export to csv
.last record first field = TOTAL
.,,,Type,,Date,,Num,,Name,,Memo,,Account,,Class,,Clr,,Split,,Amount,,Balance
.,,,Check,,04/01/2016,,25420,,ADVENTURE CYCLING ASSOCIATION,,Quarterly List Rental Income - 1st Qtr,,Clearing,,,,,,Cash - Union Bank,,"3,311.37",,"3,311.37"
.,,,Check,,04/01/2016,,25476,,Yes!Magazine,,Monthly List Rental Income - Mar 2016,,Clearing,,,,,,Cash - Union Bank,,812.90,,"271,056.78"
.Total,,,,,,,,,,,,,,,,,,,,,"271,056.78",,"271,056.78"


.,"Type","Date","Num","Name","Memo","Account","Class","Clr","Split","Amount","Balance".
.,,,,,,,,,,,""
.,"Bill Pmt -Check","10/01/2015","24825","American Farmland Trust","Quarterly List Rental Income - 3rd quarter","Cash - Union Bank",,"*","Accounts Payable - List Owners",-1231.18,-1231.18
.,"Bill Pmt -Check","10/01/2015","24825","American Farmland Trust","Quarterly List Rental Income - 3rd quarter","Accounts Payable - List Owners",,,"Cash - Union Bank",1231.18,0.00
.,"Bill Pmt -Check","10/01/2015","24826","AFLAC","Acct# SG706","Cash - Union Bank",,"*","Accounts Payable - Vendors",-291.06,-291.06
.,"Bill Pmt -Check","10/01/2015","24826","AFLAC","Acct# SG706","Accounts Payable - Vendors",,,"Cash - Union Bank",291.06,0.00
.,"Bill Pmt -Check","08/02/2002","24825","FEDEX","1012-1044-8","ZZZ Cash - Bank of America",,"X","Accounts Payable - Vendors",-94.69,-94.69
.,"Bill Pmt -Check","08/02/2002","24825","FEDEX","1012-1044-8","Accounts Payable - Vendors",,,"ZZZ Cash - Bank of America",94.69,0.00
.,"Bill Pmt -Check","08/02/2002","24826","PITNEY BOWES INC.-001","6284-7170-20-6","ZZZ Cash - Bank of America",,"X","Accounts Payable - Vendors",-209.00,-209.00
.,"Bill Pmt -Check","08/02/2002","24826","PITNEY BOWES INC.-001","6284-7170-20-6","Accounts Payable - Vendors",,,"ZZZ Cash - Bank of America",209.00,0.00
."Total",,,,,,,,,,0.00,0.00


QBVars     List
Null       Dim        12
QB1        dim        1
QB2        Dim        1
Type       Dim        25
QB3        dim        1
CKDate     Dim        10
QB4        Dim        1
Check      Dim        6
QB5        Dim        1
Payee      dim        35         .name
QB6        Dim        1
Memo       Dim        10         
QB7        Dim        1
account    dim        35         .want it to be "Cash - Union Bank"
class      dim        15
Clear      Dim        1
Split      Dim        1
QB8        Dim        1
QB9        Dim        1
QB10        Dim        1
QB11        Dim        1
CKAmount     dim        15         
Balance    Dim        1
           ListEnd

Output    file     
RCAPAY   FORM      10.2
filename1   Dim        55
FileCheck FIle
trapcount form      4
UBDate    Dim       10
tab       init     09

. 
TWO5     INIT      "                         "
TWO9     INIT      "                            "
DATE     DIM       6
SYSDATE  DIM       8
DETDATE  DIM       8
FORTY5  FORM      "45"
FOUR     FORM      "4"
ONETHOUS FORM      "1000"
DASH5    INIT      "-----"
DASH6    INIT      "------"
DASH7    INIT      "-------"
DASH8    INIT      "--------"
DASH9    INIT      "---------"
PAGECNTR FORM      "00"
LINECNTR FORM      2
PAGENUM  FORM      2
HEADATE  DIM       8
PAYMASK  DIM       18
TOTMASK  DIM       14
CHECKOUT DIM       6
RCPAYOUT FORM      9.2
DATEOUT  DIM       8
RLCRNOUT DIM       6
RCNTLOUT FORM      3
HEADISP  INIT      "NIN QB Pos PAY PROGRAM"
TOTALAP  FORM      9.2
PRTFLAG  DIM       1
LOCAL    INIT      "LOCAL"
rfile    dim       55
chkcnt   form      3
Bankfile  FIle      Fixed=80
Amount    Dim       10
tempAmt   Dim       13
ProcessDT Dim       8
Countin   FOrm      5
TempTot   Dim       12
Total     dim       10
CheckN    DIm       10
hashcheck form      10
boacount  form      5

. 
+..............................................................................
          IFNZ      PC
         CLOCK     DATE TO DATE
         MOVE      "99/99/99" TO DETDATE
         EDIT      DATE TO DETDATE
         MOVE      DETDATE TO TODAY
         UNPACK    DATE INTO MM,DD,YY
         XIF
.
         IFZ       PC
         CLOCK     DATE TO DETDATE
         MOVE      DETDATE TO TODAY
         UNPACK    DETDATE INTO MM,STR1,DD,STR1,YY
         XIF
          Pack      ProcessDT from CC,yy,mm,dd

         MOVE      "QuickenUB" TO PROGRAM
         MOVE      "Pos Pay PROGRAM" TO STITLE
         MOVE      "Names in the News" TO COMPNME
.prompt for input name
Input_Quit
                    call      debug
                    move      "c:\work\",Taskname                    ."
                    Clear        str45  
                    getfname open,"Open CSV File",str45,taskname,"csv"
                    pack      filename1,taskname,str45
                    if (str45 = "")     .User hit 'Cancel'
.Simulate a Quit
                              noreturn
                              goto Input_Quit
                    endif


         CALL      PAINT
           MOVE      "Exit" TO PF5
         TRAP      END IF F5
         CALL      FUNCDISP
         DISPLAY   *P01:06,"Input File  : ":
                   *P01:07,"Print File  : ":
                   *P01:08,"Input Count : "
.
INPGET   TRAP      FILENG IF IO
         OPEN      TESTFILE,filename1
         TRAPCLR   IO
         DISPLAY   *P15:06,filename1
         CLOSE     TESTFILE
         OPEN      Input,filename1
          GOTO      start
FILENG   NORETURN
         KEYIN     *P01:24,*EL,"The Input file is not on-line. ":
                   *P15:06,filename1
         GOTO      INPGET
.

. 
START
.
LOOPER  READ      INPUT,SEQ;*CDFON,QBVARS


.
         GOTO      DONE IF OVER
         Scan      "TOTAL",Null
         goto         Done if equal
. 
. 
. 
. 
. 
PROCESS
          add       c1,countin
          if        (countin = 1)       .first record create bank file
          Pack      Str55 from "e:\data\UB",ProcessDT,".txt|NINS1:502"
          rep       Zfill in str55
          pack      MailAttach from str55
          Prepare   Bankfile,Str55
          endif
          reset     Check
          move      c0,n6
          move      Check,n6
          if        Eos                        .not numeric
          goto      SkipBOA
          endif
          add       n6,HashCheck
          Pack      CheckN from b4,Check
          rep       Zfill,CHeckN
          Clear     TempAmt
          call      trim using ckamount
          move      CKAmount,tempAmt
          call      RemoveChar using tempamt,"-"
          call      RemoveChar using tempamt,","
          call      trim using TempAmt
          count     n2,TempAmt
          if        (N2=10)
          move      TempAmt,Amount
          elseif    (N2=9)
          pack      Amount from c0,TempAmt
          elseif    (N2=8)
          pack      Amount from c0,c0,TempAmt
          elseif    (N2=7)
          pack      Amount from c0,c0,c0,TempAmt
          elseif    (N2=6)
          pack      Amount from c0,c0,c0,c0,TempAmt
          elseif    (N2=5)
          pack      Amount from c0,c0,c0,c0,c0,TempAmt
          elseif    (N2=4)
          pack      Amount from c0,c0,c0,c0,c0,c0,TempAmt
          elseif    (N2=3)
          pack      Amount from c0,c0,c0,c0,c0,c0,c0,TempAmt
          elseif    (N2=2)
          pack      Amount from c0,c0,c0,c0,c0,c0,c0,c0,TempAmt
          elseif    (N2=1)
          pack      Amount from c0,c0,c0,c0,c0,c0,c0,c0,c0,TempAmt
          endif
          rep       Zfill,Amount
          add       c1,Boacount
         UNPACK    CKDATE INTO MM,str1,DD,str1,cc,YY
         REP       ZFILL,MM
         REP       ZFILL,DD
         REP       ZFILL,YY
         PACK      UBDATE FROM MM,SLASH,DD,SLASH,cc,YY

           Move       Amount,RCAPAY
          if        (rcapay > 0)
          Write     Bankfile,seq;*edion=tab,UBDate,CheckN,Amount,payee
          endif
SkipBOA
          reset     Check
         GOTO      LOOPER
DONE      
. 
. 
. 
EOJ
          move      BOACount,Str5
          rep       Zfill,str5
          Clear     TempTot
          move      TotalAP,tempTot
          call      RemoveChar using temptot,Period
          call      trim using temptot
          count     n2,temptot
          if        (N2=10)
          move      temptot,total
          elseif    (N2=9)
          pack      total from c0,temptot
          elseif    (N2=8)
          pack      total from c0,c0,temptot
          elseif    (N2=7)
          pack      total from c0,c0,c0,temptot
          elseif    (N2=6)
          pack      total from c0,c0,c0,c0,temptot
          elseif    (N2=5)
          pack      total from c0,c0,c0,c0,c0,temptot
          elseif    (N2=4)
          pack      total from c0,c0,c0,c0,c0,c0,temptot
          elseif    (N2=3)
          pack      total from c0,c0,c0,c0,c0,c0,c0,temptot
          elseif    (N2=2)
          pack      total from c0,c0,c0,c0,c0,c0,c0,c0,temptot
          elseif    (N2=1)
          pack      total from c0,c0,c0,c0,c0,c0,c0,c0,c0,temptot
          endif
          rep       Zfill,Total
          clear     str10
          move      Hashcheck,str10
          rep       zfill,str10
          weof      Bankfile,seq
          close     Bankfile
          move      "3000",str4                   .30 seconds
          call      Waitin using str4
          Move      "Union Bank Recon",MailSubjct
          Move      "Creques@nincal.com",MailFrom
          Move      "GemmaSpranza@nincal.com",MailTo

CheckFile
          Move      MailAttach,Str55
          trap      WaitForEnd giving error if IO
          open      FileCheck,STR55,Exclusive     
          Close     FIleCHeck
          trap      IOMssg GIVING ERROR IF IO

          Clear     MailBody
          
          Append    "Please upload to bank",MailBody
          Append    CRLF,MailBOdy
          Reset     MailBody
          Call      SendMail


         DISPLAY   *P1:1,*ES,*P10:12,"JOB DONE!!!!!!",*W2
         shutdown  "cls"
         STOP

WaitForEnd
                    TrapClr   IO
.check the error if file does not exist just get out
                    add       C1,Trapcount
                    move      "3000",str4
                    call      waitin using str4
.                    pause     "30"
                    noreturn
                   if        (trapcount > 60)   . 5 min are you kidding me. clearly not waiting 5 min
                    Pack       MailSubjct,"union Bank recon",str55
                    append    CRLF,MailBOdy
                    append    "mailTo = ",mailbody
                    append    mailto,mailbody
                    append    CRLF,MailBOdy
                    append    "maiLFrom = ",mailbody
                    append    maiLFrom,mailbody
                    
                    append    CRLF,MailBOdy
                    append    str45,MailBody
                    append    CRLF,MailBOdy
                    append    str55,MailBody
                    append    CRLF,MailBOdy
                    append    "I am sorry I could not send the file",Mailbody
                    reset     Mailbody
                    Clear     Mailto
                    Pack      MailTO,"CReques@nincal.com"
                    Move      B1,Mailattach
                    call      SendMail
                    return
                    
                    endif
          
                    goto      checkfile
.end patch 2.72

. 
. 
ABORT      BEEP
         DISPLAY   *P1:1,*HON,*ES,"JOB ABORTED VIA F4 KEY, OH NO!!!!",*W3
         GOTO      EOJ
         
IO       TRAPCLR   IO 
         NORETURN
         DISPLAY   *P12:15,"ERROR IS ",ERROR,*W5
         shutdown  "cls"
         STOP
. 
         INCLUDE   COMLOGIC.inc
. 

