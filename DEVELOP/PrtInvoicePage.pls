.test test.test
               include      common.inc
               include      cons.inc
.               include               norddd.inc
str500               dim               500
               include      hp.inc

;prtdetail external "NCMP0002;prtdetail"


font1                              font
Font4                              font
font5                              font
Font08                              font
font09                               font
Font09I                              font
Font09B                              font
Font09BI                              font
Font010                              font
Font010B               font
Font012B               font
Font014                              font
Font014B               font
Font014BI               font
Font018I               font
Font07                              font
Font07dot5               font
Font07dot5B               font
Font07dot5I               font
Font07dot5BI               font
Font018B               font
Font018BI               font
PRTPG24B               font
PRTPG24I               font
PRTPG10                              font
sevenfive               form               "7.5"
..Create fonts to be used
               create               font1,"Times New Roman",size=14,bold
               create               Font08,"Times New Roman",size=8
               create               font5,"Times New Roman",size=11
               create               font09,"Times New Roman",size=9
               create               Font09I,"Times New Roman",size=9,Italic
               create               Font09B,"Times New Roman",size=9,Bold,Italic
               create               Font09BI,"Times New Roman",size=9,Bold,Italic
               create               Font010,"Times New Roman",size=10
               create               Font010B,"Times New Roman",size=10,Bold
               create               Font012B,"Times New Roman",size=12,Bold
               create               Font014,"Times New Roman",size=14
               create               Font014B,"Times New Roman",size=14,Bold
               create               Font014BI,"Times New Roman",size=14,Bold,Italic
               create               Font018I,"Times New Roman",size=18,Italic
               create               Font07,"Times New Roman",size=7
               create               Font07dot5,"Times New Roman",size=sevenfive
               create               Font07dot5I,"Times New Roman",size=sevenfive,Italic
               create               Font07dot5b,"Times New Roman",size=sevenfive,Bold
               create               Font07dot5bI,"Times New Roman",size=sevenfive,Bold,Italic
               create               Font018B,"Times New Roman",size=18,Bold
               create               Font018BI,"Times New Roman",size=18,Bold,Italic
..
               create               PRTpg24B,"Times New Roman",size=24,Bold
               create               PRTpg24I,"Times New Roman",size=24,Italic
               create               PRTpg10,"Times New Roman",size=10


.Laser               PFILE               ^
Laser               PFILE
               PRTOPEN               Laser,str500,"test fax"

testing
.               pack               NORDFLD,"425663"
.               move               C1,NORDPATH
.               call               NORDKEY
.               call               prtmlrboxGui
                call               prtInvfrm
.               call               prtdetail using laser,NORDFLD
               prtclose       Laser
               shutdown
;
.hi English 1000 units to an inch
.old code 300 units per inch

prtINVFrm
.               prtpage               Laser;"D 14154337796   NAndrew Harkins SDavid Herrick  (510) 302-4660  !^]":
               prtpage               Laser;*units=*HIENGLISH,*ROWSPACE=0,*COLSPACE=0,*Overlayon:
                              *p=3250:125,*font=Font018b,"Names  ":             
                              *font=Font018I,"in the News":
                              *PENSIZE=10,*p=2652:400,*Line=5652:400:
                              *p=3250:450,*font=Font010,"C  A  L  I  F  O  R  N  I  A     I  N  C .":
                              *p=3207:693,*font=Font07,"1300 Clay Street, 11th Floor, Oakland, CA 94612-1429":
                              *p=3489:793,"415-989-3350 ","·"," Fax 415-433-7796":
                              *RECT=1115:1340:3750:4500:
                              *p=3785:1125,*font=Font014BI,"Invoice":
                              *p=500:1962,*font=Font08,*line=7580:1962:                      ;top Hori line
                              *p=500:1962,*line=500:3462:                          ;left side (top) 
                              *p=7580:1962,*line=7580:3462:                        ;right side (top) 
                              *p=500:3522,*line=7580:3522:                      ;Middle Hori line
                              *p=500:3522,*line=500:4522:                          ;left side (Middle) 
                              *p=7580:3522,*line=7580:4522:                        ;right side (Middle) 
                              *RECT=4582:10582:500:7580:                           ;bottom section drawn as rectangle
                              *p515:10166,*font=font09,"Note: List owner requires payment of this invoice before": 
                              *p515:10283,*font=font09,"additional orders for this mailer can be processed.":
                              *p515:10400,*font=Font09Bi,"Payment due upon receipt. Please return copy with payment.":               
                              *p5625:10400,*font=Font09i,"Member Direct Marketing Association"               
               prtpage               Laser;*units=*HIENGLISH,*ROWSPACE=0,*COLSPACE=0,*Overlayon:
                              *p750:2212,*font=Font07Dot5I,"Date:":
                              *p4250:2212,"Invoice##":
                              *p5750:2212,"Mailer's P.O.":
                              *p750:2400,"Client##":
                              *p5750:2712,"NIN LR ##":
                              *p5750:3212,"Mail Date:":
                              *p750:3662,"Mailer's Offer:":
                              *p750:3912,"Key:":
                              *p750:4162,"List:":
                              *p=4250:4712,"$ Per M":
                              *p-5750:4712,"Amount Due":
                              *p750:10166,*font=Font09B,"Total Due:":
                              *font=Font07Dot5I
               return


.prtmlrboxGui Routine Laser
prtmlrboxGui
               prtpage               Laser;*units=*HIENGLISH,*ROWSPACE=0,*COLSPACE=0,*Overlayon:
                              *p=2800:125,*font=Font014b,"Client Copy"
               return
.
.prtRemitboxGui Routine Laser
prtRemitboxGui
               prtpage               Laser;*units=*HIENGLISH,*ROWSPACE=0,*COLSPACE=0,*Overlayon:
                              *p=2800:125,*font=Font014b,"Remitance Copy"
               return
.
.prtownerboxGui Routine Laser
prtownerboxGui
               prtpage               Laser;*units=*HIENGLISH,*ROWSPACE=0,*COLSPACE=0,*Overlayon:
                              *p=2675:125,*font=Font014b,"List Owner Copy"
               return
.

