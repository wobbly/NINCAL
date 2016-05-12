.test test.test
.         include      common.inc
.         include      cons.inc
.         include   norddd.inc
.str500   dim       500
.         include      hp.inc

.patch 1.7  2014 February 21 DLH Rewrite of text by Susan & Senior staff
.patch 1.6  20 February 2013 DLH Rewrite of text by Susan
.patch 1.5  26February 2010 DLH turn of PL logo
.patch 1.42 28Jan2009 DLH Zip+4
.patch 1.41 10April2008 DLH new cancellation fee
.patch 1.4  16Jan2008 DLH change addressing
.Patch 1.31         12Jul2007  JD   Fixed zip
.Patch 1.3          8JAN2007   DLH  PL LOGO 
.Patch 1.21         30JAN2006  JD  Company/Broker address print Mailer/Office copy.
.Patch 1.2          06AUG2004  ASH      Logo Conversion
.Patch 1.1          17FEB2004  ASH  DATACARD CONVERSION

prtdetail external "NCMP0002;prtdetail"

.Blockout PICT
.Blockout1          PICT

.font1              font
.Font4              font
.font5              font
.fontO8             font
.fontO9I            font
.fontO10            font
..fontO10B          font
.fontO12B font
.fontO14            font
.FontO14B font
.FontO14BI          font
.FontO18I font
.FontO7             font
.FontO7dot5         font
.FontO7dot5B        font
.FontO7dot5I        font
.FontO7dot5BI       font
.FontO18B font
.FontO18BI          font
.PRTPG24B font
.PRTPG24I font
.PRTPG10            font
..Create fonts to be used
.sevenfive          form      "7.5"
.         create    font1,"Times New Roman",size=14,bold
.         create    fontO8,"Times New Roman",size=8
.         create    font5,"Times New Roman",size=11
.         create    fontO9I,"Times New Roman",size=9,Italic
.         create    fontO10,"Times New Roman",size=10
..        create    fontO10B,"Times New Roman",size=10,Bold
.         create    fontO12B,"Times New Roman",size=12,Bold
.         create    fontO14,"Times New Roman",size=14
.         create    fontO14B,"Times New Roman",size=14,Bold
.         create    fontO14BI,"Times New Roman",size=14,Bold,Italic
.         create    fontO18I,"Times New Roman",size=18,Italic
.         create    fontO7,"Times New Roman",size=7
.         create    fontO7dot5,"Times New Roman",size=sevenfive
.         create    fontO7dot5I,"Times New Roman",size=sevenfive,Italic
.         create    fontO7dot5b,"Times New Roman",size=sevenfive,Bold
.         create    fontO7dot5bI,"Times New Roman",size=sevenfive,Bold,Italic
.         create    fontO18B,"Times New Roman",size=18,Bold
.         create    fontO18BI,"Times New Roman",size=18,Bold,Italic
..
.         create    PRTpg24B,"Times New Roman",size=24,Bold
.         create    PRTpg24I,"Times New Roman",size=24,Italic
.         create    PRTpg10,"Times New Roman",size=10
GreyFill  Color
.Black    Color
NoFIll    Color
colornum form       24


.Laser    PFILE     ^
.Laser    PFILE
.         PRTOPEN   Laser,str500,"test fax"

.testing
.         pack      NORDFLD,"425663"
.         move      C1,NORDPATH
.         call      NORDKEY
.         call      prtmlrboxGui
.         call      prtordfrmGuiA
.         call      prtdetail using laser,NORDFLD
.         shutdown

.hi English 1000 units to an inch
.old code 300 units per inch
.PortraitLTRHEADGuiLand Routine Laser
.        PRTPAGE     Laser;*units=*HIENGLISH,*ROWSPACE=0,*COLSPACE=0,*Overlayon,*ORIENT=*LANDSCAPE:
.                   *p=3925:250,*font=prtpg24B,"Names  ":
.                   *font=prtpg24I," in the News":
.                   *PENSIZE=10,*p=3550:550,*Line=7050:550:
.                   *p=3925:650,*font=prtpg10,"C    A    L    I    F    O    R    N    I    A       I    N    C.":
.                   *p=2800:7900,*font=prtpg10,"1300 Clay Street, 11th Floor, ":
.                    "Oakland, CA 94612-1429 ","·":
.                    " 415-989-3350 ","·"," Fax 415-433-7796"
.              goto    eoj
.

.prtordfrmGuiA Routine Laser
prtordfrmGuiA
.List Management
.         prtpage   Laser;"D 14154337796   NAndrew Harkins SDavid Herrick  (510) 302-4660  !^]":
.begin patch 1.3
.         prtpage   Laser;*units=*HIENGLISH,*ROWSPACE=0,*COLSPACE=0,*Overlayon:
          prtpage   Laser;*units=*HIENGLISH,*ROWSPACE=0,*COLSPACE=0,*Overlayon
.end patch 1.3
.START PATCH 1.2 REPLACED LOGIC
.                   *p=5663:125,*font=fontO18b,"Names  ":
.                   *font=FontO18I,"in the News":
.                   *PENSIZE=10,*p=5402:400,*Line=8402:400:
.                   *p=5663:450,*font=fontO10,"C  A  L  I  F  O  R  N  I  A     I  N  C .":
.                   *p=6120:693,*font=fontO7,"1300 Clay Street, 11th Floor":
.                   *p=6204:793,"Oakland, CA 94612-1429":
.                   *p=6027:893,"415-989-3350 ","·"," Fax 415-433-7796":
.begin argh
.begin patch 1.3
..        IF        (OcompID2 = "P")
.         IF        (OcompID = "P")
.         prtpage   Laser;*p=5663:25,*font=fontO18b,"Pacific Lists, Inc.":
.                   *p=6120:343,*font=fontO7,"1300 Clay St. 11th Floor":
..START PATCH 1.31
.                   *p=6014:443,"Oakland, CA 94612-1492":
..                  *p=6014:443,"Oakland, CA 94312-1492":
..End PATCH 1.31
.                   *p=5980:543,"415-945-9450 ","·"," Fax 415-945-9451":
.                   *p=5980:643,"A Division of Names in the News"
.
..        prtpage   Laser;*p=5663:25,*font=fontO18b,"Pacific Lists, Inc.":
..                  *p=6120:343,*font=fontO7,"100 Tamal Plaza, Suite 50":
..                  *p=6014:443,"Corte Madera, CA 94925-1182":
..                  *p=5980:543,"415-945-9450 ","·"," Fax 415-945-9451":
..                  *p=5980:643,"A Division of Names in the News"
.         Else
.         prtpage   Laser;*Pictrect=*off,*PICT=0:800:5350:10350:NINLogo
.         endif
.end argh
.END PATCH 1.2 REPLACED LOGIC
          prtpage   Laser;*RECT=115:365:0:1000:
.end patch 1.3
                    *RECT=115:365:2500:4250:
                    *p=125:125,*font=FontO14bi,"List Order":
                    *p=0:637,*line=5233:637:
.START PATCH 1.2 REPLACED LOGIC
.                   *p=5400:637,*line=8067:637:
                    *p=5400:787,*line=8067:787:
.END PATCH 1.2 REPLACED LOGIC
.                   *p=0:637,*line=0:10625:
                    *p=5:637,*line=5:10625:
.START PATCH 1.2 REPLACED LOGIC
.                   *p5400:637,*line=5400:10625:
                    *p5400:787,*line=5400:10625:
.END PATCH 1.2 REPLACED LOGIC
.begin patch 1.6
.begin patch 1.7
.                    *p5625:1292,*Font=FontO7Dot5B,"Note":
                    *p5625:1292,*Font=FontO7Dot5B,"Rules of use":
                    *p5625:1475,*Font=FontO7Dot5,"Names are furnished for one-time use to the":
                    *p5625:1592,"organization or individual requesting the rental or":
                    *p5625:1709,"exchange and are not to be copied, revised in any":
                    *p5625:1826,"form, or sold. Re-use or Email append is not":
                    *p5625:1943,"allowed without prior approval. Although we believe":
                    *p5625:2060,"the information concerning this list to be accurate":
                    *p5625:2177,"we cannot guarantee its accuracy or the outcome of the":
                    *p5625:2294,"mailing. Names cannot be used for telemarketing":
                    *p5625:2411,"without written permission from the list owner."
.                    *p5625:1826,"form or sold. Although we believe the information":
.                    *p5625:1943,"concerning this list to be accurate we cannot":
.                    *p5625:2060,"guarantee its accuracy or the outcome of the":
.                    *p5625:2177,"mailing. Names cannot be used for telemarketing":
.                    *p5625:2294,"without written permission from the list owner."
.end patch 1.7
.                    *p5625:2411,"Post-merge tapes files may be used for modeling with":
.                    *p5625:2528,"a third party. The source of the list will not be":
.                    *p5625:2645,"provided.  Mailer reserves the right to mail":
.                    *p5625:2762,"duplicates at a later date."
.
.                    *p5625:1475,*Font=FontO7Dot5,"Names are furnished for one-time use to the":
.                    *p5625:1592,"organization or individual requesting the rental or":
.                    *p5625:1709,"exchange and are not to be copied, revised in any":
.                    *p5625:1826,"form, sold, or given to any other party.  Although":
.                    *p5625:1943,"we believe the information concerning this list to":
.                    *p5625:2060,"be accurate we cannot guarantee its accuracy or":
.                    *p5625:2177,"the outcome of the mailing. Names cannot be used":
.                    *p5625:2294,"for telemarketing without written permission from":
.                    *p5625:2411,"the list owner. Post-merge tapes cannot be used":
.                    *p5625:2528,"for storing and scanning purposes without":
.                    *p5625:2645,"notification to the list owner.  Mailer reserves the":
.                    *p5625:2762,"right to mail duplicates at a later date.":
.
.
.                    *p5625:3062,*Font=FontO7Dot5B,"Billing":
                    prtpage   Laser;*p5625:3062,*Font=FontO7Dot5B,"Billing":
.end patch 1.6
.begin patch 1.3
.                   *p5625:3245,*Font=FontO7Dot5,"On completion of addressing, bill rentals c/o":
                    *p5625:3245,*Font=FontO7Dot5,"On completion of addressing, bill rentals c/o"
.START PATCH 1.2 REPLACED LOGIC
.                   *p5625:3362,"Names in the News, California, Inc., less":
.                   *p5625:3479,"brokerage commission. We will bill mailer on":
.                   *p5625:3596,"behalf of list owner; payment (less commission)":
.                   *p5625:3713,"will be made upon receipt of payment from the":
.                   *p5625:3830,"mailer. We reserve the right to deduct for":
.                   *p5625:3947,"unusable names from payment to list owner. We":
.                   *p5625:4064,"act only as agent for the list owner or the mailer":
.                   *p5625:4181,"in these transactions.":
.....................................................
.begin patch 1.5
.begin patch 1.3
.          IF        (OcompID = "P")
.          PrtPage   Laser;*p5625:3362,"Pacific Lists, Inc., less brokerage commission.":
.                    *p5625:3479,"We will bill mailer on behalf of list owner;":
.                    *p5625:3596,"payment (less commission) will be made upon":
.                    *p5625:3713,"receipt of payment from the mailer. We reserve":
.                    *p5625:3830,"the right to deduct for unusable names from":
.                    *p5625:3947,"payment to list owner. We act only as agent":
.                    *p5625:4064,"for the list owner or the mailer in these":
.                    *p5625:4181,"transactions."
.          Else      
          PrtPage   Laser;*p5625:3362,"Names in the News, less brokerage commission.":
                    *p5625:3479,"We will bill mailer on behalf of list owner;":
                    *p5625:3596,"payment (less commission) will be made upon":
                    *p5625:3713,"receipt of payment from the mailer. We reserve":
                    *p5625:3830,"the right to deduct for unusable names from":
                    *p5625:3947,"payment to list owner. We act only as agent":
                    *p5625:4064,"for the list owner or the mailer in these":
                    *p5625:4181,"transactions."
.          Endif
.eidn patch 1.5
.END PATCH 1.2 REPLACED LOGIC
          PrtPage   Laser;*p5625:4548,*font=FontO7Dot5B,"Payment":
.end patch 1.3
                    *p5625:4731,*font=FontO7Dot5,"Due upon receipt of invoice. Full amount":
                    *p5625:4848,"required on orders cancelled after mail date; ":
                    *p5625:4965,"Cancellation by mailer prior to or on mail date ":
                    *p5625:5082,"subject to a $100.00 processing fee. Net discount":
.                   *p5625:5082,"subject to a $100.00 processing fee. Volume discount":
                    *p5625:5199,"includes allowances for rejected data. No additional":
                    *p5625:5316,"deductions allowed. Discount on Base rate only -- ":
                    *p5625:5433,"select fees are applied to gross quantity. No":
                    *p5625:5550,"computer verification necessary.":
.begin patch 1.4
                    *p5625:5917,*font=FontO7Dot5B,"Addressing":
                    *p5625:6100,*font=FontO7Dot5,"Unless stated in special instructions, it is":
                    *p5625:6217,"assumed list is in strict zip sequence. Address":
                    *p5625:6334,"USA names only. All Canadian, foreign, military,":
                    *p5625:6451,"& records without individual contact names are":
                    *p5625:6568,"to be omitted.":
.                   *p5625:6334,"USA names only. All Canadian, foreign, military,":
.                   *p5625:6451,"business, library and institutional names are to":
.                   *p5625:6568,"be omitted.":
.end patch 1.4
                    *RECT=6733:6891:5625:5750:
                    *p5775:6733,"This is a test. Address a representative cross":
                    *p5775:6858,"section and keep a record to avoid duplication":
                    *p5775:6975,"on continuations.":
                    *RECT=7116:7266:5625:5750:
                    *p5775:7116,"This is a continuation. No omit required.":
                    *RECT=7366:7516:5625:5750:
                    *p5775:7366,"This is a continuation. Omit the following:":
                    *RECT=7866:8016:5625:5750:
                    *p5775:7866,"Entire list/or all available in specified":
                    *p5775:7983,"select. Advise if quantity differs by 10% or more."
.begin patch 1.6

.                    *p5625:8233,*font=FontO7Dot5B,"Important":
.                    *p5625:8416,*font=FontO7Dot5,"If you cannot comply with these instructions, or":
.                    *p5625:8533,"if charges shown are not correct, notify us by":
.                    *p5625:8650,"phone for our approval prior to addressing.":
.                    *p5625:8767,"Adjustments not approved before addressing will":
.                    *p5625:8884,"not be accepted."
.                    *p5625:8884,"not be accepted. Magnetic Tape orders - tape will":
.                    *p5625:9001,"not be returned unless requested in writing by ":
.                    *p5625:9001,"not be returned unless requested in writing by ":
.begin patch 1.3
.                    *p5625:9118,"list owner."
.end patch 1.6
.                   *p5625:9118,"list owner.":
.begin patch 1.5
.          IF        (OcompID = "P")
.          PrtPage   Laser;*p5625:9300,"PL Contact:"
.          Else      
          PrtPage   Laser;*p5625:9300,"NIN Contact:"
.          Endif
.end patch 1.5
.                   *p5875:10400,*font=fonto9i,"Member Direct Marketing Association"
          PrtPage   Laser;*p5875:10400,*font=fonto9i,"Member Direct Marketing Association"
.end patch 1.3      
.START PATCH 1.1 REPLACED LOGIC
.         prtpage   Laser;*units=*HIENGLISH,*ROWSPACE=0,*COLSPACE=0,*Overlayon:
.                   *p125:887,*font=FontO7Dot5B,"LR ##":
.                   *p2500:887,*font=FontO7Dot5I,"Date:":
.                   *p125:1075,"Mailer P.O.":
.                   *p2500:1075,"Client No.:":
.                   *p125:1263,"Mailer:":
.                   *p125:1763,"Offer:":
.                   *p125:2263,"List Owner:":
.                   *p125:3138,"CC To:":
.                   *p125:3451,"List:":
.                   *p125:4076,"Quantity:":
.                   *p2500:4076,"Per M $":
.                   *p125:4451,"Key/List ID:":
.                   *p125:4801,"Furnished on:":
.                   *p125:5001,"Return to:":
.                   *p125:6501,"Ship to arrive by:":
.                   *p2500:6501,"Via:":
.                   *p125:6876,"Mail Date:":
.                   *p125:7126,*font=FontO7Dot5BI,"Special Instructions:",*font=FontO7Dot5
          prtpage   Laser;*units=*HIENGLISH,*ROWSPACE=0,*COLSPACE=0,*Overlayon:
                    *p125:800,*font=FontO7Dot5B,"LR ##":
                    *p2500:800,*font=FontO7Dot5I,"Date:":
                    *p125:988,"Mailer P.O.":
                    *p2500:988,"Client No.:":
                    *p125:1176,"Mailer:":
;                   *p125:1603,"Offer:":
;                   *p125:2093,"List Owner:":
;                   *p125:2918,"CC To:":
.START PATCH 1.21
                    *p125:1876,"Offer:":
                    *p125:2281,"List Owner:":
                    *p125:3006,"CC To:":
.START PATCH 1.2 
                    *p125:3211,"List:":
                    *p125:3587,"Quantity:":
                    *p2500:3587,"Base $":
                    *p125:5251,"Key/List ID:":
                    *p125:5501,"Furnished on:":
                    *p125:5751,"Return to:":
                    *p125:6689,"Ship to arrive by:":
                    *p2200:6689,"Via:":
                    *p125:6876,"Mail Date:":
                    *p125:7126,*font=FontO7Dot5BI,"Special Instructions:",*font=FontO7Dot5
.END PATCH 1.1 REPLACED LOGIC
          return

.prtordfrmGuiB Routine Laser - Brokerage orders
prtordfrmGuiB
.         prtpage   Laser;"D 14154337796   NAndrew Harkins SDavid Herrick  (510) 302-4660  !^]":
          prtpage   Laser;*units=*HIENGLISH,*ROWSPACE=0,*COLSPACE=0,*Overlayon

.begin argh
.begin patch 1.3
.         IF        (OcompID = "P")
.         prtpage   Laser;*p=5663:25,*font=fontO18b,"Pacific Lists, Inc.":
.                   *p=6120:343,*font=fontO7,"1300 Clay St. 11th Floor":
..START PATCH 1.31
.                   *p=6014:443,"Oakland, CA 94612-1492":
..                  *p=6014:443,"Oakland, CA 94312-1492":
..End PATCH 1.31
.                   *p=5980:543,"415-945-9450 ","·"," Fax 415-945-9451":
.                   *p=5980:643,"A Division of Names in the News"
..        prtpage   Laser;*p=5663:25,*font=fontO18b,"Pacific Lists, Inc.":
..                  *p=6120:343,*font=fontO7,"100 Tamal Plaza, Suite 50":
..                  *p=6014:443,"Corte Madera, CA 94925-1182":
..                  *p=5980:543,"415-945-9450 ","·"," Fax 415-945-9451":
..                  *p=5980:643,"A Division of Names in the News"
..                  *p=6120:393,*font=fontO7,"100 Tamal Plaza, Suite 50":
..                  *p=6014:493,"Corta Madera, CA 94942-1429":
..                  *p=5980:593,"415-945-9450 ","·"," Fax 415-945-9451":
..                  *p=6204:793,"A Division of Names in the News"
.         Else
..START PATCH 1.2 REPLACED LOGIC
..                  *p=5663:125,*font=fontO18b,"Names  ":
..                  *font=FontO18I,"in the News":
..                  *PENSIZE=10,*p=5402:400,*Line=8402:400:
..                  *p=5663:450,*font=fontO10,"C  A  L  I  F  O  R  N  I  A     I  N  C .":
..                  *p=6120:693,*font=fontO7,"1300 Clay Street, 11th Floor":
..                  *p=6204:793,"Oakland, CA 94612-1429":
..                  *p=6027:893,"415-989-3350 ","·"," Fax 415-433-7796":
.         prtpage   Laser;*Pictrect=*off,*PICT=0:800:5350:10350:NINLogo
.         endif
.end argh
.End patch 1.3
.END PATCH 1.2 REPLACED LOGIC
          prtpage   Laser;*RECT=115:365:0:1000:
                    *RECT=115:365:2500:4250:
                    *p=125:125,*font=FontO14bi,"List Order":
                    *p=0:637,*line=5233:637:
.START PATCH 1.2 REPLACED LOGIC
.                   *p=5400:637,*line=8067:637:
                    *p=5400:787,*line=8067:787:
.END PATCH 1.2 REPLACED LOGIC
.                   *p=0:637,*line=0:10625:
                    *p=5:637,*line=5:10625:
.START PATCH 1.2 REPLACED LOGIC
.                   *p5400:637,*line=5400:10625:
                    *p5400:787,*line=5400:10625:
.END PATCH 1.2 REPLACED LOGIC
.begin patch 1.7
.                    *p5625:1292,*Font=FontO7Dot5B,"Note":
                    *p5625:1292,*Font=FontO7Dot5B,"Rules of use":
                    *p5625:1475,*Font=FontO7Dot5,"Names are furnished for one-time use to the":
                    *p5625:1592,"organization or individual requesting the rental or":
                    *p5625:1709,"exchange and are not to be copied, revised in any":
                    *p5625:1826,"form, or sold. Re-use or Email append is not":
                    *p5625:1943,"allowed without prior approval. Although we believe":
                    *p5625:2060,"the information concerning this list to be accurate":
                    *p5625:2177,"we cannot guarantee its accuracy or the outcome of the":
                    *p5625:2294,"mailing. Names cannot be used for telemarketing":
                    *p5625:2411,"without written permission from the list owner.":
                    *p5625:2528,"Post-merge files may be used for modeling with a":
                    *p5625:2645,"third party. List cannot be alluded to in the mailer’s":
                    *p5625:2762,"copy.  Mailer reserves the right to mail":
                    *p5625:2879,"duplicates at a later date."





.                    *p5625:1292,*Font=FontO7Dot5B,"Note":
..begin patch 1.6
.                    *p5625:1475,*Font=FontO7Dot5,"Names are furnished for one-time use to the":
.                    *p5625:1592,"organization or individual requesting the rental or":
.                    *p5625:1709,"exchange and are not to be copied, revised in any":
.                    *p5625:1826,"form or sold. Although we believe the information":
.                    *p5625:1943,"concerning this list to be accurate we cannot":
.                    *p5625:2060,"guarantee its accuracy or the outcome of the":
.                    *p5625:2177,"mailing. Names cannot be used for telemarketing":
.                    *p5625:2294,"without written permission from the list owner.":
.                    *p5625:2411,"Post-merge files may be used for modeling with a":
.                    *p5625:2528," third party. The source of the list will not be":
.                    *p5625:2645,"provided.  Mailer reserves the right to mail":
.                    *p5625:2762,"duplicates at a later date."
.end patch 1.7

.                    *p5625:1658,*Font=FontO7Dot5,"Names are furnished for one-time use to the":
.                    *p5625:1775,"organization or individual requesting the rental":
.                    *p5625:1892,"or exchange and are not to be copied, revised in":
.                    *p5625:2009,"any form, sold, or given to any other party.":
.                    *p5625:2126,"Although we believe the information concerning":
.                    *p5625:2243,"this list to be accurate we cannot guarantee its":
.                    *p5625:2360,"accuracy or the outcome of the mailing. Names":
.                    *p5625:2477,"cannot be used for telemarketing without written":
.                    *p5625:2594,"permission from the list owner. Post-merge tapes":
.                    *p5625:2711,"cannot be used for storing and scanning":
.                    *p5625:2828,"purposes without notification to the list owner.":
.                    *p5625:2945,"Mailer reserves the right to mail duplicates at a":
.                    *p5625:3062,"later date.":

.end patch 1.6

          prtpage   Laser;*p5625:3062,*Font=FontO7Dot5B,"Billing":
                    *p5625:3245,*Font=FontO7Dot5,"On completion of addressing, bill rentals c/o"

.Begin patch 1.3
.                   *p5625:3495,*Font=FontO7Dot5,"On completion of addressing, bill rentals c/o":
.START PATCH 1.2 REPLACED LOGIC
.                   *p5625:3612,"Names in the News, California, Inc., less":
.                   *p5625:3729,"brokerage commission. We will bill mailer on":
.                   *p5625:3846,"behalf of list owner; payment (less commission)":
.                   *p5625:3963,"will be made upon receipt of payment from the":
.                   *p5625:4080,"mailer. We reserve the right to deduct for":
.                   *p5625:4197,"unusable names from payment to list owner. We":
.                   *p5625:4314,"act only as agent for the list owner or the mailer":
.                   *p5625:4431,"in these transactions.":
.....................................................
.begin patch 1.5
.          IF        (OcompID = "P")
.                    prtpage   Laser;*p5625:3612,"Pacific Lists, Inc., less brokerage commission.":
.                    *p5625:3729,"We will bill mailer on behalf of list owner;":
.                    *p5625:3846,"payment (less commission) will be made upon":
.                    *p5625:3963,"receipt of payment from the mailer. We reserve":
.                    *p5625:4080,"the right to deduct for unusable names from":
.                    *p5625:4197,"payment to list owner. We act only as agent":
.                    *p5625:4314,"for the list owner or the mailer in these":
..begin patch 1.42
..                    *p5625:4431,"transactions."
.                    *p5625:4431,"transactions. Deductions will be taken for":
.                    *p5625:4548,"any addresses that cannot be corrected to zip+4"
..end patch 1.42
.          Else
          PrtPage   Laser;*p5625:3362,"Names in the News, less brokerage commission.":
                    *p5625:3479,"We will bill mailer on behalf of list owner;":
                    *p5625:3596,"payment (less commission) will be made upon":
                    *p5625:3713,"receipt of payment from the mailer. We reserve":
                    *p5625:3830,"the right to deduct for unusable names from":
                    *p5625:3947,"payment to list owner. We act only as agent":
                    *p5625:4064,"for the list owner or the mailer in these":
                    *p5625:4181,"transactions. Deductions will be taken for":
                    *p5625:4298,"any addresses that cannot be corrected to zip+4"
.                    prtpage   Laser;*p5625:3612,"Names in the News, less brokerage commission.":
.                    *p5625:3729,"We will bill mailer on behalf of list owner;":
.                    *p5625:3846,"payment (less commission) will be made upon":
.                    *p5625:3963,"receipt of payment from the mailer. We reserve":
.                    *p5625:4080,"the right to deduct for unusable names from":
.                    *p5625:4197,"payment to list owner. We act only as agent":
.                    *p5625:4314,"for the list owner or the mailer in these":
..begin patch 1.42
..                    *p5625:4431,"transactions."
.                    *p5625:4431,"transactions. Deductions will be taken for":
.                    *p5625:4548,"any addresses that cannot be corrected to zip+4"
.end patch 1.42
.          endif
.end patch 1.5
.END PATCH 1.2 REPLACED LOGIC
.                   *p5625:4681,*font=FontO7Dot5B,"Payment":
          PrtPage   Laser;*p5625:4681,*font=FontO7Dot5B,"Payment":
.End patch 1.3      
                    *p5625:4864,*font=FontO7Dot5,"Due upon receipt of invoice. Full amount":
                    *p5625:4981,"required on orders cancelled after mail date;":
                    *p5625:5098,"Cancellation by mailer prior to or on mail date":
                    *p5625:5215,"subject to a $100.00 processing fee.":
.begin patch 1.4
                    *p5625:5565,*font=FontO7Dot5B,"Addressing":
                    *p5625:5748,*font=FontO7Dot5,"Unless stated in special instructions, it is":
                    *p5625:5865,"assumed list is in strict zip sequence. Address":
                    *p5625:5982,"USA names only. All Canadian, foreign, military,":
                    *p5625:6099,"& records without individual contact names are ":
                    *p5625:6216,"to be omitted.":
.                   *p5625:5982,"USA names only. All Canadian, foreign, military,":
.                   *p5625:6099,"business, library and institutional names are to":
.                   *p5625:6216,"be omitted.":
.end patch 1.4
                    *RECT=6733:6891:5625:5750:
                    *p5775:6733,"This is a test. Address a representative cross":
                    *p5775:6858,"section and keep a record to avoid duplication":
                    *p5775:6975,"on continuations.":
                    *RECT=7116:7266:5625:5750:
                    *p5775:7116,"This is a continuation. No omit required.":
                    *RECT=7366:7516:5625:5750:
                    *p5775:7366,"This is a continuation. Omit the following:":
                    *RECT=7866:8016:5625:5750:
                    *p5775:7866,"Entire list/or all available in specified":
                    *p5775:7983,"select. Advise if quantity differs by 10% or more.":
                    *p5625:8233,*font=FontO7Dot5B,"Important":
                    *p5625:8416,*font=FontO7Dot5,"If you cannot comply with these instructions, or":
                    *p5625:8533,"if charges shown are not correct, notify us by":
                    *p5625:8650,"phone for our approval prior to addressing.":
                    *p5625:8767,"Adjustments not approved before addressing will":
.begin patch 1.6
                    *p5625:8884,"not be accepted."
.                    *p5625:8884,"not be accepted. Magnetic Tape orders - tape will":
.                    *p5625:9001,"not be returned unless requested in writing by ":
.                    *p5625:9001,"not be returned unless requested in writing by ":
.end patch 1.6

.begin patch 1.3
.                   *p5625:9118,"list owner.":
.begin patch 1.5
.          IF        (OcompID = "P")
.          PrtPage   Laser;*p5625:9300,"PL Contact:"
.          Else                
          PrtPage   Laser;*p5625:9300,"NIN Contact:"
.          Endif               
.                   *p5875:10400,*font=fonto9i,"Member Direct Marketing Association"
          PrtPage   Laser;*p5875:10400,*font=fonto9i,"Member Direct Marketing Association"
.end patch 1.5
.end patch 1.3
.START PATCH 1.1 REPLACED LOGIC
.         prtpage   Laser;*units=*HIENGLISH,*ROWSPACE=0,*COLSPACE=0,*Overlayon:
.                   *p125:887,*font=FontO7Dot5B,"LR ##":
.                   *p2500:887,*font=FontO7Dot5I,"Date:":
.                   *p125:1075,"Mailer P.O.":
.                   *p2500:1075,"Client No.:":
.                   *p125:1263,"Mailer:":
.                   *p125:1763,"Offer:":
.                   *p125:2263,"List Owner:":
.                   *p125:3138,"CC To:":
.                   *p125:3451,"List:":
.                   *p125:4076,"Quantity:":
.                   *p2500:4076,"Per M $":
.                   *p125:4451,"Key/List ID:":
.                   *p125:4801,"Furnished on:":
.                   *p125:5051,"Return to:":
.                   *p125:6501,"Ship to arrive by:":
.                   *p2500:6501,"Via:":
.                   *p125:6876,"Mail Date:":
.                   *p125:7126,*font=FontO7Dot5BI,"Special Instructions:",*font=FontO7Dot5
          prtpage   Laser;*units=*HIENGLISH,*ROWSPACE=0,*COLSPACE=0,*Overlayon:
                    *p125:800,*font=FontO7Dot5B,"LR ##":
                    *p2500:800,*font=FontO7Dot5I,"Date:":
                    *p125:988,"Mailer P.O.":
                    *p2500:988,"Client No.:":
                    *p125:1176,"Mailer:":
;                   *p125:1603,"Offer:":
;                   *p125:2093,"List Owner:":
;                   *p125:2918,"CC To:":
.START PATCH 1.21 
                    *p125:1876,"Offer:":
                    *p125:2281,"List Owner:":
                    *p125:3006,"CC To:":
.START PATCH 1.21 
                    *p125:3211,"List:":
                    *p125:3587,"Quantity:":
                    *p2500:3587,"Base $":
                    *p125:5251,"Key/List ID:":
                    *p125:5501,"Furnished on:":
                    *p125:5751,"Return to:":
                    *p125:6689,"Ship to arrive by:":
                    *p2200:6689,"Via:":
                    *p125:6876,"Mail Date:":
                    *p125:7126,*font=FontO7Dot5BI,"Special Instructions:",*font=FontO7Dot5
.END PATCH 1.1 REPLACED LOGIC
          return

.
.prtmlrboxGui Routine Laser
prtmlrboxGui
          clear     str2
          pack      str2 from osales10,osales
.begin patch 1.5
.          IF        (Ocompid = "P")
.          call      PrtPLILOgo
.          Elseif    (Ocompid2 = "P" & (str2 = "27" | str2 = "28"))
.          call      PrtPLILOgo
.          else
          call      PrtNINLOgo
.          endif
.end patch 1.5
          prtpage   Laser;*units=*HIENGLISH,*ROWSPACE=0,*COLSPACE=0,*Overlayon:
                    *p=2800:125,*font=FontO14b,"Mailer Copy"
          return
.
.prtofficeboxGui Routine Laser
prtofficeboxGui
.begin patch 1.5
.          IF        (Ocompid = "P")
.          call      PrtPLILOgo
.          else
          call      PrtNINLOgo
.          endif
.end patch 1.5
          prtpage   Laser;*units=*HIENGLISH,*ROWSPACE=0,*COLSPACE=0,*Overlayon:
                    *p=2800:125,*font=FontO14b,"Office Copy"
          return
.
.prtownerboxGui Routine Laser
prtownerboxGui
.begin patch 1.5
.          IF        (Ocompid = "P" & OCompid2  <> "N")
.          call      PrtPLILOgo
.          ElseIF    (Ocompid2 = "P")
.          call      PrtPLILOgo
.          else
          call      PrtNINLOgo
.          endif
.end patch 1.5
          prtpage   Laser;*units=*HIENGLISH,*ROWSPACE=0,*COLSPACE=0,*Overlayon:
                    *p=2700:125,*font=FontO14b,"List Owner Copy"
.                   *p=2675:125,*font=FontO14b,"List Owner Copy"
          return
.
.prtFulfilboxGui Routine Laser
prtFulfilboxGui
.begin patch 1.5
.          IF        (Ocompid = "P" & OCompid2  <> "N")
.          call      PrtPLILOgo
.          ElseIF    (Ocompid2 = "P")
.          call      PrtPLILOgo
.          else
          call      PrtNINLOgo
.          endif
.end patch 1.5
          prtpage   Laser;*units=*HIENGLISH,*ROWSPACE=0,*COLSPACE=0,*Overlayon:
                    *p=2700:125,*font=FontO14b,"Fulfillment Copy"
          return
.         include   NORDIO.INC
.prtordfrmPL Routine Laser temporary to rpint old PLI forms
prtordfrmPL
          prtpage   Laser;*units=*HIENGLISH,*ROWSPACE=0,*COLSPACE=0,*Overlayon

          prtpage   Laser;*p=0:0,*font=PRTpg24B,"Pacific Lists, Inc.":
                    *p=3500:20,*font=fonto10,"180 Grand Ave. Ste 1545, ":
.START PATCH 1.31
.                   "Oakland, CA 94312-1492":
                    "Oakland, CA 94612-3799":
.End PATCH 1.31
                    *p=3750:175,"Office: 415-945-9450 ","·"," Fax 415-945-9451":
                    *p=0:350,*font=fontO7,"A Division of Names in the News":
                    *p0:675,*font=fonto10,"OUR ##":
                    *p0:875,"DATE:":
                    *p0:1075,"CLIENT REF.NO.":
                    *p0:1325,"CLIENT:":
                    *p0:2375,"MAILER:":
                    *p0:2675,"OFFER:":
                    *p0:3000,"SUPPLIER:":
                    *p0:4000,"LIST:":
                    *p0:4375,"SELECTION:":
                    *p0:4675,"FORMAT:":
                    *p0:5025,"KEY EACH LABEL:":
                    *p3125:5025,"RATE:":
                    *p0:5325,"QUANTITY:":
                    *p0:5675,"ADD'L:":
                    *p0:5785,"CHARGES:":
                    *p0:6625,"SHIP TO:":
                    *p4900:1525,"CONTACT:":
                    *p5375:1650,"AT:":
                    *p4900:2050,"WANTED BY:":
                    *p4900:2375,"MAIL DATE:":
                    *p4900:2675,"SHIP VIA:":
                    *p6275:3160,*font=fonto10B,"MAILER":
                    *p6075:4300,"LIST OWNER",*font=FontO9:
                    *p5120:3375,"The names furnished for this order may be used once":
                    *p5120:3500,"only for the offer stated and may not be duplicated or":
                    *p5120:3625,"remailed. We believe information concerning this list":
                    *p5120:3750,"to be accurate, but we do not guarantee its accuracy or":
                    *p5120:3875,"the outcome of the mailing.":
                    *p5120:4550,"Please send a statement of charges to Pacific Lists, Inc.":
                    *p5120:4675,"We are acting as agent for the list owner only, and will":
                    *p5120:4800,"invoice our client on the owner's behalf. Upon receipt ":
                    *p5120:4925,"of payment, we will remit promptly less the standard ":
                    *p5120:5050,"brokerage commission. We do not guarantee payment ":
                    *p5120:5175,"from the client who is solely responsible for payment on":
                    *p5120:5300,"this order. If liability for sales, use or other tax is ":
                    *p5120:5425,"asserted, Pacific Lists as agent disclaims all liability ":
                    *p5120:5550,"or responsibility for payment or collection of such tax.":
                    *p5120:5800,"Unless otherwise stated, names are to be in zip sequence.":
                    *p5120:5925,"Mailer resrves the right to mail multi-buyers.":
                    *p5120:6175,"Omit: Canadia, Puerto Rico, foreign and military names":
                    *p5120:6300,"unless otherwise specified."
          
.         prtpage   Laser;*RECT=1250:2250:1100:4625:
.                   *RECT=2900:3900:1100:4625:
          prtpage   Laser;*RECT=1250:2250:1300:4825:
                    *RECT=3020:4020:1300:4825:
                    *RECT=3175:6600:5100:8000
          PrtPage   Laser;*p5875:10400,*font=fonto9i,"Member Direct Marketing Association":
                    *p3575:7450,*font=fonto10,"Special Instructions",*font=FontO7Dot5:
                    *p0:7525,*line=3500:7525:
                    *p4725:7525,*line=10000:7525
          return
.New Argh
PrtNINLogo
.          prtpage   Laser;*Pictrect=*off,*PICT=0:800:5350:10350:NINLogo
.          prtpage   Laser;*Pictrect=*off,*PICT=0:720:5350:9850:NINLogo                 .90%
          prtpage   Laser;*Pictrect=*off,*PICT=0:760:5350:10100:NINLogo                 .95%
          Return
PrtPLILOGO
          prtpage   Laser;*p=5663:25,*font=fontO18b,"Pacific Lists, Inc.":
                    *p=6120:343,*font=fontO7,"180 Grand Ave, Ste 1545":
                    *p=6014:443,"Oakland, CA 94612-3799":
                    *p=5980:543,"415-945-9450 ","·"," Fax 415-945-9451":
                    *p=5980:643,"A Division of Names in the News"
          Return
.end New Argh
.for temporary pli forms only
..Greenbox           .blockout supplier
.Greenbox
.                   CREATE    blockout=3:20:30:50:
.                             "\\nts0\c\netutils\blockout.tif"
.                   CREATE    blockout1=3:20:30:50:
.                             "\\nts0\c\netutils\blockout2.tif"
.
.               prtpage               Laser;*Pictrect=*on,*PICT=3020:4020:1300:4825:Blockout1
.         return
..               Create         Black=000:000:000
..               Create         NoFill=255:255:255
.
..            getitem         Black,0,colornum
.         prtpage   Laser;*fill=*on,*BgColor=colornum,*RECT=2900:3900:1300:4825
..            getitem         NoFill,0,colornum
.         prtpage   Laser;*fill=*off,*BgColor=colornum
.
.         return
.for temporary pli forms only
.Whitebox
.                   CREATE    blockout=3:20:30:50:
.                             "\\nts0\c\netutils\blockout.tif"
.                   CREATE    blockout1=3:20:30:50:
.                             "\\nts0\c\netutils\blockout2.tif"
..        CREATE  blockout=3:13:30:50,"\\nts0\c\netutils\blockout.jpg"
.  .             prtpage               Laser;*Pictrect=*off,*PICT=1550:2250:1300:4825:Blockout
.               return

.               Create         Black=000:000:000
.               Create         NoFill=255:255:255
..            getitem         Black,0,colornum
.         prtpage   Laser;*fill=*on,*BgColor=colornum,*RECT=1550:2250:1225:4750
..            getitem         NoFill,0,colornum
.         prtpage   Laser;*fill=*off,*BgColor=colornum
.
.         return
.