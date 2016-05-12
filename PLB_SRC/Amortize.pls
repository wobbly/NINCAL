. amortization schedule
. c. kozel
. chris@kozel.net
.
prin     form      8.2
rate     form      1.4
num      form      3
payper   form      2

pay      form      8.2
pay2     form      8.2

i        form      3
balp     form      8.2
int      form      5.2
prn      form      5.2
intt     form      8.2
prnt     form      8.2
payt     form      8.2
out      file
s        form      "-1"

         prep      out,"amort"


.         loop
         keyin     *r,*hd,"Principal:  ",prin;
         keyin     *r,*hd,"Rate as a factor (.05=5%): ",rate;
         keyin     *r,*hd,"Total Number of Payments(360 is 30 years) ",num;
         keyin     *r,*hd,"Payments per Year(12=monthly): ",payper;

         call      amort using prin,rate,num,payper,pay

         write     out,s;"principal      ",prin
         write     out,s;"rate (as a factor)   ",rate
         write     out,s;"number of payments ",num
         write     out,s;"payments per year   ",payper
         write     out,s;"payment       ",pay,015,012,015,012:
                   "---------------------------------------------",015,012:
                   "Num    Balance Interest Principal    Payment",015,012:
                   "---------------------------------------------"

         display   *r,*hd,pay;
         call      sched
         weof      out,s
.         repeat

         stop


sched    unpack    "",i,prnt,intt,payt
         move      prin,balp

         loop
         add       "1",i
         calc      int=(balp*rate)/(payper*1.0)
         compare   i,num
         until     equal
         calc      prn=pay-int
         call      calcit
         repeat

         move      balp,prn
         call      calcit
         write     out,s;015,012,"     Total  ",intt,prnt,payt,015,012:
                   "------------------------------------------",015,012,014



         return

calcit
         calc      pay2=int+prn
         write     out,s;i,balp," ",int,"  ",prn,pay2
         sub       prn,balp
         add       int,intt
         add       prn,prnt
         add       pay2,payt
         return


#prin     form      ^
#rate     form      ^
#numpay   form      ^
#peryear  form      ^

#payment  form      ^

#n1       form      12.12
#n2       form      12.12
#n3       form      12.12
#i1       form      3

amort    lroutine  #prin,#rate,#numpay,#peryear,#payment
         calc      #n1=((#rate/#peryear)+1.0)

         move      "1",#n3
         unpack    "",#i1
         loop
         add       "1",#i1
         compare   #i1,#numpay
         until     less
         mult      #n1,#n3
         repeat
         move      #n3,#n1

.         power     #numpay,#n1

         calc      #n2=#prin/(((1.0-(1.0/#n1))*#peryear)/#rate)
         move      #n2,#payment



         return







