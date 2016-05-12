PC       EQU       0
         INC       COMMON.inc
         INC       CONS.inc
	include tinvdd.inc


ConvFile IFILE     KEYLEN=6,dup
Convlist	list
Tinvlr2 DIM 6
TinvB12   DIM 4
TinvDate2 DIM 8
TINVDesc2 DIM 30
TinvInv2	 DIM 11
TinvDolr2 DIM 12
TinvFiller2 DIM 20
	listend
release init "1.0"	
	open convfile,"triplexconv"
	


	
	loop
	        call tinvseq
        until over
	
		move Tinvlr to Tinvlr2
		move TinvB1 to tinvb12
		move TinvDate to TinvDate2
		move TinvDesc to TinvDesc2
		pack TinvInv2 with "     ",TinvInv
		move TinvDolr to TinvDolr2
		clear Tinvfiller2
		write convfile,Tinvlr2;TINVLR2,TINVB12,TINVDATE2,TINVDESC2,TINVINV2,TINVdolr2,TINVfiller2
	repeat
	
	stop
	
	
	
	
	
	
	
	
	
	
	
	
	
	include tinvio.inc
         INCLUDE   COMLOGIC.inc	