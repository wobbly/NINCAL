.1.  Ask for LO# and the new price
.2   Start at the beginning and read through the datacard file for a match between UserLONum and OWNNUM until over ----> STOP
.  /\        -Per each LONum match
.  |         -Read throught NINADD fatbase until over
.  |_________________________________________________|                                                                |
.            -Find LSTNUM
.            /\          -Get code from record
.            |                     -If code is '025' or '024' change NADDPRICE to UserNewPrice (in memory)
.            |                                 -delete current record and write the new one---
.            |_______________________________________________________________________________|



PC EQU 0

.Include some files

          Include Common.inc
          Include Cons.inc  
          Include NAdddd.inc
          Include NDatdd.inc   

Release   Init      "1.00"    RVW  Initial release
reldate   Init      "2015 July 31st"

.intialize some variables

Akey2                 Init      "02R"
CheckAgain            Form       1
UserLONum             Dim        6
UserNewPrice          Form       5.2                   


Main
           Call Paint
           Call GetUserInput
           Call ReadDCFile
          
GetOut           
           Display *P10:17,"Finished Updating E-mail and FTP price records.  Goodbye!"
           Pause "5"
           
           Close NDatFile
           Close Naddfile
           
           Stop
           
GetUserInput
  	  KeyIn *P10:8,*El,"For which LO# do you want to update the email and ftp prices? ",*JR,*ZF,UserLONum
	  KeyIn *P10:9,*El,"What is the new price for E-mail and FTP? ",*JR,*ZF,UserNewPrice

	  return

ReadDCFile           
.We're gonna get our data card record and let the user what number we're on.
	  loop
SkipToNextDC
		  CALL      NDATKS
		  Display *P10:11,"Checking List#: ",LSTNUM," for the right owner"
		  until over
		  If (OWNNUM == UserLONum)
		  	  Display *P10:13,"I'm updating the E-mail and FTP Prices for: ",LSTNUM 
				  Call CheckForFTP
				  If not over
					  Call ReplaceRecord 
				  Endif
 				  Call CheckForEmail
				  If not over 
					  Call ReplaceRecord
				  Endif
			  Endif                           
	 repeat
           
         return    //we shoudld never get to here.
           
CheckForFTP
                      reset NADDFLD
                      Pack NADDFLD,LSTNUM,"027"
                      Call NADDKEY

           return
           
CheckForEmail
                      reset NADDFLD
                      Pack NADDFLD,LSTNUM,"025"
                      Call NADDKEY
           
           return

ReplaceRecord
           Call NADDDEL
           Call BuildNewRecord
           Call NADDWRT
           
           return



BuildNewRecord
           Move UserNewPrice to NADDPRICE
           return
         
         
         
           Include Ndatio.inc
           Include NAddio.inc
           Include Comlogic.inc