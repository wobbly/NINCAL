.///////////////////////////////////////////////////////////////////////////////
.//                                                                           //
.//   PROGRAM:   WebDBFunctions.inc                                           //
.//                                                                           //
.//    AUTHOR:   Robb Whiting rwhitin@nincal.com                              //
.//                                                                           //
.//      DATE:   15 May 2016                                                  //
.//                                                                           //
.// COPYRIGHT:   2015-2016 Names in the News California Inc.                  //
.//              All rights reserved.                                         //
.//                                                                           //
.//  PURPOSE:    These are the supporting fuctions,                           //
.//              for RebuildDB and UpdateDB.			              //
.//              Written by RVW September 2015.                               //
.//                                                                           //
.// REVISION:    Ver 2.00  15 Sep 2015 RVW        Changing the order of       //
.//                                               Addressing, Sources,        //
.//                                               Selections, and Members     //
.//                                                                           //
.//              Ver 1.00  15 Sep 2015 RVW        Created.                    //
.//                                                                           //
.///////////////////////////////////////////////////////////////////////////////
.This is for not going through the entire file when we're only doing a couple lists
OpenWebFile
           Trap FileMissing if io
           Move "Couldn't open the web file" to ErrorMessage
           Open Webfile, "\\nins1\e\data\webfile.txt", Exclusive
           if (FileFlag == 1)                                                //This means that the file ain't there so we gotta make it.
                      Display *P10:22,"Creating the file"
                      Prep Webfile, "\\nins1\e\data\webfile.txt", Share
           Else                                                              /.Else clear it by deleting it and remaking it.
                      Display *P10:22,"Clearing existing file"
                      Close Webfile
                      Erase "\\nins1\e\data\webfile.txt"           
                      Prep Webfile, "\\nins1\e\data\webfile.txt", Share
           Endif
           Pause "1"
           Display *P10:21,"                                                                         "
           Display *P10:22,"                                                                         "
           move c0 to FileFlag
           
           return
           
OpenDCDB  
.This opens the datacard database
           Trap FileMissing if io
           Move "Couldn't open the datacard file" to ErrorMessage
           Open Ndatfile, "Nindat", Read
           if (FileFlag == 1)
                      Display *P10:22,"Quitting"
                      Stop
           Endif
           
           return

GetToday
.this gets the number of days in the Julian Calender that corresponds to today
           Clock Date to Date
           Unpack Date into SysMo, str1, SysDay, str1, SysYr  
           Move SysMo to mm
           Move SysDay to dd
           Move SysYr to yy
           Call      CvtJul
           Move      Juldays to TodaysJulDate           
           return
           
CheckAge
.This function checks the age of the current LR/Order number which is called                      
           Goto SkipToNextOrd if equal  
           CMatch "x" to OStat       //Cancelled Pending order ?
           Goto SkipToNextOrd if equal  
           CMatch "1" to OStat       //LCR order ?
           Goto SkipToNextOrd if equal  
           CMatch "z" to OStat       //Cancelled LCR order ?
           Goto SkipToNextOrd if equal  

           Display *P10:13,"                                                                           "     
           Display *P10:12,"Checking the age of Order Number: ",OLRN
           
           Move OoDtem TO mm
           Move OoDted TO dd
           Move OoDtey to yy
           Move OoDtec to cc
           Call CvtJul
           Move JulDays to OrdDate
           Clear DaysOld
           Move TodaysJulDate to DaysOld
           Sub OrdDate from DaysOld
           Display *P10:13,"The age is ",DaysOld," days."          
           Compare "730" to DaysOld
           If Less
                      Goto WriteCardLine
           EndIf          
           Goto SkipToNextOrd           
           return  //we should never get here
                      
WriteCardLine
.In order for the website to be able to build a datacard we will need the following information:

           Display *P10:16,"Writing the data from Datacard: ",LSTNUM 
           Display *P10:17,"Into Webfile."
           Pause "1"
           Add c1 to CountCards
           Write Webfile,SEQ;*CDFON,LSTNUM,"Begin_Datacard"
           Write Webfile,SEQ;*CDFON,DATVARS
                      
.For Now I'm just going to build the file with all the linked databases and then I'll weed 
.out variables I don't need later if it appears there's a benefit to that.
           
           Call PrintTextVars
           Call PrintNAddVars
           Call PrintNArrVars
           Call PrintNCatVars
           Call PrintNSltVars
           Call PrintNSelVars
           Call PrintNSrcVars
.           Call PrintNMSCVars
.           Call PrintNMDCCVARS
           Call PrintNQRCVars

.don't know if I need this...
.           Move LSTNUM to NModLIST
.           Call NModKEY
.           Write Webfile,SEQ;*CDFON,NMODVARS

.Clear Our Displays
           Display *P10:16,"                                                                        "      
           Display *P10:17,"                                                                        "                    
	   Write Webfile,SEQ;*CDFON,LSTNUM,"End_Datacard"        
           Goto SkipToNextDC
           
           return  //we should never get here

PrintTextVars
.This prints all the text needed to build the DB into the web file
           Move "00" to LoopNum2Dig
           Write Webfile,SEQ;*CDFON,LSTNUM,"Begin_TextVars"
           loop                      
                      PackKey NTxtFld,LSTNUM,LoopNum2Dig
                      Rep ZFill,NTxtFld
                      Call NTxtKey
                      If not over
                                 Write Webfile,SEQ;*CDFON,LSTNUM,NTXTTEXT
                      Endif
                      until (LoopNum2Dig == 99)
                      Add "01" to LoopNum2Dig
           repeat
           Write Webfile,SEQ;*CDFON,LSTNUM,"End_TextVars"
           
           return
           
PrintNAddVars
.This prints all the NAddVars needed to build the DB into the web file

	Write Webfile,SEQ;*CDFON,LSTNUM,"Begin_Addressing" 
	pack      NADDFLD1,"01X",LSTNUM
	move      "D.Load-NADDAIM",Location
	pack      KeyLocation,"Key: ",NADDFLD1
	call      NADDAIM
	loop
		until over
		packkey NREFFLD, "A", NAddNum
		Call NREFKEY
		packkey NModFLD, NAddDESC
		Call NMODKEY
		Write Webfile,SEQ;*CDFON,NAddVARS,NREFVARS,NMODDESC
		move      "D.Load-NADDKG",Location
		pack      KeyLocation,"Key: ",NADDFLD1
		call      NADDKG
        repeat		
	Write Webfile,SEQ;*CDFON,LSTNUM,"End_Addressing"
	return
           
.Ver 2.00 -- This is old code, commenting out. -- RVW
.	Move "000" to LoopNum3Dig
.	Write Webfile,SEQ;*CDFON,LSTNUM,"Begin_Addressing" 
.	loop
.		until (LoopNum3Dig == 999)
.		PackKey NAddFld,LSTNUM,LoopNum3Dig 
.		Rep ZFill,NAddFld
.		Call NAddKey
.		If not over
.			packkey NREFFLD, "A", NAddNum
.			Call NREFKEY
.			packkey NModFLD, NAddDESC
.			Call NMODKEY
.			Write Webfile,SEQ;*CDFON,NAddVARS,NREFVARS,NMODDESC
.		Endif                     
.		Add "001" to LoopNum3Dig
.	repeat                      
.	Write Webfile,SEQ;*CDFON,LSTNUM,"End_Addressing"
.Ver 2.00 -- This is the end of the old code. -- RVW  	   


PrintNArrVars
.This prints all the NArrVars needed to build the DB into the web file
           Move "000" to LoopNum3Dig
           Write Webfile,SEQ;*CDFON,LSTNUM,"Begin_Arrangements"
           loop                      
                      PackKey NArrFld,LSTNUM,LoopNum3Dig 
                      Rep ZFill,NArrFld
                      Call NArrKey
                      If not over
				packkey NREFFLD, "R", NARRNUM
				Call NREFKEY
                                Write Webfile,SEQ;*CDFON,NArrVARS,NREFVARS
                      Endif
                      until (LoopNum3Dig == 999)
                      Add "001" to LoopNum3Dig
           repeat
           Write Webfile,SEQ;*CDFON,LSTNUM,"End_Arrangements"
 
           return

PrintNCatVars
.This prints all the NCatVars needed to build the DB into the web file
           Move "000" to LoopNum3Dig
           Write Webfile,SEQ;*CDFON,LSTNUM,"Begin_Categories"
           loop                      
                      PackKey NCatFld,LSTNUM,LoopNum3Dig
                      Rep ZFill,NCatFld
                      Call NCatKey
                      If not over
                      	 	packkey NREFFLD, "T", NCatCODE,NCatNUM
				Call NREFKEY
                                Write Webfile,SEQ;*CDFON,NCatVARS,NREFVARS
                      Endif
                      until (LoopNum3Dig== 999)
                      Add "001" to LoopNum3Dig
           repeat
           Write Webfile,SEQ;*CDFON,LSTNUM,"End_Categories"
                    
           return
           
PrintNSltVars
.This prints all the NSltVars needed to build the DB into the web file
           Move "000" to LoopNum3Dig
           Write Webfile,SEQ;*CDFON,LSTNUM,"Begin_Selections"
           loop                      
                      PackKey NSltFld,LSTNUM,LoopNum3Dig 
                      Rep ZFill,NSltFld
                      Call NSltKey
                      If not over
                                Packkey NREFFLD, "L", NSltNUM
				Call NREFKEY
		         	packkey NMODFLD, NSLTDESC
			        Call NMODKEY
                                Write Webfile,SEQ;*CDFON,NSltVARS,NREFVARS,NMODVARs
                      Endif
                      until (LoopNum3Dig == 999)
                      Add "001" to LoopNum3Dig
           repeat
           Write Webfile,SEQ;*CDFON,LSTNUM,"End_Selections"
         
           return

PrintNSelVars
.This prints all the NSltVars needed to build the DB into the web file
           Move "0000" to LoopNum4Dig
           Write Webfile,SEQ;*CDFON,LSTNUM,"Begin_Members"
           loop                      
                      PackKey NSelFld,LSTNUM,LoopNum4Dig 
                      Rep ZFill,NSelFld
                      Call NSelKey
                      If not over
                                packkey NMODFLD, NSELDESC
			        Call NMODKEY
			        Call Trim using NSelsName
			        Write Webfile,SEQ;*CDFON,NSelVARS,NMODVARs
                      Endif
                      until (LoopNum4Dig == 999)
                      Add "0001" to LoopNum4Dig
           repeat
           Write Webfile,SEQ;*CDFON,LSTNUM,"End_Members"
         
           return

PrintNSrcVars
.This prints all the NSrcVars needed to build the DB into the web file

	Write Webfile,SEQ;*CDFON,LSTNUM,"Begin_Sources" 
	pack      NSRCFLD1,"01X",LSTNUM
	move      "D.Load-NSRCAIM",Location
	pack      KeyLocation,"Key: ",NSRCFLD1
	call      NSRCAIM
	loop
		until over
		packkey NREFFLD, "S", NSrcNum
		Call NREFKEY
		Write Webfile,SEQ;*CDFON,NSrcVARS,NREFVARS
		move      "D.Load-NSrcKG",Location
		pack      KeyLocation,"Key: ",NSrcFLD1
		call      NSrcKG
        repeat		
	Write Webfile,SEQ;*CDFON,LSTNUM,"End_Sources"
	return
           
.Ver 2.00 -- This is old code, commenting out. -- RVW
.	Move "000" to LoopNum3Dig
.           Write Webfile,SEQ;*CDFON,LSTNUM,"Begin_Sources"
.           loop                      
.                      PackKey NSrcFld,LSTNUM,LoopNum3Dig
.                      Rep ZFill,NSrcFld
.                      Call NSrcKey
.                      If not over
.                                Packkey NREFFLD, "S", NSrcNUM
.				Call NREFKEY
.                                Write Webfile,SEQ;*CDFON,NSrcVARS,NREFVARS
.                      Endif
.                      until (LoopNum3Dig == 999)
.                      Add "001" to LoopNum3Dig
.           repeat
.           Write Webfile,SEQ;*CDFON,LSTNUM,"End_Sources"

PrintNMSCVars
.This prints all the NMSCVARSVars needed to build the DB into the web file
           loop                      
                      PackKey NMscFld,LSTNUM
                      Rep ZFill,NMscFld  
                      Call NMscKey
                      until over
                      Write Webfile,SEQ;*CDFON,NMscVARS    
           repeat      

           return
           
PrintNMDCCVARS
.This prints all the NMDCCVARS needed to build the DB into the web file
           PackKey NMdccFld,LSTNUM
           Rep ZFill,NMdccFld
           Call NMdccKey
           loop                      
                      Write Webfile,SEQ;*CDFON,NMdccVars
                      Call NMDCCKS
                      until (NMdccNum <> LSTNUM)
           repeat
           
           return


PrintNQRCVars
.This prints all the NQRCVars needed to build the DB into the web file
           Write Webfile,SEQ;*CDFON,LSTNUM,"Begin_Recommendations"
           loop                      
                      PackKey NQrcFld,LSTNUM,LoopNum3Dig 
                      Rep ZFill,NQrcFld
                      Call NQrcKey
                      If not over
                                 Write Webfile,SEQ;*CDFON,NQrcVARS
                      Endif
                      until (LoopNum3Dig == 999)
                      Add "001" to LoopNum3Dig
           repeat
           Write Webfile,SEQ;*CDFON,LSTNUM,"End_Recommendations"
         
           return
          
OpenOrdDB  
.This opens the orders database at the beginning of the order file
           Trap FileMissing if io
           Move "Couldn't open the order file" to ErrorMessage
           Open Nordfle2,"Ninord.aam|NINS1:502",read
           if (FileFlag == 1)
                      Display *P10:22,"Quitting"
                      Stop
           Endif
           
           return           

FileMissing
           trapclr   io
           Display *P10:21,ErrorMessage
           move c1 to FileFlag

           return
