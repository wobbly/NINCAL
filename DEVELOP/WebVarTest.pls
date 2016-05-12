PC EQU 0

.Include some files

           Include Common.inc
           Include Cons.inc           
           
           Include NTxtdd.inc
           Include NAdddd.inc
           Include NArrdd.inc 
           Include NCatdd.inc
           Include NSeldd.inc
           Include NSltdd.inc
           Include NSrcdd.inc
           Include NModdd.inc
           Include NMDCMSCdd.INC
           Include NMDCCATdd.inc
           Include NDatdd.inc   
           Include NQrcdd.inc
           Include NRefdd.inc
           Include NOrddd.inc


Release   Init      "1.00"    RVW  Initial release
reldate   Init      "2015 July 15th"
           
.Initialize some variables

ErrorMessage          Dim        150
TodaysJulDate         Form       5
DaysOld               Form       5
OrdDate               Form       5
Akey2                 Init      "02R"
SysMo                 Dim        2
SysDay                Dim        2
SysYr                 Dim        2
Date                  Dim        8
WebFile               File     
FileFlag              Form       1
LoopNum2Dig           Form       2
LoopNum3Dig           Form       3
LoopNum4Dig           Form       4

.This is for not going through the entire file when we're only doing a couple lists
CountCards            Form       1
CountOrders           Form       1
           move c0 to CountCards

.Print the release date and change the background to blue

           Call Paint
RebuildDB       
.Go through the datacard file starting at the first record
           Call OpenWebFile
           Call OpenDCDB
           Call OpenOrdDB  
           Call GetToday
           move "006062" to NDatFld              //Set the starting point           
SkipToNextDC
.We're gonna get our data card record and let the user know what number we're on.
           CALL      NDATKS
           If over 
.           If (CountCards > 2)   //for testing
                      Goto GetOut
           Endif
           If (NDATWEB == "1")
                      Goto SkipToNextDC
           Elseif (ELstCde == " ")
                      Goto SkipToNextDC
           Elseif (NDatOff == "1")
                      Goto SkipToNextDC
           Endif
           Display *P10:8,"I'm checking to see if List Number: ",LSTNUM 
           Display *P10:9,"has been used in the past 2 years."
.Let's get an Order for the DataCard.  First we need to put the required Data into the order variables                      
           Display *P10:12,"                                                                               "      
           Display *P10:13,"                                                                               "      //Clear the outputs                                  
           Clear     NOrdFld1
           Pack      NOrdFld2,Akey2,LstNum  //Put the LSTNUM from the datacard record in the search index for the order
           Clear     NOrdFld3
           Clear     NOrdFld4
           Clear     NOrdFld6
           Clear     NOrdFld7
           Clear     NOrdFld8                                 
           Call NOrdLast                    //Get the last record (most recent) in the order file that matches
           If over
                      Goto SkipToNextDC     //If there are no orders go to the nextdata card
           Endif
           Goto CheckAge
           move "0" to CountOrders           
           loop
SkipToNextOrd           
                      until (Countorders > 2)    //If the two most recent data cards are too old all the others are too, so skip to next DC
                      Call NOrdKGP               //Get the previous record that matches
                      Add "1" to CountOrders
                      Goto CheckAge
           repeat
           Goto SkipToNextDC


           
GetOut           
           Display *P10:17,"Finished writing the Webcards DB File.  Goodbye!"
           Pause "5"

           Close NDatFile
           Close NOrdFile
           Close Webfile
           Close NTxtFile
           Close NAddFile           
           Close NSelFile     
           Close NSltFile     
           Close NQrcFile           
           Close NMdccFile           
           Close NArrFile           
           Close NCatFile           
           Close NSrcFile           
           Close NMscFile           
           
           Stop


           Include WebDBFunctions.inc
           Include NRefio.inc
           Include Nordio.inc
           Include Ndatio.inc
           Include NTxtio.inc
           Include NAddio.inc
           Include NModio.inc
           Include NArrio.inc
           Include NCatio.inc
           Include NSelio.inc
           Include NSltio.inc
           Include NSrcio.inc
           Include NMDCMscio.inc
           Include NMDCCATIO.inc
           Include NQrcio.inc
           Include Comlogic.inc