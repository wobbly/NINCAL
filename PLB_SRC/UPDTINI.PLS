...............................................................................
. 
. UPDTINI - GetCommandLine and WritePrivateProfileString
.
. 04/26/2000 Gerhard Weiss      BeneSys Inc.
.                               2075 W. Big Beaver Rd.
.                               Troy MI 48084-3446
.                               Phone: (248) 822-4200
.                               Fax  : (248) 822-4224
.                               email: gerhard@benesysinc.com
.                               email: geweiss@bigfoot.com
.
.
. This is an Example program on how to use the GetCommandLine and 
. WritePrivateProfileString Window API's.
.
. The GetCommandLine shows the command line that started up PLBWIN. This will
. also have the INI file name if used. Example Command Line
.   "T:\SUNBELT\CODE\plbwin.exe" -i PlbBenTT
.
. The WritePrivateProfileString will write to an INI file.
.
. We have not got around to combining the two functions yet. In this program
. they are just stand alone.
. Here are Some ideas you can do a CLOCK INI on "PLB_SYSTEM" which in my example
. will return T:\SUNBELT\CODE then append the ini file of PlbBenTT.INI that was
. taken from the GetCommandLine then use the WritePrivateProfileString to 
. write out variables.
. 
...............................................................................
. Sunbelt Equates Located in \sunbelt\code ----------------------------
.
..$PLBDEF   EQU      1               .1 ==> include KEYPRESS defines    8.2
..$PLBCONST EQU      1               .1 ==> include KEYPRESS CONST      8.2
..$PLBALERT EQU      1               .1 ==> include ALERT TYPE INTEGERs 8.3f
..         INC       PLBEQU.INC      .GUI designer support equates
..         INC       PLBMETH.INC     .PLB METHOD support definitions
.
cccGetCommandLine EQU 1,REDEFINE
cccWritePrivateProfileString EQU 1,REDEFINE
         INC       WinBase.API
................................................................................
.
CCMENU   DIM       %12   .Return to menu name
.
................................................................................
ANS      DIM       1
................................................................................
.
................................................................................
lngRetVal INTEGER 4
.
.
strTitle INIT "GetCommandLine and WritePrivateProfileString"
         DISPLAY   *ES,strTitle,*N
.
strCommandLine DIM 250
.
         DISPLAY   *P1:3,*EF:
                   "GetCommandLine",*N
.
         CALL      GetCommandLine USING strCommandLine
         DISPLAY   strCommandLine
.
...............................................................................
strWritePrivateProfileStringSection DIM 250   // address of section name 
strWritePrivateProfileStringKey DIM 250       // address of key name 
strWritePrivateProfileStringString DIM 250    // address of string to add 
strWritePrivateProfileStringFile DIM 250      // address of initialization filename 
.
.
. INI FileName 
.
. [Acrobat PDFWriter]
. PDFFilename=C:\SUNBELT\GW.PDF
.
         MOVE      "Gerhard Section",strWritePrivateProfileStringSection
         MOVE      "PDFFilename",strWritePrivateProfileStringKey
         MOVE      "C:\SUNBELT\GW.PDF",strWritePrivateProfileStringString
.
.
         MOVE      "PLB_SYSTEM",strWritePrivateProfileStringFile
         CLOCK     INI,strWritePrivateProfileStringFile
.
.                                       Searching for the ini is not 100%
.                                       but it is close enough. Some more
.                                       thought will have to be put into this.
.                                       Our CommandLine looked like this
.                                       "T:\SUNBELT\CODE\plbwin.exe" -i PlbBenTT
.                                       You could have other options after 
.                                       the -i that could cause problems.
.                                       You will need to PARSE it out.
         LOWERCASE strCommandLine
         SCAN      "-i ",strCommandLine
         IF        EQUAL
           BUMP      strCommandLine,3
           PACK      strWritePrivateProfileStringFile WITH strWritePrivateProfileStringFile:
                       "\",strCommandLine,".ini"
         ELSE
           PACK      strWritePrivateProfileStringFile WITH strWritePrivateProfileStringFile:
                       "\plbwin.ini"
         ENDIF
.
         DISPLAY   *N,"INI=",strWritePrivateProfileStringFile,*N
.
         DISPLAY   *N,"WritePrivateProfileString"
.
WPPSSection
         KEYIN     "Section: ",*DVEDIT=strWritePrivateProfileStringSection
         GOTO      TstWritePrivateProfileStringEnd IF F1
.
WPPSKey
         KEYIN     "Key    : ",*DVEDIT=strWritePrivateProfileStringKey
         GOTO      WPPSSection IF F1
.
WPPSString
         KEYIN     "String : ",*DVEDIT=strWritePrivateProfileStringString
         GOTO      WPPSKey IF F1
.
WPPSFile
         KEYIN     "File   : ",*DVEDIT=strWritePrivateProfileStringFile
         GOTO      WPPSString IF F1
.
         DISPLAY   "Before WritePrivateProfileString"
         KEYIN     "Tap ENTER ",ANS
         GOTO      WPPSFile IF F1
.
         CALL      WritePrivateProfileString USING lngRetVal:
                     strWritePrivateProfileStringSection:
                     strWritePrivateProfileStringKey:
                     strWritePrivateProfileStringString:
                     strWritePrivateProfileStringFile
.
         DISPLAY   "After WritePrivateProfileString lngRetVal=",lngRetVal
         GOTO      WPPSSection
.
TstWritePrivateProfileStringEnd
.
.
CHAINOUT MATCH     " ",CCMENU
         STOP      IF EOS
         CHAIN     CCMENU
.
.

