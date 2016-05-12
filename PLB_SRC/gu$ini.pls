.****************************************************************************
.         GU$INI.PLS
.         Routines for reading/writing ini files
.         David McOrist - Phoenix Software
.****************************************************************************
. Version   Date   Who Modification
.-------- -------- --- -----------------------------------------------------
.# .00    27/04/01 McO Create
.****************************************************************************
. GET_PLBWIN_INI_NAME ROUTINE
. To use:
.         CALL      "GU$INI;GET_PLBWIN_INI_NAME" USING <file name>
. <file name> is returned with the fully qualified name of the ini file.
.............................................................................
          INCLUDE   SYSDATA.UDA
INI_FILE_NAME       DIM       ^
Wk_Name             DIM       256
Wk_Line_A           DIM       256
Wk_Line_B           DIM       256
OPEN_FAIL           FORM      1
Indx                FORM      1
PLBDir              DIM       128
Wk_FILE             FILE
D_2                 DIM       2
F_3                 FORM      3
.............................................................................
GET_PLBWIN_INI_NAME ROUTINE   INI_FILE_NAME
. Get the the command line used to start the session
          CALL      GET_COMMAND_LINE USING Wk_Line_A
. If the -i option was specified extract the name, otherwise use 'PLBWIN.INI'
          LOWERCASE Wk_Line_A,Wk_Line_B
          SCAN      "-i" IN Wk_Line_B
          IF EQUAL
             MOVEFPTR  Wk_Line_B TO F_3
             RESET     Wk_Line_A TO F_3
             UNPACK    Wk_Line_A INTO D_2,Wk_Line_B
             LOOP
                MATCH     " " TO Wk_Line_B        /remove leading spaces
             WHILE EQUAL AND NOT EOS
                BUMP      Wk_Line_B
             REPEAT
             MOVE      Wk_Line_B TO Wk_Line_A
             SCAN      " " IN Wk_Line_A
             IF EQUAL
                LENSET     Wk_Line_A
                RESET      Wk_Line_A
             ENDIF
          ELSE
             MOVE   "PLBWIN.INI" TO Wk_Line_A
          ENDIF
          CHOP      Wk_Line_A TO Wk_Name
          SCAN      "\" IN Wk_Name                          ."
          IF EQUAL
             RESET     Wk_Name
             MOVE      Wk_Name TO INI_FILE_NAME
          ELSE
             CALL      FIND_LOCATION_OF_INI
          ENDIF
          RETURN
.............................................................................
. Try to open the file in 1. The current directory
.                         2. The windows directory
.                         3. The PLB_SYTEM directory
. The first one found is returned in INI_FILE_NAME. If not found, the field
. is cleared.
FIND_LOCATION_OF_INI
          CLEAR     Indx
          LOOP
             ADD       "1" TO Indx
             PERFORM   Indx OF Get_CurrentDir,Get_WindowsDir,Get_PLBDir
             CLEAR     OPEN_FAIL
             TRAP      OPEN_FAIL IF IO
             OPEN      Wk_FILE,INI_FILE_NAME
             TRAPCLR   IO
          REPEAT IF (OPEN_FAIL AND Indx<3)
          IF (OPEN_FAIL)
             CLEAR     INI_FILE_NAME
          ELSE
             CLOSE     Wk_FILE
          ENDIF
          RETURN
.............................................................................
. First try the current directory
Get_CurrentDir
          PACK      INI_FILE_NAME FROM ".\",Wk_Name
          RETURN
.............................................................................
. Next try the windows directory
Get_WindowsDir
          GETINFO   SYSTEM,SysData_D
          UNPACK    SysData_D INTO SysData_VL
          CHOP      WinDir,WinDir
          PACK      INI_FILE_NAME FROM WinDir,"\",Wk_Name
          RETURN
.............................................................................
. Next try the plbwin directory (assume that it is in the PLB_SYSTEM dir)
Get_PLBDir
          PACK      PLBDir FROM "PLB_SYSTEM"
          CLOCK     INI,PLBDir
          ENDSET    PLBDir
          CMATCH    "\" TO PLBDir
          IF EQUAL
             RESET     PLBDir
          ELSE
             RESET     PLBDir
             PACK      PLBDir FROM PLBDir,"\"
          ENDIF
          PACK      INI_FILE_NAME FROM PLBDir,Wk_Name
          RETURN
............................................................................
OPEN_FAIL
          SET       OPEN_FAIL
          RETURN
.****************************************************************************
. GET_COMMAND_LINE ROUTINE
. To use:
.         CALL      "GU$INI;GET_COMMAND_LINE" USING <command line>
. <command line> returns the command line used to start the session
.                (dim field).
.............................................................................
Out_CommandLineString         DIM ^
GetCommandLine      PROFILE   !Kernel32,GetCommandLineA,Int4
LStrlen             PROFILE   !Kernel32,lstrlenA,Int4,Int4
MoveMemory          PROFILE   !Kernel32,RtlMoveMemory,None,Dim,Int4,Int4
OutString_Len       INTEGER   4        .Physical Lenght of String
CommandLine_Lpt     INTEGER   4         Pointer to command line string
CommandLine_Len     INTEGER   4         Length of string
.............................................................................
GET_COMMAND_LINE    ROUTINE Out_CommandLineString
         WINAPI    GetCommandLine GIVING CommandLine_Lpt
         WINAPI    LStrlen GIVING CommandLine_Len USING CommandLine_Lpt
         MOVEPLEN  Out_CommandLineString,OutString_Len
         IF (CommandLine_Len>OutString_Len)      .cannot be longer than
             MOVE      OutString_Len,CommandLine_Len      .the DIM field
         ENDIF
         WINAPI    MoveMemory USING Out_CommandLineString:
                                    CommandLine_Lpt,CommandLine_Len
         SETLPTR   Out_CommandLineString,CommandLine_Len
         RETURN
.****************************************************************************
. GET_FROM_INI Routine
. To use:
.         CALL      "GU$INI;GET_FROM_INI" USING <ini file name>:
.                                               <section name>:
.                                               <key name>:
.                                               <value>
. <ini file name>= the name of the INI file (dim field or literal)
. <section name> = section in the INI file  (dim field or literal)
. <key name>     = key name in the INI file (dim field or literal)
. <value>        = returned value (dim field)
. IF not found, <value> is cleared and the OVER flag is set
.****************************************************************************
.GetPrivateProfileString       PROFILE   !kernel32,GetPrivateProfileStringA:
.                                        INT2,DIM,DIM,DIM,DIM,INT2,DIM
GetPrivateProfileString       PROFILE   !kernel32,GetPrivateProfileStringA:
                                        INT2,DIM,DIM,DIM,PDIM,INT2,DIM
WritePrivateProfileString     PROFILE   !kernel32,WritePrivateProfileStringA:
                                        INT2,DIM,DIM,DIM,DIM
INI_FileName        DIM       128
SectionName         DIM       128
KeyName             DIM       128
String_D            DIM        ^
....................
.porttest dim       %%
....................
Value               DIM       512
Result              FORM      ^
NotFound            INIT      "*** Not found",0X00
iSize               INTEGER   2         initial size of return var
rSize               INTEGER   2         result size of return var
Nul                 INIT      0x00
iR                  INTEGER   2
.............................................................................
GET_FROM_INI        ROUTINE   INI_FileName,SectionName,KeyName:
                              String_D
          MOVEPLEN  String_D,iSize
          CHOP      INI_FileName,INI_FileName
          PACK      INI_FileName,INI_FileName,Nul
          CHOP      SectionName,SectionName
          PACK      SectionName FROM SectionName,Nul
          CHOP      KeyName,KeyName
          PACK      KeyName,KeyName,Nul
          CLEAR     String_D
          WINAPI    GetPrivateProfileString GIVING rSize USING SectionName:
                           KeyName,NotFound,String_D,iSize,INI_FileName
          MATCH     "*** Not found" TO String_D
          IF EQUAL
             CLEAR     String_D
             SETFLAG   OVER
          ELSE
             SETLPTR   String_D,rSize
             SETFLAG   NOT OVER
          ENDIF
          RETURN
.GET_FROM_INI        ROUTINE   INI_FileName,SectionName,KeyName:
.                              porttest
.          MOVEPLEN  porttest,iSize
.          CHOP      INI_FileName,INI_FileName
.          PACK      INI_FileName,INI_FileName,Nul
.          CHOP      SectionName,SectionName
.          PACK      SectionName FROM SectionName,Nul
.          CHOP      KeyName,KeyName
.          PACK      KeyName,KeyName,Nul
.          CLEAR     porttest
.          WINAPI    GetPrivateProfileString GIVING rSize USING SectionName:
.                           KeyName,NotFound,porttest,iSize,INI_FileName
.          MATCH     "*** Not found" TO porttest
.          IF EQUAL
.             CLEAR     porttest
.             SETFLAG   OVER
.          ELSE
.             SETLPTR   porttest,rSize
.             SETFLAG   NOT OVER
.          ENDIF
.          RETURN
.****************************************************************************
. WRITE_TO_INI Routine
. To use:
.         CALL      "GU$INI;WRITE_TO_INI" USING <ini file name>:
.                                               <section name>:
.                                               <key name>:
.                                               <value>:
.                                               <result>
. <ini file name>= the name of the INI file (dim field or literal)
.                  If the name does not contain the path, it defaults
.                  to the windows direcory.
.                  If the file by does not exist, it is created.
. <section name> = section in the INI file  (dim field or literal)
.                  If the section does not exist, it is created.
. <key name>     = key name in the INI file (dim field or literal)
.                  If the key does not exist, it is created.
.                  If this is NULL (cleared) then the section is deleted
.                  from the INI file
. <value>        = value to write (dim field or literal)
.                  If value is NULL (cleared) then the key is deleted.
. <result>       = Result flag (form field)
.                  returned as 0 if write OK
.                  returned as 1 if write failed
. Notes:
. If the key
.............................................................................
WRITE_TO_INI        ROUTINE   INI_FileName,SectionName,KeyName:
                              Value,Result
          CHOP      INI_FileName,INI_FileName
          PACK      INI_FileName,INI_FileName,Nul
          CHOP      SectionName,SectionName
          PACK      SectionName FROM SectionName,Nul
          CHOP      KeyName,KeyName
          PACK      KeyName,KeyName,Nul
          CHOP      Value,Value
          PACK      Value,Value,Nul
          WINAPI    WritePrivateProfileString GIVING iR USING SectionName:
                                             KeyName,Value,INI_FileName
          IF (iR)
             CLEAR     Result
          ELSE
             SET       Result
          ENDIF
          RETURN
.****************************************************************************
