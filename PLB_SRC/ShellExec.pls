
.......................................................
. routine to execute a program as admin on vista...
.         By Matthew Lake
.
. API posted on forums by David Gabler.
.
.ShellExecute(0, // owner window
.L"runas",
.L"C:\\Windows\\Notepad.exe",
.0, // params
.0, // directory
.SW_SHOWNORMAL); 
Param     Dim       ^

Shell     Routine   Param
          Display   *p10:10,param,*w10
          pause     "20"
         keyin     S$CMDLIN
          CALL      Runas using Param
          return

ShellExecute PROFILE shell32.dll,ShellExecuteA,int4,int4,DIM,DIM,DIM,DIM,INT4


RunAs FUNCTION
CMDLIN    DIM       ^
          ENTRY
RET                 INTEGER   4
HWND                INTEGER   4
verb                INIT      "runas",0
ProgFile  DIM       250
Parameters          DIM       250
Directory DIM       260
SW_SHOWNORM         INTEGER   4,"1"
SW_MINIMIZE         INTEGER   4,"6"
SW_Param  INTEGER   4
null                INTEGER   1
.................................................................................................

          CMATCH    "!",CMDLIN
          IF EQUAL
           BUMP     CMDLIN
           MOVE     SW_MINIMIZE,SW_Param
          ELSE
           MOVE     SW_SHOWNORM,SW_Param
          ENDIF
.
          PARSEFNAME          CMDLIN,ProgFile
          BUMP      CMDLIN
          IF NOT EOS
          MOVE      CMDLIN,Parameters
          ENDIF
          PATH      CURRENT,Directory
          PACK      Parameters,Parameters,null
          PACK      ProgFile,ProgFile,null
          PACK      Directory,Directory,null
          APICALL   ShellExecute giving ret using hwnd:
                              verb:
                              ProgFile:
                              Parameters:
                              Directory:
                              SW_Param
          
          IF ( ret > 32 )
           SETFLAG NOT OVER
          ELSE
           SETFLAG OVER
          ENDIF
          
          FUNCTIONEND USING ret
          
          Return          
          
.         keyin     S$CMDLIN

