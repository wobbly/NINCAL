
.                                          Author : Festus Redelinghuys

.   This program requires the ActiveX control "PlbUtilities.Dll".
.   I have written this ActiveX control, so it is a free control with no royalties at all.
.   If you have suggestions for some utilties to be added please let me know at Festus@CyBry.com

.   Register this ActiveX control before you use it.



.   Get all the function names exported by a Dll.

F1                 Form      1

DllName            Init      "Kernel32.Dll"                                    . [in/out] Dll name to look up
DllPath            Init      "C:\Winnt\System32"                               . [in/out] Path to look for Dll
FunctionName       Dim       64                                                . [out]    Function Name selected
StatusText         Dim       100                                               . [out]    Status text

         WinHide
         CallS     "GetExpNames;GetFunctionName",DllName:                      . [in/out] Dll name to look up
                                                 DllPath:                      . [in/out] Path to look for Dll
                                                 FunctionName:                 . [out]    Function Name selected
                                                 StatusText                    . [out]    Status text
         If        Equal
          ClipSet  FunctionName
          Alert    Stop,FunctionName,F1,"Success - Function name copied to ClipBoard"
         Else
          Alert    Stop,StatusText,F1,"ERROR! - Failed to obtain Function name"
         Endif
         Stop



