
.                                          Author : Festus Redelinghuys

.   This program requires the ActiveX control "PlbUtilities.Dll".
.   I have written this ActiveX control, so it is a free control with no royalties at all.
.   If you have suggestions for some utilties to be added please let me know at Festus@CyBry.com

.   Register this ActiveX control before you use it.

Utilities          Control   CLASS="PlbUtilities.uShell"
Window             Window
hWnd               Integer   4
Path               Dim       260
F1                 Form      1
Title              Init      "Get some folder....."

.        Create some window. We need this handle to pass to the GetFolder method
.        so that if this window is killed then the folder selection window will also be killed
         Create              Window=0:200:0:200,TITLE="Test Window"
         GetProp             Window,HWND=hWnd                          . Get handle

.        Select a folder
         Create              Window;Utilities=2000:2000:2000:2000      . Create the control
         Utilities.GetFolder GIVING Path USING hWnd,Title              . Select a fodler

         Alert               Stop,Path,F1,"You have selected the path :"

         Stop                                                          . Blah blah blah
