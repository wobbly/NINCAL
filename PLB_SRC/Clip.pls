.
. Clip.LM - Clipboard using a EDITTEXT Loadmod
.
. The routines in this loadmod keeps track of what to write to the clipboard.
. It uses a EDITTEXT, which allows for dynamic memory allocation.
. The Fields are separated with a Tab Character and Records with a Del 
. Character. A drawback of using a EDITTEXT is it has around a 30,000 maximum
. character limit.
.
. Example:
. LAST     DIM 20
. FIRST    DIM 20
. MIDDLE   DIM 1
. 
.          CALL      "Clip;AppendVStr" USING "Last Name"
.          CALL      "Clip;AppendVStr" USING "First Name"
.          CALL      "Clip;AppendVStr" USING "Middle"
.          CALL      "Clip;AppendEOR"
.          LOOP
.            READ      FILE,SEQ;LAST,FIRST,MI
.            UNTIL     OVER
.            CALL      "Clip;AppendRStr" USING LAST
.            CALL      "Clip;AppendRStr" USING FIRST
.            CALL      "Clip;AppendRStr" USING MIDDLE
.            CALL      "Clip;AppendEOR"
.          REPEAT
.          CALL      "Clip;Set"
.
. The example above is pretty basic. Clip.LM can be used to export information 
. from a LISTVIEW to the clipboard, which in turn lets the user paste it into
. other applications (i.e. Excel Spreadsheet). The user was able to select 
. certain rows in the LISTVIEW then invoke a routine that would loop through 
. the selected rows and call routines inside of Clip.LM.
.
. 03/08/01 GW Wrote Loadmod
. 03/13/01 GW Added alert message stating the clipboard was truncated if the 
.        EDITTEXT went over its maximum character limit.
. 12/11/01 GW AppendEOR: CALL AppendInit if bytFldSeparatorEnabled=0
. 02/12/02 GW Paste: Added the Paste routine
. 02/12/02 GW Get: Added the Get routine
.             Paste: Modified to paste multiple lines. Replace special SendKeys
.                    Characters.
. 03/11/02 GW Paste: Removing Dashes from Soc. Sec. number.
. 02/03/04 GW GetRecord, GetClear: Added both routines
. 12/08/05 GW Paste: Doing a Replace from uppercase to lowercase. This is done
.        because the runtime does a replace to uppercase.
.
         INC       PlbEquD.INC
cccSendMessage EQU 1,REDEFINED
cccSetPropLParamRStr EQU 1,REDEFINED
         INC       WinUser.API
.
...............................................................................
. prgmisc.inc variables
. ALERT Dialog Boxes
.         PACK      strAlertMess WITH "",strAlertCR
.         ALERT     STOP,strAlertMess,curAlertRetVal,strAlertTitle
.
strAlertMess DIM 255          .Alert Dialog Message
strAlertCR INIT 0x7F          .Alert Dialog Carriage Return (Del Char)
curAlertRetVal FORM 2         .Alert Dialog Return Value
strAlertTitle DIM 40
...............................................................................
.
cccShowTxtClipEnabled EQU $Off     .$On to show Sort window. Used for testing
cccShowByteEnabled EQU $Off        .$On to show Byte count. Used for testing
.
txtClip  EDITTEXT
lngClipHWnd INTEGER 4
lngRetVal INTEGER 4
rlngRetVal INTEGER ^
.
bytFldSeparatorEnabled INTEGER 1
bytRecSeparatorEnabled INTEGER 1
strFldSeparator INIT 011           .Field Separator (Tab Character)
strRecSeparator INIT 0x7F          .EDITTEXT Record Separator (Del)
.
...............................................................................
. AppendRStr - Append a ByRef String
. Pass a Character String Variable only. This allows for very big fields.
.
rstrAppend DIM ^
AppendRStr ROUTINE rstrAppend
         CALL      AppendInit
         CALL      InsertItemTxtClip USING rstrAppend
         RETURN
.
...............................................................................
. AppendVStr - Append a ByVal String
. Pass a Character String Variable or Literal. Size is limited to 50 bytes.
.
vstrAppend DIM 50
AppendVStr ROUTINE vstrAppend
         CALL      AppendRStr USING vstrAppend
         RETURN
.
...............................................................................
. AppendVStr - Append a ByRef Currency (FORM)
. Pass a Numeric Variable only.
.
rcurAppend FORM ^
AppendRCur ROUTINE rcurAppend
         CALL      AppendInit
         SQUEEZE   rcurAppend,vstrAppend     .Remove leading blanks
         CALL      InsertItemTxtClip USING vstrAppend
         RETURN
.
...............................................................................
. AppendInit - Checks if a Field or Record Separator is needed.
. This is only called from within this loadmod.
.
AppendInit LROUTINE
         IF        (bytFldSeparatorEnabled=0)
           MOVE      "1",bytFldSeparatorEnabled
           IF        (bytRecSeparatorEnabled=0)        .First time through then
             MOVE      "1",bytRecSeparatorEnabled      .Yes
             SETMODE   *MCURSOR=*WAIT
             CREATE    txtClip=100:200:300:800:        .CREATE the EDITTEXT
                             MULTILINE:
                             BORDER=$ON:
                             STYLE=3DON:
                             BGCOLOR=$WINDOW:
                             BDRCOLOR=$BTNTEXT:
                             ZORDER=100
..                             FONT=gfntDfltTxt:
         %IF       cccShowTxtClipEnabled = $On
             ACTIVATE   txtClip
         %ENDIF
           ELSE
             CALL      InsertItemTxtClip USING strRecSeparator
           ENDIF
         ELSE
           CALL      InsertItemTxtClip USING strFldSeparator
         ENDIF
         RETURN
.
...............................................................................
. AppendEOR - A EOR is only appended if another Field is Appended.
. This routine sets a flag that is check before the next field is appended and
. at that time the EOR character is appended before the field.
AppendEOR ROUTINE
         IF        (bytFldSeparatorEnabled=0)
           CALL      AppendInit
         ENDIF
         CLEAR     bytFldSeparatorEnabled
         RETURN
.
...............................................................................
. InsertItemTxtClip - Insertitem into txtClip
.
lngInsertItemTxtClipFPLP INTEGER 4
curTxtClipByteAccu FORM 7               .Accumulate number of bytes
.
rstrInsertItemTxtClip DIM ^
InsertItemTxtClip LROUTINE rstrInsertItemTxtClip
         INSERTITEM txtClip,0,rstrInsertItemTxtClip
.
         MOVELPTR  rstrInsertItemTxtClip,lngInsertItemTxtClipFPLP
         IF        (lngInsertItemTxtClipFPLP!=0)  .0=Null String. Do not calc.
           ADD       lngInsertItemTxtClipFPLP,curTxtClipByteAccu
           MOVEFPTR  rstrInsertItemTxtClip,lngInsertItemTxtClipFPLP
           ADD       "1",curTxtClipByteAccu
           SUB       lngInsertItemTxtClipFPLP,curTxtClipByteAccu
         ENDIF
         RETURN
...............................................................................
. Set - Copy the Information from the EDITTEXT to the Clipboard then 
. DESTROY the EDITTEXT to free up the memory.
.
curTxtClipByteCurr FORM 7
Set      ROUTINE
         SETITEM   txtClip,1,0               .Select All the text
         SETITEM   txtClip,2,9999999
.
         GETITEM   txtClip,0,curTxtClipByteCurr   .Total up all characters
.
         GETPROP   txtClip,HWND=lngClipHWnd  .Copy the EDITTEXT to the CLIPBOARD
         CALL      SendMessage USING lngRetVal,lngClipHWnd,(WM_COPY)
.
         %IF       cccShowTxtClipEnabled = $Off
         DESTROY   txtClip                   .Destroy the EDITTEXT
         %ENDIF
.
.                             .Check if the EDITTEXT truncate the total number
.                             .of bytes that was suppose to be written.
.                             .A EDITTEXT has about a 30,000 byte maximum.
.                                            
         IF        (curTxtClipByteCurr!=curTxtClipByteAccu)
           PACK      strAlertMess WITH "Clipboard data was truncated.",strAlertCR
           ALERT     NOTE,strAlertMess,curAlertRetVal,"Clip: Clipboard Error"
.
..                       "curTxtClipByteCurr=",curTxtClipByteCurr,strAlertCR:
..                       "curTxtClipByteAccu=",curTxtClipByteAccu
         ENDIF
.
         %IF       cccShowByteEnabled = $On
.gw ===============
         PACK      strAlertMess WITH "Byte Count=",curTxtClipByteAccu,strAlertCR
         ALERT     STOP,strAlertMess,curAlertRetVal,strAlertTitle
.gw ===============
         %ENDIF
.
.                                            .Reset variables so the EDITEXT
.                                            .will be recreated next time needed
         CLEAR     bytRecSeparatorEnabled,bytFldSeparatorEnabled:
                     curTxtClipByteAccu
         SETMODE   *MCURSOR=*ARROW
         RETURN
.
...............................................................................
. Get - Copy the Information from the Clipboard to the EDITTEXT then 
. DESTROY the EDITTEXT to free up the memory.
.
rstrGet  DIM       ^
Get      ROUTINE   rstrGet
.                                       .Had to use a EDITTEXT because CLIPGET
.                                       .did not return all the lines in the
.                                       .Clipboard
..         CLIPGET   rstrGet
         CREATE    txtClip=100:200:300:800:        .CREATE the EDITTEXT
                         MULTILINE:
                         BORDER=$ON:
                         STYLE=3DON:
                         BGCOLOR=$WINDOW:
                         BDRCOLOR=$BTNTEXT:
                         ZORDER=100
         GETPROP   txtClip,HWND=lngClipHWnd  .Paste the CLIPBOARD to the EDITTEXT
         CALL      SendMessage USING lngRetVal,lngClipHWnd,(WM_PASTE)
         GETITEM   txtClip,0,rstrGet
         DESTROY   txtClip                   .Destroy the EDITTEXT
         RETURN
...............................................................................
. GetRecord - Get a Record (Line) from the Clipboard
.
strGRRng INIT      0x00,0x7E,0x80,0xFF
lngGRLin INTEGER    4
lngGRLPtr INTEGER   4
.
..rstrParse DIM       ^
GetRecord ROUTINE  rlngRetVal,rstrGet
         CLEAR     rstrGet
         IF        (lngGRLin=0)
           CREATE    txtClip=100:200:300:800:        .CREATE the EDITTEXT
                           MULTILINE:
                           BORDER=$ON:
                           STYLE=3DON:
                           BGCOLOR=$WINDOW:
                           BDRCOLOR=$BTNTEXT:
                           ZORDER=100
           GETPROP   txtClip,HWND=lngClipHWnd  .Paste the CLIPBOARD to the EDITTEXT
           CALL      SendMessage USING lngRetVal,lngClipHWnd,(WM_PASTE)
         ENDIF
.
.                                       Add in GetLine once upgraded from 
.                                       PL/B 8.4B. 
..         txtClip.GetLine GIVING rstrGet USING *LINE=lngGRLin
.
         MOVEPLEN  rstrGet,lngGRLPtr
         MOVE      lngGRLPtr,rstrGet
         CALL      SetPropLParamRStr USING rstrGet
         CALL      SendMessage USING rlngRetVal,lngClipHWnd,(EM_GETLINE):
                     lngGRLin
.
.
         ADD       "1",lngGRLin
.
         RETURN
...............................................................................
. GetClear - Clear the Current Blipboard Word Area
.
GetClear ROUTINE
         IF        (lngGRLin!=0)
           CLEAR     lngGRLin
           DESTROY   txtClip                   .Destroy the EDITTEXT
         ENDIF
         RETURN
...............................................................................
. Paste - Copy the Information from the CLIPBOARD to the Keyboard Buffer 
.
autWshShell AUTOMATION
.
strRepRecSeparatorWithTilda INIT 0x7F,"~"    .Replace Record Sep with Tilda
curWshShellEnabled FORM 1
..vstrPaste DIM ^
vstrPaste DIM 1000
Paste    ROUTINE
.
. Do not allow under Citrix until the "WScript.Shell" object is available!
. 02/19/02 GW Phil added WScript.Shell
.
..curRetVal FORM 4
..strClientName DIM 4
..         CALL      "CLOCKIE" USING curRetVal,"CLIENTNAME",strClientName
..         IF        (curRetVal!=0)
.
         CALL      InitWshShell
.
         CALL      Get USING vstrPaste  .Get information from the clipboard
.
.                                       .Enclose special characters in {}
         LOOP
           FINDCHAR  "(){}[]+^%~",vstrPaste
           UNTIL     NOT EQUAL
           MOVE      "{",vstrAppend
           SINSERT   vstrPaste,vstrAppend
           BUMP      vstrPaste,1
           IF        EOS
             APPEND    "}",vstrPaste
             BREAK
           ELSE
             MOVE      "}",vstrAppend
             SINSERT   vstrPaste,vstrAppend
           ENDIF
         REPEAT
         RESET     vstrPaste
.                                                 Remove Dashes from SS#
         MOVELPTR  vstrPaste,lngInsertItemTxtClipFPLP
         IF        (lngInsertItemTxtClipFPLP=11)
           MATCH     "???-??-????",vstrPaste USING "?"
           IF        EQUAL
             SQUEEZE   vstrPaste,strAlertMess,"-"
             TYPE      strAlertMess
             IF        EQUAL
               MOVE      strAlertMess,vstrPaste
             ENDIF
           ENDIF
         ENDIF
.
.                                       .When SendKeys encounters a Tilda the
.                                       .ENTER key is sent
         REP       strRepRecSeparatorWithTilda,vstrPaste
         REP       "AaBbCcDdEeFfGgHhIiJjKkLlMmNnOoPpQqRrSsTtUuVvWwXxYyZz",vstrPaste
.
         CALL      SendKeys USING vstrPaste
..         ENDIF
         RETURN
...............................................................................
. SendKeys - Send Keys
.
. http://msdn.microsoft.com/archive/default.asp?url=/archive/en-us/wsh/htm/wsMthSendKeys.asp
.
. To specify characters that aren't displayed when you press a key, such as 
. ENTER or TAB, and keys that represent actions rather than characters, use the 
. codes shown below:
.
. Key            Code 
. BACKSPACE      {BACKSPACE}, {BS}, or {BKSP} 
. BREAK          {BREAK} 
. CAPS LOCK      {CAPSLOCK} 
. DEL or DELETE  {DELETE} or {DEL} 
. DOWN ARROW     {DOWN} 
. END            {END} 
. ENTER          {ENTER} or ~ 
. ESC            {ESC} 
. HELP           {HELP} 
. HOME           {HOME} 
. INS or INSERT  {INSERT} or {INS} 
. LEFT ARROW     {LEFT} 
. NUM LOCK       {NUMLOCK} 
. PAGE DOWN      {PGDN} 
. PAGE UP        {PGUP} 
. PRINT SCREEN   {PRTSC} 
. RIGHT ARROW    {RIGHT} 
. SCROLL LOCK    {SCROLLLOCK} 
. TAB            {TAB} 
. UP ARROW       {UP} 
. F1             {F1} 
. F2             {F2} 
. F3             {F3} 
. F4             {F4} 
. F5             {F5} 
. F6             {F6} 
. F7             {F7} 
. F8             {F8} 
. F9             {F9} 
. F10            {F10} 
. F11            {F11} 
. F12            {F12} 
. F13            {F13} 
. F14            {F14} 
. F15            {F15} 
. F16            {F16} 
. 
. To specify keys combined with any combination of the SHIFT, CTRL, and ALT keys, 
. precede the key code with one or more of the following codes:
. 
. Key    Code 
. SHIFT  + 
. CTRL   ^ 
. ALT    % 
. 
. To specify that any combination of SHIFT, CTRL, and ALT should be held down 
. while several other keys are pressed, enclose the code for those keys in 
. parentheses. For example, to specify to hold down SHIFT while E and C are 
. pressed, use "+(EC)". To specify to hold down SHIFT while E is pressed, 
. followed by C without SHIFT, use "+EC".
. 
. To specify repeating keys, use the form {key number}. You must put a space 
. between key and number. For example, {LEFT 42} means press the LEFT ARROW 
. key 42 times; {h 10} means press H 10 times.
. 
. Note You can't use SendKeys to send keystrokes to an application that is not 
. designed to run in Microsoft Windows. Sendkeys also can't send the PRINT SCREEN 
. key {PRTSC} to any application. 
. 
SendKeys ROUTINE   vstrPaste
         CALL      InitWshShell
         autWshShell.SendKeys USING vstrPaste
         RETURN
.
InitWshShell
         IF        (curWshShellEnabled=$OFF)
           MOVE      ($ON),curWshShellEnabled
           CREATE    autWshShell,Class="WScript.Shell"
..           SMAKE     vstrPaste,1000
         ENDIF
         RETURN
.
