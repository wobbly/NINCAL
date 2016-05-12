	include	cons.inc

VT_BOOL EQU 11
OTRUE   variant

. Custom Draw Listview Processing
.
. This form will sublcass it's own message queue and listen out for any WM_NOTIFY
. messages from the Listview. When one is found, we tell the listview to notify us on
. every item paint.
.
. Every second item in the listview should be painted in red
.
.
. Chris Eastwood May 1998
.
. Feel free to use any of this code as you wish
.

.
. Custom Draw Message to intercept
.
Public Enum WinNotifications
    NM_FIRST = -0& . (0U- 0U) ' // generic to all controls
    NM_LAST = -99& . (0U- 99U)
    NM_OUTOFMEMORY = (NM_FIRST - 1)
    NM_CLICK = (NM_FIRST - 2)
    NM_DBLCLK = (NM_FIRST - 3)
    NM_RETURN = (NM_FIRST - 4)
    NM_RCLICK = (NM_FIRST - 5)
    NM_RDBLCLK = (NM_FIRST - 6)
    NM_SETFOCUS = (NM_FIRST - 7)
    NM_KILLFOCUS = (NM_FIRST - 8)
    NM_CUSTOMDRAW = (NM_FIRST - 12)
    NM_HOVER = (NM_FIRST - 13)
End Enum
.
. Win API Rect structure
.
RECT		Record
Left		Integer 4
Top		Integer 4
Right 		Integer 4
Bottom 		Integer 4
		RecordEnd


.
. Custom Draw Structures
.
. The NMHDR structure contains information about a notification message. The pointer
. to this structure is specified as the lParam member of the WM_NOTIFY message.
.
NMHDR		Record
hwndFrom 	Integer 4		. Window handle of control sending message
idFrom 		Integer 4		. Identifier of control sending message
code 		Integer 4		. Specifies the notification code
		RecordEnd

NMCUSTOMDRAWINFO Record
hdr		Record Like NMHDR
dwDrawStage	Integer 4
hdc		Integer 4
rc		Record Like RECT
dwItemSpec	Integer 4
iItemState	Integer 4
lItemLParam	Integer 4
		RecordEnd

NMLVCUSTOMDRAW	Record
nmcmd		Record Like NMCUSTOMDRAWINFO
clrText		Integer 4
clrTextBk	Integer 4
iSubItem	Integer 1
End Type
.
. Notify Message
.
WM_NOTIFY	integer 1,"0x4E"
.
. Custom Draw Messages
.
CDDS_PREPAINT	integer 1,"0x1"
CDDS_POSTPAINT	integer 1,"0x2"
CDDS_PREERASE	integer 1,"0x3"
CDDS_POSTERASE	integer 1,"0x4"
CDDS_ITEM	integer 1,"0x10000"
CDDS_ITEMPREPAINT integer 1	.CDDS_ITEM Or CDDS_PREPAINT
CDDS_ITEMPOSTPAINT integer 1	.CDDS_ITEM Or CDDS_POSTPAINT
CDDS_ITEMPREERASE integer 1	.CDDS_ITEM Or CDDS_PREERASE
CDDS_ITEMPOSTERASE integer 1	.CDDS_ITEM Or CDDS_POSTERASE
CDDS_SUBITEM	integer 1,"0x20000"

CDRF_DODEFAULT	integer 1,"0x0"
CDRF_NEWFONT	integer 1,"0x2"
CDRF_SKIPDEFAULT	integer 1,"0x4"
CDRF_NOTIFYPOSTPAINT	integer 1,"0x10"
CDRF_NOTIFYITEMDRAW	integer 1,"0x20"
CDRF_NOTIFYSUBITEMDRAW	integer 1,"0x20"	. flags are the same, we can distinguish by context
CDRF_NOTIFYPOSTERASE	integer 1,"0x40"
CDRF_NOTIFYITEMERASE	integer 1,"0x80"
.
. Win API Declarations
.
.Private Declare Sub CopyMemory Lib "kernel32" Alias "RtlMoveMemory" (pDest As Any, pSource As Any, ByVal dwLength As Long)
.Private Declare Function SendMessage Lib "user32" Alias "SendMessageA" (ByVal hwnd As Long, ByVal wMsg As Long, ByVal wParam As Long, lParam As Any) As Long
APIResult 		Integer		4,"0"		Result from API call
pDest			Integer		4
pSource			Integer		4
dwLength		Integer		4
hwnd			Integer		4
wMsg			Integer		4
wParam			Integer		4
lParam			Integer		4
GenInt			Integer		4

RtlMoveMemoryProfile    Profile         kernel32.dll:   Dll library
                                        RtlMoveMemory:  Entry Point
                                        Int4:           Type return
                                        Int4:           pDest
                                        Int4:           pSource
                                        Int4:           dwLength

SendMessageAProfile     Profile         user32.dll:     Dll library
                                        SendMessageA:   Entry Point
                                        Int4:           Type return
                                        Int4:           hwnd
                                        Int4:           wMsg
                                        Int4:           wParam
                                        Int4:           lParam

WM_GETFONT	integer 1,"0x31"

        create  OTRUE,VarType=VT_BOOL,VarValue=1
.
. Setup the listview with 1 column and 25 listitems
.
	ListView1.InsertColumn using "Test",0,1
	ListView1.InsertColumn using "Column 1",50,1
	for result,"0","25"
		move	result,str9
		call	trim using str9
		pack	str25,"key",str9
		ListView1.InsertItem giving N9 using str25
		pack	str25,"This is line ",str9
		ListView1.SetItemText using N9,str25,1
	repeat

.
. Now subclass the form and watch for WM_NOTIFY messages coming from the listview
.
. I'm using the Softcircuits subclass control here, although you can use any other
. (they all work in pretty much the same way). You could also do any AddressOf processing
. if you so wish. I just use the SC control because it's a lot quicker (and you don't have
. to remember to un-subclass your window afterwards)
.
	getprop	Form1,hwnd=GenInt
	setprop	Subclass1,*hwnd=GenInt
	setprop	Subclass1,*Messages(WM_NOTIFY)=OTRUE
.    With Subclass1
.        .hwnd = Me.hwnd
.        .Messages(WM_NOTIFY) = True
.    End With

        eventreg Subclass1,1,Subclass1_WndProc

	loop
		waitevent
	repeat
.Event
Subclass1_WndProc		.	(Msg As Long, wParam As Long, lParam As Long, Result As Long)

tMessage	Record like NMHDR
lCode		Integer 4
tLVRedrawMessage Record like NMLVCUSTOMDRAW

	if (Msg = WM_NOTIFY)
.
. Should only be WM_NOTIFY (that's all we've subclassed)
.
.
. Same as in C : tMessage = (NMHDR) lParam;
.
.
. The .code section of the NMHDR notify structure contains the submessage
.
.		CopyMemory tMessage, ByVal lParam, Len(tMessage)
		moveaddr lParam,tMessage	
		move	tMessage.code,lcode

		if (lCode = NM_CUSTOMDRAW)
.
. Make sure it's our listview raising the Custom Redraw message
.
				if (tMessage.hwndFrom <> ListView1.hwnd)
.
. It's not ! - Return default processing to windows
.
					Result = Subclass1.CallWndProc(Msg, wParam, lParam)
					return
				endif
.
. Copy the message into our local structure
.
				CopyMemory tLVRedrawMessage, ByVal lParam, Len(tLVRedrawMessage)
.
. Now process the Custom Redraw Messages in Order :
.
. CDDS_PREPAINT is at the beginning of the paint cycle.
. You must return the property value to get Custom painting
. to work correctly. In this example, we're only looking for
. item specific painting - although theoretically, you should
. be able to paint just about anything on the control, from
. bitmap backgrounds to changing fonts etc.
.
. (Just don't ask me how to do it (yet)).
.
				If tLVRedrawMessage.nmcmd.dwDrawStage = CDDS_PREPAINT Then
.
. Request a notification for each item being painted
.
					Result = CDRF_NOTIFYITEMDRAW
					Exit Sub
				End If
.
. Because we returned CDRF_NOTIFYITEMDRAW in the above code, CDDS_ITEMPREPAINT is now sent
. when the control is ready to paint an Item
.
				If tLVRedrawMessage.nmcmd.dwDrawStage = CDDS_ITEMPREPAINT Then
.
. The item's about to be repainted - Here's where you can trap to see which item is being
. painted and so set the color accordingly
.
. To see which item is about to be painted, check :
.
. if tLVRedrawMessage.nmcm.dwItemSpec = required listview item number Then
.
. To Change the text and background colours in a list view control,
. set the clrText and clrTextBk members of the NMLVCUSTOMDRAW structure to the
. required color. Most other controls rely on the SetTextColor and SetBkColor API
. calls on the passed in hdc
.
. In this code I'm setting every second listitem to be red
.
.
					With tLVRedrawMessage
						If .nmcmd.dwItemSpec / 2 = CInt(.nmcmd.dwItemSpec / 2) Then
							.clrTextBk = vbWhite
							.clrText = vbRed
.
. You must remember to copy back the changes made in tLVRedrawMessage to the LPARAM value
.
							CopyMemory ByVal lParam, tLVRedrawMessage, Len(tLVRedrawMessage)
							Exit Sub
						Else
.
. This is standard painting stuff - let windows do it for us
.
							Result = CDRF_DODEFAULT
							Exit Sub
						End If
					End With
				End If
			Case Else
.
. Other messages from the listview which we're not interested in should be passed back
.
				Result = Subclass1.CallWndProc(Msg, wParam, lParam)
				Exit Sub
			End Select
		endif
	return
.    Select Case Msg
..
.. Should only be WM_NOTIFY (that's all we've subclassed)
..
.        Case WM_NOTIFY
..
.. Same as in C : tMessage = (NMHDR) lParam;
..
..
.. The .code section of the NMHDR notify structure contains the submessage
..
.            CopyMemory tMessage, ByVal lParam, Len(tMessage)
.            lCode = tMessage.code
.
.            Select Case lCode
.                Case NM_CUSTOMDRAW
..
.. Make sure it's our listview raising the Custom Redraw message
..
.                    If tMessage.hwndFrom <> ListView1.hwnd Then
..
.. It's not ! - Return default processing to windows
..
.                        Result = Subclass1.CallWndProc(Msg, wParam, lParam)
.                        Exit Sub
.                    End If
..
.. Copy the message into our local structure
..
.                    CopyMemory tLVRedrawMessage, ByVal lParam, Len(tLVRedrawMessage)
..
.. Now process the Custom Redraw Messages in Order :
..
.. CDDS_PREPAINT is at the beginning of the paint cycle.
.. You must return the property value to get Custom painting
.. to work correctly. In this example, we're only looking for
.. item specific painting - although theoretically, you should
.. be able to paint just about anything on the control, from
.. bitmap backgrounds to changing fonts etc.
..
.. (Just don't ask me how to do it (yet)).
..
.                    If tLVRedrawMessage.nmcmd.dwDrawStage = CDDS_PREPAINT Then
..
.. Request a notification for each item being painted
..
.                        Result = CDRF_NOTIFYITEMDRAW
.                        Exit Sub
.                    End If
..
.. Because we returned CDRF_NOTIFYITEMDRAW in the above code, CDDS_ITEMPREPAINT is now sent
.. when the control is ready to paint an Item
..
.                    If tLVRedrawMessage.nmcmd.dwDrawStage = CDDS_ITEMPREPAINT Then
..
.. The item's about to be repainted - Here's where you can trap to see which item is being
.. painted and so set the color accordingly
..
.. To see which item is about to be painted, check :
..
.. if tLVRedrawMessage.nmcm.dwItemSpec = required listview item number Then
..
.. To Change the text and background colours in a list view control,
.. set the clrText and clrTextBk members of the NMLVCUSTOMDRAW structure to the
.. required color. Most other controls rely on the SetTextColor and SetBkColor API
.. calls on the passed in hdc
..
.. In this code I'm setting every second listitem to be red
..
..
.                        With tLVRedrawMessage
.                            If .nmcmd.dwItemSpec / 2 = CInt(.nmcmd.dwItemSpec / 2) Then
.                                .clrTextBk = vbWhite
.                                .clrText = vbRed
..
.. You must remember to copy back the changes made in tLVRedrawMessage to the LPARAM value
..
.                                CopyMemory ByVal lParam, tLVRedrawMessage, Len(tLVRedrawMessage)
.                                Exit Sub
.                            Else
..
.. This is standard painting stuff - let windows do it for us
..
.                                Result = CDRF_DODEFAULT
.                                Exit Sub
.                            End If
.                        End With
.                    End If
.
.                Case Else
..
.. Other messages from the listview which we're not interested in should be passed back
..
.                    Result = Subclass1.CallWndProc(Msg, wParam, lParam)
.                    Exit Sub
.        End Select
.    End Select
.End Sub

RtlMoveMemory           WinApi          RtlMoveMemoryProfile Giving APIResult Using pDest,pSource,dwLength
                        Call            GetLastError    Load the last error value
                        RETURN

SendMessageA            WinApi          SendMessageAProfile Giving APIResult Using hwnd,wMsg,wParam,lParam
                        Call            GetLastError    Load the last error value
                        RETURN

	include	comlogic.inc
