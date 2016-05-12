. 12-01-00
. 4-25-02 GENERIC
. 4-26
. Usage:
.    CALL    REORDER USING mylistviewLV,LVCOLS
. where LVCOLS is the number of columns in the listview
. If 1st call, also set GroupBox Border:
.    CALL GBBORDER USING myWindowW, mylistviewLV
. where myWindowW is thw WINDOW containing the listview.
.
. To cancel:
.    CALL REORDER_OFF USING mylistviewLV
.
....................................................................................
result	form	9
T	form	9
L	form	9
W	form	9
H	form	9
ret	integer	4
$MOVE         EQU    12
$MOUSEDOWN    EQU    13
$MOUSEUP      EQU    14
$MOUSEMOVE    EQU    15
NO_MOVE   FORM      1
I40       INTEGER   4
I41       INTEGER   4
REORDERED FORM      1         // INDICATES LISTVIEW ITEMS WERE REORDERED
.
SENDMSGD  PROFILE   user32,SendMessageA,INT4,INT4,INT2,INT4,DIM
RES5      FORM      5
RES4      FORM      4
#DIM9      DIM       9
DELTA     FORM      "20"
POINT     DIM       8
FLAGS     INTEGER   4
IZERO     INTEGER   4
LVHITTESTINFO       DIM       20
LVM_SUBITEMHITTEST  INTEGER   2,"4153"
LV_ITEM   INTEGER   4
LV_SUBITEM INTEGER  4
LV_MAXCOLS          CONST     "20"   // 20 COLUMNS OF DATA, ZERO BASED
LV_DATA   DIM       ^1000(0..LV_MAXCOLS)
.
#CNT       FORM      3
ROW       FORM      5
ROW1      FORM      5
ROW2      FORM      5
COLUMN    FORM      5
OBJHANDL  INTEGER   4
PARAM     FORM      5
.
N4	form	4
str4	dim	4
zfill	init	" 0"
colordim dim	8
colordim2 dim	8
...............................................................................
reorder1 routine 
..
P_REORD_LV LISTVIEW ^         // LISTVIEW TO REARRANGE
P_COLCT   FORM      ^         // NUMBER OF COLUMNS IN LISTVIEW

          GOTO      #S
REORDER   ROUTINE  P_REORD_LV,P_COLCT
.
. Get the Listview handle
.
          GETPROP   P_REORD_LV,HWND=OBJHANDL
.
          EVENTREG  P_REORD_LV,$MOUSEDOWN,MOUSE_DN,RESULT=RESULT
          RETURN
.
REORDER_OFF ROUTINE P_REORD_LV
          EVENTREG  P_REORD_LV,$MOUSEDOWN
          EVENTREG  P_REORD_LV,$MOUSEUP
          RETURN
..
MOUSE_DN
.
.	setprop	P_REORD_LV,SortHeader=0,SortOrder=3
.
          SETFOCUS  P_REORD_LV
          CALL      LVHITTEST_NOW
          MOVE      ROW,ROW1
.          SETMODE   *MCURSOR=*DROP
          EVENTREG  P_REORD_LV,$MOUSEUP,MOUSE_UP,RESULT=RESULT
          CALL      SET_GBX
          RETURN
MOUSE_UP
.          SETMODE   *MCURSOR=*ARROW
          IF        ( NO_MOVE )
            EVENTREG P_REORD_LV,$MOUSEUP
            RETURN
          ENDIF
          CALL      LVHITTEST_NOW
          IF        (ROW = ROW1)
            RETURN
          ENDIF
          SET       REORDERED
          MOVE      ROW,ROW2
          P_REORD_LV.GetItemParam GIVING PARAM USING ROW1
          CALL      GET_LV_ITEM_DATA
          P_REORD_LV.DeleteItem USING ROW1
          CALL      REINSERT_LV_ITEM_DATA
          EVENTREG  P_REORD_LV,14
..Re-Index the items and establish new Sort Order
.	P_REORD_LV.GetItemCount giving result
.	for N4,"0",(result-1)
.		move	N4,str4
.		rep	zfill,str4
.		P_REORD_LV.SetItemText using N4,str4,0
.	repeat
.	setprop	P_REORD_LV,SortHeader=1,SortOrder=1
          RETURN
.
GET_LV_ITEM_DATA
          CLEAR     #CNT
          LOOP
            P_REORD_LV.GetItemText GIVING LV_DATA(#CNT) USING ROW1,#CNT
            INCR    #CNT
          REPEAT UNTIL ( #CNT = P_COLCT )
          P_REORD_LV.GetItemText giving colordim using ROW1,9
          P_REORD_LV.GetItemText giving colordim2 using ROW1,10
          RETURN
.
REINSERT_LV_ITEM_DATA
          CLEAR    #CNT
          P_REORD_LV.InsertItem GIVING RESULT USING LV_DATA(#CNT),ROW2:
                    *PARAM=PARAM,*State=3,*Statemask=3
          P_REORD_LV.SetItemText using ROW2,colordim,9
          P_REORD_LV.SetItemText using ROW2,colordim2,10

          LOOP
            INCR    #CNT
            P_REORD_LV.SetItemText USING RESULT, LV_DATA(#CNT), #CNT
          REPEAT UNTIL ( #CNT = P_COLCT )
          RETURN
.
LVHITTEST_NOW
          MOVE      RESULT,#DIM9         // Get cursor position relative
          UNPACK    #DIM9,RES5,RES4      // to the listview from the
          MOVE      RES5,I40            // MOUSEDOWN Event Result
          MOVE      RES4,I41
          PACK      POINT WITH I40,I41
.
. Now send the LVM_SUBITEMHITTEST message to the Listview
.
          CLEAR     I40,I41,FLAGS
          PACK      LVHITTESTINFO WITH POINT,FLAGS,I40,I41
          WINAPI    SENDMSGD GIVING RET USING OBJHANDL,LVM_SUBITEMHITTEST:
                                                  IZERO,LVHITTESTINFO
.
          UNPACK    LVHITTESTINFO, POINT,FLAGS,LV_ITEM,LV_SUBITEM
          MOVE      LV_ITEM,ROW
          MOVE      LV_SUBITEM,COLUMN
          RETURN
...
. LISTVIEW BORDER BOXES
$NGB      EQU       2
GBGROUP   FORM      " 1"
GBT       GROUPBOX ($NGB)
GBB       GROUPBOX ($NGB)
GBL       GROUPBOX ($NGB)
GBR       GROUPBOX ($NGB)
COLOR1    COLOR
GBX_SW    FORM      1
$TRANSPARENT  EQU       2
..
..
.
MouseMv_GBCOLL
	EVENTREG GBT(GBGROUP),15
	EVENTREG GBB(GBGROUP),15
	EVENTREG GBL(GBGROUP),15
	EVENTREG GBR(GBGROUP),15
	CLEAR 	GBX_SW
.	SETMODE   *MCURSOR=*ARROW
          EVENTREG  P_REORD_LV,14
	RETURN
..
W1	WINDOW	^
P_REORD_LV2      LISTVIEW  ^
GBBORDER  ROUTINE   W1,P_REORD_LV2
          GETPROP   P_REORD_LV2,TOP=T,LEFT=L,WIDTH=W,HEIGHT=H
          GETPROP   W1,BGCOLOR=COLOR1
          CREATE    W1;GBT(GBGROUP)=(T-DELTA):T:(L-DELTA):(L+W+DELTA):
                        STYLE=2,BDRCOLOR=COLOR1,BACKSTYLE=2,VISIBLE=1
          CREATE    W1;GBB(GBGROUP)=(T+H):(T+H+DELTA):(L-DELTA):(L+W+DELTA):
                        STYLE=2,BDRCOLOR=COLOR1,BACKSTYLE=2,VISIBLE=1
          CREATE    W1;GBL(GBGROUP)=(T):(T+H):(L-DELTA):(L):
                        STYLE=2,BDRCOLOR=COLOR1,BACKSTYLE=2,VISIBLE=1
          CREATE    W1;GBR(GBGROUP)=(T):(T+H):(L+W):(L+W+DELTA):
                        STYLE=2,BDRCOLOR=COLOR1,BACKSTYLE=2,VISIBLE=1
          RETURN
..
SET_GBX
	EVENTREG  GBT(GBGROUP),15,MouseMv_GBCOLL
	EVENTREG  GBB(GBGROUP),15,MouseMv_GBCOLL
	EVENTREG  GBL(GBGROUP),15,MouseMv_GBCOLL
	EVENTREG  GBR(GBGROUP),15,MouseMv_GBCOLL
	SET       GBX_SW
	RETURN
...........................................................................
........Incomplete Logic for MS ListView 6.0..............................
...........................................................................
AutoLV		Automation ^         // LISTVIEW TO REARRANGE
stattextP	stattext ^
.
IntIndex	integer 4
IntIndex2	integer 4
ColHeads	automation
ColHead		automation
ListIts		automation
ListIt		automation
SubIt		automation
.
REORDER2	Routine AutoLV,stattextP
.
. Get the Listview handle
.
	getprop	AutoLV,*hWnd=OBJHANDL
	getprop	AutoLV,*ColumnHeaders=ColHeads
	getprop	ColHeads,*Count=P_COLCT
.
	eventreg AutoLV,$MOUSEDOWN,MOUSE_DN2,RESULT=RESULT
	setitem	stattextP,0,"Mouse Down registered"
	return
.
.REORDER_OFF2 ROUTINE AutoLV
.          EVENTREG  AutoLV,$MOUSEDOWN
.          EVENTREG  AutoLV,$MOUSEUP
.          RETURN
..
MOUSE_DN2
.	setfocus AutoLV
	setitem	stattextP,0,"Mouse Down activated"
	call	LVHITTEST_NOW2
	move	ROW,ROW1
	setmode	*MCURSOR=*DROP
	eventreg AutoLV,$MOUSEUP,MOUSE_UP2,RESULT=RESULT
	setitem	stattextP,0,"Mouse Up registered"
	call	SET_GBX
	return

MOUSE_UP2
	setitem	stattextP,0,"Mouse up activated"
	setmode	*MCURSOR=*ARROW
	if ( NO_MOVE )
		eventreg AutoLV,$MOUSEUP
		return
	endif
	call	LVHITTEST_NOW2
	if (ROW = ROW1)
		return
	endif
	set	REORDERED
	move	ROW,ROW2
	AutoLV.GetItemParam GIVING PARAM USING ROW1
	call	GET_LV_ITEM_DATA
	AutoLV.DeleteItem USING ROW1
	call	REINSERT_LV_ITEM_DATA
	eventreg AutoLV,14
	return
.
GET_LV_ITEM_DATA2
	clear	#CNT
	loop
		AutoLV.GetItemText GIVING LV_DATA(#CNT) USING ROW1,#CNT
		incr	#CNT
	repeat until ( #CNT = P_COLCT )
	return
.
REINSERT_LV_ITEM_DATA2
	clear	#CNT
	AutoLV.InsertItem GIVING RESULT USING LV_DATA(#CNT),ROW2:
	*PARAM=PARAM,*State=3,*Statemask=3
	loop
		incr	#CNT
		AutoLV.SetItemText USING RESULT, LV_DATA(#CNT), #CNT
	repeat until ( #CNT = P_COLCT )
	return
.
LVHITTEST_NOW2
	move	RESULT,#DIM9         // Get cursor position relative
	unpack	#DIM9,RES5,RES4      // to the listview from the
	move	RES5,I40            // MOUSEDOWN Event Result
	move	RES4,I41
	pack	POINT,I40,I41
.
. Now send the LVM_SUBITEMHITTEST message to the Listview
.
	clear	I40,I41,FLAGS
	pack	LVHITTESTINFO,POINT,FLAGS,I40,I41
	winapi	SENDMSGD GIVING RET USING OBJHANDL,LVM_SUBITEMHITTEST:
		IZERO,LVHITTESTINFO
.
	unpack	LVHITTESTINFO,POINT,FLAGS,LV_ITEM,LV_SUBITEM
	move	LV_ITEM,ROW
	move	LV_SUBITEM,COLUMN
	return
...
. LISTVIEW BORDER BOXES
..
..
.
MouseMv_GBCOLL2
	eventreg GBT(GBGROUP),15
	eventreg GBB(GBGROUP),15
	eventreg GBL(GBGROUP),15
	eventreg GBR(GBGROUP),15
	clear	GBX_SW
	setmode	*MCURSOR=*ARROW
	eventreg AutoLV,14
	return
..
AutoLV3      LISTVIEW  ^
GBBORDER2 ROUTINE W1,AutoLV3
	getprop	AutoLV3,TOP=T,LEFT=L,WIDTH=W,HEIGHT=H
	getprop	W1,BGCOLOR=COLOR1
	create	W1;GBT(GBGROUP)=(T-DELTA):T:(L-DELTA):(L+W+DELTA):
		STYLE=2,BDRCOLOR=COLOR1,BACKSTYLE=2,VISIBLE=1
	create	W1;GBB(GBGROUP)=(T+H):(T+H+DELTA):(L-DELTA):(L+W+DELTA):
		STYLE=2,BDRCOLOR=COLOR1,BACKSTYLE=2,VISIBLE=1
	create	W1;GBL(GBGROUP)=(T):(T+H):(L-DELTA):(L):
		STYLE=2,BDRCOLOR=COLOR1,BACKSTYLE=2,VISIBLE=1
	create	W1;GBR(GBGROUP)=(T):(T+H):(L+W):(L+W+DELTA):
		STYLE=2,BDRCOLOR=COLOR1,BACKSTYLE=2,VISIBLE=1
	return
..
SET_GBX2
	eventreg GBT(GBGROUP),15,MouseMv_GBCOLL
	eventreg GBB(GBGROUP),15,MouseMv_GBCOLL
	eventreg GBL(GBGROUP),15,MouseMv_GBCOLL
	eventreg GBR(GBGROUP),15,MouseMv_GBCOLL
	set	GBX_SW
	return
...
#S
.................................

