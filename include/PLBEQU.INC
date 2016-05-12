*==============================================================================
. GUI designer support equates
.
. Last Revision	Date:  22 Apr 1997
.
. Revision History:
.
.:: 15 Jan 97 -	Create PLB equate include definitions.			 :8.1
.:: 06 Feb 96 -	Added ICON to APPEARANCE property.			 :8.1
.::	      -	Added GROUPID to DROPID	property.			 :8.1
.::	      -	Added SHAPE to BGCOLOR and BDRCOLOR properties.		 :8.1
.:: 25 Feb    -	Added LINKHSCROLL and LINKVSCROLL.			 :8.1
.:: 05 Mar    -	Added AUTOSCALE, PICTSIZEH, PICTSIZEV, and WINPOS.	 :8.1
.:: 02 Apr    -	Changed	EDITTYPE to use	DECIMAL	and INTEGER		 :8.1
.:: 22 Apr    -	Added LINE object.					 :8.1
.::	      -	Added X1, X2, Y1, and Y2 properties for	LINE object.	 :8.1
.::
.
*------------------------------------------------------------------------------
. General Usage	Equates
.
$ON	EQU	   1
$OFF	EQU	   0
$NONE	EQU	   0
.
*------------------------------------------------------------------------------
. Event	Type Indicators
.
$LOAD	      EQU    0
$ACTIVATE     EQU    1
$DEACTIVATE   EQU    2
$CHANGE	      EQU    3
$CLICK	      EQU    4
$CLOSE	      EQU    5
$DBLCLICK     EQU    6
$DRAGDROP     EQU    7
$DRAGOVER     EQU    8
$GOTFOCUS     EQU    9
$KEYPRESS     EQU    10
$LOSTFOCUS    EQU    11
$MOVE	      EQU    12
$MOUSEDOWN    EQU    13
$MOUSEUP      EQU    14
$MOUSEMOVE    EQU    15
$PAINT	      EQU    16
$RESIZE	      EQU    17
$TIMER	      EQU    18
$OLDEVENT     EQU    19
$FORMINIT     EQU    20
.
*==============================================================================
. Object Properties
.
*------------------------------------------------------------------------------
.   ALIGNMENT=dnumnvar			     CREATE  SETPROP  GETPROP
.
. Description:
.	The ALIGNMENT property defines how the text is to be output
.	horizontally for the object.
.
.	Note1: The EDITTEXT object doesn't support changing of this property
.	       using the SETPROP statement. 
.
. Objects:
.	EDITTEXT
.	STATTEXT
.
. dnumnvar values:
.
.$NONE	      EQU	0	       // Default
$CENTER	      EQU	1
$LEFT	      EQU	2
$RIGHT	      EQU	3
.
*------------------------------------------------------------------------------
.   APPEARANCE=dnumnvar			     CREATE  SETPROP  GETPROP
.
. Description:
.	The APPEARANCE property	specifies when an object should
.	have a 3D look or a non-3D look.  This property	should replace
.	the STYLE property for these objects.  If this property	is
.	not specified, the default appearance is determined by the
.	current	SETMODE	*3D state.  If the SETMODE *3D state has not
.	been specified,	then the appearance defaults to	be the same
.	as defined for the window.
.
. Note:	Under Windows95	these objects always have a 3D presentation 
.	regardless of the control value	provided.
.
. Objects:
.	CHECKBOX
.	CHECKGRP
.	COMBOBOX
.	DATALIST
.	ICON
.	POPUPMENU  (OBSELETE)
.	RADIO
.	RADIOGRP
.	WINDOW
.
. dnumnvar values:
.
$3D	      EQU	1
$FLAT	      EQU	2
.
*------------------------------------------------------------------------------
.   AUTOREDRAW=dnumnvar			     CREATE  SETPROP  GETPROP
.
. Description:
.	When this property is set to zero, then	the repainting of the
.	object is disabled.  When this property	is set to one, then the
.	repainting of the objects takes	place as normal.
.
. Objects:
.	COMBOBOX
.	DATALIST
.	POPUPMENU  (OBSELETE)
.	WINDOW
.
. dnumnvar values:
.
.$OFF	      EQU	0
.$ON	      EQU	1	       // Default
.
*------------------------------------------------------------------------------
.   AUTOSCALE=dnumnvar			     CREATE  SETPROP  GETPROP
.
. Description:
.	This property specifies	how a picture should be	scaled to fit
.	pict object rectangle coordinates. This	property is overridden
.	by the RESIZE property.
.
. Objects:
.	PICT
.
. dnumnvar values:
.
.$NONE	      EQU	0
$SCALEBEST    EQU	1		// Default 
$SCALEHORZ    EQU	2		
$SCALENONE    EQU	3		// Same	as RESIZE=$OFF
$SCALEVERT    EQU	4	
.
*------------------------------------------------------------------------------
.   AUTOSIZE[=dnumnvar]			     CREATE  SETPROP  GETPROP
.
. Description:
.	This property defines if the bottom border is to be adjusted
.	automatically to fit the number	of items specified.  If	the
.	AUTOSIZE property is not specified the default action is to
.	not resize the bottom border.  If the AUTOSIZE property	is
.	specified without an assignment, the action is the same	as
.	if AUTOSIZE=1 where given.
.
. Objects:
.	CHECKGRP
.	RADIOGRP
.
. dnumnvar values:
.
.$OFF	      EQU	0	       // Default
.$ON	      EQU	1
.
*------------------------------------------------------------------------------
.   AUTOZOOM[=dnumnvar]			     CREATE  SETPROP  GETPROP
.
. Description:
.	This property enables or disables the zooming capabilty	for
.	a pict object.
.
. Objects:
.	PICT
.
. dnumnvar values:
.
.$OFF	      EQU	0	       // Default
.$ON	      EQU	1
.
*------------------------------------------------------------------------------
.   BACKSTYLE=dnumnvar			     CREATE  SETPROP  GETPROP
.
. Description:
.	This property defines the type of background to	be used	for
.	supported objects.  The	BACKSTYLE can be OPAQUE	which prevents
.	underlying objects from	showing	through	this object.  The can
.	BACKSTYLE can also be TRANSPARENT which	allows underlying 
.	objects	to show	through	the supported objects.
.
. Objects:
.	GROUPBOX
.	SHAPE
.	STATTEXT
.
. dnumnvar values:
.
.$NONE	      EQU	0	       // Default
$OPAQUE	      EQU	1
$TRANSPARENT  EQU	2
.
*------------------------------------------------------------------------------
.   BDRCOLOR=dnumnvar|color object	     CREATE  SETPROP  GETPROP
.
. Description:
.	This property allows the color of the BORDER to	be specified by
.	the user program.  If the BDRCOLOR parameter is	a <color object>,
.	then the <color	object>	must be	created	before it is used.  The
.	<dnumnvar> value can also be used to specify the color as a
.	RGB value or a Windows system color index.  The	<dnumnvar> value
.	can be thought of as 4 byte values.  When the high order byte
.	contains a value of 0x00, then the next	3 bytes	(24 bits) get
.	interpreted as a RGB value.  When the high order byte has a value
.	of 0x80, then the lower	order byte is used as an index value into
.	the Windows System colors.  The	Windows	system colors can be 
.	defined	by the user under Windows 95 and Windows NT.
.
.	Index		Value		Meaning
.
.	COLOR_ACTIVEBORDER		Active window border.
.	COLOR_ACTIVECAPTION		Active window caption.
.	COLOR_APPWORKSPACE		Background color of multiple 
.					document interface (MDI) applications.
.	COLOR_BACKGROUND		Desktop.
.	COLOR_BTNFACE			Face shading on	push buttons.
.	COLOR_BTNSHADOW			Edge shading on	push buttons.
.	COLOR_BTNTEXT			Text on	push buttons.
.	COLOR_CAPTIONTEXT		Text in	caption, size box, 
.					and scroll bar arrow box.
.	COLOR_GRAYTEXT			Grayed (disabled) text.
.					This color is set to 0 if the
.					current	display	driver does not	
.					support	a solid	gray color.
.	COLOR_HIGHLIGHT			Item(s)	selected in a control.
.	COLOR_HIGHLIGHTTEXT		Text of	item(s)	selected in a control.
.	COLOR_INACTIVEBORDER		Inactive window	border.
.	COLOR_INACTIVECAPTION		Inactive window	caption.
.	COLOR_INACTIVECAPTIONTEXT	Color of text in an inactive 
.					    caption.
.	COLOR_MENU			Menu background.
.	COLOR_MENUTEXT			Text in	menus.
.	COLOR_SCROLLBAR			Scroll bar gray	area.
.	COLOR_SHADOW			Color of automatic window shadows.
.	COLOR_WINDOW			Window background.
.	COLOR_WINDOWFRAME		Window frame.
.	COLOR_WINDOWTEXT		Text in	windows.
.
.
. Objects:
.	STATTEXT
.	GROUPBOX
.	PROGRESS
.	CHECKGRP
.	EDITTEXT
.	LINE
.	MOVIE
.	PICT
.	RADIOGRP
.	SHAPE
.
. dnumnvar values:
.
$SCROLLBAR	      CONST   "2147483648"    ;Hex value 0x80000000
$BACKGROUND	      CONST   "2147483649"    ;Hex value 0x80000001
$ACTIVECAPTION	      CONST   "2147483650"    ;Hex value 0x80000002
$INACTIVECAPTION      CONST   "2147483651"    ;Hex value 0x80000003
$MENU		      CONST   "2147483652"    ;Hex value 0x80000004
$WINDOW		      CONST   "2147483653"    ;Hex value 0x80000005
$WINDOWFRAME	      CONST   "2147483654"    ;Hex value 0x80000006
$MENUTEXT	      CONST   "2147483655"    ;Hex value 0x80000007
$WINDOWTEXT	      CONST   "2147483656"    ;Hex value 0x80000008
$CAPTIONTEXT	      CONST   "2147483657"    ;Hex value 0x80000009
$ACTIVEBORDER	      CONST   "2147483658"    ;Hex value 0x8000000A
$INACTIVEBORDER	      CONST   "2147483659"    ;Hex value 0x8000000B
$APPWORKSPACE	      CONST   "2147483660"    ;Hex value 0x8000000C
$HIGHLIGHT	      CONST   "2147483661"    ;Hex value 0x8000000D
$HIGHLIGHTTEXT	      CONST   "2147483662"    ;Hex value 0x8000000E
$BTNFACE	      CONST   "2147483663"    ;Hex value 0x8000000F
$BTNSHADOW	      CONST   "2147483664"    ;Hex value 0x80000010
$GRAYTEXT	      CONST   "2147483665"    ;Hex value 0x80000011
$BTNTEXT	      CONST   "2147483666"    ;Hex value 0x80000012
$INACTIVECAPTIONTEXT  CONST   "2147483667"    ;Hex value 0x80000013
$BTNHIGHLIGHT	      CONST   "2147483668"    ;Hex value 0x80000014
.
*------------------------------------------------------------------------------
.   BDRPATTERN=dnumnvar			     CREATE  SETPROP  GETPROP
.
. Description:
.	This property allows the user to specify the type of border 
.	pattern	is to be used for the object.  The possible border
.	patterns are as	follows:
.
.	Dash		- Pen is dashed.
.	DashDot		- Pen has alternating dashes and dots.
.	DashDot2	- Pen has dashes and double dots.
.	Dot		- Pen is dotted.
.	InsideFrame	- Pen is solid.	 The figure and	border is created
.			  inside the frame defined by the user specified
.			  rectangle.
.	Solid		- Pen is solid.
.	None		- Pen defaults to solid.
.
.	Note1: The border pattern only takes affect when the BDRWIDTH
.	       property	has a value of 1.
.
. Objects:
.	LINE
.	SHAPE
.
. dnumnvar values:
.
.$NONE	      EQU	0	       // Default
$DASH	      EQU	1
$DASHDOT      EQU	2
$DASHDOT2     EQU	3
$DOT	      EQU	4
$INSIDEFRAME  EQU	5
$SOLID	      EQU	6
.
*------------------------------------------------------------------------------
.   BDRWIDTH=dnumnvar			     CREATE  SETPROP  GETPROP
.
. Description:
.	This property defines the width	of the pen used	to draw	the
.	object.
.
.	Note1: The border pattern only takes affect when the BDRWIDTH
.	       property	has a value of 1.
.
. Objects:
.	LINE
.	SHAPE
.
. dnumnvar values:
.
.	The <dnumnvar> value specifies number of pixels	for width.
.
*------------------------------------------------------------------------------
.   BGCOLOR=dnumnvar|color object	     CREATE  SETPROP  GETPROP
.
. Description:
.	This property allows the user program to specify the color to
.	be used	for the	background color.  The parameter description
.	is the same as for the BDRCOLOR	property.
.
. Objects:
.	CHECKBOX
.	CHECKGRP
.	COMBOBOX
.	DATALIST
.	EDITTEXT
.	GROUPBOX
.	HSCROLLBAR
.	PICT
.	POPUPMENU ( OBSELETE )
.	PROGRESS
.	RADIOGRP
.	RADIO
.	SHAPE
.	STATTEXT
.	VSCROLLBAR
.	WINDOW
.
. dnumnvar values:
.
.	See the	BDRCOLOR property for the color	equate values.
.
*------------------------------------------------------------------------------
.   BOLD[=dnumnvar]			     CREATE  SETPROP  GETPROP
.
. Description:
.	This property specifies	that the font should be	bolded.	 When
.	the <dnumnvar> value is	not provided, this indicates that the
.	BOLD is	turned on.
.
. Objects:
.	FONT
.
. dnumnvar values:
.
.$OFF	      EQU	0	       // Default
.$ON	      EQU	1
.
*------------------------------------------------------------------------------
.   BORDER[=dnumnvar]			     CREATE  SETPROP  GETPROP
.
. Description:
.	This property defines when an object is	to have	a border.
.	If the <dnumnvar> parameter is not provided, this indicates that
.	the border is to be used.
.
. Objects:
.	CHECKGRP
.	EDITTEXT
.	MOVIE
.	PICT
.	RADIOGRP
.	SLIDER
.	STATTEXT
.	WINDOW
.
. dnumnvar values:
.
.$OFF	      EQU	0	       // Default
.$ON	      EQU	1
.
*------------------------------------------------------------------------------
.   CANCEL[=dnumnvar]			     CREATE  SETPROP  GETPROP
.
. Description:
.	This property specifies	that the BUTTON	object activation routine
.	is to gain program control when	the escape key is entered.
.
. Objects:
.	BUTTON
.
. dnumnvar values:
.
.$OFF	      EQU	0	       // Default
.$ON	      EQU	1
.
*------------------------------------------------------------------------------
.   CAPTION=dnumnvar			     CREATE  SETPROP  GETPROP
.
. Description:
.	This property allows a user to specify if a Window is to be
.	created	with or	without	a title	bar.  This property is
.	provided to the	OS to eliminate/retain the caption bar as
.	appropriate.
.
.	NOTE1: Under Windows95 and Win32s, the CAPTION property	will 
.	       have an effect on a MODAL or MODELESS window.  The
.	       CAPTION property	does not have an affect	on the PRIMARY
.	       window types.
.
. Objects:
.	WINDOW
.
. dnumnvar values:
.
.$OFF	      EQU	0
.$ON	      EQU	1	       // Default
.
*------------------------------------------------------------------------------
.   CENTER[=dnumnvar]			     CREATE
.
. Description:
.	The CENTER property specifies that the text is to be centered 
.	horizontally for the object.  The property is replaced with
.	the ALIGNMENT property.	 The ALIGNMENT property	is to be used
.	for performing SETPROP and GETPROP operations. 
.
.	If the syntax form is used where the <dnumnvar>	value is not
.	specified, this	indicates that the CENTER property is to be
.	turned on.
.
.	Also note that if the CENTER property is turned	off, this causes
.	the text output	to be justified	to the left.
.
. Objects:
.	EDITTEXT
.	STATTEXT
.
. dnumnvar values:
.
.$OFF	      EQU	0	       // Default
.$ON	      EQU	1
.
*------------------------------------------------------------------------------
.   CENTERPOS=dnumnvar			     CREATE
.
. Description:
.	When the user defined rectangle	is larger than the EDITTEXT 
.	object,	this property controls whether the EDITTEXT object is to
.	be centered TOP/BOTTOM or not.	The default action is to
.	have the EDITTEXT object centered TOP/BOTTOM within the	user
.	defined	rectangle parameters.  If the CENTERPOS	property is
.	set to $TOPLEFTPOS, then the CREATE action is to have the
.	EDITTEXT object	rectangle be located at	the TOP/LEFT coordinates.
.
. Objects:A
.	EDITTEXT
.
. dnumnvar values:
.
$CENTERPOS    EQU	0	       // Default
$TOPLEFTPOS   EQU	1
.
*------------------------------------------------------------------------------
.   CLIPCTRL=dnumnvar			     CREATE  SETPROP  GETPROP
.
. Description:
.	Excludes the area occupied by child windows/objects when 
.	drawing	within the parent window.  If the property is turned
.	on, this can slow the performance of drawing objects due to
.	OS overhead in determining valid clipping regions.
.
. Objects:
.	WINDOW
.
. dnumnvar values:
.
.$OFF	      EQU	0	       // Default
.$ON	      EQU	1
.
*------------------------------------------------------------------------------
.   COMBOSTYLE=dnumnvar			     CREATE	      GETPROP
.
. Description:
.	The COMBOSTYLE property	specifies the type of COMBOBOX window
.	object to be generated.	 A default combobox list object	is a
.	combox with a drop down	list where the combox does not have
.	an editable capability.	 The $EDIT <dnumnvar> value
.	defines	a combobox list	like the default except	the combobox
.	does allow editing capability.	Only the current selection field
.	can be edited.	The $SIMPLE <dnumnvar> value is	the
.	same as	the $EDIT except the list is always present.
.
. Objects:
.	COMBOBOX
.	POPUPMENU ( OBSELETE )
.
. dnumnvar values:
.
.$NONE	      EQU	0	       // Same as $LIST
$EDIT	      EQU	1
$LIST	      EQU	2	       // Default
$SIMPLE	      EQU	3
.
*------------------------------------------------------------------------------
.   DATA=svarslit			     CREATE  SETPROP  GETPROP
.
. Description:
.	This property allows the items in a DATALIST to	be initialized using
.	a string composed of a list of items with each item separated with
.	semi-colon.
.
. Objects:
.	DATALIST
.
. svarslit value:
.
.	String of semicolon separated list items.
.
*------------------------------------------------------------------------------
.   DEFAULT[=dnumnvar]			     CREATE  SETPROP  GETPROP
.
. Description:
.	This property specifies	that the BUTTON	object activation routine
.	is to gain program control when	the enter key is entered and 
.	a non-button object has	the focus.  However, if	a button object
.	other than the DEFAULT button object has the focus when	the enter
.	key is entered,	then the activation routine of the button object
.	with the focus gains the program control.
.
.	Note1:	The operation when a button object other than the DEFAULT
.		button object has the focus is different from prior releases
.		to release 8.1.
.
. Objects:
.	BUTTON
.
. dnumnvar values:
.
.$OFF	      EQU	0	       // Default
.$ON	      EQU	1
.
*------------------------------------------------------------------------------
.   DIGITS[=dnumnvar]			     CREATE
.
. Description:
.	This property is a CREATE only property.
.	The property specifies that the	EDITTEXT object	is to only
.	allow numeric digits 0 through 9.  When	this property is set to
.	indicate off state, then this indicates	that all characters are
.	allowed.
.
.	Note1: Use of the property should be replaced through use of 
.	       the EDITTYPE property.
.
. Objects:
.	EDITTEXT
.
. dnumnvar values:
.
.$OFF	      EQU	0	       // Default
.$ON	      EQU	1
.
*------------------------------------------------------------------------------
.   DRAWMODE=dnumnvar			     CREATE  SETPROP  GETPROP
.
. Description:
.	This property specifies	the current foreground mix mode.
.	The foreground mix mode	defines	how colors from	the object
.	and the	colors in the existing presentation image are to be
.	combined.
.
.	The following is a Windows summary of the drawmodes.
.
.	Mix mode	Description
.
.	BLACKNESS	Pixel is always	0.
.
.	COPYPEN		Pixel is the pen color.
.
.	INVERT		Pixel is the inverse of	the screen color.
.
.	MASKNOTPEN	Pixel is a combination of the colors common
.			to both	the screen and the inverse of the pen.
.
.	MASKPEN		Pixel is a combination of the colors common
.			to both	the pen	and the	screen.
.
.	MASKPENNOT	Pixel is a combination of the colors common
.			to both	the pen	and the	inverse	of the screen.
.
.	MERGENOTPEN	Pixel is a combination of the screen color and
.			the inverse of the pen color.
.
.	MERGEPEN	Pixel is a combination of the pen color	and
.			the screen color.
.
.	MERGEPENNOT	Pixel is a combination of the pen color	and
.			the inverse of the screen color.
.
.	NOP		Pixel remains unchanged.
.
.	NOTCOPYPEN	Pixel is the inverse of	the pen	color.
.
.	NOTMASKPEN	Pixel is the inverse of	the MASKPEN color.
.
.	NOTMERGEPEN	Pixel is the inverse of	the MERGEPEN color.
.
.	NOTXORPEN	Pixel is the inverse of	the XORPEN color.
.
.	WHITENESS	Pixel is always	1.
.
.	XORPEN		Pixel is a combination of the pen color	and
.			the screen color, but not in both.
.
. Objects:
.	LINE
.	SHAPE
.
. dnumnvar values:
.
.$NONE	      EQU	0	//Same as default
$BLACKNESS    EQU	1
$COPYPEN      EQU	2	//Default
$INVERT	      EQU	3
$MASKNOTPEN   EQU	4
$MASKPEN      EQU	5
$MASKPENNOT   EQU	6
$MERGENOTPEN  EQU	7
$MERGEPEN     EQU	8
$MERGEPENNOT  EQU	9
$NOP	      EQU	10
$NOTCOPYPEN   EQU	11
$NOTMASKPEN   EQU	12
$NOTMERGEPEN  EQU	13
$NOTXORPEN    EQU	14
$WHITENESS    EQU	15
$XORPEN	      EQU	16
.
*------------------------------------------------------------------------------
.   DROPID=dnumnvar			     CREATE  SETPROP  GETPROP
.
. Description:
.	When this property is specified, then the object which has the
.	DROPID property	can receive an item drop action.  The value of
.	the DROPID is sent to the activation routine of	the object which
.	is being dragged.
.
. Objects:
.	BUTTON
.	CHECKBOX
.	CHECKGRP
.	COMBOBOX
.	DATALIST
.	EDITTEXT
.	GROUPBOX
.	HSCROLLBAR
.	ICON
.	MOVIE
.   	PICT
.	POPUPMENU ( OBSELETE )
.	PROGRESS
.	RADIOGRP
.	RADIO
.	SHAPE
.	SLIDER
.	STATTEXT
.	TABCONTROL
.	VSCROLLBAR
.
. dnumnvar values:
.
*------------------------------------------------------------------------------
.   EDITHIDESEL=dnumnvar		     CREATE  SETPROP  GETPROP
.
. Description:
.	This property negates the default behavior for EDITTEXT	object.
.	The default behavior hides the selection when the edittext 
.	loses the input	focus and inverts the selection	when the 
.	edittext receives the input focus.  If this property is	turned
.	on, the	selected text is inverted, even	if the edittext	does
.	not have the focus.
.
. Objects:
.	EDITTEXT
.
. dnumnvar values:
.
.$OFF	      EQU	0	       // Default
.$ON	      EQU	1
.
*------------------------------------------------------------------------------
.   EDITTYPE=dnumnvar			     CREATE  SETPROP  GETPROP
.
. Description:
.	This property specifies	the type of edittext object as follows:
.
.	ALL		-	Allow data entry of any	keyed characters.
.
.	DECIMAL		-	Only allow data	entry into the edittext
.				of numeric digits ( 0 through 9	), '-',
.				and '.'.
.	INTEGER		-	Only allow data	entry into the edittext
.				of numeric digits only.	( 0 through 9 ).
.	LOWERCASE	-	Convert	all characters to lowercase as they
.				are entered into the edittext.
.	UPPERCASE	-	Convert	all characters to uppercase as they
.				are entered into the edittext.
.
.	Note1:	This property should be	used in	place of older properties
.		which provide the same functions.
.
. Objects:
.	EDITTEXT
.
. dnumnvar values:
.
.$NONE	      EQU	0	       // Default
$ALL	      EQU	1
$DECIMAL      EQU	2
$INTEGER      EQU	3
$LOWERCASE    EQU	4
$UPPERCASE    EQU	5
.
*------------------------------------------------------------------------------
.   ENABLED=dnumnvar			     CREATE  SETPROP  GETPROP
.
. Description:
.	This property specifies	when an	object is to be	DISABLED/ENABLED.
.	If the ENABLED property	is set to $OFF for the CREATE/SETPROP
.	statements, this is the	same as	if a DISABLEITEM with an ITEMNO
.	value of zero was executed for the object.  If the ENABLED
.	property is set	to $ON for the SETPROP,	this is	the same as if
.	an ENABLEITEM with an ITEMNO valueo of zero was	executed for the
.	object.	 The GETPROP statement returns the current DISABLED/ENABLED
.	state for the object.
.
.	Note1: If the ENABLED property is set to $OFF for the objects
.	       ICON, MREGION, MOVIE, GROUPBOX, or PROGRESS there is
.	       no visible graying which	identifies that	the object is
.	       disabled.  However, there are no	events processed for
.	       these object when the ENABLED property is set to	$OFF.
.
. Objects:
.	BUTTON
.	CHECKBOX
.	CHECKGRP
.	COMBOBOX
.	DATALIST
.	EDITTEXT
.	GROUPBOX
.	HSCROLLBAR
.	ICON
.	MREGION
.	MOVIE
.	PICT
.	POPUPMENU
.	PROGRESS
.	RADIO
.	RADIOGRP
.	SHAPE
.	SLIDER
.	STATTEXT
.	TABCONTROL
.	VSCROLLBAR
.	WINDOW
.
. dnumnvar values:
.
.$OFF	      EQU	0
.$ON	      EQU	1	       // Default
.
*------------------------------------------------------------------------------
.   EVENTFILTER=svarslit		     CREATE  SETPROP  GETPROP
.
. Description:
.	This property specifies	a string control structure which allows
.	events for the edittext	object to be controlled.  See manual
.	discussion under create	for specific details.
.
. Objects:
.	EDITTEXT
.
. svarslit values:
.
.	The <svarslit> string is composed of 'Y' and 'N'
.	characters which relate	to a given edittext object
.	event type by position in the string.
.
*------------------------------------------------------------------------------
.   FGCOLOR=dnumnvar|color object	     CREATE  SETPROP  GETPROP
.
. Description:
.	This property allows the user program to specify the color to
.	be used	for the	foreground color of the	object.	 The parameter 
.	description is the same	as for the BDRCOLOR property.
.
. Objects:
.	CHECKBOX
.	CHECKGRP
.	COMBOBOX
.	DATALIST
.	EDITTEXT
.	GROUPBOX
.	POPUPMENU ( OBSELETE )
.	PROGRESS
.  	RADIOGRP
.	RADIO
.	STATTEXT
.	WINDOW
.
. dnumnvar values:
.
.	See BDRCOLOR property for details.
.
*------------------------------------------------------------------------------
.   FILLCOLOR=dnumnvar|color object	     CREATE  SETPROP  GETPROP
.
. Description:
.	This property specifies	the color to be	used when filling the
.	shape object.  Please see the description under	BDRCOLOR for
.	a detailed discussion of colors.
.
. Objects:
.	SHAPE
.
. dnumnvar values:
.
.	See BDRCOLOR property for details.
.
*------------------------------------------------------------------------------
.   FILLSTYLE=dnumnvar			     CREATE  SETPROP  GETPROP
.
. Description:
.	This property defines the type of hatch	used to	fill a shape 
.	object.	 The following is a summary of the types of hatches
.	available under	Windows.
.
.		Value		Meaning
.
.		DOWNDIAG	45-degree downward left-to-right hatch
.		CROSS		Horizontal and vertical	crosshatch
.		DIAGCROSS	45-degree crosshatch
.		UPDIAG		45-degree upward left-to-right hatch
.		HORIZLINE	Horizontal hatch
.		VERTLINE	Vertical hatch
.		NOHATCH		SHAPE object filled with solid color as
.				specified by FILLCOLOR.
.		CLEARHATCH	SHAPE object fill area becomes transparent
.				such that underlying colors show through
.				the object.
.
. Objects:
.	SHAPE
.
. dnumnvar values:
.
.$NONE	      EQU	0	       // Same as $NOHATCH
$CROSS	      EQU	1
$DIAGCROSS    EQU	2
$DOWNDIAG     EQU	3
$HORZLINE     EQU	4
$NOHATCH      EQU	5	       // Default
$CLEARHATCH   EQU	6
$UPDIAG	      EQU	7
$VERTLINE     EQU	8
.
*------------------------------------------------------------------------------
.   FONT=svarslit|font object		     CREATE  SETPROP  GETPROP
.
. Description:
.	This property specifies	the font to be used for	the supported
.	objects.
.
. Objects:
.	BUTTON
.	CHECKBOX
.	CHECKGRP
.	COMBOBOX
.	DATALIST
.	EDITTEXT
.	GROUPBOX
.	POPUPMENU ( OBSELETE )
.	PROGRESS
.	RADIOGRP
.	RADIO
.	STATTEXT
.	TABCONTROL
.	WINDOW
.
. svarslit|font:
.
*------------------------------------------------------------------------------
.   GROUPID=dnumnvar			     CREATE  SETPROP  GETPROP
.
. Description:
.	This property specifies	the group identifier for a radio button	or
.	checkbox object. All objects with the same group identifier are	
.	treated	as one group for purposes of moving focus using	the TAB	or 
.	arrow keys. For	radio buttons, this property is	also used to 
.	determine which	radio button to	turn off when one is turned on.
.
. Objects:
.	CHECKBOX
.	RADIO
.
. dnumnvar values:
.
*------------------------------------------------------------------------------
.   HEIGHT=dnumnvar				     SETPROP  GETPROP
.
. Description:
.	This property is a GETPROP/SETPROP only	property.  The height
.	of the object can be retrieved using the GETPROP statement.
.	The height is determined by subtracting	the top	object rectangle
.	coordinate from	the bottom object rectangle coordinate.	 The
.	SETPROP	statement can be used to change	the current height of
.	an object.
.
. Objects:
.	BUTTON
.	CHECKBOX
.	CHECKGRP
.	COMBOBOX
.	DATALIST
.	DIALOG
.	EDITTEXT
.	GROUPBOX
.	HSCROLLBAR
.	ICON
.	MREGION
.	MOVIE
.	PICT
.	POPUPMENU ( OBSELETE )
.	PROGRESS
.	RADIOGRP
.	RADIO
.	SLIDER
.	STATTEXT
.	TABCONTROL
.	VSCROLLBAR
.	WINDOW
.
. dnumnvar values:
.
.	The <dnumnvar> value is	provided in the	same units as used for
.	the creation of	an object.  This means that if an object was
.	created	using pixels, then the height expects to use pixels.
.
*------------------------------------------------------------------------------
.   HELPID=dnumnvar			     CREATE  SETPROP  GETPROP
.
. Description:
.	This property is used in conjunction with a Windows Help file
.	provided by the	user.  If a program is executing with the F1HELP
.	set to ON using	a SETMODE statement, then this HELPID value is
.	used as	the help index context value for finding a given subject
.	in the help file when an object	has the	focus and the F1 function
.	key is entered.
.
. Objects:
.	BUTTON
.	CHECKBOX
.	CHECKGRP
.	COMBOBOX
.	DATALIST
.	EDITTEXT
.	HSCROLLBAR
.	ICON
.	POPUPMENU ( OBSELETE )
.	PROGRESS
.	RADIOGRP
.	RADIO
.	SLIDER
.	TABCONTROL
.	VSCROLLBAR
.
. dnumnvar values:
.
.	The <dnumnvar> value defines a help file context index value.
.
*------------------------------------------------------------------------------
.   HWND=nvar					      GETPROP
.
. Description:
.	This property is a GETPROP only	property.  The HWND property
.	value is a Windows window handle for the supported object.
.	This HWND value	could be used in WINAPI	functions where
.	a window handle	is required for	the function.
.
. Objects:
.	BUTTON
.	CHECKBOX
.	COMBOBOX
.	DATALIST
.	DIALOG
.	EDITTEXT
.	HSCROLLBAR
.	MOVIE
.	POPUPMENU ( OBSELETE )
.	RADIO
.	SLIDER
.	TABCONTROL
.	VSCROLLBAR
.	WINDOW
.
. nvar values:
.
.	The <nvar> value retrieve is a Windows window handle for
.	the supported object.
.
*------------------------------------------------------------------------------
.   INTEGRAL=dnumnvar			     CREATE  SETPROP  GETPROP
.
. Description:
.	This property specifies	when a DATALIST	is to resize for the
.	number of items	in a list versus leaving the size of the DATALIST
.	to be as defined by the	user rectangle coordinates.  When the
.	INTEGRAL property is set to OFF, then the size of the list box
.	is the size specified by the user coordinate values.  When the
.	INTEGRAL property is set to ON,	then the normal	action of Windows
.	is to resize a list box	so that	the list box does not display
.	partial	items.
.
. Objects:
.	DATALIST
.
. dnumnvar values:
.
.$OFF	      EQU	0
.$ON	      EQU	1	       // Default
.
*------------------------------------------------------------------------------
.   ITALIC[=dnumnvar]			     CREATE  SETPROP  GETPROP
.
. Description:
.	This property specifies	that the font should be	italic.	 When
.	the <dnumnvar> value is	not provided, this indicates that the
.	ITALIC property	is turned on.
.
. Objects:
.	FONT
.
. dnumnvar values:
.
.$OFF	      EQU	0	       // Default
.$ON	      EQU	1
.
*------------------------------------------------------------------------------
.   LEFT=dnumnvar				     SETPROP  GETPROP
.
. Description:
.	This property is a GETPROP/SETPROP only	property.  The LEFT
.	property returns the left coordinate of	a created objected.
.
. Objects:
.	BUTTON
.	CHECKBOX
.	CHECKGRP
.	COMBOBOX
.	DATALIST
.	DIALOG
.	EDITTEXT
.	FLOATMENU
.	GROUPBOX
.	HSCROLLBAR
.	ICON
.	MREGION
.	MOVIE
.	PICT
.	POPUPMENU ( OBSELETE )
.	PROGRESS
.	RADIOGRP
.	RADIO
.	SLIDER
.	STATTEXT
.	TABCONTROL
.	VSCROLLBAR
.	WINDOW
.
. dnumnvar values:
.
.	The <dnumnvar> value is	provided in the	same units as used when
.	the object was created.
.
*------------------------------------------------------------------------------
.   LINKHSCROLL=<hscrollbar>		     CREATE	      GETPROP
.
. Description:
.	This property is to allow a horizontal scrollbar object	to be 
.	linked to the scrollbar	which may be generated for a PICT or a
.	WINDOW object.	After the logical linkage is established, then
.	operations on the linked scrollbar can be executed.  This means
.	that a GETITEM for the <hscrollbar> object can be executed to
.	determine the current settings of the scrollbar	which has be
.	generated for another object.
.
.	Note1: It is not possible to change the	MIN, MAX, or SHIFT values
.	       for a picture linked scrollbar using either the SETPROP or 
.	       SETITEM.	A picture linked scrollbar can change the thumb	
.	       position	and tab	id. These restrictions do not apply to
.	       a window	linked scrollbar.
.
. Objects:
.	PICT
.	WINDOW
.
*------------------------------------------------------------------------------
.   LINKVSCROLL=<vscrollbar>		     CREATE	      GETPROP
.
. Description:
.	This property is to allow a vertical scrollbar object to be
.	linked to the scrollbar	which may be generated for a PICT or a
.	WINDOW object.	After the logical linkage is established, then
.	operations on the linked scrollbar can be executed.  This means
.	that a GETITEM for the <vscrollbar> object can be executed to
.	determine the current settings of the scrollbar	which has be
.	generated for another object.
.
.	Note1: It is not possible to change the	MIN, MAX, or SHIFT values
.	       for a linked scrollbar using either the SETPROP or SETITEM.
.	       It is possible to change	the thumb position however.
.
. Objects:
.	PICT
.	WINDOW
.
*------------------------------------------------------------------------------
.   LOWERCASE[=dnumnvar]		     CREATE
.
. Description:
.	This property is a CREATE only property.
.	The property specifies that the	EDITTEXT object	is to convert
.	all entered data to lowercase.	When this property is set to
.	be OFF,	then this indicates that no data conversion occurs.
.
.	Note1: Use of the property should be replaced through use of 
.	       the EDITTYPE property.
.
. Objects:
.	EDITTEXT
.
. dnumnvar values:
.
.$OFF	      EQU	0	       // Default
.$ON	      EQU	1
.
*------------------------------------------------------------------------------
.   MAX=dnumnvar			     See Objects:
.
. Description:
.	This property defines the maximum value	when the slider	thumb is at
.	the bottom or right most position of the supported object.
.
. Objects:
.	HSCROLLBAR				     SETPROP  GETPROP
.	SLIDER				     CREATE  SETPROP  GETPROP
.	VSCROLLBAR				     SETPROP  GETPROP
.
. dnumnvar values:
.
.	The value is a logical unit as specified by the	user.
.
*------------------------------------------------------------------------------
.   MAXBOX=dnumnvar			     CREATE  SETPROP  GETPROP
.
. Description:
.	This property specifies	when a MAXIMIZE	box for	the window
.	object should be allowed or not.
.
.	Note1: The MAXBOX property does	not have an effect if the 
.	       CAPTION property	is set to $OFF for a MODAL or MODELESS
.	       window type.
.
. Objects:
.	WINDOW
.
. dnumnvar values:
.
.$OFF	      EQU	0
.$ON	      EQU	1	       // Default
.
*------------------------------------------------------------------------------
.   MAXCHARS=dnumnvar			     CREATE  SETPROP  GETPROP
.
. Description:
.	Specifies the maximum number of	characters allowed to
.	be entered into	edittext object. This applies whether
.	edittext is single line	or multiline.
.
. Objects:
.	EDITTEXT
.
. dnumnvar values:
.
.	The <dnumnvar> value defines number of characters.
.
*------------------------------------------------------------------------------
.   MAXCOLS=dnumnvar			     CREATE  SETPROP  GETPROP
.
. Description:
.	The property is	the maximum number of characters allowed for
.	an individual line in a	multiline edittext object.  This property
.	is ignored if the MULTILINE is not used.
.
. Objects:
.	EDITTEXT
.
. dnumnvar values:
.
.	The <dnumnvar> value defines number of characters.
.
*------------------------------------------------------------------------------
.   MIN=dnumnvar			     See Objects:
.
. Description:
.	This property defines the minimum value	when the slider	thumb is at
.	the top	or left	most position of the supported object.
.
. Objects:
.	HSCROLLBAR				     SETPROP  GETPROP
.	SLIDER				     CREATE  SETPROP  GETPROP
.	VSCROLLBAR				     SETPROP  GETPROP
.
. dnumnvar values:
.
.	The value is a logical unit as specified by the	user.
.
*------------------------------------------------------------------------------
.   MINBOX=dnumnvar			     CREATE  SETPROP  GETPROP
.
. Description:
.	This property specifies	when a MINIMIZE	box for	the window
.	object should be allowed or not.
.
.	Note1: The MINBOX property does	not have an effect if the 
.	       CAPTION property	is set to $OFF for a MODAL or MODELESS
.	       window type.
.
. Objects:
.	WINDOW
.
. dnumnvar values:
.
.$OFF	      EQU	0
.$ON	      EQU	1	       // Default
.
*------------------------------------------------------------------------------
.   MULTILINE[=dnumnvar]		     CREATE  SETPROP  GETPROP
.
. Description:
.	This property defines the number of lines to be	allowed
.	for an edittext	object.	 If a user <dnumnvar> value
.	is not specified, then the number of lines allowed is
.	set to 32000.  If the number of	lines specified	can
.	fit into the user specified rectangle coordinates, then
.	scrollbars are not provided.  Otherwise, scrollbars will
.	exist for the edittext object.
.
. Objects:
.	EDITTEXT
.
. dnumnvar values:
.
.	The <dnumnvar> value specifies number of lines.
.
*------------------------------------------------------------------------------
.   MULTIROW=dnumnvar			     CREATE  SETPROP  GETPROP
.
. Description:
.	This property is used to indicate if labels are	allowed	to be 
.	shown in multiple rows.	If multiple rows are not allowed, and
.	the labels are too large to fit	in one row, left and right 
.	arrow buttons are drawn	to allow access	to the other labels.
.
. Objects:
.	TABCONTROL
.
. dnumnvar values:
.
.$OFF	      EQU	0	     //	Default
.$ON	      EQU	1	       
.
*------------------------------------------------------------------------------
.   MULTISELECT[=dnumnvar]		     CREATE  SETPROP  GETPROP
.
. Description:
.	This property allows a multiple	items to be selected from a
.	DATALIST.  If the <dnumnvar> parameter is not provided,	the
.	MULTISELECT is set to ON.
.
. Objects:
.	DATALIST
.
. dnumnvar values:
.
.$OFF	      EQU	0	       // Default
.$ON	      EQU	1
.
*------------------------------------------------------------------------------
.   NOAUTOZOOM	(OBSELETE)		     CREATE
.
. Description:
.	This is	an obselete property maintained	only for backward
.	source compatibility.  The NOAUTOZOOM property is the same
.	as AUTOZOOM=0.
.
.	Note1:	The AUTOZOOM property must be used if SETPROP/GETPROP
.		operations are required.
.
. Objects:
.	PICT
.
. dnumnvar values:
.
.	None
.
*------------------------------------------------------------------------------
.   NOINTEGRAL	(OBSELETE)		     CREATE
.
. Description:
.	This property is a CREATE only property	and is obselete.
.	This property maintained only for backward source compatibility.
.	The NOINTEGRAL property	is the same as INTEGRAL=0.
.
.	Note1:	The INTEGRAL property must be used if SETPROP/GETPROP
.		operations are required.
.
. Objects:
.	DATALIST
.
. dnumnvar values:
.
.	None
.
*------------------------------------------------------------------------------
.   NOPERCENT	(OBSELETE)		     CREATE
.
. Description:
.	This property is a CREATE only property	and is obselete.
.	This property maintained only for backward source compatibility.
.	The NOPERCENT property is the same as PERCENT=0.
.
.	Note1:	The PERCENT property must be used if SETPROP/GETPROP
.		operations are required.
.
. Objects:
.	PROGRESS
.
. dnumnvar values:
.
.	None
.
*------------------------------------------------------------------------------
.   NORESIZE	(OBSELETE)		     CREATE
.
. Description:
.	This property is a CREATE only property	and is obselete.
.	This property maintained only for backward source compatibility.
.	The NORESIZE property is the same as RESIZE=0.
.
.	Note1:	The RESIZE property must be used if SETPROP/GETPROP
.		operations are required.
.
. Objects:
.	PICT
.
. dnumnvar values:
.
.	None
.
*------------------------------------------------------------------------------
.   NOSCROLL	(OBSELETE)		     CREATE
.
. Description:
.	This property is a CREATE only property	and is obselete.
.	This property maintained only for backward source compatibility.
.	The NOSCROLL property is the same as SCROLLBAR=0.
.
.	Note1:	The SCROLLBAR property must be used if SETPROP/GETPROP
.		operations are required.
.
. Objects:
.	PICT
.
. dnumnvar values:
.
.	None
.
*------------------------------------------------------------------------------
.   NUMERIC[=dnumnvar]			     CREATE
.
. Description:
.	This property is a CREATE only property.
.	The property specifies that the	EDITTEXT object	is to allow 
.	numeric	digits 0 through 9, '-'	character, and '.' character.
.	When this property is set to the OFF state, this indicates that
.	all characters are allowed.  If	the <dnumnvar> value is	not
.	provided, the NUMERIC property is set to ON.
.
.	Note1: Use of the property should be replaced through use of 
.	       the EDITTYPE property.
.
. Objects:
.	EDITTEXT
.
. dnumnvar values:
.
.$OFF	      EQU	1	       // Default
.$ON	      EQU	1
.
*------------------------------------------------------------------------------
.   OBJECTID=dnumnvar			     CREATE  SETPROP  GETPROP
.
. Description:
.	This property specifies	a identification number	for an object.
.	The OBJECTID value is available	through	the use	of the EVENTINFO
.	or EVENTREG statements to identify the object which caused
.	an event to occur.
.
. Objects:
.	BUTTON
.	CHECKBOX
.	CHECKGRP
.	COMBOBOX
.	DATALIST
.	DIALOG
.	EDITTEXT
.	FLOATMENU
.	GROUPBOX
.	HSCROLLBAR
.	ICON
.	LINE
.	MENU
.	MREGION
.	MOVIE
.	PICT
.	POPUPMENU ( OBSELETE )
.	PROGRESS
.	RADIOGRP
.	RADIO
.	SHAPE
.	SLIDER
.	STATTEXT
.	SUBMENU
.	TABCONTROL
.	TIMER
.	VSCROLLBAR
.	WINDOW
.
. dnumnvar values:
.
.	The <dnumvar> is a user	specified number to identify an	object.
.
*------------------------------------------------------------------------------
.   ORIENTATION=dnumnvar		     CREATE  SETPROP  GETPROP
.
. Description:
.	The property controls the whether a slider object is to	be
.	horizontal or vertical.
.
. Objects:
.	SLIDER
.
. dnumnvar values:
.
.$NONE	      EQU	0	       // Same as $VERT
$HORZ	      EQU	1
$VERT	      EQU	2	       // Default
.
*------------------------------------------------------------------------------
.   PAGE=dnumnvar			     CREATE 
.
. Description:
.	This is	a CREATE only property.
.	This property specifies	the page to be used for	the pict object
.	when a DCX picture is used.
.
. Objects:
.	PICT
.
. dnumnvar values:
.
.	The <dnumnvar> specifies a page	value.
.
*------------------------------------------------------------------------------
.   PASSWORD[=dnumnvar]			     CREATE
.
. Description:
.	This is	a CREATE only property.
.	This property specifies	that the edittext is to	have each 
.	character echoed as a '*' character.  If the <dnumnvar>	value
.	is not specified, the PASSWORD property	is set to ON.
.
. Objects:
.	EDITTEXT
.
. dnumnvar values:
.
.$OFF	      EQU	0	       // Default
.$ON	      EQU	1
.
*------------------------------------------------------------------------------
.   PERCENT=dnumnvar			     CREATE  SETPROP  GETPROP
.
. Description:
.	This property specifies	if a percentage	value is to be displayed
.	for the	progress object	or not.
.
. Objects:
.	PROGRESS
.
. dnumnvar values:
.
.$OFF	      EQU	0	       // Default
.$ON	      EQU	1
.
*------------------------------------------------------------------------------
.   PICTSIZEH=nvar					    GETPROP
.
. Description:
.	This property is a GETPROP only	property. This retrieves the
.	horizontal size	of a picture. The value	is expressed in	pixels.
.
. Objects:
.	PICT
.
. nvar values:
.
.	The <nvar> value retrieved is the horizontal pixel size	of the picture.
.
*------------------------------------------------------------------------------
.   PICTSIZEV=nvar					    GETPROP
.
. Description:
.	This property is a GETPROP only	property. This retrieves the
.	vertical size of a picture. The	value is expressed in pixels.
.
. Objects:
.	PICT
.
. nvar values:
.
.	The <nvar> value retrieved is the vertical pixel size of the picture.
.
*------------------------------------------------------------------------------
.   POS=dnumnvarH:dnumnvarV			     SETPROP  GETPROP
.
. Description:
.	This property is a SETPROP/GETPROP only	property.
.	The POS	property specifies the starting	horizontal and vertical
.	pixel coordinate in a pict object where	the picture is to be
.	displayed.
.
. Objects:
.	PICT
.
. dnumnvar values:
.
.	The <dnumnvarH>	value is the horizontal	pixel coordinate in the	pict.
.	The <dnumnvarV>	value is the vertical pixel coordinate in the pict.
.
*------------------------------------------------------------------------------
.   PRINTER[=dnumnvar]			     CREATE  SETPROP  GETPROP
.
. Description:
.	This property specifies	that the datalist object is to be filled
.	with the current Windows printer names.	 If the	<dnumnvar> value
.	is not provided, the PRINTER property is set to	ON.
.
. Objects:
.	DATALIST
.
. dnumnvar values:
.
.$OFF	      EQU	0	       // Default
.$ON	      EQU	1
.
*------------------------------------------------------------------------------
.   READONLY[=dnumnvar]			     CREATE  SETPROP  GETPROP
.
. Description:
.	This property specifies	that the edittext object is a read only
.	object.	 The user can place the	caret into the edittext	object
.	to cut/copy data, however, the current edittext	data can not
.	be changed by the user.	 If the	<dnumnvar> value is not	specified,
.	the READONLY property is turned	on.
.
. Objects:
.	EDITTEXT
.
. dnumnvar values:
.
.$OFF	      EQU	0	       Default
.$ON	      EQU	1
.
*------------------------------------------------------------------------------
.   RESIZE=dnumnvar			     CREATE  SETPROP  GETPROP
.
. Description:
.	This property specifies	if a picture is	to be resized to fit
.	pict object rectangle coordinates.
.
. Objects:
.	PICT
.
. dnumnvar values:
.
.$OFF	      EQU	0	       // Default
.$ON	      EQU	1
.
*------------------------------------------------------------------------------
.   RESOURCE=dnumnvar				     SETPROP  GETPROP
.
. Description:
.	This property is a SETPROP/GETPROP only	property.
.	The RESOURCE property allows a resource	id to be retreived/changed
.	for supported objects.
.
. Objects:
.	ICON
.	PICT
.
. dnumnvar values:
.
.	The <dnumnvar> value is	valid resource id which	can be found
.	in a resource DLL or resources loaded for a PLFORM.
.
*------------------------------------------------------------------------------
.   RIGHT[=dnumnvar]			     CREATE
.
. Description:
.	The RIGHT property specifies that the text is to be right justified 
.	horizontally for the object.  The property is replaced by 
.	the ALIGNMENT property.	 The ALIGNMENT property	is to be used
.	for performing SETPROP and GETPROP operations.
.
.	If the syntax form is used where the <dnumnvar>	value is not
.	specified, this	indicates that the RIGHT property is to	be
.	turned on.
.
.	Also note that if the RIGHT property is	turned off, this causes
.	the text output	to be justified	to the left.
.
. Objects:
.	EDITTEXT
.	STATTEXT
.
. dnumnvar values:
.
.$OFF	      EQU	0	       // Default
.$ON	      EQU	1
.
*------------------------------------------------------------------------------
.   RIGHTBUTTON[=dnumnvar]		     CREATE  SETPROP  GETPROP
.
. Description:
.	This property specifies	whether	the right mouse	button can be
.	used to	cause an event or not.	If the <dnumnvar> value
.	is not provided, the RIGHTBUTTON property is turned on.
.
. Objects:
.	FLOATMENU
.
. dnumnvar values:
.
.$OFF	      EQU	0	       // Default
.$ON	      EQU	1
.
*------------------------------------------------------------------------------
.   SCALE=dnumnvar			     CREATE  SETPROP  GETPROP
.
. Description:
.	This property specifies	a percentage by	which a	picture	object
.	is scaled.
.
. Objects:
.	PICT
.
. dnumnvar values:
.
.	The <dnumnvar> value is	percentage value.
.
*------------------------------------------------------------------------------
.   SCROLLBAR[=dnumnvar]		     CREATE  SETPROP  GETPROP
.
. Description:
.	This property specifies	what type of scrollbars	are to be provided
.	for a picture or window	object.	 If the	<dnumnvar> value
.	is not provided, the SCROLLBAR property	is set to $SCRBOTH.
.
. Objects:
.	PICT
.	WINDOW
.
. dnumnvar values:
.
.$NONE	      EQU	0	       // Default
$SCRBOTH      EQU	1
$SCRHORZ      EQU	2
$SCRNONE      EQU	3
$SCRVERT      EQU	4
.
*------------------------------------------------------------------------------
.   SELLENGTH=dnumnvar			     CREATE  SETPROP  GETPROP
.
. Description:
.	This property specifies	the number of units to highlight in
.	the slider object view when the	SELRANGE property is set to ON.
.	It is recommended that the SELLENGTH value be defined in the
.	range specified	by the MAX and MIN property values.
.
.	Note1:	The highlighted	output does not	occur when the SELLENGTH
.		property value plus the	SELSTART value is greater than
.		32767.	The highlighted	output may not occur if	'nvar'
.		value is negative.
.		
.
. Objects:
.	SLIDER
.
. dnumnvar values:
.
.	The <dnumnvar> value is	logical	user specified units.
.
*------------------------------------------------------------------------------
.   SELRANGE=dnumnvar			     CREATE  SETPROP  GETPROP
.
. Description:
.	The property specifies if a highlighted	selection is to	be 
.	applied	to the slider object.  When this PROPERTY it turned
.	ON, the	slider object uses the SELSTART	and SELLENGTH
.	values to define the highlighted selection output.
.
. Objects:
.	SLIDER
.
. dnumnvar values:
.
.$OFF	      EQU	0	       // Default
.$ON	      EQU	1
.
*------------------------------------------------------------------------------
.   SELSTART=dnumnvar			     CREATE  SETPROP  GETPROP
.
. Description:
.	This property specifies	the starting logical unit from which
.	the slider object begins the highlighed	output when the	SELRANGE
.	property is set	to ON.
.
.	Note1:	The highlighted	output does not	occur when the SELLENGTH
.		property value plus the	SELSTART value is greater than
.		32767.	The highlighted	output may not occur if	'nvar'
.		value is negative.
.
. Objects:
.	SLIDER
.
. dnumnvar values:
.
.	The <dnumnvar> value is	logical	user specified unit.
.
*------------------------------------------------------------------------------
.   SHAPE=dnumnvar			     CREATE  SETPROP  GETPROP
.
. Description:
.	This property specifies	the type of shape to be	output for the
.	shape object.  If the SHAPE property is	invalid, the shape
.	object defaults	to a rectangle output.
.
. Objects:
.	SHAPE
.
. dnumnvar values:
.
.$NONE	      EQU	0	       // Same as $RECTANGLE
$OVAL	      EQU	1
$RECTANGLE    EQU	2	       // Default
$RNDRECT      EQU	3
.
*------------------------------------------------------------------------------
.   SHIFT=dnumnvar			     See Objects:
.
. Description:
.	This property defines the number of units to move the slider
.	thumb when the mouse is	clicked	in the object slide area.
.	This also occurs when the page up or page down keys are	entered.
.
. Objects:
.	HSCROLLBAR				     SETPROP  GETPROP
.	SLIDER				     CREATE  SETPROP  GETPROP
.	VSCROLLBAR				     SETPROP  GETPROP
.
. dnumnvar values:
.
.	The value is a logical unit as specified by the	user.
.
*------------------------------------------------------------------------------
.   SIZE=dnumnvar			     CREATE  SETPROP  GETPROP
.
. Description:
.	This property specifies	the point size for a font object.
.
. Objects:
.	FONT
.
. dnumnvar values:
.
.	The <dnumnvar> value is	a point	size for a font.
.
*------------------------------------------------------------------------------
.   SORTED[=dnumnvar]			     CREATE  SETPROP  GETPROP
.
. Description:
.	This property specifies	that the items in the datalist object
.	are to be sorted.  If the <dnumnvar> value is not provided,
.	the SORTED property is turned ON.
.
.	Note1:	When the SORTED	property is used, Windows controls the
.		sorting	method.	 The Windows method is to sort the
.		item name field	alphabetically starting	with the first
.		character.  Windows does not support any alternate sorting
.		methods.
.
. Objects:
.	COMBOBOX
.	DATALIST
.	POPUPMENU ( OBSELETE )
.
. dnumnvar values:
.
.$OFF	      EQU	0	       // Default
.$ON	      EQU	1
.
*------------------------------------------------------------------------------
.   STATIC[=dnumnvar]			     CREATE  SETPROP  GETPROP
.
. Description:
.	This property specifies	that the edittext object is used for
.	output only purposes.  An edittext object with this property
.	can not	get the	focus and not cut/copy operations can be perform.
.
. Objects:
.	EDITTEXT
.
. dnumnvar values:
.
.$OFF	      EQU	0	       // Default
.$ON	      EQU	1
.
*------------------------------------------------------------------------------
.   STYLE=dnumnvar|3DON|3DOFF|3DFLAT|3DOUT   CREATE  SETPROP  GETPROP
.
. Description:
.	The STYLE property specifies when an object should
.	have a 3D look or a non-3D look.  The various forms of STYLE
.	defines	that an	object can have	presentations as follows:
.
.	  3DOFF	- Object does not have a 3D look.
.	  3DON	- Ojbect has a 3D sunken look.
.	  3DFLAT- Object has a 3D flat look.
.	  3DOUT	- Object has a 3D outward look.
.
.	Note1: Some objects are	limited	to presentations described as
.	       3DON or 3DOFF only.  For	these objects the user should
.	       use the APPEARANCE property and not the STYLE property.
.
. Objects:
.	CHECKBOX		(See Note1:)
.	CHECKGRP		(See Note1:)
.	COMBOBOX		(See Note1:)
.	DATALIST		(See Note1:)
.	EDITTEXT
.	GROUPBOX
.	MOVIE
.	PICT
.	POPUPMENU ( OBSELETE )	(See Note1:)
.	PROGRESS
.	RADIOGRP		(See Note1:)
.	RADIO			(See Note1:)
.	STATTEXT
.	WINDOW			(See Note1:)
.
. dnumnvar values:
.
$DEFAULT      EQU	0	       // Default
$3DON	      EQU	1
$3DOFF	      EQU	2
$3DFLAT	      EQU	3
$3DOUT	      EQU	4
.
*------------------------------------------------------------------------------
.   SYSMENU=dnumnvar			     CREATE  SETPROP  GETPROP
.
. Description:
.	This property specifies	whether	a window object	should output
.	a sysmenu or not.
.
.	Note1: The SYSMENU property does not have an effect if the 
.	       CAPTION property	is set to $OFF for a MODAL or MODELESS
.	       window type.
.
. Objects:
.	WINDOW
.
. dnumnvar values:
.
.$OFF	      EQU	0
.$ON	      EQU	1	       // Default
.
*------------------------------------------------------------------------------
.   TABFIXHTH=dnumnvar			     CREATE  SETPROP  GETPROP
.
. Description:
.	This property specifies	the height in pixels for the 
.	TABCONTROL labels.  This property only takes affect when the
.	TABWTHSTYLE property is	set to FIXED.
.
. Objects:
.	TABCONTROL
.
. dnumnvar values:
.
.	The <dnumnvar> value is	the number of pixels for label height.
.
*------------------------------------------------------------------------------
.   TABFIXWTH=dnumnvar			     CREATE  SETPROP  GETPROP
.
. Description:
.	This property specifies	the width in pixels for	the 
.	TABCONTROL labels.  This property only takes affect when the
.	TABWTHSTYLE property is	set to FIXED.
.
. Objects:
.	TABCONTROL
.
. dnumnvar values:
.
.	The <dnumnvar> value is	the number of pixels for label width.
.
*------------------------------------------------------------------------------
.   TABID=dnumnvar			     CREATE  SETPROP  GETPROP
.
. Description:
.	This property specifies	the location of	this object in the
.	tabbing	order sequence.	 If the	TABID property is not used, the
.	object is assigned an internal tabbing order value which is 
.	determined by the order	that objects are created and an	
.	intial TABID value specified for the window of the object.  If the
.	TABID property id is specified,	then the value of the TABID
.	determines the location	of the object in the tabbing sequence.
.	The tabbing sequence processes from lower TABID	valued objects
.	to higher TABID	valued objects as the TAB key is entered.
.
.	Note1:	The *NODEFTABID=OFF in the SETMODE statement can be
.		used to	exclude	any object with	an internally assigned
.		tabid from the tabbing sequence.
.
. Objects:
.	BUTTON
.	CHECKBOX
.	CHECKGRP
.	COMBOBOX
.	DATALIST
.	EDITTEXT
.	HSCROLLBAR
.	POPUPMENU ( OBSELETE )
.	RADIOGRP
.	RADIO
.	SLIDER
.	TABCONTROL
.	VSCROLLBAR
.	WINDOW
.
. dnumnvar values:
.
.	The <dnumnvar> value is	a user specified value to tabbing order.
.
*------------------------------------------------------------------------------
.   TABLABEL=svarslit			     CREATE  SETPROP  GETPROP
.
. Description:
.	This property provides the data	items used to generate the
.	TABCONTROL labels.  The	data provided in the <svarslit>	is
.	set of items separated by semicolon characters.	 If the	TABLABEL 
.	is not specified or the	<svarslit> is NULL, then the TABCONTROL
.	will not have any tab labels.
.
. Objects:
.	TABCONTROL
.
. svarslit values:
.
.	The <svarslit> is a list of items separated by a semicolon.
.
*------------------------------------------------------------------------------
.   TABSTOPS=svarslit			     CREATE  SETPROP  GETPROP
.
. Description:
.	This property specifies	a data string which is composed	of tabstop
.	location values	separated by semicolon characters.  See	the manual
.	for detailed description.
.
. Objects:
.	DATALIST
.
. svarslit values:
.
.	The <svarslit> data string is tabstop location values separated
.	by semicolons.
.
*------------------------------------------------------------------------------
.   TABWTHSTYLE=dnumnvar		     CREATE  SETPROP  GETPROP
.
. Description:
.	This property specifies	the look of the	labels.	If the $FIXED 
.	value is used, the TABFIXHTH and TABFIXWTH properties determine
.	the height and width of	the labels. The	$JUST value causes any
.	multiple row TABCONTROL	to size	the labels evenly to fill each row.
.
. Objects:
.	TABCONTROL
.
. dnumnvar values:
.
.$NONE	      EQU	0
$FIXED	      EQU	1
$JUST	      EQU	2
$NOJUST	      EQU	3
.
*------------------------------------------------------------------------------
.   TICKFREQ=dnumnvar			     CREATE  SETPROP  GETPROP
.
. Description:
.	This property specifies	the frequency at which tick marks are
.	applied	to the slider object output.  The <dnumnvar> value for
.	this property should be	in the range of	the MIN	and MAX	values.
.
. Objects:
.	SLIDER
.
. dnumnvar values:
.
.	The <dnumnvar> is a user logical unit value.
.
*------------------------------------------------------------------------------
.   TICKSTYLE=dnumnvar			     CREATE  SETPROP  GETPROP
.
. Description:
.	This property specifies	the type of look that a	slider object
.	takes on.  This	property causes	tick unit marks	
.	to be placed on	either side of the slide thumb control;
.	on both	sides of the slide thumb control; or no	tick
.	mark output at all.  The number	of tick	units to be applied
.	depend on the MAX, MIN,	and TICKFREQ property values.
.
.	Note1: Values other than those shown for <dnumnvar> result in
.	       no tick mark output.
.
. Objects:
.	SLIDER
.
. dnumnvar values:
.
.$NONE	      EQU	0
$TICKBOTH     EQU	1
$TICKBOTTOM   EQU	2
$TICKRIGHT    EQU	2
$TICKTOP      EQU	4
$TICKLEFT     EQU	4
.
*------------------------------------------------------------------------------
.   TITLE=svarslit			     CREATE  SETPROP  GETPROP
.
. Description:
.	This property allows a title to	be specified for the supported
.	objects.
.
. Objects:
.	CHECKGRP
.	GROUPBOX
.	RADIOGRP
.	WINDOW
.
. svarslit values:
.
.	The <svarslit> variable	is a title string.
.
*------------------------------------------------------------------------------
.   TOP=dnumnvar				     SETPROP  GETPROP
.
. Description:
.	This property is a SETPROP/GETPROP only	property.  The property
.	is the current top coordinate for an object.
.
. Objects:
.	BUTTON
.	CHECKBOX
.	CHECKGRP
.	COMBOBOX
.	DATALIST
.	DIALOG
.	EDITTEXT
.	FLOATMENU
.	GROUPBOX
.	HSCROLLBAR
.	ICON
.	MREGION
.	MOVIE
.	PICT
.	POPUPMENU ( OBSELETE )
.	PROGRESS
.	RADIOGRP
.	RADIO
.	SLIDER
.	STATTEXT
.	TABCONTROL
.	VSCROLLBAR
.	WINDOW
.
. dnumnvar values:
.
.	The <dnumnvar> value is	provided in the	same units as used when
.	the object was created.	
.
*------------------------------------------------------------------------------
.   TRISTATE[=dnumnvar]			     CREATE  SETPROP  GETPROP
.
. Description:
.	This property specifies	that a checkbox	object is to have the
.	capability of a	third state or not.  When the TRISTATE property
.	is turned ON, this allows the checkbox object to be checked
.	and grayed as a	third state.  When the TRISATE property	is turned
.	OFF, the checkbox object only has two states where the checkbox
.	is either blanked or checked.
.
.	Note1:	Under program control the SETITEM statement can	be used
.		to set the current state of the	checkbox object.
.
. Objects:
.	CHECKBOX
.	CHECKGRP
.
. dnumnvar values:
.
.$OFF	      EQU	0	       // Default
.$ON	      EQU	1
.
*------------------------------------------------------------------------------
.   UPPERCASE[=dnumnvar]		     CREATE
.
. Description:
.	This property is a CREATE only property.
.	This property specifies	that the edittext object is to convert
.	all entered data to uppercase.	When this property is set to
.	be OFF,	then this indicates that no data conversion occurs.
.
.	Note1: Use of the property should be replaced by using the EDITTYPE
.	       property.
.
. Objects:
.	EDITTEXT
.
. dnumnvar values:
.
.$OFF	      EQU	0	       // Default
.$ON	      EQU	1
.
*------------------------------------------------------------------------------
.   USEALTKEY=dnumnvar			     CREATE  SETPROP  GETPROP
.
. Description:
.	This property specifies	if the Windows ALT+KEY is to be	allowed
.	for the	stattext object.  If the USEALTKEY property is set to ON,
.	then using an '&' character in the name	of the stattext	object
.	will allow the ALT+KEY feature to work.	 If the	USEALTKEY property
.	is set to OFF, then any	'&' character in the name of the
.	stattext object	is output as part of the name and the ALT+KEY
.	feature	does not generate any events for the object.
.
. Objects:
.	STATTEXT
.
. dnumnvar values:
.
.$OFF	      EQU	0	       // Default
.$ON	      EQU	1
.
*------------------------------------------------------------------------------
.   VISIBLE=dnumnvar			     CREATE  SETPROP  GETPROP
.
. Description:
.	This property specifies	if an object is	to be made visible or
.	invisible.  If the VISIBLE property is set to $ON and the current
.	state of the object is invisible, then this is the same	as if
.	if an ACTIVATE statement without any activation	routine	was 
.	executed for the object.  However, if the VISIBLE property is set 
.	$ON and	the object is already visible, then the	object remains
.	visible	and no change occurs.  If the VISIBLE property is set to
.	$OFF, this is the same as if a DEACTIVATE statement was	executed
.	for the	object.	 The GETPROP retrieves the current VISIBLE state
.	for the	object.
.
.	Note1: The VISIBLE property is ignored for a WINDOW object when
.	       the WINTYPE is set for MODAL.  This can not be allowed 
.	       since the program execution remains at the ACTIVATE
.	       statement while the MODAL dialog	is processed.  If the
.	       VISIBLE property	were allowed, then this	would cause
.	       program execution to remain at the CREATE statement
.	       causing a program to hang since no events could be 
.	       registered for use.
.	       
. Objects:
.	BUTTON
.	CHECKBOX
.	CHECKGRP
.	COMBOBOX
.	DATALIST
.	EDITTEXT
.	GROUPBOX
.	HSCROLLBAR
.	ICON
.	MOVIE
.	PICT
.	POPUPMENU
.	PROGRESS
.	RADIO
.	RADIOGRP
.	SHAPE
.	SLIDER
.	STATTEXT
.	TABCONTROL
.	VSCROLLBAR
.	WINDOW
.
. dnumnvar values:
.
.$OFF	      EQU	0	       // Default
.$ON	      EQU	1
.
*------------------------------------------------------------------------------
.   WIDTH=dnumnvar				     SETPROP  GETPROP
.
. Description:
.	This property is a GETPROP/SETPROP only	property.  The width
.	of the object can be retrieved using the GETPROP statement.
.	The width is determined	by subtracting the left	object rectangle
.	coordinate from	the right object rectangle coordinate.	The
.	SETPROP	statement can be used to change	the current width of
.	an object.
.
. Objects:
.	BUTTON
.	CHECKBOX
.	CHECKGRP
.	COMBOBOX
.	DATALIST
.	DIALOG
.	EDITTEXT
.	GROUPBOX
.	HSCROLLBAR
.	ICON
.	MREGION
.	MOVIE
.	PICT
.	POPUPMENU ( OBSELETE )
.	PROGRESS
.	RADIOGRP
.	RADIO
.	SLIDER
.	STATTEXT
.	TABCONTROL
.	VSCROLLBAR
.	WINDOW
.
. dnumnvar values:
.
.	The <dnumnvar> value is	provided in the	same units as used for
.	the creation of	an object.  This means that if an object was
.	created	using pixels, then the width expects to	use pixels.
.
*------------------------------------------------------------------------------
.   WINOFFSETH=dnumnvar				     SETPROP  GETPROP
.
. Description:
.	This property is a GETPROP/SETPROP only	property.  The WINOFFSETH
.	property identifies a relative horizontal offset for the
.	visible	viewing	area of	a WINDOW.  By default the visible
.	viewing	offset starts with a zero value.
.
.	Note1: It is possible to create	objects	outside	the normal
.	       viewing area for	a Window.  If this is done, then the
.	       WINOFFSETH and WINOFFSETV properties can	be used	to
.	       bring these objects into	the viewing area.
.
. Objects:
.	WINDOW
.
. dnumnvar values:
.
.	The <dnumnvar> value is	a pixel	value which defines a horizontal
.	offset for the visible area of a Window.
.
*------------------------------------------------------------------------------
.   WINOFFSETV=dnumnvar				     SETPROP  GETPROP
.
. Description:
.	This property is a GETPROP/SETPROP only	property.  The WINOFFSETV
.	property identifies a relative vertical	offset for the
.	visible	viewing	area of	a WINDOW.  By default the
.	visible	viewing	offset starts with a zero value.
.
.	Note1: It is possible to create	objects	outside	the normal
.	       viewing area for	a Window.  If this is done, then the
.	       WINOFFSETH and WINOFFSETV properties can	be used	to
.	       bring these objects into	the viewing area.
.
. Objects:
.	WINDOW
.
. dnumnvar values:
.
.	The <dnumnvar> value is	a pixel	value which defines a vertical
.	offset for the visible area of a Window.
.
*------------------------------------------------------------------------------
.   WINPOS=dnumnvar			     CREATE	SETPROP	GETPROP
.
. Description:
.	This property specifies	the positioning	of a window when a
.	window object is activated.
.
.	The allowed window positioning types are as follows:
.
.	ABSOLUTE   -	This causes a window object to be positioned to
.			the top/left co-ordinates of the window	object.
.		     
.	DESKCENTER   -	This causes a window object to be centered on the 
.			desktop.
.		      
.	PARENTCENTER -	This centers a MODAL or	MODELESS window	object over the	
.			parent window. If the window object is not a MODAL or 
.			MODELESS window	object,	it is treated as if DESKCENTER
.			was used.
.
. Objects:
.	WINDOW
.
. dnumnvar values:
.
.$NONE	      EQU	0		// Same	as Default
$ABSOLUTE     EQU	1		// Default
$DESKCENTER   EQU	2
$PARENTCENTER EQU	3	       
.
*------------------------------------------------------------------------------
.   WINTYPE=dnumnvar			     CREATE	      GETPROP
.
. Description:
.	This property specifies	the type of window applied for the
.	window object.
.
.	The allowed window types are as	follows:
.
.	MODAL	   -  This causes a MODAL dialog window	to be created.
.		      When this	window is activated using the ACTIVATE
.		      statement, the program execution remains at the
.		      at the ACTIVATE statement	until the window is
.		      destroyed	or deactivated.	 While a MODAL window
.		      is active, no events other than events for the MODAL
.		      window are processed until the MODAL window is
.		      terminated.
.
.	MODELESS   -  This causes a MODELESS dialog window to be created.
.		      This window is a Window's	popup window which will
.		      always remain on top of any primary window.  Another
.		      feature is that this window acts as a child window
.		      to a primary window.  If the primary window gets
.		      minimized, then the MODELESS dialog window is also
.		      minimized.  However, the MODELESS	dialog window
.		      does not affect the minimized task bar selections.
.		      The MODELESS dialog window can not be minimized.
.		      The MODELESS dialog window does not lock out event
.		      processing for other windows while the MODELESS
.		      window is	active.
 
.	OBJONLY	   -  The use for the objects only window type is reserved
.		      for use by the PLB designer.  If this type is used
.		      in a program, then it is implemented as a	
.		      PRIMARYSIZE window type.
.	
.     PRIMARYFIXED -  This type	creates	a Window's Primary window which
.		      can not be sized.	 A primary window can be minimized
.		      and maximized.  A	primary	window will always be
.		      presented	under a	MODAL/MODELESS dialog window.
.		      If a primary window is active when a MODELESS window
.		      is created, then the primary window acts as the
.		      parent window for	the MODELESS window.
.
.     PRIMARYSIZE  -  The primary sized	window type is a Windows' Primary
.		      window which can be sized.  All other features are
.		      the same as PRIMARYFIXED.
.
. Objects:
.	WINDOW
.
. dnumnvar values:
.
.$NONE	      EQU	0	       // Same as Default
$MODAL	      EQU	1
$MODELESS     EQU	2
$OBJONLY      EQU	3	       // PLFORM use only
$PRIMARYFIXED EQU	4
$PRIMARYSIZE  EQU	5	       // Default
.
*------------------------------------------------------------------------------
.   WORDWRAP[=dnumnvar]			     CREATE  SETPROP  GETPROP
.
. Description:
.	For a MULTILINE	edittext object	this property causes 
.	the data entry to continue at the beginning of the
.	next line when the right most boundary of the edittext
.	object is encountered.	If the <dnumnvar> value	is not
.	specified, the WORDWRAP	property is set	to ON.
.
. Objects:
.	EDITTEXT
.
. dnumnvar values:
.
.$OFF	      EQU	0	       // Default
.$ON	      EQU	1
.
*------------------------------------------------------------------------------
.   X1=dnumnvar					     SETPROP  GETPROP
.
. Description:
.	This property is a GETPROP/SETPROP only	property.  The X1 property
.	specifies the X	coordinate for the starting point of the LINE
.	object.
.
. Objects:
.	LINE
.
. dnumnvar values:
.
.	The <dnumnvar> value is	provided in the	same units as used for
.	the creation of	an object.  This means that if an object was
.	created	using pixels, then the X1 value	expects	to use pixels.
*------------------------------------------------------------------------------
.   X2=dnumnvar					     SETPROP  GETPROP
.
. Description:
.	This property is a GETPROP/SETPROP only	property.  The X2 property
.	specifies the X	coordinate for the ending point	of the LINE
.	object.
.
. Objects:
.	LINE
.
. dnumnvar values:
.
.	The <dnumnvar> value is	provided in the	same units as used for
.	the creation of	an object.  This means that if an object was
.	created	using pixels, then the X2 value	expects	to use pixels.
*------------------------------------------------------------------------------
.   Y1=dnumnvar					     SETPROP  GETPROP
.
. Description:
.	This property is a GETPROP/SETPROP only	property.  The Y1 property
.	specifies the Y	coordinate for the starting point of the LINE
.	object.
.
. Objects:
.	LINE
.
. dnumnvar values:
.
.	The <dnumnvar> value is	provided in the	same units as used for
.	the creation of	an object.  This means that if an object was
.	created	using pixels, then the Y1 value	expects	to use pixels.
*------------------------------------------------------------------------------
.   Y2=dnumnvar					     SETPROP  GETPROP
.
. Description:
.	This property is a GETPROP/SETPROP only	property.  The Y2 property
.	specifies the Y	coordinate for the ending point	of the LINE
.	object.
.
. Objects:
.	LINE
.
. dnumnvar values:
.
.	The <dnumnvar> value is	provided in the	same units as used for
.	the creation of	an object.  This means that if an object was
.	created	using pixels, then the Y2 value	expects	to use pixels.
*------------------------------------------------------------------------------
.   ZORDER=dnumnvar			     CREATE  SETPROP  GETPROP
.
. Description:
.	This property specifies	which object should be displayed
.	on top when more than one object exists	in the same space
.	and in the same	plane.
.
.	Note1:	Any objects in 'Plane 1' will always be	on top of
.		any object in 'Plane 0'	with the exception of the
.		TABCONTROL object.  Although, the TABCONTROL object
.		exists in 'Plane 1', the 'Plane	0' objects can be
.		placed on top of the TABCONTROL	by having a higher
.		ZORDER value than the TABCONTROL.
.
.	Note2:	When the ZORDER	value for any object is	higher than
.		any other object in the	same plane, then the object 
.		with the higher	value exists on	top. 
.
.	Note3:	The ZORDER exists for a	window object only to define
.		a default zorder starting value	for objects created
.		for that window	object without an explicit ZORDER.
.
. Objects:
.	BUTTON			;Plane	1
.	CHECKBOX		;Plane	1
.	CHECKGRP		;Plane	1
.	COMBOBOX		;Plane	1
.	DATALIST		;Plane	1
.	EDITTEXT		;Plane	1
.	GROUPBOX		;Plane	0
.	HSCROLLBAR		;Plane	1
.	ICON			;Plane	0
.	LINE			;Plane	0
.	MREGION			;Plane	0
.	MOVIE			;Plane	1
.	PICT			;Plane	0
.	POPUPMENU ( OBSELETE )	;Plane	1
.	PROGRESS		;Plane	0
.	RADIOGRP		;Plane	1
.	RADIO			;Plane	1
.	SHAPE			;Plane	0
.	SLIDER			;Plane	1
.	STATTEXT		;Plane	0
.	TABCONTROL		;Plane	1
.	VSCROLLBAR		;Plane	1
.	WINDOW			;No Plane
.
. dnumnvar values:
.
.	The <dnumnvar> value is	a user defined value.
.
 
