FILE SPECS

*******************************************
***************LRFile.dat******************
*******************************************
(Note:  These are identical to file specs previously provided,)
(with the exception of the Exchange History, which will now be)
(provided in a separate file.  I have used this space to store)
(the Broker Company Name.)
Order Code				1	Text
Order Status				1	Text		“0”=Live, “B”=Billed, “Q”=Cancelled Billed, “X”=Cancelled, “l”=LCR, “z”=Cancelled LCR, “p”=Pending, “x”=Cancelled Pending
Mailer Number				4	Numeric
LR Number (Key)				6	Numeric
Mailer Contact Number			3	Numeric		Obsolete
List Number				6	Numeric
List Owner Number			4	Numeric
Mailer Purchase Order Number		12	Text
Order Quantity				9	Numeric
*Price Per Thousand			5	Numeric
Mailer Key				12	Text
Media Code				2	Numeric
Return Date				8	Numeric
Mail Date				8	Numeric
Order Test Code				1	Numeric
Selection on Test Code			1	Numeric
Continuation Code			1	Numeric
Continuation LR Number			6	Numeric
Order Date of Continuation LR		8	Numeric
Qty of Continuation LR			9	Numeric
Spec. Instruction Codes			24	Numeric		Obsolete
Bill Direct Code			1	Numeric
Broker Guarantee Code			1	Numeric
Entire/Rent/Exchange Code		1	Numeric		“1”=Entire, “2”=Exchange, “3”=Entire & Exchange 
Offer Number				7 	Numeric		first 4 bytes are Obsolete
Offer Description			5	Text		Obsolete
Order Net Quantity			9	Numeric
Order Campaign Number			6	Numeric
Clearance Status			1	Numeric
Clearance Initials			3	Text
Broker Notification Byte		1	Numeric		Used only by List Management
Clearance Date				8	Numeric
LCR Rental Request Byte			1	Numeric
LCR History Byte			1	Text
*Exchange Price per Thousand		5	Numeric
Return To Company Number		4	Numeric
Tape Return Byte			1	Numeric		Obsolete
List/Select Universe			9	Numeric
Salesperson Code			2	Numeric
NINCA Contact Code			2	Numeric
NINCA Caller Code			2	Numeric
Order Date				8	Numeric
Sample Code				1	Numeric
Comselect Code				1	Numeric
Shipping Method Code			2	Numeric
List Name				35	Text
Select Name				35	Text
ReUse LR Number				6	Numeric
Typist Initials				3	Text
Order Exchange Quantity			9	Numeric
NINCA Guarantee Code			1	Numeric
Broker Number				4	Numeric
Broker Contact Number			3	Numeric
Sample Number				3	Numeric
Net Name Percentage			2	Numeric
Net name Running Charge			3.2	Numeric		(total of 6 bytes)
Net Flag				1	Numeric
Net Name Minimum			7	Numeric
Filler					80	Text
Mailer Name				45	Text
Owner Name				25	Text
Offer Name				45	Text
Broker Name				45	Text
Merge Percentage			10	Numeric		Merge % Percentage for this LR
Datacard Exc/Rent Limitations		1	Numeric		“1”=Rent or Exchange, “2”=Exchange Only, “3”=Rental Only
Datacard Revision Date			10	Text		Formatted as MM/DD/CCYY
Last Merge % for Mlr/List		6	Numeric		Merge % for LR with this Mlr/List with latest Order Date
LCR/Pending Order SubStatus Desc.	45	Text		Will remain blank until LCRs/Pending Orders are sent to Integral
Gross Billed Quantity			8	Numeric
Net Merge Quantity			8	Numeric
Total Billed (AR)			10.2	Numeric		(total of 13 bytes)
Base Price				5.2	Numeric
Select Price				5.2	Numeric		If applicable
Price Calculator			20	Text		"/m", "Flat", etc.
Select Number				4	Numeric		Valid Number OR "XXXX" = keyed in Select OR "----" = Order placed before new Datacard logic
Select Name				75	Text

*********************************************
***************ListFile.dat******************
*********************************************
List Number(Key)			6	Numeric
List Name				75	Text
Status					1	Text		"W"=Withdrawn, "T"=Temporarily Withdrawn
Universe				9	Numeric

*********************************************
***************BaseFile.dat******************
*********************************************
(Note:  The Base Text should be pretty solid.  However, there MAY)
(be lines that contain garbage.  We are working with standards, when)
(extracting the data, which MAY not have been followed for the Old/Inactive Lists.)
(The only bad Base Text fields that I found were all attached to Inactive Lists.)
List Number(Key)			6	Numeric
Base Number(Key)			2	Numeric
Base Code				4	Text		"BASE"=Base, "SEC."=Secondary Base
Base Text				46	Text		May Include:  Name, Universe, Price

*********************************************
***************SelFile.dat******************
*********************************************
List Number(Key)			6	Numeric
Select Number (Key)			4	Unique Numeric Identifier
Select Name				75	Text
Select Quantity				10	Numeric
Select Price				5.2	Numeric		(Total of 8 bytes)
								If the record is a Base/Secondary Base or unassociated Select,
								this field represents the total Select Price.  If the record
								does have an associated Base/Secondary Base, this field would need
								to be added to the Select Price of the associated Base/Secondary Base
								in order to achieve the correct total.  It was done this way to allow
								different print options.
Commission Flag				1	Numeric
Price Modifier				3	Numeric
Inactive Flag				1	Numeric		'1' = Inactive Select
Select Status				1	Numeric		'1' = Special (does not print), '2' = Office Use Only
Notes Flag				1	Numeric		'1' = User should view Notes field in corresponding Datacard
Exchange Flag				1	Numeric		' ' of '0' = Exchange or Rent, '1' = Exchange Only, '2' = Rental Only
Base Association			4	Text		'BASE' = Base Select - 1 per Datacard
								'SEC.' = Secondary Base Select - 0+ per Datacard
								A 4 byte Numeric field in this location establishes this record as a Select
								off of a Base/Secondary Base.  That 4 byte number would be the corresponding 
								Select Number of a Base/Secondary Base record.
								There is the possibility of no data in this location.  This would pertain to a Select
								that is independent of an associated Base/Secondary Base, but is not a Base/Secondary Base
								itself.
Index Flag				4	Numeric		Established Print/Display order of Select Records for a Datacard
Select Date				8	Numeric
Initials				3	Text
Filler					11	Text

********************************************
***************PkgFile.dat******************
********************************************
(The following fields are mutually exclusive:  )
(Master Package Indicator)
(Master Package Associaton Number)
(Note on Master Packages:  A Master Package will have an Indicator value of "1",)
(and the Association Number will be cleared.  Regular Packages may have a valid)
(Master Package Number in the Association Number field.)
Mailer Number(Key)			4	Numeric
Package Number(Key)			6	Numeric
Package Name				150	Text
Client Package ID			25	Text
Package Date				8	Numeric
Package Notes				500	Text
Master Package Indicator		1	Numeric
Master Package Association Number	6	Numeric
Filler					93	Text

*********************************************
***************PkgPFile.dat******************
*********************************************
Mailer Number(Key)			4	Numeric
Package Number(Key)			6	Numeric
Price Date(Key)				8	Numeric
Package Print Price			6.2	Numeric		(total of 9 bytes)
Package Postage Price			6.2	Numeric		(total of 9 bytes)
Package Premium Price			6.2	Numeric		(total of 9 bytes)
Package Total Price			8.2	Numeric		(total of 9 bytes)
Price Notes				200	Text
Filler					100	Text

*********************************************
***************XRefFile.dat******************
*********************************************
List Number(Key)			6	Numeric
Mailer Number(Key)			4	Numeric

*********************************************
***************ExchFile.dat******************
*********************************************
(Note on Inactivated Date:  This field will USUALLY contain valid data)
(if the Flag has a value of "I".  Otherwise do not be surprised to garbage)
(in this field!)
Mailer1 Number(Key)			4	Numeric
Mailer2 Number(Key)			4	Numeric
Inactivated Date			8	Text** See Notes
Entry Number				5	Numeric
Flag					1	Text		" "=normal, "I" = inactive
Mailer1 Usage				10	Numeric
Mailer2 Usage				10	Numeric

*********************************************
***************SamFile.dat******************
*********************************************
Mailer Number				4	Numeric
Sample Number				3	Numeric
Sample Description			30	Text
Sample Description 2			30	Text
Sample Date				8	Numeric
Record Date				8	Numeric
User Name				9	Text
Inactive Code				1	Numeric		"1" = Sample is Inactive

*********************************************
***************OffFile.dat******************
*********************************************
Offer code				1	Text		(Obsolete)
Mailer Number				4	Numeric
Offer Number				3	Numeric
Offer Description			40	Text
User Name				10	Text
Record Date				8	Numeric

*********************************************
***************ConFile.dat******************
*********************************************
Contact Number				2	Numeric
Contact Name				35	Text
Contact Phone Number			25	Text
Contact Port Number			3	Numeric
Team Number				2	Numeric		(Not implemented)
Rights					1	Numeric		(Not implemented)
Rights 2				40	Numeric		(Not implemented)
Default Printer				1	Numeric
Caller/Planner				1	Numeric

*********************************************
***************MedFile.dat******************
*********************************************
Media Number				2	Numeric
Media Name				50	Text

*********************************************
***************ShipFile.dat******************
*********************************************
Shipping Number				2	Numeric
Shipping Description			35	Text

*********************************************
***************TxtFile.dat******************
*********************************************
(Unique identifier starts at '1', allowing up to nine 500 byte records of Free TExt per Datacard.)
Datacard Number				6	Numeric
Text Number				1	Numeric
Text					500	Text

*********************************************
***************PrcFile.dat******************
*********************************************
(Additional Prices associated with an Order)
LR Number (Key)				6	Numeric
Additional Price Amount			5.2	Numeric
Additional Price Text			46	Text
Additional Price Calculator		20	Text