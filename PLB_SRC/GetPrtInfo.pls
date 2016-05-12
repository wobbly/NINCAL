.
. INFOPFIL - Get Information about the PFILE Object
.
. Place include at the top of the program so fields can be used.
.
. CALL     INFOPFIL USING {pfile1}
.
. Note the following:
.
.   1. {pfile1} is the pfile being inquired.
.
. Instruction Examples:
.
.   rpfDfltTxt PFILE
.
.            CALL      INFOPFIL USING rpfDfltTxt
.
. For more examples see ????
.
. 08/02/00 GW Wrote include from INFOFONT
.
         %IFNDEF   curPrinterDrv
         GOTO      #INCEND
#lleIP          LIST
curIPPrinterDrv          FORM  8   .Current Printer Driver version
curIPPageWidthPix        FORM  8   .Page width in pixels
curIPPageHeightPix       FORM  8   .Page height in pixels
curIPPageWidthMM         FORM  8   .Page width in millimeters
curIPPageHeightMM        FORM  8   .Page height in millimeters
curIPColorBitPerPix      FORM  8   .Number of color bits per pixel
curIPDeviceFont          FORM  8   .Number of device specific fonts
strIPPrintPict           DIM   1   .Can Print Pictures ? (Y or N)
curIPCopiesSpecified     FORM  4   .User specified copies specified by user (future)
strIPPrintAllPages       DIM   1   .Print All Pages (Y or N)
strIPPrintSelectionOnly  DIM   1   .Print Selection Only (Y or N)
curIPPrintStartPage      FORM  8   .Print dialog start page value
.
curIPPrintEndPage        FORM  8   .Print dialog end page value
strIPPrintAlignRight     DIM   1   .Print Alignment Right (Y or N)
strIPPrintAlignDecimal   DIM   1   .Print Alignment Decimal (Y or )
strIPFillOnEnabled       DIM   1   .Fill On Enabled (Y or N)
strIPPictRectEnabled     DIM   1   .Pict Rectangle Enabled (Y or N)
strIPOverlayEnabled      DIM   1   .Overlay Enabled (Y or N)
curIPPrinterNameLP       FORM  4   .Number of characters in Printer Name
strIPPrinterName         DIM   31  .Printer name
curIPPrintOrientation    FORM  10  .Print orientation as follows
.                                  1 = Portrait
.                                  2 = Landscape
.
curIPPaperSize           FORM  10  .Paper size as follows:
.                                  1 = Letter, 8 1/2- by 11-inch
.                                  2 = Letter Small, 8 1/2- by 11-inch
.                                  3 = Tabloid, 11- by 17-inch
.                                  4 = Ledger, 17- by 11-inch
.                                  5 = Legal, 8 1/2- by 14-inch
.                                  6 = Statement, 5 1/2- by 8 1/2-inch
.                                  7 = Executive, 7 1/4- by 10 1/2-inch
.                                  8 = A3 sheet, 297- by 420-mm
.                                  9 = A4 Sheet, 210- by 297-mm
.                                  10 = A4 small sheet, 210- by 297-mm
.                                  11 = A5 sheet, 148- by 210-mm
.                                  12 = B4 sheet, 250- by 354-mm
.                                  13 = B5 sheet, 182- by 257-mm
.                                  14 = Folio, 8 1/2- by 13-inch
.
.                                  15 = Quarto, 215- by 275-mm
.                                  16 = 10- by 14-inch
.                                  17 = 11- by 17-inch
.                                  18 = Note, 8 1/2- by 11-inch
.                                  19 = #9 Envelope, 3 7/8- by 8 7/8-inch
.                                  20 = #10 Envelope, 4 1/8- by 9 1/2-inch
.                                  21 = #11 Envelope, 4 1/2- by 10 3/8-inch
.                                  22 = #12 Envelope, 4 3/4- by 11-inch
.                                  23 = #14 Envelope, 5- by 11 1/2-inch
.                                  24 = C Sheet, 17- by 22-inch
.                                  25 = D Sheet, 22- by 34-inch
.                                  26 = E Sheet, 34- by 44-inch
.                                  27 = DL Envelope, 110- by 220-mm
.                                  28 = C5 Envelope, 162- by 229-mm
.
.                                  29 = C3 Envelope, 324- by 458-mm
.                                  30 = C4 Envelope, 229- by 324-mm
.                                  31 = C6 Envelope, 114- by 162-mm
.                                  32 = C65 Envelope, 114- by 229-mm
.                                  33 = B4 Envelope, 250- by 353-mm
.                                  34 = B5 Envelope, 176- by 250-mm
.                                  35 = B6 Envelope, 176- by 125-mm
.                                  36 = Italy Envelope, 110- by 230-mm
.                                  37 = Monarch Envelope, 3 7/8- by 7 1/2-inch
.                                  38 = 6 3/4 Envelope, 3 5/8- by 6 1/2-inch
.                                  39 = US Standard Fanfold, 14 7/8- by 11-inch
.                                  40 = German Standard Fanfold, 8 1/2- by 12-inch
.
.                                  41 = German Legal Fanfold, 8 1/2 by 13-inch
.                                  256= Custom user defined
.
curIPCustomPaperLengthMM FORM  10  .Custom paper length in 10ths of mm
curIPCustomPaperWidthMM  FORM  10  .Custom paper width in 10ths of mm
curIPOutputScalingFactor FORM  10  .Printed output scaling factor
curIPMultiPageCopies     FORM  10  .Number of copies printed if multi-page copies supported
curIPPaperSource         FORM  10  .Paper feed source as follows:
.                                  1 = Upper tray or only one
.                                  2 = Lower tray
.                                  3 = Middle tray
.                                  4 = Manual
.                                  5 = Envelope
.                                  6 = Envelope Manual
.                                  7 = Auto
.                                  8 = Tractor feed
.                                  9 = Small FMT
.                                  10 = Large FMT
.                                  11 = Large Capacity
.                                  14 = Cassette
.                                  15 = Form source
curIPPaperQuality        FORM  10  .Print quality as follows:
.                                  -1 = Draft
.                                  -2 = Low
.                                  -3 = Medium
.                                  -4 = High
.                                  nnn = Dots per inch when positive
curIPColorUsage          FORM  10  .Color usage as follows:
.                                  1 = Monochrome
.                                  2 = Color
curIPDoubleSideType      FORM  10  .Type of double sided print as follows:
.                                  1 = Simplex
.                                  2 = Vertical
.                                  3 = Horizontal
curIPCollated            FORM  10  .Copies being collated as follows
.                                  0 = No
.                                  1 = Yes
curIPVertPos             FORM  8   .Current vertical print position
curIPHorzPos             FORM  8   .Current horizontal print position
         LISTEND
.
.
#rpfIP PFILE ^
vstrIPStringChar DIM 250
INFOPFIL LROUTINE #rpfIP
.
         GETINFO   TYPE=#rpfIP,vstrIPStringChar
         UNPACK    vstrIPStringChar INTO #lleIP
.
         RETURN
#INCEND
.
         %ENDIF
.
.

