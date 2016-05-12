.THIS IS A DEVELOPMENT FILE. NOT YET READY FOR PRIME TIME.

.Variant Types
VT_EMPTY  EQU       0         .nothing
VT_NULL             EQU       1         .SQL style null
VT_I2               EQU       2         .2 byte signed integer
VT_I4               EQU       3         .4 byte signed integer
VT_R4               EQU       4         .4 byte real - Float
VT_R8               EQU       5         .8 byte real - Double
VT_CY               EQU       6         .Currency
VT_DATE             EQU       7         .Date
VT_BSTR             EQU       8         .OLE automation string
VT_DISPATCH         EQU       9         .Idispatch FAR *
VT_ERROR  EQU       10        .SCODE
VT_BOOL             EQU       11        .Boolean - True = -1, False = 0
VT_VARIANT          EQU       12        .VARIANT FAR *
VT_UNKNOWN          EQU       13        .Unknown type
VT_UI1              EQU       17        .Unsigned character

books   automation
book    automation
sheets  automation
sheet   automation
ex      automation      class="Excel.Application"
.Variant objects used to talk to outside applications
xlRowHeight   variant
Zoom80  variant
.Formatting vars 

.Generic Variant objects useful in Excel
.Booleans
.PL/B does not have a Boolean datatype, so we have to create our own.
OTRUE   variant
OFALSE  variant

.Formatting vars needed
.This constant was found in the Object Browser in Excel under the Help topic for the
.HorizontalAlignment property of the Range object.
AlignCenter         integer 4,"0xffffeff4"
AlignLeft           integer 4,"0xffffefdd"
AlignRight          integer 4,"0xffffefc8"
AllMargin           variant
MedThick            integer 4,"0xFFFFEFD6"
SheetsDefault       integer 4,"0x00000000"
xlAlignCenter       integer 4,"0xffffeff4"
xlColWidth          variant
xlLandscape         integer 4,"0x2"                     .2
xlMaximized         integer 4,"0xFFFFEFD7"
xlMinimized         integer 4,"0xFFFFEFD4"
xlPaperLegal        integer 4,"0x5"
xlUnderlineStyleSingle integer 4,"0x2"
SReturn       init            0x0a                                                        .soft return/line feed
LOText        dim             100
range         dim             20
range2        dim             20
.............................................................................................................
.Sample
.        Book.PrintOut using *From=1, *To=1, *Copies=1, *Preview=0:
.                            *PrintToFile=0, *Collate=0
.............................................................................................................

          //Boolean values, in reality, are expressed as follows:
          //False = '0', True = anything other than '0', usually '-1'
          create    OTRUE,VarType=VT_BOOL,VarValue=1
          create    OFALSE,VarType=VT_BOOL,VarValue=0
