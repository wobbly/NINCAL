
...............................................................................
.               'PROFILES.API' INCLUDE/EXCLUDE COMPILER DIRECTIVES
...............................................................................
.
.   These compiler directives will include or exclude the routine depending
.   on your setting of the 'Stat' equation. By default will all WinApi's be
.   EXCLUDED unless you set the corresponding equation to '1'.
.   If this file, the compiler directives, is not included into your program
.   will the decompiler default all directives to Zero and excluded all WinApi
.   routines. Please remember that compiler directives does not take up any
.   code or data space.
.
.   Change 'Stat' to '1' for each WinApi call you want included into your program
.
.   'Stat' :  0 = Excluded the selected WinApi routine
.             1 = Include WinApi profile and WinApi call in compiled output
.
.ApiRef  Stat  Routine entry points       Library      Description
.===== === =   ========================   ============ ================================
Api001 Equ 1   GetLastError             - kernel32.Dll Get last windows operation error
Api002 Equ 1   LoadLibrary              - kernel32.Dll Loads Dll or Driver API library
Api003 Equ 1   GetFileTime              - kernel32.Dll Get date/Time stamp of a file
Api004 Equ 0   SetFileTime              - kernel32.Dll Change the date/time stamp of a file
Api005 Equ 0   SystemTimeToFileTime     - kernel32.Dll Convert System time to file time format
Api006 Equ 1   FileTimeToSystemTime     - kernel32.Dll Convert File time to system time format
Api007 Equ 0   FileTimeToLocalFileTime  - kernel32.Dll Convert file time to local file time
Api008 Equ 0   LocalFileTimeToFileTime  - kernel32.Dll Convert local time to file time
Api009 Equ 0   CompareFileTime          - kernel32.Dll Compare two 64 bit file times
Api010 Equ 1   TwainIsAvailable         -*Eztw32.Dll   Test if twain driver is loaded
Api011 Equ 1   TwainSelectImageSource   -*Eztw32.Dll   Select twain input device ( Scanner etc.)
Api012 Equ 1   TwainAcquireToClipboard  -*Eztw32.Dll   Acquire an image to the clipboard
Api013 Equ 1   TwainAcquireToFilename   -*Eztw32.Dll   Acquire an image to a file name
Api014 Equ 1   CopyFile                 - kernel32.Dll Copy a file
Api015 Equ 0   GetDriveType             - kernel32.Dll Get the type of drive
Api016 Equ 1   GetWindowsDirectory      - kernel32.Dll Get windows directory name
Api017 Equ 1   FindFirstFile            - kernel32.Dll Find first file and file information
Api018 Equ 1   FindNextFile             - kernel32.Dll Find the next file after FindFirstFile
Api019 Equ 0   FindClose                - kernel32.Dll Close find-handle used
Api020 Equ 0   CloseHandle              - kernel32.Dll Close a handle used ( Basically the same as FindClose)
Api021 Equ 0   OpenPrinter              - winspool.Drv Open a printer ( Driver)
Api022 Equ 0   ClosePrinter             - winspool.Drv Close a printer ( Driver)
Api023 Equ 0   DocumentProperties       - winspool.Drv Read/Write to printer ( Driver)
Api024 Equ 0   mciSendString            -*winmm.Dll    Send MCI command to media device
Api025 Equ 0   mciGetErrorString        -*winmm.Dll    Get error message from MCI device
Api026 Equ 1   GetSystemDirectory       - kernel32.Dll Get the path to the windows system directory
Api027 Equ 0   LoadCursor               - kernel32.Dll Load cursor
Api028 Equ 0   SetCursor                - kernel32.Dll Set new cursor
Api029 Equ 0   LoadCursorFromFile       - user32.Dll   Load a new cursor from a .CUR file
Api030 Equ 0   SetClassLong             - user32.Dll   Changes properties of a window
Api031 Equ 0   GetClassLong             - user32.Dll   Retrieve selected window property
Api032 Equ 0   QueryServiceConfig       - advapi.Dll   Retrieves configuration parameters for a service
Api033 Equ 1   DeleteFile               - kernel32.lib Deletes specified file
.
Api999 Equ 0   Skeleton
.
...............................................................................
.               INCLUDE 'PROFILES.API' WINAPI PROFILES AND WINAPI CALLS
...............................................................................

                INCLUDE    winapi.API      .Version 1.04

.==============================================================================
.                    END OF 'PROFILES.API' COMPILER DIRECTIVES
.==============================================================================
