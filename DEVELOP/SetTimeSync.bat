@ECHO OFF
 CLS
 rem ==========================================================================
 rem === Set Time Synchronization for Local NT/AD domains
 rem ==========================================================================
 rem
 rem Created On: 29 Jan 2008
 rem         By: Andrew S. Baker
 rem
 rem Updated On: 24 Dec 2011  (Prior Update: 23 Nov 2011)
 rem         By: Andrew S. Baker
 rem


 rem ==========================================================================
 rem  OS Required: Windows 2003 or later
 rem
 rem  Non-Native Utilities Required:
 rem    Resource Kit ........... http://KB.UltraTech-llc.com/?File=ResKit.TXT
 rem
 rem  Input Files Used By Script (stored in %SystemDrive%\Scripts\Bat\Input):
 rem    CustomVariables.TXT .... Set File/Directory Inclusions and Exclusions
 rem
 rem  Other Scripts Called:
 rem    SetDrive.BAT ........... Primary Script for Setting Common Variables
 rem ==========================================================================

 :::  This script uses the NET TIME and the Win32tm commands to view and
 :::  configure the NTP settings of the machines in your domain.  By default,
 :::  the servers listed as Domain Controllers are configured to the external
 :::  time source, and all other systems are configured to get their time
 :::  from a domain controller.
 :::
 :::  Prior to version 4.0, the list of systems to enable for external NTP
 :::  synchronization was derived from the "AD-DCs.TXT" input file.  This has
 :::  been changed to the "CustomVariables.TXT" input file via the @TIMESERVER
 :::  variable.
 :::
 :::  The settings enabled by this script were derived from the following
 :::  Microsoft Knowledgebase articles:
 :::  ---- http://support.microsoft.com/kb/816042
 :::  ---- http://technet.microsoft.com/en-us/library/cc773013(WS.10).aspx
 :::
 :::
 :::  For More Scripting Assistance, see the following:
 :::  http://KB.UltraTech-llc.com/?File=Scripting.TXT


 rem ==========================================================================
 rem === Enable Debug Mode If 'Debug' Variable is Set to "TRUE"
 rem === Last Modified On: 03 Aug 2010  (Prior Update: n/a)
 rem ==========================================================================
:DebugMode
 IF /I "%Debug%"=="TRUE" (
           SET @PAUSE_IF_DEBUG=TIMEOUT 5
           SET @END_DEBUG_MODE=ECHO.^& ECHO *** DEBUG MODE FINISHED ***^& ECHO.^& SET @^& TIMEOUT 30
           ECHO *** DEBUG MODE = ON ***
           ECHO ON
 ) ELSE (
           SET @PAUSE_IF_DEBUG=
           SET @END_DEBUG_MODE=
 )


 rem ==========================================================================
 rem === Initialize Environment Variables
 rem === Last Modified On: 06 May 2011  (Prior Update: 28 Mar 2011)
 rem ==========================================================================
:Variables
 SETLOCAL ENABLEDELAYEDEXPANSION & CALL :ShowStatus "STARTED" v4.7.0

 rem --------------------------------------------------------------------------
 rem -- Verify If Syntax Help Is Required/Requested
 rem --------------------------------------------------------------------------
 FOR %%H IN (H HELP) DO FOR %%P IN (%*) DO IF /I "%%~P"=="/%%~H" GOTO :HelpMessage
 FOR /F %%H IN ('ECHO . %* ^| FIND /I "/?"') DO IF ERRORLEVEL 0 GOTO :HelpMessage
 rem --------------------------------------------------------------------------

 rem -- Call Centralized Script for Setting Global Log and Date/Time Variables
 SET @STORAGE=%SystemDrive%\STORAGE& IF EXIST "%~dp0SetDrive.BAT" (
           CALL "%~dp0SetDrive.BAT"
 ) ELSE (
           ECHO *** WARNING: CRITICAL SCRIPT COMPONENT MISSING ***
           ECHO.
           ECHO Unable to initialize custom script variables. Could not find %~dp0SetDrive.BAT
           ECHO.
           DIR "%~dp0SetDrive.BAT"
           ECHO.
           GOTO :EOF
 )

 rem -- Main Variables
 SET @USEW32TM=TRUE& IF %@OSBUILD% LEQ 3790 SET @USEW32TM=FALSE
 SET @SYSTEMLIST=
 SET @OLD_TIMESOURCE=
 SET @NEW_TIMESOURCE=
 SET @NTP_CLIENT_KEY=HKEY_LOCAL_MACHINE\SYSTEM\CurrentControlSet\Services\W32Time\TimeProviders\NtpClient
 SET @NTP_PARAM_KEY=HKEY_LOCAL_MACHINE\SYSTEM\CurrentControlSet\Services\W32Time\Parameters
 SET @NTP_CONFIG_KEY=HKEY_LOCAL_MACHINE\SYSTEM\CurrentControlSet\Services\W32Time\Config
 SET @SPECIAL_POLL=1800
 SET @NORMAL_POLL=15
 SET @POS_CORRECT=3700
 SET @NEG_CORRECT=3700
 SET @MAX_OFFSET=5

 rem -- Determine Input Source For Script
 SET @SOURCE='TYPE "%~1" '& IF "%~1"=="." SET @SOURCE=%@SERVERLIST%
 IF "%~1"=="" SET @SOURCE='NET VIEW ^^^| FIND "\\" '

 rem -- Custom Variables -- Grab from CustomVariables.TXT -OR- Set Manually
 IF EXIST "%@CUSTOM-VARS%" (
           FOR /F "USEBACKQ TOKENS=1-2* DELIMS=,; " %%M IN ("%@CUSTOM-VARS%") DO FOR %%Z IN ("&" "%~n0") DO IF /I "%%~M"=="%%~Z" CALL :RunCMD SET %%N=%%O
 ) ELSE (
           SET @TIMESOURCE=nist1-ny.ustiming.org  nist1-dc.ustiming.org
           SET @TIMESERVER=SERVER1  SERVER2  SERVER3
 )

 rem -- Ensure Correct Time Source Format for NET TIME vs W32TM Commands
 FOR %%T IN (%@TIMESOURCE%) DO (
           SET @OLD_TIMESOURCE=!@OLD_TIMESOURCE!,%%~T
           SET @NEW_TIMESOURCE=!@NEW_TIMESOURCE! %%~T,0x1
           SET @BAK_TIMESOURCE=!@BAK_TIMESOURCE! %%~T,0x2
 )

 rem -- Generate System List for Time Sync, Including Current Machine and Designated Time Servers
 FOR %%S IN (%@TIMESERVER%) DO SET @SYSTEMLIST=!@SYSTEMLIST! %%~S
 FOR /F "TOKENS=1 EOL=;" %%S IN (%@SOURCE%) DO SET @SYSTEMLIST=!@SYSTEMLIST! %%~S
 SET @TIMESERVER=%@TIMESERVER:\=%
 SET @SYSTEMLIST=%@SYSTEMLIST:\=%

 rem -- Standard Variables
 SET @PARAMS=%1 %2 %3 %4 %5 %6 %7 %8 %9
 SET @LOGPATH=%@STORAGE%\Logs\Maint
 SET @SAVELOG=%@LOGPATH%\%~n0.%@FILESTAMP%
 SET @LOGFILE=%@LOGPATH%\%~n0.TXT


 rem ==========================================================================
 rem === Update Time Server Settings for Windows Systems (Determine Method)
 rem === Last Modified On: 30 Mar 2011  (Prior Update: 23 Mar 2011)
 rem ==========================================================================
:SetTimeSync
 ECHO Configuring Time Servers...
 ECHO.

 rem -- Cycle through list of systems.
 FOR %%S IN (%@SYSTEMLIST%) DO (
           IF NOT EXIST "\\%%~S\ADMIN$" SET @%%~S-SET=TRUE

           IF /I NOT "!@%%~S-SET!"=="TRUE" (
                     ECHO Evaluating %%~S Settings... >CON
                     ECHO Evaluating %%~S Settings...
                     ECHO %@MINORDIV%
                     SET @EXTERNALSYNC=FALSE

                     rem -- Make Registry Configuration Changes to the W32Time Service on the Remote System
                     REG ADD "\\%%~S\%@NTP_CLIENT_KEY%" /v "SpecialPollInterval"   /t REG_DWORD /d %@SPECIAL_POLL% /f
                     REG ADD "\\%%~S\%@NTP_CONFIG_KEY%" /v "MaxPollInterval"       /t REG_DWORD /d %@NORMAL_POLL%  /f
                     REG ADD "\\%%~S\%@NTP_CONFIG_KEY%" /v "MaxPosPhaseCorrection" /t REG_DWORD /d %@POS_CORRECT%  /f
                     REG ADD "\\%%~S\%@NTP_CONFIG_KEY%" /v "MaxNegPhaseCorrection" /t REG_DWORD /d %@NEG_CORRECT%  /f
                     REG ADD "\\%%~S\%@NTP_CONFIG_KEY%" /v "MaxAllowedPhaseOffset" /t REG_DWORD /d %@MAX_OFFSET%   /f

                     rem -- Identify Local Time Servers to Set Against External NTP Servers
                     FOR %%T IN (%@TIMESERVER%) DO IF /I "%%~S"=="%%~T" SET @EXTERNALSYNC=TRUE

                     IF /I "!@EXTERNALSYNC!"=="TRUE" (
                               rem ----------------------------------------
                               rem -- Configure Authoritative NTP Sources
                               rem ----------------------------------------
                               ECHO Configuring %%~S as an Authoritative NTP Source >CON
                               ECHO Configuring %%~S as an Authoritative NTP Source
                               REG ADD "\\%%~S\%@NTP_CONFIG_KEY%" /v "AnnounceFlags" /t REG_DWORD /d 5 /f >NUL
                               IF /I "%@USEW32TM%"=="TRUE" (
                                         W32TM /CONFIG /COMPUTER:%%~S "/ManualPeerList:!@NEW_TIMESOURCE:~1,999!" /SyncFromFlags:MANUAL /RELIABLE:YES /UPDATE
                               ) ELSE (
                                         NET TIME \\%%~S /SETSNTP:"!@OLD_TIMESOURCE:~1,999!,0x1"
                               )
                     ) ELSE (
                               rem ----------------------------------------
                               rem -- Configure NTP Client Systems
                               rem ----------------------------------------
                               ECHO Configuring %%~S as an NTP Client System >CON
                               ECHO Configuring %%~S as an NTP Client System
                               REG ADD "\\%%~S\%@NTP_CONFIG_KEY%" /v "AnnounceFlags" /t REG_DWORD /d 10 /f >NUL
                               REG ADD "\\%%~S\%@NTP_PARAM_KEY%" /v "NtpServer" /t REG_SZ /d "!@BAK_TIMESOURCE:~1,999!" /f >NUL
                               IF /I "%@USEW32TM%"=="TRUE" (
                                         W32TM /CONFIG /COMPUTER:%%~S "/ManualPeerList:!@BAK_TIMESOURCE:~1,999!" /SyncFromFlags:DOMHIER /RELIABLE:NO /UPDATE
                               ) ELSE (
                                         NET TIME \\%%~S /SETSNTP:""
                               )
                     )

                     rem -- Set Variable Based on Computer Name for Each Successful Machine Configuration
                     SET @%%~S-SET=TRUE
                     CALL :WaitForRestart \\%%~S "W32Time"

                     rem -- Display Current NTP Setting
                     IF /I "%@USEW32TM%"=="TRUE" (
                               W32TM /RESYNC /COMPUTER:%%~S /REDISCOVER
                               W32TM /QUERY /COMPUTER:%%~S /PEERS
                               ECHO.
                               W32TM /TZ
                     ) ELSE (
                               NET TIME \\%%S /QUERYSNTP
                     )
                     ECHO ---------------------------------------------------------------------------
                     ECHO.
                     ECHO.
                     ECHO. >CON
           )
 ) >>"%@SAVELOG%"


 rem ==========================================================================
 rem === Reset Environment Variables and Exit Batch File
 rem === Last Modified On: 30 Mar 2011  (Prior Update: 23 Mar 2011)
 rem ==========================================================================
:ExitBatch
 COPY "%@SAVELOG%" "%@LOGFILE%" /V
 DIR "%@LOGFILE%" & TYPE "%@LOGFILE%"
 ECHO FINISHED Configuring Time Servers!
 CALL :ShowStatus "FINISHED"
 ENDLOCAL
 GOTO :EOF


 rem ==========================================================================
 rem === SUBROUTINE: Execute Dynamic Command From Variable
 rem === Last Modified On: 03 Aug 2010  (Prior Update: n/a)
 rem ==========================================================================
:RunCMD
 rem %1 = Command To Execute

 %*
 GOTO :EOF


 rem ==========================================================================
 rem === SUBROUTINE: Wait For Service Restart To Fully Process
 rem === Last Modified On: 24 Dec 2011  (Prior Update: 22 Dec 2010)
 rem ==========================================================================
:WaitForRestart
 rem %1 = Name of System
 rem %2 = Name of Service
 SET /A @LOOP=0
 SET /A @LIMIT=1000
 ECHO.
 ECHO Restarting "%~2" Service on %~1
 SC %~1 STOP "%~2"  | FIND "STATE"

:QueryStop_Loop
 FOR /F "TOKENS=4" %%Q IN ('SC %~1 QUERY "%~2" ^| FIND "STATE"') DO SET @STATE=%%~Q
 IF /I "%DEBUG%"=="TRUE" ECHO  -@LOOP=!@LOOP!
 SET /A @LOOP+=1& IF !@LOOP! GTR !@LIMIT! GOTO :QueryStart
 IF /I NOT "!@STATE!"=="STOPPED" GOTO :QueryStop_Loop

 rem -- Start the W32Time Service
:QueryStart
 SET /A @LOOP=0
 SC %~1 START "%~2" | FIND "STATE"

:QueryStart_Loop
 FOR /F "TOKENS=4" %%Q IN ('SC %~1 QUERY "%~2" ^| FIND "STATE"') DO SET @STATE=%%~Q
 IF /I "%DEBUG%"=="TRUE" ECHO  -@LOOP=!@LOOP!
 SET /A @LOOP+=1& IF !@LOOP! GTR !@LIMIT! GOTO :EOF
 IF /I NOT "!@STATE!"=="RUNNING" GOTO :QueryStart_Loop
 GOTO :EOF


 rem ==========================================================================
 rem === SUBROUTINE: Set/Display Script Version and Execution Status
 rem === Last Modified On: 23 Nov 2011  (Prior Update: 03 Apr 2011)
 rem ==========================================================================
:ShowStatus
 rem %1 = Run Status of Script
 rem %2 = Current Application Version

 SET @SCRIPTSTATUS=%~1& IF "%~1"=="" SET @SCRIPTSTATUS=RUNNING
 IF NOT "%~2"=="" SET @VER=%~nx0 %~2
 IF /I "%~1"=="FINISHED" IF DEFINED @END_DEBUG_MODE %@END_DEBUG_MODE:"=%
 ECHO.
 ECHO *** %@SCRIPTSTATUS%: %@VER% [%DATE% at %TIME%] ***
 ECHO.
 GOTO :EOF


 rem ==========================================================================
 rem === SUBROUTINE: Display Syntax Help if Required/Requested
 rem === Last Modified On: 04 May 2011  (Prior Update: 28 Mar 2011)
 rem ==========================================================================
:HelpMessage
 ECHO Configures the NTP settings for a set of machines in a domain or workgroup
 ECHO.
 ECHO.
 ECHO   YOU TYPED: %0 %*
 ECHO.
 ECHO  CMD SYNTAX: %~n0  [. ^| list_of_machines]
 ECHO.
 ECHO     . = Use default server list
 ECHO.
 ECHO  EXAMPLE(S): %~n0
 ECHO              %~n0 .
 ECHO              %~n0 C:\Temp\Servers.TXT
 ECHO              %~n0 %~dp0Input\ServerList.TXT
 ECHO              %~n0 "D:\Lists\My Server List.TXT"
 ECHO              %~n0 "%TEMP%\Lists\My Workgroup.TXT"
 ECHO.
 ECHO ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 ECHO NOTE: Any parameters which contain spaces should be surrounded by quotes.
 ECHO       Shortnames *can* be surrounded by quotes if you choose, but it is
 ECHO       not mandatory by any means.
 ECHO ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 ECHO.
 CALL :ShowStatus "FINISHED"