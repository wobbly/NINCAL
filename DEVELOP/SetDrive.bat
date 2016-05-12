 rem ==========================================================================
 rem === Set Variables for Logging, Input Files, Date/Time and OS Version Info
 rem ==========================================================================
 rem
 rem Created On: 22 Jun 2003
 rem         By: Andrew S. Baker
 rem
 rem Updated On: 19 Sep 2012  (Prior Update: 10 May 2012)
 rem         By: Andrew S. Baker
 rem


 rem ==========================================================================
 rem  OS Required: Windows NT4 and later
 rem
 rem  Non-Native Utilities Required: (for the DATE calculation)
 rem    DOFF ................... http://www.jfitz.com/dos/index.html#DOFF
 rem    Resource Kit ........... http://KB.UltraTech-llc.com/?File=ResKit.TXT
 rem
 rem  Input Files Used By Script (stored in %SystemDrive%\Scripts\Bat\Input):
 rem    FolderDefaults.TXT ..... Determine Location for Scripts and Logs
 rem ==========================================================================

 :::  The purpose of this relatively simple batch file is to provide key
 :::  variables to allow your scripts to run on a variety of machines with
 :::  different drive configurations, without having to modify each script
 :::  whenever a simple change is needed to logging locations or input files.
 :::
 :::  This script, along with its companion script (@Variables.BAT) provides a
 :::  means of abstraction so that custom configurations for a particular
 :::  environment can be stored separately from the scripts themselves.
 :::
 :::  This way a single script which has to process or create log files does
 :::  not have to be changed when dealing with machines that have the log
 :::  directory on D: instead of E: or C:
 :::
 :::  This script has been modified to calculate the DATE using the ResKit
 :::  utility (NOW.EXE).  This is to ensure consistent date calculations across
 :::  a variety of Regional Settings other than just the US-English.
 :::
 :::  As of early 2009, the DOFF.EXE utility has been implemented for date
 :::  functionality, and this makes it much easier to determine the previous
 :::  day and month than in times past.  This is now the basis for all of the
 :::  DATE/TIME scripts.
 :::
 :::  (Nov 2010) Changed the default log extension of the reports from .LOG to
 :::  .TXT so that the reports that are mailed to mobile devices can be read
 :::  automatically. (Not too many devices know what to do with the .LOG files)
 :::
 :::  (Dec 2010) Updated Date Routines to handle dates of 08 and 09 without
 :::  miscalculating them as invalid octal numbers by stripping the leading 0.
 :::
 :::  (Nov 2011) Added an option to have local backups use a different drive
 :::  than the normal Logging drive (mapped to @STORAGE). This variable will
 :::  be stored in the @BACKUPS variable.
 :::
 :::  (Mar 2012) Added two calculations for obtaining the current day of the
 :::  year, the faster method of which relies on a new executable named
 :::  DateInfo.EXE which should be stored in the \Scripts\Utils folder. Also
 :::  added a variable (@THREADS) for supporting multithreaded ROBOCOPY jobs
 :::  under Windows 7/2008-R2.
 :::
 :::
 :::  For More Scripting Assistance, see the following:
 :::  http://KB.UltraTech-llc.com/?File=Scripting.TXT


 rem ==========================================================================
 rem === Enable Debug Mode If 'Debug' Variable is Set to "TRUE"
 rem === Last Modified On: 25 Oct 2005
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
 rem === Set Customized Variables for Input Files, Logs and Storage
 rem === Last Modified On: 10 May 2012  (Prior Update: 23 Apr 2012)
 rem ==========================================================================
:Variables
 SET @VAR_VER=%~nx0 v4.5.3
 SET @ALPHABET=A B C D E F G H I J K L M N O P Q R S T U V W X Y Z
 SET @WHICHDRV=%SystemDrive%
 SET @SCRIPTS=%SystemDrive%\Scripts
 SET @UTILS=%@SCRIPTS%\UTILS& IF EXIST "%SystemRoot%\SysWOW64" SET @UTILS=%@SCRIPTS%\UTILS\x64;%@SCRIPTS%\UTILS


 rem -- Pre-pend our Local Scripts Archive to the current PATH
 rem -- Last Modified On: 06 Mar 2011  (Prior Update: 11 Feb 2011)
 IF NOT DEFINED @PATH SET @PATH=%PATH%
 SET PATH=%@SCRIPTS%\BAT;%@SCRIPTS%\PS;%@UTILS%;%PATH%


 rem -- Variables for Scripts, Input Files, and Text Comparisons
 rem -- Last Modified On: 09 Nov 2010  (Prior Update: 20 Jul 2010)
 SET @NODIFF=FC: no differences encountered
 SET @TEXTFILES=BAT CMD KIX VBS PL PS PS1 TXT INI DAT INPUT
 SET @EXTENSIONS=
 FOR %%E IN (%@TEXTFILES%) DO SET @EXTENSIONS=!@EXTENSIONS! *.%%~E


 rem -- Variables for Dividing Sections of Output
 rem -- Last Modified On: 13 Oct 2011  (Prior Update: 14 Nov 2010)
 SET @MINORDIV=-------------------------------------------------------------------------------
 SET @MAJORDIV=*******************************************************************************
 SET @OTHERDIV=###############################################################################


 rem -- Server Input Lists (Default and Customized)
 rem -- Last Modified On: 28 Sep 2011  (Prior Update: 09 Mar 2011)
 SET @INPUT=%~dp0Input\

 SET @SOURCELIST=%@INPUT%ScriptSource.%COMPUTERNAME%.TXT
 SET @SOURCEINPUT=%@INPUT%ScriptInput.%COMPUTERNAME%.TXT
 SET @STORAGELIST=%@INPUT%FolderDefaults.%COMPUTERNAME%.TXT
 SET @CUSTOMINPUT=%@INPUT%CopyCustomInput.DAT
 SET @DONT_COPY=%@INPUT%DontCopyFlag.DAT

 SET @DC-LIST=%@INPUT%AD-DCs.%COMPUTERNAME%.TXT
 SET @SERVERLIST=%@INPUT%ServerList.%COMPUTERNAME%.TXT
 SET @TESTSRVLIST=%@INPUT%SmallList.%COMPUTERNAME%.TXT
 SET @SECURITYLIST=%@INPUT%SecurityList.%COMPUTERNAME%.TXT
 SET @EXCHANGELIST=%@INPUT%ExchangeList.%COMPUTERNAME%.TXT
 SET @EXCHANGETEST=%@INPUT%ExchangeTest.%COMPUTERNAME%.TXT
 SET @LOWDISKLIST=%@INPUT%LowDisk.%COMPUTERNAME%.TXT
 SET @UPTIMELIST=%@INPUT%UptimeList.%COMPUTERNAME%.TXT

 SET @CUSTOM-VARS=%@INPUT%CustomVariables.%COMPUTERNAME%.TXT
 SET @ARCHIVE-INFO=%@INPUT%ArchiveInfo.%COMPUTERNAME%.TXT
 SET @AUDIT-INFO=%@INPUT%AuditSettings.%COMPUTERNAME%.TXT
 SET @REPL-INFO=%@INPUT%ReplicationInfo.%COMPUTERNAME%.TXT
 SET @MRTG-INFO=%@INPUT%MRTG-Info.%COMPUTERNAME%.TXT
 SET @ZIP-INFO=%@INPUT%ArchiveInfo.%COMPUTERNAME%.TXT

 SET @EXEMPTION=%@INPUT%CopyExemption.%COMPUTERNAME%.TXT
 SET @PROCESS-INFO=%@INPUT%LongProcesses.%COMPUTERNAME%.TXT
 SET @BAD_PROCESS=%@INPUT%BadProcesses.%COMPUTERNAME%.TXT
 SET @BAD_SERVICE=%@INPUT%BadServices.%COMPUTERNAME%.TXT

 SET @BKUPSET=%@INPUT%SystemState-Plus.%COMPUTERNAME%.BKS
 SET @NOBACKUP=%@INPUT%NoSystemState.%COMPUTERNAME%.DAT
 SET @NOEXCHANGE=%@INPUT%NoExchangeData.%COMPUTERNAME%.DAT

 rem -- If Customized Lists Do NOT Exist, Then Use the Default Lists...
 IF NOT EXIST "%@SOURCELIST%"   SET @SOURCELIST=%@INPUT%ScriptSource.TXT
 IF NOT EXIST "%@SOURCEINPUT%"  SET @SOURCEINPUT=%@INPUT%ScriptInput.TXT
 IF NOT EXIST "%@STORAGELIST%"  SET @STORAGELIST=%@INPUT%FolderDefaults.TXT
 IF NOT EXIST "%@CUSTOMINPUT%"  SET @CUSTOMINPUT=%@INPUT%CopyCustomInput.DAT
 IF NOT EXIST "%@DONT_COPY%"    SET @DONT_COPY=%@INPUT%DontCopyFlag.DAT

 IF NOT EXIST "%@DC-LIST%"      SET @DC-LIST=%@INPUT%AD-DCs.TXT
 IF NOT EXIST "%@SERVERLIST%"   SET @SERVERLIST=%@INPUT%ServerList.TXT
 IF NOT EXIST "%@TESTSRVLIST%"  SET @TESTSRVLIST=%@INPUT%SmallList.TXT
 IF NOT EXIST "%@SECURITYLIST%" SET @SECURITYLIST=%@INPUT%SecurityList.TXT
 IF NOT EXIST "%@EXCHANGELIST%" SET @EXCHANGELIST=%@INPUT%ExchangeList.TXT
 IF NOT EXIST "%@EXCHANGETEST%" SET @EXCHANGETEST=%@INPUT%ExchangeTest.TXT
 IF NOT EXIST "%@LOWDISKLIST%"  SET @LOWDISKLIST=%@INPUT%LowDisk.TXT
 IF NOT EXIST "%@UPTIMELIST%"   SET @UPTIMELIST=%@INPUT%UptimeList.TXT

 IF NOT EXIST "%@CUSTOM-VARS%"  SET @CUSTOM-VARS=%@INPUT%CustomVariables.TXT
 IF NOT EXIST "%@ARCHIVE-INFO%" SET @ARCHIVE-INFO=%@INPUT%ArchiveInfo.TXT
 IF NOT EXIST "%@AUDIT-INFO%"   SET @AUDIT-INFO=%@INPUT%AuditSettings.TXT
 IF NOT EXIST "%@REPL-INFO%"    SET @REPL-INFO=%@INPUT%ReplicationInfo.TXT
 IF NOT EXIST "%@MRTG-INFO%"    SET @MRTG-INFO=%@INPUT%MRTG-Info.TXT
 IF NOT EXIST "%@ZIP-INFO%"     SET @ZIP-INFO=%@INPUT%ArchiveInfo.TXT

 IF NOT EXIST "%@EXEMPTION%"    SET @EXEMPTION=%@INPUT%CopyExemption.TXT
 IF NOT EXIST "%@PROCESS-INFO%" SET @PROCESS-INFO=%@INPUT%LongProcesses.TXT
 IF NOT EXIST "%@BAD_PROCESS%"  SET @BAD_PROCESS=%@INPUT%BadProcesses.TXT
 IF NOT EXIST "%@BAD_SERVICE%"  SET @BAD_SERVICE=%@INPUT%BadServices.TXT

 IF NOT EXIST "%@BKUPSET%"      SET @BKUPSET=%@INPUT%SystemState-Plus.BKS
 IF NOT EXIST "%@NOBACKUP%"     SET @NOBACKUP=%@INPUT%NoSystemState.DAT
 IF NOT EXIST "%@NOEXCHANGE%"   SET @NOEXCHANGE=%@INPUT%NoExchangeData.DAT

 rem -- Determine If Backup Set should Be Used, Or Just SystemState Only...
 IF EXIST "%@NOBACKUP%" (
           SET @SSBACKUP=TRUE
 ) ELSE (
           SET @SSBACKUP=FALSE
 )

 rem -- Cycle Through List of Valid Script Input Files
 SET @SOURCETXT=& IF EXIST "%@SOURCEINPUT%" FOR /F "USEBACKQ SKIP=2 DELIMS=\" %%I IN ("%@SOURCEINPUT%") DO SET @SOURCETXT=!@SOURCETXT! %%~I
 IF NOT DEFINED @SOURCETXT SET @SOURCETXT=AD-DCs ArchiveInfo AuditSettings Browser-Exempt CopyExemption CustomVariables ExchangeList ExchangeTest FolderDefaults IIS6Setup KnownServices LongProcesses LowDisk ReplicationInfo ScriptInput ScriptSource SecurityList ServerList SmallList SpecialRoutes UptimeList


 rem -- Get OS Version for NT and Higher
 rem -- Last Modified On: 24 Oct 2010  (Prior Update: 26 May 2006)
 FOR /F "TOKENS=2 DELIMS=[]" %%V IN ('VER') DO (
           FOR /F "TOKENS=2-4 DELIMS=. " %%O IN ('ECHO %%~V') DO (
                     SET @OSVER=%%~O.%%~P.%%~Q
                     SET @OSMAJ=%%~O
                     SET @OSMIN=%%~P
                     SET @OSBUILD=%%~Q
           )
 )

 rem -- Determine OS-specific Variables for Task Scheduler & ROBOCOPY
 rem -- Last Modified On: 13 Apr 2012  (Prior Update: 20 Mar 2012)
 SET @FORCE=& IF %@OSBUILD% GEQ 3790 SET @FORCE=/F
 SET @THREADS=& IF %@OSBUILD% GEQ 7600 SET @THREADS=/MT:4
 SET @JOBLEVEL=& IF %@OSBUILD% GEQ 6000 SET @JOBLEVEL=/RL HIGHEST
 SET @ROBOCOPY=%SystemRoot%\System32\ROBOCOPY.EXE& IF NOT EXIST "!@ROBOCOPY!" SET @ROBOCOPY=RoboCopy.EXE
 SET @RETRIES=/R:3 /W:60

 rem -- Calculate Current Date/Time (Long Procedure -- Using ResKit [Includes Day/Month names])
 rem -- Last Modified On: 16 Mar 2008  (Prior Update: 07 Jan 2007)
 FOR /F "TOKENS=2" %%D IN ('DATE /T') DO SET @DATE=%%D
 FOR /F "TOKENS=*" %%T IN ('TIME /T') DO SET @TIME12=%%T
 FOR /F "TOKENS=1-5" %%D IN ('NOW') DO (
           SET @DAY=%%D
           SET @MMM=%%E
           SET @TIME=%%G
           SET @TIME24=%%G
 )

 rem -- Calculate Current Date/Time (Long Procedure -- Using Doff.exe)
 rem -- Last Modified On: 27 May 2010  (Prior Update: 16 Mar 2008)
 FOR /F "TOKENS=1-7" %%D IN ('DOFF "yyyy yy mm dd hh mi ss"') DO (
           SET @YYYY=%%D
           SET @YY=%%E
           SET @MM=%%F
           SET @DD=%%G
           SET @HR=%%H
           SET @MIN=%%I
           SET @SEC=%%J
           SET @NOW=%%H%%I%%J
           SET @NOW_EXP=%%H.%%I.%%J
 )

 rem -- Determine Current Day of Year (Using DateInfo.exe)
 rem -- Last Modified On: 19 Sep 2012  (Prior Update: 10 May 2012)
 SET @DATE-INFO=& FOR %%V IN (%@UTILS%) DO IF EXIST "%%~V\DateInfo.EXE" SET @DATE-INFO="%%~V\DateInfo.EXE"
 IF DEFINED @DATE-INFO (
           FOR /F "TOKENS=2 DELIMS=." %%D IN ('DATEINFO ^| FIND /I "Day of Year"') DO SET @DAYOFYEAR=%%D
 ) ELSE (
           SET @MONTH= %@MM%
           SET /A @MAXDAY=!@MONTH: 0=! * 31
           FOR /L %%C IN (0,1,!@MAXDAY!) DO FOR /F "TOKENS=1" %%D IN ('DOFF "yyyymmdd" -%%Cd') DO IF /I "%%D"=="%@YYYY%0101" SET @DAYOFYEAR=%%C
 )

 rem -- Determine EVEN or ODD Day
 rem -- Last Modified On: 08 Dec 2010  (Prior Update: 11 Nov 2010)
 SET /A @DD_DIV_2=%@DD:~-1% %% 2
 IF %@DD_DIV_2% GEQ 1 (
           SET @DATE_IS_ODD=TRUE
           SET @DATE_IS_EVEN=FALSE
           SET @DATE_IS=ODD
 ) ELSE (
           SET @DATE_IS_ODD=FALSE
           SET @DATE_IS_EVEN=TRUE
           SET @DATE_IS=EVEN
 )

 rem -- Calculate Future Dates (Long Procedure -- Using DOff.exe)
 rem -- Last Modified On: 13 Mar 2012  (Prior Update: n/a)
 FOR /F "TOKENS=1"   %%Y IN ('DOFF "yyyy"') DO SET /A @NEXTYEAR=%%Y + 1
 FOR /F "TOKENS=1"   %%M IN ('DOFF "mm" +30') DO SET @NEXTMONTH=%%M
 FOR /F "TOKENS=1-3" %%W IN ('DOFF "yyyy mm dd" +7d') DO (SET @NEXTWEEK=%%W%%X%%Y& SET @NEXTWEEK_EXP=%%W-%%X-%%Y)
 FOR /F "TOKENS=1-3" %%T IN ('DOFF "yyyy mm dd" +1d') DO (SET @TOMORROW=%%T%%U%%V& SET @TOMORROW_EXP=%%T-%%U-%%V)

 rem -- Calculate Previous Dates (Long Procedure -- Using DOff.exe)
 rem -- Last Modified On: 13 Mar 2012  (Prior Update: 08 Nov 2010)
 FOR /F "TOKENS=1"   %%Y IN ('DOFF "yyyy"') DO SET /A @LASTYEAR=%%Y - 1
 FOR /F "TOKENS=1"   %%M IN ('DOFF "mm" -%@DD%') DO SET @LASTMONTH=%%M
 FOR /F "TOKENS=1-3" %%W IN ('DOFF "yyyy mm dd" -7d') DO (SET @LASTWEEK=%%W%%X%%Y& SET @LASTWEEK_EXP=%%W-%%X-%%Y)
 FOR /F "TOKENS=1-3" %%D IN ('DOFF "yyyy mm dd" -1d') DO (SET @YESTERDAY=%%D%%E%%F& SET @YESTERDAY_EXP=%%D-%%E-%%F)

 SET @BEGMONTH=%@MM%01%@YYYY%
 SET @MMDDYYYY=%@MM%%@DD%%@YYYY%
 SET @TODAY=%@YYYY%%@MM%%@DD%
 SET @TODAY_EXP=%@YYYY%-%@MM%-%@DD%
 SET @ZIPSTAMP=%@TODAY%-%@NOW%.ZIP
 SET @FILESTAMP=%@TODAY%-%@NOW%.TXT
 SET @TIMESTAMP=%@DATE:/=-% at %@TIME12::=.%
 SET @TIMESTAMP24=%@DATE:/=-% at %TIME%


 rem -- Set Storage Drive for Current System (Obtained from Central List)
 rem -- Last Modified On: 17 Nov 2011  (Prior Update: 14 Jul 2010)
 FOR /F "TOKENS=1-3" %%F IN (%@STORAGELIST%) DO IF /I "%COMPUTERNAME%"=="%%~F" (
           SET @WHICHDRV=%%~G
           SET @BACKUPDRV=!@WHICHDRV!
           FOR %%V IN (%@ALPHABET%) DO IF /I "%%~H"=="%%~V:" SET @BACKUPDRV=%%~H
 )
 SET @STORAGE=%@WHICHDRV%\Storage
 SET @BACKUPS=%@BACKUPDRV%\Storage\Backups


 rem ==========================================================================
 rem === Reset Environment Variables and Exit Batch File
 rem === Last Modified On: 11 Feb 2011  (Prior Update: 07 Jan 2007)
 rem ==========================================================================
:ExitBatch
 IF /I "%DEBUG%"=="TRUE" ECHO SETTING STORAGE FOLDER TO "%@STORAGE%" [%@VAR_VER%]
 SET @INPUT=
 SET @UTILS=
 SET @SCRIPTS=

