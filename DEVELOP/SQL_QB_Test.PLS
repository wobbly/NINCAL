...............................................................................
. 
. Date    : 11/14/2009
.
. To      : Sunbelt Forum
.       
. From    : Gerhard Weiss       BeneSys Inc.
.                               700 Tower Drive, Suite 300
.                               Troy MI 48098-2808
.                               Phone: (248) 813-9800
.                               Fax  : (248) 813-9320
.                               email: gerhard.weiss@benesysinc.com
.                                      gerhard.weiss@live.com
. 
. PROGRAM : IMPORT FLAT FILE INTO SQL SERVER
.
.
. O/S     : Windows XP Service Pack 3
.
. Note    :
.
.
CCMENU   DIM    %12   .Return to menu name
         DISPLAY   *UC;
.
FILENAME DIM       50
FILETXT1 DIM       50
FILETXT2 DIM       50
FILESQL  DIM       50
FILEFMT  DIM       50
..............................................................................
FILESPL  DIM       30
STPAGE   FORM      4
ENDPAGE  FORM      4
WPRT     DIM       75
.
N        INIT      "N"
Y        INIT      "Y"
N0       FORM      "0"
N1       FORM      " 1"
N2       FORM      " 2"
N3       FORM      " 3"
N4       FORM      " 4"
N7       FORM      "7"
NEG1     FORM      "-1"
B1       INIT      " "
B2       INIT      "  "
B3       INIT      "   "
B4       INIT      "    "
B50      INIT      "                                                  "
D3       DIM       3
ANS      DIM       1
ENTSEL   FORM      1
...
STTIME   DIM       8
EDTIME   DIM       8
.
...............................................................................
.
..         INC       SPLTFLNM.INC
.
strSqlServerName DIM 30
strSqlDatabaseName DIM 30
strSqlTableName DIM 30
strSqlCreateTable DIM ^50000
cnnSqlConnection AUTOMATION             .Connection Object
curPropSqlEnabled FORM 1
strSqlConnection DIM ^200
.
JBCMDLN  DIM       255
SEQ      FORM      "-1"
.
strTitle INIT "IMPORT FLAT FILE INTO SQL SERVER"
         DISPLAY   *ES,strTitle,*N
.
.
.**************    CHANGE THESE VALUES     ****************************
SUNEPDIR INIT      "T:\PLSOLD\"
         MOVE      "NTS1",strSqlServerName
         MOVE      "C_K200",strSqlDatabaseName 
         MOVE      "SUNEP080",strSqlTableName
.**********************************************************************
.
.
         DISPLAY   *P1:03,*EF:
                   *P1:03,"Server  : ",strSqlServerName:
                   *P1:04,"Database: ",strSqlDatabaseName:
                   *P1:05,"Table   : ",strSqlTableName;
SAVEASER KEYIN     *P11:03,*DVEDIT=strSqlServerName;
         GOTO      CHAINOUT IF F1
SAVEASDB KEYIN     *P11:04,*DVEDIT=strSqlDatabaseName;
         GOTO      SAVEASER IF F1
         KEYIN     *P11:05,*DVEDIT=strSqlTableName;
         GOTO      SAVEASDB IF F1
...............................................................................
         DISPLAY   *SAVESW:
                   *P1:24,*EL;
.
...............................................................................
.                  Connect to the Server/Database
.
.                             Windows Authentication
         PACK      strSqlConnection WITH "Provider=SQLOLEDB;Data Source=",strSqlServerName,";":
                   "Initial Catalog=",strSqlDatabaseName,";Integrated Security=SSPI;"
.
.                             Change xxxx to User Name and Password if Windows Authentication is not used
..         PACK      strSqlConnection WITH "Provider=SQLOLEDB;Data Source=",strSqlServerName,";":
..                   "Initial Catalog=",strSqlDatabaseName,";User ID=xxxx;Password=xxxx;"
.
         CREATE    cnnSqlConnection,Class="ADODB.Connection"
         SETPROP   cnnSqlConnection,*ConnectionTimeout=45
         SETPROP   cnnSqlConnection,*CommandTimeout=0
         cnnSqlConnection.Open Using strSqlConnection
.
..       DISPLAY   "After OpenCnn";
..       KEYIN     ANS;
.
...............................................................................
.                  Build a String that will TRUNCATE and DROP the table
.                  if it exists. Then do a CREATE TABLE
         PACK      strSqlCreateTable WITH "IF EXISTS (SELECT * FROM sys.OBJECTS WHERE name = '",strSqlTableName,"') ":
                   "BEGIN ":
                   " TRUNCATE TABLE ",strSqlTableName," ":
                    " DROP TABLE ",strSqlTableName," ":
                   "END ":
                    "CREATE TABLE ",strSqlTableName," "
.
SAVASFFMT FILE     VAR=200
SAVASVFMT DIM      200

SAVASVIN  FORM     2
SAVASVLS  FORM     2
SAVASVTB  DIM      50(99)
.
.                  We generate these .SQL files when FDs are created. 
         TRAP      NOFILE IF IO
         PACK      FILESQL WITH SUNEPDIR,"SUNEP080.SQL"
         MOVE      FILESQL,FILENAME
         OPEN      SAVASFFMT,FILESQL
         TRAPCLR   IO
         ENDSET    strSqlCreateTable
         READ      SAVASFFMT,SEQ;SAVASVFMT
         LOOP
           READ      SAVASFFMT,SEQ;SAVASVFMT
           UNTIL     OVER
           CHOP      SAVASVFMT
           EXPLODE   SAVASVFMT,B1 GIVING SAVASVLS USING SAVASVTB
           FOR       SAVASVIN FROM "1" TO SAVASVLS
             IF        (SAVASVTB(SAVASVIN)!="")
               APPEND    SAVASVTB(SAVASVIN),strSqlCreateTable
               APPEND    B1,strSqlCreateTable
.
..       DISPLAY   *P1:24,*EL,"SAVASVLS=",SAVASVLS," SAVASVTB(",SAVASVIN,")=",*LL,SAVASVTB(SAVASVIN),*HON,"<",*HOFF;
..       KEYIN     ANS;
.
             ENDIF
           REPEAT
         REPEAT
         CLOSE     SAVASFFMT
         CHOP      SAVASVFMT
         APPEND    SAVASVFMT,strSqlCreateTable
         APPEND    B1,strSqlCreateTable
         RESET     strSqlCreateTable
.
         KEYIN     *N,*N,"Before ExecuteCnn"
.
.
         CALL      ExecuteCnn USING cnnSqlConnection,strSqlCreateTable
...............................................................................
.                             Remove Deleted Data from Text File using SORT
.
         PACK      FILETXT1 WITH SUNEPDIR,"SUNEP080.TXT"
         PACK      FILETXT2 WITH SUNEPDIR,"SUNEP080_SORTED.TXT"
         PACK      JBCMDLN WITH FILETXT1," ",FILETXT2," -10000"
         SORT      JBCMDLN
.
...............................................................................
.                             bcp (import) the data into SQL Server
.
         PACK      FILEFMT WITH SUNEPDIR,"SUNEP080.FMT"
.
.                             -T is using Windows Authentication.
         PACK      JBCMDLN WITH "bcp ",strSqlDatabaseName,".dbo.",strSqlTableName," in ":
                     FILETXT2," -f",FILEFMT," -S",strSqlServerName," -T"
.                             Change xxxx to User Name and Password if Windows Authentication is not used
..         PACK      JBCMDLN WITH "bcp ",strSqlDatabaseName,".dbo.",strSqlTableName," in ":
..                     FILETXT2," -f",FILEFMT," -S",strSqlServerName," -Uxxxx -Pxxxx"
         DISPLAY   *N,*+,JBCMDLN,*N
..         CALL      CHV1JBEX USING ":ADD",JBCMDLN
         EXECUTE   JBCMDLN
.
...............................................................................
.                             Delete Sorted work file
.
         ERASE     FILETXT2
.
...............................................................................
         DISPLAY   *ES,*RESTSCR;
.
         LOOP
           MOVE      B1,ANS
           KEYIN     *P01:08,"Import Completed. Open SQL Server Management Studio Y/N ? ":
                     *DVRV=ANS
           UNTIL     F1
           UNTIL     (ANS=Y|ANS=N)
         REPEAT
...............................................................................
.                             Command Line option to Open SQL Server Management Studio 
         IF        (ANS=Y)
           DISPLAY   *SAVESW;
           PACK      JBCMDLN WITH "SqlWb.exe -S ",strSqlServerName," -d ",strSqlDatabaseName," -E -nosplash"
           BATCH     JBCMDLN
           DISPLAY   *ES,*RESTSCR;
         ENDIF
...............................................................................
.
         KEYIN     *P1:24,*EL,"Processing Completed.  Tap ENTER ",ANS;
         GOTO      CHAINOUT
         
NOFILE   NORETURN
         DISPLAY   *N,*RPTCHAR "-":80,*N,*B,"File Not Found: ",FILENAME
         GOTO      CHAINOUT
.
CHAINOUT MATCH     B1,CCMENU
         STOP      IF EOS
         CHAIN     CCMENU
.
rcnnOpenCnn AUTOMATION ^
rstrExecSQLCmd DIM ^
ExecuteCnn ROUTINE rcnnOpenCnn,rstrExecSQLCmd
         TRAP      ExecuteTrapSet IF OBJECT
         rcnnOpenCnn.Execute Using rstrExecSQLCmd
         TRAPCLR   OBJECT
         RETURN
.
ExecuteTrapSet
         NORETURN
         DISPLAY   *P1:24,*N:
                   "BenAdo: Execute Trap Object Tripped!"
         KEYIN     *CL,"BenAdo: Execute Trap Object Tripped!",*N:
                   "Tap ENTER ",ANS;
         GOTO      CHAINOUT
.
.
.
..         INC       CCCOMCAL.INC
.
