               include        common.inc
               include        cons.inc
               include        Nprmdd.inc
pc             equ            0
release        Init           "temp"
input          file

PhoneNum       dim	20
Name	dim	29
CompName       dim	50
.
count          Form           5
.

               Open           input,"c:\work\DCweekly.wrk"
               Prepare        PrmFile2,"\\nts1\e\data\text\FaxPromo.dat","\\nts1\e\data\index\FaxPromo","106-111","125"
               Prepare        PrMFile,"\\nts1\e\data\text\FaxPromo.dat","\\nts1\e\data\index\FaxPromo","U,56-105,21-55","125"
.               close          prmFile1
.               Close          PrmFile
               Open           PrmFlist
               move           c1 to count

.
Looper         read           input,seq;Phonenum,Name,Compname
               goto           looperX if over
               clear          PrmWeekLy
               Clear          PrmPromo
               clear          PrmBroker
               clear          PrmClient
               clear          PrmFill
               clear          PrmPhnum
               clear          PrmCntName
               clear          PrmCOName
               move           Phonenum to PrmPhnum
               move           Name to PrmCntName
               move           compname to PrmCoName
               move           "Y" to prmWeekLy
               move           count to PrmIdnum
               rep            zfill in PrmIdnum
               write          PrmFlist;PrmVars
               add            c1 to count
               goto           looper
looperx
               Open           input,"c:\work\Promolst.wrk"
.
Looper2         read           input,seq;Phonenum,Name,Compname
               goto           looper2X if over
               clear          PrmWeekLy
               Clear          PrmPromo
               clear          PrmBroker
               clear          PrmClient
               clear          PrmFill
               clear          PrmPhnum
               clear          PrmCntName
               clear          PrmCOName
               move           Phonenum to PrmPhnum
               move           Name to PrmCntName
               move           compname to PrmCoName
               move           "Y" to prmPromo
               clear          PrmAkey1
               clear          PrmAkey2
               if             (prmCoName <> "")
               packkey        PrmAkey1 from "01L",PrmCoName
               endif
               if             (prmCntName <> "")
               packkey        PrmAkey2 from "02L",PrmCntName
               endif
               read           prmFile,PrmAkey1,PrmAkey2;PrmVars
               if             over
               move           Phonenum to PrmPhnum
               move           Name to PrmCntName
               move           compname to PrmCoName
               move           "Y" to prmPromo
               move           count to PrmIdnum
               rep            zfill in PrmIdnum
               write          PrmFlist;PrmVars
               add            c1 to count
               else
               move           "Y" to prmPromo
               update         PrmFlist;PrmVars
               endif
               goto           looper2
looper2x
               Open           input,"c:\work\broker.wrk"
.
Looper3         read           input,seq;Phonenum,Name,Compname
               goto           looper3X if over
               clear          PrmWeekLy
               Clear          PrmPromo
               clear          PrmBroker
               clear          PrmClient
               clear          PrmFill
               clear          PrmPhnum
               clear          PrmCntName
               clear          PrmCOName
               move           Phonenum to PrmPhnum
               move           Name to PrmCntName
               move           compname to PrmCoName
               move           "Y" to PrmBroker
               clear          PrmAkey1
               clear          PrmAkey2
               call           Trim using PrmConame
               call           Trim using PrmCntname
               if             (prmCoName = "" & PrmCntname = "")
               goto           looper3
               endif
               if             (prmCoName <> "")
               packkey        PrmAkey1 from "01L",PrmCoName
               endif
               if             (prmCntName <> "")
               packkey        PrmAkey2 from "02L",PrmCntName
               endif
               read           PrmFile,PrmAkey1,PrmAkey2;PrmVars
               if             over
               move           Phonenum to PrmPhnum
               move           Name to PrmCntName
               move           compname to PrmCoName
               move           "Y" to prmBroker
               move           count to PrmIdnum
               rep            zfill in PrmIdnum
               write          PrmFlist;PrmVars
               add            c1 to count
               else
               move           "Y" to prmBroker
               Update         PrmFList;PrmVars
               endif
               goto           looper3
               
looper3x
               Open           input,"c:\work\Clientlst.wrk"
.
Looper4         read           input,seq;Phonenum,Name,Compname
               goto           looper4X if over
               clear          PrmWeekLy
               Clear          PrmPromo
               clear          PrmBroker
               clear          PrmClient
               clear          PrmFill
               clear          PrmPhnum
               clear          PrmCntName
               clear          PrmCOName
               move           Phonenum to PrmPhnum
               move           Name to PrmCntName
               move           compname to PrmCoName
               move           "Y" to PrmClient
               clear          PrmAkey1
               clear          PrmAkey2
               call           Trim using PrmConame
               call           Trim using PrmCntname
               if             (prmCoName <> "")
               packkey        PrmAkey1 from "01L",PrmCoName
               endif
               if             (prmCntName <> "")
               packkey        PrmAkey2 from "02L",PrmCntName
               endif
               read           PrmFile,PrmAkey1,PrmAkey2;PrmVars
               if             over
               move           Phonenum to PrmPhnum
               move           Name to PrmCntName
               move           compname to PrmCoName
               move           "Y" to prmClient
               move           count to PrmIdnum
               rep            zfill in PrmIdnum
               write          PrmFlist;PrmVars
               add            c1 to count
               else
               move           "Y" to prmClient
               Update         PrmFlist;PrmVars
               endif
               goto           looper4
Looper4x
               stop

               include        comlogic.inc
