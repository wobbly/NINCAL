           inc        common.inc
           inc        cons.inc
           
          Move      "2007",str4
          move      "12",str2
          Pack      Taskname from "\\nins1\e\data\text\Nininv.dat,\\nins1\e\data\nininv.open;7-12,S=#"217=<'",STR4,'#""
          Sort      Taskname,SunDM="10.10.30.103:502"

          stop 