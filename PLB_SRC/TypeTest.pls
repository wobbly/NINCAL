
Str10       Dim               10


               Display        *es,*p1:10,"TEST"
Key            Keyin         *p1:12,*el,"Just let time out pls",*t15,*rv,*JR,str10
               type           str10
               goto           exit if eos
               Display        *p1:13,"Not empty, try again"
               goto           Key
Exit           Display       *p1:12,*ef,*b,"bye",*w2
               Stop

