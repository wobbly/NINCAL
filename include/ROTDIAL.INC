;    Last change:  MLO  29 Apr 94   12:10 pm
rotdial    add     c1 to position
           branch   position of p1,p2,p3,p4,p5,p6,p7,p8
           move    c0 to position
p1         display *p79:24,*hon,*red,i179,*white,*hoff;
           return           
p2         display *p79:24,*hon,*red,"/",*white,*hoff;
           return                      
p3         display *p79:24,*hon,*red,dash,*white,*hoff;
           return                   
p4         display *p79:24,*hon,*red,"\",*white,*hoff;
           return
p5         display *p79:24,*hon,*red,i179,*white,*hoff;
           return                         
p6         display *p79:24,*hon,*red,"/",*white,*hoff;
           return                         
p7         display *p79:24,*hon,*red,dash,*white,*hoff;
           return                                    
p8         display *p79:24,*hon,*red,"\",*white,*hoff;
           return
                   
