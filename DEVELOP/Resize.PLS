........................................
. Program:      Resize.PLS
. Author:       Andrew Harkins

str4    dim     4
fontsize form   9
.Define Fonts to be used
font5   font

coll1   collection
specs   form          4(4)
size    form          "1.000"
infostring dim        590

.Getinfo
        getinfo system,infostring
        bump    infostring,12
        move    infostring,str4

x       plform  REsize
        winhide
        
.Load Forms, Always declare parent form first
        formload x

        move    "10",fontsize
        if (str4 = "1024")
                move    "1.25",size
                move    "14",fontsize                       
        endif

        create  font5,"Arial",size=fontsize
        getprop test2,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
        mult    size,specs
        setprop test2,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.
        getprop test2buttonok,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
        mult    size,specs
        setprop test2buttonok,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5
.
        getprop test2stattext1,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
        mult    size,specs
        setprop test2stattext1,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5         
.
        getprop test2stattext2,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
        mult    size,specs
        setprop test2stattext2,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5        

.Added to try and rehide object
        WINREFRESH resize
        loop
                waitevent
        repeat
        

