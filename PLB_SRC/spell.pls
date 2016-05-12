

         include        direct.inc
         
HexZero  Init      0x0             Hex zero
DLLNAME1 INIT      "C:\ANDREW\PLB\SPELLC~1\ZIP\SPELMATE.DLL",0x00
HANDLE   INTEGER   4          .Windows Handle (used by API)
SPELLRET INTEGER   1          .Variable returned after API call
SPELLOK  INTEGER   4
SPLLHNDL INTEGER   4          .Dummy return variable to determine if

LOADLIB  PROFILE   Kernel32,LoadLibraryA,INT4,DIM

         WINAPI    LOADLIB GIVING SPLLHNDL USING DLLNAME1

chekpro
SPELLINT PROFILE   Spelmate,SpelMateInit,INT1

         WINAPI   SPELLINT GIVING SPELLRET
.
chekrtn
         IF       (SPELLRET >= 0)
                  if (SPELLRET <> 255)
                  DISPLAY    "Spell Checking program not loaded properly",*B,*W4
                  STOP
                  ENDIF
         ENDIF

CHECKTXT PROFILE    Spelmate,SpellEdit,INT1,INT4
x        plform     spell
         formload   x

         GETPROP    SpellEditText1,HWND=HANDLE

         LOOP
                WAITEVENT
         REPEAT

CHECKIT
             WINAPI     CHECKTXT GIVING SPELLOK USING HANDLE
         RETURN

