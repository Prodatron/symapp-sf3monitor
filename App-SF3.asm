;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@                                                                            @
;@                 S Y M B i F A C E   I I I   M o n i t o r                  @
;@                                                                            @
;@               (c) 2019 by Prodatron / SymbiosiS (Jörn Mika)                @
;@                                                                            @
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

;Todo

;==============================================================================
;### CODE-TEIL ################################################################
;==============================================================================

;### PRGPRZ -> Programm-Prozess
prgwinid    db 0    ;main window ID
olewinid    db -1   ;oled window ID

prgprz  call SySystem_HLPINI

        call sf3det
        call sf3mes
        call sf3spc
        call sf3usb
        call sf3mos
        ld a,(sf3detf)
        dec a
        jr nz,prgprz2
        ld hl,(syslstlst+4)         ;sf3 present
        ld (stactltxa),hl
        ld hl,statxttxa0
        ld (stactltxa0),hl

prgprz2 ld a,(App_BnkNum)
        ld de,prgwindat
        call SyDesktop_WINOPN
        jp c,prgend
        ld (prgwinid),a

prgprz0 ld ix,(App_PrcID)           ;check for messages
        db #dd:ld h,-1
        ld iy,App_MsgBuf
        rst #08
        db #dd:dec l
        jr nz,prgprz0
        ld a,(App_MsgBuf+0)
        or a
        jr z,prgend
        cp MSR_DSK_WCLICK
        jr nz,prgprz0
        ld a,(App_MsgBuf+2)
        cp DSK_ACT_CLOSE
        jr z,prgprz3
        cp DSK_ACT_CONTENT
        jr nz,prgprz0
prgprz1 ld hl,(App_MsgBuf+8)
        ld a,h
        or l
        jr z,prgprz0
        jp (hl)
prgprz3 ld a,(App_MsgBuf+1)
        ld hl,prgwinid
        cp (hl)
        jr z,prgend
        jp oleclo

;### PRGEND -> Programm beenden
prgend  ld hl,(App_BegCode+prgpstnum)
        call SySystem_PRGEND
prgend0 rst #30
        jr prgend0

;### PRGTAB -> tab has been pressed
prgtaba db 0        ;actual tab
prgtabt db 41:dw syslstobj
        db 33:dw usbinpobj
        db 33:dw mosinpobj

prgtab  ld a,(tabctlobj+2)
        ld hl,prgtaba
        cp (hl)
        jp z,prgprz0
        ld (hl),a
        ld c,a
        add a
        add c
        ld c,a
        ld b,0
        ld hl,prgtabt
        add hl,bc
        ld de,prgwinobj1+2
        ldi
        inc de
        ldi:ldi
        ld de,256*13+256-2
prgtab1 ld a,(prgwinid)
        call SyDesktop_WINDIN
        jp prgprz0

;### Refresh measurements
butref  call sf3mes
        ld de,256*7+256-3
        jr prgtab1

;### Beep
butbep  ld a,(sf3detf)
        sub 1
        ret c
        ld de,#0203
        call butbep0
        ld b,50*6
butbep3 rst #30
        djnz butbep3
        ld de,#0000
        call butbep0
        jp prgprz0
butbep0 call sf3rdy
        xor a
        out (c),a
        inc c
        out (c),d
        out (c),e
        dec c
        ld a,80
        out (c),a
        call sf3rsp
        ei
        ret

;### BUTOLE -> opens OLED dialogue
butole  ld a,(olewinid)
        inc a
        jp nz,prgprz0
        ld a,(App_BnkNum)
        ld de,olewindat
        call SyDesktop_WINOPN
        jp c,prgprz0
        ld (olewinid),a
        jp prgprz0

;### OLEPRT -> prints text on OLED display
oleprtx db 0
oleprty db 0

oleprt  ld a,(oletxtinp+8)
        or a
        jp z,prgprz0
        ld a,(olepossta)
        cp 2
        jr nc,oleprt3
        add 202                 ;upper/lower position
        push af
        call sf3rdy
        xor a
        out (c),a
        inc c
        call oleprt1 
        pop af
oleprt6 dec c
        out (c),a
        call sf3rsp
        ei
        jp prgprz0

oleprt3 ld hl,olexpsbuf
        call clcr08
        ld a,127
        call oleprt4
        push af
        ld hl,oleypsbuf
        call clcr08
        ld a,31
        call oleprt4
        pop bc
        ld c,a
        push bc
        call sf3rdy
        xor a
        out (c),a
        inc c
        pop de
        out (c),d
        out (c),e
        ld a,(olefntsta)        ;10,18,26
        add a:add a:add a
        add 10
        out (c),a
        call oleprt1
        ld a,210
        jr oleprt6

oleprt4 jr c,oleprt5
        cp e
        jr c,oleprt5
        ld a,e
        ret
oleprt5 xor a
        ret

oleprt1 ld de,(oletxtinp+8)     ;send text
        ld hl,oletxtbuf
oleprt2 ld a,(hl)
        out (c),a
        inc hl
        dec e
        jr nz,oleprt2
        ret

;### OLECLR -> clears OLED display
oleclr  call sf3rdy
        ld a,200
        out (c),a
        call sf3rsp
        ei
        jp prgprz0

;### OLECLO -> closes OLED dialogue
oleclo  ld hl,olewinid
        ld a,(hl)
        ld (hl),-1
        call SyDesktop_WINCLS
        jp prgprz0


;### INPPRE -> prepares input control
;### Input      HL=text, DE=textend, IX=data structure
;### Destroyed  ?
inppre  ld (ix+0),l
        ld (ix+1),h
        ex de,hl
        or a
        sbc hl,de
        ld (ix+8),l
        ld (ix+9),h
        ret

;### CLCR08 -> Converts String into 8bit value
;### Input      HL=string (0-terminated)
;### Output     CF=0 -> ok, E=number
;###            CF=1 -> wrong format/>=250
;### Destroyed  AF,BC,HL
clcr08  ld e,0
clcr081 ld a,(hl)
        inc hl
        or a
        ret z
        sub "0"
        ret c
        cp 10
        ccf
        ret c
        ld b,a
        ld a,e
        cp 25
        ccf
        ret c
        add a:ld c,a:add a:add a:add c
        add b
        ld e,a
        jr clcr081
        

;==============================================================================
;### SF3-ROUTINES (HARDWARE) ##################################################
;==============================================================================

;### SF3RED -> reads info data from the SF3
;### Input      E=function number, HL=destination buffer (2KB max)
;### Output     CF=1 no SF3 present
;### Destroyed  AD,BC,DE,HL
sf3red  ld a,(sf3detf)
        sub 1
        ret c
        call sf3rdy
        out (c),e
        call sf3rsp
        inc c
        ld de,2048
sf3red1 in a,(c)
        ld (hl),a
        inc hl
        cp 10
        jr z,sf3red2
        dec de
        ld a,e
        or d
        jr nz,sf3red1
sf3red2 ei
        or a
        ret

;### SF3DET -> detects SYMBiFACE III
;### Output     (sf3detf)=1 SYMBiFACE III present
;### Veraendert AF,BC,HL
sf3detf db 0
sf3det  xor a
        ld (sf3detf),a
        call sf3det2
        ret nc
        ld a,71
        out (c),a
        call sf3det2
        ret nc
        inc c
        in a,(c):cp "S":ret nz
        in a,(c):cp "y":ret nz
        in a,(c):cp "s":ret nz
        call sf3det2
        ret nc
        ld hl,sf3detf
        inc (hl)
        ret
sf3det2 ld hl,5000          ;wait for sf3
        ld bc,#fd41
sf3det1 dec hl
        ld a,l
        or h
        ret z
        in a,(c)
        dec a
        jr z,sf3det1
        scf
        ret

;### SF3RDY -> wait for SF3 ready
;### Output     DI,BC=#FD41
;### Veraendert AF
sf3rdy  ld bc,#fd41
        di
        in a,(c)
        cp 1
        ret c
        ei
        rst #30
        jr sf3rdy

;### SF3RSP -> wait for SF3 response
;### Output     BC=#FD41
;### Veraendert AF
sf3rsp  ld bc,#fd41
        in a,(c)
        dec a
        jr z,sf3rsp
        ret


;==============================================================================
;### SF3-ROUTINES (SOFTWARE) ##################################################
;==============================================================================

;### SF3MES -> prepares measurements
sf3mest db "C",0
sf3mes  ld e,125
        ld hl,srcsf3txt
        call sf3red
        ret c
        ld hl,srcsf3txt
        ld de,statxttxb0
        call sf3mes1
        ld de,statxttxc0
        call sf3mes1
        ld de,statxttxd0
        call sf3lin
        dec de
        dec de
        ld hl,sf3mest
        ldi:ldi
        ret
sf3mes1 ld b,5
        ld a," "
sf3mes6 ld (de),a
        inc de
        djnz sf3mes6
        ld b,-1
sf3mes2 inc b
        ld a,(hl)
        inc hl
        cp 13
        jr nz,sf3mes2
        push hl
        dec hl
        ld c,3
sf3mes3 inc b:dec b
        ld a,"0"
        jr z,sf3mes4
        dec hl
        ld a,(hl)
        dec b
sf3mes4 ld (de),a
        dec de
        dec c
        jr nz,sf3mes5
        ld a,"."
        ld (de),a
        dec de
sf3mes5 inc b:dec b
        jr nz,sf3mes3
        bit 7,c
        jr z,sf3mes3
        pop hl
        ret

;### SF3SPC -> prepares USB drive info box
sf3spc  ld e,71
        ld hl,srcsf3txt
        call sf3red
        ret c
        ld hl,srcsf3txt
        ld de,spclsttxt
        ld ix,syslstlst
        db #fd:ld l,32
sf3spc1 ld a,(hl)
        cp 10
        jr z,sf3spc7    ;end
        cp 13
        jr z,sf3spc9    ;empty line -> skip
        ld c,e
        ld b,d          ;BC=1st column start
sf3spc2 ld a,(hl)       ;**LOOP 1st column
        cp ":"
        jr z,sf3spc3
        cp 13
        jr z,sf3spc8
        ldi
        inc bc
        jr sf3spc2
sf3spc8 ld (ix+4),c     ;only one column -> use as 2nd one
        ld (ix+5),b
        ld bc,lstemptxt
        ld (ix+2),c
        ld (ix+3),b
        jr sf3spc6
sf3spc3 ld (ix+2),c     ;1st column finished -> save start adr
        ld (ix+3),b
        xor a
        ld (de),a
        inc de
        ld (ix+4),e     ;save 2nd column start
        ld (ix+5),d
sf3spc4 inc hl
        ld a,(hl)       ;skip spaces
        cp 32
        jr z,sf3spc4
sf3spc5 ld a,(hl)       ;**LOOP 2nd column
        cp 13
        jr z,sf3spc6
        ldi
        jr sf3spc5
sf3spc6 xor a
        ld (de),a
        inc de
        db #fd:dec l
        ld bc,6
        add ix,bc
sf3spc9 inc hl
        db #fd:inc l
        db #fd:dec l
        jr nz,sf3spc1
sf3spc7 ld a,32
        db #fd:sbc l
        ld (syslstobj),a
        ret

;### SF3USB -> prepares USB drive info box
sf3usb  ld e,54
        ld hl,usbinptxt
        ld ix,usbinpobj
sf3usb1 push hl
        ld hl,srcsf3txt
        call sf3red
        pop de
        ret c
        ld hl,srcsf3txt
        db #dd:ld l,128
        ld bc,2048-1
        push de
        call sf3txt
        pop hl
        xor a
        ld (de),a
        jp inppre

;### SF3MOS -> prepares mouse info box
sf3mos  ld e,21
        ld hl,mosinptxt
        ld ix,mosinpobj
        jr sf3usb1

;### SF3LIN -> reads one textline from the SF3 buffer
;### Input      HL=buffer, DE=destination, BC=max length
;### Output     HL=next buffer pos, DE=next destination pos, BC=remaining length
;###            CF=1 -> destination full
;###            ZF=1 -> end reached
;### Destroyed  AF
sf3lin  ld a,(hl)
        inc hl
        cp 10
        ret z
        dec hl
sf3lin1 ld a,c
        or b
        scf
        ret z
        ld a,(hl)
        ldi
        cp 13
        jr nz,sf3lin1
        ld a,c
        or b
        scf
        ret z
        ld a,10
        ld (de),a
        inc de
        dec bc
        or a
        ret

;### SF3TXT -> reads multiple textlines from the SF3 buffer
;### Input      HL=buffer, DE=destination, BC=max length, IXL=max number of lines
;### Output     HL=next buffer pos, DE=next destination pos, BC=remaining length
;###            CF=1  -> destination full
;###            IXL=0 -> all lines read
;### Destroyed  AF,BC
sf3txt  call sf3lin
        ret c
        ret z
        db #dd:dec l
        jr nz,sf3txt
        ret


;==============================================================================
;### DATA-AREA ################################################################
;==============================================================================

App_BegData

prgicn16c db 12,24,24:dw $+7:dw $+4,12*24:db 5
db #88,#88,#88,#88,#88,#18,#81,#88,#88,#88,#88,#88,#88,#88,#88,#88,#11,#11,#11,#11,#88,#88,#88,#88,#88,#11,#88,#11,#00,#00,#00,#CC,#11,#88,#11,#88,#81,#1F,#11,#00,#00,#C0,#0C,#00,#CC,#11,#F1,#18
db #81,#11,#10,#0C,#CC,#D0,#CC,#DC,#CC,#11,#11,#18,#88,#11,#00,#C0,#0D,#CC,#CC,#CC,#DC,#C1,#11,#88,#88,#10,#00,#0C,#CC,#CC,#CC,#CC,#DD,#C1,#11,#88,#88,#10,#CC,#CC,#CD,#DC,#CD,#DC,#1D,#C1,#11,#88
db #81,#00,#CD,#CC,#DD,#DD,#DD,#DD,#CD,#1D,#11,#18,#81,#0C,#00,#CD,#CC,#11,#1D,#DD,#DD,#DD,#11,#18,#11,#CC,#0D,#CD,#D1,#00,#D1,#DD,#DD,#1D,#11,#11,#81,#00,#DC,#DD,#D1,#00,#D1,#1D,#1D,#D1,#11,#1D
db #11,#C0,#0C,#CD,#D1,#DD,#11,#11,#D1,#D1,#11,#11,#11,#C0,#CC,#CD,#DD,#11,#11,#11,#DD,#1D,#11,#11,#81,#1C,#CC,#DD,#DD,#D1,#11,#11,#1D,#11,#11,#18,#81,#1D,#CC,#D1,#D1,#D1,#11,#11,#D1,#D1,#11,#18
db #88,#11,#D1,#DD,#1D,#1D,#D1,#1D,#11,#11,#11,#88,#88,#11,#1D,#11,#D1,#D1,#DD,#11,#D1,#11,#11,#88,#88,#81,#11,#1D,#11,#D1,#11,#11,#11,#11,#18,#88,#88,#81,#11,#11,#11,#11,#11,#11,#11,#11,#18,#88
db #88,#1F,#11,#11,#11,#11,#11,#11,#11,#11,#F1,#88,#88,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#88,#88,#81,#18,#88,#11,#11,#11,#11,#88,#81,#18,#88,#88,#88,#88,#88,#88,#11,#D1,#88,#88,#88,#88,#88

;### Verschiedenes
prgmsginf1 db "SYMBiFACE III Monitor",0
prgmsginf2 db " Version 1.0 (Build 190325pdt)",0
prgmsginf3 db " Copyright <c> 2019 SymbiosiS",0

prgwintit   db "SYMBiFACE III Monitor",0
olewintit   db "SF3 OLED Display",0

prgtxtok    db "Ok",0
prgtxtcnc   db "Cancel",0
prgtxtyes   db "Yes",0
prgtxtno    db "No",0

;### Status
statxttxb   db "Power Supply",0
statxttxc   db "RTC battery",0
statxttxd   db "MPU temperatur",0

statxttxa   db "SYMBiFACE 3",0
statxttxa0  db "FOUND!",0
statxttxa1  db "NOT FOUND",0

statxttxb0  db " -.---V",0
statxttxc0  db " -.---V",0
statxttxd0  db " --.--C",0

butctltxa   db "Refresh",0
butctltxb   db "Beep!",0
butctltxc   db "OLED...",0

stactltxte  db "Measurements",0

tabctltxt1  db "Specifications",0
tabctltxt2  db "USB drive",0
tabctltxt3  db "Mouse",0

;### infos
spclsttxt   ds 2048
usbinptxt   ds 2048
mosinptxt   ds 2048

srcsf3txt   ds 2048

lstemptxt   db 0

;### OLED
oletxttxt   db "Text",0
oletxtbuf   ds 12+1

olepostxt1  db "display in upper row",0
olepostxt2  db "display in lower row",0
olepostxt3  db "define position and font",0

olexpstxt   db "X (0-127)",0
oleypstxt   db "Y (0-31)",0
olexpsbuf   db "0",0,0,0
oleypsbuf   db "0",0,0

olefnttxt1  db "10px",0
olefnttxt2  db "18px",0
olefnttxt3  db "26px",0

butoletxa   db "Print",0
butoletxb   db "CLS",0
butoletxc   db "Close",0


;==============================================================================
;### TRANSFER-AREA ############################################################
;==============================================================================

App_BegTrns
;### PRGPRZS -> stack for application process
        ds 64
prgstk  ds 6*2
        dw prgprz

App_PrcID   db 0
App_MsgBuf  ds 14


;### INFO-WINDOW ##############################################################

prgmsginf  dw prgmsginf1,4*1+2,prgmsginf2,4*1+2,prgmsginf3,4*1+2,prgicnbig

;### OLED-WINDOW ##############################################################

olewindat dw #1501,0,82,28,124,109,0,0,124,109,1,1,1000,1000,prgicnsml,olewintit,0,0,olewingrp,0,0:ds 136+14
olewingrp db 16,0:dw olewinobj,0,0,0,0,0,0
olewinobj
dw     00,  255*256+00, 2,             0,     0,  1000,  1000, 0    ;00=Hintergrund
dw      0,  255*256+ 1, oletxtdsc,     4,     6,    24,     8, 0    ;01=description text
dw      0,  255*256+32, oletxtinp,     30,    4,    90,    12, 0    ;02=input       text
dw      0,  255*256+18, oleposobj1,    4,    20,    116,    8, 0    ;03=radio "upper row"
dw      0,  255*256+18, oleposobj2,    4,    30,    116,    8, 0    ;04=radio "lower row"
dw      0,  255*256+18, oleposobj3,    4,    40,    116,    8, 0    ;05=radio "defined"

dw      0,  255*256+ 1, olexpsdsc,     14,   52,    40,     8, 0    ;06=description xpos
dw      0,  255*256+32, olexpsinp,     58,   50,    30,    12, 0    ;07=input       xpos
dw      0,  255*256+ 1, oleypsdsc,     14,   66,    40,     8, 0    ;08=description ypos
dw      0,  255*256+32, oleypsinp,     58,   64,    30,    12, 0    ;09=input       ypos

dw      0,  255*256+18, olefntobj1,    14,   80,    34,    8, 0     ;10=radio font1
dw      0,  255*256+18, olefntobj2,    48,   80,    34,    8, 0     ;11=radio font2
dw      0,  255*256+18, olefntobj3,    82,   80,    34,    8, 0     ;12=radio font3

dw oleprt,  255*256+16, butoletxa,     4,    93,    34,    12, 0    ;??=button      print
dw oleclr,  255*256+16, butoletxb,    40,    93,    34,    12, 0    ;??=button      clear
dw oleclo,  255*256+16, butoletxc,    86,    93,    34,    12, 0    ;??=button      close

oletxtdsc   dw oletxttxt,2+4
oletxtinp   dw oletxtbuf,0,0,0,0,12,0

olexpsdsc   dw olexpstxt,2+4
oleypsdsc   dw oleypstxt,2+4
olexpsinp   dw olexpsbuf,0,0,0,1,3,0
oleypsinp   dw oleypsbuf,0,0,0,1,2,0

oleposobj1  dw olepossta,olepostxt1,256*0+2+4,oleposkrd
oleposobj2  dw olepossta,olepostxt2,256*1+2+4,oleposkrd
oleposobj3  dw olepossta,olepostxt3,256*2+2+4,oleposkrd
oleposkrd   dw -1,-1
olepossta   db 0

olefntobj1  dw olefntsta,olefnttxt1,256*0+2+4,olefntkrd
olefntobj2  dw olefntsta,olefnttxt2,256*1+2+4,olefntkrd
olefntobj3  dw olefntsta,olefnttxt3,256*2+2+4,olefntkrd
olefntkrd   dw -1,-1
olefntsta   db 0


;### MAIN-WINDOW ##############################################################

prgwindat dw #1501,0,60,5,180,143,0,0,180,143,1,1,1000,1000,prgicnsml,prgwintit,0,0,prgwingrp,0,0:ds 136+14
prgwingrp db 15,0:dw prgwinobj,0,0,0,0,0,0
prgwinobj
dw     00,  255*256+00, 2,             0,     0,  1000,  1000, 0    ;00=Hintergrund
dw      0,  255*256+ 1, stactltxa,     4,     4,   112,     8, 0    ;01=description status
dw      0,  255*256+ 1, stactltxa0,  116,     4,    60,     8, 0    ;02=display     status
dw      0,  255*256+ 3, stactlfra,     1,    16,   125,    48, 0    ;03=frame       measurements
dw      0,  255*256+ 1, stactltxb,     9,    28,    56,     8, 0    ;04=description power
dw      0,  255*256+ 1, stactltxc,     9,    38,    56,     8, 0    ;05=description rtc
dw      0,  255*256+ 1, stactltxd,     9,    48,    56,     8, 0    ;06=description temp
dw      0,  255*256+ 1, stactltxb0,   89,    28,    30,     8, 0    ;07=display     power
dw      0,  255*256+ 1, stactltxc0,   89,    38,    30,     8, 0    ;08=display     rtc
dw      0,  255*256+ 1, stactltxd0,   89,    48,    30,     8, 0    ;09=display     temp
dw butref,  255*256+16, butctltxa,   126,    19,    50,    12, 0    ;10=button      refresh
dw butbep,  255*256+16, butctltxb,   126,    34,    50,    12, 0    ;11=button      beep
dw butole,  255*256+16, butctltxc,   126,    49,    50,    12, 0    ;12=button      oled
dw prgtab,  255*256+20, tabctlobj,     0,    68,   180,    11, 0    ;13=tab         details
prgwinobj1
dw      0,  255*256+41, syslstobj,     4,    82,   172,    58, 0    ;14=liste

;### status control data
tabctlobj   db 3,2+4+48+64
            db 0:dw tabctltxt1:db -1:dw tabctltxt2:db -1:dw tabctltxt3:db -1


stactltxa   dw statxttxa ,2+4
stactltxa0  dw statxttxa1,2+12+256
stactltxb   dw statxttxb ,2+4
stactltxc   dw statxttxc ,2+4
stactltxd   dw statxttxd ,2+4
stactltxb0  dw statxttxb0,2+12+256+128
stactltxc0  dw statxttxc0,2+12+256+128
stactltxd0  dw statxttxd0,2+12+256+128
stactlfra   dw stactltxte,2+4

;### system specifications
syslstobj   dw 1, 0,syslstlst,0,2,syslstrow,0, 1
syslstrow   dw 0, 40,0,0
            dw 0,120,0,0
syslstlst   ds 6*32

;### usb drive
usbinpobj   dw usbinptxt,0,0,0
            dw 0                        ;length
            dw 2048,1,0,0
            dw 0                        ;number of lines
            dw -1,128,-8,0,usbinpobj,0,0,0,0,3,0,0,0,0
            ds 2*128

;### mouse
mosinpobj   dw mosinptxt,0,0,0
            dw 0                        ;length
            dw 2048,1,0,0
            dw 0                        ;number of lines
            dw -1,128,-8,0,mosinpobj,0,0,0,0,3,0,0,0,0
            ds 2*128
