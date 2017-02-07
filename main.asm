org 25000


menuscreen
di
ld a, 40; ink + paper*8
ld (23693),a
call 3503

ld a,5              ; border
call 8859  


ld hl,mapI
ld (hl), 0
ld hl, levels ;lets point to the beginning of levels data
ld (map),hl


ld hl, defAttr
ld (hl), 13;paper blue, ink cyan
ld hl, spr ; start of the map
ld b, 5
call drawmap


ld hl, defAttr
ld (hl),00101001b


;lets print something
       ld hl, menuText1  
       ld d, 11 ;y
       ld e, 7 ;x
       call printLine

       ld hl, menuText2
       ld d, 13 ;y
       ld e, 7 ;x
       call printLine

       ld hl, menuText3
       ld d, 14 ;y
       ld e, 7 ;x
       call printLine

       ld hl, menuText4
       ld d, 15 ;y
       ld e, 7 ;x
       call printLine

       ld hl, menuText5
       ld d, 22 ;y
       ld e, 12 ;x
       call printLine
       
       ld hl, menuText6
       ld d, 23 ;y
       ld e, 12 ;x
       call printLine


       ei

       ld hl,23560         ; LAST K system variable.
       ld (hl),0           ; put null value there.
loop   ld a,(hl)           ; new value of LAST K.
       cp 0                ; is it still zero?
       jr z,loop           ; yes, so no key pressed.
       

       cp 'k' 
       jr z, setKeys
       cp 'j'
       jr z, setJoy
       cp 'c'
       jr z, setCur
    
      jp loop

setKeys ld hl,useJoy
        ld (hl), 0
        ld hl,useCursor
        ld (hl), 0
jr init

setJoy ld hl,useJoy
       ld (hl), 1
       ld hl, useCursor
       ld (hl), 0

       jr init

setCur ld hl, useCursor
       ld (hl),1
       ld hl,useJoy
       ld (hl), 0

init
    di
    ld hl, monsterGotMe
    ld (hl), 0

    ld hl, (map)
    ld de, TILEDATASI
    add hl, de

    ld de, startPos
    ld a, (hl)
    ld (de), a
    ld (px), a
    inc hl
    inc de
    ld a, (hl)
    ld (de), a
    ld (py), a
    inc hl


    ld de, exitPos
    ld a, (hl)
    ld (de), a
    inc hl
    inc de

    ld a, (hl)
    ld (de), a
    inc hl

    ld a, (hl)
    ld de, maxmon
    ld (de),a
    inc hl

    ld b, a
    ld de, mdata

rep ld a, (hl) ;load up monster coords
    ld (de), a
    inc de
    inc hl
    ld a, (hl)
    ld (de), a
    inc de
    inc hl
    inc de;go past tim
    djnz rep


    ld hl, restart
    ld (hl), 0

cont
    call playsound

    ld hl, defAttr
    ld (hl), 13;paper blue, ink cyan
    ld hl, (map) ; start of the map
    ld b, MAXSCREENY
    call drawmap

mainloop
       
       ld hl, defAttr
       ld (hl), 00001110b

       ;clear player position
       ld a, (py) ;y
       ld d, a
       ld a, (px) ;x
       ld e, a

       ld a, 5
       ld hl,sprs
       call drawBlock



       ;--- clear old monster positions
       ld hl, defAttr
       ld (hl), 00001110b
       ld a,(maxmon)
       ld b, a ;monster count
       ld a,0
clmon       
       
       push bc
       ld b, a
       inc a
       push af
       call getMonster
       pop af
       pop bc
       push af


       ld e, (hl)
       inc hl
       ld d, (hl)
       dec hl
       push hl
       push bc

       ld hl, sprs
       ld a, EMPTYTILE
       call drawBlock

       pop bc
       pop hl

       pop af

    djnz clmon
       ;----------------

       ld a, (useJoy)
       cp 1
       jr z, readJoy

       ld a, (useCursor)
       cp 1
       jr z, readC

       call readKeys

       jp proceed

readC       
       call readKeysC
     
       jp proceed
readJoy
       call joycon
proceed
       ;change ink to yellow
       ld hl, defAttr
       ld (hl), 14

       ;draw player
       ld a, (py) ;y
       ld d,a
       ld a, (px) ;x
       ld e,a
       ld a, 1 ;smiley
       ld hl, sprs
       call drawBlock


       ld a, (maxmon)
       ld b, a
       ld a, 0
 monAI;--------- 
       ;count till MONSTERSP before getting new direction
       push bc
       ld b, a
       inc a
       push af
       call getMonster
       

       inc hl ;y
       inc hl ;monster timer


       ld a, MONSTERSP
       cp 0
       jr z, reset
       inc (hl)
       ld a, (hl)
       cp MONSTERSP
       jr z, reset
       dec hl
       dec hl;restore hl back to x
       jr stay

reset  ld (hl), 0
       dec hl ;restore hl if jumped here
       dec hl

       push hl
       ld hl,fail ;reset fail count
       ld (hl), 0
       pop hl

       ;lets compare player and monster x
       ld a,(px)
       ld b,a
       ld a,(hl)
       cp b
       jr z, checky
       jr nc, goL

       call movemonsterright
       jr checky
goL    call movemonsterleft

checky ld a,(py)
       ld b, a
       inc hl;y
       ld a,(hl)
       dec hl
       cp b
       jr z, check
       jr nc, goU

       call movemonsterdown
       jr check;stay
goU    call movemonsterup

check  ld a,(fail)
       cp 0
       jr z, stay

call randomDirection

stay:
      ;----------------

       ;call setxy      ; set up x/y coords.
       ld a,0        ; draw monster
       ld e,(hl)
       inc hl
       ld d,(hl)
       dec hl
       push hl
       ld hl, defAttr
       ld (hl), 10
       ld hl, sprs
       call drawBlock          ; 
       pop hl
       ;--------------

       ld a,(px)
       cp (hl)
       call z, checkMonY

       pop af ;restore a & b for the loop
       pop bc
djnz monAI

       ;draw exit
       ld hl, defAttr
       ld (hl), 10001110b;flashing yellow on a blue bg
       
       ld hl, exitPos

       ld e, (hl) ;x coordinate.
       inc hl
       ld d,(hl)   ; y
       ld hl, sprs
       ld a,2        
       call drawBlock ; display it.


       
       call checkExit

       
       call delay      ; want a delay.

       ld a, (monsterGotMe)
       cp 1
       jp z, init


       ld a, (restart);restart if victorious
       cp 0
       jp nz, init
       jp mainloop

ret

;--------------------------------------
;textline in HL
;d-y, e -x
printLine

       ld b, (hl)
       inc hl
       ;ld d, 0
nextc: 
       ld a,(hl)
       inc hl

       push hl
       push bc
       push de

       ld hl, 0x3c00 ;chars in ROM
       call drawBlock

       pop de
       pop bc
       pop hl

       inc e


       djnz nextc

ret


;-----------------------------------------
 readKeys;check keys yuiop
       ld bc, 57342
       in a, (c)
       rra
       push af
       call nc,right ;p
       pop af
       rra 
       call nc,left  ;o
       xor a
       ld bc, 64510 ;trewq
       in a, (c)
       rra
       call nc,up ;q
       xor a
       ld bc, 65022 ;gfdsa
       in a, (c)
       rra
       call nc,down ;a
ret
;-----------------------------------------
 readKeysC
       ld bc, 61438 ;09876
       in a, (c)
       
       push af
       and 00010000b
       call z, down ;6
       pop af
       push af
       and 00001000b
       call z, up  ;7
       pop af
       and 00000100b
       call z,right ;8

       ld bc,0xf7fe ;
       in a, (c)
       and 00010000b
       call z,left ;5
ret



;-----------------------------------------
randomDirection
    push hl
    call random
    pop hl
    and 10
    cp 2
    call z, movemonsterleft
    cp 0
    call z, movemonsterup
    cp 8
    call z, movemonsterright
    cp 10
    call z, movemonsterdown
ret
;-------------------------------------------
checkExit

     ld hl, exitPos
     ld a, (px)
     cp (hl) ;x
     ret nz
     inc hl
     ld a, (py)
     cp (hl) ;y
     ret nz

     ld hl, mapI
     inc (hl) ;increase level index
     ld a,(hl)
     cp MAXLEVEL     ;is it > maxLevel
     jr z, gogo
     jr ext
gogo call playsound2
     jp menuscreen 
   
ext ld hl, (map)
    ld de, LEVSIZE
    add hl, de 
    ld (map), hl

    ld hl,restart
    ld (hl), 1
ret
;--------------------------------------
checkMonY
    ld a, (py)
    inc hl
    cp (hl)
    dec hl
    ret nz
    
    push hl
    ld hl, monsterGotMe
    ld (hl), 1
    pop hl

    
ret

;--------------------
incFailCounter
    push hl 
    ld hl, fail
    inc (hl)
    pop hl
ret

;----------------------------------------
;mdata's in hl
movemonsterleft

    ld a, (hl)
    and a
    ret z

    inc hl;y
    ld a, (hl)
    ld b, a
    dec hl
    ld a, (hl)
    dec a ;x--
    push hl
    call fetchmapcell
    pop hl
    cp EMPTYTILE
    jr nz, epicfaill ;can go on anything but ' '
    dec (hl)
    jr exitl

epicfaill call incFailCounter
exitl ret
;------------------------------------------
movemonsterright

    ld a, (hl)
    cp MAXSCREENX-1
    ret z

    inc hl;y
    ld a, (hl)
    ld b, a
    dec hl
    ld a, (hl)
    inc a ;x++
    push hl
    call fetchmapcell
    pop hl
    cp EMPTYTILE
    jr nz, epicfailr ;can go on anything but ' '
    inc (hl)
    jr exitr

epicfailr call incFailCounter
exitr ret

;-------------------------------------
movemonsterup

    inc hl
    ld a,(hl);y
    and a
    ret z
      
    ld a, (hl)
    dec a ;y--
    ld b, a
    dec hl
    ld a, (hl)
    push hl
    call fetchmapcell
    pop hl
    cp EMPTYTILE

    jr nz, epicfailu ;can go on anything but ' '
    inc hl
    dec (hl) ;y
    dec hl
    jr exitu

epicfailu call incFailCounter
exitu ret
;----------------------------------------
movemonsterdown

    inc hl
    ld a,(hl);y
    cp MAXSCREENY-1; maxY
    ret z
      
    ld a, (hl)
    inc a ;y++
    ld b, a
    dec hl
    ld a, (hl)
    push hl
    call fetchmapcell
    pop hl
    cp EMPTYTILE
    jr nz, epicfaild ;can go on anything but ' '
    inc hl
    inc (hl) ;y
    dec hl
    jr exitd

epicfaild call incFailCounter

exitd ret

;-----------------------------------------

delay  ld b, 4        ; length of delay.
delay0 halt            ; wait for an interrupt.
       djnz delay0     ; loop.
       ret
;-------------------------------------
left 
       ld a, (px)
       and a
       ret z

       ld a, (py)
       ld b, a
       ld a, (px)
       dec a ;x--
       call fetchmapcell
       cp EMPTYTILE
       ret nz ;can go on anything but ' '


       ld hl, px
       dec (hl)
ret
;-------------------------------------
right
       ld a, (px)
       cp MAXSCREENX-1
       ret z

       ld a, (py)
       ld b, a
       ld a, (px)
       inc a ;x++
       call fetchmapcell
       cp EMPTYTILE
       ret nz ;can go on anything but ' '


       ld hl, px
       inc (hl)
ret
;-------------------------------------
down
       ld a, (py)
       cp MAXSCREENY-1
       ret z; out of screen
    
       ld a, (py)
       inc a ;y++
       ld b, a
       ld a, (px)
       call fetchmapcell
       cp EMPTYTILE
       ret nz ;can go on anything but ' '

       ld hl, py
       
       inc (hl)
ret
;-------------------------------------
up
      ld a, (py)
      and a
      ret z
      
      ld a, (py)
      dec a ;y--
      ld b, a
      ld a, (px)
      call fetchmapcell
      cp EMPTYTILE
      ret nz ;can go on anything but ' '


      ld hl, py

      dec (hl)
ret
;-----------------------------------
;restore the cell from px py to A
;a-x,b-y
fetchmapcell 
             ld l,b
             ld h,0
             add hl,hl ;b*32
             add hl,hl
             add hl,hl
             add hl,hl
             add hl,hl

             ld de, (map);
             add hl, de

             ld d, 0
             ld e, a
             add hl, de
    noYs     ld a, (hl) ;
ret


;-------------------------------------
;hl - map pointer
;b - map height
drawmap  
         ld d, b ; store map height in d
         push de
         draw2  
            ld c, b; save b index in c
            ld b, MAXSCREENX
            draw 
                pop de 
                ld a, d; ;restore map heigt
                push de
                sub c
                ld d, a

                ld a, MAXSCREENX ;x
                sub b
                ld e, a

                ld a, (hl) 
                push hl
                push bc

                ld hl, sprs
                
                call drawBlock

                pop bc
                pop hl

                inc hl

            djnz draw

            ld b, c ;restore y index   

        djnz draw2
        pop de
        ret

;-------------------------------------
setxy  
       ld a,22         ; ASCII control code for AT.
       rst 16          ; print it.
       inc hl
       ld a,(hl)   ; vertical position.
       rst 16          ; print it.
       dec hl
       ld a,(hl)   ; x coordinate.
       rst 16          ; print it.
ret
;------------------------------------
; Simple pseudo-random number generator.
; Steps a pointer through the ROM (held in seed), returning
; the contents of the byte at that location.

random ld hl,(seed)        ; Pointer
       ld a,h
       and 31              ; keep it within first 8k of ROM.
       ld h,a
       ld a,(hl)           ; Get "random" number from location.
       inc hl              ; Increment pointer.
       ld (seed),hl
       ret
;------------------------------------
joycon ld bc,31            ; Kempston joystick port.
       in a,(c)            ; read input.
       and 2               ; check "left" bit.
       call nz,left        ; move left.
       in a,(c)            ; read input.
       and 1               ; test "right" bit.
       call nz,right        ; move right.
       in a,(c)            ; read input.
       and 8               ; check "up" bit.
       call nz,up        ; move up.
       in a,(c)            ; read input.
       and 4               ; check "down" bit.
       call nz,down        ; move down.
       in a,(c)            ; read input.
       and 16              ; try the fire bit.
       call nz,down        ; fire pressed.
ret
;-----------------------------------
playsound
    ld b, 200
    ld hl, 200


soundloop   push bc
            push hl
            ld de, 1
            call 949

            pop hl
            dec hl
            pop bc
            
            djnz soundloop

ret
;-----------------------------------
playsound2
ld c, 4
rstrt      ld b, 200
           ld hl, 200


sndloop   push bc
            push hl
            ld de, 10
            call 949

            pop hl
            dec hl
            pop bc
            
            djnz sndloop
dec c
ld a, c
cp 0
jr nz, rstrt

ret

;------------------------------------
;b - monster num, out - adress in hl
getMonster
       ld hl, mdata
       ld de, 3 ;size of monster record

       ld a, b
       cp 0
       jr z,tt
    ci add hl, de
       djnz ci
tt ret
;------------------------------------


include "gfx.asm"


MAXSCREENX  equ 32
MAXSCREENY  equ 24

MENUTXTLEN  equ 85
MENUTXTLINEL equ 17

MONSTERSP   equ 4

EMPTYTILE   equ 5

MAXLEVEL    equ 5
TILEDATASI  equ 768
LEVSIZE     equ 781
ATTRIBUTES  equ 0x5800



map    defw 0 ;pointer to the current map
mapI   defb 0 ;map index
useJoy defb 0
useCursor defb 0
seed   defw 0
px     defb 1
py     defb 0
maxmon defb 0
fail   defb 0
restart defb 0
defAttr defb 13 ;default attribute color
monsterGotMe defb 0

mdata   defb 27, 2, 0 ;x,y,timer
        defb 0, 0, 0 ;x,y,timer
        defb 0, 0, 0
        defb 0, 0, 0

exitPos defb 0,0
startPos defb 0,1

menuText1 defb 16, "SELECT CONTROLS:"
menuText2 defb 16, " k - qaop  keys "
menuText3 defb 16, " c - arrow keys "
menuText4 defb 16, " j - kempston   "

menuText5 defb 6, "jrs0ul"
menuText6 defb 6, " 2016 "


sprs   defb 10111101b
       defb 11111111b
       defb 10011001b
       defb 11011011b
       defb 10111101b
       defb 11011011b
       defb 01100110b
       defb 00111100b
       
       defb 00111100b
       defb 01111110b
       defb 11111111b
       defb 11011011b
       defb 11111111b
       defb 11000011b
       defb 01100110b
       defb 00111100b

       defb 11111111b
       defb 10000001b
       defb 10111101b
       defb 10100101b
       defb 10100101b
       defb 10111101b
       defb 10000001b
       defb 11111111b

       defb 11111111b
       defb 11011111b
       defb 11111111b
       defb 11110111b
       defb 10111111b
       defb 11011111b
       defb 10101011b
       defb 11111111b

       defb 00011000b
       defb 00110100b
       defb 01111010b
       defb 11111101b
       defb 11111101b
       defb 01111010b
       defb 00110100b
       defb 00011000b

       defb 00000000b
       defb 00000000b
       defb 00000000b
       defb 00000000b
       defb 00000000b
       defb 00000000b
       defb 00000000b
       defb 00000000b

       defb 00000011b
       defb 00001111b
       defb 00011011b
       defb 00111111b
       defb 01101111b
       defb 01110111b
       defb 01111111b
       defb 11111111b

       defb 11000000b
       defb 11110000b
       defb 11011000b
       defb 11111100b
       defb 10101110b
       defb 11110110b
       defb 10111110b
       defb 11111111b

       defb 11111111b
       defb 11111110b
       defb 11011110b
       defb 11111110b
       defb 11101100b
       defb 10111000b
       defb 11110000b
       defb 11000000b

       defb 11111111b
       defb 01111111b
       defb 01011111b
       defb 01101111b
       defb 00111111b
       defb 00011111b
       defb 00001111b
       defb 00000011b




levels 
    defb  3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3
    defb  3, 5, 5, 5, 5, 5, 5, 3, 5, 5, 5, 5, 5, 5, 3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 3
    defb  3, 5, 5, 5, 5, 5, 5, 3, 5, 5, 5, 5, 5, 5, 3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 3
    defb  3, 5, 5, 5, 5, 5, 5, 3, 3, 5, 5, 5, 5, 5, 3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 3
    defb  3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 3, 4, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 3
    defb  3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 3, 4, 4, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 3
    defb  3, 4, 5, 5, 4, 5, 5, 3, 3, 5, 5, 5, 5, 5, 3, 3, 3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 3
    defb  3, 4, 6, 7, 4, 5, 5, 3, 5, 5, 5, 5, 5, 5, 3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 3
    defb  3, 3, 3, 3, 3, 7, 5, 3, 5, 5, 5, 5, 5, 5, 3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 3
    defb  3, 3, 3, 3, 3, 3, 3, 3, 5, 5, 5, 5, 5, 5, 3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 3
    defb  3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 3, 5, 5, 5, 5, 3, 3, 3, 5, 5, 5, 5, 5, 6, 7, 5, 5, 3
    defb  3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 3, 5, 5, 5, 5, 5, 3, 5, 5, 5, 5, 3, 3, 3, 3, 3, 3, 3
    defb  3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 3
    defb  3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 3
    defb  3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 3
    defb  3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 3
    defb  3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 3, 5, 5, 5, 5, 5, 3, 3, 3, 3, 3, 5, 5, 5, 5, 5, 5, 3
    defb  3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 3, 5, 5, 5, 5, 5, 3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 3
    defb  3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 3, 5, 5, 5, 5, 5, 3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 3
    defb  3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 3, 5, 5, 5, 5, 5, 3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 3
    defb  3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 3, 5, 5, 5, 5, 5, 3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 3
    defb  3, 5, 5, 5, 5, 5, 5, 5, 4, 5, 5, 5, 5, 5, 3, 4, 5, 5, 5, 5, 3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 3
    defb  3, 5, 5, 6, 7, 5, 5, 6, 3, 7, 5, 5, 5, 5, 3, 4, 4, 5, 5, 5, 3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 3
    defb  3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3

       defb 2, 6; start 
       defb 21,22;exit
       defb 1;monster count
       defb 27, 6;monster coords
       defb 0, 0
       defb 0, 0
       defb 0, 0


 
    defb  3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3
    defb  3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 3, 5, 5, 5, 5, 5, 5, 5, 3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 3
    defb  3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 3, 5, 5, 5, 5, 5, 5, 5, 3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 3
    defb  3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 3, 5, 5, 5, 5, 5, 5, 5, 3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 3
    defb  3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 3, 5, 5, 5, 5, 5, 5, 5, 3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 3
    defb  3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 3, 5, 5, 5, 5, 5, 5, 5, 3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 3
    defb  3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 3, 5, 5, 5, 5, 5, 5, 5, 3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 3
    defb  3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 3, 3, 5, 5, 5, 5, 5, 5, 5, 3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 3
    defb  3, 5, 5, 5, 5, 5, 5, 5, 5, 3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 3
    defb  3, 5, 5, 5, 5, 5, 5, 5, 5, 3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 3, 4, 5, 5, 5, 5, 5, 5, 5, 5, 5, 3
    defb  3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 3, 3, 5, 5, 5, 5, 5, 5, 5, 3, 3, 4, 5, 5, 5, 5, 5, 5, 5, 5, 3
    defb  3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 3, 5, 5, 5, 5, 5, 5, 5, 3, 5, 3, 4, 5, 5, 5, 5, 5, 5, 5, 3
    defb  3, 5, 5, 4, 4, 5, 5, 5, 5, 5, 5, 5, 3, 5, 5, 5, 5, 5, 5, 5, 3, 5, 5, 3, 5, 5, 5, 5, 5, 5, 5, 3
    defb  3, 3, 3, 3, 3, 3, 3, 3, 5, 5, 5, 5, 3, 5, 5, 5, 5, 5, 5, 5, 3, 5, 5, 5, 5, 5, 5, 5, 5, 4, 5, 3
    defb  3, 5, 5, 5, 5, 5, 5, 3, 5, 5, 5, 5, 3, 4, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 3, 3, 3, 5, 3, 3, 3, 3
    defb  3, 5, 5, 5, 5, 5, 5, 3, 5, 5, 5, 5, 3, 3, 3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 3, 5, 5, 5, 5, 5, 5, 3
    defb  3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 3, 5, 5, 5, 5, 5, 5, 3
    defb  3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 3, 5, 5, 5, 5, 5, 5, 3
    defb  3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 3, 5, 5, 5, 5, 5, 5, 3
    defb  3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 4, 3, 4, 5, 5, 5, 5, 5, 5, 5, 6, 3, 5, 5, 5, 5, 5, 5, 3
    defb  3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 4, 3, 4, 4, 5, 5, 5, 5, 5, 6, 3, 3, 5, 5, 5, 5, 5, 5, 3
    defb  3, 5, 5, 5, 5, 5, 5, 3, 5, 5, 4, 5, 4, 4, 3, 4, 4, 5, 5, 5, 5, 6, 3, 3, 3, 5, 5, 5, 5, 5, 5, 3
    defb  3, 5, 5, 5, 5, 5, 5, 3, 5, 4, 4, 4, 4, 4, 3, 4, 4, 4, 5, 5, 6, 3, 3, 3, 3, 5, 6, 3, 7, 5, 5, 3
    defb  3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3

       defb 2,22; start 
       defb 27,21
       defb 2;monster count
       defb 27, 10;monster coords
       defb 14, 8
       defb 0, 0
       defb 0, 0


 
    defb  5, 5, 5, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3
    defb  5, 5, 3, 3, 5, 5, 5, 5, 5, 4, 4, 4, 4, 3, 5, 5, 5, 5, 5, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 5, 3
    defb  5, 5, 3, 5, 5, 5, 5, 5, 5, 5, 4, 5, 5, 3, 5, 5, 5, 5, 3, 3, 3, 3, 3, 3, 3, 5, 5, 5, 5, 5, 5, 3
    defb  5, 5, 3, 5, 5, 5, 5, 5, 5, 5, 4, 4, 5, 3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 3
    defb  5, 5, 3, 5, 5, 5, 5, 5, 5, 5, 4, 5, 5, 3, 5, 5, 5, 5, 3, 3, 3, 3, 3, 3, 3, 5, 5, 5, 5, 5, 5, 3
    defb  5, 5, 3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 3, 5, 5, 5, 5, 5, 3, 3, 5, 5, 5, 3, 3, 3, 3, 5, 3, 3, 3
    defb  5, 5, 3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 3, 5, 5, 5, 5, 5, 3, 3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 3
    defb  5, 5, 3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 3, 5, 5, 5, 5, 5, 3, 3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 3
    defb  5, 5, 3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 3, 5, 5, 5, 5, 5, 3, 3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 3
    defb  5, 5, 3, 3, 5, 5, 3, 7, 5, 5, 5, 5, 5, 3, 3, 3, 5, 3, 3, 3, 3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 3
    defb  5, 5, 5, 5, 5, 5, 5, 3, 5, 3, 5, 3, 5, 3, 5, 5, 5, 5, 5, 3, 3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 3
    defb  5, 5, 5, 5, 5, 5, 5, 3, 3, 3, 5, 3, 3, 3, 5, 5, 5, 5, 5, 3, 3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 3
    defb  5, 5, 5, 4, 4, 5, 5, 3, 5, 5, 5, 5, 5, 3, 5, 5, 5, 5, 5, 3, 3, 5, 5, 5, 5, 5, 5, 5, 5, 4, 5, 3
    defb  6, 3, 3, 3, 3, 3, 3, 5, 5, 5, 5, 5, 5, 3, 5, 5, 5, 5, 5, 3, 3, 5, 5, 5, 5, 5, 5, 5, 5, 4, 4, 3
    defb  3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 3, 5, 5, 5, 5, 5, 3, 3, 3, 3, 3, 3, 3, 3, 5, 3, 3, 3, 3
    defb  3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 3, 3, 5, 5, 5, 5, 3, 4, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 3
    defb  3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 3, 5, 5, 5, 5, 3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 3
    defb  3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 3, 5, 5, 5, 5, 3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 3
    defb  3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 3
    defb  3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 4, 4, 5, 5, 5, 3, 5, 5, 4, 5, 5, 5, 5, 5, 5, 5, 5, 3
    defb  3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 4, 4, 4, 4, 4, 5, 5, 3, 5, 4, 4, 4, 5, 5, 5, 5, 5, 5, 5, 3
    defb  3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 3, 3, 7, 4, 4, 5, 5, 3, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 3
    defb  3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 3, 3, 3, 3, 7, 4, 4, 5, 3, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 3
    defb  3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3

       defb 1,1; start 
       defb 22,18 ;exit
       defb 3;monster count
       defb 28, 6;monster coords
       defb 14, 8
       defb 3, 7
       defb 0, 0





    defb  3, 5, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3
    defb  3, 5, 3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 3, 5, 5, 5, 5, 5, 5, 5, 3, 5, 5, 5, 5, 5, 5, 5, 3
    defb  3, 5, 3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 3, 5, 3, 5, 5, 5, 5, 5, 5, 3, 3, 5, 5, 5, 5, 5, 5, 5, 3
    defb  3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 3, 5, 3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 3
    defb  3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 3, 5, 3, 5, 5, 5, 5, 5, 5, 3, 3, 5, 5, 5, 5, 5, 5, 5, 3
    defb  3, 4, 5, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 5, 3, 5, 5, 5, 5, 5, 5, 5, 3, 5, 5, 5, 5, 5, 5, 5, 3
    defb  3, 4, 4, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 3, 5, 5, 5, 5, 5, 5, 5, 3, 5, 5, 5, 5, 5, 5, 4, 3
    defb  3, 4, 4, 4, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 3, 5, 5, 5, 5, 5, 5, 5, 3, 3, 3, 3, 3, 5, 3, 3, 3
    defb  3, 3, 3, 3, 3, 3, 3, 5, 3, 3, 3, 3, 3, 3, 3, 3, 5, 5, 5, 5, 5, 5, 5, 3, 5, 5, 5, 5, 5, 5, 5, 3
    defb  3, 5, 5, 5, 5, 5, 3, 5, 5, 5, 5, 5, 5, 5, 5, 3, 3, 3, 3, 5, 5, 3, 3, 3, 5, 5, 5, 5, 5, 5, 5, 3
    defb  3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 3, 5, 5, 5, 5, 5, 5, 5, 3, 5, 5, 5, 5, 5, 5, 5, 3
    defb  3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 5, 5, 5, 3, 5, 5, 5, 5, 5, 5, 5, 3, 4, 4, 4, 5, 4, 4, 4, 3
    defb  3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 3, 5, 5, 5, 5, 5, 5, 5, 3, 5, 5, 4, 5, 4, 5, 5, 3
    defb  3, 5, 5, 5, 5, 5, 3, 5, 5, 5, 5, 5, 5, 5, 5, 3, 5, 5, 5, 5, 5, 5, 5, 3, 5, 5, 5, 5, 5, 5, 5, 3
    defb  3, 3, 3, 3, 5, 5, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 5, 5, 3, 3, 3, 3, 3, 5, 5, 5, 5, 5, 5, 5, 3
    defb  3, 5, 5, 5, 3, 5, 5, 5, 5, 5, 4, 5, 5, 5, 5, 3, 5, 5, 5, 5, 5, 5, 5, 3, 5, 5, 5, 5, 5, 5, 5, 3
    defb  3, 5, 5, 5, 5, 5, 5, 5, 5, 4, 4, 4, 5, 5, 5, 3, 5, 5, 5, 5, 5, 5, 5, 3, 5, 5, 5, 5, 5, 5, 5, 3
    defb  3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 4, 5, 5, 5, 5, 3, 5, 5, 5, 5, 5, 5, 5, 3, 5, 5, 5, 5, 5, 5, 5, 3
    defb  3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 3, 5, 5, 5, 5, 5, 5, 5, 3, 5, 5, 5, 5, 5, 5, 5, 3
    defb  3, 3, 3, 5, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 5, 5, 5, 5, 5, 5, 5, 3, 5, 5, 5, 5, 5, 5, 5, 3
    defb  3, 5, 5, 5, 5, 5, 5, 5, 5, 3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 4, 3, 5, 5, 5, 4, 5, 5, 5, 3
    defb  3, 5, 5, 5, 5, 5, 5, 5, 5, 3, 5, 5, 5, 5, 5, 3, 4, 5, 5, 5, 5, 4, 4, 3, 5, 5, 4, 4, 4, 5, 5, 3
    defb  3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 3, 4, 4, 5, 5, 4, 4, 4, 3, 5, 4, 4, 4, 4, 4, 5, 3
    defb  3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3

       defb 1,0; start 
       defb 27,19
       defb 4;monster count
       defb 23, 3;monster coords
       defb 14, 3
       defb 5, 20
       defb 27, 17

    defb  5, 5, 5, 5, 5, 5, 5, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 5, 5, 5, 5, 3, 3, 3
    defb  5, 5, 5, 5, 5, 5, 5, 3, 5, 5, 5, 5, 5, 5, 5, 3, 5, 5, 5, 5, 5, 5, 5, 3, 5, 5, 5, 5, 5, 5, 3, 3
    defb  5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 3, 5, 5, 5, 5, 5, 5, 3, 5, 5, 5, 5, 5, 5, 5, 5, 3
    defb  5, 5, 5, 5, 5, 5, 5, 3, 5, 5, 5, 5, 5, 5, 5, 3, 5, 5, 5, 5, 5, 4, 3, 5, 5, 5, 5, 5, 5, 5, 5, 3
    defb  5, 5, 5, 5, 5, 5, 5, 3, 5, 5, 5, 5, 5, 5, 5, 3, 5, 5, 5, 5, 5, 4, 3, 5, 5, 5, 5, 5, 5, 5, 5, 3
    defb  3, 3, 5, 3, 3, 3, 3, 3, 3, 5, 3, 3, 3, 5, 5, 3, 5, 5, 5, 5, 5, 5, 3, 5, 5, 4, 5, 5, 5, 5, 5, 3
    defb  3, 5, 5, 5, 5, 5, 5, 3, 5, 5, 5, 3, 5, 5, 5, 3, 5, 5, 5, 5, 5, 5, 3, 5, 4, 4, 4, 5, 5, 5, 4, 3
    defb  3, 5, 5, 5, 5, 5, 5, 3, 5, 5, 5, 3, 5, 5, 5, 3, 5, 5, 5, 5, 5, 5, 5, 3, 3, 3, 3, 3, 5, 3, 3, 3
    defb  3, 3, 3, 3, 5, 3, 3, 3, 3, 3, 5, 3, 3, 5, 5, 3, 5, 5, 3, 3, 5, 4, 5, 3, 5, 5, 5, 5, 5, 5, 5, 3
    defb  3, 5, 5, 5, 5, 5, 5, 3, 5, 5, 5, 5, 5, 3, 3, 3, 3, 5, 3, 5, 5, 3, 3, 3, 5, 5, 5, 5, 5, 5, 5, 3
    defb  3, 5, 5, 5, 5, 5, 5, 3, 5, 5, 5, 5, 5, 5, 5, 3, 5, 5, 3, 5, 5, 5, 5, 3, 5, 5, 5, 5, 5, 5, 5, 3
    defb  3, 3, 5, 3, 3, 3, 3, 3, 3, 3, 3, 3, 5, 5, 5, 3, 5, 5, 3, 5, 5, 5, 5, 3, 4, 4, 4, 5, 4, 4, 4, 3
    defb  3, 5, 5, 5, 5, 5, 5, 3, 5, 5, 5, 5, 5, 5, 5, 3, 5, 5, 3, 5, 5, 5, 5, 3, 3, 3, 3, 5, 3, 3, 3, 3
    defb  3, 5, 5, 5, 5, 5, 4, 3, 5, 5, 5, 5, 5, 5, 5, 3, 5, 5, 3, 5, 5, 5, 5, 3, 5, 5, 5, 5, 5, 5, 5, 3
    defb  3, 3, 3, 3, 5, 5, 3, 3, 3, 3, 5, 3, 3, 3, 3, 3, 3, 5, 3, 3, 3, 5, 3, 3, 5, 5, 5, 4, 5, 5, 5, 3
    defb  3, 5, 5, 5, 3, 5, 5, 3, 5, 5, 5, 5, 5, 5, 5, 3, 5, 5, 5, 3, 5, 5, 5, 3, 5, 5, 5, 5, 5, 5, 5, 3
    defb  3, 5, 5, 5, 5, 5, 5, 3, 5, 5, 5, 5, 5, 5, 5, 3, 5, 5, 5, 3, 5, 5, 5, 3, 5, 5, 5, 5, 5, 5, 5, 3
    defb  3, 5, 5, 5, 5, 5, 5, 3, 5, 3, 5, 5, 5, 5, 5, 3, 5, 5, 5, 3, 5, 5, 5, 3, 5, 5, 5, 5, 5, 5, 5, 3
    defb  3, 5, 5, 5, 5, 5, 5, 3, 5, 3, 5, 5, 5, 5, 4, 3, 5, 5, 5, 3, 5, 5, 5, 3, 5, 5, 5, 5, 5, 5, 5, 3
    defb  3, 4, 5, 5, 5, 5, 4, 3, 5, 3, 3, 3, 3, 3, 3, 3, 5, 5, 5, 3, 5, 5, 5, 3, 5, 5, 5, 5, 5, 5, 5, 3
    defb  3, 5, 5, 5, 5, 5, 5, 3, 5, 3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 3, 5, 5, 3, 3, 5, 5, 5, 4, 5, 5, 5, 3
    defb  3, 5, 5, 5, 5, 5, 5, 3, 5, 3, 5, 5, 5, 5, 5, 4, 4, 5, 5, 3, 5, 5, 5, 5, 5, 5, 4, 4, 4, 5, 5, 3
    defb  3, 5, 5, 5, 5, 5, 5, 3, 5, 5, 5, 4, 4, 4, 4, 4, 4, 4, 5, 3, 5, 5, 5, 3, 5, 5, 5, 4, 5, 5, 5, 3
    defb  3, 6, 7, 6, 7, 6, 7, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3

       defb 3, 22; start 
       defb 27,3
       defb 4;monster count
       defb 3, 3;monster coords
       defb 11, 17
       defb 21, 12
       defb 27, 3


spr    defb 5 ,5 ,5 ,5 ,5 ,3 ,3 ,3 ,3 ,7 ,5 ,4 ,5 ,5 ,5 ,5 ,5 ,5 ,5 ,5 ,5 ,3 ,5 ,5 ,5 ,5 ,5 ,3 ,5 ,5 ,5 ,5
       defb 5 ,5 ,5 ,5 ,5 ,3 ,5 ,5 ,5 ,3 ,5 ,5 ,5 ,7 ,6 ,7 ,5 ,6 ,3 ,3 ,3 ,3 ,3 ,3 ,5 ,5 ,5 ,3 ,5 ,5 ,5 ,5
       defb 5 ,5 ,5 ,5 ,5 ,3 ,3 ,3 ,3 ,8 ,5 ,3 ,5 ,3 ,5 ,3 ,5 ,9 ,5 ,5 ,5 ,3 ,5 ,5 ,5 ,5 ,5 ,3 ,5 ,5 ,5 ,5
       defb 4 ,5 ,5 ,5 ,5 ,3 ,5 ,5 ,5 ,5 ,5 ,3 ,5 ,3 ,5 ,5 ,5 ,5 ,7 ,5 ,5 ,3 ,5 ,5 ,5 ,5 ,5 ,5 ,5 ,5 ,5 ,4
       defb 4 ,4 ,5 ,5 ,5 ,3 ,5 ,5 ,5 ,5 ,5 ,3 ,5 ,3 ,5 ,5 ,3 ,3 ,8 ,5 ,5 ,9 ,3 ,5 ,5 ,5 ,5 ,4 ,5 ,5 ,4 ,4
