getBlockAddress: 
            ld a,d
            and 00000111b
            rra
            rra
            rra
            rra

            or e
            ld e,a
            ;---
            ld a,d
            and 00011000b
            or  01000000b
            ld d,a
ret             


;--------------------------------------------------------------------------
;d - y , e - x
;hl - sprites or chars
;a - sprite index
drawBlock:
            push de
            push af
            call getBlockAddress
            pop af

            ;ld hl, 0x3c00 
            ;add a, 32
          
            push hl
            ld l,a
            ld h,0
            add hl,hl  ;multiply index by 8 
            add hl,hl
            add hl,hl
            ld b,h
            ld c,l

            pop hl
            
            add hl, bc



           ld b, 8
 ;copy 8x8 block to screen
  ccloop   ld a,(hl) 
           ;ld a, (de)
           ;xor (hl)
           ld (de),a
           inc d
           inc hl
           djnz ccloop


;--------------
           pop de


           ld b, e ;x
           ld l, d ;y
           ld h, 0
           add hl, hl ;multiply y by 32
           add hl, hl
           add hl, hl
           add hl, hl
           add hl, hl

           ld d,h
           ld e,l
           ld hl,ATTRIBUTES

           add hl,de
           ld e, b
           ld d, 0
           add hl, de

           ld a, (defAttr)
           ld (hl), a
ret
