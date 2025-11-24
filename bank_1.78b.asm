; MACRO.H

; Based on the 2600 macro.h file.
; Macros irrelevant to the 7800 have been removed, and the sleep macro 
; has been adapted to give accurate results on the 7800.

; Version 1.0 2019/12/11 (based on the 2600 Version 1.05, 13/NOVEMBER/2003)

; Available macros...
;   SLEEP n             - sleep for n cycles
;   SET_POINTER         - load a 16-bit absolute to a 16-bit variable

;-------------------------------------------------------------------------------
; SLEEP duration
; Original author: Thomas Jentzsch
; Inserts code which takes the specified number of cycles to execute.  This is
; useful for code where precise timing is required.
; ILLEGAL-OPCODE VERSION DOES NOT AFFECT FLAGS OR REGISTERS.
; LEGAL OPCODE VERSION MAY AFFECT FLAGS
; Uses illegal opcode (DASM 2.20.01 onwards).

            MAC SLEEP            ;usage: SLEEP n (n>1)
.CYCLES     SET {1}

                IF .CYCLES < 2
                    ECHO "MACRO ERROR: 'SLEEP': Duration must be > 1"
                    ERR
                ENDIF

                IF .CYCLES & 1
                    IFNCONST NO_ILLEGAL_OPCODES
                        nop $80
                    ELSE
                        bit $80
                    ENDIF
.CYCLES             SET .CYCLES - 3
                ENDIF
            
                REPEAT .CYCLES / 2
                    nop
                REPEND
            ENDM


;-------------------------------------------------------------------------------
; FRACSLEEP duration
; Based on Thomas Jentzsch's SLEEP macro, but takes cycles*2 to allow for
; 7800 based 0.5 cycle sleep.

            MAC FRACSLEEP            ;usage: FRACSLEEP n (n>1)
.CYCLES     SET {1}

                IF .CYCLES < 4
                    ECHO "MACRO ERROR: 'FRACSLEEP': Duration must be > 4"
                    ERR
                ENDIF
                IF .CYCLES = 5
                    ECHO "MACRO ERROR: 'FRACSLEEP': Duration = 5 is impossible"
                    ERR
                ENDIF

                IF .CYCLES & 1
                    IFNCONST NO_ILLEGAL_OPCODES
                        nop $0 ; TIA access is 3.5 cycles
                    ELSE
                        bit $0 ; TIA access is 3.5 cycles
                    ENDIF
.CYCLES             SET .CYCLES - 7
                ENDIF
 
                IF .CYCLES & 2
                    IFNCONST NO_ILLEGAL_OPCODES
                        nop $80
                    ELSE
                        bit $80
                    ENDIF
.CYCLES             SET .CYCLES - 6
                ENDIF
            
                REPEAT .CYCLES / 4
                    nop
                REPEND
            ENDM


;-------------------------------------------------------
; SET_POINTER
; Original author: Manuel Rotschkar
;
; Sets a 2 byte RAM pointer to an absolute address.
;
; Usage: SET_POINTER pointer, address
; Example: SET_POINTER SpritePTR, SpriteData
;
; Note: Alters the accumulator, NZ flags
; IN 1: 2 byte RAM location reserved for pointer
; IN 2: absolute address

            MAC SET_POINTER
.POINTER    SET {1}
.ADDRESS    SET {2}

                LDA #<.ADDRESS  ; Get Lowbyte of Address
                STA .POINTER    ; Store in pointer
                LDA #>.ADDRESS  ; Get Hibyte of Address
                STA .POINTER+1  ; Store in pointer+1

            ENDM

; EOF
 ; Provided under the CC0 license. See the included LICENSE.txt for details.

; 7800MACRO.H

;-------------------------------------------------------
; BOXCOLLISIONCHECK
; author: Mike Saarna
;
; A general bounding box collision check. compares 2 rectangles of differing size
; and shape for overlap. Carry is set for collision detected, clear for none.
; 
; Usage: BOXCOLLISIONCHECK x1var,y1var,w1var,h1var,x2var,y2var,w2var,h2var
;

 MAC BOXCOLLISIONCHECK
.boxx1    SET {1}
.boxy1    SET {2}
.boxw1    SET {3}
.boxh1    SET {4}
.boxx2    SET {5}
.boxy2    SET {6}
.boxw2    SET {7}
.boxh2    SET {8}

.DoXCollisionCheck
     lda .boxx1 ;3
     cmp .boxx2 ;2
     bcs .X1isbiggerthanX2 ;2/3
.X2isbiggerthanX1
     adc #.boxw1 ;2
     cmp .boxx2 ;3
     bcs .DoYCollisionCheck ;3/2
     bcc .noboxcollision ;3
.X1isbiggerthanX2
     clc ;2
     sbc #.boxw2 ;2
     cmp .boxx2 ;3
     bcs .noboxcollision ;3/2
.DoYCollisionCheck
     lda .boxy1 ;3
     cmp .boxy2 ;3
     bcs .Y1isbiggerthanY2 ;3/2
.Y2isbiggerthanY1
     adc #.boxh1 ;2
     cmp .boxy2 ;3
     jmp .checkdone ;6 
.Y1isbiggerthanY2
     clc ;2
     sbc #.boxh2 ;2
     cmp .boxy2 ;3
     bcs .noboxcollision ;3/2
.boxcollision
     sec ;2
     .byte $24 ; hardcoded "BIT [clc opcode]", used to skip over the following clc
.noboxcollision
     clc ;2
.checkdone

 ENDM

; QBOXCOLLISIONCHECK
; author: unknown
;
; A general bounding box collision check. compares 2 rectangles of differing size
; and shape for overlap. Carry is CLEAR for collision detected, SET for none.
; 
; Usage: QBOXCOLLISIONCHECK x1var,y1var,w1var,h1var,x2var,y2var,w2var,h2var
;
 MAC QBOXCOLLISIONCHECK
.boxx1    SET {1}
.boxy1    SET {2}
.boxw1    SET {3}
.boxh1    SET {4}
.boxx2    SET {5}
.boxy2    SET {6}
.boxw2    SET {7}
.boxh2    SET {8}

	lda .boxx2
	clc
	adc #.boxw2
	sbc .boxx1
	cmp #.boxw1+.boxw2-1
	bcs .qboxcollisiondone
	;if we're here, carry is clear
 	lda .boxy2
	adc #.boxh2
	sbc .boxy1
	cmp #.boxh1+.boxh2-1
.qboxcollisiondone
	rol ; temp for testing - invert carry...
	eor #1
	ror
 ENDM


 MAC MEDIAN3

	; A median filter (for smoothing paddle jitter)
	;   this macro takes the current paddle value, compares it to historic
	;   values, and replaces the current paddle value with the median.
	; 
	; called as:  MEDIAN3 STORAGE CURRENT
	;    where STORAGE points to 3 consecutive bytes of memory. The first 2
	;        must be dedicated to this MEDIAN filter. The last 1 is a temp.
	;    where CURRENT is memory holding the new value you wish to compare to
	;        the previous values, and update with the median value.
	;
	; returns: CURRENT (modified to contain median value)
	;
	; author: Mike Saarna (aka RevEng)

.MedianBytes    SET {1}
.NewValue       SET {2}

	lda #0
	ldy .NewValue
	sty .MedianBytes+2 ; put the new value in the most "recent" slot

	; build an index from relative size comparisons between our 3 values.
	cpy .MedianBytes
	rol
	cpy .MedianBytes+1
	rol
	ldy .MedianBytes
	cpy .MedianBytes+1
	rol
	tay

	ldx MedianOrderLUT,y ; convert the size-comparison index to an index to the median value
	lda .MedianBytes,x
	sta .NewValue ; we replace the new value memory with the median value

	; then shift values from "newer" bytes to "older" bytes, leaving the 
	; newest byte (.MedianBytes+2) empty for next time.
	lda .MedianBytes+1 
	sta .MedianBytes
	lda .MedianBytes+2
	sta .MedianBytes+1
 ifnconst MedianOrderLUT
	jmp MedianOrderLUTend
MedianOrderLUT ; converts our "comparison index" to an index to the median value
	.byte 0 ; 0  B2 < B0 < B1
	.byte 1 ; 1  B2 < B1 < B0
	.byte 2 ; 2   impossible 
	.byte 2 ; 3  B1 < B2 < B0
	.byte 2 ; 4  B0 < B2 < B1
	.byte 2 ; 5   impossible 
	.byte 1 ; 6  B0 < B1 < B2
	.byte 0 ; 7  B1 < B0 < B2
MedianOrderLUTend
 endif
   ENDM

 MAC PLOTSPRITE

	; A macro version of the plotsprite command. 
	; This trades off rom space for speed.
	; It also doesn't check if the visible screen is displayed or not.
	; It has no training wheels. It is all rusty sharp edges.

.GFXLabel   SET {1} ; constant
.Palette    SET {2} ; constant/variable MACARG2CONST
.SpriteX    SET {3} ; constant/variable MACARG3CONST
.SpriteY    SET {4} ; constant/variable MACARG4CONST
.ByteOffset SET {5} ; constant/variable MACARG5CONST

 if MACARG4CONST = 0
	lda .SpriteY
 else
	lda #.SpriteY
 endif

        lsr
        lsr
        asr #%11111110 ; ensure carry is clear
   if WZONEHEIGHT = 16
        asr #%11111110 ; ensure carry is clear
   endif
 
	tax

        cpx #WZONECOUNT
	bcs .PLOTSPRITEnext
	; carry is clear
	
        ifconst VSCROLL
		ldy Xx3,x
		lda DLLMEM+11,y
        else  ; !VSCROLL
		lda DLPOINTL,x ; Get pointer to DL that this sprite starts in
        endif
	ifconst DOUBLEBUFFER
		adc doublebufferdloffset
	endif ; DOUBLEBUFFER
	sta dlpnt
	ifconst VSCROLL
		lda DLLMEM+10,y
	else  ; !VSCROLL
		lda DLPOINTH,x
	endif ; !VSCROLL
	ifconst DOUBLEBUFFER
		adc #0
	endif ; DOUBLEBUFFER
	sta dlpnt+1
	
 	ldy dlend,x ; find the next new object position in this zone

 ifconst .ByteOffset

 if MACARG5CONST = 1
	lda #.ByteOffset 
 else
	lda .ByteOffset 
 endif
	ifconst DOUBLEBUFFER
 	if {1}_width = 1
        	clc
 	endif
 	endif
 if {1}_width = 2
        asl
 endif
 if {1}_width = 3
        asl
 if MACARG5CONST = 1
	adc #.ByteOffset 
 else
	adc .ByteOffset 
 endif
 endif
 if {1}_width = 4
        asl
        asl
 endif
 if {1}_width = 5
        asl
        asl
 if MACARG5CONST = 1
	adc #.ByteOffset 
 else
	adc .ByteOffset 
 endif
 endif
 if {1}_width = 6
        asl
 if MACARG5CONST = 1
	adc #.ByteOffset 
 else
	adc .ByteOffset 
 endif
        asl
 endif
 if {1}_width = 7
        asl
 if MACARG5CONST = 1
	adc #.ByteOffset 
 else
	adc .ByteOffset 
 endif
        asl
 if MACARG5CONST = 1
	adc #.ByteOffset 
 else
	adc .ByteOffset 
 endif
 endif
 if {1}_width = 8
        asl
        asl
        asl
 endif
 if {1}_width = 9
        asl
        asl
        asl
 if MACARG5CONST = 1
	adc #.ByteOffset 
 else
	adc .ByteOffset 
 endif
 endif
 if {1}_width = 10
        asl
        asl
 if MACARG5CONST = 1
	adc #.ByteOffset 
 else
	adc .ByteOffset 
 endif
        asl
 endif
 if {1}_width = 11
        asl
        asl
 if MACARG5CONST = 1
	adc #.ByteOffset 
 else
	adc .ByteOffset 
 endif
        asl
 if MACARG5CONST = 1
	adc #.ByteOffset 
 else
	adc .ByteOffset 
 endif
 endif
 if {1}_width = 12
        asl
 if MACARG5CONST = 1
	adc #.ByteOffset 
 else
	adc .ByteOffset 
 endif
        asl
        asl
 endif
 if {1}_width = 13
        asl
 if MACARG5CONST = 1
	adc #.ByteOffset 
 else
	adc .ByteOffset 
 endif
        asl
        asl
 if MACARG5CONST = 1
	adc #.ByteOffset 
 else
	adc .ByteOffset 
 endif
 
 endif
 if {1}_width = 14
        asl
 if MACARG5CONST = 1
	adc #.ByteOffset 
 else
	adc .ByteOffset 
 endif
        asl
 if MACARG5CONST = 1
	adc #.ByteOffset 
 else
	adc .ByteOffset 
 endif
        asl
 endif
 if {1}_width = 15
        asl
 if MACARG5CONST = 1
	adc #.ByteOffset 
 else
	adc .ByteOffset 
 endif
        asl
 if MACARG5CONST = 1
	adc #.ByteOffset 
 else
	adc .ByteOffset 
 endif
        asl
 if MACARG5CONST = 1
	adc #.ByteOffset 
 else
	adc .ByteOffset 
 endif
 endif
 if {1}_width = 16
        asl
        asl
        asl
        asl
 endif
	adc #<.GFXLabel ; carry is clear via previous asl or asr
 else
	lda #<.GFXLabel ; carry is clear via previous asl or asr
 endif ; .ByteOffset
        sta (dlpnt),y ; #1 - low byte object address

	iny

	lda #({1}_mode | %01000000)
	sta (dlpnt),y ; #2 - graphics mode , indirect

	iny

 if MACARG4CONST = 0
	lda .SpriteY
 else
	lda #.SpriteY
 endif
	and #(WZONEHEIGHT - 1)
	cmp #1 ; clear carry if our sprite is just in this zone
	ora #>.GFXLabel
	sta (dlpnt),y ; #3 - hi byte object address

	iny

 if MACARG2CONST = 1
	lda #({1}_width_twoscompliment | (.Palette * 32))
 else
	lda #({1}_width_twoscompliment)
	ora .Palette
 endif
	sta (dlpnt),y ; #4 - palette|width

	iny

 if MACARG3CONST = 1
	lda #.SpriteX
 else
	lda .SpriteX
 endif
	sta (dlpnt),y ; #5 - x object position

        iny
        sty dlend,x

    ifconst ALWAYSTERMINATE
         iny
         lda #0
         sta (dlpnt),y
     endif

	bcc .PLOTSPRITEend

.PLOTSPRITEnext
	inx ; next zone

        cpx #WZONECOUNT
	bcs .PLOTSPRITEend 
	; carry is clear

	ifconst VSCROLL
		ldy Xx3,x
		lda DLLMEM+11,y
	else  ; !VSCROLL
		lda DLPOINTL,x ;Get pointer to DL that this sprite starts in
	endif ; !VSCROLL
	ifconst DOUBLEBUFFER
		adc doublebufferdloffset
	endif ; DOUBLEBUFFER
	sta dlpnt
	ifconst VSCROLL
		lda DLLMEM+10,y
	else  ; !VSCROLL
		lda DLPOINTH,x
	endif ; !VSCROLL
	ifconst DOUBLEBUFFER
		adc #0
	endif ; DOUBLEBUFFER
	sta dlpnt+1
	
 	ldy dlend,x ; find the next new object position in this zone

 ifconst .ByteOffset

 if MACARG5CONST = 1
	lda #.ByteOffset 
 else
	lda .ByteOffset 
 endif
 if {1}_width = 1
        clc
 endif
 if {1}_width = 2
        asl ; carry clear
 endif
 if {1}_width = 3
        asl ; carry clear
 if MACARG5CONST = 1
	adc #.ByteOffset 
 else
	adc .ByteOffset 
 endif
 endif
 if {1}_width = 4
        asl ; carry clear
        asl
 endif
 if {1}_width = 5
        asl ; carry clear
        asl
 if MACARG5CONST = 1
	adc #.ByteOffset 
 else
	adc .ByteOffset 
 endif
 endif
 if {1}_width = 6
        asl ; carry clear
 if MACARG5CONST = 1
	adc #.ByteOffset 
 else
	adc .ByteOffset 
 endif
        asl
 endif
 if {1}_width = 7
        asl ; carry clear
 if MACARG5CONST = 1
	adc #.ByteOffset 
 else
	adc .ByteOffset 
 endif
        asl
 endif
 if {1}_width = 8
        asl ; carry clear
        asl
        asl
 endif
 if {1}_width = 9
        asl ; carry clear
        asl
        asl
 if MACARG5CONST = 1
	adc #.ByteOffset 
 else
	adc .ByteOffset 
 endif
 endif
 if {1}_width = 10
        asl ; carry clear
        asl
 if MACARG5CONST = 1
	adc #.ByteOffset 
 else
	adc .ByteOffset 
 endif
        asl
 endif
 if {1}_width = 11
        asl ; carry clear
        asl
 if MACARG5CONST = 1
	adc #.ByteOffset 
 else
	adc .ByteOffset 
 endif
        asl
 if MACARG5CONST = 1
	adc #.ByteOffset 
 else
	adc .ByteOffset 
 endif
 endif
 if {1}_width = 12
        asl ; carry clear
 if MACARG5CONST = 1
	adc #.ByteOffset 
 else
	adc .ByteOffset 
 endif
        asl
        asl
 endif
 if {1}_width = 13
        asl ; carry clear
 if MACARG5CONST = 1
	adc #.ByteOffset 
 else
	adc .ByteOffset 
 endif
        asl
        asl
 if MACARG5CONST = 1
	adc #.ByteOffset 
 else
	adc .ByteOffset 
 endif
 endif
 if {1}_width = 14
        asl ; carry clear
 if MACARG5CONST = 1
	adc #.ByteOffset 
 else
	adc .ByteOffset 
 endif
        asl
 if MACARG5CONST = 1
	adc #.ByteOffset 
 else
	adc .ByteOffset 
 endif
        asl
 endif
 if {1}_width = 15
        asl
 if MACARG5CONST = 1
	adc #.ByteOffset 
 else
	adc .ByteOffset 
 endif
        asl
 if MACARG5CONST = 1
	adc #.ByteOffset 
 else
	adc .ByteOffset 
 endif
        asl
 if MACARG5CONST = 1
	adc #.ByteOffset 
 else
	adc .ByteOffset 
 endif
 endif
 if {1}_width = 16
        asl
        asl
        asl
        asl
 endif
	adc #<.GFXLabel
 else
	lda #<.GFXLabel
 endif ; .ByteOffset

        sta (dlpnt),y ; #1 - low byte object address

	iny

	lda #({1}_mode | %01000000)
	sta (dlpnt),y ; #2 - graphics mode , indirect

	iny

 if MACARG4CONST = 0
	lda .SpriteY
 else
	lda #.SpriteY
 endif
	and #(WZONEHEIGHT - 1)
	ora #>(.GFXLabel - (WZONEHEIGHT * 256)) ; start in the dma hole
	sta (dlpnt),y ; #3 - hi byte object address

	iny

 if MACARG2CONST = 1
	lda #({1}_width_twoscompliment | (.Palette * 32))
 else
	lda #({1}_width_twoscompliment)
	ora .Palette
 endif
	sta (dlpnt),y ; #4 - palette|width

	iny

 if MACARG3CONST = 1
	lda #.SpriteX
 else
	lda .SpriteX
 endif
	sta (dlpnt),y ; #5 - x object position

	iny
	sty dlend,x

    ifconst ALWAYSTERMINATE
         iny
         lda #0
         sta (dlpnt),y
     endif

.PLOTSPRITEend
 ENDM

 MAC PLOTSPRITE4

	; A macro version of plotsprite. (with 4 byte objects)
	; This trades off rom space for speed.
	; It also doesn't check if the visible screen is displayed or not.
	; It has no training wheels. It is all rusty sharp edges.

.GFXLabel   SET {1}
.Palette    SET {2} ; constant
.SpriteX    SET {3} ; variable
.SpriteY    SET {4} ; variable
.ByteOffset SET {5} ; variable 

 if MACARG4CONST = 0
	lda .SpriteY
 else
	lda #.SpriteY
 endif
        lsr
        lsr
        asr #%11111110 ; ensure carry is clear
   if WZONEHEIGHT = 16
        asr #%11111110 ; ensure carry is clear
   endif
 
	tax

        cpx #WZONECOUNT
	bcs .PLOTSPRITEnext
	; carry is clear
	ifconst VSCROLL
		ldy Xx3,x
		lda DLLMEM+11,y
	else  ; !VSCROLL
		lda DLPOINTL,x ;Get pointer to DL that this sprite starts in
	endif ; !VSCROLL
	ifconst DOUBLEBUFFER
		adc doublebufferdloffset
	endif ; DOUBLEBUFFER
	sta dlpnt
	ifconst VSCROLL
		lda DLLMEM+10,y
	else  ; !VSCROLL
		lda DLPOINTH,x
	endif ; !VSCROLL
	ifconst DOUBLEBUFFER
		adc #0
	endif ; DOUBLEBUFFER
	sta dlpnt+1
	
 	ldy dlend,x ; find the next new object position in this zone

 ifconst .ByteOffset

 if MACARG5CONST = 1
	lda #.ByteOffset 
 else
	lda .ByteOffset 
 endif
	ifconst DOUBLEBUFFER
 	if {1}_width = 1
       		clc
 	endif
 	endif
 if {1}_width = 2
        asl
 endif
 if {1}_width = 3
        asl
 if MACARG5CONST = 1
	adc #.ByteOffset 
 else
	adc .ByteOffset 
 endif
 endif
 if {1}_width = 4
        asl
        asl
 endif
 if {1}_width = 5
        asl
        asl
 if MACARG5CONST = 1
	adc #.ByteOffset 
 else
	adc .ByteOffset 
 endif
 endif
 if {1}_width = 6
        asl
 if MACARG5CONST = 1
	adc #.ByteOffset 
 else
	adc .ByteOffset 
 endif

        asl
 endif
 if {1}_width = 7
        asl
 if MACARG5CONST = 1
	adc #.ByteOffset 
 else
	adc .ByteOffset 
 endif
        asl
 if MACARG5CONST = 1
	adc #.ByteOffset 
 else
	adc .ByteOffset 
 endif
 endif
 if {1}_width = 8
        asl
        asl
        asl
 endif
 if {1}_width = 9
        asl
        asl
        asl
 if MACARG5CONST = 1
	adc #.ByteOffset 
 else
	adc .ByteOffset 
 endif
 endif
 if {1}_width = 10
        asl
        asl
 if MACARG5CONST = 1
	adc #.ByteOffset 
 else
	adc .ByteOffset 
 endif
        asl
 endif
 if {1}_width = 11
        asl
        asl
 if MACARG5CONST = 1
	adc #.ByteOffset 
 else
	adc .ByteOffset 
 endif
        asl
 if MACARG5CONST = 1
	adc #.ByteOffset 
 else
	adc .ByteOffset 
 endif
 endif
 if {1}_width = 12
        asl
 if MACARG5CONST = 1
	adc #.ByteOffset 
 else
	adc .ByteOffset 
 endif
        asl
        asl
 endif
 if {1}_width = 13
        asl
 if MACARG5CONST = 1
	adc #.ByteOffset 
 else
	adc .ByteOffset 
 endif
        asl
        asl
 if MACARG5CONST = 1
	adc #.ByteOffset 
 else
	adc .ByteOffset 
 endif
 endif
 if {1}_width = 14
        asl
 if MACARG5CONST = 1
	adc #.ByteOffset 
 else
	adc .ByteOffset 
 endif
        asl
 if MACARG5CONST = 1
	adc #.ByteOffset 
 else
	adc .ByteOffset 
 endif
        asl
 endif
 if {1}_width = 15
        asl
 if MACARG5CONST = 1
	adc #.ByteOffset 
 else
	adc .ByteOffset 
 endif
        asl
 if MACARG5CONST = 1
	adc #.ByteOffset 
 else
	adc .ByteOffset 
 endif
        asl
 if MACARG5CONST = 1
	adc #.ByteOffset 
 else
	adc .ByteOffset 
 endif
 endif
 if {1}_width = 16
        asl
        asl
        asl
        asl
 endif
	adc #<.GFXLabel ; carry is clear via previous asl or asr
 else
	lda #<.GFXLabel ; carry is clear via previous asl or asr
 endif ; .ByteOffset
        sta (dlpnt),y ; #1 - low byte object address

	iny

 if MACARG2CONST = 1
	lda #({1}_width_twoscompliment | (.Palette * 32))
 else
	lda #({1}_width_twoscompliment)
	ora .Palette
 endif
	sta (dlpnt),y ; #2 - palette|width

	iny
 if MACARG4CONST = 0
	lda .SpriteY
 else
	lda #.SpriteY
 endif
	and #(WZONEHEIGHT - 1)
	cmp #1 ; clear carry if our sprite is just in this zone
	ora #>.GFXLabel
	sta (dlpnt),y ; #3 - hi byte object address

	iny
 if MACARG3CONST = 1
	lda #.SpriteX
 else
	lda .SpriteX
 endif
	sta (dlpnt),y ; #4 - x object position

        iny
        sty dlend,x

    ifconst ALWAYSTERMINATE
         iny
         lda #0
         sta (dlpnt),y
     endif

	bcc .PLOTSPRITEend

.PLOTSPRITEnext
	inx ; next zone

        cpx #WZONECOUNT
	bcs .PLOTSPRITEend 
	; carry is clear
	ifconst VSCROLL
		ldy Xx3,x
		lda DLLMEM+11,y
	else  ; !VSCROLL
		lda DLPOINTL,x ;Get pointer to DL that this sprite starts in
	endif ; !VSCROLL
	ifconst DOUBLEBUFFER
		adc doublebufferdloffset
	endif ; DOUBLEBUFFER
	sta dlpnt
	ifconst VSCROLL
		lda DLLMEM+10,y
	else  ; !VSCROLL
		lda DLPOINTH,x
	endif ; !VSCROLL
	ifconst DOUBLEBUFFER
		adc #0
	endif ; DOUBLEBUFFER
	sta dlpnt+1
	
 	ldy dlend,x ; find the next new object position in this zone

 ifconst .ByteOffset

 if MACARG5CONST = 1
	lda #.ByteOffset 
 else
	lda .ByteOffset 
 endif
 if {1}_width = 1
        clc
 endif
 if {1}_width = 2
        asl ; carry clear
 endif
 if {1}_width = 3
        asl ; carry clear
 if MACARG5CONST = 1
	adc #.ByteOffset 
 else
	adc .ByteOffset 
 endif
 endif
 if {1}_width = 4
        asl ; carry clear
        asl
 endif
 if {1}_width = 5
        asl ; carry clear
        asl
 if MACARG5CONST = 1
	adc #.ByteOffset 
 else
	adc .ByteOffset 
 endif
 endif
 if {1}_width = 6
        asl ; carry clear
 if MACARG5CONST = 1
	adc #.ByteOffset 
 else
	adc .ByteOffset 
 endif
 
        asl
 endif
 if {1}_width = 7
        asl ; carry clear
 if MACARG5CONST = 1
	adc #.ByteOffset 
 else
	adc .ByteOffset 
 endif
        asl
 endif
 if {1}_width = 8
        asl ; carry clear
        asl
        asl
 endif
 if {1}_width = 9
        asl ; carry clear
        asl
        asl
 if MACARG5CONST = 1
	adc #.ByteOffset 
 else
	adc .ByteOffset 
 endif
 endif
 if {1}_width = 10
        asl ; carry clear
        asl
 if MACARG5CONST = 1
	adc #.ByteOffset 
 else
	adc .ByteOffset 
 endif
        asl
 endif
 if {1}_width = 11
        asl ; carry clear
        asl
 if MACARG5CONST = 1
	adc #.ByteOffset 
 else
	adc .ByteOffset 
 endif
        asl
 if MACARG5CONST = 1
	adc #.ByteOffset 
 else
	adc .ByteOffset 
 endif
 endif
 if {1}_width = 12
        asl ; carry clear
 if MACARG5CONST = 1
	adc #.ByteOffset 
 else
	adc .ByteOffset 
 endif
        asl
        asl
 endif
 if {1}_width = 13
        asl ; carry clear
 if MACARG5CONST = 1
	adc #.ByteOffset 
 else
	adc .ByteOffset 
 endif
        asl
        asl
 if MACARG5CONST = 1
	adc #.ByteOffset 
 else
	adc .ByteOffset 
 endif
 endif
 if {1}_width = 14
        asl ; carry clear
 if MACARG5CONST = 1
	adc #.ByteOffset 
 else
	adc .ByteOffset 
 endif
        asl
 if MACARG5CONST = 1
	adc #.ByteOffset 
 else
	adc .ByteOffset 
 endif
        asl
 endif
 if {1}_width = 15
        asl
 if MACARG5CONST = 1
	adc #.ByteOffset 
 else
	adc .ByteOffset 
 endif
        asl
 if MACARG5CONST = 1
	adc #.ByteOffset 
 else
	adc .ByteOffset 
 endif
        asl
 if MACARG5CONST = 1
	adc #.ByteOffset 
 else
	adc .ByteOffset 
 endif
 endif
 if {1}_width = 16
        asl
        asl
        asl
        asl
 endif
	adc #<.GFXLabel
 else
	lda #<.GFXLabel
 endif ; .ByteOffset
        sta (dlpnt),y ; #1 - low byte object address

	iny
 if MACARG2CONST = 1
	lda #({1}_width_twoscompliment | (.Palette * 32))
 else
	lda #({1}_width_twoscompliment)
	ora .Palette
 endif

	sta (dlpnt),y ; #2 - palette|width

	iny
 if MACARG4CONST = 0
	lda .SpriteY
 else
	lda #.SpriteY
 endif
	and #(WZONEHEIGHT - 1)
	ora #>(.GFXLabel - (WZONEHEIGHT * 256)) ; start in the dma hole
	sta (dlpnt),y ; #3 - hi byte object address

	iny
 if MACARG3CONST = 1
	lda #.SpriteX
 else
	lda .SpriteX
 endif
	sta (dlpnt),y ; #4 - x object position

	iny
	sty dlend,x

    ifconst ALWAYSTERMINATE
         iny
         lda #0
         sta (dlpnt),y
     endif

.PLOTSPRITEend
 ENDM

 MAC SCROLLSETUP

        ; If vertical scrolling is enabled...
        ;   * Fills the DLs with hidden masking sprites.
	; Adds blank sprites to the DLs to fill the screen.
	; If horizontal scrolling is enabled...
	;   * Adds another blank DL off-screen

	; {1} - constant - the first dl of the scrolling area
	; {2} - symbol   - blank tile label

	; *** clear the saved dl ending for scrolling zones...
	ldx #{1}
	lda #0
.scrollcleardls	
	sta dlend,x
	inx
	cpx #WZONECOUNT
	bne .scrollcleardls

 ifconst VSCROLL
	; *** adjust the ending for our mask dl to allow for mask objects...
	dex
	lda #(maskscrollspriteend-maskscrollsprite)
	sta dlend,x

	; *** Add 4x dma masking objects to last zone...
	ldx #(maskscrollspriteend-maskscrollsprite-1)
.scrollpopulateloop1
	lda maskscrollsprite,x
	sta LASTZONEADDRESS+0,x
	ifconst DOUBLEBUFFER
		sta LASTZONEADDRESS+0+DOUBLEBUFFEROFFSET,x
	endif ; DOUBLEBUFFER
	dex
	bpl .scrollpopulateloop1
	inx ; x=0
	stx finescrolly
 endif ; VSCROLL

	; *** Add blank sprite-tile objects to the scrolling zones...
PLOTSP4 = 1 ; ensure we use 4 byte sprites

	; convert byte width of the sprit to coordinate width...
 if {2}_mode = 0  ; ### 160A, 320A, 320D
.scrollXWIDTH SET ({2}_width * 4) ; 4x 160-mode pixels per byte
 else             ; ### 160B, 320B, 320C
.scrollXWIDTH SET ({2}_width * 2) ; 2x 160-mode pixels per byte
 endif

        ; figure out how many sprites we need to fill a screen width...
.scrollSPRITECOuNT SET ((160+.scrollXWIDTH-1)/.scrollXWIDTH)
 ifconst HSCROLL
.scrollSPRITECOuNT SET (.scrollSPRITECOuNT+1) 
 endif ; HSCROLL

	; setup plotsprite4 parameters...
	lda #<{2}
	sta temp1
	lda #>{2}
	sta temp2
	lda #{2}_width_twoscompliment
	sta temp3 ; width

	lda #{1}
	asl
	asl
	asl
 if WZONEHEIGHT
	asl
 endif
	sta temp5 ; Y
.scrollpopulateloop2
	lda #0
	sta temp4 ; X
.scrollpopulateloop3
	jsr skipplotsprite4wait
	lda temp4 ; X
	clc
	adc #.scrollXWIDTH
	sta temp4 ; X
	cmp #(.scrollSPRITECOuNT*.scrollXWIDTH)
	bne .scrollpopulateloop3
	lda temp5 ; Y
	clc
	adc #WZONEHEIGHT
	sta temp5 ; Y
	cmp #((WZONECOUNT*WZONEHEIGHT)+WZONEHEIGHT)
	bne .scrollpopulateloop2
 ENDM ; SCROLLSETUP

 MAC SIZEOF

	; echoes the size difference between the current address and the
	; a label that was passed as an argument. This is a quick way to
	; determine the size of a structure.

.NAME SETSTR {1}
        echo " The Size of",.NAME,"is:",[* - {1}]d,[* - {2}]d,"bytes."
  ENDM

;
; speakjet.inc
;
;
; AtariVox Speech Synth Driver
;
; By Alex Herbert, 2004
;




; Constants


SERIAL_OUTMASK  equ     $01
SERIAL_RDYMASK  equ     $02



; Macros

        mac     SPKOUT

        ; check buffer-full status
        lda     SWCHA
        and     #SERIAL_RDYMASK
        beq     .speech_done

        ; get next speech byte
        ldy     #$00
        lda     (speech_addr),y

        ; invert data and check for end of string
        eor     #$ff
 ;sta BACKGRND ; debug - uncomment to flash the background color with vox data
        beq     .speech_done
        sta     {1}

        ; increment speech pointer
        inc     speech_addr
        bne     .incaddr_skip
        inc     speech_addr+1
.incaddr_skip

        ; output byte as serial data

        sec     ; start bit
.byteout_loop
        ; put carry flag into bit 0 of SWACNT, preserving other bits
        lda     SWACNT          ; 4
        and     #$fe            ; 2 6
        adc     #$00            ; 2 8
        sta     SWACNT          ; 4 12

        ; 10 bits sent? (1 start bit, 8 data bits, 1 stop bit)
        cpy     #$09            ; 2 14
        beq     .speech_done    ; 2 16
        iny                     ; 2 18

	; the 7800 is 1.5x faster than the 2600. Waste more cycles here
	; to match the original baud rate...
        ;ldx     #$07 ; 2600
        ldx     #$0D

.delay_loop
        dex			; 
        bne     .delay_loop     ; 36 54

        ; shift next data bit into carry
        lsr     {1}             ; 5 59

        ; and loop (branch always taken)
        bpl     .byteout_loop   ; 3 62 cycles for loop

.speech_done

        endm


        mac     SPEAK

        lda     #<{1}
        sta     speech_addr
        lda     #>{1}
        sta     speech_addr+1

        endm



     ; Provided under the CC0 license. See the included LICENSE.txt for details.

     processor 6502

     include "7800basic.h"
     include "7800_extravars.h"

     ; BEADHEADER... disabled for now
     ; A BEAD header gets automatically incorportated into the ROM header. 
     ; For more BEAD executable info, check out the spec...
     ; http://7800.8bitdev.org/index.php/The_Atari_7800_BEAD_Execuable_Specification

GAMEDESCRIPTIONSET     = 1
GAMEDESCRIPTION     = "Test Name"


BDHSC     = %01000000
BDYM     = %00100000
BDPOKEY     = %00010000
BDROF     = %00001000
BD16K     = %00000000
BD32K     = %00000001
BD48K     = %00000010
BD1800     = %00000101
BD4000     = %00000110

     ifconst BEADHEADER
BEADHARDWARE         SET 0
         ifconst ROM16K 
BEADHARDWARE             SET (BEADHARDWARE|BD16K)
         endif
         ifconst ROM32K 
BEADHARDWARE             SET (BEADHARDWARE|BD32K)
         endif
         ifconst ROM48K 
BEADHARDWARE             SET (BEADHARDWARE|BD48K)
         endif
         ifconst pokeysupport
BEADHARDWARE             SET (BEADHARDWARE|BDPOKEY)
         endif
         ifconst HSSUPPORT
BEADHARDWARE             SET (BEADHARDWARE|BDHSC)
         endif
     endif

     ;start address of cart...

BANK_WAS_SET SET 0

     ifconst ROM8K
         ORG $E000,0
BANK_WAS_SET SET 1
     endif ; ROM8K

     ifconst ROM16K
         ORG $C000,0
BANK_WAS_SET SET 1
         ifconst BEADHEADER
             .byte $BE,$AD,BEADHARDWARE
             ifconst GAMEDESCRIPTION
                 CLC
                 BCC _SKIPDESCRIPTION
                 .byte GAMEDESCRIPTION,0
_SKIPDESCRIPTION
             endif ; GAMEDESCRIPTION
             jmp ($FFFC)
         endif ; BEADHEADER
     endif ; ROM16K

     ifconst ROM32K
         ORG $8000,0
BANK_WAS_SET SET 1
         ifconst BEADHEADER
             .byte $BE,$AD,BEADHARDWARE
             ifconst GAMEDESCRIPTION
                 CLC
                 BCC _SKIPDESCRIPTION
                 .byte GAMEDESCRIPTION,0
_SKIPDESCRIPTION
             endif ; GAMEDESCRIPTION
             jmp ($FFFC)
         endif ; BEADHEADER
     endif ; ROM32K

     ifconst ROM48K
         ORG $4000,0
BANK_WAS_SET SET 1
         ifconst BEADHEADER
             .byte $BE,$AD,BEADHARDWARE
             ifconst GAMEDESCRIPTIONSET
                 CLC
                 BCC _SKIPDESCRIPTION
                 .byte GAMEDESCRIPTION,0
_SKIPDESCRIPTION
             endif ; GAMEDESCRIPTIONSET
             jmp ($FFFC)
         endif ; BEADHEADER
     endif ; ROM48K

     ifconst ROM52K
BANK_WAS_SET SET 1
         ORG $3000,0
     endif ; ROM52K

     ifconst bankswitchmode
         ifconst ROMAT4K
BANK_WAS_SET SET 1
             ORG $4000,0
             RORG $4000
         else ; ROMAT4K
BANK_WAS_SET SET 1
             ORG $8000,0
             RORG $8000
         endif
     endif

     if BANK_WAS_SET = 0
         ORG $8000,0 ; default is 32K
     endif

START_OF_ROM SET .
SPACEOVERFLOW SET 0
 ifnconst SPACEOVERFLOWPASS
SPACEOVERFLOWPASS SET 0
 endif SPACEOVERFLOWPASS
game
.
 ;;line 1;; 

.
 ;;line 2;; 

.
 ;;line 3;; 

.
 ;;line 4;; 

.
 ;;line 5;; 

.L00 ;;line 6;;  incgraphic gfx/sprites/font_spr.png 160A 3 0 1

.L01 ;;line 7;;  incgraphic gfx/sprites/bar0.png 160A 0 1

.L02 ;;line 8;;  incgraphic gfx/sprites/bar1.png 160A 0 2 1

.L03 ;;line 9;;  incgraphic gfx/sprites/bar2.png 160A 0 2 1

.L04 ;;line 10;;  incgraphic gfx/sprites/bar3.png 160A 0 2 1

.L05 ;;line 11;;  incgraphic gfx/sprites/bar4.png 160A 0 2 1

.L06 ;;line 12;;  incgraphic gfx/sprites/bar5.png 160A 0 2 1

.L07 ;;line 13;;  incgraphic gfx/sprites/bar6.png 160A 0 2 1

.L08 ;;line 14;;  incgraphic gfx/sprites/bar7.png 160A 0 2 1

.L09 ;;line 15;;  incgraphic gfx/sprites/bar8.png 160A 0 2 1

.L010 ;;line 16;;  incgraphic gfx/sprites/hp.png 160B 0 3 7 8 1 2

.L011 ;;line 17;;  incgraphic gfx/sprites/mp.png 160A 0 3 1

.L012 ;;line 18;;  incgraphic gfx/sprites/coin.png 160A 0 3 2

.L013 ;;line 19;;  incgraphic gfx/sprites/prism.png 160A 0 3 2 1

.L014 ;;line 20;;  incgraphic gfx/sprites/chimkin.png 160A 0 3 1 2

.L015 ;;line 21;;  incgraphic gfx/sprites/torch0.png 160B 0 1 2 3 6 5 4

.L016 ;;line 22;;  incgraphic gfx/sprites/torch1.png 160B 0 1 2 3 5 6 4

.L017 ;;line 23;;  incgraphic gfx/sprites/torch2.png 160B 0 1 2 3 6 4 5

.L018 ;;line 24;;  incgraphic gfx/sprites/torch3.png 160B 0 1 2 6 4 5

.L019 ;;line 25;;  incgraphic gfx/sprites/potion0.png 160A 0 2 3 1

.L020 ;;line 26;;  incgraphic gfx/sprites/armor0.png 160A 0 2 1

.L021 ;;line 27;;  incgraphic gfx/sprites/rosary.png 160A 0 3 2 1

.L022 ;;line 28;;  incgraphic gfx/sprites/relic0.png 160A 0 2 3 1

.L023 ;;line 29;;  incgraphic gfx/sprites/relic1.png 160A 0 1 2 3

.L024 ;;line 30;;  incgraphic gfx/sprites/relic2.png 160A 0 3 2 1

.L025 ;;line 31;;  incgraphic gfx/sprites/map0.png 160A 0 3 2 1

.L026 ;;line 32;;  incgraphic gfx/sprites/door0.png 160A 0 1 2 3

.L027 ;;line 33;;  incgraphic gfx/sprites/door1.png 160A 0 1 2 3

.L028 ;;line 34;;  incgraphic gfx/sprites/key0.png 160A 0 3 1

.
 ;;line 35;; 

.init
 ;;line 36;; init

.L029 ;;line 37;;  goto title_init bank10

  sta temp9
  lda #>(.title_init-1)
  pha
  lda #<(.title_init-1)
  pha
  lda temp9
  pha
  txa
  pha
 ifconst BANKRAM
  lda #9
  sta currentbank
  ora currentrambank
 else
  lda #9
 endif
  jmp BS_jsr
.
 ;;line 38;; 

.tileset_transition
 ;;line 39;; tileset_transition

.L030 ;;line 40;;  tileset = exit_tileset

  lda exit_tileset
  sta tileset
.L031 ;;line 41;;  first_load = 1

  lda #1
  sta first_load
.clear_menu_map
 ;;line 42;; clear_menu_map

.L032 ;;line 43;;  asm

          ldx #100       ; menu_map_room array + 2 to clear menu_map_revealed and exit_tileset as well

          lda #0

          sta entered_save_room

          sta old_tileset

tranloop  sta exit_tileset,x

          dex

          bpl tranloop

          rts

.
 ;;line 53;; 

.load_map
 ;;line 54;; load_map

.L033 ;;line 55;;  clearscreen

 jsr clearscreen
.L034 ;;line 56;;  gosub clear_palettes

  jsr .clear_palettes
.L035 ;;line 57;;  displaymode 160A

    lda #%01000000 ;Enable DMA, mode=160x2/160x4
    sta CTRL

    sta sCTRL

.L036 ;;line 58;;  adjustvisible 0 14

  lda #(0*3)
  sta temp1
  lda #(14*3)
  sta temp2
  jsr adjustvisible
USED_ADJUSTVISIBLE = 1
.L037 ;;line 59;;  if exit_tileset then gosub tileset_transition

  lda exit_tileset
  beq .skipL037
.condpart0
  jsr .tileset_transition
.skipL037
.L038 ;;line 60;;  asm

          lda #0

          sta in_save_room

.L039 ;;line 64;;  if tileset = T_FOREST then gosub load_forest bank2

  lda tileset
  cmp T_FOREST
  bne .skipL039
.condpart1
  sta temp9
  lda #>(ret_point1-1)
  pha
  lda #<(ret_point1-1)
  pha
  lda #0
  pha
  lda #0
  pha
  lda #>(.load_forest-1)
  pha
  lda #<(.load_forest-1)
  pha
  lda temp9
  pha
  txa
  pha
 ifconst BANKRAM
  lda #1
  sta currentbank
  ora currentrambank
 else
  lda #1
 endif
  jmp BS_jsr
ret_point1
.skipL039
.L040 ;;line 65;;  if tileset = T_CASTLE then gosub load_castle bank4

  lda tileset
  cmp T_CASTLE
  bne .skipL040
.condpart2
  sta temp9
  lda #>(ret_point2-1)
  pha
  lda #<(ret_point2-1)
  pha
  lda #0
  pha
  lda #0
  pha
  lda #>(.load_castle-1)
  pha
  lda #<(.load_castle-1)
  pha
  lda temp9
  pha
  txa
  pha
 ifconst BANKRAM
  lda #3
  sta currentbank
  ora currentrambank
 else
  lda #3
 endif
  jmp BS_jsr
ret_point2
.skipL040
.L041 ;;line 66;;  if tileset = T_RIVER then gosub load_river bank6

  lda tileset
  cmp T_RIVER
  bne .skipL041
.condpart3
  sta temp9
  lda #>(ret_point3-1)
  pha
  lda #<(ret_point3-1)
  pha
  lda #0
  pha
  lda #0
  pha
  lda #>(.load_river-1)
  pha
  lda #<(.load_river-1)
  pha
  lda temp9
  pha
  txa
  pha
 ifconst BANKRAM
  lda #5
  sta currentbank
  ora currentrambank
 else
  lda #5
 endif
  jmp BS_jsr
ret_point3
.skipL041
.L042 ;;line 67;;  if tileset = T_HILLS then gosub load_hills bank8

  lda tileset
  cmp T_HILLS
  bne .skipL042
.condpart4
  sta temp9
  lda #>(ret_point4-1)
  pha
  lda #<(ret_point4-1)
  pha
  lda #0
  pha
  lda #0
  pha
  lda #>(.load_hills-1)
  pha
  lda #<(.load_hills-1)
  pha
  lda temp9
  pha
  txa
  pha
 ifconst BANKRAM
  lda #7
  sta currentbank
  ora currentrambank
 else
  lda #7
 endif
  jmp BS_jsr
ret_point4
.skipL042
.L043 ;;line 68;;  gosub draw_hud

  jsr .draw_hud
.draw_pickup_hud
 ;;line 69;; draw_pickup_hud

.L044 ;;line 70;;  Xposition = 36

  lda #36
  sta Xposition
.L045 ;;line 71;;  for index = 0 to 13

  lda #0
  sta index
.L045forindex
.L046 ;;line 72;;  Xposition = Xposition  +  6

  lda Xposition
	CLC
	ADC #6
  sta Xposition
.L047 ;;line 73;;  PLOTSPRITE font_spr 0 Xposition 208 0

 if font_spr_width > 16
 echo "*** ERROR: PLOTSPRITE encountered sprite wider than 16 bytes. (font_spr)"
 echo "*** PLOTSPRITE/PLOTSPRITE4 is limited to sprites 16 bytes wide or less."
 ERR
 endif
MACARG2CONST SET 1
MACARG3CONST SET 0
MACARG4CONST SET 1
MACARG5CONST SET 1
 PLOTSPRITE font_spr,0,Xposition,208,0
.L048 ;;line 74;;  next

  lda index
  cmp #13
  inc index
 if ((* - .L045forindex) < 127) && ((* - .L045forindex) > -128)
  bcc .L045forindex
 else
  bcs .0skipL045forindex
  jmp .L045forindex
.0skipL045forindex
 endif
.map_loaded
 ;;line 75;; map_loaded

.L049 ;;line 76;;  savescreen

 jsr savescreen
.L050 ;;line 77;;  gosub update_hp_bar

  jsr .update_hp_bar
.L051 ;;line 78;;  gosub update_mp_bar

  jsr .update_mp_bar
.L052 ;;line 79;;  if !save_objects then gosub load_objects

  lda save_objects
  bne .skipL052
.condpart5
  jsr .load_objects
.skipL052
.L053 ;;line 80;;  asm

        lda #0

        sta reload_map

        sta save_objects

        sta player_obscure

        sta pw_success

.
 ;;line 87;; 

.L054 ;;line 88;;  if exit_dir = EXIT_SOUTH then player_Ypos = 15  :  player_Ydir = DIR_DOWN  :  object_Ypos[13] = 8

  lda exit_dir
  cmp EXIT_SOUTH
  bne .skipL054
.condpart6
  lda #15
  sta player_Ypos
  lda DIR_DOWN
  sta player_Ydir
  lda #8
	LDX #13
  sta object_Ypos,x
.skipL054
.L055 ;;line 89;;  if exit_dir = EXIT_NORTH then player_Ypos = 175  :  player_Ydir = DIR_UP  :  object_Ypos[13] = 183

  lda exit_dir
  cmp EXIT_NORTH
  bne .skipL055
.condpart7
  lda #175
  sta player_Ypos
  lda DIR_UP
  sta player_Ydir
  lda #183
	LDX #13
  sta object_Ypos,x
.skipL055
.L056 ;;line 90;;  if exit_dir = EXIT_EAST then player_Xpos = 147  :  player_Xdir = DIR_LEFT  :  object_Xpos[13] = 153

  lda exit_dir
  cmp EXIT_EAST
  bne .skipL056
.condpart8
  lda #147
  sta player_Xpos
  lda DIR_LEFT
  sta player_Xdir
  lda #153
	LDX #13
  sta object_Xpos,x
.skipL056
.L057 ;;line 91;;  if exit_dir = EXIT_WEST then player_Xpos = 7  :  player_Xdir = DIR_RIGHT  :  object_Xpos[13] = 0

  lda exit_dir
  cmp EXIT_WEST
  bne .skipL057
.condpart9
  lda #7
  sta player_Xpos
  lda DIR_RIGHT
  sta player_Xdir
  lda #0
	LDX #13
  sta object_Xpos,x
.skipL057
.
 ;;line 92;; 

.main
 ;;line 93;; main

.L058 ;;line 94;;  drawscreen

 jsr drawscreen
.L059 ;;line 95;;  if switchreset then reboot

 jsr checkresetswitch
  bne .skipL059
.condpart10
	JMP START
.skipL059
.L060 ;;line 96;;  if !switchselect then select_debounce = 0

 jsr checkselectswitch
  beq .skipL060
.condpart11
  lda #0
  sta select_debounce
.skipL060
.L061 ;;line 97;;  if !switchpause then pause_debounce = 0

 lda #8
 bit SWCHB
  beq .skipL061
.condpart12
  lda #0
  sta pause_debounce
.skipL061
.L062 ;;line 98;;  if !joy0fire0  &&  action_button = 0 then action_debounce = 0

 bit sINPT1
  bvs .skipL062
.condpart13
  lda action_button
  cmp #0
  bne .skip13then
.condpart14
  lda #0
  sta action_debounce
.skip13then
.skipL062
.L063 ;;line 99;;  if !joy0fire0  &&  action_button = 1 then menu_debounce = 0

 bit sINPT1
  bvs .skipL063
.condpart15
  lda action_button
  cmp #1
  bne .skip15then
.condpart16
  lda #0
  sta menu_debounce
.skip15then
.skipL063
.L064 ;;line 100;;  if !joy0fire1  &&  action_button = 1 then action_debounce = 0

 bit sINPT1
  bmi .skipL064
.condpart17
  lda action_button
  cmp #1
  bne .skip17then
.condpart18
  lda #0
  sta action_debounce
.skip17then
.skipL064
.L065 ;;line 101;;  if !joy0fire1  &&  action_button = 0 then menu_debounce = 0

 bit sINPT1
  bmi .skipL065
.condpart19
  lda action_button
  cmp #0
  bne .skip19then
.condpart20
  lda #0
  sta menu_debounce
.skip19then
.skipL065
.L066 ;;line 102;;  if !joy0left then left_debounce = 0

 bit sSWCHA
  bvc .skipL066
.condpart21
  lda #0
  sta left_debounce
.skipL066
.L067 ;;line 103;;  if !joy0right then right_debounce = 0

 bit sSWCHA
  bpl .skipL067
.condpart22
  lda #0
  sta right_debounce
.skipL067
.L068 ;;line 104;;  if !joy0up then up_debounce = 0

 lda #$10
 bit sSWCHA
  beq .skipL068
.condpart23
  lda #0
  sta up_debounce
.skipL068
.L069 ;;line 105;;  if !joy0down then down_debounce = 0

 lda #$20
 bit sSWCHA
  beq .skipL069
.condpart24
  lda #0
  sta down_debounce
.skipL069
.
 ;;line 106;; 

.L070 ;;line 107;;  if joy0left then left_debounce = left_debounce  +  1

 bit sSWCHA
  bvs .skipL070
.condpart25
  lda left_debounce
	CLC
	ADC #1
  sta left_debounce
.skipL070
.L071 ;;line 108;;  if joy0right then right_debounce = right_debounce  +  1

 bit sSWCHA
  bmi .skipL071
.condpart26
  lda right_debounce
	CLC
	ADC #1
  sta right_debounce
.skipL071
.L072 ;;line 109;;  if joy0up then up_debounce = up_debounce  +  1

 lda #$10
 bit sSWCHA
  bne .skipL072
.condpart27
  lda up_debounce
	CLC
	ADC #1
  sta up_debounce
.skipL072
.L073 ;;line 110;;  if joy0down then down_debounce = down_debounce  +  1

 lda #$20
 bit sSWCHA
  bne .skipL073
.condpart28
  lda down_debounce
	CLC
	ADC #1
  sta down_debounce
.skipL073
.L074 ;;line 111;;  if switchselect  &&  !select_debounce then gosub swap_action_button

 jsr checkselectswitch
  bne .skipL074
.condpart29
  lda select_debounce
  bne .skip29then
.condpart30
  jsr .swap_action_button
.skip29then
.skipL074
.main_animation_timer
 ;;line 112;; main_animation_timer

.L075 ;;line 113;;  animation_timer = animation_timer  +  1

  lda animation_timer
	CLC
	ADC #1
  sta animation_timer
.L076 ;;line 114;;  if animation_timer  >  ANIM_TIME then animation_timer = 0

  lda ANIM_TIME
  cmp animation_timer
  bcs .skipL076
.condpart31
  lda #0
  sta animation_timer
.skipL076
.
 ;;line 115;; 

.L077 ;;line 116;;  animation_frame = 0

  lda #0
  sta animation_frame
.L078 ;;line 117;;  if animation_timer  >   ( ANIM_TIME  /  2 )  then animation_frame = 1

; complex condition detected
; complex statement detected
  lda ANIM_TIME
  lsr
  cmp animation_timer
  bcs .skipL078
.condpart32
  lda #1
  sta animation_frame
.skipL078
.
 ;;line 118;; 

.main_lightning
 ;;line 119;; main_lightning

.L079 ;;line 120;;  asm

        lda lightning_timer

        beq lightning_flash

        dec lightning_timer

        bne lightning_done

lightning_flash

        dec lightning_delay

        lda lightning_delay

        bne lightning_done

        jsr randomize

        bmi lightning_done

        lda #LIGHTNING_FRAMES

        sta lightning_timer

lightning_done

.
 ;;line 135;; 

.main_pal_animation
 ;;line 136;; main_pal_animation

.L080 ;;line 137;;  if !pal_animation then main_pal_done

  lda pal_animation
 if ((* - .main_pal_done) < 127) && ((* - .main_pal_done) > -128)
  beq .main_pal_done
 else
  bne .1skipmain_pal_done
  jmp .main_pal_done
.1skipmain_pal_done
 endif
.L081 ;;line 138;;  if !animation_frame then main_pal_done

  lda animation_frame
 if ((* - .main_pal_done) < 127) && ((* - .main_pal_done) > -128)
  beq .main_pal_done
 else
  bne .2skipmain_pal_done
  jmp .main_pal_done
.2skipmain_pal_done
 endif
.
 ;;line 139;; 

.L082 ;;line 140;;  temp7 = sP7C3

  lda sP7C3
  sta temp7
.L083 ;;line 141;;  sP7C3 = sP7C2

  lda sP7C2
  sta sP7C3
.L084 ;;line 142;;  sP7C2 = sP7C1

  lda sP7C1
  sta sP7C2
.L085 ;;line 143;;  sP7C1 = temp7

  lda temp7
  sta sP7C1
.L086 ;;line 144;;  P7C1 = sP7C1

  lda sP7C1
  sta P7C1
.L087 ;;line 145;;  P7C2 = sP7C2

  lda sP7C2
  sta P7C2
.L088 ;;line 146;;  P7C3 = sP7C3

  lda sP7C3
  sta P7C3
.main_pal_done
 ;;line 147;; main_pal_done

.
 ;;line 148;; 

.main_state_handler
 ;;line 149;; main_state_handler

.L089 ;;line 150;;  if game_state = STATE_TITLE then goto title_handler

  lda game_state
  cmp STATE_TITLE
  bne .skipL089
.condpart33
  jmp .title_handler
.skipL089
.L090 ;;line 151;;  if game_state = STATE_FIELD then goto field_handler

  lda game_state
  cmp STATE_FIELD
  bne .skipL090
.condpart34
  jmp .field_handler
.skipL090
.L091 ;;line 152;;  if game_state = STATE_MENU then goto menu_handler bank3

  lda game_state
  cmp STATE_MENU
  bne .skipL091
.condpart35
  sta temp9
  lda #>(.menu_handler-1)
  pha
  lda #<(.menu_handler-1)
  pha
  lda temp9
  pha
  txa
  pha
 ifconst BANKRAM
  lda #2
  sta currentbank
  ora currentrambank
 else
  lda #2
 endif
  jmp BS_jsr
.skipL091
.L092 ;;line 153;;  goto main

  jmp .main
.
 ;;line 154;; 

.plot_player
 ;;line 155;; plot_player

.
 ;;line 156;; 

.
 ;;line 157;; 

.L093 ;;line 158;;  asm

          ldx locked_zone

          bmi .plot_player_no_lock      ; default locked_zone is $FF so that zone 0 can be locked

          ; don't lock zone if elevation > 0

          lda object_elevation

          bne .plot_player_no_lock

          jsr lockzonex

.plot_player_no_lock
 ;;line 166;; plot_player_no_lock

.L094 ;;line 167;;  if obscure_player  &&  player_obscure then plot_player_unlock

  lda obscure_player
  beq .skipL094
.condpart36
  lda player_obscure
 if ((* - .plot_player_unlock) < 127) && ((* - .plot_player_unlock) > -128)
  bne .plot_player_unlock
 else
  beq .3skipplot_player_unlock
  jmp .plot_player_unlock
.3skipplot_player_unlock
 endif
.skipL094
.L095 ;;line 168;;  if player_state = STATE_ATTACK  &&  player_sword then plotsprite sword0 sword_pal sword_Xpos sword_Ypos sword_frame

  lda player_state
  cmp STATE_ATTACK
  bne .skipL095
.condpart37
  lda player_sword
  beq .skip37then
.condpart38
    lda #<sword0
    ldy #sword0_width
    beq plotspritewidthskip0
plotspritewidthloop0
      clc
      adc sword_frame
      dey
      bne plotspritewidthloop0
plotspritewidthskip0
    sta temp1

    lda #>sword0
    sta temp2

    lda sword_pal
    asl
    asl
    asl
    asl
    asl
    ora #sword0_width_twoscompliment
    sta temp3

    lda sword_Xpos
    sta temp4

    lda sword_Ypos
    sta temp5

    lda #(sword0_mode|%01000000)
    sta temp6

 jsr plotsprite
.skip37then
.skipL095
.
 ;;line 169;; 

.
 ;;line 170;; 

.L096 ;;line 171;;  if player_facing = DIR_UP  ||  player_facing = DIR_RIGHT then gosub plot_player_shield

  lda player_facing
  cmp DIR_UP
  bne .skipL096
.condpart39
  jmp .condpart40
.skipL096
  lda player_facing
  cmp DIR_RIGHT
  bne .skip7OR
.condpart40
  jsr .plot_player_shield
.skip7OR
.plot_johanna
 ;;line 172;; plot_johanna

.L097 ;;line 173;;  if player_flags{1} then plotsprite joswim0 0 player_Xpos player_Ypos player_frame else plotsprite heroine0 0 player_Xpos player_Ypos player_frame

  lda player_flags
  and #2
  beq .skipL097
.condpart41
    lda #<joswim0
    ldy #joswim0_width
    beq plotspritewidthskip1
plotspritewidthloop1
      clc
      adc player_frame
      dey
      bne plotspritewidthloop1
plotspritewidthskip1
    sta temp1

    lda #>joswim0
    sta temp2

    lda #(0|joswim0_width_twoscompliment)
    sta temp3

    lda player_Xpos
    sta temp4

    lda player_Ypos
    sta temp5

    lda #(joswim0_mode|%01000000)
    sta temp6

 jsr plotsprite
  jmp .skipelse0
.skipL097
    lda #<heroine0
    ldy #heroine0_width
    beq plotspritewidthskip2
plotspritewidthloop2
      clc
      adc player_frame
      dey
      bne plotspritewidthloop2
plotspritewidthskip2
    sta temp1

    lda #>heroine0
    sta temp2

    lda #(0|heroine0_width_twoscompliment)
    sta temp3

    lda player_Xpos
    sta temp4

    lda player_Ypos
    sta temp5

    lda #(heroine0_mode|%01000000)
    sta temp6

 jsr plotsprite
.skipelse0
.L098 ;;line 174;;  if player_facing = DIR_DOWN  ||  player_facing = DIR_LEFT then gosub plot_player_shield

  lda player_facing
  cmp DIR_DOWN
  bne .skipL098
.condpart42
  jmp .condpart43
.skipL098
  lda player_facing
  cmp DIR_LEFT
  bne .skip8OR
.condpart43
  jsr .plot_player_shield
.skip8OR
.plot_player_unlock
 ;;line 175;; plot_player_unlock

.L099 ;;line 176;;  asm

          ldx locked_zone

          bmi .plot_hud

          jsr unlockzonex

.
 ;;line 181;; 

.plot_hud
 ;;line 182;; plot_hud

.L0100 ;;line 183;;  asm

          ldx zone1_objects

          ldy #0

          SetObjectImageLo hp_bar_1

          inx

          ldy #0

          SetObjectImageLo hp_bar_2

          inx

          ldy #0

          SetObjectImageLo hp_bar_3

          inx

          ldy #0

          SetObjectImageLo hp_bar_4

          inx

          ldy #0

          SetObjectImageLo hp_bar_5

          inx

          ldy #0

          SetObjectImageLo hp_bar_6

          ldx zone2_objects

          ldy #1

          SetObjectImageLo mp_bar_1

          inx

          ldy #1

          SetObjectImageLo mp_bar_2

          inx

          ldy #1

          SetObjectImageLo mp_bar_3

          inx

          ldy #1

          SetObjectImageLo mp_bar_4

          inx

          ldy #1

          SetObjectImageLo mp_bar_5

          inx

          ldy #1

          SetObjectImageLo mp_bar_6

.L0101 ;;line 221;;  if debug_hud then gosub plot_debug_hud

  lda debug_hud
  beq .skipL0101
.condpart44
  jsr .plot_debug_hud
.skipL0101
.
 ;;line 222;; 

.
 ;;line 223;; 

.
 ;;line 224;; 

.
 ;;line 225;; 

.float_sprites
 ;;line 226;; float_sprites

.L0102 ;;line 227;;  goto main

  jmp .main
.
 ;;line 228;; 

.
 ;;line 229;; 

.
 ;;line 230;; 

.
 ;;line 231;; 

.
 ;;line 232;; 

.
 ;;line 233;; 

.
 ;;line 234;; 

.
 ;;line 235;; 

.
 ;;line 236;; 

.
 ;;line 237;; 

.
 ;;line 238;; 

.
 ;;line 239;; 

.
 ;;line 240;; 

.
 ;;line 241;; 

.
 ;;line 242;; 

.
 ;;line 243;; 

.
 ;;line 244;; 

.
 ;;line 245;; 

.plot_player_shield
 ;;line 246;; plot_player_shield

.L0103 ;;line 247;;  if player_state = STATE_DEATH  ||  player_state = STATE_CHANT then return thisbank

  lda player_state
  cmp STATE_DEATH
  bne .skipL0103
.condpart45
  jmp .condpart46
.skipL0103
  lda player_state
  cmp STATE_CHANT
  bne .skip9OR
.condpart46
  rts
.skip9OR
.
 ;;line 248;; 

.L0104 ;;line 249;;  if player_shield then plotsprite shield0 shield_pal shield_Xpos shield_Ypos shield_frame

  lda player_shield
  beq .skipL0104
.condpart47
    lda #<shield0
    ldy #shield0_width
    beq plotspritewidthskip3
plotspritewidthloop3
      clc
      adc shield_frame
      dey
      bne plotspritewidthloop3
plotspritewidthskip3
    sta temp1

    lda #>shield0
    sta temp2

    lda shield_pal
    asl
    asl
    asl
    asl
    asl
    ora #shield0_width_twoscompliment
    sta temp3

    lda shield_Xpos
    sta temp4

    lda shield_Ypos
    sta temp5

    lda #(shield0_mode|%01000000)
    sta temp6

 jsr plotsprite
.skipL0104
.L0105 ;;line 250;;  return thisbank

  rts
.
 ;;line 251;; 

.kill_player
 ;;line 252;; kill_player

.L0106 ;;line 253;;  player_frame = FRAME_DEATH

  lda FRAME_DEATH
  sta player_frame
.L0107 ;;line 254;;  if !fade_count then fade_count = 60

  lda fade_count
  bne .skipL0107
.condpart48
  lda #60
  sta fade_count
.skipL0107
.L0108 ;;line 255;;  goto cc_next

  jmp .cc_next
.
 ;;line 256;; 

.
 ;;line 257;; 

.death_fade
 ;;line 258;; death_fade

.L0109 ;;line 259;;  if !fade then fade_done

  lda fade
 if ((* - .fade_done) < 127) && ((* - .fade_done) > -128)
  beq .fade_done
 else
  bne .4skipfade_done
  jmp .fade_done
.4skipfade_done
 endif
.L0110 ;;line 260;;  fade = fade  -  1

  lda fade
	SEC
	SBC #1
  sta fade
.fade_done
 ;;line 261;; fade_done

.L0111 ;;line 262;;  fade_count = fade_count  -  1

  lda fade_count
	SEC
	SBC #1
  sta fade_count
.L0112 ;;line 263;;  if !fade_count then password_display

  lda fade_count
 if ((* - .password_display) < 127) && ((* - .password_display) > -128)
  beq .password_display
 else
  bne .5skippassword_display
  jmp .password_display
.5skippassword_display
 endif
.L0113 ;;line 264;;  goto player_handler_done

  jmp .player_handler_done
.
 ;;line 265;; 

.save_room_handler
 ;;line 266;; save_room_handler

.L0114 ;;line 267;;  drawwait

 jsr drawwait
.L0115 ;;line 268;;  gosub encode_password bank11

  sta temp9
  lda #>(ret_point5-1)
  pha
  lda #<(ret_point5-1)
  pha
  lda #0
  pha
  lda #0
  pha
  lda #>(.encode_password-1)
  pha
  lda #<(.encode_password-1)
  pha
  lda temp9
  pha
  txa
  pha
 ifconst BANKRAM
  lda #10
  sta currentbank
  ora currentrambank
 else
  lda #10
 endif
  jmp BS_jsr
ret_point5
.password_display
 ;;line 269;; password_display

.L0116 ;;line 270;;  menu = MENU_GAMEOVER

  lda MENU_GAMEOVER
  sta menu
.L0117 ;;line 271;;  menu_debounce = 1

  lda #1
  sta menu_debounce
.L0118 ;;line 272;;  pause_debounce = 1

  lda #1
  sta pause_debounce
.L0119 ;;line 273;;  action_debounce = 1

  lda #1
  sta action_debounce
.L0120 ;;line 274;;  goto menu_init bank3

  sta temp9
  lda #>(.menu_init-1)
  pha
  lda #<(.menu_init-1)
  pha
  lda temp9
  pha
  txa
  pha
 ifconst BANKRAM
  lda #2
  sta currentbank
  ora currentrambank
 else
  lda #2
 endif
  jmp BS_jsr
.
 ;;line 275;; 

.L0121 ;;line 276;;  data inv_bits

  jmp .skipL0121
inv_bits
  .byte         %00000001, %00000010, %00000100, %00001000, %00010000

.skipL0121
inv_bits_length = [. - inv_bits]
inv_bits_lo SET #<inv_bits
inv_bits_hi SET #>inv_bits
.
 ;;line 279;; 

.prep_inv_bits
 ;;line 280;; prep_inv_bits

.L0122 ;;line 281;;  if temp1  >=  5 then prep_bits_2

  lda temp1
  cmp #5
 if ((* - .prep_bits_2) < 127) && ((* - .prep_bits_2) > -128)
  bcs .prep_bits_2
 else
  bcc .6skipprep_bits_2
  jmp .prep_bits_2
.6skipprep_bits_2
 endif
.L0123 ;;line 282;;  temp1 = inv_bits[temp1]

	LDX temp1
  lda inv_bits,x
  sta temp1
.L0124 ;;line 283;;  temp2 = 0

  lda #0
  sta temp2
.L0125 ;;line 284;;  return

  rts
.prep_bits_2
 ;;line 285;; prep_bits_2

.L0126 ;;line 286;;  temp1 = temp1  -  5

  lda temp1
	SEC
	SBC #5
  sta temp1
.L0127 ;;line 287;;  temp2 = inv_bits[temp1]

	LDX temp1
  lda inv_bits,x
  sta temp2
.L0128 ;;line 288;;  temp1 = 0

  lda #0
  sta temp1
.L0129 ;;line 289;;  return

  rts
.
 ;;line 290;; 

.pickup_map
 ;;line 291;; pickup_map

.L0130 ;;line 292;;  temp1 = temp1  -  TYPE_MAP1

  lda temp1
	SEC
	SBC TYPE_MAP1
  sta temp1
.L0131 ;;line 293;;  pickup_ptr_lo = pickup_maps_lo[temp1]

	LDX temp1
  lda pickup_maps_lo,x
  sta pickup_ptr_lo
.L0132 ;;line 294;;  pickup_ptr_hi = pickup_maps_hi[temp1]

	LDX temp1
  lda pickup_maps_hi,x
  sta pickup_ptr_hi
.L0133 ;;line 295;;  gosub prep_inv_bits

  jsr .prep_inv_bits
.add_map
 ;;line 296;; add_map

.L0134 ;;line 297;;  m_map_bits = m_map_bits  |  temp1

  lda m_map_bits
	ORA temp1
  sta m_map_bits
.L0135 ;;line 298;;  goto kill_object

  jmp .kill_object
.
 ;;line 299;; 

.pickup_key
 ;;line 300;; pickup_key

.L0136 ;;line 301;;  temp1 = temp1  -  TYPE_KEY1

  lda temp1
	SEC
	SBC TYPE_KEY1
  sta temp1
.L0137 ;;line 302;;  pickup_ptr_lo = pickup_keys_lo[temp1]

	LDX temp1
  lda pickup_keys_lo,x
  sta pickup_ptr_lo
.L0138 ;;line 303;;  pickup_ptr_hi = pickup_keys_hi[temp1]

	LDX temp1
  lda pickup_keys_hi,x
  sta pickup_ptr_hi
.L0139 ;;line 304;;  gosub prep_inv_bits

  jsr .prep_inv_bits
.add_key
 ;;line 305;; add_key

.L0140 ;;line 306;;  m_key_bits = m_key_bits  |  temp1

  lda m_key_bits
	ORA temp1
  sta m_key_bits
.L0141 ;;line 307;;  goto kill_object

  jmp .kill_object
.
 ;;line 308;; 

.pickup_spell
 ;;line 309;; pickup_spell

.L0142 ;;line 310;;  temp1 = temp1  -  TYPE_SPELL1

  lda temp1
	SEC
	SBC TYPE_SPELL1
  sta temp1
.L0143 ;;line 311;;  pickup_ptr_lo = pickup_spells_lo[temp1]

	LDX temp1
  lda pickup_spells_lo,x
  sta pickup_ptr_lo
.L0144 ;;line 312;;  pickup_ptr_hi = pickup_spells_hi[temp1]

	LDX temp1
  lda pickup_spells_hi,x
  sta pickup_ptr_hi
.L0145 ;;line 313;;  gosub prep_inv_bits

  jsr .prep_inv_bits
.add_spell
 ;;line 314;; add_spell

.L0146 ;;line 315;;  m_spell_bits_1 = m_spell_bits_1  |  temp1

  lda m_spell_bits_1
	ORA temp1
  sta m_spell_bits_1
.L0147 ;;line 316;;  m_spell_bits_2 = m_spell_bits_2  |  temp2

  lda m_spell_bits_2
	ORA temp2
  sta m_spell_bits_2
.L0148 ;;line 317;;  goto kill_object

  jmp .kill_object
.
 ;;line 318;; 

.pickup_item
 ;;line 319;; pickup_item

.L0149 ;;line 320;;  temp1 = temp1  -  TYPE_ITEM1

  lda temp1
	SEC
	SBC TYPE_ITEM1
  sta temp1
.
 ;;line 321;; 

.L0150 ;;line 322;;  temp2 = temp1  +  1

  lda temp1
	CLC
	ADC #1
  sta temp2
.L0151 ;;line 323;;  m_item_qty[temp2] = m_item_qty[temp2]  +  1

	LDX temp2
  lda m_item_qty,x
	CLC
	ADC #1
	LDX temp2
  sta m_item_qty,x
.
 ;;line 324;; 

.L0152 ;;line 325;;  if m_item_qty[temp2]  >=  9 then m_item_qty[temp2] = 9

	LDX temp2
  lda m_item_qty,x
  cmp #9
  bcc .skipL0152
.condpart49
  lda #9
	LDX temp2
  sta m_item_qty,x
.skipL0152
.L0153 ;;line 326;;  pickup_ptr_lo = pickup_items_lo[temp1]

	LDX temp1
  lda pickup_items_lo,x
  sta pickup_ptr_lo
.L0154 ;;line 327;;  pickup_ptr_hi = pickup_items_hi[temp1]

	LDX temp1
  lda pickup_items_hi,x
  sta pickup_ptr_hi
.L0155 ;;line 328;;  gosub prep_inv_bits

  jsr .prep_inv_bits
.add_item
 ;;line 329;; add_item

.L0156 ;;line 330;;  m_item_bits_1 = m_item_bits_1  |  temp1

  lda m_item_bits_1
	ORA temp1
  sta m_item_bits_1
.L0157 ;;line 331;;  m_item_bits_2 = m_item_bits_2  |  temp2

  lda m_item_bits_2
	ORA temp2
  sta m_item_bits_2
.L0158 ;;line 332;;  goto kill_object

  jmp .kill_object
.
 ;;line 333;; 

.pickup_sword
 ;;line 334;; pickup_sword

.L0159 ;;line 335;;  temp1 = temp1  -  TYPE_SWORD1

  lda temp1
	SEC
	SBC TYPE_SWORD1
  sta temp1
.L0160 ;;line 336;;  pickup_ptr_lo = pickup_swords_lo[temp1]

	LDX temp1
  lda pickup_swords_lo,x
  sta pickup_ptr_lo
.L0161 ;;line 337;;  pickup_ptr_hi = pickup_swords_hi[temp1]

	LDX temp1
  lda pickup_swords_hi,x
  sta pickup_ptr_hi
.L0162 ;;line 338;;  gosub prep_inv_bits

  jsr .prep_inv_bits
.add_sword
 ;;line 339;; add_sword

.L0163 ;;line 340;;  m_sword_bits_1 = m_sword_bits_1  |  temp1

  lda m_sword_bits_1
	ORA temp1
  sta m_sword_bits_1
.L0164 ;;line 341;;  m_sword_bits_2 = m_sword_bits_2  |  temp2

  lda m_sword_bits_2
	ORA temp2
  sta m_sword_bits_2
.L0165 ;;line 342;;  goto kill_object

  jmp .kill_object
.
 ;;line 343;; 

.pickup_shield
 ;;line 344;; pickup_shield

.L0166 ;;line 345;;  temp1 = temp1  -  TYPE_SHIELD1

  lda temp1
	SEC
	SBC TYPE_SHIELD1
  sta temp1
.L0167 ;;line 346;;  pickup_ptr_lo = pickup_shields_lo[temp1]

	LDX temp1
  lda pickup_shields_lo,x
  sta pickup_ptr_lo
.L0168 ;;line 347;;  pickup_ptr_hi = pickup_shields_hi[temp1]

	LDX temp1
  lda pickup_shields_hi,x
  sta pickup_ptr_hi
.L0169 ;;line 348;;  gosub prep_inv_bits

  jsr .prep_inv_bits
.add_shield
 ;;line 349;; add_shield

.L0170 ;;line 350;;  m_shield_bits_1 = m_shield_bits_1  |  temp1

  lda m_shield_bits_1
	ORA temp1
  sta m_shield_bits_1
.L0171 ;;line 351;;  m_shield_bits_2 = m_shield_bits_2  |  temp2

  lda m_shield_bits_2
	ORA temp2
  sta m_shield_bits_2
.L0172 ;;line 352;;  goto kill_object

  jmp .kill_object
.
 ;;line 353;; 

.pickup_armor
 ;;line 354;; pickup_armor

.L0173 ;;line 355;;  temp1 = temp1  -  TYPE_ARMOR1

  lda temp1
	SEC
	SBC TYPE_ARMOR1
  sta temp1
.L0174 ;;line 356;;  pickup_ptr_lo = pickup_armor_lo[temp1]

	LDX temp1
  lda pickup_armor_lo,x
  sta pickup_ptr_lo
.L0175 ;;line 357;;  pickup_ptr_hi = pickup_armor_hi[temp1]

	LDX temp1
  lda pickup_armor_hi,x
  sta pickup_ptr_hi
.L0176 ;;line 358;;  gosub prep_inv_bits

  jsr .prep_inv_bits
.add_armor
 ;;line 359;; add_armor

.L0177 ;;line 360;;  m_armor_bits_1 = m_armor_bits_1  |  temp1

  lda m_armor_bits_1
	ORA temp1
  sta m_armor_bits_1
.L0178 ;;line 361;;  m_armor_bits_2 = m_armor_bits_2  |  temp2

  lda m_armor_bits_2
	ORA temp2
  sta m_armor_bits_2
.L0179 ;;line 362;;  goto kill_object

  jmp .kill_object
.
 ;;line 363;; 

.pickup_charm
 ;;line 364;; pickup_charm

.L0180 ;;line 365;;  temp1 = temp1  -  TYPE_CHARM1

  lda temp1
	SEC
	SBC TYPE_CHARM1
  sta temp1
.L0181 ;;line 366;;  pickup_ptr_lo = pickup_charms_lo[temp1]

	LDX temp1
  lda pickup_charms_lo,x
  sta pickup_ptr_lo
.L0182 ;;line 367;;  pickup_ptr_hi = pickup_charms_hi[temp1]

	LDX temp1
  lda pickup_charms_hi,x
  sta pickup_ptr_hi
.L0183 ;;line 368;;  gosub prep_inv_bits

  jsr .prep_inv_bits
.add_charm
 ;;line 369;; add_charm

.L0184 ;;line 370;;  m_charm_bits_1 = m_charm_bits_1  |  temp1

  lda m_charm_bits_1
	ORA temp1
  sta m_charm_bits_1
.L0185 ;;line 371;;  m_charm_bits_2 = m_charm_bits_2  |  temp2

  lda m_charm_bits_2
	ORA temp2
  sta m_charm_bits_2
.L0186 ;;line 372;;  goto kill_object

  jmp .kill_object
.
 ;;line 373;; 

.pickup_relic
 ;;line 374;; pickup_relic

.L0187 ;;line 375;;  temp1 = temp1  -  TYPE_RELIC1

  lda temp1
	SEC
	SBC TYPE_RELIC1
  sta temp1
.L0188 ;;line 376;;  pickup_ptr_lo = pickup_relics_lo[temp1]

	LDX temp1
  lda pickup_relics_lo,x
  sta pickup_ptr_lo
.L0189 ;;line 377;;  pickup_ptr_hi = pickup_relics_hi[temp1]

	LDX temp1
  lda pickup_relics_hi,x
  sta pickup_ptr_hi
.L0190 ;;line 378;;  gosub prep_inv_bits

  jsr .prep_inv_bits
.add_relic
 ;;line 379;; add_relic

.L0191 ;;line 380;;  m_relic_bits_1 = m_relic_bits_1  |  temp1

  lda m_relic_bits_1
	ORA temp1
  sta m_relic_bits_1
.L0192 ;;line 381;;  m_relic_bits_2 = m_relic_bits_2  |  temp2

  lda m_relic_bits_2
	ORA temp2
  sta m_relic_bits_2
.L0193 ;;line 382;;  goto kill_object

  jmp .kill_object
.
 ;;line 383;; 

.set_locked_string
 ;;line 384;; set_locked_string

.L0194 ;;line 385;;  pickup_ptr_lo = door_locked_lo

  lda #door_locked_lo
  sta pickup_ptr_lo
.L0195 ;;line 386;;  pickup_ptr_hi = door_locked_hi

  lda #door_locked_hi
  sta pickup_ptr_hi
.L0196 ;;line 387;;  goto cc_next

  jmp .cc_next
.
 ;;line 388;; 

.
 ;;line 389;; 

.
 ;;line 390;; 

.
 ;;line 391;; 

.
 ;;line 392;; 

.
 ;;line 393;; 

.
 ;;line 394;; 

.
 ;;line 395;; 

.load_objects
 ;;line 396;; load_objects

.L0197 ;;line 397;;  asm

        ldx #MAX_OBJECTS

        lda #0

clear_obj_loop

        sta object_type,x

        sta object_Xpos,x

        sta object_Ypos,x

        sta object_Xvel_lo,x

        sta object_Xvel_hi,x

        sta object_Yvel_lo,x

        sta object_Yvel_hi,x

        sta object_Xdir,x

        sta object_Ydir,x

        sta object_facing,x

        sta object_frame,x

        sta object_state,x

        sta object_speed_lo,x

        sta object_speed_hi,x

        sta object_vel_cap_lo,x

        sta object_vel_cap_hi,x

        sta object_friction_lo,x

        sta object_friction_hi,x

        sta object_hp,x

        sta object_damage,x

        sta object_elem_atk,x

        sta object_def,x

        sta object_mdef,x

        sta object_elem_def,x

        sta object_parent,x

        sta object_flags,x

        sta object_timer,x

        sta object_dest,x

        sta object_elevation,x

        sta object_pal,x

        dex

        bne clear_obj_loop

        sta temp1

        ; failsafe for objects without sprite pointer

        lda #<.plot_next

        sta object_spr_ptr_lo,x

        lda #>.plot_next

        sta object_spr_ptr_hi,x

.L0198 ;;line 440;;  for obj_index = 1 to MAX_MAP_OBJECTS

  lda #1
  sta obj_index
.L0198forobj_index
.L0199 ;;line 441;;  object_type[obj_index] = pointer[[temp1]]

	LDY temp1
  lda (pointer),y
	LDX obj_index
  sta object_type,x
.L0200 ;;line 442;;  if !object_type[obj_index] then object_familiars

	LDX obj_index
  lda object_type,x
 if ((* - .object_familiars) < 127) && ((* - .object_familiars) > -128)
  beq .object_familiars
 else
  bne .7skipobject_familiars
  jmp .object_familiars
.7skipobject_familiars
 endif
.L0201 ;;line 443;;  temp1 = temp1  +  1

  lda temp1
	CLC
	ADC #1
  sta temp1
.L0202 ;;line 444;;  object_Xpos[obj_index] = pointer[[temp1]]

	LDY temp1
  lda (pointer),y
	LDX obj_index
  sta object_Xpos,x
.L0203 ;;line 445;;  temp1 = temp1  +  1

  lda temp1
	CLC
	ADC #1
  sta temp1
.L0204 ;;line 446;;  object_Ypos[obj_index] = pointer[[temp1]]

	LDY temp1
  lda (pointer),y
	LDX obj_index
  sta object_Ypos,x
.L0205 ;;line 447;;  temp1 = temp1  +  1

  lda temp1
	CLC
	ADC #1
  sta temp1
.L0206 ;;line 448;;  next

  lda obj_index
  cmp MAX_MAP_OBJECTS
  inc obj_index
 if ((* - .L0198forobj_index) < 127) && ((* - .L0198forobj_index) > -128)
  bcc .L0198forobj_index
 else
  bcs .8skipL0198forobj_index
  jmp .L0198forobj_index
.8skipL0198forobj_index
 endif
.
 ;;line 449;; 

.object_familiars
 ;;line 450;; object_familiars

.L0207 ;;line 451;;  if m_relic_bits_1{0} then object_type[13] = TYPE_F_FAIRY

  lda m_relic_bits_1
  lsr
  bcc .skipL0207
.condpart50
  lda TYPE_F_FAIRY
	LDX #13
  sta object_type,x
.skipL0207
.
 ;;line 452;; 

.object_stats
 ;;line 453;; object_stats

.L0208 ;;line 454;;  for obj_index = 1 to MAX_OBJECTS

  lda #1
  sta obj_index
.L0208forobj_index
.L0209 ;;line 455;;  index = object_type[obj_index]

	LDX obj_index
  lda object_type,x
  sta index
.L0210 ;;line 456;;  if !index then obj_next_stats

  lda index
 if ((* - .obj_next_stats) < 127) && ((* - .obj_next_stats) > -128)
  beq .obj_next_stats
 else
  bne .9skipobj_next_stats
  jmp .obj_next_stats
.9skipobj_next_stats
 endif
.L0211 ;;line 457;;  if index  >=  128 then obj_next_stats

  lda index
  cmp #128
 if ((* - .obj_next_stats) < 127) && ((* - .obj_next_stats) > -128)
  bcs .obj_next_stats
 else
  bcc .10skipobj_next_stats
  jmp .obj_next_stats
.10skipobj_next_stats
 endif
.L0212 ;;line 458;;  pointer = object_stats_lo[index]

	LDX index
  lda object_stats_lo,x
  sta pointer
.L0213 ;;line 459;;  pointer_hi = object_stats_hi[index]

	LDX index
  lda object_stats_hi,x
  sta pointer_hi
.L0214 ;;line 460;;  asm

          jmp (pointer)

.
 ;;line 463;; 

.obj_next_stats
 ;;line 464;; obj_next_stats

.L0215 ;;line 465;;  next

  lda obj_index
  cmp MAX_OBJECTS
  inc obj_index
 if ((* - .L0208forobj_index) < 127) && ((* - .L0208forobj_index) > -128)
  bcc .L0208forobj_index
 else
  bcs .11skipL0208forobj_index
  jmp .L0208forobj_index
.11skipL0208forobj_index
 endif
.
 ;;line 466;; 

.
 ;;line 467;; 

.
 ;;line 468;; 

.
 ;;line 469;; 

.
 ;;line 470;; 

.
 ;;line 471;; 

.
 ;;line 472;; 

.
 ;;line 473;; 

.
 ;;line 474;; 

.remove_duplicates
 ;;line 475;; remove_duplicates

.L0216 ;;line 476;;  item_spawned = 0

  lda #0
  sta item_spawned
.L0217 ;;line 477;;  for index = 1 to MAX_OBJECTS

  lda #1
  sta index
.L0217forindex
.L0218 ;;line 478;;  if object_type[index]  >=  TYPE_CHARM1 then check_charm_bits

	LDX index
  lda object_type,x
  cmp TYPE_CHARM1
 if ((* - .check_charm_bits) < 127) && ((* - .check_charm_bits) > -128)
  bcs .check_charm_bits
 else
  bcc .12skipcheck_charm_bits
  jmp .check_charm_bits
.12skipcheck_charm_bits
 endif
.L0219 ;;line 479;;  if object_type[index]  >=  TYPE_ARMOR1 then check_armor_bits

	LDX index
  lda object_type,x
  cmp TYPE_ARMOR1
 if ((* - .check_armor_bits) < 127) && ((* - .check_armor_bits) > -128)
  bcs .check_armor_bits
 else
  bcc .13skipcheck_armor_bits
  jmp .check_armor_bits
.13skipcheck_armor_bits
 endif
.L0220 ;;line 480;;  if object_type[index]  >=  TYPE_SHIELD1 then check_shield_bits

	LDX index
  lda object_type,x
  cmp TYPE_SHIELD1
 if ((* - .check_shield_bits) < 127) && ((* - .check_shield_bits) > -128)
  bcs .check_shield_bits
 else
  bcc .14skipcheck_shield_bits
  jmp .check_shield_bits
.14skipcheck_shield_bits
 endif
.L0221 ;;line 481;;  if object_type[index]  >=  TYPE_SWORD1 then check_sword_bits

	LDX index
  lda object_type,x
  cmp TYPE_SWORD1
 if ((* - .check_sword_bits) < 127) && ((* - .check_sword_bits) > -128)
  bcs .check_sword_bits
 else
  bcc .15skipcheck_sword_bits
  jmp .check_sword_bits
.15skipcheck_sword_bits
 endif
.L0222 ;;line 482;;  if object_type[index]  >=  TYPE_RELIC1 then check_relic_bits

	LDX index
  lda object_type,x
  cmp TYPE_RELIC1
 if ((* - .check_relic_bits) < 127) && ((* - .check_relic_bits) > -128)
  bcs .check_relic_bits
 else
  bcc .16skipcheck_relic_bits
  jmp .check_relic_bits
.16skipcheck_relic_bits
 endif
.L0223 ;;line 483;;  if object_type[index]  >=  TYPE_SPELL1 then check_spell_bits

	LDX index
  lda object_type,x
  cmp TYPE_SPELL1
 if ((* - .check_spell_bits) < 127) && ((* - .check_spell_bits) > -128)
  bcs .check_spell_bits
 else
  bcc .17skipcheck_spell_bits
  jmp .check_spell_bits
.17skipcheck_spell_bits
 endif
.L0224 ;;line 484;;  if object_type[index]  >=  TYPE_MAP1 then check_map_bits

	LDX index
  lda object_type,x
  cmp TYPE_MAP1
 if ((* - .check_map_bits) < 127) && ((* - .check_map_bits) > -128)
  bcs .check_map_bits
 else
  bcc .18skipcheck_map_bits
  jmp .check_map_bits
.18skipcheck_map_bits
 endif
.remove_duplicates_next
 ;;line 485;; remove_duplicates_next

.L0225 ;;line 486;;  next

  lda index
  cmp MAX_OBJECTS
  inc index
 if ((* - .L0217forindex) < 127) && ((* - .L0217forindex) > -128)
  bcc .L0217forindex
 else
  bcs .19skipL0217forindex
  jmp .L0217forindex
.19skipL0217forindex
 endif
.L0226 ;;line 487;;  return thisbank

  rts
.
 ;;line 488;; 

.L0227 ;;line 489;;  data object_stats_lo

  jmp .skipL0227
object_stats_lo
  .byte         0, obj_torch_stats_lo, obj_wolf_stats_lo, obj_warg_stats_lo, obj_nix_stats_lo

  .byte         obj_octo_stats_lo, obj_slime_stats_lo, obj_slime_stats_lo, obj_raven_stats_lo

  .byte         obj_priest_stats_lo, obj_skel_stats_lo, obj_redslime_stats_lo, obj_redslime_stats_lo

  .byte         obj_ghost_stats_lo, obj_rghost_stats_lo, obj_harpy_stats_lo, obj_spider_stats_lo, obj_spider_stats_lo

  .byte         obj_hslime_stats_lo, obj_hslime_stats_lo, obj_hghost_stats_lo

.skipL0227
object_stats_lo_length = [. - object_stats_lo]
object_stats_lo_lo SET #<object_stats_lo
object_stats_lo_hi SET #>object_stats_lo
.
 ;;line 496;; 

.L0228 ;;line 497;;  data object_stats_hi

  jmp .skipL0228
object_stats_hi
  .byte         0, obj_torch_stats_hi, obj_wolf_stats_hi, obj_warg_stats_hi, obj_nix_stats_hi

  .byte         obj_octo_stats_hi, obj_slime_stats_hi, obj_slime_stats_hi, obj_raven_stats_hi

  .byte         obj_priest_stats_hi, obj_skel_stats_hi, obj_redslime_stats_hi, obj_redslime_stats_hi

  .byte         obj_ghost_stats_hi, obj_rghost_stats_hi, obj_harpy_stats_hi, obj_spider_stats_hi, obj_spider_stats_hi

  .byte         obj_hslime_stats_hi, obj_hslime_stats_hi, obj_hghost_stats_hi

.skipL0228
object_stats_hi_length = [. - object_stats_hi]
object_stats_hi_lo SET #<object_stats_hi
object_stats_hi_hi SET #>object_stats_hi
.
 ;;line 504;; 

.check_map_bits
 ;;line 505;; check_map_bits

.L0229 ;;line 506;;  if object_type[index] = TYPE_KEY1  &&  m_key_bits{0} then object_type[index] = 0

	LDX index
  lda object_type,x
  cmp TYPE_KEY1
  bne .skipL0229
.condpart51
  lda m_key_bits
  lsr
  bcc .skip51then
.condpart52
  lda #0
	LDX index
  sta object_type,x
.skip51then
.skipL0229
.L0230 ;;line 507;;  if object_type[index] = TYPE_KEY2  &&  m_key_bits{1} then object_type[index] = 0

	LDX index
  lda object_type,x
  cmp TYPE_KEY2
  bne .skipL0230
.condpart53
  lda m_key_bits
  and #2
  beq .skip53then
.condpart54
  lda #0
	LDX index
  sta object_type,x
.skip53then
.skipL0230
.L0231 ;;line 508;;  if object_type[index] = TYPE_KEY3  &&  m_key_bits{2} then object_type[index] = 0

	LDX index
  lda object_type,x
  cmp TYPE_KEY3
  bne .skipL0231
.condpart55
  lda m_key_bits
  and #4
  beq .skip55then
.condpart56
  lda #0
	LDX index
  sta object_type,x
.skip55then
.skipL0231
.L0232 ;;line 509;;  if object_type[index] = TYPE_KEY4  &&  m_key_bits{3} then object_type[index] = 0

	LDX index
  lda object_type,x
  cmp TYPE_KEY4
  bne .skipL0232
.condpart57
  lda m_key_bits
  and #8
  beq .skip57then
.condpart58
  lda #0
	LDX index
  sta object_type,x
.skip57then
.skipL0232
.L0233 ;;line 510;;  if object_type[index] = TYPE_KEY5  &&  m_key_bits{4} then object_type[index] = 0

	LDX index
  lda object_type,x
  cmp TYPE_KEY5
  bne .skipL0233
.condpart59
  lda m_key_bits
  and #16
  beq .skip59then
.condpart60
  lda #0
	LDX index
  sta object_type,x
.skip59then
.skipL0233
.L0234 ;;line 511;;  if object_type[index] = TYPE_MAP1  &&  m_map_bits{0} then object_type[index] = 0

	LDX index
  lda object_type,x
  cmp TYPE_MAP1
  bne .skipL0234
.condpart61
  lda m_map_bits
  lsr
  bcc .skip61then
.condpart62
  lda #0
	LDX index
  sta object_type,x
.skip61then
.skipL0234
.L0235 ;;line 512;;  if object_type[index] = TYPE_MAP2  &&  m_map_bits{1} then object_type[index] = 0

	LDX index
  lda object_type,x
  cmp TYPE_MAP2
  bne .skipL0235
.condpart63
  lda m_map_bits
  and #2
  beq .skip63then
.condpart64
  lda #0
	LDX index
  sta object_type,x
.skip63then
.skipL0235
.L0236 ;;line 513;;  if object_type[index] = TYPE_MAP3  &&  m_map_bits{2} then object_type[index] = 0

	LDX index
  lda object_type,x
  cmp TYPE_MAP3
  bne .skipL0236
.condpart65
  lda m_map_bits
  and #4
  beq .skip65then
.condpart66
  lda #0
	LDX index
  sta object_type,x
.skip65then
.skipL0236
.L0237 ;;line 514;;  if object_type[index] = TYPE_MAP4  &&  m_map_bits{3} then object_type[index] = 0

	LDX index
  lda object_type,x
  cmp TYPE_MAP4
  bne .skipL0237
.condpart67
  lda m_map_bits
  and #8
  beq .skip67then
.condpart68
  lda #0
	LDX index
  sta object_type,x
.skip67then
.skipL0237
.L0238 ;;line 515;;  if object_type[index] = TYPE_MAP5  &&  m_map_bits{4} then object_type[index] = 0

	LDX index
  lda object_type,x
  cmp TYPE_MAP5
  bne .skipL0238
.condpart69
  lda m_map_bits
  and #16
  beq .skip69then
.condpart70
  lda #0
	LDX index
  sta object_type,x
.skip69then
.skipL0238
.L0239 ;;line 516;;  goto remove_duplicates_next

  jmp .remove_duplicates_next
.
 ;;line 517;; 

.check_spell_bits
 ;;line 518;; check_spell_bits

.L0240 ;;line 519;;  if object_type[index] = TYPE_SPELL1  &&  m_spell_bits_1{0} then object_type[index] = 0

	LDX index
  lda object_type,x
  cmp TYPE_SPELL1
  bne .skipL0240
.condpart71
  lda m_spell_bits_1
  lsr
  bcc .skip71then
.condpart72
  lda #0
	LDX index
  sta object_type,x
.skip71then
.skipL0240
.L0241 ;;line 520;;  if object_type[index] = TYPE_SPELL2  &&  m_spell_bits_1{1} then object_type[index] = 0

	LDX index
  lda object_type,x
  cmp TYPE_SPELL2
  bne .skipL0241
.condpart73
  lda m_spell_bits_1
  and #2
  beq .skip73then
.condpart74
  lda #0
	LDX index
  sta object_type,x
.skip73then
.skipL0241
.L0242 ;;line 521;;  if object_type[index] = TYPE_SPELL3  &&  m_spell_bits_1{2} then object_type[index] = 0

	LDX index
  lda object_type,x
  cmp TYPE_SPELL3
  bne .skipL0242
.condpart75
  lda m_spell_bits_1
  and #4
  beq .skip75then
.condpart76
  lda #0
	LDX index
  sta object_type,x
.skip75then
.skipL0242
.L0243 ;;line 522;;  if object_type[index] = TYPE_SPELL4  &&  m_spell_bits_1{3} then object_type[index] = 0

	LDX index
  lda object_type,x
  cmp TYPE_SPELL4
  bne .skipL0243
.condpart77
  lda m_spell_bits_1
  and #8
  beq .skip77then
.condpart78
  lda #0
	LDX index
  sta object_type,x
.skip77then
.skipL0243
.L0244 ;;line 523;;  if object_type[index] = TYPE_SPELL5  &&  m_spell_bits_1{4} then object_type[index] = 0

	LDX index
  lda object_type,x
  cmp TYPE_SPELL5
  bne .skipL0244
.condpart79
  lda m_spell_bits_1
  and #16
  beq .skip79then
.condpart80
  lda #0
	LDX index
  sta object_type,x
.skip79then
.skipL0244
.L0245 ;;line 524;;  if object_type[index] = TYPE_SPELL6  &&  m_spell_bits_2{0} then object_type[index] = 0

	LDX index
  lda object_type,x
  cmp TYPE_SPELL6
  bne .skipL0245
.condpart81
  lda m_spell_bits_2
  lsr
  bcc .skip81then
.condpart82
  lda #0
	LDX index
  sta object_type,x
.skip81then
.skipL0245
.L0246 ;;line 525;;  if object_type[index] = TYPE_SPELL7  &&  m_spell_bits_2{1} then object_type[index] = 0

	LDX index
  lda object_type,x
  cmp TYPE_SPELL7
  bne .skipL0246
.condpart83
  lda m_spell_bits_2
  and #2
  beq .skip83then
.condpart84
  lda #0
	LDX index
  sta object_type,x
.skip83then
.skipL0246
.L0247 ;;line 526;;  if object_type[index] = TYPE_SPELL8  &&  m_spell_bits_2{2} then object_type[index] = 0

	LDX index
  lda object_type,x
  cmp TYPE_SPELL8
  bne .skipL0247
.condpart85
  lda m_spell_bits_2
  and #4
  beq .skip85then
.condpart86
  lda #0
	LDX index
  sta object_type,x
.skip85then
.skipL0247
.L0248 ;;line 527;;  if object_type[index] = TYPE_SPELL9  &&  m_spell_bits_2{3} then object_type[index] = 0

	LDX index
  lda object_type,x
  cmp TYPE_SPELL9
  bne .skipL0248
.condpart87
  lda m_spell_bits_2
  and #8
  beq .skip87then
.condpart88
  lda #0
	LDX index
  sta object_type,x
.skip87then
.skipL0248
.L0249 ;;line 528;;  if object_type[index] = TYPE_SPELL10  &&  m_spell_bits_2{4} then object_type[index] = 0

	LDX index
  lda object_type,x
  cmp TYPE_SPELL10
  bne .skipL0249
.condpart89
  lda m_spell_bits_2
  and #16
  beq .skip89then
.condpart90
  lda #0
	LDX index
  sta object_type,x
.skip89then
.skipL0249
.L0250 ;;line 529;;  goto remove_duplicates_next

  jmp .remove_duplicates_next
.
 ;;line 530;; 

.check_relic_bits
 ;;line 531;; check_relic_bits

.L0251 ;;line 532;;  if object_type[index] = TYPE_RELIC1  &&  m_relic_bits_1{0} then object_type[index] = 0

	LDX index
  lda object_type,x
  cmp TYPE_RELIC1
  bne .skipL0251
.condpart91
  lda m_relic_bits_1
  lsr
  bcc .skip91then
.condpart92
  lda #0
	LDX index
  sta object_type,x
.skip91then
.skipL0251
.L0252 ;;line 533;;  if object_type[index] = TYPE_RELIC2  &&  m_relic_bits_1{1} then object_type[index] = 0

	LDX index
  lda object_type,x
  cmp TYPE_RELIC2
  bne .skipL0252
.condpart93
  lda m_relic_bits_1
  and #2
  beq .skip93then
.condpart94
  lda #0
	LDX index
  sta object_type,x
.skip93then
.skipL0252
.L0253 ;;line 534;;  if object_type[index] = TYPE_RELIC3  &&  m_relic_bits_1{2} then object_type[index] = 0

	LDX index
  lda object_type,x
  cmp TYPE_RELIC3
  bne .skipL0253
.condpart95
  lda m_relic_bits_1
  and #4
  beq .skip95then
.condpart96
  lda #0
	LDX index
  sta object_type,x
.skip95then
.skipL0253
.L0254 ;;line 535;;  if object_type[index] = TYPE_RELIC4  &&  m_relic_bits_1{3} then object_type[index] = 0

	LDX index
  lda object_type,x
  cmp TYPE_RELIC4
  bne .skipL0254
.condpart97
  lda m_relic_bits_1
  and #8
  beq .skip97then
.condpart98
  lda #0
	LDX index
  sta object_type,x
.skip97then
.skipL0254
.L0255 ;;line 536;;  if object_type[index] = TYPE_RELIC5  &&  m_relic_bits_1{4} then object_type[index] = 0

	LDX index
  lda object_type,x
  cmp TYPE_RELIC5
  bne .skipL0255
.condpart99
  lda m_relic_bits_1
  and #16
  beq .skip99then
.condpart100
  lda #0
	LDX index
  sta object_type,x
.skip99then
.skipL0255
.L0256 ;;line 537;;  if object_type[index] = TYPE_RELIC6  &&  m_relic_bits_2{0} then object_type[index] = 0

	LDX index
  lda object_type,x
  cmp TYPE_RELIC6
  bne .skipL0256
.condpart101
  lda m_relic_bits_2
  lsr
  bcc .skip101then
.condpart102
  lda #0
	LDX index
  sta object_type,x
.skip101then
.skipL0256
.L0257 ;;line 538;;  if object_type[index] = TYPE_RELIC7  &&  m_relic_bits_2{1} then object_type[index] = 0

	LDX index
  lda object_type,x
  cmp TYPE_RELIC7
  bne .skipL0257
.condpart103
  lda m_relic_bits_2
  and #2
  beq .skip103then
.condpart104
  lda #0
	LDX index
  sta object_type,x
.skip103then
.skipL0257
.L0258 ;;line 539;;  if object_type[index] = TYPE_RELIC8  &&  m_relic_bits_2{2} then object_type[index] = 0

	LDX index
  lda object_type,x
  cmp TYPE_RELIC8
  bne .skipL0258
.condpart105
  lda m_relic_bits_2
  and #4
  beq .skip105then
.condpart106
  lda #0
	LDX index
  sta object_type,x
.skip105then
.skipL0258
.L0259 ;;line 540;;  if object_type[index] = TYPE_RELIC9  &&  m_relic_bits_2{3} then object_type[index] = 0

	LDX index
  lda object_type,x
  cmp TYPE_RELIC9
  bne .skipL0259
.condpart107
  lda m_relic_bits_2
  and #8
  beq .skip107then
.condpart108
  lda #0
	LDX index
  sta object_type,x
.skip107then
.skipL0259
.L0260 ;;line 541;;  if object_type[index] = TYPE_RELIC10  &&  m_relic_bits_2{4} then object_type[index] = 0

	LDX index
  lda object_type,x
  cmp TYPE_RELIC10
  bne .skipL0260
.condpart109
  lda m_relic_bits_2
  and #16
  beq .skip109then
.condpart110
  lda #0
	LDX index
  sta object_type,x
.skip109then
.skipL0260
.L0261 ;;line 542;;  goto remove_duplicates_next

  jmp .remove_duplicates_next
.
 ;;line 543;; 

.check_sword_bits
 ;;line 544;; check_sword_bits

.L0262 ;;line 545;;  if object_type[index] = TYPE_SWORD1  &&  m_sword_bits_1{0} then object_type[index] = 0

	LDX index
  lda object_type,x
  cmp TYPE_SWORD1
  bne .skipL0262
.condpart111
  lda m_sword_bits_1
  lsr
  bcc .skip111then
.condpart112
  lda #0
	LDX index
  sta object_type,x
.skip111then
.skipL0262
.L0263 ;;line 546;;  if object_type[index] = TYPE_SWORD2  &&  m_sword_bits_1{1} then object_type[index] = TYPE_ELIXIR

	LDX index
  lda object_type,x
  cmp TYPE_SWORD2
  bne .skipL0263
.condpart113
  lda m_sword_bits_1
  and #2
  beq .skip113then
.condpart114
  lda TYPE_ELIXIR
	LDX index
  sta object_type,x
.skip113then
.skipL0263
.L0264 ;;line 547;;  if object_type[index] = TYPE_SWORD3  &&  m_sword_bits_1{2} then object_type[index] = 0

	LDX index
  lda object_type,x
  cmp TYPE_SWORD3
  bne .skipL0264
.condpart115
  lda m_sword_bits_1
  and #4
  beq .skip115then
.condpart116
  lda #0
	LDX index
  sta object_type,x
.skip115then
.skipL0264
.L0265 ;;line 548;;  if object_type[index] = TYPE_SWORD4  &&  m_sword_bits_1{3} then object_type[index] = 0

	LDX index
  lda object_type,x
  cmp TYPE_SWORD4
  bne .skipL0265
.condpart117
  lda m_sword_bits_1
  and #8
  beq .skip117then
.condpart118
  lda #0
	LDX index
  sta object_type,x
.skip117then
.skipL0265
.L0266 ;;line 549;;  if object_type[index] = TYPE_SWORD5  &&  m_sword_bits_1{4} then object_type[index] = 0

	LDX index
  lda object_type,x
  cmp TYPE_SWORD5
  bne .skipL0266
.condpart119
  lda m_sword_bits_1
  and #16
  beq .skip119then
.condpart120
  lda #0
	LDX index
  sta object_type,x
.skip119then
.skipL0266
.L0267 ;;line 550;;  if object_type[index] = TYPE_SWORD6  &&  m_sword_bits_2{0} then object_type[index] = 0

	LDX index
  lda object_type,x
  cmp TYPE_SWORD6
  bne .skipL0267
.condpart121
  lda m_sword_bits_2
  lsr
  bcc .skip121then
.condpart122
  lda #0
	LDX index
  sta object_type,x
.skip121then
.skipL0267
.L0268 ;;line 551;;  if object_type[index] = TYPE_SWORD7  &&  m_sword_bits_2{1} then object_type[index] = 0

	LDX index
  lda object_type,x
  cmp TYPE_SWORD7
  bne .skipL0268
.condpart123
  lda m_sword_bits_2
  and #2
  beq .skip123then
.condpart124
  lda #0
	LDX index
  sta object_type,x
.skip123then
.skipL0268
.L0269 ;;line 552;;  if object_type[index] = TYPE_SWORD8  &&  m_sword_bits_2{2} then object_type[index] = 0

	LDX index
  lda object_type,x
  cmp TYPE_SWORD8
  bne .skipL0269
.condpart125
  lda m_sword_bits_2
  and #4
  beq .skip125then
.condpart126
  lda #0
	LDX index
  sta object_type,x
.skip125then
.skipL0269
.L0270 ;;line 553;;  if object_type[index] = TYPE_SWORD9  &&  m_sword_bits_2{3} then object_type[index] = 0

	LDX index
  lda object_type,x
  cmp TYPE_SWORD9
  bne .skipL0270
.condpart127
  lda m_sword_bits_2
  and #8
  beq .skip127then
.condpart128
  lda #0
	LDX index
  sta object_type,x
.skip127then
.skipL0270
.L0271 ;;line 554;;  if object_type[index] = TYPE_SWORD10  &&  m_sword_bits_2{4} then object_type[index] = 0

	LDX index
  lda object_type,x
  cmp TYPE_SWORD10
  bne .skipL0271
.condpart129
  lda m_sword_bits_2
  and #16
  beq .skip129then
.condpart130
  lda #0
	LDX index
  sta object_type,x
.skip129then
.skipL0271
.L0272 ;;line 555;;  goto remove_duplicates_next

  jmp .remove_duplicates_next
.
 ;;line 556;; 

.check_shield_bits
 ;;line 557;; check_shield_bits

.L0273 ;;line 558;;  if object_type[index] = TYPE_SHIELD1  &&  m_shield_bits_1{0} then object_type[index] = 0

	LDX index
  lda object_type,x
  cmp TYPE_SHIELD1
  bne .skipL0273
.condpart131
  lda m_shield_bits_1
  lsr
  bcc .skip131then
.condpart132
  lda #0
	LDX index
  sta object_type,x
.skip131then
.skipL0273
.L0274 ;;line 559;;  if object_type[index] = TYPE_SHIELD2  &&  m_shield_bits_1{1} then object_type[index] = TYPE_POTION

	LDX index
  lda object_type,x
  cmp TYPE_SHIELD2
  bne .skipL0274
.condpart133
  lda m_shield_bits_1
  and #2
  beq .skip133then
.condpart134
  lda TYPE_POTION
	LDX index
  sta object_type,x
.skip133then
.skipL0274
.L0275 ;;line 560;;  if object_type[index] = TYPE_SHIELD3  &&  m_shield_bits_1{2} then object_type[index] = 0

	LDX index
  lda object_type,x
  cmp TYPE_SHIELD3
  bne .skipL0275
.condpart135
  lda m_shield_bits_1
  and #4
  beq .skip135then
.condpart136
  lda #0
	LDX index
  sta object_type,x
.skip135then
.skipL0275
.L0276 ;;line 561;;  if object_type[index] = TYPE_SHIELD4  &&  m_shield_bits_1{3} then object_type[index] = 0

	LDX index
  lda object_type,x
  cmp TYPE_SHIELD4
  bne .skipL0276
.condpart137
  lda m_shield_bits_1
  and #8
  beq .skip137then
.condpart138
  lda #0
	LDX index
  sta object_type,x
.skip137then
.skipL0276
.L0277 ;;line 562;;  if object_type[index] = TYPE_SHIELD5  &&  m_shield_bits_1{4} then object_type[index] = 0

	LDX index
  lda object_type,x
  cmp TYPE_SHIELD5
  bne .skipL0277
.condpart139
  lda m_shield_bits_1
  and #16
  beq .skip139then
.condpart140
  lda #0
	LDX index
  sta object_type,x
.skip139then
.skipL0277
.L0278 ;;line 563;;  if object_type[index] = TYPE_SHIELD6  &&  m_shield_bits_2{0} then object_type[index] = 0

	LDX index
  lda object_type,x
  cmp TYPE_SHIELD6
  bne .skipL0278
.condpart141
  lda m_shield_bits_2
  lsr
  bcc .skip141then
.condpart142
  lda #0
	LDX index
  sta object_type,x
.skip141then
.skipL0278
.L0279 ;;line 564;;  if object_type[index] = TYPE_SHIELD7  &&  m_shield_bits_2{1} then object_type[index] = 0

	LDX index
  lda object_type,x
  cmp TYPE_SHIELD7
  bne .skipL0279
.condpart143
  lda m_shield_bits_2
  and #2
  beq .skip143then
.condpart144
  lda #0
	LDX index
  sta object_type,x
.skip143then
.skipL0279
.L0280 ;;line 565;;  if object_type[index] = TYPE_SHIELD8  &&  m_shield_bits_2{2} then object_type[index] = 0

	LDX index
  lda object_type,x
  cmp TYPE_SHIELD8
  bne .skipL0280
.condpart145
  lda m_shield_bits_2
  and #4
  beq .skip145then
.condpart146
  lda #0
	LDX index
  sta object_type,x
.skip145then
.skipL0280
.L0281 ;;line 566;;  if object_type[index] = TYPE_SHIELD9  &&  m_shield_bits_2{3} then object_type[index] = 0

	LDX index
  lda object_type,x
  cmp TYPE_SHIELD9
  bne .skipL0281
.condpart147
  lda m_shield_bits_2
  and #8
  beq .skip147then
.condpart148
  lda #0
	LDX index
  sta object_type,x
.skip147then
.skipL0281
.L0282 ;;line 567;;  if object_type[index] = TYPE_SHIELD10  &&  m_shield_bits_2{4} then object_type[index] = 0

	LDX index
  lda object_type,x
  cmp TYPE_SHIELD10
  bne .skipL0282
.condpart149
  lda m_shield_bits_2
  and #16
  beq .skip149then
.condpart150
  lda #0
	LDX index
  sta object_type,x
.skip149then
.skipL0282
.L0283 ;;line 568;;  goto remove_duplicates_next

  jmp .remove_duplicates_next
.
 ;;line 569;; 

.check_armor_bits
 ;;line 570;; check_armor_bits

.L0284 ;;line 571;;  if object_type[index] = TYPE_ARMOR1  &&  m_armor_bits_1{0} then object_type[index] = 0

	LDX index
  lda object_type,x
  cmp TYPE_ARMOR1
  bne .skipL0284
.condpart151
  lda m_armor_bits_1
  lsr
  bcc .skip151then
.condpart152
  lda #0
	LDX index
  sta object_type,x
.skip151then
.skipL0284
.L0285 ;;line 572;;  if object_type[index] = TYPE_ARMOR2  &&  m_armor_bits_1{1} then object_type[index] = 0

	LDX index
  lda object_type,x
  cmp TYPE_ARMOR2
  bne .skipL0285
.condpart153
  lda m_armor_bits_1
  and #2
  beq .skip153then
.condpart154
  lda #0
	LDX index
  sta object_type,x
.skip153then
.skipL0285
.L0286 ;;line 573;;  if object_type[index] = TYPE_ARMOR3  &&  m_armor_bits_1{2} then object_type[index] = 0

	LDX index
  lda object_type,x
  cmp TYPE_ARMOR3
  bne .skipL0286
.condpart155
  lda m_armor_bits_1
  and #4
  beq .skip155then
.condpart156
  lda #0
	LDX index
  sta object_type,x
.skip155then
.skipL0286
.L0287 ;;line 574;;  if object_type[index] = TYPE_ARMOR4  &&  m_armor_bits_1{3} then object_type[index] = 0

	LDX index
  lda object_type,x
  cmp TYPE_ARMOR4
  bne .skipL0287
.condpart157
  lda m_armor_bits_1
  and #8
  beq .skip157then
.condpart158
  lda #0
	LDX index
  sta object_type,x
.skip157then
.skipL0287
.L0288 ;;line 575;;  if object_type[index] = TYPE_ARMOR5  &&  m_armor_bits_1{4} then object_type[index] = 0

	LDX index
  lda object_type,x
  cmp TYPE_ARMOR5
  bne .skipL0288
.condpart159
  lda m_armor_bits_1
  and #16
  beq .skip159then
.condpart160
  lda #0
	LDX index
  sta object_type,x
.skip159then
.skipL0288
.L0289 ;;line 576;;  if object_type[index] = TYPE_ARMOR6  &&  m_armor_bits_2{0} then object_type[index] = 0

	LDX index
  lda object_type,x
  cmp TYPE_ARMOR6
  bne .skipL0289
.condpart161
  lda m_armor_bits_2
  lsr
  bcc .skip161then
.condpart162
  lda #0
	LDX index
  sta object_type,x
.skip161then
.skipL0289
.L0290 ;;line 577;;  if object_type[index] = TYPE_ARMOR7  &&  m_armor_bits_2{1} then object_type[index] = 0

	LDX index
  lda object_type,x
  cmp TYPE_ARMOR7
  bne .skipL0290
.condpart163
  lda m_armor_bits_2
  and #2
  beq .skip163then
.condpart164
  lda #0
	LDX index
  sta object_type,x
.skip163then
.skipL0290
.L0291 ;;line 578;;  if object_type[index] = TYPE_ARMOR8  &&  m_armor_bits_2{2} then object_type[index] = 0

	LDX index
  lda object_type,x
  cmp TYPE_ARMOR8
  bne .skipL0291
.condpart165
  lda m_armor_bits_2
  and #4
  beq .skip165then
.condpart166
  lda #0
	LDX index
  sta object_type,x
.skip165then
.skipL0291
.L0292 ;;line 579;;  if object_type[index] = TYPE_ARMOR9  &&  m_armor_bits_2{3} then object_type[index] = 0

	LDX index
  lda object_type,x
  cmp TYPE_ARMOR9
  bne .skipL0292
.condpart167
  lda m_armor_bits_2
  and #8
  beq .skip167then
.condpart168
  lda #0
	LDX index
  sta object_type,x
.skip167then
.skipL0292
.L0293 ;;line 580;;  if object_type[index] = TYPE_ARMOR10  &&  m_armor_bits_2{4} then object_type[index] = 0

	LDX index
  lda object_type,x
  cmp TYPE_ARMOR10
  bne .skipL0293
.condpart169
  lda m_armor_bits_2
  and #16
  beq .skip169then
.condpart170
  lda #0
	LDX index
  sta object_type,x
.skip169then
.skipL0293
.L0294 ;;line 581;;  goto remove_duplicates_next

  jmp .remove_duplicates_next
.
 ;;line 582;; 

.check_charm_bits
 ;;line 583;; check_charm_bits

.L0295 ;;line 584;;  if object_type[index] = TYPE_CHARM1  &&  m_charm_bits_1{0} then object_type[index] = 0

	LDX index
  lda object_type,x
  cmp TYPE_CHARM1
  bne .skipL0295
.condpart171
  lda m_charm_bits_1
  lsr
  bcc .skip171then
.condpart172
  lda #0
	LDX index
  sta object_type,x
.skip171then
.skipL0295
.L0296 ;;line 585;;  if object_type[index] = TYPE_CHARM2  &&  m_charm_bits_1{1} then object_type[index] = TYPE_ELIXIR

	LDX index
  lda object_type,x
  cmp TYPE_CHARM2
  bne .skipL0296
.condpart173
  lda m_charm_bits_1
  and #2
  beq .skip173then
.condpart174
  lda TYPE_ELIXIR
	LDX index
  sta object_type,x
.skip173then
.skipL0296
.L0297 ;;line 586;;  if object_type[index] = TYPE_CHARM3  &&  m_charm_bits_1{2} then object_type[index] = 0

	LDX index
  lda object_type,x
  cmp TYPE_CHARM3
  bne .skipL0297
.condpart175
  lda m_charm_bits_1
  and #4
  beq .skip175then
.condpart176
  lda #0
	LDX index
  sta object_type,x
.skip175then
.skipL0297
.L0298 ;;line 587;;  if object_type[index] = TYPE_CHARM4  &&  m_charm_bits_1{3} then object_type[index] = 0

	LDX index
  lda object_type,x
  cmp TYPE_CHARM4
  bne .skipL0298
.condpart177
  lda m_charm_bits_1
  and #8
  beq .skip177then
.condpart178
  lda #0
	LDX index
  sta object_type,x
.skip177then
.skipL0298
.L0299 ;;line 588;;  if object_type[index] = TYPE_CHARM5  &&  m_charm_bits_1{4} then object_type[index] = 0

	LDX index
  lda object_type,x
  cmp TYPE_CHARM5
  bne .skipL0299
.condpart179
  lda m_charm_bits_1
  and #16
  beq .skip179then
.condpart180
  lda #0
	LDX index
  sta object_type,x
.skip179then
.skipL0299
.L0300 ;;line 589;;  if object_type[index] = TYPE_CHARM6  &&  m_charm_bits_2{0} then object_type[index] = 0

	LDX index
  lda object_type,x
  cmp TYPE_CHARM6
  bne .skipL0300
.condpart181
  lda m_charm_bits_2
  lsr
  bcc .skip181then
.condpart182
  lda #0
	LDX index
  sta object_type,x
.skip181then
.skipL0300
.L0301 ;;line 590;;  if object_type[index] = TYPE_CHARM7  &&  m_charm_bits_2{1} then object_type[index] = 0

	LDX index
  lda object_type,x
  cmp TYPE_CHARM7
  bne .skipL0301
.condpart183
  lda m_charm_bits_2
  and #2
  beq .skip183then
.condpart184
  lda #0
	LDX index
  sta object_type,x
.skip183then
.skipL0301
.L0302 ;;line 591;;  if object_type[index] = TYPE_CHARM8  &&  m_charm_bits_2{2} then object_type[index] = 0

	LDX index
  lda object_type,x
  cmp TYPE_CHARM8
  bne .skipL0302
.condpart185
  lda m_charm_bits_2
  and #4
  beq .skip185then
.condpart186
  lda #0
	LDX index
  sta object_type,x
.skip185then
.skipL0302
.L0303 ;;line 592;;  if object_type[index] = TYPE_CHARM9  &&  m_charm_bits_2{3} then object_type[index] = 0

	LDX index
  lda object_type,x
  cmp TYPE_CHARM9
  bne .skipL0303
.condpart187
  lda m_charm_bits_2
  and #8
  beq .skip187then
.condpart188
  lda #0
	LDX index
  sta object_type,x
.skip187then
.skipL0303
.L0304 ;;line 593;;  if object_type[index] = TYPE_CHARM10  &&  m_charm_bits_2{4} then object_type[index] = 0

	LDX index
  lda object_type,x
  cmp TYPE_CHARM10
  bne .skipL0304
.condpart189
  lda m_charm_bits_2
  and #16
  beq .skip189then
.condpart190
  lda #0
	LDX index
  sta object_type,x
.skip189then
.skipL0304
.L0305 ;;line 594;;  goto remove_duplicates_next

  jmp .remove_duplicates_next
.
 ;;line 595;; 

.obj_torch_stats
 ;;line 596;; obj_torch_stats

.L0306 ;;line 597;;  object_hp[obj_index] = 1

  lda #1
	LDX obj_index
  sta object_hp,x
.L0307 ;;line 598;;  asm

          ldx obj_index

          lda #<.plot_torch

          sta object_spr_ptr_lo,x

          lda #>.plot_torch

          sta object_spr_ptr_hi,x

.L0308 ;;line 605;;  goto obj_next_stats

  jmp .obj_next_stats
.
 ;;line 606;; 

.magic_collision
 ;;line 607;; magic_collision

.L0309 ;;line 608;;  inline magic_collision.asm

 include magic_collision.asm
included.magic_collision.asm.bank = 0
.
 ;;line 609;; 

.cast_fireball
 ;;line 610;; cast_fireball

.L0310 ;;line 611;;  m_spell_bits_1{0} = 1

  lda m_spell_bits_1
  ora #1
  sta m_spell_bits_1
.L0311 ;;line 612;;  playsfx sfx_bubbleup

 ifnconst NOTIALOCKMUTE
    lda #1
    sta sfxschedulelock
    lda #<sfx_bubbleup
    sta sfxinstrumentlo
    lda #>sfx_bubbleup
    sta sfxinstrumenthi
    lda #0
    sta sfxpitchoffset ; no pitch modification
    sta sfxnoteindex ; not a musical note
    jsr schedulesfx
    lda #0
    sta sfxschedulelock
 endif ; NOTIALOCKMUTE
.L0312 ;;line 613;;  asm

        lda #FIREBALL_MP_COST

        sta mp_change

        jsr .test_spell_cost

        bne .load_fireball_index

        jmp .test_joy_done ; allow player to try a different input

.load_fireball_index

        ldx #14

        ldy #0  ; player index

load_fireball_data

        lda #TYPE_FIREBALL

        sta object_type,x

        lda #FIREBALL_SPEED_HI

        sta object_vel_cap_hi,x

        sta object_speed_hi,x

        lda #FIREBALL_SPEED_LO

        sta object_vel_cap_lo,x

        sta object_speed_lo,x

        lda #FIREBALL_DAMAGE

        clc

        adc player_wisdom

        sta object_damage,x

        lda #%00010000

        sta object_flags,x

        lda object_elevation,y

        sta object_elevation,x

        lda #<.plot_fireball

        sta object_spr_ptr_lo,x

        lda #>.plot_fireball

        sta object_spr_ptr_hi,x

set_cast_direction

        lda player_facing,y

        cmp #DIR_UP

        beq cast_fireball_up

        cmp #DIR_DOWN

        beq cast_fireball_down

        cmp #DIR_LEFT

        bne cast_fireball_right

        jmp cast_fireball_left  ; branch out of range

cast_fireball_right

        sta object_facing,x

        sta object_Xdir,x

        lda player_Xpos,y

        clc

        adc #14

        sta object_Xpos,x

        lda player_Ypos,y

        sta object_Ypos,x

        lda object_speed_hi,x

        sta object_Xvel_hi,x

        lda object_speed_lo,x

        sta object_Xvel_lo,x

        lda #0

        sta object_Ydir,x

        sta object_Yvel_hi,x

        sta object_Yvel_lo,x

        rts

cast_fireball_up

        sta object_facing,x

        sta object_Ydir,x

        lda player_Xpos,y

        clc

        adc #2

        sta object_Xpos,x

        lda player_Ypos,y

        sec

        sbc #10

        sta object_Ypos,x

        lda #0

        sta object_Xdir,x

        sta object_Xvel_hi,x

        sta object_Xvel_lo,x

        lda object_speed_lo,x

        sta object_Yvel_lo,x

        lda object_speed_hi,x

        sta object_Yvel_hi,x

        rts

cast_fireball_down

        sta object_facing,x

        sta object_Ydir,x

        lda player_Xpos,y

        clc

        adc #2

        sta object_Xpos,x

        lda player_Ypos,y

        clc

        adc #10

        sta object_Ypos,x

        lda #0

        sta object_Xdir,x

        sta object_Xvel_hi,x

        sta object_Xvel_lo,x

        lda object_speed_lo,x

        sta object_Yvel_lo,x

        lda object_speed_hi,x

        sta object_Yvel_hi,x

        rts

cast_fireball_left

        sta object_facing,x

        sta object_Xdir,x

        lda player_Xpos,y

        sec

        sbc #8

        sta object_Xpos,x

        lda player_Ypos,y

        sta object_Ypos,x

        lda object_speed_hi,x

        sta object_Xvel_hi,x

        lda object_speed_lo,x

        sta object_Xvel_lo,x

        lda #0

        sta object_Ydir,x

        sta object_Yvel_hi,x

        sta object_Yvel_lo,x

        rts

.
 ;;line 729;; 

.
 ;;line 730;; 

.
 ;;line 731;; 

.
 ;;line 732;; 

.
 ;;line 733;; 

.
 ;;line 734;; 

.fairy_ai
 ;;line 735;; fairy_ai

.L0313 ;;line 736;;  timer = timer  +  1

  lda timer
	CLC
	ADC #1
  sta timer
.L0314 ;;line 737;;  if timer  <  FAIRY_ITEM_TIME then fairy_move

  lda timer
  cmp FAIRY_ITEM_TIME
 if ((* - .fairy_move) < 127) && ((* - .fairy_move) > -128)
  bcc .fairy_move
 else
  bcs .20skipfairy_move
  jmp .fairy_move
.20skipfairy_move
 endif
.L0315 ;;line 738;;  timer = 0

  lda #0
  sta timer
.
 ;;line 739;; 

.fairy_pick_item
 ;;line 740;; fairy_pick_item

.
 ;;line 741;; 

.L0316 ;;line 742;;  if player_state = STATE_DEATH  &&  m_item_qty[10] then temp2 = 10  :  goto fairy_revive

  lda player_state
  cmp STATE_DEATH
  bne .skipL0316
.condpart191
	LDX #10
  lda m_item_qty,x
  beq .skip191then
.condpart192
  lda #10
  sta temp2
  jmp .fairy_revive
.skip191then
.skipL0316
.L0317 ;;line 743;;  if player_status{3}  &&  m_item_qty[9] then temp1 = MASK_STONE  :  temp2 = 9  :  goto fairy_use_heal

  lda player_status
  and #8
  beq .skipL0317
.condpart193
	LDX #9
  lda m_item_qty,x
  beq .skip193then
.condpart194
  lda MASK_STONE
  sta temp1
  lda #9
  sta temp2
  jmp .fairy_use_heal
.skip193then
.skipL0317
.L0318 ;;line 744;;  if player_status{0}  &&  m_item_qty[6] then temp1 = MASK_POISON  :  temp2 = 6  :  goto fairy_use_heal

  lda player_status
  lsr
  bcc .skipL0318
.condpart195
	LDX #6
  lda m_item_qty,x
  beq .skip195then
.condpart196
  lda MASK_POISON
  sta temp1
  lda #6
  sta temp2
  jmp .fairy_use_heal
.skip195then
.skipL0318
.L0319 ;;line 745;;  if player_status{1}  &&  m_item_qty[7] then temp1 = MASK_DARK  :  temp2 = 7  :  goto fairy_use_heal

  lda player_status
  and #2
  beq .skipL0319
.condpart197
	LDX #7
  lda m_item_qty,x
  beq .skip197then
.condpart198
  lda MASK_DARK
  sta temp1
  lda #7
  sta temp2
  jmp .fairy_use_heal
.skip197then
.skipL0319
.L0320 ;;line 746;;  if player_status{2}  &&  m_item_qty[8] then temp1 = MASK_CURSE  :  temp2 = 8  :  goto fairy_use_heal

  lda player_status
  and #4
  beq .skipL0320
.condpart199
	LDX #8
  lda m_item_qty,x
  beq .skip199then
.condpart200
  lda MASK_CURSE
  sta temp1
  lda #8
  sta temp2
  jmp .fairy_use_heal
.skip199then
.skipL0320
.L0321 ;;line 747;;  if player_hp  <  PLAYER_HP_QUARTER  &&  m_item_qty[2] then temp2 = 2  :  goto fairy_use_hipotion

  lda player_hp
  cmp PLAYER_HP_QUARTER
  bcs .skipL0321
.condpart201
	LDX #2
  lda m_item_qty,x
  beq .skip201then
.condpart202
  lda #2
  sta temp2
  jmp .fairy_use_hipotion
.skip201then
.skipL0321
.L0322 ;;line 748;;  if player_hp  <  PLAYER_HP_HALF  &&  m_item_qty[1] then temp2 = 1  :  goto fairy_use_potion

  lda player_hp
  cmp PLAYER_HP_HALF
  bcs .skipL0322
.condpart203
	LDX #1
  lda m_item_qty,x
  beq .skip203then
.condpart204
  lda #1
  sta temp2
  jmp .fairy_use_potion
.skip203then
.skipL0322
.L0323 ;;line 749;;  if player_mp  <  PLAYER_MP_QUARTER  &&  m_item_qty[4] then temp2 = 4  :  goto fairy_use_hiether

  lda player_mp
  cmp PLAYER_MP_QUARTER
  bcs .skipL0323
.condpart205
	LDX #4
  lda m_item_qty,x
  beq .skip205then
.condpart206
  lda #4
  sta temp2
  jmp .fairy_use_hiether
.skip205then
.skipL0323
.L0324 ;;line 750;;  if player_mp  <  PLAYER_MP_HALF  &&  m_item_qty[3] then temp2 = 3  :  goto fairy_use_ether

  lda player_mp
  cmp PLAYER_MP_HALF
  bcs .skipL0324
.condpart207
	LDX #3
  lda m_item_qty,x
  beq .skip207then
.condpart208
  lda #3
  sta temp2
  jmp .fairy_use_ether
.skip207then
.skipL0324
.
 ;;line 751;; 

.L0325 ;;line 752;;  goto fairy_item_done

  jmp .fairy_item_done
.
 ;;line 753;; 

.fairy_item_used
 ;;line 754;; fairy_item_used

.
 ;;line 755;; 

.L0326 ;;line 756;;  asm

          ldx temp2

          dec m_item_qty,x

.
 ;;line 760;; 

.L0327 ;;line 761;;  if animation_frame then frame = F_FAIRY_ITEM2 else frame = F_FAIRY_ITEM1

  lda animation_frame
  beq .skipL0327
.condpart209
  lda F_FAIRY_ITEM2
  sta frame
  jmp .skipelse1
.skipL0327
  lda F_FAIRY_ITEM1
  sta frame
.skipelse1
.L0328 ;;line 762;;  gosub check_item_qtys

  jsr .check_item_qtys
.L0329 ;;line 763;;  playsfx sfx_bubbleup

 ifnconst NOTIALOCKMUTE
    lda #1
    sta sfxschedulelock
    lda #<sfx_bubbleup
    sta sfxinstrumentlo
    lda #>sfx_bubbleup
    sta sfxinstrumenthi
    lda #0
    sta sfxpitchoffset ; no pitch modification
    sta sfxnoteindex ; not a musical note
    jsr schedulesfx
    lda #0
    sta sfxschedulelock
 endif ; NOTIALOCKMUTE
.
 ;;line 764;; 

.fairy_item_done
 ;;line 765;; fairy_item_done

.
 ;;line 766;; 

.fairy_move
 ;;line 767;; fairy_move

.L0330 ;;line 768;;  if animation_frame then frame = F_FAIRY_WING1  :  return thisbank

  lda animation_frame
  beq .skipL0330
.condpart210
  lda F_FAIRY_WING1
  sta frame
  rts
.skipL0330
.L0331 ;;line 769;;  frame = F_FAIRY_WING2

  lda F_FAIRY_WING2
  sta frame
.
 ;;line 770;; 

.
 ;;line 771;; 

.L0332 ;;line 772;;  goto test_follow_box

  jmp .test_follow_box
.
 ;;line 773;; 

.fairy_revive
 ;;line 774;; fairy_revive

.
 ;;line 775;; 

.L0333 ;;line 776;;  player_state = STATE_STAND

  lda STATE_STAND
  sta player_state
.L0334 ;;line 777;;  hp_change = PLAYER_HP_MAX

  lda PLAYER_HP_MAX
  sta hp_change
.L0335 ;;line 778;;  gosub increase_player_hp

  jsr .increase_player_hp
.L0336 ;;line 779;;  fade_count = 0

  lda #0
  sta fade_count
.L0337 ;;line 780;;  goto fairy_item_used

  jmp .fairy_item_used
.
 ;;line 781;; 

.fairy_use_heal
 ;;line 782;; fairy_use_heal

.L0338 ;;line 783;;  asm

          lda player_status

          and temp1

          sta player_status

.L0339 ;;line 788;;  goto fairy_item_used

  jmp .fairy_item_used
.
 ;;line 789;; 

.fairy_use_hipotion
 ;;line 790;; fairy_use_hipotion

.L0340 ;;line 791;;  hp_change = HP_CHANGE_HIPOTION

  lda HP_CHANGE_HIPOTION
  sta hp_change
.L0341 ;;line 792;;  gosub increase_player_hp

  jsr .increase_player_hp
.L0342 ;;line 793;;  goto fairy_item_used

  jmp .fairy_item_used
.
 ;;line 794;; 

.fairy_use_potion
 ;;line 795;; fairy_use_potion

.L0343 ;;line 796;;  hp_change = HP_CHANGE_POTION

  lda HP_CHANGE_POTION
  sta hp_change
.L0344 ;;line 797;;  gosub increase_player_hp

  jsr .increase_player_hp
.L0345 ;;line 798;;  goto fairy_item_used

  jmp .fairy_item_used
.
 ;;line 799;; 

.fairy_use_hiether
 ;;line 800;; fairy_use_hiether

.L0346 ;;line 801;;  mp_change = MP_CHANGE_XETHER

  lda MP_CHANGE_XETHER
  sta mp_change
.L0347 ;;line 802;;  gosub increase_player_mp

  jsr .increase_player_mp
.L0348 ;;line 803;;  goto fairy_item_used

  jmp .fairy_item_used
.
 ;;line 804;; 

.fairy_use_ether
 ;;line 805;; fairy_use_ether

.L0349 ;;line 806;;  mp_change = MP_CHANGE_ETHER

  lda MP_CHANGE_ETHER
  sta mp_change
.L0350 ;;line 807;;  gosub increase_player_mp

  jsr .increase_player_mp
.L0351 ;;line 808;;  goto fairy_item_used

  jmp .fairy_item_used
.
 ;;line 809;; 

.obj_fairy_stats
 ;;line 810;; obj_fairy_stats

.L0352 ;;line 811;;  object_hp[obj_index] = FAIRY_HP

  lda FAIRY_HP
	LDX obj_index
  sta object_hp,x
.L0353 ;;line 812;;  object_vel_cap_hi[obj_index] = FAIRY_VEL_CAP_HI

  lda FAIRY_VEL_CAP_HI
	LDX obj_index
  sta object_vel_cap_hi,x
.L0354 ;;line 813;;  object_vel_cap_lo[obj_index] = FAIRY_VEL_CAP_LO

  lda FAIRY_VEL_CAP_LO
	LDX obj_index
  sta object_vel_cap_lo,x
.L0355 ;;line 814;;  object_damage[obj_index] = FAIRY_DAMAGE

  lda FAIRY_DAMAGE
	LDX obj_index
  sta object_damage,x
.L0356 ;;line 815;;  object_def[obj_index] = FAIRY_DEF

  lda FAIRY_DEF
	LDX obj_index
  sta object_def,x
.L0357 ;;line 816;;  object_mdef[obj_index] = FAIRY_MDEF

  lda FAIRY_MDEF
	LDX obj_index
  sta object_mdef,x
.L0358 ;;line 817;;  object_friction_hi[obj_index] = FAIRY_FRICTION_HI

  lda FAIRY_FRICTION_HI
	LDX obj_index
  sta object_friction_hi,x
.L0359 ;;line 818;;  object_friction_lo[obj_index] = FAIRY_FRICTION_LO

  lda FAIRY_FRICTION_LO
	LDX obj_index
  sta object_friction_lo,x
.L0360 ;;line 819;;  object_speed_hi[obj_index] = FAIRY_SPEED_HI

  lda FAIRY_SPEED_HI
	LDX obj_index
  sta object_speed_hi,x
.L0361 ;;line 820;;  object_speed_lo[obj_index] = FAIRY_SPEED_LO

  lda FAIRY_SPEED_LO
	LDX obj_index
  sta object_speed_lo,x
.L0362 ;;line 821;;  object_flags[obj_index] = 0

  lda #0
	LDX obj_index
  sta object_flags,x
.L0363 ;;line 822;;  object_flags[obj_index] = %00000100

  lda #%00000100
	LDX obj_index
  sta object_flags,x
.L0364 ;;line 823;;  asm

          ldx obj_index

          lda #<.plot_fairy

          sta object_spr_ptr_lo,x

          lda #>.plot_fairy

          sta object_spr_ptr_hi,x

.L0365 ;;line 830;;  goto obj_next_stats

  jmp .obj_next_stats
.
 ;;line 831;; 

.plot_fairy
 ;;line 832;; plot_fairy

.L0366 ;;line 833;;  plotsprite fairy0 0 Xposition Yposition frame

    lda #<fairy0
    ldy #fairy0_width
    beq plotspritewidthskip4
plotspritewidthloop4
      clc
      adc frame
      dey
      bne plotspritewidthloop4
plotspritewidthskip4
    sta temp1

    lda #>fairy0
    sta temp2

    lda #(0|fairy0_width_twoscompliment)
    sta temp3

    lda Xposition
    sta temp4

    lda Yposition
    sta temp5

    lda #(fairy0_mode|%01000000)
    sta temp6

 jsr plotsprite
.L0367 ;;line 834;;  goto plot_next

  jmp .plot_next
.
 ;;line 835;; 

.check_item_qtys
 ;;line 836;; check_item_qtys

.L0368 ;;line 837;;  if !m_item_qty[1] then m_item_bits_1{0} = 0

	LDX #1
  lda m_item_qty,x
  bne .skipL0368
.condpart211
  lda m_item_bits_1
  and #254
  sta m_item_bits_1
.skipL0368
.L0369 ;;line 838;;  if !m_item_qty[2] then m_item_bits_1{1} = 0

	LDX #2
  lda m_item_qty,x
  bne .skipL0369
.condpart212
  lda m_item_bits_1
  and #253
  sta m_item_bits_1
.skipL0369
.L0370 ;;line 839;;  if !m_item_qty[3] then m_item_bits_1{2} = 0

	LDX #3
  lda m_item_qty,x
  bne .skipL0370
.condpart213
  lda m_item_bits_1
  and #251
  sta m_item_bits_1
.skipL0370
.L0371 ;;line 840;;  if !m_item_qty[4] then m_item_bits_1{3} = 0

	LDX #4
  lda m_item_qty,x
  bne .skipL0371
.condpart214
  lda m_item_bits_1
  and #247
  sta m_item_bits_1
.skipL0371
.L0372 ;;line 841;;  if !m_item_qty[5] then m_item_bits_1{4} = 0

	LDX #5
  lda m_item_qty,x
  bne .skipL0372
.condpart215
  lda m_item_bits_1
  and #239
  sta m_item_bits_1
.skipL0372
.L0373 ;;line 842;;  if !m_item_qty[6] then m_item_bits_2{0} = 0

	LDX #6
  lda m_item_qty,x
  bne .skipL0373
.condpart216
  lda m_item_bits_2
  and #254
  sta m_item_bits_2
.skipL0373
.L0374 ;;line 843;;  if !m_item_qty[7] then m_item_bits_2{1} = 0

	LDX #7
  lda m_item_qty,x
  bne .skipL0374
.condpart217
  lda m_item_bits_2
  and #253
  sta m_item_bits_2
.skipL0374
.L0375 ;;line 844;;  if !m_item_qty[8] then m_item_bits_2{2} = 0

	LDX #8
  lda m_item_qty,x
  bne .skipL0375
.condpart218
  lda m_item_bits_2
  and #251
  sta m_item_bits_2
.skipL0375
.L0376 ;;line 845;;  if !m_item_qty[9] then m_item_bits_2{3} = 0

	LDX #9
  lda m_item_qty,x
  bne .skipL0376
.condpart219
  lda m_item_bits_2
  and #247
  sta m_item_bits_2
.skipL0376
.L0377 ;;line 846;;  if !m_item_qty[10] then m_item_bits_2{4} = 0

	LDX #10
  lda m_item_qty,x
  bne .skipL0377
.condpart220
  lda m_item_bits_2
  and #239
  sta m_item_bits_2
.skipL0377
.L0378 ;;line 847;;  return thisbank

  rts
.
 ;;line 848;; 

.
 ;;line 849;; 

.swap_action_button
 ;;line 850;; swap_action_button

.L0379 ;;line 851;;  select_debounce = 1

  lda #1
  sta select_debounce
.
 ;;line 852;; 

.
 ;;line 853;; 

.L0380 ;;line 854;;  if player_state  >=  STATE_ATTACK then return thisbank

  lda player_state
  cmp STATE_ATTACK
  bcc .skipL0380
.condpart221
  rts
.skipL0380
.L0381 ;;line 855;;  if action_button then action_button = 0  :  return thisbank

  lda action_button
  beq .skipL0381
.condpart222
  lda #0
  sta action_button
  rts
.skipL0381
.L0382 ;;line 856;;  action_button = 1  :  return thisbank

  lda #1
  sta action_button
  rts
.
 ;;line 857;; 

.topscreenroutine
 ;;line 858;; topscreenroutine

.
 ;;line 859;; 

.L0383 ;;line 860;;  WSYNC = 1

  lda #1
  sta WSYNC
.L0384 ;;line 861;;  if game_state = STATE_MENU then set_menu_top_pal

  lda game_state
  cmp STATE_MENU
 if ((* - .set_menu_top_pal) < 127) && ((* - .set_menu_top_pal) > -128)
  beq .set_menu_top_pal
 else
  bne .21skipset_menu_top_pal
  jmp .set_menu_top_pal
.21skipset_menu_top_pal
 endif
.L0385 ;;line 862;;  WSYNC = 1

  lda #1
  sta WSYNC
.L0386 ;;line 863;;  setfade fade

    lda fade
    asl
    asl
    asl
    asl
    sta fourbitfadevalue
.L0387 ;;line 864;;  BACKGRND = getfade ( bg_color ,  black ) 

    lda fourbitfadevalue
    beq .fadezeroskip6
    lda bg_color
    jsr fourbitfade
.fadezeroskip6
  sta BACKGRND
.L0388 ;;line 865;;  if game_state = STATE_TITLE then set_title_top_pal

  lda game_state
  cmp STATE_TITLE
 if ((* - .set_title_top_pal) < 127) && ((* - .set_title_top_pal) > -128)
  beq .set_title_top_pal
 else
  bne .22skipset_title_top_pal
  jmp .set_title_top_pal
.22skipset_title_top_pal
 endif
.L0389 ;;line 866;;  return thisbank

  rts
.
 ;;line 867;; 

.bottomscreenroutine
 ;;line 868;; bottomscreenroutine

.L0390 ;;line 869;;  if game_state = STATE_MENU then BACKGRND = $10  :  return

  lda game_state
  cmp STATE_MENU
  bne .skipL0390
.condpart223
  lda #$10
  sta BACKGRND
  rts
.skipL0390
.L0391 ;;line 870;;  BACKGRND = $00

  lda #$00
  sta BACKGRND
.L0392 ;;line 871;;  if game_state = STATE_TITLE then set_title_bottom_pal

  lda game_state
  cmp STATE_TITLE
 if ((* - .set_title_bottom_pal) < 127) && ((* - .set_title_bottom_pal) > -128)
  beq .set_title_bottom_pal
 else
  bne .23skipset_title_bottom_pal
  jmp .set_title_bottom_pal
.23skipset_title_bottom_pal
 endif
.L0393 ;;line 872;;  flash_color = $00

  lda #$00
  sta flash_color
.L0394 ;;line 873;;  if player_state = STATE_CHANT  &&  animation_timer  >  ANIM_TIME  /  2 then flash_color = CHANT_COLOR

  lda player_state
  cmp STATE_CHANT
  bne .skipL0394
.condpart224
; complex condition detected
  lda ANIM_TIME
  lsr
  cmp animation_timer
  bcs .skip224then
.condpart225
  lda CHANT_COLOR
  sta flash_color
.skip224then
.skipL0394
.
 ;;line 874;; 

.L0395 ;;line 875;;  BACKGRND = flash_color

  lda flash_color
  sta BACKGRND
.L0396 ;;line 876;;  if tileset = T_CASTLE then hair_flash

  lda tileset
  cmp T_CASTLE
 if ((* - .hair_flash) < 127) && ((* - .hair_flash) > -128)
  beq .hair_flash
 else
  bne .24skiphair_flash
  jmp .hair_flash
.24skiphair_flash
 endif
.L0397 ;;line 877;;  P4C1 = flash_color

  lda flash_color
  sta P4C1
.L0398 ;;line 878;;  P5C1 = flash_color

  lda flash_color
  sta P5C1
.L0399 ;;line 879;;  P6C1 = flash_color

  lda flash_color
  sta P6C1
.
 ;;line 880;; 

.L0400 ;;line 881;;  if pal_animation then hair_flash

  lda pal_animation
 if ((* - .hair_flash) < 127) && ((* - .hair_flash) > -128)
  bne .hair_flash
 else
  beq .25skiphair_flash
  jmp .hair_flash
.25skiphair_flash
 endif
.L0401 ;;line 882;;  P7C1 = flash_color

  lda flash_color
  sta P7C1
.hair_flash
 ;;line 883;; hair_flash

.
 ;;line 884;; 

.L0402 ;;line 885;;  if game_state = STATE_FIELD then P1C1 = getfade ( HAIR_COLOR ,  black ) 

  lda game_state
  cmp STATE_FIELD
  bne .skipL0402
.condpart226
    lda fourbitfadevalue
    beq .fadezeroskip7
    lda HAIR_COLOR
    jsr fourbitfade
.fadezeroskip7
  sta P1C1
.skipL0402
.L0403 ;;line 886;;  if player_state = STATE_CHANT  &&  animation_timer  >  ANIM_TIME  /  2 then P1C1 = CHANT_COLOR

  lda player_state
  cmp STATE_CHANT
  bne .skipL0403
.condpart227
; complex condition detected
  lda ANIM_TIME
  lsr
  cmp animation_timer
  bcs .skip227then
.condpart228
  lda CHANT_COLOR
  sta P1C1
.skip227then
.skipL0403
.L0404 ;;line 887;;  white_value = getfade ( PAL_WHITE ) 

    lda PAL_WHITE
    jsr fourbitfade
.fadezeroskip8
  sta white_value
.L0405 ;;line 888;;  if !white_value then white_value = WHITE_DARKNESS

  lda white_value
  bne .skipL0405
.condpart229
  lda WHITE_DARKNESS
  sta white_value
.skipL0405
.L0406 ;;line 889;;  return thisbank

  rts
.
 ;;line 890;; 

.clear_palettes
 ;;line 891;; clear_palettes

.L0407 ;;line 892;;  BACKGRND = $00

  lda #$00
  sta BACKGRND
.L0408 ;;line 893;;  P0C1 = $00  :  P0C2 = $00  :  P0C3 = $00

  lda #$00
  sta P0C1
  sta P0C2
  sta P0C3
.L0409 ;;line 894;;  P1C1 = $00  :  P1C2 = $00  :  P1C3 = $00

  lda #$00
  sta P1C1
  sta P1C2
  sta P1C3
.L0410 ;;line 895;;  P2C1 = $00  :  P2C2 = $00  :  P2C3 = $00

  lda #$00
  sta P2C1
  sta P2C2
  sta P2C3
.L0411 ;;line 896;;  P3C1 = $00  :  P3C2 = $00  :  P3C3 = $00

  lda #$00
  sta P3C1
  sta P3C2
  sta P3C3
.L0412 ;;line 897;;  P4C1 = $00  :  P4C2 = $00  :  P4C3 = $00

  lda #$00
  sta P4C1
  sta P4C2
  sta P4C3
.L0413 ;;line 898;;  P5C1 = $00  :  P5C2 = $00  :  P5C3 = $00

  lda #$00
  sta P5C1
  sta P5C2
  sta P5C3
.L0414 ;;line 899;;  P6C1 = $00  :  P6C2 = $00  :  P6C3 = $00

  lda #$00
  sta P6C1
  sta P6C2
  sta P6C3
.L0415 ;;line 900;;  P7C1 = $00  :  P7C2 = $00  :  P7C3 = $00

  lda #$00
  sta P7C1
  sta P7C2
  sta P7C3
.L0416 ;;line 901;;  return

  rts
.
 ;;line 902;; 

.L0417 ;;line 903;;  data f_key_pals

  jmp .skipL0417
f_key_pals
  .byte         0, 3, 0, 0, 0, 0

.skipL0417
f_key_pals_length = [. - f_key_pals]
f_key_pals_lo SET #<f_key_pals
f_key_pals_hi SET #>f_key_pals
.
 ;;line 906;; 

.L0418 ;;line 907;;  data f_spell_pals

  jmp .skipL0418
f_spell_pals
  .byte         0, 2, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0

.skipL0418
f_spell_pals_length = [. - f_spell_pals]
f_spell_pals_lo SET #<f_spell_pals
f_spell_pals_hi SET #>f_spell_pals
.
 ;;line 910;; 

.L0419 ;;line 911;;  data f_item_pals

  jmp .skipL0419
f_item_pals
  .byte         0, 3, 3, 0, 0, 0, 2, 0, 1, 2, 2

.skipL0419
f_item_pals_length = [. - f_item_pals]
f_item_pals_lo SET #<f_item_pals
f_item_pals_hi SET #>f_item_pals
.
 ;;line 914;; 

.L0420 ;;line 915;;  data f_relic_pals

  jmp .skipL0420
f_relic_pals
  .byte         0, 0, 0, 0, 0, 1, 3, 2, 2, 2, 2

.skipL0420
f_relic_pals_length = [. - f_relic_pals]
f_relic_pals_lo SET #<f_relic_pals
f_relic_pals_hi SET #>f_relic_pals
.
 ;;line 918;; 

.L0421 ;;line 919;;  data f_relic_frames

  jmp .skipL0421
f_relic_frames
  .byte         0, 0, 0, 0, 0, 1, 2

.skipL0421
f_relic_frames_length = [. - f_relic_frames]
f_relic_frames_lo SET #<f_relic_frames
f_relic_frames_hi SET #>f_relic_frames
.
 ;;line 922;; 

.L0422 ;;line 923;;  data f_sword_pals

  jmp .skipL0422
f_sword_pals
  .byte         0, 2, 0, 3, 0, 2, 2, 2, 2, 2, 2

.skipL0422
f_sword_pals_length = [. - f_sword_pals]
f_sword_pals_lo SET #<f_sword_pals
f_sword_pals_hi SET #>f_sword_pals
.
 ;;line 926;; 

.L0423 ;;line 927;;  data f_shield_pals

  jmp .skipL0423
f_shield_pals
  .byte         0, 1, 0, 3, 0, 0, 0, 0, 0, 0, 0

.skipL0423
f_shield_pals_length = [. - f_shield_pals]
f_shield_pals_lo SET #<f_shield_pals
f_shield_pals_hi SET #>f_shield_pals
.
 ;;line 930;; 

.L0424 ;;line 931;;  data f_armor_pals

  jmp .skipL0424
f_armor_pals
  .byte         0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0

.skipL0424
f_armor_pals_length = [. - f_armor_pals]
f_armor_pals_lo SET #<f_armor_pals
f_armor_pals_hi SET #>f_armor_pals
.
 ;;line 934;; 

.L0425 ;;line 935;;  data f_charm_pals

  jmp .skipL0425
f_charm_pals
  .byte         0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0

.skipL0425
f_charm_pals_length = [. - f_charm_pals]
f_charm_pals_lo SET #<f_charm_pals
f_charm_pals_hi SET #>f_charm_pals
.
 ;;line 938;; 

.ledge_hop
 ;;line 939;; ledge_hop

.L0426 ;;line 940;;  asm

        lda Xdirection

        cmp #DIR_LEFT

        beq hop_left

        cmp #DIR_RIGHT

        beq hop_right

        lda Ydirection

        cmp #DIR_UP

        beq hop_up

        ; must be down



hop_down

        lda timer

        bne hop_down_up

        lda #16

        sta timer

        lda Yposition

        clc

        adc #16

        sta hop_dest

hop_down_up

        cmp #8

        bcc hop_down_down

        dec Yposition

        dec timer

        rts

hop_down_down

        inc Yposition

        lda hop_dest

        cmp Yposition

        beq hop_done

        rts



hop_left

        lda timer

        bne hop_left_up

        lda #11

        sta timer

        lda Xposition

        sec

        sbc #12

        sta hop_dest

hop_left_up

        cmp #6

        bcc hop_left_down

        dec Yposition

        dec Xposition

        dec timer

        rts

hop_left_down

        inc Yposition

        dec Xposition

        lda hop_dest

        cmp Xposition

        beq hop_done

        rts



hop_right

        lda timer

        bne hop_right_up

        lda #11

        sta timer

        lda Xposition

        clc

        adc #12

        sta hop_dest

hop_right_up

        cmp #6

        bcc hop_right_down

        dec Yposition

        inc Xposition

        dec timer

        rts

hop_right_down

        inc Yposition

        inc Xposition

        lda hop_dest

        cmp Xposition

        beq hop_done

        rts



hop_up

        lda timer

        bne hop_up_up

        lda #16

        sta timer

        lda Yposition

        sec

        sbc #16

        sta hop_dest

hop_up_up

        cmp #8

        bcc hop_up_down

        dec Yposition

        dec Yposition

        dec timer

        rts

hop_up_down

        dec Yposition

        lda hop_dest

        cmp Yposition

        beq hop_done

        rts



hop_done

        lda #0

        sta flags

        sta timer

        sta hop_dest

        rts

.
 ;;line 1051;; 

.set_enemy_name
 ;;line 1052;; set_enemy_name

.
 ;;line 1053;; 

.L0427 ;;line 1054;;  if item_pickup then return thisbank

  lda item_pickup
  beq .skipL0427
.condpart230
  rts
.skipL0427
.
 ;;line 1055;; 

.L0428 ;;line 1056;;  if !m_relic_bits_1{4} then return thisbank

  lda m_relic_bits_1
  and #16
  bne .skipL0428
.condpart231
  rts
.skipL0428
.
 ;;line 1057;; 

.L0429 ;;line 1058;;  asm

          ldx index

          lda object_type,x

          cmp #TYPE_SPAWNER

          bcc set_name_ptr

          rts

set_name_ptr

          lda object_type,x

          tax

          lda enemy_names_lo,x

          sta pickup_ptr_lo

          lda enemy_names_hi,x

          sta pickup_ptr_hi

          lda #PICKUP_TIME

          sta new_pickup

          sta pickup_timer

          rts

.
 ;;line 1076;; 

.L0430 ;;line 1077;;         incbasic music.78b

.L0431 ;;line 2;;   incrmtfile castle.rmta

castle
  include "castle.rmta"
.L0432 ;;line 3;;  incbasicend

.
 ;;line 2;; 

.L0433 ;;line 3;;  dmahole 0

 jmp dmahole_0
gameend
DMAHOLEEND0 SET .
 echo " ",[($E000 - gameend)]d , "bytes of ROM space left in the main area."
 if ($E000 - gameend) < 0
SPACEOVERFLOW SET (SPACEOVERFLOW+1)
 endif
 if START_OF_ROM = . ; avoid dasm empty start-rom truncation.
     .byte 0
 endif
START_OF_ROM SET 0 ; scuttle so we always fail subsequent banks

 ORG $E000,0  ; *************

font_spr = $E000

font_spr
       HEX 0000
font_spr_tallsprite_00 = $E002

font_spr_tallsprite_00
       HEX 0000
font_spr_tallsprite_01 = $E004

font_spr_tallsprite_01
       HEX 0000
font_spr_tallsprite_02 = $E006

font_spr_tallsprite_02
       HEX 0000
font_spr_tallsprite_03 = $E008

font_spr_tallsprite_03
       HEX 0000
font_spr_tallsprite_04 = $E00A

font_spr_tallsprite_04
       HEX 0000
font_spr_tallsprite_05 = $E00C

font_spr_tallsprite_05
       HEX 0000
font_spr_tallsprite_06 = $E00E

font_spr_tallsprite_06
       HEX 0000
font_spr_tallsprite_07 = $E010

font_spr_tallsprite_07
       HEX 0000
font_spr_tallsprite_08 = $E012

font_spr_tallsprite_08
       HEX 0000
font_spr_tallsprite_09 = $E014

font_spr_tallsprite_09
       HEX 0000
font_spr_tallsprite_10 = $E016

font_spr_tallsprite_10
       HEX 0000
font_spr_tallsprite_11 = $E018

font_spr_tallsprite_11
       HEX 0000
font_spr_tallsprite_12 = $E01A

font_spr_tallsprite_12
       HEX 0000
font_spr_tallsprite_13 = $E01C

font_spr_tallsprite_13
       HEX 0000
font_spr_tallsprite_14 = $E01E

font_spr_tallsprite_14
       HEX 0000
font_spr_tallsprite_15 = $E020

font_spr_tallsprite_15
       HEX 0000
font_spr_tallsprite_16 = $E022

font_spr_tallsprite_16
       HEX 0000
font_spr_tallsprite_17 = $E024

font_spr_tallsprite_17
       HEX 0000
font_spr_tallsprite_18 = $E026

font_spr_tallsprite_18
       HEX 0000
font_spr_tallsprite_19 = $E028

font_spr_tallsprite_19
       HEX 0000
font_spr_tallsprite_20 = $E02A

font_spr_tallsprite_20
       HEX 0000
font_spr_tallsprite_21 = $E02C

font_spr_tallsprite_21
       HEX 0000
font_spr_tallsprite_22 = $E02E

font_spr_tallsprite_22
       HEX 0000
font_spr_tallsprite_23 = $E030

font_spr_tallsprite_23
       HEX 0000
font_spr_tallsprite_24 = $E032

font_spr_tallsprite_24
       HEX 0000
font_spr_tallsprite_25 = $E034

font_spr_tallsprite_25
       HEX 0000
font_spr_tallsprite_26 = $E036

font_spr_tallsprite_26
       HEX 0000
font_spr_tallsprite_27 = $E038

font_spr_tallsprite_27
       HEX 0000
font_spr_tallsprite_28 = $E03A

font_spr_tallsprite_28
       HEX 0000
font_spr_tallsprite_29 = $E03C

font_spr_tallsprite_29
       HEX 0000
font_spr_tallsprite_30 = $E03E

font_spr_tallsprite_30
       HEX 0000
font_spr_tallsprite_31 = $E040

font_spr_tallsprite_31
       HEX 0000
font_spr_tallsprite_32 = $E042

font_spr_tallsprite_32
       HEX 0000
font_spr_tallsprite_33 = $E044

font_spr_tallsprite_33
       HEX 0000
font_spr_tallsprite_34 = $E046

font_spr_tallsprite_34
       HEX 0000
font_spr_tallsprite_35 = $E048

font_spr_tallsprite_35
       HEX 0000
font_spr_tallsprite_36 = $E04A

font_spr_tallsprite_36
       HEX 0000
font_spr_tallsprite_37 = $E04C

font_spr_tallsprite_37
       HEX 0000
font_spr_tallsprite_38 = $E04E

font_spr_tallsprite_38
       HEX 0000
font_spr_tallsprite_39 = $E050

font_spr_tallsprite_39
       HEX 0000
font_spr_tallsprite_40 = $E052

font_spr_tallsprite_40
       HEX 0000
font_spr_tallsprite_41 = $E054

font_spr_tallsprite_41
       HEX 0000
font_spr_tallsprite_42 = $E056

font_spr_tallsprite_42
       HEX 0000
font_spr_tallsprite_43 = $E058

font_spr_tallsprite_43
       HEX 0000
font_spr_tallsprite_44 = $E05A

font_spr_tallsprite_44
       HEX 0000
font_spr_tallsprite_45 = $E05C

font_spr_tallsprite_45
       HEX 0000
font_spr_tallsprite_46 = $E05E

font_spr_tallsprite_46
       HEX 0000
font_spr_tallsprite_47 = $E060

font_spr_tallsprite_47
       HEX 0000
font_spr_tallsprite_48 = $E062

font_spr_tallsprite_48
       HEX 0000
font_spr_tallsprite_49 = $E064

font_spr_tallsprite_49
       HEX 0000
font_spr_tallsprite_50 = $E066

font_spr_tallsprite_50
       HEX 0000
font_spr_tallsprite_51 = $E068

font_spr_tallsprite_51
       HEX 0000
font_spr_tallsprite_52 = $E06A

font_spr_tallsprite_52
       HEX 0000
font_spr_tallsprite_53 = $E06C

font_spr_tallsprite_53
       HEX 0000
font_spr_tallsprite_54 = $E06E

font_spr_tallsprite_54
       HEX 0000
font_spr_tallsprite_55 = $E070

font_spr_tallsprite_55
       HEX 0000
font_spr_tallsprite_56 = $E072

font_spr_tallsprite_56
       HEX 0000
font_spr_tallsprite_57 = $E074

font_spr_tallsprite_57
       HEX 0000
font_spr_tallsprite_58 = $E076

font_spr_tallsprite_58
       HEX 0000
font_spr_tallsprite_59 = $E078

font_spr_tallsprite_59
       HEX 0000
font_spr_tallsprite_60 = $E07A

font_spr_tallsprite_60
       HEX 0000
font_spr_tallsprite_61 = $E07C

font_spr_tallsprite_61
       HEX 0000
font_spr_tallsprite_62 = $E07E

font_spr_tallsprite_62
       HEX 0000
font_spr_tallsprite_63 = $E080

font_spr_tallsprite_63
       HEX 0000
bar0 = $E082

bar0
       HEX 0000
bar1 = $E084

bar1
       HEX 0000
bar2 = $E086

bar2
       HEX 0000
bar3 = $E088

bar3
       HEX 0000
bar4 = $E08A

bar4
       HEX 0000
bar5 = $E08C

bar5
       HEX 0000
bar6 = $E08E

bar6
       HEX 0000
bar7 = $E090

bar7
       HEX 0000
bar8 = $E092

bar8
       HEX 0000
hp = $E094

hp
       HEX 0000000000000000
mp = $E09C

mp
       HEX 00000000
coin = $E0A0

coin
       HEX 0000
prism = $E0A2

prism
       HEX 0000
chimkin = $E0A4

chimkin
       HEX 0000
torch0 = $E0A6

torch0
       HEX 55555555
torch1 = $E0AA

torch1
       HEX 55555555
torch2 = $E0AE

torch2
       HEX 00555544
torch3 = $E0B2

torch3
       HEX 00555544
potion0 = $E0B6

potion0
       HEX 0000
armor0 = $E0B8

armor0
       HEX 000000
rosary = $E0BB

rosary
       HEX 0150
relic0 = $E0BD

relic0
       HEX 000000
relic1 = $E0C0

relic1
       HEX 000000
relic2 = $E0C3

relic2
       HEX 000000
map0 = $E0C6

map0
       HEX 0000
door0 = $E0C8

door0
       HEX 00000000
door1 = $E0CC

door1
       HEX 00155500
key0 = $E0D0

key0
       HEX 00

 ORG $E100,0  ; *************

;font_spr
       HEX 0000
;font_spr_tallsprite_00
       HEX 0000
;font_spr_tallsprite_01
       HEX 0000
;font_spr_tallsprite_02
       HEX 0000
;font_spr_tallsprite_03
       HEX 0000
;font_spr_tallsprite_04
       HEX 0000
;font_spr_tallsprite_05
       HEX 0000
;font_spr_tallsprite_06
       HEX 0000
;font_spr_tallsprite_07
       HEX 0000
;font_spr_tallsprite_08
       HEX 0000
;font_spr_tallsprite_09
       HEX 0000
;font_spr_tallsprite_10
       HEX 0000
;font_spr_tallsprite_11
       HEX 0000
;font_spr_tallsprite_12
       HEX 0000
;font_spr_tallsprite_13
       HEX 0000
;font_spr_tallsprite_14
       HEX 0000
;font_spr_tallsprite_15
       HEX 0000
;font_spr_tallsprite_16
       HEX 0000
;font_spr_tallsprite_17
       HEX 0000
;font_spr_tallsprite_18
       HEX 0000
;font_spr_tallsprite_19
       HEX 0000
;font_spr_tallsprite_20
       HEX 0000
;font_spr_tallsprite_21
       HEX 0000
;font_spr_tallsprite_22
       HEX 0000
;font_spr_tallsprite_23
       HEX 0000
;font_spr_tallsprite_24
       HEX 0000
;font_spr_tallsprite_25
       HEX 0000
;font_spr_tallsprite_26
       HEX 0000
;font_spr_tallsprite_27
       HEX 0000
;font_spr_tallsprite_28
       HEX 0000
;font_spr_tallsprite_29
       HEX 0000
;font_spr_tallsprite_30
       HEX 0000
;font_spr_tallsprite_31
       HEX 0000
;font_spr_tallsprite_32
       HEX 0000
;font_spr_tallsprite_33
       HEX 0000
;font_spr_tallsprite_34
       HEX 0000
;font_spr_tallsprite_35
       HEX 0000
;font_spr_tallsprite_36
       HEX 0000
;font_spr_tallsprite_37
       HEX 0000
;font_spr_tallsprite_38
       HEX 0000
;font_spr_tallsprite_39
       HEX 0000
;font_spr_tallsprite_40
       HEX 0000
;font_spr_tallsprite_41
       HEX 0000
;font_spr_tallsprite_42
       HEX 0000
;font_spr_tallsprite_43
       HEX 0000
;font_spr_tallsprite_44
       HEX 0000
;font_spr_tallsprite_45
       HEX 0000
;font_spr_tallsprite_46
       HEX 0000
;font_spr_tallsprite_47
       HEX 0000
;font_spr_tallsprite_48
       HEX 0000
;font_spr_tallsprite_49
       HEX 0000
;font_spr_tallsprite_50
       HEX 0000
;font_spr_tallsprite_51
       HEX 0000
;font_spr_tallsprite_52
       HEX 0000
;font_spr_tallsprite_53
       HEX 0000
;font_spr_tallsprite_54
       HEX 0000
;font_spr_tallsprite_55
       HEX 0000
;font_spr_tallsprite_56
       HEX 0000
;font_spr_tallsprite_57
       HEX 0000
;font_spr_tallsprite_58
       HEX 0000
;font_spr_tallsprite_59
       HEX 0000
;font_spr_tallsprite_60
       HEX 0000
;font_spr_tallsprite_61
       HEX 0000
;font_spr_tallsprite_62
       HEX 0000
;font_spr_tallsprite_63
       HEX 0000
;bar0
       HEX 5555
;bar1
       HEX 5555
;bar2
       HEX 5555
;bar3
       HEX 5555
;bar4
       HEX 5555
;bar5
       HEX 5555
;bar6
       HEX 5555
;bar7
       HEX 5555
;bar8
       HEX 5555
;hp
       HEX 125a5a5a5a5a5a5a
;mp
       HEX 15555555
;coin
       HEX 0000
;prism
       HEX 0000
;chimkin
       HEX 0500
;torch0
       HEX b5e5a595
;torch1
       HEX b5e5a595
;torch2
       HEX 00f59500
;torch3
       HEX 00f59500
;potion0
       HEX 0fc0
;armor0
       HEX 000000
;rosary
       HEX 0254
;relic0
       HEX 000000
;relic1
       HEX 000000
;relic2
       HEX 000000
;map0
       HEX 0000
;door0
       HEX 001a2900
;door1
       HEX 00215200
;key0
       HEX 05

 ORG $E200,0  ; *************

;font_spr
       HEX 0000
;font_spr_tallsprite_00
       HEX 0000
;font_spr_tallsprite_01
       HEX 0000
;font_spr_tallsprite_02
       HEX 0000
;font_spr_tallsprite_03
       HEX 0000
;font_spr_tallsprite_04
       HEX 0000
;font_spr_tallsprite_05
       HEX 0000
;font_spr_tallsprite_06
       HEX 0000
;font_spr_tallsprite_07
       HEX 0000
;font_spr_tallsprite_08
       HEX 0000
;font_spr_tallsprite_09
       HEX 0000
;font_spr_tallsprite_10
       HEX 0000
;font_spr_tallsprite_11
       HEX 0000
;font_spr_tallsprite_12
       HEX 0000
;font_spr_tallsprite_13
       HEX 0000
;font_spr_tallsprite_14
       HEX 0000
;font_spr_tallsprite_15
       HEX 0000
;font_spr_tallsprite_16
       HEX 0000
;font_spr_tallsprite_17
       HEX 0000
;font_spr_tallsprite_18
       HEX 0000
;font_spr_tallsprite_19
       HEX 0000
;font_spr_tallsprite_20
       HEX 0000
;font_spr_tallsprite_21
       HEX 0000
;font_spr_tallsprite_22
       HEX 0000
;font_spr_tallsprite_23
       HEX 0000
;font_spr_tallsprite_24
       HEX 0000
;font_spr_tallsprite_25
       HEX 0000
;font_spr_tallsprite_26
       HEX 0000
;font_spr_tallsprite_27
       HEX 0000
;font_spr_tallsprite_28
       HEX 0000
;font_spr_tallsprite_29
       HEX 0000
;font_spr_tallsprite_30
       HEX 0000
;font_spr_tallsprite_31
       HEX 0000
;font_spr_tallsprite_32
       HEX 0000
;font_spr_tallsprite_33
       HEX 0000
;font_spr_tallsprite_34
       HEX 0000
;font_spr_tallsprite_35
       HEX 0000
;font_spr_tallsprite_36
       HEX 0000
;font_spr_tallsprite_37
       HEX 0000
;font_spr_tallsprite_38
       HEX 0000
;font_spr_tallsprite_39
       HEX 0000
;font_spr_tallsprite_40
       HEX 0000
;font_spr_tallsprite_41
       HEX 0000
;font_spr_tallsprite_42
       HEX 0000
;font_spr_tallsprite_43
       HEX 0000
;font_spr_tallsprite_44
       HEX 0000
;font_spr_tallsprite_45
       HEX 0000
;font_spr_tallsprite_46
       HEX 0000
;font_spr_tallsprite_47
       HEX 0000
;font_spr_tallsprite_48
       HEX 0000
;font_spr_tallsprite_49
       HEX 0000
;font_spr_tallsprite_50
       HEX 0000
;font_spr_tallsprite_51
       HEX 0000
;font_spr_tallsprite_52
       HEX 0000
;font_spr_tallsprite_53
       HEX 0000
;font_spr_tallsprite_54
       HEX 0000
;font_spr_tallsprite_55
       HEX 0000
;font_spr_tallsprite_56
       HEX 0000
;font_spr_tallsprite_57
       HEX 0000
;font_spr_tallsprite_58
       HEX 0000
;font_spr_tallsprite_59
       HEX 0000
;font_spr_tallsprite_60
       HEX 0000
;font_spr_tallsprite_61
       HEX 0000
;font_spr_tallsprite_62
       HEX 0000
;font_spr_tallsprite_63
       HEX 0000
;bar0
       HEX 5555
;bar1
       HEX 5555
;bar2
       HEX 5555
;bar3
       HEX 5555
;bar4
       HEX 5555
;bar5
       HEX 5555
;bar6
       HEX 5555
;bar7
       HEX 5555
;bar8
       HEX 5555
;hp
       HEX 12f05af078d25a5a
;mp
       HEX 1d5f7d55
;coin
       HEX 0000
;prism
       HEX 0000
;chimkin
       HEX 2954
;torch0
       HEX 31756544
;torch1
       HEX 31756544
;torch2
       HEX 00314400
;torch3
       HEX 00314400
;potion0
       HEX 3570
;armor0
       HEX 000000
;rosary
       HEX 0a80
;relic0
       HEX 000000
;relic1
       HEX 000000
;relic2
       HEX 000000
;map0
       HEX 0001
;door0
       HEX 00da29c0
;door1
       HEX 00e002c0
;key0
       HEX 54

 ORG $E300,0  ; *************

;font_spr
       HEX 0000
;font_spr_tallsprite_00
       HEX 0000
;font_spr_tallsprite_01
       HEX 0000
;font_spr_tallsprite_02
       HEX 0000
;font_spr_tallsprite_03
       HEX 0000
;font_spr_tallsprite_04
       HEX 0000
;font_spr_tallsprite_05
       HEX 0000
;font_spr_tallsprite_06
       HEX 0000
;font_spr_tallsprite_07
       HEX 0000
;font_spr_tallsprite_08
       HEX 0000
;font_spr_tallsprite_09
       HEX 0000
;font_spr_tallsprite_10
       HEX 0000
;font_spr_tallsprite_11
       HEX 0000
;font_spr_tallsprite_12
       HEX 0000
;font_spr_tallsprite_13
       HEX 0000
;font_spr_tallsprite_14
       HEX 0000
;font_spr_tallsprite_15
       HEX 0000
;font_spr_tallsprite_16
       HEX 0000
;font_spr_tallsprite_17
       HEX 0000
;font_spr_tallsprite_18
       HEX 0000
;font_spr_tallsprite_19
       HEX 0000
;font_spr_tallsprite_20
       HEX 0000
;font_spr_tallsprite_21
       HEX 0000
;font_spr_tallsprite_22
       HEX 0000
;font_spr_tallsprite_23
       HEX 0000
;font_spr_tallsprite_24
       HEX 0000
;font_spr_tallsprite_25
       HEX 0000
;font_spr_tallsprite_26
       HEX 0150
;font_spr_tallsprite_27
       HEX 0000
;font_spr_tallsprite_28
       HEX 0000
;font_spr_tallsprite_29
       HEX 0000
;font_spr_tallsprite_30
       HEX 0000
;font_spr_tallsprite_31
       HEX 0000
;font_spr_tallsprite_32
       HEX 0000
;font_spr_tallsprite_33
       HEX 0000
;font_spr_tallsprite_34
       HEX 0000
;font_spr_tallsprite_35
       HEX 0000
;font_spr_tallsprite_36
       HEX 0000
;font_spr_tallsprite_37
       HEX 0000
;font_spr_tallsprite_38
       HEX 0000
;font_spr_tallsprite_39
       HEX 0000
;font_spr_tallsprite_40
       HEX 0000
;font_spr_tallsprite_41
       HEX 0000
;font_spr_tallsprite_42
       HEX 1500
;font_spr_tallsprite_43
       HEX 0000
;font_spr_tallsprite_44
       HEX 0000
;font_spr_tallsprite_45
       HEX 1400
;font_spr_tallsprite_46
       HEX 0000
;font_spr_tallsprite_47
       HEX 0000
;font_spr_tallsprite_48
       HEX 0000
;font_spr_tallsprite_49
       HEX 0000
;font_spr_tallsprite_50
       HEX 0000
;font_spr_tallsprite_51
       HEX 5400
;font_spr_tallsprite_52
       HEX 0150
;font_spr_tallsprite_53
       HEX 0000
;font_spr_tallsprite_54
       HEX 0000
;font_spr_tallsprite_55
       HEX 0000
;font_spr_tallsprite_56
       HEX 0000
;font_spr_tallsprite_57
       HEX 0000
;font_spr_tallsprite_58
       HEX 0000
;font_spr_tallsprite_59
       HEX 0000
;font_spr_tallsprite_60
       HEX 5000
;font_spr_tallsprite_61
       HEX 0000
;font_spr_tallsprite_62
       HEX 0000
;font_spr_tallsprite_63
       HEX 0000
;bar0
       HEX 0000
;bar1
       HEX 8000
;bar2
       HEX a000
;bar3
       HEX a800
;bar4
       HEX aa00
;bar5
       HEX aa80
;bar6
       HEX aaa0
;bar7
       HEX aaa8
;bar8
       HEX aaaa
;hp
       HEX 12f05af078d25a5a
;mp
       HEX 1d5f7d55
;coin
       HEX 0000
;prism
       HEX 0000
;chimkin
       HEX aa40
;torch0
       HEX 31756544
;torch1
       HEX 31756544
;torch2
       HEX 00214400
;torch3
       HEX 00214400
;potion0
       HEX 3970
;armor0
       HEX 005400
;rosary
       HEX 0240
;relic0
       HEX 155000
;relic1
       HEX 055000
;relic2
       HEX 154000
;map0
       HEX 1556
;door0
       HEX 001a2900
;door1
       HEX 00200200
;key0
       HEX f4

 ORG $E400,0  ; *************

;font_spr
       HEX 0000
;font_spr_tallsprite_00
       HEX 1400
;font_spr_tallsprite_01
       HEX 1550
;font_spr_tallsprite_02
       HEX 5550
;font_spr_tallsprite_03
       HEX 1500
;font_spr_tallsprite_04
       HEX 0150
;font_spr_tallsprite_05
       HEX 5500
;font_spr_tallsprite_06
       HEX 1500
;font_spr_tallsprite_07
       HEX 5500
;font_spr_tallsprite_08
       HEX 1500
;font_spr_tallsprite_09
       HEX 1400
;font_spr_tallsprite_10
       HEX 5550
;font_spr_tallsprite_11
       HEX 5500
;font_spr_tallsprite_12
       HEX 1500
;font_spr_tallsprite_13
       HEX 5500
;font_spr_tallsprite_14
       HEX 5550
;font_spr_tallsprite_15
       HEX 5400
;font_spr_tallsprite_16
       HEX 1500
;font_spr_tallsprite_17
       HEX 5550
;font_spr_tallsprite_18
       HEX 1550
;font_spr_tallsprite_19
       HEX 1400
;font_spr_tallsprite_20
       HEX 5150
;font_spr_tallsprite_21
       HEX 5550
;font_spr_tallsprite_22
       HEX 5050
;font_spr_tallsprite_23
       HEX 5150
;font_spr_tallsprite_24
       HEX 1500
;font_spr_tallsprite_25
       HEX 5400
;font_spr_tallsprite_26
       HEX 17d0
;font_spr_tallsprite_27
       HEX 5550
;font_spr_tallsprite_28
       HEX 5500
;font_spr_tallsprite_29
       HEX 0540
;font_spr_tallsprite_30
       HEX 1500
;font_spr_tallsprite_31
       HEX 0400
;font_spr_tallsprite_32
       HEX 1100
;font_spr_tallsprite_33
       HEX 4050
;font_spr_tallsprite_34
       HEX 5000
;font_spr_tallsprite_35
       HEX 5550
;font_spr_tallsprite_36
       HEX 1450
;font_spr_tallsprite_37
       HEX 5500
;font_spr_tallsprite_38
       HEX 1500
;font_spr_tallsprite_39
       HEX 1550
;font_spr_tallsprite_40
       HEX 1550
;font_spr_tallsprite_41
       HEX 1500
;font_spr_tallsprite_42
       HEX 3f40
;font_spr_tallsprite_43
       HEX 5150
;font_spr_tallsprite_44
       HEX 0540
;font_spr_tallsprite_45
       HEX 7d00
;font_spr_tallsprite_46
       HEX 5150
;font_spr_tallsprite_47
       HEX 0540
;font_spr_tallsprite_48
       HEX 5050
;font_spr_tallsprite_49
       HEX 5550
;font_spr_tallsprite_50
       HEX 1500
;font_spr_tallsprite_51
       HEX f400
;font_spr_tallsprite_52
       HEX 03d0
;font_spr_tallsprite_53
       HEX 5400
;font_spr_tallsprite_54
       HEX 5500
;font_spr_tallsprite_55
       HEX 0140
;font_spr_tallsprite_56
       HEX 1500
;font_spr_tallsprite_57
       HEX 0400
;font_spr_tallsprite_58
       HEX 1100
;font_spr_tallsprite_59
       HEX 5050
;font_spr_tallsprite_60
       HEX f400
;font_spr_tallsprite_61
       HEX 5550
;font_spr_tallsprite_62
       HEX 0000
;font_spr_tallsprite_63
       HEX 5500
;bar0
       HEX 0000
;bar1
       HEX 8000
;bar2
       HEX a000
;bar3
       HEX a800
;bar4
       HEX aa00
;bar5
       HEX aa80
;bar6
       HEX aaa0
;bar7
       HEX aaa8
;bar8
       HEX aaaa
;hp
       HEX 12f05af078d25a5a
;mp
       HEX 1d5f7d55
;coin
       HEX 0000
;prism
       HEX 0000
;chimkin
       HEX aa00
;torch0
       HEX 31656544
;torch1
       HEX 31656544
;torch2
       HEX 00214400
;torch3
       HEX 00214400
;potion0
       HEX 3a70
;armor0
       HEX 01a900
;rosary
       HEX 0240
;relic0
       HEX be4000
;relic1
       HEX 1ff000
;relic2
       HEX ba5000
;map0
       HEX fffe
;door0
       HEX 00000000
;door1
       HEX 00000000
;key0
       HEX 34

 ORG $E500,0  ; *************

;font_spr
       HEX 0000
;font_spr_tallsprite_00
       HEX 7d00
;font_spr_tallsprite_01
       HEX 3fd0
;font_spr_tallsprite_02
       HEX ffd0
;font_spr_tallsprite_03
       HEX 7f40
;font_spr_tallsprite_04
       HEX 57d0
;font_spr_tallsprite_05
       HEX ff40
;font_spr_tallsprite_06
       HEX 7f40
;font_spr_tallsprite_07
       HEX fd00
;font_spr_tallsprite_08
       HEX 7f40
;font_spr_tallsprite_09
       HEX 3d00
;font_spr_tallsprite_10
       HEX f7d0
;font_spr_tallsprite_11
       HEX ff40
;font_spr_tallsprite_12
       HEX 7f40
;font_spr_tallsprite_13
       HEX ff40
;font_spr_tallsprite_14
       HEX ffd0
;font_spr_tallsprite_15
       HEX f400
;font_spr_tallsprite_16
       HEX 7f40
;font_spr_tallsprite_17
       HEX f7d0
;font_spr_tallsprite_18
       HEX 3fd0
;font_spr_tallsprite_19
       HEX 7d00
;font_spr_tallsprite_20
       HEX f7d0
;font_spr_tallsprite_21
       HEX ffd0
;font_spr_tallsprite_22
       HEX d0d0
;font_spr_tallsprite_23
       HEX d3d0
;font_spr_tallsprite_24
       HEX 7f40
;font_spr_tallsprite_25
       HEX f400
;font_spr_tallsprite_26
       HEX 7f40
;font_spr_tallsprite_27
       HEX f7d0
;font_spr_tallsprite_28
       HEX ff40
;font_spr_tallsprite_29
       HEX 0f40
;font_spr_tallsprite_30
       HEX 7f40
;font_spr_tallsprite_31
       HEX 0d00
;font_spr_tallsprite_32
       HEX 3740
;font_spr_tallsprite_33
       HEX d1d0
;font_spr_tallsprite_34
       HEX f400
;font_spr_tallsprite_35
       HEX ffd0
;font_spr_tallsprite_36
       HEX 7dd0
;font_spr_tallsprite_37
       HEX ff40
;font_spr_tallsprite_38
       HEX 7f40
;font_spr_tallsprite_39
       HEX 7fd0
;font_spr_tallsprite_40
       HEX 7fd0
;font_spr_tallsprite_41
       HEX 3d00
;font_spr_tallsprite_42
       HEX 17d0
;font_spr_tallsprite_43
       HEX f7d0
;font_spr_tallsprite_44
       HEX 0f40
;font_spr_tallsprite_45
       HEX df40
;font_spr_tallsprite_46
       HEX f7d0
;font_spr_tallsprite_47
       HEX 0f40
;font_spr_tallsprite_48
       HEX d0d0
;font_spr_tallsprite_49
       HEX f7d0
;font_spr_tallsprite_50
       HEX 7f40
;font_spr_tallsprite_51
       HEX f500
;font_spr_tallsprite_52
       HEX 17d0
;font_spr_tallsprite_53
       HEX f400
;font_spr_tallsprite_54
       HEX ff40
;font_spr_tallsprite_55
       HEX 07c0
;font_spr_tallsprite_56
       HEX 7f40
;font_spr_tallsprite_57
       HEX 1d00
;font_spr_tallsprite_58
       HEX 3740
;font_spr_tallsprite_59
       HEX f7d0
;font_spr_tallsprite_60
       HEX 3d00
;font_spr_tallsprite_61
       HEX ffd0
;font_spr_tallsprite_62
       HEX 0000
;font_spr_tallsprite_63
       HEX ff40
;bar0
       HEX 0000
;bar1
       HEX 8000
;bar2
       HEX a000
;bar3
       HEX a800
;bar4
       HEX aa00
;bar5
       HEX aa80
;bar6
       HEX aaa0
;bar7
       HEX aaa8
;bar8
       HEX aaaa
;hp
       HEX 00f0f0f078f0f05a
;mp
       HEX 0ddf7ff5
;coin
       HEX 0300
;prism
       HEX 0240
;chimkin
       HEX aa00
;torch0
       HEX 11555544
;torch1
       HEX 11555544
;torch2
       HEX 00214400
;torch3
       HEX 00214400
;potion0
       HEX 0cc0
;armor0
       HEX 02aa00
;rosary
       HEX 0240
;relic0
       HEX ff0000
;relic1
       HEX 155000
;relic2
       HEX fe8000
;map0
       HEX fffe
;door0
       HEX 00db39c0
;door1
       HEX 00e002c0
;key0
       HEX 74

 ORG $E600,0  ; *************

;font_spr
       HEX 0000
;font_spr_tallsprite_00
       HEX f740
;font_spr_tallsprite_01
       HEX 0f40
;font_spr_tallsprite_02
       HEX f5d0
;font_spr_tallsprite_03
       HEX f7d0
;font_spr_tallsprite_04
       HEX ffd0
;font_spr_tallsprite_05
       HEX 03d0
;font_spr_tallsprite_06
       HEX f7d0
;font_spr_tallsprite_07
       HEX 3d00
;font_spr_tallsprite_08
       HEX f7d0
;font_spr_tallsprite_09
       HEX 0f40
;font_spr_tallsprite_10
       HEX f7d0
;font_spr_tallsprite_11
       HEX f7d0
;font_spr_tallsprite_12
       HEX f7d0
;font_spr_tallsprite_13
       HEX f7d0
;font_spr_tallsprite_14
       HEX f4d0
;font_spr_tallsprite_15
       HEX f400
;font_spr_tallsprite_16
       HEX f5d0
;font_spr_tallsprite_17
       HEX f7d0
;font_spr_tallsprite_18
       HEX 0f40
;font_spr_tallsprite_19
       HEX df40
;font_spr_tallsprite_20
       HEX f7d0
;font_spr_tallsprite_21
       HEX f4d0
;font_spr_tallsprite_22
       HEX d0d0
;font_spr_tallsprite_23
       HEX d3d0
;font_spr_tallsprite_24
       HEX f7d0
;font_spr_tallsprite_25
       HEX f400
;font_spr_tallsprite_26
       HEX f7c0
;font_spr_tallsprite_27
       HEX f7d0
;font_spr_tallsprite_28
       HEX d3d0
;font_spr_tallsprite_29
       HEX 0f40
;font_spr_tallsprite_30
       HEX ffd0
;font_spr_tallsprite_31
       HEX 1d00
;font_spr_tallsprite_32
       HEX 7f40
;font_spr_tallsprite_33
       HEX f7d0
;font_spr_tallsprite_34
       HEX 3d00
;font_spr_tallsprite_35
       HEX f4d0
;font_spr_tallsprite_36
       HEX f7d0
;font_spr_tallsprite_37
       HEX f7d0
;font_spr_tallsprite_38
       HEX f7d0
;font_spr_tallsprite_39
       HEX f7d0
;font_spr_tallsprite_40
       HEX f500
;font_spr_tallsprite_41
       HEX 3d00
;font_spr_tallsprite_42
       HEX 7fd0
;font_spr_tallsprite_43
       HEX f7d0
;font_spr_tallsprite_44
       HEX 0f40
;font_spr_tallsprite_45
       HEX 0f40
;font_spr_tallsprite_46
       HEX f7d0
;font_spr_tallsprite_47
       HEX 0f40
;font_spr_tallsprite_48
       HEX d4d0
;font_spr_tallsprite_49
       HEX f7d0
;font_spr_tallsprite_50
       HEX f7d0
;font_spr_tallsprite_51
       HEX ff40
;font_spr_tallsprite_52
       HEX 7fd0
;font_spr_tallsprite_53
       HEX f400
;font_spr_tallsprite_54
       HEX 17d0
;font_spr_tallsprite_55
       HEX 0f40
;font_spr_tallsprite_56
       HEX f7d0
;font_spr_tallsprite_57
       HEX 7f40
;font_spr_tallsprite_58
       HEX ffd0
;font_spr_tallsprite_59
       HEX 3f40
;font_spr_tallsprite_60
       HEX 1f40
;font_spr_tallsprite_61
       HEX f4d0
;font_spr_tallsprite_62
       HEX 0000
;font_spr_tallsprite_63
       HEX 17d0
;bar0
       HEX 0000
;bar1
       HEX 8000
;bar2
       HEX a000
;bar3
       HEX a800
;bar4
       HEX aa00
;bar5
       HEX aa80
;bar6
       HEX aaa0
;bar7
       HEX aaa8
;bar8
       HEX aaaa
;hp
       HEX 00f048f078d230d2
;mp
       HEX 0fff7d3d
;coin
       HEX 0ec0
;prism
       HEX 0a50
;chimkin
       HEX 2b40
;torch0
       HEX 75f5e555
;torch1
       HEX 65f5a555
;torch2
       HEX 00214400
;torch3
       HEX 00214400
;potion0
       HEX 0cc0
;armor0
       HEX 015500
;rosary
       HEX 1cd0
;relic0
       HEX ef0000
;relic1
       HEX 1aa000
;relic2
       HEX 3f0000
;map0
       HEX eeee
;door0
       HEX 00000000
;door1
       HEX 00000000
;key0
       HEX f4

 ORG $E700,0  ; *************

;font_spr
       HEX 0000
;font_spr_tallsprite_00
       HEX f7d0
;font_spr_tallsprite_01
       HEX 0f40
;font_spr_tallsprite_02
       HEX 3d00
;font_spr_tallsprite_03
       HEX 07d0
;font_spr_tallsprite_04
       HEX f7d0
;font_spr_tallsprite_05
       HEX 57d0
;font_spr_tallsprite_06
       HEX f7d0
;font_spr_tallsprite_07
       HEX 3d00
;font_spr_tallsprite_08
       HEX f7d0
;font_spr_tallsprite_09
       HEX 17d0
;font_spr_tallsprite_10
       HEX ffd0
;font_spr_tallsprite_11
       HEX f7d0
;font_spr_tallsprite_12
       HEX f400
;font_spr_tallsprite_13
       HEX f7d0
;font_spr_tallsprite_14
       HEX f540
;font_spr_tallsprite_15
       HEX f540
;font_spr_tallsprite_16
       HEX f7d0
;font_spr_tallsprite_17
       HEX f7d0
;font_spr_tallsprite_18
       HEX 0f40
;font_spr_tallsprite_19
       HEX 0f40
;font_spr_tallsprite_20
       HEX f740
;font_spr_tallsprite_21
       HEX f400
;font_spr_tallsprite_22
       HEX d4d0
;font_spr_tallsprite_23
       HEX d7d0
;font_spr_tallsprite_24
       HEX f7d0
;font_spr_tallsprite_25
       HEX f500
;font_spr_tallsprite_26
       HEX f7d0
;font_spr_tallsprite_27
       HEX f7d0
;font_spr_tallsprite_28
       HEX 17d0
;font_spr_tallsprite_29
       HEX 0f40
;font_spr_tallsprite_30
       HEX f7d0
;font_spr_tallsprite_31
       HEX 3740
;font_spr_tallsprite_32
       HEX ddd0
;font_spr_tallsprite_33
       HEX 3f40
;font_spr_tallsprite_34
       HEX 1f40
;font_spr_tallsprite_35
       HEX 3400
;font_spr_tallsprite_36
       HEX 3fd0
;font_spr_tallsprite_37
       HEX f7d0
;font_spr_tallsprite_38
       HEX f540
;font_spr_tallsprite_39
       HEX f7d0
;font_spr_tallsprite_40
       HEX ff40
;font_spr_tallsprite_41
       HEX 7d40
;font_spr_tallsprite_42
       HEX f7d0
;font_spr_tallsprite_43
       HEX f7d0
;font_spr_tallsprite_44
       HEX 0f40
;font_spr_tallsprite_45
       HEX 0f40
;font_spr_tallsprite_46
       HEX ff40
;font_spr_tallsprite_47
       HEX 0f40
;font_spr_tallsprite_48
       HEX ddd0
;font_spr_tallsprite_49
       HEX f7d0
;font_spr_tallsprite_50
       HEX f7d0
;font_spr_tallsprite_51
       HEX f7d0
;font_spr_tallsprite_52
       HEX f7d0
;font_spr_tallsprite_53
       HEX f400
;font_spr_tallsprite_54
       HEX 7f40
;font_spr_tallsprite_55
       HEX 0f40
;font_spr_tallsprite_56
       HEX f7d0
;font_spr_tallsprite_57
       HEX f7d0
;font_spr_tallsprite_58
       HEX ddd0
;font_spr_tallsprite_59
       HEX 1d00
;font_spr_tallsprite_60
       HEX 7fd0
;font_spr_tallsprite_61
       HEX 7d00
;font_spr_tallsprite_62
       HEX 0000
;font_spr_tallsprite_63
       HEX 7f40
;bar0
       HEX 0000
;bar1
       HEX 0000
;bar2
       HEX 0000
;bar3
       HEX 0000
;bar4
       HEX 0000
;bar5
       HEX 0000
;bar6
       HEX 0000
;bar7
       HEX 0000
;bar8
       HEX 0000
;hp
       HEX 00f048f078d278d2
;mp
       HEX 0f7f7d7d
;coin
       HEX 3ab0
;prism
       HEX 2a94
;chimkin
       HEX 00d4
;torch0
       HEX f4f0f195
;torch1
       HEX b5f0e595
;torch2
       HEX 00e54400
;torch3
       HEX 00e54400
;potion0
       HEX 3ef0
;armor0
       HEX 019900
;rosary
       HEX 2020
;relic0
       HEX eb0000
;relic1
       HEX 1aa000
;relic2
       HEX 0c0000
;map0
       HEX eeee
;door0
       HEX 001a2900
;door1
       HEX 00200200
;key0
       HEX 34

 ORG $E800,0  ; *************

;font_spr
       HEX 0000
;font_spr_tallsprite_00
       HEX f7d0
;font_spr_tallsprite_01
       HEX 0f40
;font_spr_tallsprite_02
       HEX 0f40
;font_spr_tallsprite_03
       HEX 0f40
;font_spr_tallsprite_04
       HEX f7d0
;font_spr_tallsprite_05
       HEX ff40
;font_spr_tallsprite_06
       HEX f7d0
;font_spr_tallsprite_07
       HEX 0f40
;font_spr_tallsprite_08
       HEX 7f40
;font_spr_tallsprite_09
       HEX 7fd0
;font_spr_tallsprite_10
       HEX f7d0
;font_spr_tallsprite_11
       HEX ff40
;font_spr_tallsprite_12
       HEX f400
;font_spr_tallsprite_13
       HEX f7d0
;font_spr_tallsprite_14
       HEX ff40
;font_spr_tallsprite_15
       HEX ff40
;font_spr_tallsprite_16
       HEX f400
;font_spr_tallsprite_17
       HEX ffd0
;font_spr_tallsprite_18
       HEX 0f40
;font_spr_tallsprite_19
       HEX 0f40
;font_spr_tallsprite_20
       HEX fd00
;font_spr_tallsprite_21
       HEX f400
;font_spr_tallsprite_22
       HEX ddd0
;font_spr_tallsprite_23
       HEX dfd0
;font_spr_tallsprite_24
       HEX f7d0
;font_spr_tallsprite_25
       HEX ff40
;font_spr_tallsprite_26
       HEX f7d0
;font_spr_tallsprite_27
       HEX ff40
;font_spr_tallsprite_28
       HEX 7f00
;font_spr_tallsprite_29
       HEX 0f40
;font_spr_tallsprite_30
       HEX f7d0
;font_spr_tallsprite_31
       HEX 7740
;font_spr_tallsprite_32
       HEX ddd0
;font_spr_tallsprite_33
       HEX 1d00
;font_spr_tallsprite_34
       HEX 7fd0
;font_spr_tallsprite_35
       HEX 0d00
;font_spr_tallsprite_36
       HEX 17d0
;font_spr_tallsprite_37
       HEX ff40
;font_spr_tallsprite_38
       HEX f7d0
;font_spr_tallsprite_39
       HEX 3fd0
;font_spr_tallsprite_40
       HEX f7d0
;font_spr_tallsprite_41
       HEX ff40
;font_spr_tallsprite_42
       HEX f7d0
;font_spr_tallsprite_43
       HEX ff40
;font_spr_tallsprite_44
       HEX 0f40
;font_spr_tallsprite_45
       HEX 0f40
;font_spr_tallsprite_46
       HEX ff40
;font_spr_tallsprite_47
       HEX 0f40
;font_spr_tallsprite_48
       HEX ffd0
;font_spr_tallsprite_49
       HEX f7d0
;font_spr_tallsprite_50
       HEX f7d0
;font_spr_tallsprite_51
       HEX f7d0
;font_spr_tallsprite_52
       HEX f7d0
;font_spr_tallsprite_53
       HEX fd50
;font_spr_tallsprite_54
       HEX f500
;font_spr_tallsprite_55
       HEX 1f50
;font_spr_tallsprite_56
       HEX f7d0
;font_spr_tallsprite_57
       HEX f7d0
;font_spr_tallsprite_58
       HEX d0d0
;font_spr_tallsprite_59
       HEX 7f40
;font_spr_tallsprite_60
       HEX f7d0
;font_spr_tallsprite_61
       HEX d740
;font_spr_tallsprite_62
       HEX 0000
;font_spr_tallsprite_63
       HEX f500
;bar0
       HEX 0000
;bar1
       HEX 0000
;bar2
       HEX 0000
;bar3
       HEX 0000
;bar4
       HEX 0000
;bar5
       HEX 0000
;bar6
       HEX 0000
;bar7
       HEX 0000
;bar8
       HEX 0000
;hp
       HEX 00f048f078f0f048
;mp
       HEX 0d0f7ff4
;coin
       HEX 3ab0
;prism
       HEX 3ea8
;chimkin
       HEX 003c
;torch0
       HEX 10505000
;torch1
       HEX 10505000
;torch2
       HEX 31f19500
;torch3
       HEX 31f59500
;potion0
       HEX 0a80
;armor0
       HEX 02aa00
;rosary
       HEX 3030
;relic0
       HEX eb0000
;relic1
       HEX 1aa000
;relic2
       HEX 0c0000
;map0
       HEX fbbe
;door0
       HEX 001a2900
;door1
       HEX 00200200
;key0
       HEX 34

 ORG $E900,0  ; *************

;font_spr
       HEX 0000
;font_spr_tallsprite_00
       HEX f7d0
;font_spr_tallsprite_01
       HEX 1f40
;font_spr_tallsprite_02
       HEX 57d0
;font_spr_tallsprite_03
       HEX 57d0
;font_spr_tallsprite_04
       HEX 37d0
;font_spr_tallsprite_05
       HEX f450
;font_spr_tallsprite_06
       HEX ff40
;font_spr_tallsprite_07
       HEX 03d0
;font_spr_tallsprite_08
       HEX f7d0
;font_spr_tallsprite_09
       HEX f7d0
;font_spr_tallsprite_10
       HEX f7d0
;font_spr_tallsprite_11
       HEX f7d0
;font_spr_tallsprite_12
       HEX f550
;font_spr_tallsprite_13
       HEX f7d0
;font_spr_tallsprite_14
       HEX f450
;font_spr_tallsprite_15
       HEX f450
;font_spr_tallsprite_16
       HEX f550
;font_spr_tallsprite_17
       HEX f7d0
;font_spr_tallsprite_18
       HEX 0f40
;font_spr_tallsprite_19
       HEX 0f40
;font_spr_tallsprite_20
       HEX f740
;font_spr_tallsprite_21
       HEX f400
;font_spr_tallsprite_22
       HEX ffd0
;font_spr_tallsprite_23
       HEX ffd0
;font_spr_tallsprite_24
       HEX f7d0
;font_spr_tallsprite_25
       HEX f7d0
;font_spr_tallsprite_26
       HEX f7d0
;font_spr_tallsprite_27
       HEX f7d0
;font_spr_tallsprite_28
       HEX f450
;font_spr_tallsprite_29
       HEX 0f40
;font_spr_tallsprite_30
       HEX f7d0
;font_spr_tallsprite_31
       HEX f7d0
;font_spr_tallsprite_32
       HEX d0d0
;font_spr_tallsprite_33
       HEX 7f40
;font_spr_tallsprite_34
       HEX f7d0
;font_spr_tallsprite_35
       HEX 5f40
;font_spr_tallsprite_36
       HEX 3f40
;font_spr_tallsprite_37
       HEX f400
;font_spr_tallsprite_38
       HEX 3f40
;font_spr_tallsprite_39
       HEX 03d0
;font_spr_tallsprite_40
       HEX 3f40
;font_spr_tallsprite_41
       HEX 3d50
;font_spr_tallsprite_42
       HEX 3fd0
;font_spr_tallsprite_43
       HEX f400
;font_spr_tallsprite_44
       HEX 0500
;font_spr_tallsprite_45
       HEX 0500
;font_spr_tallsprite_46
       HEX f7d0
;font_spr_tallsprite_47
       HEX 0f40
;font_spr_tallsprite_48
       HEX 3740
;font_spr_tallsprite_49
       HEX ff40
;font_spr_tallsprite_50
       HEX 3f40
;font_spr_tallsprite_51
       HEX ff40
;font_spr_tallsprite_52
       HEX 3fd0
;font_spr_tallsprite_53
       HEX f7d0
;font_spr_tallsprite_54
       HEX 3f40
;font_spr_tallsprite_55
       HEX 3fd0
;font_spr_tallsprite_56
       HEX f7d0
;font_spr_tallsprite_57
       HEX f7d0
;font_spr_tallsprite_58
       HEX 0000
;font_spr_tallsprite_59
       HEX f7d0
;font_spr_tallsprite_60
       HEX f7d0
;font_spr_tallsprite_61
       HEX ffd0
;font_spr_tallsprite_62
       HEX 0000
;font_spr_tallsprite_63
       HEX 7f40
;bar0
       HEX 0000
;bar1
       HEX 0000
;bar2
       HEX 0000
;bar3
       HEX 0000
;bar4
       HEX 0000
;bar5
       HEX 0000
;bar6
       HEX 0000
;bar7
       HEX 0000
;bar8
       HEX 0000
;hp
       HEX 0000000000000000
;mp
       HEX 00000000
;coin
       HEX 3ab0
;prism
       HEX 0fa0
;chimkin
       HEX 0030
;torch0
       HEX 60f0e040
;torch1
       HEX 60f0e040
;torch2
       HEX 21656500
;torch3
       HEX 21656500
;potion0
       HEX 0000
;armor0
       HEX 069a40
;rosary
       HEX 2020
;relic0
       HEX fb0000
;relic1
       HEX 1aa000
;relic2
       HEX 0c0000
;map0
       HEX fbbe
;door0
       HEX 00da29c0
;door1
       HEX 00e002c0
;key0
       HEX 74

 ORG $EA00,0  ; *************

;font_spr
       HEX 0000
;font_spr_tallsprite_00
       HEX 37d0
;font_spr_tallsprite_01
       HEX 3f40
;font_spr_tallsprite_02
       HEX f7d0
;font_spr_tallsprite_03
       HEX f7d0
;font_spr_tallsprite_04
       HEX 0fd0
;font_spr_tallsprite_05
       HEX f5d0
;font_spr_tallsprite_06
       HEX 3d50
;font_spr_tallsprite_07
       HEX 57d0
;font_spr_tallsprite_08
       HEX f7d0
;font_spr_tallsprite_09
       HEX f7d0
;font_spr_tallsprite_10
       HEX 3f40
;font_spr_tallsprite_11
       HEX f7d0
;font_spr_tallsprite_12
       HEX f7d0
;font_spr_tallsprite_13
       HEX f7d0
;font_spr_tallsprite_14
       HEX f5d0
;font_spr_tallsprite_15
       HEX f5d0
;font_spr_tallsprite_16
       HEX f7d0
;font_spr_tallsprite_17
       HEX f7d0
;font_spr_tallsprite_18
       HEX 1f50
;font_spr_tallsprite_19
       HEX 5f50
;font_spr_tallsprite_20
       HEX f7d0
;font_spr_tallsprite_21
       HEX f400
;font_spr_tallsprite_22
       HEX f7d0
;font_spr_tallsprite_23
       HEX f7d0
;font_spr_tallsprite_24
       HEX f7d0
;font_spr_tallsprite_25
       HEX f7d0
;font_spr_tallsprite_26
       HEX f7d0
;font_spr_tallsprite_27
       HEX f7d0
;font_spr_tallsprite_28
       HEX f5d0
;font_spr_tallsprite_29
       HEX 1f50
;font_spr_tallsprite_30
       HEX f7d0
;font_spr_tallsprite_31
       HEX f7d0
;font_spr_tallsprite_32
       HEX d0d0
;font_spr_tallsprite_33
       HEX f7d0
;font_spr_tallsprite_34
       HEX f7d0
;font_spr_tallsprite_35
       HEX d7d0
;font_spr_tallsprite_36
       HEX 0000
;font_spr_tallsprite_37
       HEX f400
;font_spr_tallsprite_38
       HEX 0000
;font_spr_tallsprite_39
       HEX 03d0
;font_spr_tallsprite_40
       HEX 0000
;font_spr_tallsprite_41
       HEX 3dd0
;font_spr_tallsprite_42
       HEX 0000
;font_spr_tallsprite_43
       HEX f400
;font_spr_tallsprite_44
       HEX 0f40
;font_spr_tallsprite_45
       HEX 0f40
;font_spr_tallsprite_46
       HEX f400
;font_spr_tallsprite_47
       HEX 1f40
;font_spr_tallsprite_48
       HEX 0000
;font_spr_tallsprite_49
       HEX 0000
;font_spr_tallsprite_50
       HEX 0000
;font_spr_tallsprite_51
       HEX 0000
;font_spr_tallsprite_52
       HEX 0000
;font_spr_tallsprite_53
       HEX 0000
;font_spr_tallsprite_54
       HEX 0000
;font_spr_tallsprite_55
       HEX 0f40
;font_spr_tallsprite_56
       HEX 0000
;font_spr_tallsprite_57
       HEX 0000
;font_spr_tallsprite_58
       HEX 0000
;font_spr_tallsprite_59
       HEX 0000
;font_spr_tallsprite_60
       HEX 0000
;font_spr_tallsprite_61
       HEX 0000
;font_spr_tallsprite_62
       HEX 0000
;font_spr_tallsprite_63
       HEX d000
;bar0
       HEX 0000
;bar1
       HEX 0000
;bar2
       HEX 0000
;bar3
       HEX 0000
;bar4
       HEX 0000
;bar5
       HEX 0000
;bar6
       HEX 0000
;bar7
       HEX 0000
;bar8
       HEX 0000
;hp
       HEX 0000000000000000
;mp
       HEX 00000000
;coin
       HEX 0ec0
;prism
       HEX 0380
;chimkin
       HEX 0000
;torch0
       HEX 60b0e040
;torch1
       HEX 50b0a040
;torch2
       HEX 31757500
;torch3
       HEX 31757500
;potion0
       HEX 0000
;armor0
       HEX 1a6690
;rosary
       HEX 3470
;relic0
       HEX ff0000
;relic1
       HEX 1be000
;relic2
       HEX 3e0000
;map0
       HEX eeee
;door0
       HEX 001a2900
;door1
       HEX 00200200
;key0
       HEX dd

 ORG $EB00,0  ; *************

;font_spr
       HEX 0000
;font_spr_tallsprite_00
       HEX 0f40
;font_spr_tallsprite_01
       HEX 0f40
;font_spr_tallsprite_02
       HEX 3f40
;font_spr_tallsprite_03
       HEX 3f40
;font_spr_tallsprite_04
       HEX 03d0
;font_spr_tallsprite_05
       HEX ffd0
;font_spr_tallsprite_06
       HEX 0fd0
;font_spr_tallsprite_07
       HEX ffd0
;font_spr_tallsprite_08
       HEX 3f40
;font_spr_tallsprite_09
       HEX 3f40
;font_spr_tallsprite_10
       HEX 0d00
;font_spr_tallsprite_11
       HEX ff40
;font_spr_tallsprite_12
       HEX 3f40
;font_spr_tallsprite_13
       HEX ff40
;font_spr_tallsprite_14
       HEX ffd0
;font_spr_tallsprite_15
       HEX ffd0
;font_spr_tallsprite_16
       HEX 3f40
;font_spr_tallsprite_17
       HEX f7d0
;font_spr_tallsprite_18
       HEX 3fd0
;font_spr_tallsprite_19
       HEX ffd0
;font_spr_tallsprite_20
       HEX f7d0
;font_spr_tallsprite_21
       HEX f400
;font_spr_tallsprite_22
       HEX d0d0
;font_spr_tallsprite_23
       HEX d3d0
;font_spr_tallsprite_24
       HEX 3f40
;font_spr_tallsprite_25
       HEX ff40
;font_spr_tallsprite_26
       HEX 3f40
;font_spr_tallsprite_27
       HEX ff40
;font_spr_tallsprite_28
       HEX 3fd0
;font_spr_tallsprite_29
       HEX 3fd0
;font_spr_tallsprite_30
       HEX f7d0
;font_spr_tallsprite_31
       HEX f7d0
;font_spr_tallsprite_32
       HEX d0d0
;font_spr_tallsprite_33
       HEX d0d0
;font_spr_tallsprite_34
       HEX f7d0
;font_spr_tallsprite_35
       HEX ffd0
;font_spr_tallsprite_36
       HEX 0000
;font_spr_tallsprite_37
       HEX f400
;font_spr_tallsprite_38
       HEX 0000
;font_spr_tallsprite_39
       HEX 03d0
;font_spr_tallsprite_40
       HEX 0000
;font_spr_tallsprite_41
       HEX 0fd0
;font_spr_tallsprite_42
       HEX 0000
;font_spr_tallsprite_43
       HEX f400
;font_spr_tallsprite_44
       HEX 0000
;font_spr_tallsprite_45
       HEX 0000
;font_spr_tallsprite_46
       HEX f400
;font_spr_tallsprite_47
       HEX 3f40
;font_spr_tallsprite_48
       HEX 0000
;font_spr_tallsprite_49
       HEX 0000
;font_spr_tallsprite_50
       HEX 0000
;font_spr_tallsprite_51
       HEX 0000
;font_spr_tallsprite_52
       HEX 0000
;font_spr_tallsprite_53
       HEX 0000
;font_spr_tallsprite_54
       HEX 0000
;font_spr_tallsprite_55
       HEX 0f40
;font_spr_tallsprite_56
       HEX 0000
;font_spr_tallsprite_57
       HEX 0000
;font_spr_tallsprite_58
       HEX 0000
;font_spr_tallsprite_59
       HEX 0000
;font_spr_tallsprite_60
       HEX 0000
;font_spr_tallsprite_61
       HEX 0000
;font_spr_tallsprite_62
       HEX 0000
;font_spr_tallsprite_63
       HEX f400
;bar0
       HEX 0000
;bar1
       HEX 0000
;bar2
       HEX 0000
;bar3
       HEX 0000
;bar4
       HEX 0000
;bar5
       HEX 0000
;bar6
       HEX 0000
;bar7
       HEX 0000
;bar8
       HEX 0000
;hp
       HEX 0000000000000000
;mp
       HEX 00000000
;coin
       HEX 0300
;prism
       HEX 0000
;chimkin
       HEX 0000
;torch0
       HEX 60b0a040
;torch1
       HEX 50709040
;torch2
       HEX 31757500
;torch3
       HEX 31757500
;potion0
       HEX 0000
;armor0
       HEX 2999a0
;rosary
       HEX 0880
;relic0
       HEX be0000
;relic1
       HEX 1aa000
;relic2
       HEX f38000
;map0
       HEX eeee
;door0
       HEX 001a2900
;door1
       HEX 00200200
;key0
       HEX dd

 ORG $EC00,0  ; *************

;font_spr
       HEX 0000
;font_spr_tallsprite_00
       HEX 0000
;font_spr_tallsprite_01
       HEX 0000
;font_spr_tallsprite_02
       HEX 0000
;font_spr_tallsprite_03
       HEX 0000
;font_spr_tallsprite_04
       HEX 0000
;font_spr_tallsprite_05
       HEX 0000
;font_spr_tallsprite_06
       HEX 0000
;font_spr_tallsprite_07
       HEX 0000
;font_spr_tallsprite_08
       HEX 0000
;font_spr_tallsprite_09
       HEX 0000
;font_spr_tallsprite_10
       HEX 0000
;font_spr_tallsprite_11
       HEX 0000
;font_spr_tallsprite_12
       HEX 0000
;font_spr_tallsprite_13
       HEX 0000
;font_spr_tallsprite_14
       HEX 0000
;font_spr_tallsprite_15
       HEX 0000
;font_spr_tallsprite_16
       HEX 0000
;font_spr_tallsprite_17
       HEX 0000
;font_spr_tallsprite_18
       HEX 0000
;font_spr_tallsprite_19
       HEX 0000
;font_spr_tallsprite_20
       HEX 0000
;font_spr_tallsprite_21
       HEX 0000
;font_spr_tallsprite_22
       HEX 0000
;font_spr_tallsprite_23
       HEX 0000
;font_spr_tallsprite_24
       HEX 0000
;font_spr_tallsprite_25
       HEX 0000
;font_spr_tallsprite_26
       HEX 0000
;font_spr_tallsprite_27
       HEX 0000
;font_spr_tallsprite_28
       HEX 0000
;font_spr_tallsprite_29
       HEX 0000
;font_spr_tallsprite_30
       HEX 0000
;font_spr_tallsprite_31
       HEX 0000
;font_spr_tallsprite_32
       HEX 0000
;font_spr_tallsprite_33
       HEX 0000
;font_spr_tallsprite_34
       HEX 0000
;font_spr_tallsprite_35
       HEX 0000
;font_spr_tallsprite_36
       HEX 0000
;font_spr_tallsprite_37
       HEX 0000
;font_spr_tallsprite_38
       HEX 0000
;font_spr_tallsprite_39
       HEX 0000
;font_spr_tallsprite_40
       HEX 0000
;font_spr_tallsprite_41
       HEX 0000
;font_spr_tallsprite_42
       HEX 0000
;font_spr_tallsprite_43
       HEX 0000
;font_spr_tallsprite_44
       HEX 0000
;font_spr_tallsprite_45
       HEX 0000
;font_spr_tallsprite_46
       HEX 0000
;font_spr_tallsprite_47
       HEX 0000
;font_spr_tallsprite_48
       HEX 0000
;font_spr_tallsprite_49
       HEX 0000
;font_spr_tallsprite_50
       HEX 0000
;font_spr_tallsprite_51
       HEX 0000
;font_spr_tallsprite_52
       HEX 0000
;font_spr_tallsprite_53
       HEX 0000
;font_spr_tallsprite_54
       HEX 0000
;font_spr_tallsprite_55
       HEX 0000
;font_spr_tallsprite_56
       HEX 0000
;font_spr_tallsprite_57
       HEX 0000
;font_spr_tallsprite_58
       HEX 0000
;font_spr_tallsprite_59
       HEX 0000
;font_spr_tallsprite_60
       HEX 0000
;font_spr_tallsprite_61
       HEX 0000
;font_spr_tallsprite_62
       HEX 0000
;font_spr_tallsprite_63
       HEX f400
;bar0
       HEX 0000
;bar1
       HEX 0000
;bar2
       HEX 0000
;bar3
       HEX 0000
;bar4
       HEX 0000
;bar5
       HEX 0000
;bar6
       HEX 0000
;bar7
       HEX 0000
;bar8
       HEX 0000
;hp
       HEX 0000000000000000
;mp
       HEX 00000000
;coin
       HEX 0000
;prism
       HEX 0000
;chimkin
       HEX 0000
;torch0
       HEX 50a09040
;torch1
       HEX 50605040
;torch2
       HEX 31757500
;torch3
       HEX 31757500
;potion0
       HEX 0000
;armor0
       HEX 002000
;rosary
       HEX 0dc0
;relic0
       HEX 000000
;relic1
       HEX 06a000
;relic2
       HEX c0c000
;map0
       HEX 3ffe
;door0
       HEX 00062400
;door1
       HEX 00100100
;key0
       HEX dd

 ORG $ED00,0  ; *************

;font_spr
       HEX 0000
;font_spr_tallsprite_00
       HEX 0000
;font_spr_tallsprite_01
       HEX 0000
;font_spr_tallsprite_02
       HEX 0000
;font_spr_tallsprite_03
       HEX 0000
;font_spr_tallsprite_04
       HEX 0000
;font_spr_tallsprite_05
       HEX 0000
;font_spr_tallsprite_06
       HEX 0000
;font_spr_tallsprite_07
       HEX 0000
;font_spr_tallsprite_08
       HEX 0000
;font_spr_tallsprite_09
       HEX 0000
;font_spr_tallsprite_10
       HEX 0000
;font_spr_tallsprite_11
       HEX 0000
;font_spr_tallsprite_12
       HEX 0000
;font_spr_tallsprite_13
       HEX 0000
;font_spr_tallsprite_14
       HEX 0000
;font_spr_tallsprite_15
       HEX 0000
;font_spr_tallsprite_16
       HEX 0000
;font_spr_tallsprite_17
       HEX 0000
;font_spr_tallsprite_18
       HEX 0000
;font_spr_tallsprite_19
       HEX 0000
;font_spr_tallsprite_20
       HEX 0000
;font_spr_tallsprite_21
       HEX 0000
;font_spr_tallsprite_22
       HEX 0000
;font_spr_tallsprite_23
       HEX 0000
;font_spr_tallsprite_24
       HEX 0000
;font_spr_tallsprite_25
       HEX 0000
;font_spr_tallsprite_26
       HEX 0000
;font_spr_tallsprite_27
       HEX 0000
;font_spr_tallsprite_28
       HEX 0000
;font_spr_tallsprite_29
       HEX 0000
;font_spr_tallsprite_30
       HEX 0000
;font_spr_tallsprite_31
       HEX 0000
;font_spr_tallsprite_32
       HEX 0000
;font_spr_tallsprite_33
       HEX 0000
;font_spr_tallsprite_34
       HEX 0000
;font_spr_tallsprite_35
       HEX 0000
;font_spr_tallsprite_36
       HEX 0000
;font_spr_tallsprite_37
       HEX 0000
;font_spr_tallsprite_38
       HEX 0000
;font_spr_tallsprite_39
       HEX 0000
;font_spr_tallsprite_40
       HEX 0000
;font_spr_tallsprite_41
       HEX 0000
;font_spr_tallsprite_42
       HEX 0000
;font_spr_tallsprite_43
       HEX 0000
;font_spr_tallsprite_44
       HEX 0000
;font_spr_tallsprite_45
       HEX 0000
;font_spr_tallsprite_46
       HEX 0000
;font_spr_tallsprite_47
       HEX 0000
;font_spr_tallsprite_48
       HEX 0000
;font_spr_tallsprite_49
       HEX 0000
;font_spr_tallsprite_50
       HEX 0000
;font_spr_tallsprite_51
       HEX 0000
;font_spr_tallsprite_52
       HEX 0000
;font_spr_tallsprite_53
       HEX 0000
;font_spr_tallsprite_54
       HEX 0000
;font_spr_tallsprite_55
       HEX 0000
;font_spr_tallsprite_56
       HEX 0000
;font_spr_tallsprite_57
       HEX 0000
;font_spr_tallsprite_58
       HEX 0000
;font_spr_tallsprite_59
       HEX 0000
;font_spr_tallsprite_60
       HEX 0000
;font_spr_tallsprite_61
       HEX 0000
;font_spr_tallsprite_62
       HEX 0000
;font_spr_tallsprite_63
       HEX 0000
;bar0
       HEX 0000
;bar1
       HEX 0000
;bar2
       HEX 0000
;bar3
       HEX 0000
;bar4
       HEX 0000
;bar5
       HEX 0000
;bar6
       HEX 0000
;bar7
       HEX 0000
;bar8
       HEX 0000
;hp
       HEX 0000000000000000
;mp
       HEX 00000000
;coin
       HEX 0000
;prism
       HEX 0000
;chimkin
       HEX 0000
;torch0
       HEX 10505000
;torch1
       HEX 40504040
;torch2
       HEX b0b0b080
;torch3
       HEX 20202000
;potion0
       HEX 0000
;armor0
       HEX 000000
;rosary
       HEX 0200
;relic0
       HEX 000000
;relic1
       HEX 000000
;relic2
       HEX 000000
;map0
       HEX 0ffc
;door0
       HEX 00000000
;door1
       HEX 00000000
;key0
       HEX 34

 ORG $EE00,0  ; *************

;font_spr
       HEX 0000
;font_spr_tallsprite_00
       HEX 0000
;font_spr_tallsprite_01
       HEX 0000
;font_spr_tallsprite_02
       HEX 0000
;font_spr_tallsprite_03
       HEX 0000
;font_spr_tallsprite_04
       HEX 0000
;font_spr_tallsprite_05
       HEX 0000
;font_spr_tallsprite_06
       HEX 0000
;font_spr_tallsprite_07
       HEX 0000
;font_spr_tallsprite_08
       HEX 0000
;font_spr_tallsprite_09
       HEX 0000
;font_spr_tallsprite_10
       HEX 0000
;font_spr_tallsprite_11
       HEX 0000
;font_spr_tallsprite_12
       HEX 0000
;font_spr_tallsprite_13
       HEX 0000
;font_spr_tallsprite_14
       HEX 0000
;font_spr_tallsprite_15
       HEX 0000
;font_spr_tallsprite_16
       HEX 0000
;font_spr_tallsprite_17
       HEX 0000
;font_spr_tallsprite_18
       HEX 0000
;font_spr_tallsprite_19
       HEX 0000
;font_spr_tallsprite_20
       HEX 0000
;font_spr_tallsprite_21
       HEX 0000
;font_spr_tallsprite_22
       HEX 0000
;font_spr_tallsprite_23
       HEX 0000
;font_spr_tallsprite_24
       HEX 0000
;font_spr_tallsprite_25
       HEX 0000
;font_spr_tallsprite_26
       HEX 0000
;font_spr_tallsprite_27
       HEX 0000
;font_spr_tallsprite_28
       HEX 0000
;font_spr_tallsprite_29
       HEX 0000
;font_spr_tallsprite_30
       HEX 0000
;font_spr_tallsprite_31
       HEX 0000
;font_spr_tallsprite_32
       HEX 0000
;font_spr_tallsprite_33
       HEX 0000
;font_spr_tallsprite_34
       HEX 0000
;font_spr_tallsprite_35
       HEX 0000
;font_spr_tallsprite_36
       HEX 0000
;font_spr_tallsprite_37
       HEX 0000
;font_spr_tallsprite_38
       HEX 0000
;font_spr_tallsprite_39
       HEX 0000
;font_spr_tallsprite_40
       HEX 0000
;font_spr_tallsprite_41
       HEX 0000
;font_spr_tallsprite_42
       HEX 0000
;font_spr_tallsprite_43
       HEX 0000
;font_spr_tallsprite_44
       HEX 0000
;font_spr_tallsprite_45
       HEX 0000
;font_spr_tallsprite_46
       HEX 0000
;font_spr_tallsprite_47
       HEX 0000
;font_spr_tallsprite_48
       HEX 0000
;font_spr_tallsprite_49
       HEX 0000
;font_spr_tallsprite_50
       HEX 0000
;font_spr_tallsprite_51
       HEX 0000
;font_spr_tallsprite_52
       HEX 0000
;font_spr_tallsprite_53
       HEX 0000
;font_spr_tallsprite_54
       HEX 0000
;font_spr_tallsprite_55
       HEX 0000
;font_spr_tallsprite_56
       HEX 0000
;font_spr_tallsprite_57
       HEX 0000
;font_spr_tallsprite_58
       HEX 0000
;font_spr_tallsprite_59
       HEX 0000
;font_spr_tallsprite_60
       HEX 0000
;font_spr_tallsprite_61
       HEX 0000
;font_spr_tallsprite_62
       HEX 0000
;font_spr_tallsprite_63
       HEX 0000
;bar0
       HEX 0000
;bar1
       HEX 0000
;bar2
       HEX 0000
;bar3
       HEX 0000
;bar4
       HEX 0000
;bar5
       HEX 0000
;bar6
       HEX 0000
;bar7
       HEX 0000
;bar8
       HEX 0000
;hp
       HEX 0000000000000000
;mp
       HEX 00000000
;coin
       HEX 0000
;prism
       HEX 0000
;chimkin
       HEX 0000
;torch0
       HEX 10405000
;torch1
       HEX 00100000
;torch2
       HEX 20202000
;torch3
       HEX 10101000
;potion0
       HEX 0000
;armor0
       HEX 000000
;rosary
       HEX 0300
;relic0
       HEX 000000
;relic1
       HEX 000000
;relic2
       HEX 000000
;map0
       HEX 0000
;door0
       HEX 00000000
;door1
       HEX 00000000
;key0
       HEX 00

 ORG $EF00,0  ; *************

;font_spr
       HEX 0000
;font_spr_tallsprite_00
       HEX 0000
;font_spr_tallsprite_01
       HEX 0000
;font_spr_tallsprite_02
       HEX 0000
;font_spr_tallsprite_03
       HEX 0000
;font_spr_tallsprite_04
       HEX 0000
;font_spr_tallsprite_05
       HEX 0000
;font_spr_tallsprite_06
       HEX 0000
;font_spr_tallsprite_07
       HEX 0000
;font_spr_tallsprite_08
       HEX 0000
;font_spr_tallsprite_09
       HEX 0000
;font_spr_tallsprite_10
       HEX 0000
;font_spr_tallsprite_11
       HEX 0000
;font_spr_tallsprite_12
       HEX 0000
;font_spr_tallsprite_13
       HEX 0000
;font_spr_tallsprite_14
       HEX 0000
;font_spr_tallsprite_15
       HEX 0000
;font_spr_tallsprite_16
       HEX 0000
;font_spr_tallsprite_17
       HEX 0000
;font_spr_tallsprite_18
       HEX 0000
;font_spr_tallsprite_19
       HEX 0000
;font_spr_tallsprite_20
       HEX 0000
;font_spr_tallsprite_21
       HEX 0000
;font_spr_tallsprite_22
       HEX 0000
;font_spr_tallsprite_23
       HEX 0000
;font_spr_tallsprite_24
       HEX 0000
;font_spr_tallsprite_25
       HEX 0000
;font_spr_tallsprite_26
       HEX 0000
;font_spr_tallsprite_27
       HEX 0000
;font_spr_tallsprite_28
       HEX 0000
;font_spr_tallsprite_29
       HEX 0000
;font_spr_tallsprite_30
       HEX 0000
;font_spr_tallsprite_31
       HEX 0000
;font_spr_tallsprite_32
       HEX 0000
;font_spr_tallsprite_33
       HEX 0000
;font_spr_tallsprite_34
       HEX 0000
;font_spr_tallsprite_35
       HEX 0000
;font_spr_tallsprite_36
       HEX 0000
;font_spr_tallsprite_37
       HEX 0000
;font_spr_tallsprite_38
       HEX 0000
;font_spr_tallsprite_39
       HEX 0000
;font_spr_tallsprite_40
       HEX 0000
;font_spr_tallsprite_41
       HEX 0000
;font_spr_tallsprite_42
       HEX 0000
;font_spr_tallsprite_43
       HEX 0000
;font_spr_tallsprite_44
       HEX 0000
;font_spr_tallsprite_45
       HEX 0000
;font_spr_tallsprite_46
       HEX 0000
;font_spr_tallsprite_47
       HEX 0000
;font_spr_tallsprite_48
       HEX 0000
;font_spr_tallsprite_49
       HEX 0000
;font_spr_tallsprite_50
       HEX 0000
;font_spr_tallsprite_51
       HEX 0000
;font_spr_tallsprite_52
       HEX 0000
;font_spr_tallsprite_53
       HEX 0000
;font_spr_tallsprite_54
       HEX 0000
;font_spr_tallsprite_55
       HEX 0000
;font_spr_tallsprite_56
       HEX 0000
;font_spr_tallsprite_57
       HEX 0000
;font_spr_tallsprite_58
       HEX 0000
;font_spr_tallsprite_59
       HEX 0000
;font_spr_tallsprite_60
       HEX 0000
;font_spr_tallsprite_61
       HEX 0000
;font_spr_tallsprite_62
       HEX 0000
;font_spr_tallsprite_63
       HEX 0000
;bar0
       HEX 0000
;bar1
       HEX 0000
;bar2
       HEX 0000
;bar3
       HEX 0000
;bar4
       HEX 0000
;bar5
       HEX 0000
;bar6
       HEX 0000
;bar7
       HEX 0000
;bar8
       HEX 0000
;hp
       HEX 0000000000000000
;mp
       HEX 00000000
;coin
       HEX 0000
;prism
       HEX 0000
;chimkin
       HEX 0000
;torch0
       HEX 00404000
;torch1
       HEX 10001000
;torch2
       HEX 10101000
;torch3
       HEX 00000000
;potion0
       HEX 0000
;armor0
       HEX 000000
;rosary
       HEX 0000
;relic0
       HEX 000000
;relic1
       HEX 000000
;relic2
       HEX 000000
;map0
       HEX 0000
;door0
       HEX 00000000
;door1
       HEX 00000000
;key0
       HEX 00
 if SPACEOVERFLOW > 0
  echo ""
  echo "######## ERROR: space overflow detected in",[SPACEOVERFLOW]d,"areas."
  echo "######## look above for areas with negative ROM space left."
 endif
 

 ; Provided under the CC0 license. See the included LICENSE.txt for details.

 ifnconst bankswitchmode
   if ( * < $f000 )
     ORG $F000
   endif
 else
     ifconst ROM128K
       if ( * < $f000 )
         ORG $27000
         RORG $F000
       endif
     endif
     ifconst ROM144K
       if ( * < $f000 )
         ORG $27000
         RORG $F000
       endif
     endif
    ifconst ROM256K
       if ( * < $f000 )
         ORG $47000
         RORG $F000
       endif
     endif
    ifconst ROM272K
       if ( * < $f000 )
         ORG $47000
         RORG $F000
       endif
     endif
    ifconst ROM512K
       if ( * < $f000 )
         ORG $87000
         RORG $F000
       endif
     endif
    ifconst ROM528K
       if ( * < $f000 )
         ORG $87000
         RORG $F000
       endif
     endif
 endif

 ; all of these "modules" have conditional clauses in them, so even though
 ; they're always included here, they don't take up rom unless the user
 ; explicitly enables support for the feature.

 ifnconst included.rmtplayer.asm
     include rmtplayer.asm ; requires page alignment, so go first
 endif
 ifnconst included.7800vox.asm
     include 7800vox.asm
 endif
 ifnconst included.pokeysound.asm
     include pokeysound.asm
 endif
 ifnconst included.snes2atari.asm
     include snes2atari.asm
 endif
 ifnconst included.mega7800.asm
     include mega7800.asm
 endif
 ifnconst included.tracker.asm
     include tracker.asm
 endif
 ifnconst included.hiscore.asm
     include hiscore.asm
 endif
 ifnconst included.fourbitfade.asm
     include fourbitfade.asm
 endif
 ifnconst included.plotsprite4.asm
     include plotsprite4.asm
 endif
 ifnconst included.lzsa1compression.asm
     include lzsa1compression.asm
 endif
     ; Provided under the CC0 license. See the included LICENSE.txt for details.

     ;standard routimes needed for pretty much all games

     ; some definitions used with "set debug color"
DEBUGCALC     = $91
DEBUGWASTE     = $41
DEBUGDRAW     = $C1

     ;NMI and IRQ handlers
NMI
     ;VISIBLEOVER is 255 while the screen is drawn, and 0 right after the visible screen is done.
     pha ; save A
     cld
     lda visibleover
     eor #255
     sta visibleover
     ifconst DEBUGINTERRUPT
         and #$93
         sta BACKGRND
     endif
     txa ; save X
     pha
     tya ; save Y
     pha
     dec interruptindex 
     bne skipreallyoffvisible
     jmp reallyoffvisible
skipreallyoffvisible
     lda visibleover
     bne carryontopscreenroutine
     ifconst .bottomscreenroutine
         lda interrupthold
         beq skipbottomroutine
         jsr .bottomscreenroutine
skipbottomroutine
     endif
     jmp NMIexit
carryontopscreenroutine
     ifconst .topscreenroutine
         lda interrupthold
         beq skiptoproutine
         jsr .topscreenroutine
skiptoproutine
     endif
     ifnconst CANARYOFF
         lda canary
         beq skipcanarytriggered
         lda #$45
         sta BACKGRND
         jmp skipbrkolorset ; common crash dump routine, if available
skipcanarytriggered
     endif

     inc frameslost ; this is balanced with a "dec frameslost" when drawscreen is called.

     ; ** Other important routines that need to regularly run, and can run onscreen.
     ; ** Atarivox can't go here, because Maria might interrupt it while it's bit-banging.

     ifconst LONGCONTROLLERREAD
longcontrollerreads         ; ** controllers that take a lot of time to read. We use much of the visible screen here.
         ldy port1control
         lda longreadtype,y
         beq LLRET1
         tay
         lda longreadroutinehiP1,y
         sta inttemp4
         lda longreadroutineloP1,y
         sta inttemp3
         jmp (inttemp3)
LLRET1
         ldy port0control
         lda longreadtype,y
         beq LLRET0
         tay
         lda longreadroutinehiP0,y
         sta inttemp4
         lda longreadroutineloP0,y
         sta inttemp3
         jmp (inttemp3)
LLRET0


         ifconst PADDLERANGE
TIMEVAL             = PADDLERANGE
         else
TIMEVAL             = 160
         endif
TIMEOFFSET         = 10

     endif ; LONGCONTROLLERREAD


     jsr servicesfxchannels 
     ifconst MUSICTRACKER
         jsr servicesong
     endif ; MUSICTRACKER
     ifconst RMT
         ifnconst RMTOFFSPEED
             ifconst RMTPALSPEED
                 lda ntscslowframe
                 bne skiprasterupdate
             endif
         endif
         lda rasterpause
         beq skiprasterupdate
 ifconst PAUSESILENT
         lda pausestate 
         bne skiprasterupdate
 endif
         jsr RASTERMUSICTRACKER+3
skiprasterupdate
RMT_Iend
     endif

     inc framecounter
     lda framecounter
     and #63
     bne skipcountdownseconds
     lda countdownseconds
     beq skipcountdownseconds
     dec countdownseconds
skipcountdownseconds

     ldx #1
buttonreadloop
     txa
     pha
     ldy port0control,x
     lda buttonhandlerlo,y
     sta inttemp3
     lda buttonhandlerhi,y
     sta inttemp4
     ora inttemp3
     beq buttonreadloopreturn
     jmp (inttemp3)
buttonreadloopreturn
     pla
     tax
     dex
     bpl buttonreadloop

     ifconst DOUBLEBUFFER
         lda doublebufferminimumframeindex
         beq skipdoublebufferminimumframeindexadjust
         dec doublebufferminimumframeindex
skipdoublebufferminimumframeindexadjust
     endif
     
     jmp NMIexit

IRQ     ; the only source of non-nmi interrupt should be the BRK opcode.
     ifnconst BREAKPROTECTOFF
         lda #$1A
         sta BACKGRND
skipbrkolorset
skipbrkdetected
         lda #$60
         sta sCTRL
         sta CTRL
         ifnconst hiscorefont
             .byte $02 ; KIL/JAM
         else ; hiscorefont is present
             ifconst CRASHDUMP
                 bit MSTAT
                 bpl skipbrkdetected ; wait for vblank to ensure we're clear of NMI

                 ifconst dumpbankswitch
                     lda dumpbankswitch
                     pha
                 endif

                 ; bankswitch if needed, to get to the hiscore font
                 ifconst bankswitchmode
                     ifconst included.hiscore.asm.bank
                         ifconst MCPDEVCART
                             lda #($18 | included.hiscore.asm.bank)
                             sta $3000
                         else
                             lda #(included.hiscore.asm.bank)
                             sta $8000
                         endif
                     endif ; included.hiscore.asm.bank
                 endif ; bankswitchmode

                 ifconst DOUBLEBUFFER
                     ;turn off double-buffering, if on...
                     lda #>DLLMEM
                     sta DPPH
                     lda #<DLLMEM
                     sta DPPL
                 endif

                 lda #$00
                 sta P0C2

                 ;update the second-from-top DL...
                 ldy #8
NMIupdatetopDL
                 lda show2700,y
                 sta ZONE1ADDRESS,y
                 dey
                 bpl NMIupdatetopDL

                 ; the hiscore font is present, so we try to output the stack
                 ldy #0
copystackloop
                 pla
                 pha
                 lsr
                 lsr
                 lsr
                 lsr
                 tax
                 lda hiscorehexlut,x
                 sta $2700,y
                 iny

                 pla
                 and #$0F
                 tax
                 lda hiscorehexlut,x
                 sta $2700,y
                 iny

                 lda #27 ; period
                 sta $2700,y
                 iny

                 cpy #30
                 bne copystackloop

                 lda #>hiscorefont
                 sta CHARBASE
                 sta sCHARBASE
                 lda #%01000011 ;Enable DMA, mode=320A
                 sta CTRL
                 sta sCTRL
                 .byte $02 ; KIL/JAM
hiscorehexlut
                 ; 0 1 2 3 4 5 6 7 8 9 A B C D E F
                 .byte 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 0, 1, 2, 3, 4, 5
show2700
                 ; lo mode hi width=29 x EODL
                 .byte $00, %01100000, $27, 3, 20, 0,0,0
             else ; CRASHDUMP
                 .byte $02 ; KIL/JAM
             endif ; crashdump
         endif ; hiscorefont
     else
         RTI
     endif

     ifconst LONGCONTROLLERREAD

longreadtype
         .byte 0, 0, 0, 1 ; NONE PROLINE LIGHTGUN PADDLE
         .byte 2, 0, 3, 0 ; TRKBALL VCSSTICK DRIVING KEYPAD
         .byte 3, 3, 0, 0 ; STMOUSE AMOUSE ATARIVOX SNES

longreadroutineloP0
         .byte <LLRET0 ; 0 = no routine
         .byte <paddleport0update ; 1 = paddle
         .byte <trakball0update ; 2 = trakball
         .byte <mouse0update ; 3 = mouse

longreadroutinehiP0
         .byte >LLRET0 ; 0 = no routine
         .byte >paddleport0update ; 1 = paddle
         .byte >trakball0update ; 2 = trackball
         .byte >mouse0update ; 3 = mouse

longreadroutineloP1
         .byte <LLRET1 ; 0 = no routine
         .byte <paddleport1update ; 1 = paddle
         .byte <trakball1update ; 2 = trakball
         .byte <mouse1update ; 3 = mouse

longreadroutinehiP1
         .byte >LLRET1 ; 0 = no routine
         .byte >paddleport1update ; 1 = paddle
         .byte >trakball1update ; 2 = trackball
         .byte >mouse1update ; 3 = mouse


SETTIM64T
         bne skipdefaulttime
         ifnconst PADDLESMOOTHINGOFF
             lda #(TIMEVAL+TIMEOFFSET+1)
         else
             lda #(TIMEVAL+TIMEOFFSET)
         endif
skipdefaulttime
         tay
         dey
.setTIM64Tloop
         sta TIM64T
         cpy INTIM
         bne .setTIM64Tloop
         rts
     endif ; LONGCONTROLLERREAD

reallyoffvisible
     sta WSYNC

     lda #0
     sta visibleover
     ifconst DEBUGINTERRUPT
         sta BACKGRND
     endif

     lda #3
     sta interruptindex

     jsr uninterruptableroutines

     ifconst .userinterrupt
         lda interrupthold
         beq skipuserintroutine
         jsr .userinterrupt
skipuserintroutine
     endif

     ifconst KEYPADSUPPORT
         jsr keypadcolumnread
         jsr keypadrowselect
     endif

NMIexit
     pla
     tay
     pla
     tax
     pla
     RTI

clearscreen
     ldx #(WZONECOUNT-1)
     lda #0
clearscreenloop
     sta dlend,x
     dex
     bpl clearscreenloop
     lda #0
     sta valbufend ; clear the bcd value buffer
     sta valbufendsave 
     rts

restorescreen
     ldx #(WZONECOUNT-1)
     lda #0
restorescreenloop
     lda dlendsave,x
     sta dlend,x
     dex
     bpl restorescreenloop
     lda valbufendsave
     sta valbufend
     rts

savescreen
     ldx #(WZONECOUNT-1)
savescreenloop
     lda dlend,x
     sta dlendsave,x
     dex
     bpl savescreenloop
     lda valbufend
     sta valbufendsave
     ifconst DOUBLEBUFFER
         lda doublebufferstate
         beq savescreenrts
         lda #1
         sta doublebufferbufferdirty
savescreenrts
     endif ; DOUBLEBUFFER
     rts

drawscreen

     ifconst interrupthold
         lda #$FF
         sta interrupthold ; if the user called drawscreen, we're ready for interrupts
     endif

     lda #0
     sta temp1 ; not B&W if we're here...

drawscreenwait
     lda visibleover
     bne drawscreenwait ; make sure the visible screen isn't being drawn

     ;restore some registers in case the game changed them mid-screen...
     lda sCTRL
     ora temp1
     sta CTRL
     lda sCHARBASE
     sta CHARBASE

     ;ensure all of the display list is terminated...
     jsr terminatedisplaylist

     ifnconst pauseroutineoff
         jsr pauseroutine
     endif ; pauseroutineoff

     ; Make sure the visible screen has *started* before we exit. That way we can rely on drawscreen
     ; delaying a full frame, but still allowing time for basic calculations.
visiblescreenstartedwait
     lda visibleover
     beq visiblescreenstartedwait
visiblescreenstartedwaitdone
     dec frameslost ; ; this gets balanced with an "inc frameslost" by an NMI at the top of the screen
     rts

     ifnconst pauseroutineoff
         ; check to see if pause was pressed and released
pauseroutine
         lda pausedisable
         bne leavepauseroutine
         lda #8
         bit SWCHB
         beq pausepressed

         ifconst SNES0PAUSE
             lda port0control
             cmp #11
             bne skipsnes0pause
             lda snesdetected0
             beq skipsnes0pause
             lda snes2atari0hi
             and #%00010000
             beq pausepressed
skipsnes0pause
         endif
         ifconst SNES1PAUSE

             lda port1control
             cmp #11
             bne skipsnes1pause
             lda snesdetected1
             beq skipsnes1pause
             lda snes2atari1hi
             and #%00010000
             beq pausepressed
skipsnes1pause
         endif
         ifconst SNESNPAUSE
             ldx snesport
             lda port0control,x
             cmp #11
             bne skipsnesNpause
             lda snesdetected0,x
             beq skipsnesNpause
             lda snes2atari0hi,x
             and #%00010000
             beq pausepressed
skipsnesNpause
         endif
         ifconst MULTIBUTTONPAUSE
             ldx #1
multibuttonpauseloop
             lda port0control,x
             cmp #11
             bcc multibuttonpauseloopbottom
             lda sINPT1,x
             and #1
             beq pausepressed
multibuttonpauseloopbottom
             dex
             bpl multibuttonpauseloop
         endif ; MULTIBUTTONPAUSE

         ;pause isn't pressed
         lda #0
         sta pausebuttonflag ; clear pause hold state in case its set

         ;check if we're in an already paused state
         lda pausestate
         beq leavepauseroutine ; nope, leave

         cmp #1 ; last frame was the start of pausing
         beq enterpausestate2 ; move from state 1 to 2

         cmp #2
         beq carryonpausing

         ;pausestate must be >2, which means we're ending an unpause 
         lda #0
         sta pausebuttonflag 
         sta pausestate 
         lda sCTRL
         sta CTRL
         jmp leavepauseroutine

pausepressed
         ;pause is pressed
         lda pausebuttonflag
         cmp #$ff
         beq carryonpausing

         ;its a new press, increment the state
         inc pausestate

         ;silence volume at the start and end of pausing
         lda #0 
         sta AUDV0
         sta AUDV1

         ifconst pokeysupport
             ldy #7
pausesilencepokeyaudioloop
             sta (pokeybase),y
             dey
             bpl pausesilencepokeyaudioloop
         endif ; pokeysupport

         lda #$ff
         sta pausebuttonflag
         bne carryonpausing

enterpausestate2
         lda #2
         sta pausestate
         bne carryonpausing
leavepauseroutine
         lda sCTRL
         sta CTRL
         rts
carryonpausing
         ifconst .pause
             jsr .pause
         endif ; .pause
         lda sCTRL
         ora #%10000000 ; turn off colorburst during pause...
         sta CTRL
         jmp pauseroutine
     endif ; pauseroutineoff


     ifconst DOUBLEBUFFER
skipterminatedisplaylistreturn
         rts
     endif ; DOUBLEBUFFER
terminatedisplaylist
     ifconst DOUBLEBUFFER
         lda doublebufferstate
         bne skipterminatedisplaylistreturn ; double-buffering runs it's own DL termination code
     endif ; DOUBLEBUFFER
terminatedisplaybuffer
     ;add DL end entry on each DL
     ldx #(WZONECOUNT-1)
dlendloop
     ifconst VSCROLL
         ldy Xx3,x
         lda DLLMEM+11,y
     else  ; !VSCROLL
         lda DLPOINTL,x ;Get pointer to DL that this sprite starts in
     endif ; !VSCROLL
     ifconst DOUBLEBUFFER
         clc
         adc doublebufferdloffset
     endif ; DOUBLEBUFFER
     sta dlpnt
     ifconst VSCROLL
         lda DLLMEM+10,y
     else  ; !VSCROLL
         lda DLPOINTH,x
     endif ; !VSCROLL
     ifconst DOUBLEBUFFER
         adc #0
     endif ; DOUBLEBUFFER
     sta dlpnt+1
     ldy dlend,x
     lda #$00
dlendmoreloops
     iny
     sta (dlpnt),y
     ifconst FRAMESKIPGLITCHFIXWEAK
         cpy #DLLASTOBJ+1
         beq dlendthiszonedone
         iny
         iny
         iny
         iny
         iny
         sta (dlpnt),y
dlendthiszonedone
     endif FRAMESKIPGLITCHFIXWEAK
     ifconst FRAMESKIPGLITCHFIX
         iny
         iny
         iny
         iny
         cpy #DLLASTOBJ-1
         bcc dlendmoreloops
     endif ; FRAMESKIPGLITCHFIX
     dex
     bpl dlendloop

     ifnconst pauseroutineoff
         jsr pauseroutine
     endif ; pauseroutineoff
     rts

uninterruptableroutines
     ; this is for routines that must happen off the visible screen, each frame.

     ifconst AVOXVOICE
         jsr serviceatarivoxqueue
     endif
     ifconst MEGA7800SUPPORT
         ldx #1
mega7800polling
         lda port0control,x
         cmp #12 ; mega7800
         bne mega7800handlercheck2
         jsr mega7800handler
         jmp mega7800handlerdone
mega7800handlercheck2
     ifconst MULTIBUTTON
             cmp #1 ; proline
             bne mega7800handlerdone
             lda framecounter
             eor #7 ; avoid the same frame as the snes2atari probe
             and #63
             bne mega7800handlerdone
             lda #12
             sta port0control,x
             jsr mega7800handler
     endif ; MULTIBUTTON
mega7800handlerdone
         dex
         bpl mega7800polling
     endif ; MEGA7800SUPPORT

     lda #0
     sta palfastframe
     sta ntscslowframe
     ldy palframes
     iny
     ldx paldetected ; 0=ntsc 1=pal
     beq ntsc2palskipcheck
pal2ntscskipcheck
     cpy #5 ; every 5th frame, add a frame
     bne palframeskipdone
     beq frameskipdo
ntsc2palskipcheck
     cpy #6 ; every 6th frame, drop a frame
     bne palframeskipdone
frameskipdo
     inc ntscslowframe,x
     ldy #0
palframeskipdone
     sty palframes
skippalframeadjusting

     ifconst MUSICTRACKER
         ; We normally run the servicesong routine from the top-screen interrupt, but if it
         ; happens to interrupt the scheduling of a sound effect in the game code, we skip it.
         ; If that happens, we try again here. Chances are very small we'll run into the same
         ; problem twice, and if we do, we just drop a musical note or two.
         lda sfxschedulemissed
         beq servicesongwasnotmissed
         jsr servicesong
servicesongwasnotmissed
     endif ; MUSICTRACKER

     ifconst RMT
         ifnconst RMTPALSPEED
             ifnconst RMTOFFSPEED
 ifconst PAUSESILENT
         lda pausestate 
         bne skiprasterupdate2
 endif
                 lda palfastframe
                 beq skiprasterupdate2
                 lda rasterpause
                 beq skiprasterupdate2
                 jsr RASTERMUSICTRACKER+3
skiprasterupdate2
             endif
         endif
     endif

     rts

serviceatarivoxqueue
     ifconst AVOXVOICE
         lda voxlock
         bne skipvoxprocessing ; the vox is in the middle of speech address update
skipvoxqueuesizedec
         jmp processavoxvoice
skipvoxprocessing
         rts

processavoxvoice
         ifconst HSSUPPORT
             ; ** we skip speech if hi-score is on and no vox was detected
             ; ** this is to avoid later collision with snes pads.
             lda hsdevice
             and #2
             beq processavoxvoicereturn
         endif ; HSSUPPORT
         lda avoxenable
         bne avoxfixport
         SPKOUT tempavox
         rts
avoxfixport
         lda #0 ; restore the port to all bits as inputs...
         sta CTLSWA
         rts
silenceavoxvoice
         SPEAK avoxsilentdata
processavoxvoicereturn
         rts
avoxsilentdata
         .byte 31,255
     else
         rts
     endif ; AVOXVOICE

prolinebuttonpadhandler
     ifconst MULTIBUTTON
         lda framecounter
         and #63
         bne jbhandlercont1
         jsr setonebuttonmode
         lda #11
         sta port0control,x
         jsr snes2atari_signal_go
         lda port0control,x
         cmp #1 ; check if it's still a proline 
         beq jbhandlercont1
         jmp buttonreadloopreturn
jbhandlercont1
     lda #2
     sta multibuttoncount0,x
     endif ; MULTIBUTTON
joybuttonpadhandler
     lda sSWCHA             ; clear previous dirs for this pad, from
     ora SWCHA_DIRMASK,x    ; our sSWCHA nibble.
     sta sSWCHA
     lda SWCHA              ; load th actual joystick dirs, ensuring
     ora SWCHA_DIRMASK+1,x  ; we don't change the other nibble.
     and sSWCHA
     sta sSWCHA 
joybuttonhandler
     txa
     asl
     tay
     lda INPT0,y
     lsr
     ;ora #%00111111
     sta sINPT1,x
     lda INPT1,y
     and #%10000000
     ora sINPT1,x
     sta sINPT1,x

     lda INPT4,x
     bmi .skip1bjoyfirecheck
     ;one button joystick is down
     eor #%10000000
     sta sINPT1,x

     lda joybuttonmode
     and thisjoy2buttonbit,x
     beq .skip1bjoyfirecheck
     lda joybuttonmode
     ora thisjoy2buttonbit,x
     sta joybuttonmode
     sta SWCHB
.skip1bjoyfirecheck
     lda #%00111111
     ora sINPT1,x
     sta sINPT1,x ; ensure multibutton bits are hi
     jmp buttonreadloopreturn

SWCHA_DIRMASK
             ;  p0  p1  p0
         .byte $F0,$0F,$F0

gunbuttonhandler     ; outside of the conditional, so our button handler LUT is valid
     ifconst LIGHTGUNSUPPORT
         cpx #0
         bne secondportgunhandler
firstportgunhandler
         lda SWCHA
         asl 
         asl 
         asl ; shift D4 to D7
         and #%10000000
         eor #%10000000
         sta sINPT1
         jmp buttonreadloopreturn
secondportgunhandler
         lda SWCHA
         lsr ; shift D0 into carry
         lsr ; shift carry into D7
         and #%10000000
         eor #%10000000
         sta sINPT3
         jmp buttonreadloopreturn
     endif ; LIGHTGUNSUPPORT

controlsusing2buttoncode
     .byte 0 ; 00=no controller plugged in
     .byte 1 ; 01=proline joystick
     .byte 0 ; 02=lightgun
     .byte 0 ; 03=paddle
     .byte 1 ; 04=trakball
     .byte 1 ; 05=vcs joystick
     .byte 1 ; 06=driving control
     .byte 0 ; 07=keypad control
     .byte 0 ; 08=st mouse/cx80
     .byte 0 ; 09=amiga mouse
     .byte 1 ; 10=atarivox
     .byte 0 ; 11=snes2atari
     .byte 0 ; 12=mega7800

buttonhandlerhi
     .byte 0                        ; 00=no controller plugged in
     .byte >prolinebuttonpadhandler ; 01=proline joystick
     .byte >gunbuttonhandler        ; 02=lightgun
     .byte >paddlebuttonhandler     ; 03=paddle
     .byte >joybuttonhandler        ; 04=trakball
     .byte >joybuttonpadhandler     ; 05=vcs joystick
     .byte >joybuttonhandler        ; 06=driving control
     .byte 0                        ; 07=keypad
     .byte >mousebuttonhandler      ; 08=st mouse
     .byte >mousebuttonhandler      ; 09=amiga mouse
     .byte >joybuttonhandler        ; 10=atarivox
     .byte >snes2atarihandler       ; 11=snes
     .byte 0                        ; 12=mega7800
buttonhandlerlo
     .byte 0                        ; 00=no controller plugged in
     .byte <prolinebuttonpadhandler ; 01=proline joystick
     .byte <gunbuttonhandler        ; 02=lightgun 
     .byte <paddlebuttonhandler     ; 03=paddle
     .byte <joybuttonhandler        ; 04=trakball
     .byte <joybuttonpadhandler     ; 05=vcs joystick
     .byte <joybuttonhandler        ; 06=driving control
     .byte 0                        ; 07=keypad
     .byte <mousebuttonhandler      ; 08=st mouse
     .byte <mousebuttonhandler      ; 09=amiga mouse
     .byte <joybuttonhandler        ; 10=atarivox
     .byte <snes2atarihandler       ; 11=snes
     .byte 0                        ; 12=mega7800

drawwait
     bit visibleover ; 255 if screen is being drawn, 0 when not.
     bmi drawwait ; make sure the visible screen isn't being drawn
     rts

drawoverwait
     bit visibleover ; 255 if screen is being drawn, 0 when not.
     bpl drawoverwait ; make sure the visible screen is being drawn
     rts


mutetia
     lda #0
     ldx #3
mutetialoop
     sta sfx1pointlo,x
     sta AUDF0,x
     dex
     bpl mutetialoop
     rts

servicesfxchannelsdone
     ifnconst pokeysupport
         rts
     else
         jmp checkpokeyplaying
     endif
servicesfxchannels
 ifconst PAUSESILENT
     lda pausestate
     beq servicesfxchannels_1
     rts
servicesfxchannels_1
 endif
     ldx #255
servicesfxchannelsloop
     inx
     ifnconst TIASFXMONO
         cpx #2
     else
         cpx #1
     endif
     beq servicesfxchannelsdone

     lda sfxschedulelock ; =1 if locked
     bne servicesfxchannelsdone ; exit if a pointer may be mid-way change

     lda sfx1pointlo,x
     sta inttemp5
     ora sfx1pointhi,x
     beq servicesfxchannelsloop
     lda sfx1pointhi,x
     sta inttemp6

     lda sfx1tick,x
     beq servicesfx_cont1 ; this chunk is over, load the next!
     dec sfx1tick,x ; frame countdown is non-zero, subtract one
     jmp servicesfxchannelsloop
servicesfx_cont1

     ldy #1 ; check to see if they're changing the frame countdown
     lda (inttemp5),y
     cmp #$10
     bne servicesfx_cont1a
     ldy #2
     lda (inttemp5),y
     sta sfx1frames,x ; change the frame countdown
     lda #0
     sta sfx1tick,x
     ; advance the sound pointer by 3...
     lda sfx1pointlo,x
     clc
     adc #3
     sta sfx1pointlo,x
     lda sfx1pointhi,x
     adc #0
     sta sfx1pointhi,x
     ; and then fetch another sample for this channel...
     dex 
     jmp servicesfxchannelsloop
servicesfx_cont1a

     lda sfx1frames,x ; set the frame countdown for this sound chunk
     sta sfx1tick,x

     lda sfx1priority,x ; decrease the sound's priority if its non-zero
     beq servicesfx_cont2
     dec sfx1priority,x
servicesfx_cont2

     ldy #0 ; play the sound
     lda (inttemp5),y
     sta inttemp1

     ifconst MUSICTRACKER
         lda sfx1notedata,x
         beq exitmusictracker ; exit if this isn't a pitched instrument
         ldy #0
         sty inttemp2
         clc
         adc (inttemp5),y
         asl ; x2
         tay
         lda tiatrackeroctavenotes,y
         sta AUDC0,x
         iny
         lda tiatrackeroctavenotes,y
         sta AUDF0,x
         ldy #1
         jmp sfxvolumeentrypt
exitmusictracker
         lda inttemp1
     endif ; MUSICTRACKER

     clc
     adc sfx1poffset,x ; take into account any pitch modification
     sta AUDF0,x
     iny
     lda (inttemp5),y
     sta AUDC0,x
     sta inttemp2
     iny
sfxvolumeentrypt
     ifconst TIAVOLUME
         lda tiavolume
         asl
         asl
         asl
         asl
         sta fourbitfadevalueint
     endif ; TIAVOLUME
     lda (inttemp5),y
     ifconst TIAVOLUME
         jsr fourbitfadeint
     endif ; TIAVOLUME
     sta AUDV0,x
     cmp #$10
     bcs sfxsoundloop ; AUDV0>$0F means the sound is looped while priority is active

     ora inttemp2
     ora inttemp1 ; check if F|C|V=0
     beq zerosfx ; if so, we're at the end of the sound.

advancesfxpointer
     ; advance the pointer to the next sound chunk
     iny
     sty inttemp3
     clc
     lda sfx1pointlo,x
     adc inttemp3
     sta sfx1pointlo,x
     lda sfx1pointhi,x
     adc #0
     sta sfx1pointhi,x
     jmp servicesfxchannelsloop

sfxsoundloop
     pha
     lda sfx1priority,x
     bne sfxsoundloop_carryon
     pla ; fix the stack before we go
     jmp advancesfxpointer
sfxsoundloop_carryon
     pla
     and #$F0
     lsr
     lsr
     lsr
     lsr
     
zerosfx
     sta sfx1pointlo,x
     sta sfx1pointhi,x
     sta sfx1priority,x
     jmp servicesfxchannelsloop


schedulesfx
     ; called with sfxinstrumentlo=<data sfxinstrumenthi=>data sfxpitchoffset=pitch-offset sfxnoteindex=note index
     ldy #0
     ifconst pokeysupport
         lda sfxinstrumenthi
         beq scheduletiasfx   ; drums have undefined instrument
         lda (sfxinstrumentlo),y
         cmp #$20 ; POKEY?
         bne scheduletiasfx
         jmp schedulepokeysfx
     endif
scheduletiasfx
     ;cmp #$10 ; TIA?
     ;beq continuescheduletiasfx
     ; rts ; unhandled!!! 
continuescheduletiasfx
     ifnconst TIASFXMONO
         lda sfx1pointhi
         beq schedulesfx1 ;if channel 1 is idle, use it
         lda sfx2pointhi
         beq schedulesfx2 ;if channel 2 is idle, use it
         ; Both channels are scheduled. 
         lda sfxinstrumenthi
         beq skipscheduledrums
         ldy #1
         lda (sfxinstrumentlo),y
         bne interruptsfx
skipscheduledrums
         rts ; the new sound has 0 priority and both channels are busy. Skip playing it.
interruptsfx
         ;Compare which active sound has a lower priority. We'll interrupt the lower one.
         lda sfx1priority
         cmp sfx2priority
         bcs schedulesfx2
     endif ; !TIASFXMONO

schedulesfx1
     ldx #0 ; channel 1
     ifnconst TIASFXMONO
         beq skipschedulesfx2
schedulesfx2
         ldx #1 ; channel 2
skipschedulesfx2
     endif ; !TIASFXMONO

     ifconst MUSICTRACKER
         lda sfxnoteindex
         bpl skipdrumkitoverride
         and #$7F ; subtract 128
         sec
         sbc #4 ; drums start at 132, i.e. octave 10
         asl
         tay
         lda tiadrumkitdefinition,y
         sta sfxinstrumentlo
         iny
         lda tiadrumkitdefinition,y
         sta sfxinstrumenthi
         lda #0
         sta sfxnoteindex ; and tell the driver it's a non-pitched instrument
skipdrumkitoverride
     endif ; MUSICTRACKER
     ldy #1 ; get priority and sound-resolution (in frames)
     lda (sfxinstrumentlo),y
     sta sfx1priority,x
     iny
     lda (sfxinstrumentlo),y
     sta sfx1frames,x
     lda sfxinstrumentlo
     clc
     adc #3
     sta sfx1pointlo,x
     lda sfxinstrumenthi
     adc #0
     sta sfx1pointhi,x
     lda sfxpitchoffset
     sta sfx1poffset,x
     lda #0
     sta sfx1tick,x
     lda sfxnoteindex
     sta sfx1notedata,x
     rts

plotsprite
     ifnconst NODRAWWAIT
         ifconst DOUBLEBUFFER
             lda doublebufferstate
             bne skipplotspritewait
         endif ; DOUBLEBUFFER
         ifconst DEBUGWAITCOLOR
             lda #$41
             sta BACKGRND
         endif
plotspritewait
         lda visibleover
         bne plotspritewait
skipplotspritewait
         ifconst DEBUGWAITCOLOR
             lda #$0
             sta BACKGRND
         endif
     endif

     ;arguments: 
     ; temp1=lo graphicdata 
     ; temp2=hi graphicdata 
     ; temp3=palette | width byte
     ; temp4=x
     ; temp5=y
     ; temp6=mode
     lda temp5 ;Y position
     lsr ; 2 - Divide by 8 or 16
     lsr ; 2
     lsr ; 2
     if WZONEHEIGHT = 16
         lsr ; 2
     endif

     tax

     ifnconst NOLIMITCHECKING

         ; the next block allows for vertical masking, and ensures we don't overwrite non-DL memory

         cmp #WZONECOUNT

         bcc continueplotsprite1 ; the sprite is fully on-screen, so carry on...
         ; otherwise, check to see if the bottom half is in zone 0...

         if WZONEHEIGHT = 16
             cmp #15
         else
             cmp #31
         endif

         bne exitplotsprite1
         ldx #0
         jmp continueplotsprite2
exitplotsprite1
         rts

continueplotsprite1
     endif

     ifconst VSCROLL
         ldy Xx3,x
         lda DLLMEM+11,y
     else  ; !VSCROLL
         lda DLPOINTL,x ;Get pointer to DL that this sprite starts in
     endif ; !VSCROLL
     ifconst DOUBLEBUFFER
         clc
         adc doublebufferdloffset
     endif ; DOUBLEBUFFER
     sta dlpnt
     ifconst VSCROLL
         lda DLLMEM+10,y
     else  ; !VSCROLL
         lda DLPOINTH,x
     endif ; !VSCROLL
     ifconst DOUBLEBUFFER
         adc #0
     endif ; DOUBLEBUFFER
     sta dlpnt+1

     ;Create DL entry for upper part of sprite

     ldy dlend,x ;Get the index to the end of this DL

     ifconst CHECKOVERWRITE
         cpy #DLLASTOBJ
         beq checkcontinueplotsprite2
continueplotsprite1a
     endif

     lda temp1 ; graphic data, lo byte
     sta (dlpnt),y ;Low byte of data address

     ifnconst ATOMICSPRITEUPDATE
         iny
         lda temp6
         sta (dlpnt),y
     else
         iny
         sty temp8
     endif

     iny

     lda temp5 ;Y position
     and #(WZONEHEIGHT - 1)
     cmp #1 ; clear carry if our sprite is just in this zone
     ora temp2 ; graphic data, hi byte
     sta (dlpnt),y

     iny
     lda temp3 ;palette|width
     sta (dlpnt),y

     iny
     lda temp4 ;Horizontal position
     sta (dlpnt),y

     iny
     sty dlend,x

     ifconst ALWAYSTERMINATE
         iny
         lda #0
         sta (dlpnt),y
     endif

     ifconst ATOMICSPRITEUPDATE
         ldy temp8
         lda temp6
         sta (dlpnt),y
     endif

checkcontinueplotsprite2

     bcc doneSPDL ;branch if the sprite was fully in the last zone

     ;Create DL entry for lower part of sprite

     inx ;Next region

     ifnconst NOLIMITCHECKING
         cpx #WZONECOUNT

         bcc continueplotsprite2 ; the second half of the sprite is fully on-screen, so carry on...
         rts
continueplotsprite2
     endif

     ifconst VSCROLL
         ldy Xx3,x
         lda DLLMEM+11,y
     else  ; !VSCROLL
         lda DLPOINTL,x ;Get pointer to next DL
     endif ; !VSCROLL
     ifconst DOUBLEBUFFER
         clc
         adc doublebufferdloffset
     endif ; DOUBLEBUFFER
     sta dlpnt
     ifconst VSCROLL
         lda DLLMEM+10,y
     else  ; !VSCROLL
         lda DLPOINTH,x
     endif ; !VSCROLL
     ifconst DOUBLEBUFFER
         adc #0
     endif ; DOUBLEBUFFER
     sta dlpnt+1
     ldy dlend,x ;Get the index to the end of this DL

     ifconst CHECKOVERWRITE
         cpy #DLLASTOBJ
         bne continueplotsprite2a
         rts
continueplotsprite2a
     endif

     lda temp1 ; graphic data, lo byte
     sta (dlpnt),y

     ifnconst ATOMICSPRITEUPDATE
         iny
         lda temp6
         sta (dlpnt),y
     else
         iny
         sty temp8
     endif

     iny

     lda temp5 ;Y position
     anc #(WZONEHEIGHT - 1) ; undocumented. A=A&IMM, then move bit 7 into carry
     ora temp2 ; graphic data, hi byte
     sbc #(WZONEHEIGHT-1) ; start at the DMA hole. -1 because carry is clear
     sta (dlpnt),y

     iny

     lda temp3 ;palette|width
     sta (dlpnt),y

     iny

     lda temp4 ;Horizontal position
     sta (dlpnt),y

     iny
     sty dlend,x

     ifconst ALWAYSTERMINATE
         iny
         lda #0
         sta (dlpnt),y
     endif

     ifconst ATOMICSPRITEUPDATE
         ldy temp8
         lda temp6
         sta (dlpnt),y
     endif

doneSPDL
     rts

     ifconst VSCROLL
     ; x3 table for fast DLL parsing
Xx3
         .byte  0,3,6,9,12,15,18,21,24,27
         .byte 30,33,36,39,42,45,48,51,54,57
         .byte 60,63,66,69,72,75,78,81,84,87
maskscrollsprite
         .byte $00,%11000000,($D0+WZONEHEIGHT),0,160  ; 5*2 + 32*3 = 106 cycles
         .byte $00,1,($D0+WZONEHEIGHT),160            ; 4*2 + 31*3 = 101 cycles 
         .byte $00,1,($D0+WZONEHEIGHT),160            ; 4*2 + 31*3 = 101 cycles 
         .byte $00,1,($D0+WZONEHEIGHT),160            ; 4*2 + 31*3 = 101 cycles 
         .byte $00,%01000000,($D0+WZONEHEIGHT),16,160 ; 5*2 + 16*3 =  58 cycles
	                                         ; MAX  ============ 467 cycles
	                                         ; MIN  ============  59 cycles
maskscrollspriteend
     endif ; VSCROLL

lockzonex
     ifconst ZONELOCKS
         ldy dlend,x
         cpy #DLLASTOBJ
         beq lockzonexreturn ; the zone is either stuffed or locked. abort!
         lda DLPOINTL,x
         ifconst DOUBLEBUFFER
             clc
             adc doublebufferdloffset
         endif ; DOUBLEBUFFER
         sta dlpnt
         lda DLPOINTH,x
         ifconst DOUBLEBUFFER
             adc #0
         endif ; DOUBLEBUFFER
         sta dlpnt+1
         iny
         lda #0
         sta (dlpnt),y
         dey
         tya
         ldy #(DLLASTOBJ-1)
         sta (dlpnt),y
         iny
         sty dlend,x
lockzonexreturn
         rts
     endif ; ZONELOCKS
unlockzonex
     ifconst ZONELOCKS
         ldy dlend,x
         cpy #DLLASTOBJ
         bne unlockzonexreturn ; if the zone isn't stuffed, it's not locked. abort!
         lda DLPOINTL,x
         ifconst DOUBLEBUFFER
             clc
             adc doublebufferdloffset
         endif ; DOUBLEBUFFER
         sta dlpnt
         lda DLPOINTH,x
         ifconst DOUBLEBUFFER
             adc #0
         endif ; DOUBLEBUFFER
         sta dlpnt+1
         dey
         lda (dlpnt),y
         tay
         sty dlend,x
unlockzonexreturn
     endif ; ZONELOCKS
     rts

plotcharloop
     ; ** read from a data indirectly pointed to from temp8,temp9
     ; ** format is: lo_data, hi_data, palette|width, x, y
     ; ** format ends with lo_data | hi_data = 0

     ifconst DOUBLEBUFFER
         lda doublebufferstate
         bne skipplotcharloopwait
     endif ; DOUBLEBUFFER
     ifconst DEBUGWAITCOLOR
         lda #$61
         sta BACKGRND
     endif
plotcharloopwait
     lda visibleover
     bne plotcharloopwait
     ifconst DEBUGWAITCOLOR
         lda #0
         sta BACKGRND
     endif
skipplotcharloopwait
plotcharlooploop
     ldy #0
     lda (temp8),y
     sta temp1
     iny
     lda (temp8),y
     sta temp2
     ora temp1
     bne plotcharloopcontinue
     ;the pointer=0, so return
     rts
plotcharloopcontinue
     iny
     lda (temp8),y
     sta temp3
     iny
     lda (temp8),y
     sta temp4
     iny
     lda (temp8),y
     ;sta temp5 ; not needed with our late entry.
     jsr plotcharactersskipentry
     lda temp8
     clc
     adc #5
     sta temp8
     lda temp9
     adc #0
     sta temp9
     jmp plotcharlooploop

plotcharacters
     ifconst DOUBLEBUFFER
         lda doublebufferstate
         bne skipplotcharacterswait
     endif ; DOUBLEBUFFER
     ifconst DEBUGWAITCOLOR
         lda #$41
         sta BACKGRND
     endif
plotcharacterswait
     lda visibleover
     bne plotcharacterswait
     ifconst DEBUGWAITCOLOR
         sta BACKGRND
     endif
skipplotcharacterswait
     ;arguments: 
     ; temp1=lo charactermap
     ; temp2=hi charactermap
     ; temp3=palette | width byte
     ; temp4=x
     ; temp5=y

     lda temp5 ;Y position

plotcharactersskipentry

     ;ifconst ZONEHEIGHT
     ; if ZONEHEIGHT = 16
     ; and #$0F
     ; endif
     ; if ZONEHEIGHT = 8
     ; and #$1F
     ; endif
     ;else
     ; and #$0F
     ;endif

     tax
     
    ifconst VSCROLL
         ldy Xx3,x
         lda DLLMEM+11,y
     else  ; !VSCROLL
         lda DLPOINTL,x ;Get pointer to DL that the characters are in
     endif ; !VSCROLL
     ifconst DOUBLEBUFFER
         clc
         adc doublebufferdloffset
     endif ; DOUBLEBUFFER
     sta dlpnt
     ifconst VSCROLL
         lda DLLMEM+10,y
     else  ; !VSCROLL
         lda DLPOINTH,x
     endif ; !VSCROLL
     ifconst DOUBLEBUFFER
         adc #0
     endif ; DOUBLEBUFFER
     sta dlpnt+1

     ;Create DL entry for the characters

     ldy dlend,x ;Get the index to the end of this DL

     ifconst CHECKOVERWRITE
         cpy #DLLASTOBJ
         bne continueplotcharacters
         rts
continueplotcharacters
     endif

     lda temp1 ; character map data, lo byte
     sta (dlpnt),y ;(1) store low address

     iny
     lda charactermode 
     sta (dlpnt),y ;(2) store mode

     iny
     lda temp2 ; character map, hi byte
     sta (dlpnt),y ;(3) store high address

     iny
     lda temp3 ;palette|width
     sta (dlpnt),y ;(4) store palette|width

     iny
     lda temp4 ;Horizontal position
     sta (dlpnt),y ;(5) store horizontal position

     iny
     sty dlend,x ; save display list end byte
     rts


     ifconst plotvalueonscreen
plotcharacterslive
         ; a version of plotcharacters that draws live and minimally disrupts the screen...

         ;arguments: 
         ; temp1=lo charactermap
         ; temp2=hi charactermap
         ; temp3=palette | width byte
         ; temp4=x
         ; temp5=y

         lda temp5 ;Y position

         tax

         ifconst VSCROLL
             ldy Xx3,x
             lda DLLMEM+11,y
         else  ; !VSCROLL
             lda DLPOINTL,x ;Get pointer to DL that the characters are in
         endif ; !VSCROLL
         ifconst DOUBLEBUFFER
             clc
             adc doublebufferdloffset
         endif ; DOUBLEBUFFER
         sta dlpnt
         ifconst VSCROLL
             lda DLLMEM+10,y
         else  ; !VSCROLL
             lda DLPOINTH,x
         endif ; !VSCROLL
         ifconst DOUBLEBUFFER
             adc #0
         endif ; DOUBLEBUFFER
         sta dlpnt+1

         ;Create DL entry for the characters

         ldy dlend,x ;Get the index to the end of this DL

         ifconst CHECKOVERWRITE
             cpy #DLLASTOBJ
             bne continueplotcharacterslive
             rts
continueplotcharacterslive
         endif

         lda temp1 ; character map data, lo byte
         sta (dlpnt),y ;(1) store low address

         iny
         ; we don't add the second byte yet, since the charmap could briefly
         ; render without a proper character map address, width, or position.
         lda charactermode 
         sta (dlpnt),y ;(2) store mode

         iny
         lda temp2 ; character map, hi byte
         sta (dlpnt),y ;(3) store high address

         iny
         lda temp3 ;palette|width
         sta (dlpnt),y ;(4) store palette|width

         iny
         lda temp4 ;Horizontal position
         sta (dlpnt),y ;(5) store horizontal position

         iny
         sty dlend,x ; save display list end byte

         rts
     endif ;plotcharacterslive

     ifconst USED_PLOTVALUE
plotvalue
         ; calling 7800basic command:
         ; plotvalue digit_gfx palette variable/data number_of_digits screen_x screen_y
         ; ...displays the variable as BCD digits
         ;
         ; asm sub arguments: 
         ; temp1=lo charactermap
         ; temp2=hi charactermap
         ; temp3=palette | width byte
         ; temp4=x
         ; temp5=y
         ; temp6=number of digits
         ; temp7=lo variable
         ; temp8=hi variable
         ; temp9=character mode

plotdigitcount         = temp6

         ifconst ZONELOCKS
             ldx temp5
             ldy dlend,x
             cpy #DLLASTOBJ
             bne carryonplotvalue
             rts
carryonplotvalue
         endif

         lda #0
         tay
         ldx valbufend

         lda plotdigitcount
         and #1
         beq pvnibble2char
         lda #0
         sta VALBUFFER,x ; just in case we skip this digit
         beq pvnibble2char_skipnibble

pvnibble2char
         ; high nibble...
         lda (temp7),y
         and #$f0 
         lsr
         lsr
         lsr
         ifnconst DOUBLEWIDE ; multiply value by 2 for double-width
             lsr
         endif

         clc
         adc temp1 ; add the offset to character graphics to our value
         sta VALBUFFER,x
         inx
         dec plotdigitcount

pvnibble2char_skipnibble
         ; low nibble...
         lda (temp7),y
         and #$0f 
         ifconst DOUBLEWIDE ; multiply value by 2 for double-width
             asl
         endif
         clc
         adc temp1 ; add the offset to character graphics to our value
         sta VALBUFFER,x 
         inx
         iny

         dec plotdigitcount
         bne pvnibble2char

         ;point to the start of our valuebuffer
         clc
         lda #<VALBUFFER
         adc valbufend
         sta temp1
         lda #>VALBUFFER
         adc #0
         sta temp2

         ;advance valbufend to the end of our value buffer
         stx valbufend

         ifnconst plotvalueonscreen
             jmp plotcharacters
         else
             jmp plotcharacterslive
         endif

     endif ; USED_PLOTVALUE


     ifconst USED_PLOTVALUEEXTRA
plotdigitcount         = temp6
plotvalueextra
         ; calling 7800basic command:
         ; plotvalue digit_gfx palette variable/data number_of_digits screen_x screen_y
         ; ...displays the variable as BCD digits
         ;
         ; asm sub arguments: 
         ; temp1=lo charactermap
         ; temp2=hi charactermap
         ; temp3=palette | width byte
         ; temp4=x
         ; temp5=y
         ; temp6=number of digits
         ; temp7=lo variable
         ; temp8=hi variable

         lda #0
         tay
         ldx valbufend
         ifnconst plotvalueonscreen
             sta VALBUFFER,x
         endif

         lda plotdigitcount
         and #1
         
         bne pvnibble2char_skipnibbleextra

pvnibble2charextra
         ; high nibble...
         lda (temp7),y
         and #$f0 
         lsr
         lsr
         ifnconst DOUBLEWIDE ; multiply value by 2 for double-width
             lsr
         endif
         clc
         adc temp1 ; add the offset to character graphics to our value
         sta VALBUFFER,x
         inx

         ; second half of the digit
         clc
         adc #1
         sta VALBUFFER,x
         inx
         dec plotdigitcount

pvnibble2char_skipnibbleextra
         ; low nibble...
         lda (temp7),y
         and #$0f 
         ifconst DOUBLEWIDE ; multiply value by 2 for double-width
             asl
         endif
         asl

         clc
         adc temp1 ; add the offset to character graphics to our value
         sta VALBUFFER,x 
         inx

         clc
         adc #1
         sta VALBUFFER,x
         inx
         iny

         dec plotdigitcount
         bne pvnibble2charextra

         ;point to the start of our valuebuffer
         clc
         lda #<VALBUFFER
         adc valbufend
         sta temp1
         lda #>VALBUFFER
         adc #0
         sta temp2

         ;advance valbufend to the end of our value buffer
         stx valbufend

         ifnconst plotvalueonscreen
             jmp plotcharacters
         else
             jmp plotcharacterslive
         endif
     endif ; USED_PLOTVALUEEXTRA

boxcollision
     ifconst BOXCOLLISION
         ; the worst case cycle-time for the code below is 43 cycles.
         ; unfortunately, prior to getting here we've burned 44 cycles in argument setup. eep!

         ;__boxx1 = accumulator
         ;__boxy1 = y
__boxw1 = temp3
__boxh1 = temp4

__boxx2 = temp5
__boxy2 = temp6
__boxw2 = temp7
__boxh2 = temp8

DoXCollisionCheck
         ;lda __boxx1 ; skipped. already in the accumulator
         cmp __boxx2 ;3
         bcs X1isbiggerthanX2 ;2/3
X2isbiggerthanX1
         ; carry is clear
         adc __boxw1 ;3
         cmp __boxx2 ;3
         bcs DoYCollisionCheck ;3/2
         rts ;6 - carry clear, no collision
X1isbiggerthanX2
         clc ;2
         sbc __boxw2 ;3
         cmp __boxx2 ;3
         bcs noboxcollision ;3/2
DoYCollisionCheck
         tya ; 2 ; use to be "lda __boxy1"
         cmp __boxy2 ;3
         bcs Y1isbiggerthanY2 ;3/2
Y2isbiggerthanY1
         ; carry is clear
         adc __boxh1 ;3
         cmp __boxy2 ;3
         rts ;6 
Y1isbiggerthanY2
         clc ;2
         sbc __boxh2 ;3
         cmp __boxy2 ;3
         bcs noboxcollision ;3/2
yesboxcollision
         sec ;2
         rts ;6
noboxcollision
         clc ;2
         rts ;6
     endif ; BOXCOLLISION

randomize
     lda rand
     lsr
     rol rand16
     bcc noeor
     eor #$B4
noeor
     sta rand
     eor rand16
     rts

     ; *** bcd conversion routine courtesy Omegamatrix
     ; *** http://atariage.com/forums/blog/563/entry-10832-hex-to-bcd-conversion-0-99/
 ifconst .calledfunction_converttobcd
converttobcd
     ;value to convert is in the accumulator
     sta temp1
     lsr
     adc temp1
     ror
     lsr
     lsr
     adc temp1
     ror
     adc temp1
     ror
     lsr
     and #$3C
     sta temp2
     lsr
     adc temp2
     adc temp1 
     rts ; return the result in the accumulator
 endif ; .calledfunction_converttobcd

 ifconst .calledfunction_mul8
     ; Y and A contain multiplicands, result in A
mul8
     sty temp1
     sta temp2
     lda #0
reptmul8
     lsr temp2
     bcc skipmul8
     clc
     adc temp1
     ;bcs donemul8 might save cycles?
skipmul8
     ;beq donemul8 might save cycles?
     asl temp1
     bne reptmul8
donemul8
     rts
 endif ; .calledfunction_mul8

 ifconst .calledfunction_div8
div8
     ; A=numerator Y=denominator, result in A
     cpy #2
     bcc div8end+1;div by 0 = bad, div by 1=no calc needed, so bail out
     sty temp1
     ldy #$ff
div8loop
     sbc temp1
     iny
     bcs div8loop
div8end
     tya
     ; result in A
     rts
 endif ; .calledfunction_div8

 ifconst .calledfunction_mul16
     ; Y and A contain multiplicands, result in temp2,A=low, temp1=high
mul16
     sty temp1
     sta temp2

     lda #0
     ldx #8
     lsr temp1
mul16_1
     bcc mul16_2
     clc
     adc temp2
mul16_2
     ror
     ror temp1
     dex
     bne mul16_1
     sta temp2
     rts
 endif ; .calledfunction_mul16

 ifconst .calledfunction_div16
     ; div int/int
     ; numerator in A, denom in temp1
     ; returns with quotient in A, remainder in temp1
div16
     sta temp2
     sty temp1
     lda #0
     ldx #8
     asl temp2
div16_1
     rol
     cmp temp1
     bcc div16_2
     sbc temp1
div16_2
     rol temp2
     dex
     bne div16_1
     sta temp1
     lda temp2
     rts
 endif ; .calledfunction_div16

     ifconst bankswitchmode
BS_jsr
         ifconst dumpbankswitch
             sta dumpbankswitch
         endif
         ifconst MCPDEVCART
             ora #$18
             sta $3000
         else
             sta $8000
         endif
         pla
         tax
         pla
         rts

BS_return
         pla ; bankswitch bank
         ifconst dumpbankswitch
             sta dumpbankswitch
         endif
         ifconst BANKRAM
             sta currentbank
             ora currentrambank
         endif
         ifconst MCPDEVCART
             ora #$18
             sta $3000
         else
             sta $8000
         endif
         pla ; bankswitch $0 flag
         rts 
     endif

checkselectswitch
     lda SWCHB ; check the real select switch...
     and #%00000010
checkselectswitchreturn
     rts

checkresetswitch
     lda SWCHB ; check the real reset switch...
     and #%00000001
     rts

     ifconst FINESCROLLENABLED
finescrolldlls
         ldx temp1 ; first DLL index x3
         lda DLLMEM,x
         and #%11110000
         ora finescrolly
         sta DLLMEM,x

         ldx temp2 ; last DLL index x3
         lda DLLMEM,x
         and #%11110000
         ora finescrolly
         eor #(WZONEHEIGHT-1)
         sta DLLMEM,x
         rts
     endif ; FINESCROLLENABLED

     ifconst USED_ADJUSTVISIBLE
adjustvisible
         ; called with temp1=first visible zone *3, temp2=last visible zone *3
         jsr waitforvblankstart ; ensure vblank just started
         ldx visibleDLLstart
findfirstinterrupt
         lda DLLMEM,x
         bmi foundfirstinterrupt
         inx
         inx
         inx
         bne findfirstinterrupt
foundfirstinterrupt
         and #%01111111 ; clear the interrupt bit
         sta DLLMEM,x
         ifconst DOUBLEBUFFER
             sta DLLMEM+DBOFFSET,x
         endif ; DOUBLEBUFFER
         ldx overscanDLLstart
findlastinterrupt
         lda DLLMEM,x
         bmi foundlastinterrupt
         dex
         dex
         dex
         bne findlastinterrupt
foundlastinterrupt
         and #%01111111 ; clear the interrupt bit
         sta DLLMEM,x
         ifconst DOUBLEBUFFER
             sta DLLMEM+DBOFFSET,x
         endif ; DOUBLEBUFFER
         ;now we need to set the new interrupts
         clc
         lda temp1
         adc visibleDLLstart
         tax
         lda DLLMEM,x
         ora #%10000000
         sta DLLMEM,x
         ifconst DOUBLEBUFFER
             sta DLLMEM+DBOFFSET,x
         endif ; DOUBLEBUFFER
         clc
         lda temp2
         adc visibleDLLstart
         tax
         lda DLLMEM,x
         ora #%10000000
         sta DLLMEM,x
         ifconst DOUBLEBUFFER
             sta DLLMEM+DBOFFSET,x
         endif ; DOUBLEBUFFER
         jsr vblankresync
         rts
     endif ; USED_ADJUSTVISIBLE

vblankresync
     jsr waitforvblankstart ; ensure vblank just started
     lda #0
     sta visibleover
     lda #3
     sta interruptindex
     rts

createallgamedlls
     ldy #(DLLLUTEND-DLLLUT)
createallgamedllsloop
     dey
     lda DLLLUT,y
     sta DLLMEM,y
  ifconst DOUBLEBUFFER
     sta DLLMEM+DBOFFSET,y
  endif ; DOUBLEBUFFER
     cpy #0
     bne createallgamedllsloop

  ifconst DOUBLEBUFFER
     ldy #(DLLLUTNONVISSTART-DLLLUTVISSTART)
fixdoublebuffer
     dey
     lda DLLMEM+DBOFFSET+DLLLUTVISSTART-DLLLUT,y
     clc
     adc #DOUBLEBUFFEROFFSET
     sta DLLMEM+DBOFFSET+DLLLUTVISSTART-DLLLUT,y
     dey
     lda DLLMEM+DBOFFSET+DLLLUTVISSTART-DLLLUT,y
     adc #0
     sta DLLMEM+DBOFFSET+DLLLUTVISSTART-DLLLUT,y
     dey
     bne fixdoublebuffer
 endif

  ifconst BANKSET_DL_IN_CARTRAM 
     ; N.B. banksets doesn't in-fact allow DL in cart-ram, so this conditional
     ; is always skipped. This is here in case some day the limitation is
     ; worked around, but it's untested. 

     ; With bankset cart ram, we added $8000 to the DL address so plot 
     ; functions would hit the cart-ram write-address. We need to subtract $80
     ; so Maria will read from the cart-ram read-address.
     ldy #(DLLLUTNONVISSTART-DLLLUTVISSTART)
fixbanksetaddresses
     dey
     dey
     lda DLLMEM+DLLLUTVISSTART-DLLLUT,y
     and #%01111111
     sta DLLMEM+DLLLUTVISSTART-DLLLUT,y
  ifconst DOUBLEBUFFER
     lda DLLMEM+DBOFFSET+DLLLUTVISSTART-DLLLUT,y
     and #%01111111
     sta DLLMEM+DBOFFSET+DLLLUTVISSTART-DLLLUT,y
  endif ; DOUBLEBUFFER
     dey
     bne fixbanksetaddresses
  endif ; BANKSET_DL_IN_CARTRAM

     lda paldetected
     beq skippaladjust
     lda #($0F|(WZONEHEIGHT*4)) ; +15 lines
     sta DLLMEM+6
 ifconst DOUBLEBUFFER
     sta DLLMEM+DBOFFSET+6
 endif
  if WSCREENHEIGHT = 192
     lda #($0D|(WZONEHEIGHT*4)) ; +6 lines
  else
     lda #($07|(WZONEHEIGHT*4)) ; +6 lines
  endif ; 
     sta DLLMEM+3
 ifconst DOUBLEBUFFER
     sta DLLMEM+DBOFFSET+3
 endif ; DOUBLEBUFFER

skippaladjust

     ; save the DL markers...
     lda #(DLLLUTVISSTART-DLLLUT)
     sta visibleDLLstart
     lda #(DLLLUTNONVISSTART-DLLLUT)
     sta overscanDLLstart
     rts

     ; N.B. max DLL length is 112 bytes (for double-buffered)

DLLLUT
  if WSCREENHEIGHT = 192
     .byte ($0F|(WZONEHEIGHT*4)),$21,$00 ; 16 blank lines
     .byte ($07|(WZONEHEIGHT*4)),$21,$00 ;  8 blank lines
     .byte ($00|(WZONEHEIGHT*4)),$21,$00 ;  1 blank lines 
                                         ;=25 blank lines
  endif ; WSCREENHEIGHT = 192
  if WSCREENHEIGHT = 208
     .byte ($0E|(WZONEHEIGHT*4)),$21,$00 ; 15 blank lines
     .byte ($00|(WZONEHEIGHT*4)),$21,$00 ;  1 blank lines
     .byte ($00|(WZONEHEIGHT*4)),$21,$00 ;  1 blank lines 
                                         ;=17 blank lines
  endif ; WSCREENHEIGHT = 208
  if WSCREENHEIGHT = 224
     .byte ($06|(WZONEHEIGHT*4)),$21,$00 ;  7 blank lines
     .byte ($00|(WZONEHEIGHT*4)),$21,$00 ;  1 blank lines
     .byte ($00|(WZONEHEIGHT*4)),$21,$00 ;  1 blank lines 
                                         ;= 9 blank lines
  endif ; WSCREENHEIGHT = 224

DLLLUTVISSTART
     .byte ($80|(WZONEHEIGHT*4)|(WZONEHEIGHT-1)),>ZONE0ADDRESS,<ZONE0ADDRESS
     ;       ^--NMI 1: start of visible
     .byte ($00|(WZONEHEIGHT*4)|(WZONEHEIGHT-1)),>ZONE1ADDRESS,<ZONE1ADDRESS
     .byte ($00|(WZONEHEIGHT*4)|(WZONEHEIGHT-1)),>ZONE2ADDRESS,<ZONE2ADDRESS
     .byte ($00|(WZONEHEIGHT*4)|(WZONEHEIGHT-1)),>ZONE3ADDRESS,<ZONE3ADDRESS
     .byte ($00|(WZONEHEIGHT*4)|(WZONEHEIGHT-1)),>ZONE4ADDRESS,<ZONE4ADDRESS
     .byte ($00|(WZONEHEIGHT*4)|(WZONEHEIGHT-1)),>ZONE5ADDRESS,<ZONE5ADDRESS
     .byte ($00|(WZONEHEIGHT*4)|(WZONEHEIGHT-1)),>ZONE6ADDRESS,<ZONE6ADDRESS
     .byte ($00|(WZONEHEIGHT*4)|(WZONEHEIGHT-1)),>ZONE7ADDRESS,<ZONE7ADDRESS
     .byte ($00|(WZONEHEIGHT*4)|(WZONEHEIGHT-1)),>ZONE8ADDRESS,<ZONE8ADDRESS
     .byte ($00|(WZONEHEIGHT*4)|(WZONEHEIGHT-1)),>ZONE9ADDRESS,<ZONE9ADDRESS
     .byte ($00|(WZONEHEIGHT*4)|(WZONEHEIGHT-1)),>ZONE10ADDRESS,<ZONE10ADDRESS
     .byte ($00|(WZONEHEIGHT*4)|(WZONEHEIGHT-1)),>ZONE11ADDRESS,<ZONE11ADDRESS
  ifconst ZONE12ADDRESS
     .byte ($00|(WZONEHEIGHT*4)|(WZONEHEIGHT-1)),>ZONE12ADDRESS,<ZONE12ADDRESS
  endif
  ifconst ZONE13ADDRESS
     .byte ($00|(WZONEHEIGHT*4)|(WZONEHEIGHT-1)),>ZONE13ADDRESS,<ZONE13ADDRESS
  endif
  ifconst ZONE14ADDRESS
     .byte ($00|(WZONEHEIGHT*4)|(WZONEHEIGHT-1)),>ZONE14ADDRESS,<ZONE14ADDRESS
  endif
  ifconst ZONE15ADDRESS
     .byte ($00|(WZONEHEIGHT*4)|(WZONEHEIGHT-1)),>ZONE15ADDRESS,<ZONE15ADDRESS
  endif
  ifconst ZONE16ADDRESS
     .byte ($00|(WZONEHEIGHT*4)|(WZONEHEIGHT-1)),>ZONE16ADDRESS,<ZONE16ADDRESS
  endif
  ifconst ZONE17ADDRESS
     .byte ($00|(WZONEHEIGHT*4)|(WZONEHEIGHT-1)),>ZONE17ADDRESS,<ZONE17ADDRESS
  endif
  ifconst ZONE18ADDRESS
     .byte ($00|(WZONEHEIGHT*4)|(WZONEHEIGHT-1)),>ZONE18ADDRESS,<ZONE18ADDRESS
  endif
  ifconst ZONE19ADDRESS
     .byte ($00|(WZONEHEIGHT*4)|(WZONEHEIGHT-1)),>ZONE19ADDRESS,<ZONE19ADDRESS
  endif
  ifconst ZONE20ADDRESS
     .byte ($00|(WZONEHEIGHT*4)|(WZONEHEIGHT-1)),>ZONE20ADDRESS,<ZONE20ADDRESS
  endif
  ifconst ZONE21ADDRESS
     .byte ($00|(WZONEHEIGHT*4)|(WZONEHEIGHT-1)),>ZONE21ADDRESS,<ZONE21ADDRESS
  endif
  ifconst ZONE22ADDRESS
     .byte ($00|(WZONEHEIGHT*4)|(WZONEHEIGHT-1)),>ZONE22ADDRESS,<ZONE22ADDRESS
  endif
  ifconst ZONE23ADDRESS
     .byte ($00|(WZONEHEIGHT*4)|(WZONEHEIGHT-1)),>ZONE23ADDRESS,<ZONE23ADDRESS
  endif
  ifconst ZONE24ADDRESS
     .byte ($00|(WZONEHEIGHT*4)|(WZONEHEIGHT-1)),>ZONE24ADDRESS,<ZONE24ADDRESS
  endif
  ifconst ZONE25ADDRESS
     .byte ($00|(WZONEHEIGHT*4)|(WZONEHEIGHT-1)),>ZONE25ADDRESS,<ZONE25ADDRESS
  endif
  ifconst ZONE26ADDRESS
     .byte ($00|(WZONEHEIGHT*4)|(WZONEHEIGHT-1)),>ZONE26ADDRESS,<ZONE26ADDRESS
  endif
  ifconst ZONE27ADDRESS
     .byte ($00|(WZONEHEIGHT*4)|(WZONEHEIGHT-1)),>ZONE27ADDRESS,<ZONE27ADDRESS
  endif
DLLLUTNONVISSTART
     .byte ($83|(WZONEHEIGHT*4)),$21,$00 ;  4 blank lines
     ;       ^--NMI 2: start of non-visible
     .byte ($8F|(WZONEHEIGHT*4)),$21,$00 ; 16 blank lines
     ;       ^--NMI 3: start of overscan
     .byte ($0F|(WZONEHEIGHT*4)),$21,$00 ; 16 blank lines
     .byte ($0F|(WZONEHEIGHT*4)),$21,$00 ; 16 blank lines
     .byte ($0F|(WZONEHEIGHT*4)),$21,$00 ; 16 blank lines
DLLLUTEND
  ;echo "DLL size: ",[(DLLLUTEND-DLLLUT)]d,"bytes"
  ;echo "DLL code size: ",[(DLLLUTEND-createallgamedlls)]d,"bytes"

waitforvblankstart
vblankendwait
     BIT MSTAT
     bmi vblankendwait
vblankstartwait
     BIT MSTAT
     bpl vblankstartwait
     rts

     ifconst DOUBLEBUFFER
flipdisplaybufferreturn
         rts
flipdisplaybuffer
         ifconst interrupthold
             lda #$FF
             sta interrupthold
         endif
         lda doublebufferstate
         beq flipdisplaybufferreturn ; exit if we're not in double-buffer

         jsr terminatedisplaybuffer ; terminate the working buffer before we flip

         ; ensure we don't flip mid-display. otherwise the displayed DL will be the one the game is working on.

flipdisplaybufferwait1
         lda visibleover
         beq flipdisplaybufferwait1

flipdisplaybufferwait
         lda visibleover
         bne flipdisplaybufferwait

quickbufferflip
         lda doublebufferstate
         lsr ; /2, so we'll see 0 or 1, rather than 1 or 3
         tax

         lda doublebufferminimumframetarget
         beq skipminimumframecode
         lda doublebufferminimumframeindex
         bne flipdisplaybufferwait1
         lda doublebufferminimumframetarget
         sta doublebufferminimumframeindex
skipminimumframecode

         lda DLLMEMLutHi,x
         sta DPPH
         lda DLLMEMLutLo,x
         sta DPPL

         lda NewPageflipstate,x
         sta doublebufferstate
         lda NewPageflipoffset,x
         sta doublebufferdloffset

         ifnconst BANKSET_DL_IN_CARTRAM
             lda doublebufferbufferdirty
             beq flipdisplaybufferreturn

             ; The doublebuffer buffer is dirty, so the game code must have issued a savescreen recently.
             ; To make savescreen work with the new working buffer, we need to copy over the saved objects
             ; from the displayed buffer to the working buffer...

             lda doublebufferdloffset
             eor #DOUBLEBUFFEROFFSET
             sta temp6 ; make temp6 the anti-doublebufferdloffset variable
             
             ldx #(WZONECOUNT-1)
copybufferzoneloop

             lda DLPOINTL,x
             clc
             adc doublebufferdloffset
             sta temp1
             lda DLPOINTH,x
             adc #0
             sta temp2

             lda DLPOINTL,x
             clc
             adc temp6
             sta temp3
             lda DLPOINTH,x
             adc #0
             sta temp4

             lda dlendsave,x
             tay
copybuffercharsloop
             lda (temp3),y
             sta (temp1),y
             dey
             bpl copybuffercharsloop
             dex
             bpl copybufferzoneloop
             lda #0
             sta doublebufferbufferdirty
         endif ; ! BANKSET_DL_IN_CARTRAM
         rts

doublebufferoff
         lda #1
         sta doublebufferstate
         jsr flipdisplaybuffer
         lda #0
         sta doublebufferstate
         sta doublebufferdloffset
         rts

DLLMEMLutLo
         .byte <DLLMEM,<(DLLMEM+DBOFFSET)
DLLMEMLutHi
         .byte >DLLMEM,>(DLLMEM+DBOFFSET)
NewPageflipstate
         .byte 3,1
NewPageflipoffset
         .byte DOUBLEBUFFEROFFSET,0

     endif ; DOUBLEBUFFER

     ifconst MOUSESUPPORT

rotationalcompare
         ; old = 00 01 10 11
         .byte $00, $01, $ff, $00 ; new=00
         .byte $ff, $00, $00, $01 ; new=01
         .byte $01, $00, $00, $ff ; new=10
         .byte $00, $ff, $01, $00 ; new=11

         ; 0000YyXx st mouse

         ; 0000xyXY amiga mouse

         ifconst MOUSEXONLY
amigatoataribits             ; swap bits 1 and 4...
             .byte %0000, %0000, %0010, %0010
             .byte %0000, %0000, %0010, %0010
             .byte %0001, %0001, %0011, %0011
             .byte %0001, %0001, %0011, %0011

             ; null change bits
             .byte %0000, %0001, %0010, %0011
             .byte %0000, %0001, %0010, %0011
             .byte %0000, %0001, %0010, %0011
             .byte %0000, %0001, %0010, %0011

         else ; !MOUSEXONLY

amigatoataribits             ; swap bits 1 and 4...
             .byte %0000, %1000, %0010, %1010
             .byte %0100, %1100, %0110, %1110
             .byte %0001, %1001, %0011, %1011
             .byte %0101, %1101, %0111, %1111
             ; null change bits
             .byte %0000, %0001, %0010, %0011
             .byte %0100, %0101, %0110, %0111
             .byte %1000, %1001, %1010, %1011
             .byte %1100, %1101, %1110, %1111
         endif ; !MOUSEXONLY

     endif ; MOUSESUPPORT

mouse0update
     ifconst MOUSE0SUPPORT

mousetableselect         = inttemp2
mousexdelta         = inttemp3
mouseydelta         = inttemp4
lastSWCHA         = inttemp6

         ; 0000YyXx st mouse
         ; 0000xyXY amiga mouse

         lda #$ff
         sta lastSWCHA

         ldy port0control

         lda #%00010000
         cpy #9 ; AMIGA?
         bne skipamigabitsfix0
         lda #0
skipamigabitsfix0
         sta mousetableselect
         ifconst DRIVINGBOOST
             cpy #6 ; DRIVING?
             bne skipdriving0setup
             ; swap mousex0 and mousey0. mousex seen by the 7800basic program
             ; trails the actual mousex0, so we can smoothly interpolate toward
             ; the actual position. This actual position is stored in mousey0 
             ; after the driver has run.
             ldx mousex0
             lda mousey0
             stx mousey0
             sta mousex0
skipdriving0setup
         endif ; DRIVINGBOOST

         lda #0
         sta mousexdelta
         sta mouseydelta

         ifnconst MOUSETIME
             ifnconst MOUSEXONLY
                 lda #180 ; minimum for x+y
             else
                 lda #100 ; minimum for just x
             endif
         else
             lda #MOUSETIME
         endif
         jsr SETTIM64T ; INTIM is in Y

mouse0updateloop
         lda SWCHA
         asr #%11110000 ; Undocumented. A = A & #IMM, then LSR A.
         cmp lastSWCHA
         beq mouse0loopcondition
         sta lastSWCHA
         lsr
         lsr
         lsr

         ora mousetableselect ; atari/amiga decoding table selection

         ; st mice encode on different bits/joystick-lines than amiga mice...
         ; 0000YyXx st mouse
         ; 0000xyXY amiga mouse
         ; ...so can shuffle the amiga bits to reuse the st driver.
         tay
         lax amigatoataribits,y

         ifnconst MOUSEXONLY
             ; first the Y...
             and #%00001100
             ora mousecodey0
             tay
             lda rotationalcompare,y
             clc 
             adc mouseydelta
             sta mouseydelta
             tya
             lsr
             lsr
             sta mousecodey0
             txa
             ; ...then the X...
             and #%00000011
             tax
         endif ; !MOUSEXONLY

         asl
         asl
         ora mousecodex0
         tay
         lda rotationalcompare,y
         adc mousexdelta ; carry was clear by previous ASL
         sta mousexdelta
         stx mousecodex0
mouse0loopcondition
         lda TIMINT
         bpl mouse0updateloop

         ; *** adapt to selected device resolution. 
         ldx port0control

         ifconst PRECISIONMOUSING
             ldy port0resolution
             bne mouse0halveddone
             cpx #6 ; half-resolution is no good for driving wheels
             beq mouse0halveddone 
             ; resolution=0 is half mouse resolution, necessary for precision 
             ; mousing on a 160x240 screen with a 1000 dpi mouse.

             lda mousexdelta
             cmp #$80
             ror ; do a signed divide by 2.
             clc
             adc mousex0
             sta mousex0
             ifnconst MOUSEXONLY
                 lda mouseydelta
                 clc
                 adc mousey0
                 sta mousey0
             endif
             ; at half resolution we just exit after updating x and y
             jmp LLRET0
mouse0halveddone
         endif ; PRECISIONMOUSING

         ifnconst MOUSEXONLY
             asl mouseydelta ; *2 because Y resolution is finer
             ldy port0resolution
             dey
             lda #0 
mousey0resolutionfix
             clc
             adc mouseydelta 
             dey
             bpl mousey0resolutionfix
             clc
             adc mousey0
             sta mousey0
         endif ; MOUSEXONLY

         ldy port0resolution
         dey
         lda #0
mousex0resolutionfix
         clc
         adc mousexdelta 
         dey
         bpl mousex0resolutionfix
         ifnconst DRIVINGBOOST
             clc
             adc mousex0
             sta mousex0
         else
             cpx #6
             beq carryonmouse0boost
             clc
             adc mousex0
             sta mousex0
             jmp LLRET0
carryonmouse0boost
             sta mousexdelta
             clc
             adc mousecodey0
             sta mousecodey0
             clc
             adc mousex0 
             tay ; save the target X
             adc mousey0 ; average in the smoothly-trailing X
             ror 
             sta mousex0 ; mousex0 now has the smoothly trailing X
             sty mousey0 ; and mousey0 has the the target X

             ; check to see if the coordinate wrapped. If so, undo the averaging code.
             ; A has mousex0, the smoothly trailing X
             sbc mousey0 ; less the target X
             bpl skipabsolutedrive0
             eor #$ff
skipabsolutedrive0
             cmp #64 ; just an unreasonably large change
             bcc skipdrivewrapfix0
             sty mousex0 ; if X wrapped, we catch the trailing X up to the target X
skipdrivewrapfix0

             ; get rid of the tweening if the distance travelled was very small
             lda mousexdelta
             cmp port0resolution
             bcs skipbetweenfix0
             lda mousex0
             sta mousey0
skipbetweenfix0

drivingboostreductioncheck0
             ; The below code amounts to mousecodey0=mousecodey0-(mousecodey0/8)
             ; +ve mousecodey0 is converted to -ve to do the calculation, and then
             ; negated again because truncation during BCD math results in 
             ; differing magnitudes, depending if the value is +ve or -ve.
driving0fix
             lax mousecodey0
             cmp #$80
             bcs driving0skipnegate1
             eor #$FF
             adc #1 
             sta mousecodey0
driving0skipnegate1
             cmp #$80
             ror
             cmp #$80
             ror
             cmp #$80
             ror
             sta inttemp1
             lda mousecodey0
             sec
             sbc inttemp1
             cpx #$80
             bcs driving0skipnegate2
             eor #$FF
             adc #1 
driving0skipnegate2
             sta mousecodey0
drivingboostdone0
         endif ; DRIVINGBOOST

         jmp LLRET0

     endif ; MOUSE0SUPPORT

mouse1update
     ifconst MOUSE1SUPPORT

mousetableselect         = inttemp2
mousexdelta         = inttemp3
mouseydelta         = inttemp4
lastSWCHA         = inttemp6

         ; 0000YyXx st mouse
         ; 0000xyXY amiga mouse

         lda #$ff
         sta lastSWCHA

         ldy port1control

         lda #%00010000
         cpy #9 ; AMIGA?
         bne skipamigabitsfix1
         lda #0
skipamigabitsfix1
         sta mousetableselect
         ifconst DRIVINGBOOST
             cpy #6 ; DRIVING?
             bne skipdriving1setup
             ; swap mousex1 and mousey1. mousex seen by the 7800basic program
             ; trails the actual mousex1, so we can smoothly interpolate toward
             ; the actual position. This actual position is stored in mousey1 
             ; after the driver has run.
             ldx mousex1
             lda mousey1
             stx mousey1
             sta mousex1
skipdriving1setup
         endif ; DRIVINGBOOST

         lda #0
         sta mousexdelta
         sta mouseydelta

         ifnconst MOUSETIME
             ifnconst MOUSEXONLY
                 lda #180 ; minimum for x+y
             else
                 lda #100 ; minimum for just x
             endif
         else
             lda #MOUSETIME
         endif
         jsr SETTIM64T ; INTIM is in Y

mouse1updateloop
         lda SWCHA
         and #%00001111 
         cmp lastSWCHA
         beq mouse1loopcondition
         sta lastSWCHA

         ora mousetableselect ; atari/amiga decoding table selection

         ; st mice encode on different bits/joystick-lines than amiga mice...
         ; 0000YyXx st mouse
         ; 0000xyXY amiga mouse
         ; ...so can shuffle the amiga bits to reuse the st driver.
         tay
         lax amigatoataribits,y

         ifnconst MOUSEXONLY
             ; first the Y...
             and #%00001100
             ora mousecodey1
             tay
             lda rotationalcompare,y
             clc 
             adc mouseydelta
             sta mouseydelta
             tya
             lsr
             lsr
             sta mousecodey1
             txa
             ; ...then the X...
             and #%00000011
             tax
         endif ; !MOUSEXONLY

         asl
         asl
         ora mousecodex1
         tay
         lda rotationalcompare,y
         adc mousexdelta ; carry was clear by previous ASL
         sta mousexdelta
         stx mousecodex1
mouse1loopcondition
         lda TIMINT
         bpl mouse1updateloop

         ; *** adapt to selected device resolution. 
         ldx port1control

         ifconst PRECISIONMOUSING
             ldy port1resolution
             bne mouse1halveddone
             cpx #6 ; half-resolution is no good for driving wheels
             beq mouse1halveddone 
             ; resolution=0 is half mouse resolution, necessary for precision 
             ; mousing on a 160x240 screen with a 1000 dpi mouse.

             lda mousexdelta
             cmp #$80
             ror ; do a signed divide by 2.
             clc
             adc mousex1
             sta mousex1
             ifnconst MOUSEXONLY
                 lda mouseydelta
                 clc
                 adc mousey1
                 sta mousey1
             endif
             ; at half resolution we just exit after updating x and y
             jmp LLRET1
mouse1halveddone
         endif ; PRECISIONMOUSING

         ifnconst MOUSEXONLY
             asl mouseydelta ; *2 because Y resolution is finer
             ldy port1resolution
             dey
             lda #0 
mousey1resolutionfix
             clc
             adc mouseydelta 
             dey
             bpl mousey1resolutionfix
             clc
             adc mousey1
             sta mousey1
         endif ; MOUSEXONLY

         ldy port1resolution
         dey
         lda #0
mousex1resolutionfix
         clc
         adc mousexdelta 
         dey
         bpl mousex1resolutionfix
         ifnconst DRIVINGBOOST
             clc
             adc mousex1
             sta mousex1
         else
             cpx #6
             beq carryonmouse1boost
             clc
             adc mousex1
             sta mousex1
             jmp LLRET1
carryonmouse1boost
             sta mousexdelta
             clc
             adc mousecodey1
             sta mousecodey1
             clc
             adc mousex1
             tay ; save the target X
             adc mousey1 ; average in the smoothly-trailing X
             ror 
             sta mousex1 ; mousex0 now has the smoothly trailing X
             sty mousey1 ; and mousey0 has the the target X

             ; check to see if the coordinate wrapped. If so, undo the averaging code.
             ; A has mousex1, the smoothly trailing X
             sbc mousey1 ; less the target X
             bpl skipabsolutedrive1
             eor #$ff
skipabsolutedrive1
             cmp #64 ; just an unreasonably large change
             bcc skipdrivewrapfix1
             sty mousex1 ; if X wrapped, we catch the trailing X up to the target X
skipdrivewrapfix1

             ; get rid of the tweening if the distance travelled was very small
             lda mousexdelta
             cmp port1resolution
             bcs skipbetweenfix1
             lda mousex1
             sta mousey1
skipbetweenfix1

drivingboostreductioncheck1
             ; The below code amounts to mousecodey0=mousecodey0-(mousecodey0/8)
             ; +ve mousecodey0 is converted to -ve to do the calculation, and then
             ; negated again because truncation during BCD math results in 
             ; differing magnitudes, depending if the value is +ve or -ve.
driving1fix
             lax mousecodey1
             cmp #$80
             bcs driving0skipnegate1
             eor #$FF
             adc #1 
             sta mousecodey1
driving0skipnegate1
             cmp #$80
             ror
             cmp #$80
             ror
             cmp #$80
             ror
             sta inttemp1
             lda mousecodey1
             sec
             sbc inttemp1
             cpx #$80
             bcs driving1skipnegate2
             eor #$FF
             adc #1 
driving1skipnegate2
             sta mousecodey1
drivingboostdone1
         endif ; DRIVINGBOOST

         jmp LLRET1

     endif ; MOUSE1SUPPORT


trakball0update
     ifconst TRAKBALL0SUPPORT
         ifnconst TRAKTIME
             ifnconst TRAKXONLY
                 lda #180 ; minimum for x+y
             else; !TRAKXONLY
                 lda #100 ; minimum for just x
             endif; !TRAKXONLY
         else ; !TRAKTIME
             lda #TRAKTIME
         endif ; !TRAKTIME
         jsr SETTIM64T ; INTIM is in Y
         ldx #0
         ifnconst TRAKXONLY
             ldy #0
         endif ; TRAKXONLY
trakball0updateloop
         lda SWCHA
         and #%00110000
         cmp trakballcodex0
         sta trakballcodex0
         beq trakball0movementXdone
         and #%00010000
         beq trakball0negativeX
trakball0positiveX
         ;(2 from beq)
         inx ; 2
         jmp trakball0movementXdone ; 3
trakball0negativeX
         ;(3 from beq)
         dex ; 2
         nop ; 2
trakball0movementXdone

         ifnconst TRAKXONLY
             lda SWCHA
             and #%11000000
             cmp trakballcodey0
             sta trakballcodey0
             beq trakball0movementYdone
             and #%01000000
             beq trakball0negativeY
trakball0positiveY
             ;(2 from beq)
             iny ; 2
             jmp trakball0movementYdone ; 3
trakball0negativeY
             ;(3 from beq)
             dey ; 2
             nop ; 2
trakball0movementYdone
         endif ; !TRAKXONLY

         lda TIMINT
         bpl trakball0updateloop
         lda #0
         cpx #0
         beq trakball0skipXadjust
         clc
trakball0Xloop
         adc port0resolution
         dex
         bne trakball0Xloop
         clc
         adc trakballx0
         sta trakballx0
trakball0skipXadjust
         ifnconst TRAKXONLY
             lda #0
             cpy #0
             beq trakball0skipYadjust
             clc
trakball0yloop
             adc port0resolution
             dey
             bne trakball0yloop
             clc
             adc trakbally0
             sta trakbally0
trakball0skipYadjust
         endif ; !TRAKXONLY

         jmp LLRET0
     endif



trakball1update
     ifconst TRAKBALL1SUPPORT
         ifnconst TRAKTIME
             ifnconst TRAKXONLY
                 lda #180 ; minimum for x+y
             else; !TRAKXONLY
                 lda #100 ; minimum for just x
             endif; !TRAKXONLY
         else ; !TRAKTIME
             lda #TRAKTIME
         endif ; !TRAKTIME
         jsr SETTIM64T ; INTIM is in Y
         ldx #0
         ifnconst TRAKXONLY
             ldy #0
         endif ; TRAKXONLY
trakball1updateloop
         lda SWCHA
         and #%00000011
         cmp trakballcodex1
         sta trakballcodex1
         beq trakball1movementXdone
         and #%00000001
         beq trakball1negativeX
trakball1positiveX
         ;(2 from beq)
         inx ; 2
         jmp trakball1movementXdone ; 3
trakball1negativeX
         ;(3 from beq)
         dex ; 2
         nop ; 2
trakball1movementXdone

         ifnconst TRAKXONLY
             lda SWCHA
             and #%00001100
             cmp trakballcodey1
             sta trakballcodey1
             beq trakball1movementYdone
             and #%00000100
             beq trakball1negativeY
trakball1positiveY
             ;(2 from beq)
             iny ; 2
             jmp trakball1movementYdone ; 3
trakball1negativeY
             ;(3 from beq)
             dey ; 2
             nop ; 2
trakball1movementYdone
         endif ; !TRAKXONLY

         lda TIMINT
         bpl trakball1updateloop
         lda #0
         cpx #0
         beq trakball1skipXadjust
         clc
trakball1Xloop
         adc port1resolution
         dex
         bne trakball1Xloop
         clc
         adc trakballx1
         sta trakballx1
trakball1skipXadjust
         ifnconst TRAKXONLY
             lda #0
             cpy #0
             beq trakball1skipYadjust
             clc
trakball1yloop
             adc port1resolution
             dey
             bne trakball1yloop
             clc
             adc trakbally1
             sta trakbally1
trakball1skipYadjust
         endif ; !TRAKXONLY

         jmp LLRET1
     endif


paddleport0update
     ifconst PADDLE0SUPPORT
         lda #6
         sta VBLANK ; start charging the paddle caps
         lda #0 ; use PADDLE timing
         jsr SETTIM64T ; INTIM is in Y

paddleport0updateloop
         lda INPT0
         bmi skippaddle0setposition
         sty paddleposition0
skippaddle0setposition         
         ifconst TWOPADDLESUPPORT
             lda INPT1
             bmi skippaddle1setposition
             sty paddleposition1
skippaddle1setposition             
         endif
         ldy INTIM
         cpy #TIMEOFFSET
         bcs paddleport0updateloop

         lda #%10000110
         sta VBLANK ; dump paddles to ground... this may not be great for genesis controllers
         sec
         lda paddleposition0
         sbc #TIMEOFFSET
         ifconst PADDLESCALEX2
             asl
         endif

         ifnconst PADDLESMOOTHINGOFF
             clc
             adc paddleprevious0
             ror
             sta paddleprevious0
         endif

         sta paddleposition0

         ifconst TWOPADDLESUPPORT
             sec
             lda paddleposition1
             sbc #TIMEOFFSET
             ifconst PADDLESCALEX2
                 asl
             endif

             ifnconst PADDLESMOOTHINGOFF
                 clc
                 adc paddleprevious1
                 ror
                 sta paddleprevious1
             endif
             sta paddleposition1
         endif ; TWOPADDLESUPPORT

         jmp LLRET0
     endif

paddleport1update
     ifconst PADDLE1SUPPORT
         lda #6
         sta VBLANK ; start charging the paddle caps

         lda #0 ; use PADDLE timing
         jsr SETTIM64T ; INTIM is in Y

paddleport1updateloop
         lda INPT2
         bmi skippaddle2setposition
         sty paddleposition2
skippaddle2setposition
         ifconst TWOPADDLESUPPORT
             lda INPT3
             bmi skippaddle3setposition
             sty paddleposition3
skippaddle3setposition
         endif
         ldy INTIM
         cpy #TIMEOFFSET
         bcs paddleport1updateloop

         lda #%10000110
         sta VBLANK ; dump paddles to ground... this may not be great for genesis controllers
         sec
         lda paddleposition2
         sbc #TIMEOFFSET
         ifconst PADDLESCALEX2
             asl
         endif

         ifnconst PADDLESMOOTHINGOFF
             clc
             adc paddleprevious2
             ror
             sta paddleprevious2
         endif

         sta paddleposition2

         ifconst TWOPADDLESUPPORT
             sec
             lda paddleposition3
             sbc #TIMEOFFSET
             ifconst PADDLESCALEX2
                 asl
             endif

             ifnconst PADDLESMOOTHINGOFF
                 clc
                 adc paddleprevious3
                 ror
                 sta paddleprevious3
             endif
             sta paddleposition3
         endif ; TWOPADDLESUPPORT

         jmp LLRET1
     endif


paddlebuttonhandler     ; outside of conditional, for button-handler LUT
     ifconst PADDLESUPPORT
         ; x=0|1 for port, rather than paddle #. 
         ; Only the first paddle button will integrate into "joy0fire" testing. If the
         ; game wants to support 2 paddles, up to the game to instead test the 
         ; joystick right+left directions instead.
         lda SWCHA ; top of nibble is first paddle button
         cpx #0 ; port 0?
         beq skippaddleport2shift
         asl ; shift second port to upper nibble
         asl
         asl
         asl
skippaddleport2shift
         and #%11000000
         eor #%11000000 ; invert
         sta sINPT1,x
         jmp buttonreadloopreturn
     endif ; PADDLESUPPORT

mousebuttonhandler     ; outside of conditional, for button-handler LUT
     ifconst MOUSESUPPORT
         ; stick the mouse buttons in the correct shadow register...
         txa
         asl
         tay ; y=x*2
         lda INPT4,x
         eor #%10000000
         lsr
         sta sINPT1,x

         lda INPT1,y
         and #%10000000
         eor #%10000000
         ora sINPT1,x
         sta sINPT1,x
         jmp buttonreadloopreturn
     endif ; MOUSESUPPORT

     ifconst KEYPADSUPPORT
         ; ** select keypad rows 0 to 3 over 4 frames...
keypadrowselect
         inc keypadcounter
         ldy #0
         lda port0control
         cmp #7
         bne skipport0val
         iny ; y=y+1
skipport0val
         lda port1control
         cmp #7
         bne skipport1val
         iny
         iny ; y=y+2
skipport1val
         cpy #0 
         beq exitkeypadrowselect 
         lda keyrowdirectionmask,y
         sta CTLSWA
         tya
         asl
         asl
         sta inttemp1
         lda keypadcounter
         and #3
         ora inttemp1
         tax
         lda keyrowselectvalue,x
         sta SWCHA
exitkeypadrowselect
         rts

keyrowdirectionmask
         .byte #%00000000 ; 0 : port0=input port1=input
         .byte #%11110000 ; 1 : port0=output port1=input
         .byte #%00001111 ; 2 : port0=input port1=output
         .byte #%11111111 ; 3 : port0=output port1=output

keyrowselectvalue
         .byte #%00000000, #%00000000, #%00000000, #%00000000 ; no row selected, all pins high, always
         .byte #%11100000, #%11010000, #%10110000, #%01110000 ; p0 keypad in
         .byte #%00001110, #%00001101, #%00001011, #%00000111 ; p1 keypad in
         .byte #%11101110, #%11011101, #%10111011, #%01110111 ; p0+p1 keypads in
     endif; KEYPADSUPPORT

     ifconst KEYPADSUPPORT
         ; TODO - split into compile-time KEYPAD0SUPPORT and KEYPAD1SUPPORT
keypadcolumnread
         lda port0control
         cmp #7
         bne skipkeypadcolumnread0
         lda keypadcounter
         and #3
         asl ; x2 because keypad variables are interleaved
         tax
         lda #0
         sta keypadmatrix0a,x
         lda INPT0
         cmp #$80
         rol keypadmatrix0a,x
         lda INPT1
         cmp #$80
         rol keypadmatrix0a,x
         lda INPT4
         cmp #$80
         rol keypadmatrix0a,x
         lda keypadmatrix0a,x
         eor #%00000111
         sta keypadmatrix0a,x
skipkeypadcolumnread0         

         lda port1control
         cmp #7
         bne skipkeypadcolumnread1
         lda keypadcounter
         and #3
         asl ; x2 because keypad variables are interleaved
         tax
         lda #0
         sta keypadmatrix1a,x
         rol keypadmatrix1a,x
         lda INPT2
         cmp #$80
         rol keypadmatrix1a,x
         lda INPT3
         cmp #$80
         rol keypadmatrix1a,x
         lda INPT5
         cmp #$80
         rol keypadmatrix1a,x
         lda keypadmatrix1a,x
         eor #%00000111
         sta keypadmatrix1a,x
skipkeypadcolumnread1
         rts
     endif ; KEYPADSUPPORT
     
setportforinput
     lda CTLSWA
     and SWCHA_DIRMASK,x
     sta CTLSWA
     rts

setonebuttonmode
     lda #6 ; in case we're in unlocked-bios mode
     sta VBLANK ; if we were on paddles, the line is grounded out.
     lda #$14
     sta CTLSWB
     lda SWCHB
     ora thisjoy2buttonbit,x ; disable: write 1 to the 2-button bit
     sta SWCHB
     rts

settwobuttonmode
     lda #6 ; in case we're in unlocked-bios mode
     sta VBLANK ; if we were on paddles, the line is grounded out.
     lda #$14
     sta CTLSWB
     lda SWCHB
     and thisjoy2buttonbit+1,x ; enable: write 0 to the 2-button bit
     sta SWCHB
     rts
     
thisjoy2buttonbit
          ; p0   p1   p0
     .byte $04, $10, $04

     ifconst CHANGEDMAHOLES
removedmaholes
     ldx #0
removedllholesloop
     lda DLLMEM,x
     and #%10001111
     sta DLLMEM,x
   ifconst DOUBLEBUFFER
     sta DLLMEM+DBOFFSET,x
   endif
     inx
     inx
     inx
   ifconst DOUBLEBUFFER
     cpx #DBOFFSET
     bcc removedllholesloop
   else
     bpl removedllholesloop
   endif
     rts

createdmaholes
     ldx #0
createdllholesloop
     lda DLLMEM,x
     ora #(WZONEHEIGHT*4)
     sta DLLMEM,x
   ifconst DOUBLEBUFFER
     sta DLLMEM+DBOFFSET,x
   endif
     inx
     inx
     inx
   ifconst DOUBLEBUFFER
     cpx #DBOFFSET
     bcc createdllholesloop
   else
     bpl createdllholesloop
   endif
     rts
 endif

     ; Provided under the CC0 license. See the included LICENSE.txt for details.

START
start

     ;******** more or less the Atari recommended startup procedure

     sei
     cld

     ifnconst NOTIALOCK
         lda #$07
     else
         lda #$06
     endif
     sta INPTCTRL ;lock 7800 into 7800 mode
     lda #$7F
     sta CTRL ;disable DMA
     lda #$00
     sta OFFSET
     ifnconst NOTIALOCK
         sta INPTCTRL
         sta BACKGRND ; black default, in case a flash cart is using something else
     endif
     ldx #$FF
     txs

     ;************** Clear Memory

     ; ** Clear 1800-27FF, pg0+pg1 memory.
ClearMemPages
     lda #0
     tay ; y=0
     sta $80
     ldx #$18
ClearMemPagesLoop
     stx $81 ; needed for when we step on ZP memory
     sta ($80),y ;Store data
     iny ;Next byte
     bne ClearMemPagesLoop
     inx
     cpx #$28
     bne ClearMemPagesLoop
     sta $81

     ;seed random number with hopefully-random timer value
     lda #1
     ora INTIM
     sta rand

     ; detect the console type...
pndetectvblankstart
     lda MSTAT
     bpl pndetectvblankstart ; if we're not in VBLANK, wait for it to start 
pndetectvblankover
     lda MSTAT
     bmi pndetectvblankover ; then wait for it to be over
     ldy #$00
     ldx #$00
pndetectvblankhappening
     lda MSTAT
     bmi pndetectinvblank ; if VBLANK starts, exit our counting loop 
     sta WSYNC
     sta WSYNC
     inx
     bne pndetectvblankhappening
pndetectinvblank
     cpx #125
     bcc pndetecispal
     ldy #$01
pndetecispal
     sty paldetected

     jsr createallgamedlls

     lda #>DLLMEM
     sta DPPH
     lda #<DLLMEM
     sta DPPL

     ifconst pokeysupport
         ; pokey support is compiled in, so try to detect it...
         jsr detectpokeylocation
     endif

     lda #1 ; default for port 0 and 1 is a regular joystick
     sta port0control
     sta port1control

     ;Setup port A to read mode
     ;lda #$00
     ;sta SWCHA
     ;sta CTLSWA

     ifconst HSSUPPORT
         ifconst bankswitchmode
             ifconst included.hiscore.asm.bank
                 ifconst MCPDEVCART
                     lda #($18 | included.hiscore.asm.bank) 
                     ifconst dumpbankswitch
                         sta dumpbankswitch
                     endif
                     sta $3000
                 else
                     lda #(included.hiscore.asm.bank)
                     ifconst dumpbankswitch
                         sta dumpbankswitch
                     endif
                     sta $8000
                 endif
             endif ; included.hiscore.asm.bank
         endif ; bankswitchmode
         ; try to detect HSC
         jsr detecthsc
         and #1
         sta hsdevice
skipHSCdetect
         ; try to detect AtariVox eeprom
         jsr detectatarivoxeeprom
         and #2
         ora hsdevice
         cmp #3
         bne storeAinhsdevice
         ; For now, we tie break by giving HSC priority over AtariVox.
         ; Later we should check each device's priority byte if set, instead, 
         lda #2 
storeAinhsdevice
         sta hsdevice
         lda #$ff
         sta hsdifficulty
         sta hsgameslot
         sta hsnewscoreline
     endif ; HSSUPPORT

     ifconst AVOXVOICE
         jsr silenceavoxvoice
     endif

 ifconst MULTIBUTTON
 ifnconst HSSUPPORT
min_detectatarivoxeeprom
         jsr i2c_startwrite
         bcc min_no_eeprom_error
         lda #$ff
         jmp min_avoxdetect_done
min_no_eeprom_error
         lda #$30
         jsr i2c_txbyte
         lda #$00
         jsr i2c_txbyte
         jsr i2c_stopwrite
min_avoxdetect_done
         eor #$FF
         sta avoxdetected
         jmp min_detectatarivoxeepromdone
         include "i2c7800.inc"
         I2C_SUBS temp9
min_detectatarivoxeepromdone
 endif ; HSSUPPORT
 endif ; MULTIBUTTON

 ifconst MULTIBUTTON
         lda avoxdetected
         beq skipassignavox
         lda #10
         sta port1control ; designate port 1 as atarivox so multibutton won't disturb it.
skipassignavox
 endif ; MULTIBUTTON

     ifconst RMT
         ifconst RMTVOLUME
             lda #$F0 ; default to full RMT volume
             sta rmtvolume
             ifconst TIAVOLUME
                 sta tiavolume
             endif ; TIAVOLUME
         endif ; RMTVOLUME
     else ; !RMT
         ifconst TIAVOLUME
             lda #$F0 ; default to full TIA volume
             sta tiavolume
         endif ; TIAVOLUME
     endif ; RMT

     ifconst bankswitchmode
         ; we need to switch to the first bank as a default. this needs to
         ; happen before DMA, in case there's a topscreenroutine in bank 0
         ifconst MCPDEVCART
             lda #$18 ; xxx11nnn - switch to bank 0
             ifconst dumpbankswitch
                 sta dumpbankswitch
             endif
             sta $3000
         else
             lda #0
             ifconst dumpbankswitch
                 sta dumpbankswitch
             endif
             sta $8000
         endif
     endif

     ; CTRL 76543210
     ; 7 colorburst kill
     ; 6,5 dma ctrl 2=normal DMA, 3=no DMA
     ; 4 character width 1=2 byte chars, 0=1 byte chars
     ; 3 border control 0=background color border, 1=black border
     ; 2 kangaroo mode 0=transparency, 1=kangaroo
     ; 1,0 read mode 0=160x2/160x4 1=N/A 2=320B/320D 3=320A/320C

     ifconst DOUBLEWIDE
         lda #%01010000 ;Enable DMA, mode=160x2/160x4, 2x character width
     else
         lda #%01000000 ;Enable DMA, mode=160x2/160x4
     endif

     jsr waitforvblankstart ; give the some vblank time to minimally update the display

     sta CTRL
     sta sCTRL

     jsr vblankresync

     ldx #1
     jsr settwobuttonmode
     ldx #0
     jsr settwobuttonmode

     ifnconst .altgamestart
         jmp game
     else
         jmp .altgamestart
     endif

 ; Provided under the CC0 license. See the included LICENSE.txt for details.

 ; A tunable parameter, to claim some memory back from DL usage
MEMSKIP = $00

     ;************** Setup DLL entries

     ; setup some working definitions, to avoid ifnconst mess elsewhere...
     ifnconst SCREENHEIGHT
WSCREENHEIGHT         = 192
     else
WSCREENHEIGHT         = SCREENHEIGHT
     endif

     ifnconst ZONEHEIGHT
WZONEHEIGHT         = 16
     else
WZONEHEIGHT         = ZONEHEIGHT
     endif

     ifnconst ZONECOUNT
         ifconst VSCROLL
WZONECOUNT         = ((WSCREENHEIGHT/WZONEHEIGHT)+1)
         else  ; !VSCROLL
WZONECOUNT         = (WSCREENHEIGHT/WZONEHEIGHT)
         endif ; !VSCROLL
     else
         ifconst VSCROLL
WZONECOUNT         = (ZONECOUNT+1)
         else  ; !VSCROLL
WZONECOUNT         = ZONECOUNT
         endif ; !VSCROLL
     endif

     ; top of the frame, non-visible lines. this is based on NTSC,
     ; but we add in extra NV lines at the end of the display to ensure
     ; our PAL friends can play the game without it crashing.
NVLINES         = ((243-WSCREENHEIGHT)/2)

    ifnconst DLMEMSTART
      ifnconst DOUBLEBUFFER
WDLMEMSTART SET $1880
      else
WDLMEMSTART SET $18E0
      endif ; DOUBLEBUFFER
    else
WDLMEMSTART SET DLMEMSTART
    endif

 if MEMSKIP > 0 
     echo "   ",[WDLMEMSTART],"to",[WDLMEMSTART+MEMSKIP-1],"was freed for game usage with MEMSKIP."
WDLMEMSTART SET (WDLMEMSTART + MEMSKIP)
 endif ; MEMSKIP > 0

    ifnconst DLMEMEND
       ifconst EXTRADLMEMORY
WDLMEMEND = $23FF
       else
WDLMEMEND = $1FFF
       endif
    else
WDLMEMEND = DLMEMEND
    endif


WMEMSIZE SET (WDLMEMEND-WDLMEMSTART+1)

 ifconst VSCROLL
 ifnconst DOUBLEBUFFER
 ; give the last zone extra ram for the dma mask objects...
WMEMSIZE SET (WMEMSIZE-(maskscrollspriteend-maskscrollsprite))
 endif ; DOUBLEBUFFER
 endif ; VSCROLL

      ifnconst DOUBLEBUFFER
DLLASTOBJ = ((((WMEMSIZE/WZONECOUNT)-2)/5)*5) ; -2 to always ensure we have 1x double-byte terminator
      else
DLLASTOBJ = ((((WMEMSIZE/WZONECOUNT)-4)/10)*5) ; -4 to always ensure we have 2x double-byte terminators
      endif

TDOUBLEBUFFEROFFSET = (DLLASTOBJ+2) ; offset between DL buffers. ie. half the real DL
  if TDOUBLEBUFFEROFFSET > 255
DOUBLEBUFFEROFFSET = 255
  else
DOUBLEBUFFEROFFSET = (DLLASTOBJ+2)
  endif

  ifconst EXTRADLMEMORY
SECONDDLHALFSTART SET $2300
  endif

DLPOINTH
DLINDEX SET 0
  REPEAT WZONECOUNT
TMPMEMADDRESS SET (((DLINDEX*WMEMSIZE)/WZONECOUNT)+WDLMEMSTART)
  ifconst EXTRADLMEMORY
     if TMPMEMADDRESS > $1FFF
TMPMEMADDRESS SET (TMPMEMADDRESS + $300)
     else
         if ((((DLINDEX+1)*WMEMSIZE)/WZONECOUNT)+WDLMEMSTART) > $1FFF
TMPMEMADDRESS SET (TMPMEMADDRESS + $300)
SECONDDLHALFSTART SET TMPMEMADDRESS
         endif 
     endif ; TMPMEMADDRESS > $1FFF
  endif ; EXTRADLMEMORY
  ;echo " "," ZONE",[DLINDEX]d,"ADDRESS: ",TMPMEMADDRESS
  .byte >TMPMEMADDRESS
DLINDEX SET DLINDEX + 1
  REPEND

  ifconst EXTRADLMEMORY
     echo "   ",[SECONDDLHALFSTART],"to",[$27FF],"was claimed as extra DL memory."
  endif


DLPOINTL
DLINDEX SET 0
  REPEAT WZONECOUNT
TMPMEMADDRESS SET (((DLINDEX*WMEMSIZE)/WZONECOUNT)+WDLMEMSTART)
  ifconst EXTRADLMEMORY
     if TMPMEMADDRESS > $1FFF
TMPMEMADDRESS SET (TMPMEMADDRESS + $300)
     else
         if ((((DLINDEX+1)*WMEMSIZE)/WZONECOUNT)+WDLMEMSTART) > $1FFF
TMPMEMADDRESS SET (TMPMEMADDRESS + $300)
         endif 
     endif ; TMPMEMADDRESS > $1FFF
  endif ; EXTRADLMEMORY
  .byte <TMPMEMADDRESS
DLINDEX SET DLINDEX + 1
  REPEND


DLINDEX SET 0
  REPEAT WZONECOUNT
TMPMEMADDRESS SET (((DLINDEX*WMEMSIZE)/WZONECOUNT)+WDLMEMSTART)
  ifconst EXTRADLMEMORY
     if TMPMEMADDRESS > $1FFF
TMPMEMADDRESS SET (TMPMEMADDRESS + $300)
     else
         if ((((DLINDEX+1)*WMEMSIZE)/WZONECOUNT)+WDLMEMSTART) > $1FFF
TMPMEMADDRESS SET (TMPMEMADDRESS + $300)
         endif 
     endif ; TMPMEMADDRESS > $1FFF
  endif ; EXTRADLMEMORY

ZONE,DLINDEX,"ADDRESS" = TMPMEMADDRESS
LASTZONEADDRESS SET TMPMEMADDRESS

DLINDEX SET DLINDEX + 1
  REPEND


  echo "   ",[WDLMEMSTART],"to",[WDLMEMEND],"used as zone memory, allowing",[(DLLASTOBJ/5)]d,"display objects per zone."

DLHEIGHT
  REPEAT WZONECOUNT
  .byte (WZONEHEIGHT-1)
  REPEND

 ; Provided under the CC0 license. See the included LICENSE.txt for details.

 ; a simple guard, than ensures the 7800basic code hasn't
 ; spilled into the encryption area...
 echo "   ",($FF7E-*)d,"bytes left in the 7800basic reserved area."
 if (*>$FF7D)
        echo
        echo  "***************************"
        echo  "*** Abort: ROM Overflow ***"
        echo  "***************************"
	ERR  ; abort the assembly
 endif

 ; throw a compile
 ifconst RMT
  ifnconst pokeysupport
    echo
    echo "************************************************************"
    echo "*** ABORT: RMT Tracker requires 'set pokeysupport $xxxx' ***"
    echo "************************************************************"
    ERR ; abort
  endif ; pokeysupport
  ifnconst pokeyaddress
    echo
    echo "************************************************************"
    echo "*** ABORT: RMT Tracker requires 'set pokeysupport $xxxx' ***"
    echo "************************************************************"
    ERR ; abort
  endif ; pokeyaddress
 endif
    
; Provided under the CC0 license. See the included LICENSE.txt for details.

  ifconst DEV
    ifnconst ZONEHEIGHT
      echo "* the 4k 7800basic area has",[($FF7E - *)]d,"bytes free."
    else
      if ZONEHEIGHT =  8
        echo "* the 4k 7800basic area has",[($FF7E - *)]d,"bytes free."
      else
        echo "* the 4k 7800basic area has",[($FF7E - *)]d,"bytes free."
      endif
    endif
  endif

  if START_OF_ROM = .
      .byte 0
  endif
START_OF_ROM SET 0

  ; FF7E/FF7F contains the 7800basic crc checksum word

  ; FF80 - FFF7 contains the 7800 encryption key 

  ifnconst bankswitchmode 
    ORG $FFF8
  else
    ifconst ROM128K
      ORG $27FF8
      RORG $FFF8
    endif
    ifconst ROM144K
      ORG $27FF8
      RORG $FFF8
    endif
    ifconst ROM256K
      ORG $47FF8
      RORG $FFF8
    endif
    ifconst ROM272K
      ORG $47FF8
      RORG $FFF8
    endif
    ifconst ROM512K
      ORG $87FF8
      RORG $FFF8
    endif
    ifconst ROM528K
      ORG $87FF8
      RORG $FFF8
    endif
  endif


  .byte   $FF	; region verification. $FF=all regions
  .byte   $F7	; high nibble:  encryption check from $N000 to $FF7F. we only hash the last 4k for faster boot.
		; low nibble :  N=7 atari rainbow start, N=3 no atari rainbow

  ;Vectors
  .word NMI
  .word START
  .word IRQ

