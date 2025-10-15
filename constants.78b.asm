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

.L00 ;;line 4;;         incbasic objects.78b

.L01 ;;line 2;;  

.L02 ;;line 3;;  const TYPE_TORCH = 1

.L03 ;;line 4;;  const TYPE_WOLF = 2

.L04 ;;line 5;;  const TYPE_WARG = 3

.L05 ;;line 6;;  const TYPE_NIX = 4

.L06 ;;line 7;;  const TYPE_MERMAN = TYPE_NIX

.L07 ;;line 8;;  const TYPE_OCTOPUS = 5

.L08 ;;line 9;;  const TYPE_SLIME = 6

.L09 ;;line 10;;  const TYPE_MSLIME = 7

.L010 ;;line 11;;  const TYPE_RAVEN = 8

.L011 ;;line 12;;  const TYPE_PRIEST = 9

.L012 ;;line 13;;  const TYPE_SKELETON = 10

.L013 ;;line 14;;  const TYPE_RSLIME = 11

.L014 ;;line 15;;  const TYPE_MRSLIME = 12

.L015 ;;line 16;;  const TYPE_GHOST = 13

.L016 ;;line 17;;  const TYPE_RGHOST = 14

.L017 ;;line 18;;  const TYPE_SPAWNER = 128

.L018 ;;line 19;;  const TYPE_SKELSWORD = 129

.L019 ;;line 20;;  const TYPE_PROJECTILE = TYPE_SKELSWORD

.L020 ;;line 21;;  const TYPE_DARKSPARK = 140

.L021 ;;line 22;;  const TYPE_OCTOINK = 141

.L022 ;;line 23;;  const TYPE_FIREBALL = 150

.L023 ;;line 24;;  const TYPE_F_FAIRY = 151

.L024 ;;line 25;;  const TYPE_STATIC = TYPE_BLOOD

.L025 ;;line 26;;  const TYPE_BLOOD = 160

.L026 ;;line 27;;  const TYPE_DOOR_OPEN = 161

.L027 ;;line 28;;  const TYPE_DOOR = 162

.L028 ;;line 29;;  const TYPE_DOOR_LOCK = 163

.L029 ;;line 30;;  const TYPE_COIN = 164

.L030 ;;line 31;;  const TYPE_PRISM = 165

.L031 ;;line 32;;  const TYPE_FOOD = 166

.L032 ;;line 33;;  const TYPE_MAP1 = 170

.L033 ;;line 34;;  const TYPE_MAP2 = 171

.L034 ;;line 35;;  const TYPE_MAP3 = 172

.L035 ;;line 36;;  const TYPE_MAP4 = 173

.L036 ;;line 37;;  const TYPE_MAP5 = 174

.L037 ;;line 38;;  const TYPE_KEY1 = 175

.L038 ;;line 39;;  const TYPE_KEY2 = 176

.L039 ;;line 40;;  const TYPE_KEY3 = 177

.L040 ;;line 41;;  const TYPE_KEY4 = 178

.L041 ;;line 42;;  const TYPE_KEY5 = 179

.L042 ;;line 43;;  const TYPE_SPELL1 = 180

.L043 ;;line 44;;  const TYPE_SPELL2 = 181

.L044 ;;line 45;;  const TYPE_SPELL3 = 182

.L045 ;;line 46;;  const TYPE_SPELL4 = 183

.L046 ;;line 47;;  const TYPE_SPELL5 = 184

.L047 ;;line 48;;  const TYPE_SPELL6 = 185

.L048 ;;line 49;;  const TYPE_SPELL7 = 186

.L049 ;;line 50;;  const TYPE_SPELL8 = 187

.L050 ;;line 51;;  const TYPE_SPELL9 = 188

.L051 ;;line 52;;  const TYPE_SPELL10 = 189

.L052 ;;line 53;;  const TYPE_ITEM1 = 190

.L053 ;;line 54;;  const TYPE_ITEM2 = 191

.L054 ;;line 55;;  const TYPE_ITEM3 = 192

.L055 ;;line 56;;  const TYPE_ITEM4 = 193

.L056 ;;line 57;;  const TYPE_ITEM5 = 194

.L057 ;;line 58;;  const TYPE_ITEM6 = 195

.L058 ;;line 59;;  const TYPE_ITEM7 = 196

.L059 ;;line 60;;  const TYPE_ITEM8 = 197

.L060 ;;line 61;;  const TYPE_ITEM9 = 198

.L061 ;;line 62;;  const TYPE_ITEM10 = 199

.
 ;;line 63;; 

.L062 ;;line 64;;  const TYPE_POTION = TYPE_ITEM1

.L063 ;;line 65;;  const TYPE_HIPOTION = TYPE_ITEM2

.L064 ;;line 66;;  const TYPE_ETHER = TYPE_ITEM3

.L065 ;;line 67;;  const TYPE_HIETHER = TYPE_ITEM4

.L066 ;;line 68;;  const TYPE_ELIXIR = TYPE_ITEM5

.L067 ;;line 69;;  const TYPE_ANTIDOTE = TYPE_ITEM6

.L068 ;;line 70;;  const TYPE_EYEDROP = TYPE_ITEM7

.L069 ;;line 71;;  const TYPE_UNCURSE = TYPE_ITEM8

.L070 ;;line 72;;  const TYPE_SOFT = TYPE_ITEM9

.L071 ;;line 73;;  const TYPE_LIFEAPPLE = TYPE_ITEM10

.L072 ;;line 74;;  const TYPE_RELIC1 = 200

.L073 ;;line 75;;  const TYPE_RELIC2 = 201

.L074 ;;line 76;;  const TYPE_RELIC3 = 202

.L075 ;;line 77;;  const TYPE_RELIC4 = 203

.L076 ;;line 78;;  const TYPE_RELIC5 = 204

.L077 ;;line 79;;  const TYPE_RELIC6 = 205

.L078 ;;line 80;;  const TYPE_RELIC7 = 206

.L079 ;;line 81;;  const TYPE_RELIC8 = 207

.L080 ;;line 82;;  const TYPE_RELIC9 = 208

.L081 ;;line 83;;  const TYPE_RELIC10 = 209

.
 ;;line 84;; 

.L082 ;;line 85;;  const TYPE_FAIRYCARD = TYPE_RELIC1

.L083 ;;line 86;;  const TYPE_CATCARD = TYPE_RELIC2

.L084 ;;line 87;;  const TYPE_GHOSTCARD = TYPE_RELIC3

.L085 ;;line 88;;  const TYPE_OWLCARD = TYPE_RELIC4

.L086 ;;line 89;;  const TYPE_BESTIARY = TYPE_RELIC5

.L087 ;;line 90;;  const TYPE_NIXTAIL = TYPE_RELIC6

.L088 ;;line 91;;  const TYPE_WATERFALL = TYPE_RELIC7

.L089 ;;line 92;;  const TYPE_SWORD1 = 210

.L090 ;;line 93;;  const TYPE_SWORD2 = 211

.L091 ;;line 94;;  const TYPE_SWORD3 = 212

.L092 ;;line 95;;  const TYPE_SWORD4 = 213

.L093 ;;line 96;;  const TYPE_SWORD5 = 214

.L094 ;;line 97;;  const TYPE_SWORD6 = 215

.L095 ;;line 98;;  const TYPE_SWORD7 = 216

.L096 ;;line 99;;  const TYPE_SWORD8 = 217

.L097 ;;line 100;;  const TYPE_SWORD9 = 218

.L098 ;;line 101;;  const TYPE_SWORD10 = 219

.
 ;;line 102;; 

.L099 ;;line 103;;  const TYPE_RAPIER = TYPE_SWORD1

.L0100 ;;line 104;;  const TYPE_BONE_SCIM = TYPE_SWORD2

.L0101 ;;line 105;;  const TYPE_SHIELD1 = 220

.L0102 ;;line 106;;  const TYPE_SHIELD2 = 221

.L0103 ;;line 107;;  const TYPE_SHIELD3 = 222

.L0104 ;;line 108;;  const TYPE_SHIELD4 = 223

.L0105 ;;line 109;;  const TYPE_SHIELD5 = 224

.L0106 ;;line 110;;  const TYPE_SHIELD6 = 225

.L0107 ;;line 111;;  const TYPE_SHIELD7 = 226

.L0108 ;;line 112;;  const TYPE_SHIELD8 = 227

.L0109 ;;line 113;;  const TYPE_SHIELD9 = 228

.L0110 ;;line 114;;  const TYPE_SHIELD10 = 229

.L0111 ;;line 115;;  const TYPE_ARMOR1 = 230

.L0112 ;;line 116;;  const TYPE_ARMOR2 = 231

.L0113 ;;line 117;;  const TYPE_ARMOR3 = 232

.L0114 ;;line 118;;  const TYPE_ARMOR4 = 233

.L0115 ;;line 119;;  const TYPE_ARMOR5 = 234

.L0116 ;;line 120;;  const TYPE_ARMOR6 = 235

.L0117 ;;line 121;;  const TYPE_ARMOR7 = 236

.L0118 ;;line 122;;  const TYPE_ARMOR8 = 237

.L0119 ;;line 123;;  const TYPE_ARMOR9 = 238

.L0120 ;;line 124;;  const TYPE_ARMOR10 = 239

.L0121 ;;line 125;;  const TYPE_CHARM1 = 240

.L0122 ;;line 126;;  const TYPE_CHARM2 = 241

.L0123 ;;line 127;;  const TYPE_CHARM3 = 242

.L0124 ;;line 128;;  const TYPE_CHARM4 = 243

.L0125 ;;line 129;;  const TYPE_CHARM5 = 244

.L0126 ;;line 130;;  const TYPE_CHARM6 = 245

.L0127 ;;line 131;;  const TYPE_CHARM7 = 246

.L0128 ;;line 132;;  const TYPE_CHARM8 = 247

.L0129 ;;line 133;;  const TYPE_CHARM9 = 248

.L0130 ;;line 134;;  const TYPE_CHARM10 = 249

.
 ;;line 135;; 

.
 ;;line 136;; 

.L0131 ;;line 137;;  const RARE_DROP_RATE = 254

.L0132 ;;line 138;;  const COMMON_DROP_RATE = 192

.
 ;;line 139;; 

.
 ;;line 140;; 

.L0133 ;;line 141;;  const WOLF_SPEED_HI = 1

.L0134 ;;line 142;;  const WOLF_SPEED_LO = 0

.L0135 ;;line 143;;  const WOLF_HP = 2

.L0136 ;;line 144;;  const WOLF_VEL_CAP_HI = 1

.L0137 ;;line 145;;  const WOLF_VEL_CAP_LO = 0

.L0138 ;;line 146;;  const WOLF_FRICTION_HI = 1

.L0139 ;;line 147;;  const WOLF_FRICTION_LO = 0

.L0140 ;;line 148;;  const WOLF_DAMAGE = 1

.L0141 ;;line 149;;  const WOLF_DEF = 0

.L0142 ;;line 150;;  const WOLF_MDEF = 2

.L0143 ;;line 151;;  const WOLF_DROP_RATE = 64

.L0144 ;;line 152;;  const WOLF_CHASE_DIST = 32

.L0145 ;;line 153;;  const WOLF_CHASE_DIST_X = 48

.L0146 ;;line 154;;  const WOLF_CHASE_DIST_Y = 64

.L0147 ;;line 155;;  const WOLF_CHASE_OFFSET_X = WOLF_CHASE_DIST_X  /  2

.L0148 ;;line 156;;  const WOLF_CHASE_OFFSET_Y = WOLF_CHASE_DIST_Y  /  2

.
 ;;line 157;; 

.L0149 ;;line 158;;  const SLIME_DROP_RATE = 128

.L0150 ;;line 159;;  const SLIME_HP = 4

.L0151 ;;line 160;;  const MSLIME_HP = SLIME_HP  /  2

.L0152 ;;line 161;;  const SLIME_VEL_CAP_HI = 1

.L0153 ;;line 162;;  const SLIME_VEL_CAP_LO = 0

.L0154 ;;line 163;;  const SLIME_DAMAGE = 4

.L0155 ;;line 164;;  const SLIME_DEF = 255

.L0156 ;;line 165;;  const SLIME_MDEF = 0

.L0157 ;;line 166;;  const MSLIME_DEF = 0

.L0158 ;;line 167;;  const MSLIME_MDEF = 0

.L0159 ;;line 168;;  const SLIME_FRICTION_HI = 1

.L0160 ;;line 169;;  const SLIME_FRICTION_LO = 0

.L0161 ;;line 170;;  const SLIME_SPEED_HI = 0

.L0162 ;;line 171;;  const SLIME_SPEED_LO = 5

.
 ;;line 172;; 

.L0163 ;;line 173;;  const RAVEN_CHASE_DIST = 50

.L0164 ;;line 174;;  const RAVEN_FLIGHT_TIME = 10

.L0165 ;;line 175;;  const RAVEN_DROP_RATE = 128

.L0166 ;;line 176;;  const RAVEN_HP = 4

.L0167 ;;line 177;;  const RAVEN_VEL_CAP_HI = 2

.L0168 ;;line 178;;  const RAVEN_VEL_CAP_LO = 0

.L0169 ;;line 179;;  const RAVEN_DAMAGE = 4

.L0170 ;;line 180;;  const RAVEN_FRICTION_HI = 0

.L0171 ;;line 181;;  const RAVEN_FRICTION_LO = 0

.L0172 ;;line 182;;  const RAVEN_SPEED_HI = 1

.L0173 ;;line 183;;  const RAVEN_SPEED_LO = 0

.L0174 ;;line 184;;  const RAVEN_DEF = 0

.L0175 ;;line 185;;  const RAVEN_MDEF = 0

.
 ;;line 186;; 

.L0176 ;;line 187;;  const PRIEST_WALK_TIMER = 20

.L0177 ;;line 188;;  const PRIEST_CAST_FRAMES = 20

.
 ;;line 189;; 

.L0178 ;;line 190;;  const DARKSPARK_SPEED_HI = 2

.L0179 ;;line 191;;  const DARKSPARK_SPEED_LO = 0

.L0180 ;;line 192;;  const DARKSPARK_DAMAGE = 6

.
 ;;line 193;; 

.L0181 ;;line 194;;  const SKELSWORD_DAMAGE = 3

.L0182 ;;line 195;;  const RETRACT_FRAMES = 4

.
 ;;line 196;; 

.L0183 ;;line 197;;  const RSLIME_DROP_RATE = 128

.L0184 ;;line 198;;  const RSLIME_HP = 4

.L0185 ;;line 199;;  const MRSLIME_HP = RSLIME_HP  /  2

.L0186 ;;line 200;;  const RSLIME_VEL_CAP_HI = 1

.L0187 ;;line 201;;  const RSLIME_VEL_CAP_LO = 0

.L0188 ;;line 202;;  const RSLIME_DAMAGE = 2

.L0189 ;;line 203;;  const RSLIME_DEF = 255

.L0190 ;;line 204;;  const RSLIME_MDEF = 0

.L0191 ;;line 205;;  const MRSLIME_DEF = 0

.L0192 ;;line 206;;  const MRSLIME_MDEF = 0

.L0193 ;;line 207;;  const RSLIME_FRICTION_HI = 1

.L0194 ;;line 208;;  const RSLIME_FRICTION_LO = 0

.L0195 ;;line 209;;  const RSLIME_SPEED_HI = 0

.L0196 ;;line 210;;  const RSLIME_SPEED_LO = 5

.
 ;;line 211;; 

.
 ;;line 212;; 

.L0197 ;;line 213;;  const F_NIX_EMERGE = 0

.L0198 ;;line 214;;  const F_NIX_ATTACK = 1

.L0199 ;;line 215;;  const F_NIX_TAIL = 2

.L0200 ;;line 216;;  const F_NIX_TAIL2 = 3

.L0201 ;;line 217;;  const F_NIX_SHADOW = 4

.L0202 ;;line 218;;  const F_NIX_BLANK = 5

.
 ;;line 219;; 

.L0203 ;;line 220;;  const NIX_WIDTH = 28

.L0204 ;;line 221;;  const NIX_HEIGHT = 40

.L0205 ;;line 222;;  const NIX_RANGE_X = 53

.L0206 ;;line 223;;  const NIX_RANGE_Y = 74

.L0207 ;;line 224;;  const NIX_OFFSET_X = NIX_RANGE_X  /  2

.L0208 ;;line 225;;  const NIX_OFFSET_Y = NIX_RANGE_Y  /  2

.
 ;;line 226;; 

.L0209 ;;line 227;;  const NIX_ATK_TIME = 20

.L0210 ;;line 228;;  const NIX_ATK2_TIME = 20

.L0211 ;;line 229;;  const NIX_TAIL_TIME = 20

.
 ;;line 230;; 

.L0212 ;;line 231;;  const OCTOPUS_HP = 4

.L0213 ;;line 232;;  const OCTOPUS_POP_TIMER = 60

.L0214 ;;line 233;;  const OCTOPUS_SWIM_TIMER = 20

.L0215 ;;line 234;;  const OCTOPUS_SHOOT_FRAMES = 30

.
 ;;line 235;; 

.L0216 ;;line 236;;  const OCTO_PAL = 2

.L0217 ;;line 237;;  const OCTO_SHADOW_PAL = 4

.
 ;;line 238;; 

.L0218 ;;line 239;;  const F_OCTO_SHADOW = 0

.L0219 ;;line 240;;  const F_OCTO_POP = 1

.L0220 ;;line 241;;  const F_OCTO_POP_2 = 2

.L0221 ;;line 242;;  const F_OCTO_SHOOT_D = 3

.L0222 ;;line 243;;  const F_OCTO_SHOOT_U = 4

.L0223 ;;line 244;;  const F_OCTO_SHOOT_L = 6

.L0224 ;;line 245;;  const F_OCTO_SHOOT_R = 5

.
 ;;line 246;; 

.L0225 ;;line 247;;  const OCTOINK_SPEED_HI = 2

.L0226 ;;line 248;;  const OCTOINK_SPEED_LO = 0

.L0227 ;;line 249;;  const OCTOINK_DAMAGE = 6

.
 ;;line 250;; 

.
 ;;line 251;; 

.
 ;;line 252;; 

.L0228 ;;line 253;;  const FAIRY_SPEED_HI = 1

.L0229 ;;line 254;;  const FAIRY_SPEED_LO = 0

.L0230 ;;line 255;;  const FAIRY_HP = 255

.L0231 ;;line 256;;  const FAIRY_VEL_CAP_HI = 1

.L0232 ;;line 257;;  const FAIRY_VEL_CAP_LO = 128

.L0233 ;;line 258;;  const FAIRY_FRICTION_HI = 0

.L0234 ;;line 259;;  const FAIRY_FRICTION_LO = 128

.L0235 ;;line 260;;  const FAIRY_DAMAGE = 0

.L0236 ;;line 261;;  const FAIRY_DEF = 255

.L0237 ;;line 262;;  const FAIRY_MDEF = 255

.
 ;;line 263;; 

.L0238 ;;line 264;;  const F_FAIRY_WING1 = 0

.L0239 ;;line 265;;  const F_FAIRY_WING2 = 1

.L0240 ;;line 266;;  const F_FAIRY_ITEM1 = 2

.L0241 ;;line 267;;  const F_FAIRY_ITEM2 = 3

.
 ;;line 268;; 

.L0242 ;;line 269;;  const FAIRY_CHASE_X = 40

.L0243 ;;line 270;;  const FAIRY_CHASE_Y = 54

.
 ;;line 271;; 

.L0244 ;;line 272;;  const FIREBALL_SPEED_HI = 2

.L0245 ;;line 273;;  const FIREBALL_SPEED_LO = 0

.L0246 ;;line 274;;  const FIREBALL_DAMAGE = 2

.L0247 ;;line 275;;  const FIREBALL_MP_COST = 8

.L0248 ;;line 276;;  const FIREBALL_MAX_FRAMES = 4

.
 ;;line 277;; 

.
 ;;line 278;; 

.L0249 ;;line 279;;  const torch_ai_lo = # < .torch_tile_block

.L0250 ;;line 280;;  const torch_ai_hi = # > .torch_tile_block

.L0251 ;;line 281;;  const wolf_ai_lo = # < .wolf_ai

.L0252 ;;line 282;;  const wolf_ai_hi = # > .wolf_ai

.L0253 ;;line 283;;  const warg_ai_lo = # < .warg_ai

.L0254 ;;line 284;;  const warg_ai_hi = # > .warg_ai

.L0255 ;;line 285;;  const nix_ai_lo = # < .nix_ai

.L0256 ;;line 286;;  const nix_ai_hi = # > .nix_ai

.L0257 ;;line 287;;  const octopus_ai_lo = # < .octopus_ai

.L0258 ;;line 288;;  const octopus_ai_hi = # > .octopus_ai

.L0259 ;;line 289;;  const slime_ai_lo = # < .slime_ai

.L0260 ;;line 290;;  const slime_ai_hi = # > .slime_ai

.L0261 ;;line 291;;  const mslime_ai_lo = # < .slime_ai

.L0262 ;;line 292;;  const mslime_ai_hi = # > .slime_ai

.L0263 ;;line 293;;  const raven_ai_lo = # < .raven_ai

.L0264 ;;line 294;;  const raven_ai_hi = # > .raven_ai

.L0265 ;;line 295;;  const priest_ai_lo = # < .priest_ai

.L0266 ;;line 296;;  const priest_ai_hi = # > .priest_ai

.L0267 ;;line 297;;  const redslime_ai_lo = # < .redslime_ai

.L0268 ;;line 298;;  const redslime_ai_hi = # > .redslime_ai

.L0269 ;;line 299;;  const mredslime_ai_lo = # < .redslime_ai

.L0270 ;;line 300;;  const mredslime_ai_hi = # > .redslime_ai

.L0271 ;;line 301;;  const ghost_ai_lo = # < .ghost_ai

.L0272 ;;line 302;;  const ghost_ai_hi = # > .ghost_ai

.L0273 ;;line 303;;  const rghost_ai_lo = # < .rghost_ai

.L0274 ;;line 304;;  const rghost_ai_hi = # > .rghost_ai

.
 ;;line 305;; 

.L0275 ;;line 306;;  const fairy_ai_lo = # < .fairy_ai

.L0276 ;;line 307;;  const fairy_ai_hi = # > .fairy_ai

.
 ;;line 308;; 

.L0277 ;;line 309;;  const skelsword_ai_lo = # < .skelsword_ai

.L0278 ;;line 310;;  const skelsword_ai_hi = # > .skelsword_ai

.L0279 ;;line 311;;  const darkspark_ai_lo = # < .fireball_ai

.L0280 ;;line 312;;  const darkspark_ai_hi = # > .fireball_ai

.L0281 ;;line 313;;  const octoink_ai_lo = # < .fireball_ai

.L0282 ;;line 314;;  const octoink_ai_hi = # > .fireball_ai

.L0283 ;;line 315;;  const fireball_ai_lo = # < .fireball_ai

.L0284 ;;line 316;;  const fireball_ai_hi = # > .fireball_ai

.L0285 ;;line 317;;  incbasicend

.
 ;;line 2;; 

.L0286 ;;line 3;;         incbasic palettes.78b

.; ;;line 2;; ; -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  - 

.
 ;;line 3;; 

.
 ;;line 4;; 

.L0287 ;;line 5;;  const PAL_BLACK = $00

.L0288 ;;line 6;;  const PAL_DKPURP = $60

.L0289 ;;line 7;;  const PAL_LTPURP = $76

.L0290 ;;line 8;;  const PAL_WHITE = $0F

.L0291 ;;line 9;;  const PAL_DKBRWN = $10

.L0292 ;;line 10;;  const PAL_LTBRWN = $23

.L0293 ;;line 11;;  const PAL_SKIN = $3B

.L0294 ;;line 12;;  const PAL_DKRED = $30

.L0295 ;;line 13;;  const PAL_LTRED = $45

.L0296 ;;line 14;;  const PAL_GOLD = $19

.L0297 ;;line 15;;  const PAL_DKBLUE = $91

.L0298 ;;line 16;;  const PAL_LTBLUE = $A3

.L0299 ;;line 17;;  const PAL_COPPER = $D5

.
 ;;line 18;; 

.L0300 ;;line 19;;  const HAIR_COLOR = PAL_DKBRWN

.L0301 ;;line 20;;  const CHANT_COLOR = $60

.L0302 ;;line 21;;  const BAR_NORMAL_COLOR = $76

.L0303 ;;line 22;;  const BAR_FLASH_COLOR = $79

.
 ;;line 23;; 

.
 ;;line 24;; 

.
 ;;line 25;; 

.
 ;;line 26;; 

.L0304 ;;line 27;;  const PAL_F_GRND = $11

.L0305 ;;line 28;;  const PAL_F_DKGRN = $C0

.L0306 ;;line 29;;  const PAL_F_LTGRN = $C3

.L0307 ;;line 30;;  const PAL_F_DKGRY = $03

.L0308 ;;line 31;;  const PAL_F_LTGRY = $06

.L0309 ;;line 32;;  const PAL_F_WATER = $A1

.L0310 ;;line 33;;  const PAL_F_LTWTR = $06

.L0311 ;;line 34;;  const PAL_F_DKBRN = $10

.L0312 ;;line 35;;  const PAL_F_LTBRN = $12

.
 ;;line 36;; 

.
 ;;line 37;; 

.
 ;;line 38;; 

.
 ;;line 39;; 

.L0313 ;;line 40;;  const PAL_C_SKY = $90

.L0314 ;;line 41;;  const PAL_C_LIGHTN = $AA

.L0315 ;;line 42;;  const PAL_C_DKWALL = $02

.L0316 ;;line 43;;  const PAL_C_MDWALL = $03

.L0317 ;;line 44;;  const PAL_C_LTWALL = $04

.L0318 ;;line 45;;  const PAL_C_BRWALL = $05

.L0319 ;;line 46;;  const PAL_C_DKCARP = $40

.L0320 ;;line 47;;  const PAL_C_LTCARP = $41

.L0321 ;;line 48;;  const PAL_C_BRCARP = $42

.L0322 ;;line 49;;  const PAL_C_DKTILE = $A0

.L0323 ;;line 50;;  const PAL_C_LTTILE = $A1

.L0324 ;;line 51;;  const PAL_C_BRTILE = $A2

.L0325 ;;line 52;;  const PAL_C_SKELBL = $00

.L0326 ;;line 53;;  const PAL_C_SKELAR = $33

.L0327 ;;line 54;;  const PAL_C_SKELWH = $0F

.L0328 ;;line 55;;  const PAL_C_DKGOLD = $23

.L0329 ;;line 56;;  const PAL_C_LTGOLD = $27

.
 ;;line 57;; 

.
 ;;line 58;; 

.
 ;;line 59;; 

.
 ;;line 60;; 

.L0330 ;;line 61;;  const PAL_WFALL1 = $A1

.L0331 ;;line 62;;  const PAL_WFALL2 = $A3

.L0332 ;;line 63;;  const PAL_WFALL3 = $0F

.
 ;;line 64;; 

.
 ;;line 65;; 

.
 ;;line 66;; 

.
 ;;line 67;; 

.L0333 ;;line 68;;  const PAL_H_GRND = $06

.L0334 ;;line 69;;  const PAL_H_DKGRY = $03

.L0335 ;;line 70;;  const PAL_H_LTGRY = $08

.L0336 ;;line 71;;  const PAL_H_DIRT = $11

.L0337 ;;line 72;;  const PAL_H_VEG = $C0

.L0338 ;;line 73;;  const PAL_H_WATER = $A1

.L0339 ;;line 74;;  incbasicend

.
 ;;line 2;; 

.L0340 ;;line 3;;         incbasic pointers.78b

.L0341 ;;line 2;;  

.L0342 ;;line 3;;  const m_item_inv_lo = # < m_item_inv

.L0343 ;;line 4;;  const m_item_inv_hi = # > m_item_inv

.L0344 ;;line 5;;  const m_item_qty_lo = # < m_item_qty

.L0345 ;;line 6;;  const m_item_qty_hi = # > m_item_qty

.L0346 ;;line 7;;  const m_sword_inv_lo = # < m_sword_inv

.L0347 ;;line 8;;  const m_sword_inv_hi = # > m_sword_inv

.L0348 ;;line 9;;  const m_shield_inv_lo = # < m_shield_inv

.L0349 ;;line 10;;  const m_shield_inv_hi = # > m_shield_inv

.L0350 ;;line 11;;  const m_armor_inv_lo = # < m_armor_inv

.L0351 ;;line 12;;  const m_armor_inv_hi = # > m_armor_inv

.L0352 ;;line 13;;  const m_charm_inv_lo = # < m_charm_inv

.L0353 ;;line 14;;  const m_charm_inv_hi = # > m_charm_inv

.L0354 ;;line 15;;  const m_relic_inv_lo = # < m_relic_inv

.L0355 ;;line 16;;  const m_relic_inv_hi = # > m_relic_inv

.
 ;;line 17;; 

.
 ;;line 18;; 

.L0356 ;;line 19;;  const plot_torch_lo = # < .plot_torch

.L0357 ;;line 20;;  const plot_torch_hi = # > .plot_torch

.L0358 ;;line 21;;  const plot_wolf_lo = # < .plot_wolf

.L0359 ;;line 22;;  const plot_wolf_hi = # > .plot_wolf

.L0360 ;;line 23;;  const plot_priest_lo = # < .plot_priest

.L0361 ;;line 24;;  const plot_priest_hi = # > .plot_priest

.L0362 ;;line 25;;  const plot_skeleton_lo = # < .plot_skeleton

.L0363 ;;line 26;;  const plot_skeleton_hi = # > .plot_skeleton

.L0364 ;;line 27;;  const plot_skelsword_lo = # < .plot_skelsword

.L0365 ;;line 28;;  const plot_skelsword_hi = # > .plot_skelsword

.L0366 ;;line 29;;  incbasicend

.
 ;;line 2;; 

.
 ;;line 3;; 

.
 ;;line 4;; 

.
 ;;line 5;; 

.L0367 ;;line 6;;  const STATE_TITLE = 0

.L0368 ;;line 7;;  const STATE_FIELD = 1

.L0369 ;;line 8;;  const STATE_MENU = 2

.
 ;;line 9;; 

.
 ;;line 10;; 

.
 ;;line 11;; 

.
 ;;line 12;; 

.L0370 ;;line 13;;  const DIR_UP = 1

.L0371 ;;line 14;;  const DIR_DOWN = 2

.L0372 ;;line 15;;  const DIR_LEFT = 3

.L0373 ;;line 16;;  const DIR_RIGHT = 4

.
 ;;line 17;; 

.L0374 ;;line 18;;  const EXIT_NONE = 0

.L0375 ;;line 19;;  const EXIT_NORTH = 1

.L0376 ;;line 20;;  const EXIT_SOUTH = 2

.L0377 ;;line 21;;  const EXIT_EAST = 3

.L0378 ;;line 22;;  const EXIT_WEST = 4

.
 ;;line 23;; 

.L0379 ;;line 24;;  const ANIM_TIME = 16

.L0380 ;;line 25;;  const BUTTON_HOLD_FRAMES = 15

.
 ;;line 26;; 

.
 ;;line 27;; 

.
 ;;line 28;; 

.
 ;;line 29;; 

.L0381 ;;line 30;;  const PLAYER_HEIGHT = 16

.L0382 ;;line 31;;  const PLAYER_WIDTH = 12

.
 ;;line 32;; 

.L0383 ;;line 33;;  const SWORD_FRAMES = 4

.L0384 ;;line 34;;  const SWORD_OFFSET_DOWN = 15

.L0385 ;;line 35;;  const SWORD_OFFSET_UP =  - 16

.L0386 ;;line 36;;  const SWORD_OFFSET_LEFT =  - 12

.L0387 ;;line 37;;  const SWORD_OFFSET_RIGHT = 12

.
 ;;line 38;; 

.L0388 ;;line 39;;  const SHIELD_FRAMES = 4

.L0389 ;;line 40;;  const SHIELD_OFFSET_X_LEFT = 6

.L0390 ;;line 41;;  const SHIELD_OFFSET_X_RIGHT = 7

.L0391 ;;line 42;;  const SHIELD_OFFSET_X_DOWN = 8

.L0392 ;;line 43;;  const SHIELD_OFFSET_X_UP =  - 3

.L0393 ;;line 44;;  const SHIELD_OFFSET_X_LEFT_STAND =  - 2

.L0394 ;;line 45;;  const SHIELD_OFFSET_X_RIGHT_STAND = 7

.L0395 ;;line 46;;  const SHIELD_OFFSET_X_DOWN_STAND = 8

.L0396 ;;line 47;;  const SHIELD_OFFSET_X_UP_STAND = 0

.L0397 ;;line 48;;  const SHIELD_OFFSET_Y = 7

.L0398 ;;line 49;;  const SHIELD_OFFSET_Y_UP = 6

.L0399 ;;line 50;;  const SHIELD_OFFSET_Y_UP_STAND = 6

.
 ;;line 51;; 

.L0400 ;;line 52;;  const STATE_STAND = 0

.L0401 ;;line 53;;  const STATE_MOVING = 1

.L0402 ;;line 54;;  const STATE_ATTACK = 4

.L0403 ;;line 55;;  const STATE_CHANT_START = 5

.L0404 ;;line 56;;  const STATE_CHANT = 6

.L0405 ;;line 57;;  const STATE_KNOCKBACK = 2

.L0406 ;;line 58;;  const STATE_HOP = 3

.L0407 ;;line 59;;  const STATE_DEATH = 7

.
 ;;line 60;; 

.L0408 ;;line 61;;  const FRAME_UP = 2

.L0409 ;;line 62;;  const FRAME_DOWN = 0

.L0410 ;;line 63;;  const FRAME_LEFT = 4

.L0411 ;;line 64;;  const FRAME_RIGHT = 6

.L0412 ;;line 65;;  const FRAME_ATK_D = 8

.L0413 ;;line 66;;  const FRAME_ATK_U = 9

.L0414 ;;line 67;;  const FRAME_ATK_L = 10

.L0415 ;;line 68;;  const FRAME_ATK_R = 11

.L0416 ;;line 69;;  const FRAME_CHANT = 12

.L0417 ;;line 70;;  const FRAME_DEATH = 13

.
 ;;line 71;; 

.L0418 ;;line 72;;  const ATTACK_FRAMES = 8

.L0419 ;;line 73;;  const CHANT_START_TIME = 12

.L0420 ;;line 74;;  const INVULN_FRAMES = 4

.
 ;;line 75;; 

.L0421 ;;line 76;;  const PLAYER_HP_MAX = 48

.L0422 ;;line 77;;  const PLAYER_HP_HALF = PLAYER_HP_MAX  /  2

.L0423 ;;line 78;;  const PLAYER_HP_QUARTER = PLAYER_HP_MAX  /  4

.L0424 ;;line 79;;  const PLAYER_MP_MAX = 48

.L0425 ;;line 80;;  const PLAYER_MP_HALF = PLAYER_MP_MAX  /  2

.L0426 ;;line 81;;  const PLAYER_MP_QUARTER = PLAYER_MP_MAX  /  4

.
 ;;line 82;; 

.
 ;;line 83;; 

.
 ;;line 84;; 

.
 ;;line 85;; 

.L0427 ;;line 86;;  const BITS_POISON = %00000001

.L0428 ;;line 87;;  const BITS_DARK = %00000010

.L0429 ;;line 88;;  const BITS_CURSE = %00000100

.L0430 ;;line 89;;  const BITS_STONE = %00001000

.L0431 ;;line 90;;  const BITS_FURY = %00010000

.L0432 ;;line 91;;  const BITS_REGEN = %00100000

.
 ;;line 92;; 

.L0433 ;;line 93;;  const MASK_POISON = %11111110

.L0434 ;;line 94;;  const MASK_DARK = %11111101

.L0435 ;;line 95;;  const MASK_CURSE = %11111011

.L0436 ;;line 96;;  const MASK_STONE = %11110111

.L0437 ;;line 97;;  const MASK_FURY = %11101111

.L0438 ;;line 98;;  const MASK_REGEN = %11011111

.
 ;;line 99;; 

.L0439 ;;line 100;;  const STATUS_NONE = 0

.L0440 ;;line 101;;  const STATUS_POISON = 1

.L0441 ;;line 102;;  const STATUS_DARK = 2

.L0442 ;;line 103;;  const STATUS_CURSE = 3

.L0443 ;;line 104;;  const STATUS_STONE = 4

.L0444 ;;line 105;;  const STATUS_FURY = 5

.L0445 ;;line 106;;  const STATUS_REGEN = 6

.
 ;;line 107;; 

.L0446 ;;line 108;;  const POISON_DMG = 1

.L0447 ;;line 109;;  const POISON_FRAMES = 120

.
 ;;line 110;; 

.L0448 ;;line 111;;  const FADE_DARKNESS = 0

.L0449 ;;line 112;;  const FADE_NORMAL = 15

.L0450 ;;line 113;;  const WHITE_DARKNESS = $50

.
 ;;line 114;; 

.
 ;;line 115;; 

.
 ;;line 116;; 

.L0451 ;;line 117;;  const TILE_HEIGHT = 16

.L0452 ;;line 118;;  const TILE_WIDTH = 8

.
 ;;line 119;; 

.L0453 ;;line 120;;  const T_TITLE = 0

.L0454 ;;line 121;;  const T_FOREST = 1

.L0455 ;;line 122;;  const T_CASTLE = 2

.L0456 ;;line 123;;  const T_RIVER = 3

.L0457 ;;line 124;;  const T_HILLS = 4

.
 ;;line 125;; 

.L0458 ;;line 126;;  const ROOMS_PER_ROW = 7

.L0459 ;;line 127;;  const MAX_MAP_OBJECTS = 12

.L0460 ;;line 128;;  const MAX_OBJECTS = 15

.
 ;;line 129;; 

.L0461 ;;line 130;;  const LIGHTNING_FRAMES = 8

.L0462 ;;line 131;;  const LIGHTNING_DELAY = 120

.
 ;;line 132;; 

.
 ;;line 133;; 

.L0463 ;;line 134;;  const ROOML_B = 0

.L0464 ;;line 135;;  const ROOMR_B = 1

.L0465 ;;line 136;;  const ROOML_UDL = 2

.L0466 ;;line 137;;  const ROOMR_UDR = 3

.L0467 ;;line 138;;  const ROOML_L = 4

.L0468 ;;line 139;;  const ROOMR_R = 5

.L0469 ;;line 140;;  const ROOML_DL = 6

.L0470 ;;line 141;;  const ROOMR_DR = 7

.L0471 ;;line 142;;  const ROOML_UL = 8

.L0472 ;;line 143;;  const ROOMR_UR = 9

.L0473 ;;line 144;;  const ROOML_UD = 10

.L0474 ;;line 145;;  const ROOMR_UD = 11

.L0475 ;;line 146;;  const ROOML_D = 12

.L0476 ;;line 147;;  const ROOMR_D = 13

.L0477 ;;line 148;;  const ROOML_U = 14

.L0478 ;;line 149;;  const ROOMR_U = 15

.L0479 ;;line 150;;  const ROOML_N = 16

.L0480 ;;line 151;;  const ROOMR_N = 17

.
 ;;line 152;; 

.
 ;;line 153;; 

.
 ;;line 154;; 

.
 ;;line 155;; 

.
 ;;line 156;; 

.L0481 ;;line 157;;  const M_RIVER_START_X = 62

.L0482 ;;line 158;;  const M_RIVER_START_Y = 112

.
 ;;line 159;; 

.L0483 ;;line 160;;  const F_RIVER_START_I = 22

.L0484 ;;line 161;;  const F_RIVER_START_X = 76

.L0485 ;;line 162;;  const F_RIVER_START_Y = $50

.L0486 ;;line 163;;  const F_RIVER_START_D = DIR_DOWN

.
 ;;line 164;; 

.
 ;;line 165;; 

.L0487 ;;line 166;;  const M_RIVER_SAVE1_X = 78

.L0488 ;;line 167;;  const M_RIVER_SAVE1_Y = 96

.
 ;;line 168;; 

.L0489 ;;line 169;;  const F_RIVER_SAVE1_I = 17

.L0490 ;;line 170;;  const F_RIVER_SAVE1_X = $50

.L0491 ;;line 171;;  const F_RIVER_SAVE1_Y = $70

.L0492 ;;line 172;;  const F_RIVER_SAVE1_D = DIR_DOWN

.
 ;;line 173;; 

.
 ;;line 174;; 

.L0493 ;;line 175;;  const M_RIVER_SAVE2_X = 94

.L0494 ;;line 176;;  const M_RIVER_SAVE2_Y = 64

.
 ;;line 177;; 

.L0495 ;;line 178;;  const F_RIVER_SAVE2_I = 5

.L0496 ;;line 179;;  const F_RIVER_SAVE2_X = $72

.L0497 ;;line 180;;  const F_RIVER_SAVE2_Y = $80

.L0498 ;;line 181;;  const F_RIVER_SAVE2_D = DIR_DOWN

.
 ;;line 182;; 

.
 ;;line 183;; 

.
 ;;line 184;; 

.
 ;;line 185;; 

.
 ;;line 186;; 

.L0499 ;;line 187;;  const M_HILLS_START_X = 62

.L0500 ;;line 188;;  const M_HILLS_START_Y = 112

.
 ;;line 189;; 

.L0501 ;;line 190;;  const F_HILLS_START_I = 22

.L0502 ;;line 191;;  const F_HILLS_START_X = $64

.L0503 ;;line 192;;  const F_HILLS_START_Y = $A0

.L0504 ;;line 193;;  const F_HILLS_START_D = DIR_UP

.
 ;;line 194;; 

.
 ;;line 195;; 

.L0505 ;;line 196;;  const M_HILLS_SAVE1_X = 78

.L0506 ;;line 197;;  const M_HILLS_SAVE1_Y = 96

.
 ;;line 198;; 

.L0507 ;;line 199;;  const F_HILLS_SAVE1_I = 17

.L0508 ;;line 200;;  const F_HILLS_SAVE1_X = $50

.L0509 ;;line 201;;  const F_HILLS_SAVE1_Y = $70

.L0510 ;;line 202;;  const F_HILLS_SAVE1_D = DIR_DOWN

.
 ;;line 203;; 

.
 ;;line 204;; 

.L0511 ;;line 205;;  const M_HILLS_SAVE2_X = 94

.L0512 ;;line 206;;  const M_HILLS_SAVE2_Y = 64

.
 ;;line 207;; 

.L0513 ;;line 208;;  const F_HILLS_SAVE2_I = 5

.L0514 ;;line 209;;  const F_HILLS_SAVE2_X = $72

.L0515 ;;line 210;;  const F_HILLS_SAVE2_Y = $80

.L0516 ;;line 211;;  const F_HILLS_SAVE2_D = DIR_DOWN

.
 ;;line 212;; 

.
 ;;line 213;; 

.
 ;;line 214;; 

.
 ;;line 215;; 

.L0517 ;;line 216;;  const MENU_MAIN = 0

.L0518 ;;line 217;;  const MENU_ITEMS = 1

.L0519 ;;line 218;;  const MENU_MAGIC = 2

.L0520 ;;line 219;;  const MENU_EQUIP = 3

.L0521 ;;line 220;;  const MENU_RELICS = 4

.L0522 ;;line 221;;  const MENU_PASSWORD = 5

.L0523 ;;line 222;;  const MENU_GAMEOVER = 6

.L0524 ;;line 223;;  const MENU_MAIN_MAX_OPTIONS = 3

.L0525 ;;line 224;;  const CHAR_SPACE = 62

.
 ;;line 225;; 

.L0526 ;;line 226;;  const MENU_MAIN_ITEMS_X = 12

.L0527 ;;line 227;;  const MENU_MAIN_ITEMS_Y = 192

.L0528 ;;line 228;;  const MENU_MAIN_MAGIC_X = 40

.L0529 ;;line 229;;  const MENU_MAIN_EQUIP_X = 72

.L0530 ;;line 230;;  const MENU_MAIN_RELICS_X = 116

.
 ;;line 231;; 

.
 ;;line 232;; 

.L0531 ;;line 233;;  const MENU_TAP_FRAMES = 4

.
 ;;line 234;; 

.L0532 ;;line 235;;  const ITEMS_HP_X = 80

.L0533 ;;line 236;;  const ITEMS_HP_Y = 32

.L0534 ;;line 237;;  const ITEM_NAME_LENGTH =  ( 8 / 2 )   *  11

.
 ;;line 238;; 

.L0535 ;;line 239;;  const MENU_EQUIP_MAX_OPTIONS = 4

.L0536 ;;line 240;;  const MENU_EQUIP_EQUIPPED_X = 76

.L0537 ;;line 241;;  const MENU_EQUIP_SWORD_Y = 64

.L0538 ;;line 242;;  const MENU_EQUIP_SHIELD_Y = 96

.L0539 ;;line 243;;  const MENU_EQUIP_ARMOR_Y = 128

.L0540 ;;line 244;;  const MENU_EQUIP_CHARM_Y = 160

.L0541 ;;line 245;;  const MENU_EQUIP_INV_X = 12

.L0542 ;;line 246;;  const MENU_EQUIP_INV0_Y = 80

.L0543 ;;line 247;;  const MENU_EQUIP_INV1_Y = 112

.L0544 ;;line 248;;  const MENU_EQUIP_INV2_Y = 142

.L0545 ;;line 249;;  const MENU_EQUIP_INV3_Y = 176

.L0546 ;;line 250;;  const MENU_EQUIP_EXIT_X = 124

.L0547 ;;line 251;;  const MENU_EQUIP_EXIT_Y = 192

.
 ;;line 252;; 

.
 ;;line 253;; 

.L0548 ;;line 254;;  const PW_0 = 19

.L0549 ;;line 255;;  const PW_A = 16

.
 ;;line 256;; 

.
 ;;line 257;; 

.L0550 ;;line 258;;  const HP_CHANGE_FOOD = $10

.L0551 ;;line 259;;  const HP_CHANGE_POTION = $20

.L0552 ;;line 260;;  const HP_CHANGE_HIPOTION = $30

.L0553 ;;line 261;;  const HP_CHANGE_XPOTION = $48

.L0554 ;;line 262;;  const MP_CHANGE_PRISM = $05

.L0555 ;;line 263;;  const MP_CHANGE_ETHER = $10

.L0556 ;;line 264;;  const MP_CHANGE_XETHER = $48

.
 ;;line 265;; 

.
 ;;line 266;; 

.L0557 ;;line 267;;  const FAIRY_ITEM_TIME = 120

.
 ;;line 268;; 

.L0558 ;;line 269;;  const PICKUP_TIME = 120

.L0559 ;;line 270;;  const PICKUP_STRING_LENGTH = 12

.
 ;;line 271;; 

.
 ;;line 272;; 

.
 ;;line 273;; 

.
 ;;line 274;; 

.
 ;;line 275;; 

.L0560 ;;line 276;;  const FONT_SPR_CHARS = 65

.
 ;;line 277;; 

.L0561 ;;line 278;;  const BAR_OFFSET = FONT_SPR_CHARS  *  2

.
 ;;line 279;; 

DMAHOLEEND0 SET .
gameend
DMAHOLEEND0 SET .
 echo " ",[($F000 - gameend)]d , "bytes of ROM space left in the main area."
 if ($F000 - gameend) < 0
SPACEOVERFLOW SET (SPACEOVERFLOW+1)
 endif
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

