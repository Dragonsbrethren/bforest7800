; This is a stripped down, specialized variation of 7800basic's standard
; plotvalue subroutine which is designed to print singlewide characters
; in doublewide mode. Should not be used outside of the menu. It makes
; assumptions that the standard subroutine does not for the sake of space

plotvaluesw
; Just front load these since they never change for menu purposes
;        lda	#<menu_font
;        sta	temp1
;        lda	#>menu_font
;        sta	temp2
;        lda	charactermode
;        sta	temp9
;        lda	#(menu_font_mode | %01100000)
;        sta	charactermode
        lda	#30	; width in two's complement
;        ora	#0	; palette left shifted 5 bits
        sta	temp3


         ; calling 7800basic command:
         ; plotvalue digit_gfx palette variable/data number_of_digits screen_x screen_y
         ; ...displays the variable as BCD digits
         ;
         ; asm sub arguments: 
         ; temp1=lo charactermap        <-- hardcoded
         ; temp2=hi charactermap        <-- unused
         ; temp3=palette | width byte   <-- hardcoded, used by plotcharacters
         ; temp4=x                      <-- loaded before jsr
         ; temp5=y                      <-- loaded before jsr
         ; temp6=number of digits       <-- unused
         ; temp7=lo variable            <-- loaded before jsr
         ; temp8=hi variable            <-- loaded before jsr
         ; temp9=character mode         <-- unused as far as I can tell

         ldy #0
         ldx valbufend
pvnibble2charsw
         ; high nibble...
         lda (temp7),y
         and #$f0 
         lsr
         lsr
         lsr
         lsr
         bne pvnibble2charsw_offset
         lda #CHAR_SPACE        ; omit leading zero
pvnibble2charsw_offset
         clc
         adc #<menu_font ; add the offset to character graphics to our value
         sta VALBUFFER,x
         inx
         dec plotdigitcount

pvnibble2char_skipnibblesw
         ; low nibble...
         lda (temp7),y
         and #$0f 
         clc
         adc #<menu_font ; add the offset to character graphics to our value
         sta VALBUFFER,x 
         inx
         iny

;         dec plotdigitcount
;         bne pvnibble2charsw

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

         jmp plotcharacters