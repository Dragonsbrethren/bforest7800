plotvaluesw
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

         lda #0
         tay
         ldx valbufend

         lda plotdigitcount
         and #1
         beq pvnibble2charsw
         lda #0
         sta VALBUFFER,x ; just in case we skip this digit
         beq pvnibble2char_skipnibblesw

pvnibble2charsw
         ; high nibble...
         lda (temp7),y
         and #$f0 
         lsr
         lsr
         lsr
         lsr
         bne pvnibble2charsw_offset
         lda #CHAR_SPACE
pvnibble2charsw_offset
         clc
         adc temp1 ; add the offset to character graphics to our value
         sta VALBUFFER,x
         inx
         dec plotdigitcount

pvnibble2char_skipnibblesw
         ; low nibble...
         lda (temp7),y
         and #$0f 
         clc
         adc temp1 ; add the offset to character graphics to our value
         sta VALBUFFER,x 
         inx
         iny

         dec plotdigitcount
         bne pvnibble2charsw

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