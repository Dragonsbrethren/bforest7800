;--------------------------------------
; SUBROUTINE: MAGIC COLLISION
;--------------------------------------
; Iterates thru all 16 objects and
; tests if the magic object collided
; Uses Y for indexing to preserve X
;--------------------------------------
        ldy #0
        ldx index       ; TODO: remove when object handler is all asm
.magic_cc_loop
        lda object_type,y
        beq .magic_cc_next
        bmi .magic_cc_next       ; objects > 128 don't take damage
        lda object_Xpos,y
        clc
        adc #8
        sbc object_Xpos,x
        cmp #19
        bcs .magic_cc_done
        lda object_Ypos,y
        adc #8
        sbc object_Ypos,x
        cmp #23
.magic_cc_done
        rol
        eor #1
        ror
        bcc .magic_cc_next
        jmp .magic_damage

.magic_cc_next
        iny
        cpy #15
        bne .magic_cc_loop
        rts

.magic_damage
        lda object_hp,y
        sta temp1
        sta temp_hp
        lda object_damage,y
        sta hp_change
        jsr .decrease_hp
        lda temp_hp
        cmp temp1
        bcs .magic_damage_nov
        lda #0
.magic_damage_nov
        sta object_hp,y
        bne .magic_damage_done
        ; TODO: multi-purpose kill object that preserves index
        lda object_type,y
        cmp #TYPE_TORCH
        beq .magic_killed_torch
.magic_kill_object
        lda #0
        sta object_type,x
        ; change to spawner
        lda object_type,y
        sta object_flags,y
        lda #TYPE_SPAWNER
        sta object_type,y
.magic_damage_done
        rts

.magic_killed_torch
        ; TODO: Rewrite subroutines this calls to preserve x/y
        tya
        pha
        jsr .kill_torch
        pla
        tay
        ldx index
        jmp .magic_kill_object