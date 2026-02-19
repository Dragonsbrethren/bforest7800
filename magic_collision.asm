;--------------------------------------
; SUBROUTINE: MAGIC COLLISION
;--------------------------------------
; Iterates thru all 16 objects and
; tests if the magic object collided
; Uses Y for indexing to preserve X
;--------------------------------------
        ldy #MAX_MAP_OBJECTS
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
        bcs .magic_damage

.magic_cc_next
        dey
        bne .magic_cc_loop
        rts

.magic_damage
        lda object_invuln,y
        beq .magic_damage_object
        tya
        tax
        dec object_invuln,x
        txa
        tay
        ldx index
        rts

.magic_damage_object
        ; back up index before running fae bestiary code
        txa
        pha
        sty index
        jsr .set_enemy_name
        ; restore index
        pla
        tax
        ldy index
        sta index
        ; if object has max defense, bypass hp calc altogether
        lda object_mdef,y
        cmp #255
        beq .magic_damage_done
        ; backup object's hp to check for underflow on high def/low atk values
        lda object_hp,y
        sta temp1
        sta temp_hp
        lda object_damage,y
        sta temp2
        sec
        sbc object_mdef,y
        cmp temp2
        bcs .nov_mdef
        ; if underflow, always do 1 damage
        lda #1
.nov_mdef
        sta hp_change
        jsr .decrease_hp
        ; test HP for underflow
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
        lda #INVULN_FRAMES
        sta object_invuln,y
        rts

.magic_killed_torch
        ; TODO: Rewrite subroutines this calls to preserve x/y
        txa
        pha
        tya
        pha
        sta index       ; overwrite current index for kill torch subroutine
        jsr .kill_torch
        pla
        sta index       ; restore index
        tay
        pla
        tax
        jmp .magic_kill_object