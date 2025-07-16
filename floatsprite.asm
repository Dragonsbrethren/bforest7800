 MAC floatsprite
ZONETODO   SET {1}   
ZONEADDR   SET {2}   
OBJECTTODO SET ({3} * 5)

    ldx dlend+ZONETODO

    lda ZONEADDR+OBJECTTODO+0
    sta ZONEADDR,x
    inx

    lda ZONEADDR+OBJECTTODO+1
    sta ZONEADDR,x
    inx

    lda ZONEADDR+OBJECTTODO+2
    ora #(WZONEHEIGHT)
    sta ZONEADDR+OBJECTTODO+2
    eor #(WZONEHEIGHT)
    sta ZONEADDR,x
    inx

    lda ZONEADDR+OBJECTTODO+3
    sta ZONEADDR,x
    inx

    lda ZONEADDR+OBJECTTODO+4
    sta ZONEADDR,x
    inx

    stx dlend+ZONETODO

 ENDM

