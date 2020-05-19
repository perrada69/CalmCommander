        DEVICE ZXSPECTRUMNEXT
        OPT reset --zxnext --syntax=abfw              
  
        org $c000-(hlavni_end-hlavni)
hlavni
        ld hl,cc1
        ld de,$6000
        ld bc,cc1len
        ldir
        jp $6000

cc1     incbin "cc1.bin"
cc1len  equ $-cc1
hlavni_end
        org $c000
cc2     incbin "cc2.bin"
cc3
cc2len  equ $-cc2
        SAVEBIN "cc.bin",hlavni,cc3-hlavni
