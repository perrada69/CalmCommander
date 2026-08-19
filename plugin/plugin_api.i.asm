VIEW_PLUGIN_ADDRESS  equ 49152
VIEW_DATA_ADDRESS    equ 57344
VIEW_PLUGIN_SIZE     equ 4096

VIEWCTX_ABI          equ 0
VIEWCTX_TYPE         equ 1
VIEWCTX_FILENAME     equ 2
VIEWCTX_SIZE_LO      equ 4
VIEWCTX_SIZE_HI      equ 6
VIEWCTX_DATA_PAGE    equ 8
VIEWCTX_DATA_ADDR    equ 9
VIEWCTX_READ_LEN     equ 11
VIEWCTX_PAGE_COUNT   equ 13
VIEWCTX_DATA_PAGES   equ 14
VIEWCTX_SERVICES     equ 16
VIEWCTX_CURPATH      equ 18   ; pointer to active panel path string
VIEWCTX_EXTRACT_FLAG equ 20
VIEWCTX_EXTRACT_OFF  equ 21
VIEWCTX_EXTRACT_CNT  equ 23
VIEWCTX_EXTRACT_NAME equ 25   ; 16 bytes, 0/255 terminated
VIEWCTX_DIRTY        equ 40
VIEWCTX_P3DOS_TYPE   equ 41    ; 1 byte: $FF=no header, else TAP type (0=BASIC,1=NumArr,2=StrArr,3=Code)
VIEWCTX_P3DOS_P1     equ 42    ; 2 bytes: TAP param1 (LINE for BASIC, load address for CODE)
VIEWCTX_P3DOS_P2     equ 44    ; 2 bytes: TAP param2
VIEWCTX_EXTRACT_OFHI equ 46    ; 2 bytes: bits 16-31 of source offset (SERVICE_EXTRACT_SEEK)

SERVICE_PRINT        equ 0
SERVICE_INKEY        equ 2
SERVICE_WINDOW       equ 4
SERVICE_LAYER0       equ 6
SERVICE_INPUT_NOWAIT equ 8
SERVICE_EXTRACT      equ 10   ; host extract helper (HL=name, DE=offset, BC=count)
SERVICE_BEEP         equ 12
SERVICE_EXTRACT_SEEK equ 14   ; extract straight from the source file:
                              ; HL=name, DE=offset bits 0-15, BC=byte count,
                              ; offset bits 16-31 in VIEWCTX_EXTRACT_OFHI.
                              ; Needed when the data lies beyond the 64KB
                              ; the viewer keeps in RAM.
