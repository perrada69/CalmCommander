VIEW_PLUGIN_ADDRESS  equ 49152
VIEW_DATA_ADDRESS    equ 57344
VIEW_PLUGIN_SIZE     equ 4096
; The plugin page is 8K. A plugin that asks the host for the big size gets
; all of it; the viewer only does that for plugin types that need it.
VIEW_PLUGIN_BIG_SIZE equ 8192
VIEW_PLUGIN_PAGE     equ 82      ; the 8K page the plugin itself runs in

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
SERVICE_WRITE_OPEN   equ 18   ; HL=name: create the output file and keep
                              ; it open, so the next two can fill it
SERVICE_WRITE_CHUNK  equ 20   ; DE=offset in the data pages, BC=count:
                              ; append that to the open output file
SERVICE_WRITE_CLOSE  equ 22   ; close it. Together these three lift the
                              ; 64KB ceiling the one-shot extract
                              ; services impose on a single file.
SERVICE_READ_AT      equ 16   ; read part of the source file into RAM:
                              ; C=destination 8K page, HL=offset in that
                              ; page (0-8191), DE=byte count, source
                              ; offset in VIEWCTX_EXTRACT_OFF plus
                              ; VIEWCTX_EXTRACT_OFHI. The counterpart of
                              ; SERVICE_EXTRACT_SEEK for data a plugin
                              ; needs to look at rather than copy out.
SERVICE_EXTRACT_SEEK equ 14   ; extract straight from the source file:
                              ; HL=name, DE=offset bits 0-15, BC=byte count,
                              ; offset bits 16-31 in VIEWCTX_EXTRACT_OFHI.
                              ; Needed when the data lies beyond the 64KB
                              ; the viewer keeps in RAM.
