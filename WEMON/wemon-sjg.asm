;================================================================================
; WEMON MONITOR REVERSE ENGINEERED SOURCE CODE
;================================================================================
;
; WEMON - "The Definitive Monitor" (C)WATFORD ELECTRONICS
;
; WEMON is a Monitor ROM for Ohio Scientific SuperBoard-II and UK-101
;
; The WEMON binary was disassembled using my CBM-Tansfer then
; commented and enhanced by Steve J. Gray, starting Jun 24, 2019.
; Formatted for ACME assembler.
;
; I have two versions of WEMON:
; The "original"  was sent to me and I submitted it to OSIWEB.ORG.
; The "alternate" was supplied by Mark Spankus.
; There are only a few differences, which are marked in the source.
;
;================================================================================
; Features
;================================================================================
;
; - Full-screen editor similar to Commodore PET and later CBM machines.
; - Enhanced Tape Loading and Saving
; - Enhanced keyboard input - shortcuts for BASIC keywords
; - Cursor control
; - Control codes inside strings
; - Machine code monitor
; - Support for swapping Video modes (24/48 columns)
;
; Power On Menu:
;
; M........ Machine Code Monitor
; C........ Cold Start
; W........ Warm Start
; U........ User Vector jump
; D........ Disk (requires additional ROM firmware)
;
; Control Codes:
;
; CTRL-A...Home Cursor (ESC)
; CTRL-D...Insert Line
; CTRL-E...BASIC Extensions
; CTRL-H...Cursor LEFT (Shift Repeat)
; CTRL-I...Cursor RIGHT (Repeat)
; CTRL-J...Cursor DOWN (Line Feed)
; CTRL-L...Clear Screen (Shift ESC)
; CTRL-N...Delete (RUBOUT)
; CTRL-P...Toggle Print Flag
; CTRL-Q...Toggle Quotes
; CTRL-S...Video Swap (Superboard Series II only) POKE 296,32 or 64
; CTRL-T...Tape On/Off
; CTRL-X...Cursor LEFT (Shift Repeat)
; CTRL-Z...Cursor UP (Shift Line Feed)
;
; Other Keys:
;
; CTRL+LEFT-SHIFT then LETTER... Return BASIC KEYWORD
; CURSORing past TOP or BOTTOM scrolls the screen
; Slow scrolling with SPACEBAR
;
; Machine Code Monitor:
;
; M xx-ssss-eeee .... Memory Search for xx from s=start to e=end
; V ssss-eeee ....... View Memory from s=start to e=end
; B dddd-ssss-eeee .. Block Move block s=start to e=end to d=destination
; S ssss-eeee ....... Save machine code
; L ................. Load to originally saved location
; L ssss ............ Load to s=start
; G ssss ............ Execute at s=start address
; F xx-ssss-eeee .... Fill Block
;
;================================================================================
; CONFIGURE MACHINE AND DISPLAY TYPE
;================================================================================
; To be added later

;================================================================================
; Symbols for ROM and IO space
;================================================================================
; Assume Superboard Memory Map

XROM    = $8000			; EXTENDED ROM SPACE (BASIC EXTENSIONS etc)
PARPORT = $8800			; PARALLEL PORT $8800 (Hardware add-on)
WEDISK  = $9800			; WATFORD ELECTRONICS Disk firmware

BASIC	= $A000			; BASIC ROM
DISK	= $C000			; DISK CONTROLLER (PIA = +$00, ACIA = +$10)
SCREEN	= $D000			; SCREEN RAM
COLOUR  = $D400			; COLOUR RAM ($D400-D7FF - with 630 board)
KEYBD	= $DF00			; KEYBOARD
SCDREG  = $D800			; Screen, Colour, DAC Control Register
ACIA	= $E000			; SERIAL PORT (MC6850 ACIA) FOR C1/Superboard
MONITOR = $F000   		; MONITOR ROM


;================================================================================
; BASIC ROM ROUTINES
;================================================================================
; ROM BASIC provides ACIA I/O for the C2/4, which has the ACIA at
; $FC00.  The C1 must reproduce these in the monitor for the ACIA
; at $F000.

BROMWARM = BASIC + $0274	; UK101 BASIC WARM START
BROMRUB1 = BASIC + $034B	; UK101 BASIC RUBOUT KEY RETURN
BROMRUB2 = BASIC + $0374	; UK101 BASIC RUBOUT KEY RETURN
BROMCC   = BASIC + $0636	; CTRL-C HANDLER
BROMCOLD = BASIC + $1D11	; BASIC COLD START
BROMCRTC = BASIC + $1F2D	; CRT SIMULATOR

BROMA084 = BASIC + $0084	; List of BASIC Keywords
BROMA359 = BASIC + $0359	; ?
BROMA374 = BASIC + $0374	; ?
BROMA636 = BASIC + $0636	; Break Execution?
BROMA866 = BASIC + $0866	; ?
BROMAFC1 = BASIC + $0FC1     	; ?
BROMB96E = BASIC + $196E	; ?
BROMBCF7 = BASIC + $1CF7	; ?

; BROMAOUT = BASIC + $1F15	; OUTPUT CHAR TO ACIA (C2/C4)
; BROMAINI = BASIC + $1F22	; INIT ACIA (C2/C4)

;================================================================================
; Symbols for Zero Page Storage
;================================================================================

INBUF = $13			; Zero Page $13
INBUF00 = $0013			; Zero Page $13 ############### 2 bytes
ZP_14 = $14			; Zero Page $14
ZP_79 = $79			; Zero Page $79
ZP_7A = $7A			; Zero Page $7A
ZP_7B = $7B			; Zero Page $7B
ZP_7C = $7C			; Zero Page $7C
ZP_87 = $87			; Zero Page $87
ZP_88 = $88			; Zero Page $88
ZP_8C = $8C			; Zero Page $8C BASIC ERROR related
ZP_C4 = $C4			; Zero Page $C4
ZP_C5 = $C5			; Zero Page $C5
ZP_D6 = $D6			; Zero Page $D6
ZP_DE = $DE			; Zero Page $DE
ZP_00DE = $00DE			; Zero Page $DE ################ 2 bytes
ZP_DF = $DF			; Zero Page $DF Index to Input buffer????????
MCLOADFLG = $E0			; Zero Page $E0 MC Load Flag
ZP_E1 = $E1			; Zero Page $E1
ZP_E2 = $E2			; Zero Page $E2
ZP_E3 = $E3			; Zero Page $E3
ZP_E4 = $E4			; Zero Page $E4
ZP_E5 = $E5			; Zero Page $E5
ZP_E9 = $E9			; Zero Page $E9
ZP_EA = $EA			; Zero Page $EA
ZP_EC = $EC			; Zero Page $EC
ZP_EF = $EF			; Zero Page $EF
ZP_F3 = $F3			; Zero Page $F3
ZP_F4 = $F4			; Zero Page $F4
SCRNLO = $F6			; Zero Page $F6 pointer to screen LO
SCRNHI = $F7			; Zero Page $F7 pointer to screen HI
ZP_F8 = $F8			; Zero Page $F8
ZP_F9 = $F9			; Zero Page $F9
ZP_FA = $FA			; Zero Page $FA
ZP_FB = $FB			; Zero Page $FB
ZP_FC = $FC			; Zero Page $FC
ZP_FD = $FD			; Zero Page $FD
ZP_FE = $FE			; Zero Page $FE
ZP_FF = $FF			; Zero Page $FF

;================================================================================
; Symbols for Stack Page
;================================================================================

STACK  = $0100			; Stack 


;================================================================================
; Symbols for Monitor ROM Storage
;================================================================================

; $0128 - manual says it's screen width
CURPSL  = $0200			; Cursor Position LO byte
CURCHR  = $0201			; Character under cursor
TEMCHR  = $0202			; Temporary character store
LDFLAG  = $0203			; Load Flag
ERRORFL = $0204			; FF = Run error
SCRSPD  = $0205			; Scroll speed 1
SCRSPD2 = $0206			; Scroll speed 2 (both seem to be used?)
CCFLAG  = $0212			; Control-C Flag
MEM0213 = $0213			; Low Mem $0213
MEM0214 = $0214			; Low Mem $0214
MEM0215 = $0215			; Low Mem $0215 (Last keypress?)
MEM0216 = $0216			; Low Mem $0216

;------------------------------ VECTORS

INVEC   = $0218			; $FFBA - Input Vector
OUTVEC  = $021A			; $F0D3 - Output Vector
CCVEC   = $021C			; $FF9A - Control-C Vector
LDVEC   = $021E			; $FF8B - Load Vector
SAVVEC  = $0220			; $xxxx - Save Vector

NMI     = $022E			; $xxxx - NMI Vector

;------------------------------ EDITOR STORAGE

MOVFLAG = $0222			; Move Flag for insert Routine
INSFLAG = $0223			; Insert Flag
SKBFLAG = $0224			; FF next character returns BASIC command
CURPSH  = $0225			; Cursor Position HI byte
QUOFLAG = $0226			; Quotes Flag
EDFLAG  = $0227			; Edit Flag. Get a line from the screen
CREGFLAG= $0228			; Copy of Control Register
USERVEC = $0229			; User Vector
CURSYM  = $022C			; Cursor Symbol
CURFL   = $022D			; Cursor Flash Rate/suppress

NMI2    = $0232			; Second NMI Vector
NAM1    = $0234			; Filename to search for or to write
NAM2    = $023A			; Filename that was found

MEM0240 = $0240			; Low Mem $0240
REGMAP  = $0241			; Register map
MEM0242 = $0242			; Low Mem $0242
MEM0243 = $0243			; Low Mem $0243
MEM0244 = $0244			; Low Mem $0244
MEM0245 = $0245			; Low Mem $0245
MEM0246 = $0246			; Low Mem $0246
EXTVEC  = $0248			; BASIC command external Vector
DISKVEC = $024B			; Disk bootstrap Vector
PRFLAG  = $024E			; Print on/off Flag
TPFlag  = $024F			; Tape on/off Flag


;================================================================================
; Set Output File
;================================================================================

!TO "MONITOR.BIN",plain		; Generic filename


;################################################################################
; [$F000] Start of Monitor ROM
;################################################################################
; WEMON is a 4K Monitor ROM with start address of $F000.

*=MONITOR

;================================================================================
; [$F000] RESET - Cold Start Entry Point
;================================================================================

RESET:
          LDX #$28     
          TXS			; Set Stack pointer
          CLD			; Clear Decimal mode
          LDA #$7F		; Enable Cursor Flash 
          STA CURFL		; Cursor Flash Rate ($80=suppress)
          LDA #$06     
          STA MEM0240		; Low Mem $0240

          LDA #$00     
          STA LDFLAG		; Load Flag
          STA TPFlag		; Tape on/off Flag
          STA SCRSPD		; Scroll delay
          STA MOVFLAG		; Move Flag for insert Routine
          STA EDFLAG		; Edit Flag. Get a line from the screen
          STA INSFLAG		; Insert Flag
          STA SKBFLAG		; FF next character returns BASIC command
          STA QUOFLAG		; Quotes Flag
          STA CCFLAG		; Control-C Flag
          STA ERRORFL		; FF = Run error
          STA PRFLAG		; Print on/off Flag
          STA DISKVEC		; Disk bootstrap Vector LO
          STA SCRSPD2		; Scroll Speed 2

          LDA #$98     
          STA DISKVEC+1		; Disk bootstrap Vector HI ($9800)
          JSR PRINTINIT		; Initialize Parallel Printer Port

          LDY #$09		; # of bytes for Vector Table     

;================================================================================
; [$F041] VECLD - Load Vectors
;================================================================================

VECLD:
          LDA VECTABLE,Y	; Read Vector Table entry
          STA INVEC,Y		; Put it in the Vector table
          DEY          
          BPL VECLD
    
          LDA #$00     		; Clear out some zero page locations
          STA SCRNLO		; Zero Page $F6
          STA MCLOADFLG		; Zero Page $E0
          STA ZP_D6		; Zero Page $D6

          JSR ACIAINIT		; Initialize the ACIA

          LDA #$A1		; Solid block character
          STA CURSYM		; Set the Cursor Symbol
          JSR CLS		; Clear the screen

;================================================================================
; [$F062] RECALL - Print the Power-On Banner
;================================================================================

RECALL:
          LDX #$00     		; Offset of Power-on Banner
          JSR MSGOUT		; Print the message


;================================================================================
; [$F062] Power ON Menu
;================================================================================

MENULOOP:
          JSR KBRD		; Get a key from Keyboard
          AND #$5F		; Strip off lowercase bits

          CMP #$4D		; Is it "M"? (Monitor)
          BNE L_F075   
          LDA #$F0     
          PHA          
          LDA #$5E     
          PHA          
          PHP          
          JMP L_F855   

L_F075:
          CMP #$43		; Is it "C"?  (Cold Start ROM BASIC)
          BNE L_F07C   
          JMP BROMCOLD

L_F07C:
          CMP #$57		; Is it "W"? (Warm Start)
          BNE L_F083   
          JMP BROMWARM

L_F083:
          CMP #$44		; Is it "D"? (Disk Boot)
          BNE L_F08A   
          JMP (DISKVEC)		; Disk bootstrap Vector

L_F08A:
          CMP #$55		; Is it "U"? (User Vector)
          BNE MENULOOP 
          JSR USERVEC		; User Vector
          JMP MENULOOP 

;================================================================================
; [$F094] ACIAINIT - Initialize the ACIA
;================================================================================
;

ACIAINIT:
          LDA #$03     
          STA ACIA		; Relocated ACIA
          LDA #$11     
          STA ACIA		; Relocated ACIA
          STA TPFlag		; Tape on/off Flag
          RTS          

;================================================================================
; [$F0A2] CLS - Clear the Screen
;================================================================================
; Currently clears 1K of Video RAM

CLS:
          LDA #$60     
          LDY #$00     
          STA CURCHR		; Character under cursor

CLSLOOP:
          STA SCREEN+$300,Y	; Video Character RAM page 4
          STA SCREEN+$200,Y	; Video Character RAM page 3
          STA SCREEN+$100,Y	; Video Character RAM page 2
          STA SCREEN,Y		; Video Character RAM
          INY          
          BNE CLSLOOP  


;================================================================================
; [$F0B8] HOCUR - Home the Cursor
;================================================================================

HOCUR:
          LDA #$D0		; Screen HOME HI address ###############
          STA CURPSH		; Cursor Position HI byte
          STA SCRNHI		; Zero Page $F7

          LDY #$4B		; Screen HOME LO address - Original ROM ################
;         LDY #$CB		; Screen HOME LO address - Alternate ROM ###############

          STY CURPSL		; Cursor Position LO byte
          BNE L_F10E   

;================================================================================
; [$F0B8] SETCUR - Set the Cursor
;================================================================================

SETCUR:
          LDA #$00     
          STA SCRNLO		; Zero Page $F6
          LDA CURPSH		; Cursor Position HI byte
          STA SCRNHI		; Zero Page $F7
          LDY CURPSL		; Cursor Position LO byte
          RTS          


;================================================================================
; [$F0D3] WRITESCRN - Write character to screen 
;================================================================================
; Output Vector pointa here

OUTVECROM:
WRITESCRN:
          STA TEMCHR		; Temporary character store
          PHA          
          TXA          
          PHA          
          TYA          
          PHA          
          JSR SETCUR   
          JSR WRITE    
          JSR PUTCUR   
          JMP L_F237   


;================================================================================
; [$F0E7] Restore character under cursor
;================================================================================

L_F0E7:
          LDA CURCHR		; Character under cursor
          STA (SCRNLO),Y	; Zero Page $F6
          RTS          

WRITE:
          LDA TEMCHR		; Temporary character store
          CMP #$0D		; Is it <CR>?
          BNE L_F124   


;================================================================================
; [$F0F4] CR - Perform Carriage Return <CR>
;================================================================================

CR:
          JSR START    
          CPY #$CA     
          BEQ L_F104   
          JSR LNFD2		; Move down one line, scrolling if needed   

          LDA #$60     
          CMP (SCRNLO),Y	; Zero Page $F6
          BEQ L_F10B   
L_F104:
          JSR L_F175   
          LDA #$60     
          STA (SCRNLO),Y	; Zero Page $F6
L_F10B:
          INY          
          DEC ZP_D6		; Zero Page $D6
L_F10E:
          LDA #$00     
          STA SCRNLO		; Zero Page $F6
          STA QUOFLAG		; Quotes Flag
          STA EDFLAG		; Edit Flag. Get a line from the screen
          STA MOVFLAG		; Move Flag for insert Routine
          STA INSFLAG		; Insert Flag
          LDA #$A1     
          STA CURSYM		; Cursor Symbol
          RTS          

L_F124:
          CMP #$22		; Is is <QUOTE>?
          BNE L_F12B   
          JSR TQUOTE   
L_F12B:
          CMP #$20		; Is it <SPACE>?
          BCC L_F146   
          CMP #$3F		; Is is <?>?
          BNE L_F143   
          JSR SOL      
          BNE L_F141   
          LDA #$FF     
          CMP ZP_88		; Zero Page $88
          BEQ L_F141   
          STA ERRORFL		; FF = Run error
L_F141:
          LDA #$3F     
L_F143:
          JMP PUT1     
L_F146:
          CMP #$0E     
          BEQ L_F155   
          BIT QUOFLAG		; Quotes Flag
          BPL L_F155   
          JSR L_F1D6   
          JMP PUT1     
L_F155:
          CMP #$08     
          BNE LNFD     
          DEY          
          JSR SOL      
          BCS L_F170   
          JSR SOP      
          BCS L_F16D   
          LDA #$D0     
          STA ZP_F3		; Zero Page $F3
          LDY #$7B     
          JMP SCRDN    
L_F16D:
          JSR OLDLN    
L_F170:
          RTS          


;================================================================================
; [$F171] LNFD - Perform Line Feed
;================================================================================

LNFD:
          CMP #$0A		; Is it <LF>?
          BNE L_F196   
L_F175:
          LDA ZP_D6		; Zero Page $D6
          BNE L_F191   
          CPY #$CA     
          BCC LNFD2   
          LDA #$D3     
          CMP SCRNHI		; Zero Page $F7
          BNE LNFD2   
          STA ZP_F3		; Zero Page $F3
          JMP SCROLL   
LNFD2:
          TYA          
          CLC          
          ADC #$40		; Line length = 64 ################### hardcoded?
          TAY          
          BCC L_F191   
          INC SCRNHI		; Zero Page $F7
L_F191:
          LDA #$00     
          STA ZP_D6		; Zero Page $D6
          RTS          

L_F196:
          CMP #$09		; Is it CTRL-I? (CURSOR RIGHT)
          BEQ L_F1F1   

;================================================================================
; [$F19A] VTAB - Perform Vertical TAB
;================================================================================

VTAB:
          CMP #$1A		; Is it CTRL-Z? (CURSOR UP)     
          BNE L_F1B9
   
          CPY #$8B     
          BCS L_F1AD   
          LDA #$D0     
          CMP SCRNHI		; Zero Page $F7
          BNE L_F1AD   
          STA ZP_F3		; Zero Page $F3
          JMP SCRDN    
L_F1AD:
          PHA          
          TYA          
          SEC          
          SBC #$40		; Screen line length 64 ########## hardcoded?
          TAY          
          BCS L_F1B7   
          DEC SCRNHI		; Zero Page $F7
L_F1B7:
          PLA          
          RTS

;================================================================================
; [$F1B9] Check More Control Characters
;================================================================================
          
L_F1B9:
          CMP #$0C		; Is it CTRL-L? (Clear Screen)
          BNE L_F1C0		; No, skip ahead
          JMP CLS      
L_F1C0:
          CMP #$01		; Is it CTRL-A? (HOME CURSOR)
          BNE L_F1C7		; No, skip ahead
          JMP HOCUR 		; Home the cursor
L_F1C7:
          CMP #$0E		; Is it CTRL-N? (DELETE/RUBOUT)
          BNE L_F1CE		; No, skip ahead
          JMP DELETE   		; Delete it
L_F1CE:
          CMP #$1E		; Is it SHIFT-DELETE/RUBOUT?
          BNE BELL		; No, skip ahead
          JMP INSERT		; Yes, Insert space   

;================================================================================
; [$F1D5] BELL
;================================================================================
; For Series II Superboard with added circuitry.
; This appears to have been removed.

BELL:
          RTS          

;================================================================================
; [$F1D6] ???
;================================================================================

L_F1D6:
          STA ZP_EC		; Zero Page $EC
          TYA          
          PHA          
          JSR L_F5CB   
          JSR L_F5D4   

          CMP #$FF		; Is it $FF?
          BEQ L_F1EA		; Yes, jump ahead

          JSR L_F5C2   
          JSR L_F5E2   
L_F1EA:
          PLA          
          TAY          
          LDA ZP_EC		; Zero Page $EC
          RTS          

;================================================================================
; [$F1EF] PUT1 - Put Character in Accumulator
;================================================================================
; Puts character in .A to screen memory pointed to by $F6/$F7 and .Y
PUT1:
          STA (SCRNLO),Y	; Zero Page $F6
L_F1F1:
          INY          
          JSR EOL      
          BCC PUTDONE   
          JSR EOP      
          BCS L_F205   
          JSR NEWLN    
PUTLOOP:
          LDA #$20     
          STA (SCRNLO),Y	; Zero Page $F6
          INY          
PUTDONE:
          RTS          

L_F205:
          LDY #$CA     
          JSR SCROLL   
          BNE PUTLOOP   

EOP:
          PHA          
          TYA          
          CMP #$FB     
          BCS L_F214   
          PLA          
          RTS          

L_F214:
          LDA SCRNHI		; Zero Page $F7
          CMP #$D3		; ????? Last page of Screen?
          PLA          
          RTS          


;================================================================================
; [$F21A] EOL - Move to End of Line
;================================================================================
; Tests Y Register for equality to the low byte of the end of and screen line.

EOL:
          PHA          
          TYA          
          AND #$3F     
          CMP #$3B     
          PLA          
          RTS          

;================================================================================
; [$F222] NEWLN - 
;================================================================================
; Sets Y register to first printing position-1 on present line. Adjusts $F7 if required

NEWLN:
          JSR START		; Start of screen
          JSR LNFD2		; Move down one line, scrolling if needed
          RTS          


;================================================================================
; [$F229] PUTCUR - Put Cursor On screen
;================================================================================
; Saves screen character as cursor position in CURCHR. Updates CURPSL/CURPSH.

PUTCUR:
          LDA (SCRNLO),Y	; Zero Page $F6
L_F22B:
          STA CURCHR		; Character under cursor
L_F22E:
          LDA SCRNHI		; Zero Page $F7
          STA CURPSH		; Cursor Position HI byte
          STY CURPSL		; Cursor Position LO byte
          RTS          

L_F237:
          PLA          
          TAY          
          PLA          
          TAX          
          PLA          
          JMP CHAROUT2   

;================================================================================
; [$F23F] SOP - Test for Start of Page
;================================================================================
; Tests $F6, $F7 and .Y for start of page. Returns carry clear if true.

SOP:
          CPY #$4B     
          BCS L_F249   
          PHA          
          LDA SCRNHI		; Zero Page $F7
          CMP #$D1		; Screen page?
          PLA          
L_F249:
          RTS          

SOL:
          TYA          
          AND #$3F     
          CMP #$0B     
          RTS          

;================================================================================
; [$F250] OLDLN - Last Position of Previous line
;================================================================================
; Puts .Y equal to last printing position on previous line. Adjusts $F7 if required

OLDLN:
          PHA          
          TYA          
          SEC          
          SBC #$10     
          TAY          
          BCS L_F25A   
          DEC SCRNHI		; Zero Page $F7
L_F25A:
          PLA          
          RTS          

;================================================================================
; [$F25C] START - Start of Current Line
;================================================================================
; Puts .Y equal to start-1 of current line.

START:
          PHA          
          TYA          
          AND #$C0     
          ORA #$0A     
          TAY          
          PLA          
          RTS          


;================================================================================
; [$F265] SCROLL - Scrolls whole screen up
;================================================================================
; Scroll whole screen up 1 line. Delays if SPACEBAR is pressed. Returns with
; cursor position updated.

SCROLL:
          JSR L_F28A   
          LDY SCRSPD2		;Scroll Speed 2
          BEQ L_F270   
          JSR DELAY    
L_F270:
          JSR FCHAR    
          BNE SCRLDONE   

          LDA #$03     
          STA ZP_E5		;Zero Page $E5
          LDY #$C0     
SCROLLOOP:
          JSR DELAY    
          JSR FCHAR    
          BNE SCRLDONE   
          DEC ZP_E5		; Zero Page $E5
          BNE SCROLLOOP   
SCRLDONE:
          JMP SETCUR   

;================================================================================
; [$F28A] SCROLL - Scrolls whole screen up
;================================================================================

L_F28A:
          JSR L_F22E   
L_F28D:
          LDY #$40     		; Second line?
          LDX #$CF     
          LDA #$40		; Second line?     
          STA ZP_F8		; Zero Page $F8

L_F295:
          INX          
          STX ZP_F9		; Zero Page $F9
          STX SCRNHI		; Zero Page $F7

L_F29A:
          LDA (ZP_F8),Y		; Zero Page $F8
          STA (SCRNLO),Y	; Zero Page $F6
          INY          
          BNE L_F29A
   
          CPX #$D3		; Last screen page? ######## hardcoded
          BNE L_F295   

          LDA #$60     
          DEY          
L_F2A8:
          STA (SCRNLO),Y	; Zero Page $F6
          DEY          
          CPY #$C0     
          BNE L_F2A8   
          RTS          


;================================================================================
; [$F2B0] Screen Down
;================================================================================
; Scrolls screen down 1 line from cursor position. Cursor position remains as
; before. Start line absolute address should be in $F7 (high) and Y register (low)
; $F6 should be $00.

SCRDN:
          JSR L_F2B5   
          BMI SCRLDONE   
L_F2B5:
          JSR L_F22E   
L_F2B8:
          TYA          
          AND #$C0     
          STA ZP_F4		; Zero Page $F4
          LDY #$BF     
          LDA #$40     
          STA ZP_F8		; Zero Page $F8
          LDX #$D4     

L_F2C5:
          DEX          
          STX SCRNHI		; Zero Page $F7
          STX ZP_F9		; Zero Page $F9

L_F2CA:
          LDA (SCRNLO),Y		; Zero Page $F6
          STA (ZP_F8),Y		; Zero Page $F8
          CPY ZP_F4		; Zero Page $F4
          BNE L_F2D6   
          CPX ZP_F3		; Zero Page $F3
          BEQ L_F2DD   

L_F2D6:
          DEY          
          CPY #$FF     
          BNE L_F2CA   
          BEQ L_F2C5   

L_F2DD:
          LDA #$3F     
          STA ZP_F8		; Zero Page $F8
          LDA CURPSL		; Cursor Position LO byte
          AND #$C0     
          TAY          
          LDA #$60     
L_F2E9:
          STA (ZP_F8),Y		; Zero Page $F8
          DEC ZP_F8		; Zero Page $F8
          BPL L_F2E9   
          RTS          

;================================================================================
; [$F2F0] INVECN - Normal INVEC Handler
;================================================================================

INVECN:
          LDA ERRORFL		; Is there a RUN error? (FF)
          BEQ RUNERR		; Yes FF, skip ahead
          BMI L_F2FF		;

          LDA #$00		; Zero
          STA ERRORFL		; FF = Run error
          JMP BROMWARM		; Jump to BASIC Warmstart

L_F2FF:
          LDA #$00		; Zero
          CMP ZP_8C		; BASIC STORAGE for ERROR?
          BNE L_F308		;   
          JMP PRERRL		; Print the ERROR LINE   

;-------- Run ERROR FF
L_F308:
          STA ERRORFL		; FF = Run error

;-------- RUN ERROR
RUNERR:
          TYA          
          PHA          
          TXA          
          PHA          
L_F30F:
          JSR KBRD		; Get a key from Keyboard     
          CMP #$0D		; Is it <CR>?
          BEQ L_F32D   
          CMP #$0E		; Is it CTRL-N? (DELETE)
          BEQ L_F323   
          BIT QUOFLAG		; Quotes Flag
          BMI L_F33D   
          CMP #$20		; Is it <SPACE>?     
          BCS L_F33D		; Higher, skip ahead
L_F323:
          BIT EDFLAG		; Edit Flag. Get a line from the screen
          BMI L_F34F   
          DEC EDFLAG		; Edit Flag. Get a line from the screen
          BNE L_F34F   
L_F32D:
          JSR SETCUR   
          LDA CURCHR		; Character under cursor
          STA (SCRNLO),Y	; Zero Page $F6
          BIT EDFLAG		; Edit Flag. Get a line from the screen
          BPL L_F358   
          JMP L_F57D   

;-------- Handle characters > SPACE
L_F33D:
          PLA          
          PHA          
          TAX          
          LDA MEM0213		; Low Mem $0213
          STA INBUF,X		; Sace it to the input buffer. Zero Page $13
          STX ZP_DF		; Save the INDEX to the input buffer. Zero Page $DF
          INX          
          PLA          
          TXA          
          PHA          
          CPX #$47     
          BEQ L_F358   

L_F34F:
          LDA MEM0213		; Low Mem $0213
          JSR WRITESCRN		; Write it to the screen
          JMP L_F30F   

L_F358:
          PLA          
          TAX          
          PLA          
          TAY          

L_F35C:
          STX ZP_F8		; Zero Page $F8
          TSX          
          INX          
          INX          
          INX          
          INX          
          TXS          
          LDX ZP_F8		; Zero Page $F8
          JMP BROMA866		; Jump to BASIC routine ??????????????


;================================================================================
; [$F369] KBRD - Keyboard Scanner
;================================================================================
; Main entry point of keyboard routine. Any keyboard controlled function command
; while in the wait loop will be executed. Characters will not be echoed to the
; screen. No registers need to be saved.

KBRD:
          TYA          
          PHA          
          TXA          
          PHA          

KEYLOOP:
          LDA #$02     
L_F36F:
          JSR INVWR    
          JSR RDCOL    
          BNE L_F391   
          ASL          
          BNE L_F36F   
          LDA #$01     
          JSR INVWR    
          JSR RDCOLA   
          STA ZP_EC		; Zero Page $EC
          AND #$FE     
          CMP #$44     
          BNE L_F3C4   
          LDA #$FF     
          STA SKBFLAG		; FF next character returns BASIC command
          BNE KEYLOOP  
L_F391:
          LSR          
          JSR L_F3FE   
          TYA          
          STA MEM0213		; Low Mem $0213
          ASL          
          ASL          
          ASL          
          SEC          
          SBC MEM0213		; Low Mem $0213
          STA MEM0213		; Low Mem $0213
          TXA          
          LSR          
          JSR L_F3FE   
          BNE L_F3C4   
          TYA          
          CLC          
          ADC MEM0213		; Low Mem $0213
          TAY          
          LDA KEYMATRIX,Y	; Convert keyboard matrix position to ASCII code
          CMP MEM0215		; Low Mem $0215 (Last keypress?)
          BNE L_F3F3   
          DEC MEM0214		; Low Mem $0214
          BEQ L_F405   
          LDY #$05     
          JSR DELAY    
          BEQ KEYLOOP  
L_F3C4:
          BIT $D7      
          BMI L_F3E9   
          DEC $D7      
          BNE L_F3EE   
          DEC MEM0240		; Low Mem $0240
          BNE L_F3E9   
          JSR SETCUR   
          LDA CURCHR		; Character under cursor
          CMP (SCRNLO),Y		; Zero Page $F6
          BEQ L_F3DF   
          STA (SCRNLO),Y		; Zero Page $F6
          BNE L_F3E4   
L_F3DF:
          LDA CURSYM		; Cursor Symbol
          STA (SCRNLO),Y		; Zero Page $F6
L_F3E4:
          LDA #$06     
          STA MEM0240		; Low Mem $0240
L_F3E9:
          LDA CURFL		; Cursor Flash Rate/suppress
          STA $D7      
L_F3EE:
          LDA #$00     
          STA MEM0216		; Low Mem $0216
L_F3F3:
          STA MEM0215		; Low Mem $0215 (Last keypress?)
          LDA #$02     
          STA MEM0214		; Low Mem $0214
          JMP KEYLOOP  
L_F3FE:
          LDY #$08     
L_F400:
          DEY          
          ASL          
          BCC L_F400   
          RTS          

L_F405:
          PHA          
          TYA          
          PHA          
          JSR SETCUR   
          LDA CURCHR		; Character under cursor
          STA (SCRNLO),Y		; Zero Page $F6
          PLA          
          TAY          
          PLA          
          BIT SKBFLAG		; FF next character returns BASIC command
          BPL L_F41B   
          JMP HOTKEYS   
L_F41B:
          LDX #$96     
          CMP MEM0216		; Low Mem $0216
          BNE L_F424   
          LDX #$14     
L_F424:
          STX MEM0214		; Low Mem $0214
          STA MEM0216		; Low Mem $0216
          LDA ZP_EC		; Zero Page $EC
          AND #$40     
          BNE L_F466   
          LDA ZP_EC		; Zero Page $EC
          LSR          
          AND #$03     
          BNE L_F449   
          BCC L_F43E   
          LDA MEM0215		; Low Mem $0215 (Last keypress?)
          BNE KEYEXIT   
L_F43E:
          LDA MEM0215		; Low Mem $0215 (Last keypress?)
          CMP #$41     
          BCC KEYEXIT   
          ORA #$20     
          BNE KEYEXIT   
L_F449:
          LDA MEM0215		; Low Mem $0215 (Last keypress?)
          CMP #$41     
          BCS KEYEXIT   
          CMP #$20     
          BEQ KEYEXIT   
          CMP #$0D     
          BEQ KEYEXIT   
          CMP #$30     
          BEQ KEYEXIT   
          BCS L_F462   
          ORA #$10     
          BNE KEYEXIT   
L_F462:
          AND #$2F     
          BNE KEYEXIT   
L_F466:
          LDA MEM0215		; Low Mem $0215 (Last keypress?)
          AND #$1F     
          CMP #$10     
          BNE L_F477   
          LDA #$FF     
          EOR PRFLAG		; Print on/off Flag
          JMP KEYLOOP  
L_F477:
          CMP #$11     
          BNE L_F480   
          JSR TQUOTE   
          BNE L_F490   
L_F480:
          CMP #$14     
          BNE L_F489   
          JSR TSW      
          BNE L_F490   
L_F489:
          CMP #$04     
          BNE L_F493   
          JSR OPEN     
L_F490:
          JMP KEYLOOP  
L_F493:
          CMP #$05     
          BNE KEYEXIT   
          JSR EXTEND   
          BMI L_F490   
KEYEXIT:
          STA MEM0213		; Low Mem $0213
          PLA          
          TAX          
          PLA          
          TAY          
          LDA MEM0213		; Low Mem $0213
          RTS          


;================================================================================
; [$F4A7] EXTEND - Modifies the CHRGET Routine (Keyboard routine)
;================================================================================

EXTEND:
          LDX #$03     
EXTLOOP:
          LDA TABLE1,X 
          STA ZP_C5,X		; Zero Page $C5
          DEX          
          BPL EXTLOOP   
          RTS          

TABLE1:
          JMP L_F4B6   
          NOP

;================================================================================
; [$F4B6] ???
;================================================================================

L_F4B6:
          PHP          
          PHA          
          LDA ZP_C4		; Zero Page $C4
          BNE L_F4CD
          LDX ZP_DF		; Zero Page $DF
          BNE L_F4CD   
          PLA          
          PHA          
          CMP #$41		; Is it "A"?
          BCC L_F4CD   
          CMP #$5B		; Is it "Z"+1?
          BCS L_F4CD   
          JSR EXTVEC		; BASIC command external Vector
L_F4CD:
          PLA          
          PLP          
          JMP BROMBCF7


;================================================================================
; [$F4D2] PRERRL - Print Error Line  (Keyboard routine)
;================================================================================
; Prints the BASIC line whose number (in binary fixed point) is in $87,$88.
; A Warm Start should be executed following this routine.

PRERRL:
          LDA ZP_88		; Zero Page $88
          LDY ZP_87		; Zero Page $87
          JSR BROMAFC1
          JSR BROMB96E
          LDX #$00     
          LDY #$39     
L_F4E0:
          LDA BANNER,Y 
          BEQ L_F4EB   
          STA INBUF,X		; Zero Page $13
          INY          
          INX          
          BNE L_F4E0   
L_F4EB:
          LDY #$00     
L_F4ED:
          LDA STACK+1,Y		; Stack offset 1
          BEQ L_F4F9   
          STA INBUF00,X		; Zero Page $13 #### Original code does NOT use zp addressing
          INY          
          INX          
          BNE L_F4ED   
L_F4F9:
          LDA #$7F     
          STA ERRORFL		; FF = Run error
          JMP L_F35C   


;================================================================================
; [$F501] TQUOTE - Toggle Quote Mode (Keyboard routine)
;================================================================================
; Toggles quotes flag and adjusts cursor symbol.

TQUOTE:
          PHA          
          LDA #$FF     
          EOR QUOFLAG		; Toggle Quotes Flag
          STA QUOFLAG		; Save it back
          BPL L_F510   
          LDA #$17		; <NE Arrow> symbol Cursor
          BNE L_F512   
L_F510:
          LDA #$A1		; Reverse <SPACE> Cursor
L_F512:
          STA CURSYM		; Cursor Symbol
          PLA          
          RTS          

;================================================================================
; [$F517] HOTKEYS - Process Keyboard BASIC TOKEN Hotkeys (Keyboard routine)
;================================================================================

HOTKEYS:
          LDA KEYTOKEN,Y	; Get Token offset
          TAY          
          TSX          
          LDA STACK+5,X		; Stack offset 5
          TAX          
HKLOOP:
          LDA BROMA084,Y        ; Get TOKEN from BASIC
          PHA			; Push it on the stack
          AND #$7F		; Strip of HI bit
          JSR CHAROUT		; Output character in accumulator
          PLA			; Pull it back
          BMI L_F532		; Was HI bit set? Yes, exit loop   
          STA INBUF,X		; Zero Page $13
          INY          
          INX          
          BNE HKLOOP
   
L_F532:
          AND #$7F		; Strip off HI bit
          STA INBUF,X		; Zero Page $13
          INX          
          CMP #$24		; Is it "$"?     
          BNE L_F543		; No, skip ahead   
          LDA #$28     		; Also print out the "("
          JSR CHAROUT		; Output character in accumulator   
          STA INBUF,X		; Zero Page $13
          INX          
L_F543:
          TXA          
          TSX          
          STA STACK+5,X		; Stack offset 5
          LDA #$00		; Turn off the flag
          STA SKBFLAG		; FF next character returns BASIC command
          JMP KEYLOOP		; Go back for more characters

;================================================================================
; [$F550] OPEN - Inserts blank line or single SPACE (Keyboard routine)
;================================================================================
; Opens a blank line or cursor one position. Cursor position (or position of line
; to be opened should be in CURPSL and CURPSH.

OPEN:
          JSR SETCUR   
          LDA CURPSH		; Cursor Position HI byte
          CPY #$CB     
          BCC L_F55F   
          CMP #$D3     
          BCC L_F55F   
L_F55E:
          RTS          

L_F55F:
          STA ZP_F3		; Zero Page $F3
          JSR L_F2B8   
          LDA #$60     
          STA CURCHR		; Character under cursor
          BNE L_F55E   

;================================================================================
; [$F56B] GETST - Get Start of Line (Keyboard routine)
;================================================================================
; Returns with absolute address-1 of start of a data line, regardless of whether
; it is less than or more than one line. Value is in $F8,$F9.

GETST:
          JSR START    
          LDA (SCRNLO),Y		; Zero Page $F6
          CMP #$60     
          BEQ L_F57C   
          JSR SOP      
          BCC L_F57C   
          JSR L_F1AD   
L_F57C:
          RTS

;--------          

L_F57D:
          LDX #$00     
          JSR GETST    
L_F582:
          INY          
          JSR EOL      
          BCC L_F595   
          CPX #$31     
          BCS L_F5BA   
          JSR EOP      
          BCS L_F5BA   
          JSR NEWLN    
          INY          
L_F595:
          LDA (SCRNLO),Y		; Zero Page $F6
          CMP #$60     
          BEQ L_F5BA   
          STA ZP_EC		; Zero Page $EC
          TYA          
          PHA          
          JSR L_F5C2   
          JSR L_F5D4   
          CMP #$FF     
          BEQ L_F5AF   
          JSR L_F5CB   
          JSR L_F5E2   
L_F5AF:
          PLA          
          TAY          
          LDA ZP_EC		; Zero Page $EC
          STA INBUF,X		; Zero Page $13
          INX          
          CPX #$47     
          BNE L_F582   
L_F5BA:
          TXA          
          TSX          
          STA STACK+1,X		; Stack offset 1
          JMP L_F358   

L_F5C2:
          LDA #$D7     
          STA ZP_FE		; Zero Page $FE
          LDA #$FF     
          STA ZP_FF		; Zero Page $FF
          RTS          

L_F5CB:
          LDA #$49     
          STA ZP_FE		; Zero Page $FE
          LDA #$FF     
          STA ZP_FF		; Zero Page $FF
          RTS          

L_F5D4:
          LDA ZP_EC		; Zero Page $EC
          LDY #$06     
L_F5D8:
          CMP (ZP_FE),Y		; Zero Page $FE
          BEQ L_F5E1   
          DEY          
          BPL L_F5D8   
          LDA #$FF     
L_F5E1:
          RTS          

L_F5E2:
          LDA (ZP_FE),Y		; Zero Page $FE
          STA ZP_EC		; Zero Page $EC
          RTS          

;================================================================================
; [$F5E7] INSERT - Insert SPACE (Keyboard routine)
;================================================================================
; Opens a 1 character space in a line of text at cursor position. All text at and
; to the right of the cursor is moved right 1 space. Will not open a line to a
; length greater than 72 characters. Scrolls up if on the bottom line and if it
; is in the middle of the screen and a new line is required.

INSERT:
          BIT INSFLAG		; Insert Flag
          BMI L_F62D   
          JSR GETST    
          LDX #$00     
          INY          

L_F5F2:
          LDA #$60     
          CMP (SCRNLO),Y	; Zero Page $F6
          BNE L_F605   
          CPX #$00     
          BEQ L_F62D   
          CPX #$47     
          BCC L_F632   
L_F600:
          DEC INSFLAG		; Insert Flag
          BNE L_F62D   
L_F605:
          CPX #$47     
          BCS L_F600   
          INX          
          INY          
          JSR EOL      
          BCC L_F5F2   
          CPX #$31     
          BCS L_F62D   
          JSR EOP      
          BCS L_F631   
          JSR NEWLN    
          LDA (SCRNLO),Y	; Zero Page $F6
          INY          
          CMP #$60     
          BEQ L_F628   
          DEC MOVFLAG		; Move Flag for insert Routine
          BNE L_F5F2   
L_F628:
          JSR OLDLN    
          BNE L_F632   
L_F62D:
          JSR SETCUR   
          RTS          

L_F631:
          DEY          
L_F632:
          LDA SCRNHI		; Zero Page $F7
          PHA          
          TYA          
          PHA          
          LDA (SCRNLO),Y	; Zero Page $F6
          PHA          
          INY          
          JSR EOL      
          BCC L_F684   
          BIT MOVFLAG		; Move Flag for insert Routine
          BMI L_F665   
          DEC MOVFLAG		; Move Flag for insert Routine
          JSR EOP      
          BCS L_F66B   
          JSR NEWLN    
          LDA SCRNHI		; Zero Page $F7
          STA ZP_F3		; Zero Page $F3
          PHA          
          TYA          
          PHA          
          JSR L_F2B8   
          PLA          
          TAY          
          PLA          
          STA SCRNHI		; Zero Page $F7
          LDA #$20     
          STA (SCRNLO),Y	; Zero Page $F6
          BNE L_F668   
L_F665:
          JSR NEWLN    
L_F668:
          INY          
          BNE L_F684   
L_F66B:
          LDA CURPSL		; Cursor Position LO byte
          SEC          
          SBC #$40     
          STA CURPSL		; Cursor Position LO byte
          TSX          
          LDA #$BA     
          STA $0102,X  
          JSR L_F28D   
          LDY #$CA     
          LDA #$20     
          STA (SCRNLO),Y	; Zero Page $F6
          INY          
L_F684:
          PLA          
          STA (SCRNLO),Y	; Zero Page $F6
          PLA          
          TAY          
          PLA          
          STA SCRNHI		; Zero Page $F7
          CPY CURPSL		; Cursor Position LO byte
          BEQ L_F69C   
          DEY          
          JSR SOL      
          BCS L_F632   
          JSR OLDLN    
          BNE L_F632   
L_F69C:
          LDA CURCHR		; Character under cursor
          INY          
          STA (SCRNLO),Y	; Zero Page $F6
          LDA #$20     
          DEY          
          STA (SCRNLO),Y	; Zero Page $F6
          STA CURCHR		; Character under cursor
          RTS          

;================================================================================
; [$F6AB] DELETE - Delete SPACE (Keyboard routine)
;================================================================================
; Does the opposite of INSERT. Moves character under cursor and all characters
; (up to 2 lines) left 1 space, overwriting character to the LEFT of cursor. Will
; not delete past the start of the line.

DELETE:
          JSR L_F0E7   
          JSR GETST    
          INY          
          CPY CURPSL		; Cursor Position LO byte
          BEQ L_F6F6   
          JSR SETCUR   
          DEC CURPSL		; Cursor Position LO byte
          LDA CURPSL		; Cursor Position LO byte
          AND #$3F     
          CMP #$0A     
          BNE L_F6D4   
          LDA CURPSL		; Cursor Position LO byte
          SEC          
          SBC #$10     
          STA CURPSL		; Cursor Position LO byte
          BCS L_F6D4   
          DEC CURPSH		; Cursor Position HI byte
L_F6D4:
          LDA SCRNHI		; Zero Page $F7
          PHA          
          TYA          
          PHA          
          LDA (SCRNLO),Y	; Zero Page $F6
          PHA          
          DEY          
          JSR SOL      
          BCS L_F6EA   
          JSR SOP      
          BCC L_F708   
          JSR OLDLN    
L_F6EA:
          PLA          
          STA (SCRNLO),Y	;Zero Page $F6
          CMP #$60     
          BNE L_F6F7   
          PLA          
          PLA          
          JSR SETCUR   
L_F6F6:
          RTS          

L_F6F7:
          PLA          
          TAY          
          PLA          
          STA SCRNHI		; Zero Page $F7
          INY          
          JSR EOL      
          BCC L_F6D4   
          JSR NEWLN    
          INY          
          BNE L_F6D4   
L_F708:
          PLA          
          PLA          
          JMP L_F22B   

;================================================================================
; [$F70D] DELAY - Delay Y milliseconds (Keyboard routine)
;================================================================================
; Delay routine that gives approximately 1ms delay for every unit value in Y
; register on entry. Y should not be ZERO on entry or delay will be 255ms.

DELAY:
          LDX #$C8     
L_F70F:
          DEX          
          BNE L_F70F   
          DEY          
          BNE DELAY    
          RTS          


;================================================================================
; [$F716] INVWR - Invert Keyboard Data and Write to Port (Keyboard routine)
;================================================================================
; Invert contents of accumulator and stores in keyboard latch.
; Accumulator is preserved.

INVWR:
          EOR #$FF     
          STA KEYBD		; Keyboard Port
          EOR #$FF     
          RTS          

;================================================================================
; [$F71E] RDCOL - Read Keyboard COLUMN (Keyboard routine)
;================================================================================
; Reads keyboard port and returns with inverted value read in X. Accumulator is
; preserved.

RDCOL:
          PHA          
          JSR RDCOLA   
          TAX          
          PLA          
          DEX          
          INX          
          RTS          


;================================================================================
; [$F727] RDCOLA - Read Keyboard Port and Invert Data (Keyboard routine)
;================================================================================
; Reads keyboard and port. Inverts value.

RDCOLA:
          LDA KEYBD		; Keyboard Port
          EOR #$FF		; Invert the bits
          RTS          

;================================================================================
; [$F72D] TSW - Inverts Tape Control State
;================================================================================
; Inverts state of tape control. Leaves TPFL se if tape now on, clear if tape off.

TSW:
          BIT TPFlag		; Tape on/off Flag
          BMI L_F73D   

L_F732:
          LDA #$51     
          STA ACIA		; Relocated ACIA
          ASL          
          STA TPFlag		; Tape on/off Flag
          BNE L_F745   
L_F73D:
          LDA #$11     
          STA ACIA		; Relocated ACIA
          STA TPFlag		; Tape on/off Flag
L_F745:
          RTS          

;================================================================================
; [$F746] FINTAPE - Turns off Tape
;================================================================================
; Turns off Tape, clears all tape related flags LOAD, SAVE, TPFL, MCLOAD.

FINTAPE:
          LDA #$00     
          STA SCRSPD		; Scroll delay
          STA LDFLAG		; Load Flag
          STA MCLOADFLG		; Zero Page $E0
          JSR L_F73D   
          RTS          

;================================================================================
; [$F754] ??? - Send 6 NULL bytes to tape
;================================================================================

TAPNULL6:
          LDX #$06		; 6 bytes     
TAPNULLA:
          LDA #$00		; <NULL>     
TAPNLOOP:
          JSR TAPOUT		; Output to tape
          DEX          
          BNE TAPNLOOP		; Loop back for more   
          RTS          

;================================================================================
; [$F75F] TAPOUT - Checks SPACEBAR to end.
;================================================================================
; Checks if SPACEBAR is down, if so calls FINTAPE then returns. If no, passes to
; TAP1.

TAPOUT:
          PHA          
          JSR FCHAR    
          BEQ L_F773   
          PLA          
;================================================================================
; [$F766] TAP1 - Tape Output
;================================================================================
; Puts a character in the accumulator to the ACIA TX buffer when it is empty,
; then returns.

TAP1:
          PHA			; Save the character
L_F767:
          LDA ACIA		; Relocated ACIA
          LSR          
          LSR          
          BCC L_F767   		; Is it busy? Yes, loop back and wait more
          PLA          		; No, get the character
          STA ACIA+1		; Send it to ACIA
          RTS          

L_F773:
          JSR FINTAPE		; Turn off tape
          PLA          
          RTS          


;=================================================================
; [$F778] Vectors Table
;=================================================================

VECTABLE:
          !BYTE <INVECROM,>INVECROM	; $0218 = $FFBA - Input Vector
	  !BYTE <OUTVECROM,>OUTVECROM	; $021A = $F0D3 - Output Vector
	  !BYTE <CCVECROM,>CCVECROM	; $021C	= $FF9A - Control-C Vector
	  !BYTE <LDVECROM,>LDVECROM	; $021E = $FF8B - Load Vector
          !BYTE <SAVVECROM,>SAVVECROM	; $022E = $FF96 - Save Vector
 
;================================================================================
; [$F782] MSGOUT - Put Message on Screen
;================================================================================
; Puts a message on the screen. Put message offset in X. Messages must be
; terminated with a $00.

MSGOUT:
          LDA BANNER,X 		; Get a character from string
          BEQ MSGDONE		; Is it zero? Yes, we're done
          JSR CHAROUT		; Output character in accumulator   
          INX			; Next position
          BNE MSGOUT		; Loop back for more   
MSGDONE:
          RTS          


;================================================================================
; [$F78E] BANNER - Power on banner and other messages
;================================================================================
; These messages are printed using the MSGOUT routine. Y register is set to the
; offset from 'BANNER'. Messages must end with $00.

BANNER: !TEXT "WEMON (C)1981.",$0D,$0A	; Power on Banner and
	!TEXT "M/C/W/D/U ?",$00		;   menu Prompt

MSG2    !TEXT $0D,$0A,"FOUND ",$00    	; Tape 'found '  message
MSG3    !TEXT $0D,$0A,"LOADING",$00   	; Tape 'Loading' message
MSG4    !TEXT $0D,$0A,"SAVING ",$00   	; Tape 'Saving'  message
MSG5    !TEXT "LIST",$00              	; 'List' message

 
;================================================================================
; [$F7CC] HEXCHECK - Check for valid HEX digit
;================================================================================
; Converts HEX character in .A to HEX digit.
; Returns with Hex character in .A or $80 if non-HEX.

HEXCHECK:
          CMP #$30     
          BMI L_F7E2   
          CMP #$3A     
          BMI L_F7DF   
          CMP #$41     
          BMI L_F7E2   
          CMP #$47     
          BPL L_F7E2   
          SEC          
          SBC #$07     
L_F7DF:
          AND #$0F     
          RTS          

L_F7E2:
          LDA #$80     
          RTS          

;================================================================================
; [$F7E5] MONPRINT - Monitor Print
;================================================================================
; Prints out in ASCII the contents of FD,FC,FA as FD,FC space space FA

MONPRINT:
          LDX #$03     
L_F7E7:
          JSR NXTDIG   
          CPX #$02     
          BNE L_F7F2   
          JSR SPC1     
          DEX          
L_F7F2:
          DEX          
          BPL L_F7E7   
          RTS          

;================================================================================
; [$F7F6] NXTDIG - Prints out HEX
;================================================================================
; Prints out HEX, a byte pointed to by X relative to FA.

NXTDIG:
          LDA ZP_FA,X		; Zero Page $FA
          LSR          
          LSR          
          LSR          
          LSR          
          JSR HEXIT    
          LDA ZP_FA,X		; Zero Page $FA
          JMP HEXIT    

;================================================================================
; [$F804] CRLF - Print a <CR> and <LF>
;================================================================================
; Executes a Carriage Return followed by a Line Feed.

CRLF:
          LDA #$0D		; <CR>     
          JSR CHAROUT		; Output character in accumulator   
          LDA #$0A		; <LF>
          JMP CHAROUT		; Output character in accumulator   

;================================================================================
; [$F80E] SPC2 and SPC1
;================================================================================
; Prints 2 spaces, or 1 space.

SPC2:
          LDA #$20		; <SPACE>
          JSR CHAROUT		; Output character in accumulator   
SPC1:
          LDA #$20		; <SPACE>
L_F815:
          JMP CHAROUT		; Output character in accumulator   


;================================================================================
; [$F818] HEXIT - Convert value to ASCII HEX digit
;================================================================================
; Converts value in accumulator to an ASCII character code 30-39 or 41-46
 
HEXIT:
          AND #$0F		; reduce to 0-15
          ORA #$30		; Add $30 -> "0"
          CMP #$3A		; Compare to "9"+1
          BMI L_F823		; Is it less? No, skip ahead   
          CLC			; Yes, clear carry then
          ADC #$07		; Add 7.
L_F823:
          BNE L_F815		; Alway branch


;================================================================================
; [$F825] ROTCHR - Rotate Characters
;================================================================================
; Rotates the lower nibble (4 LSB's) of .A and the two adjacent bytes (pointed to
; by X, relative to FA), left by 4 positions. X and Y are corrupted.
   
ROTCHR:
          LDY #$04     
          ASL          
          ASL          
          ASL          
          ASL          
L_F82B:
          ROL          
          ROL ZP_FA,X		; Zero Page $FA
          ROL ZP_FB,X		; Zero Page $FB
          DEY          
          BNE L_F82B   
          RTS          

;================================================================================
; [$F834] MINPUT - Get Monitor Input (Keyboard or ACIA)
;================================================================================
; Get Monitor input either from the keyboard if MC Load Flag ($E0) is clear, or
; from ACIA if $E0 is $FF.

MINPUT:
          LDA MCLOADFLG		; Zero Page $E0
          BEQ L_F83B   
          JMP L_FFC2   
L_F83B:
          JMP KBRD     

;================================================================================
; [$F83E] FCHAR - Check SPACEBAR down
;================================================================================
; Tests the keyboard for SPACEBAR down.
; Returns with X=0 if not down, or Z=0 if down.

FCHAR:
          LDA #$FD     
          STA KEYBD		; Keyboard Port
          LDA #$10     
          BIT KEYBD		; Keyboard Port
          RTS          


;================================================================================
; [$F849] NMI Entry Point
;================================================================================
; Machine Code Monitor entry point

IRQ:
MONBRK:
          STA MEM0245		; Low Mem $0245
          PLA          
          PHA          
          AND #$10     
          BNE L_F858   
          JMP (NMI2)  


;================================================================================
; [$F855] MACHINE LANGUAGE Monitor Entry Point
;================================================================================

L_F855:
          STA MEM0245		; Low Mem $0245
L_F858:
          STX MEM0246		; Low Mem $0246
          STY $0247    
          PLA          
          STA MEM0243		; Low Mem $0243
          PLA          
          SEC          
          SBC #$01     
          STA MEM0242		; Low Mem $0242
          PLA          
          SBC #$00     
          STA REGMAP		; Register map
          TSX          
          STX MEM0244		; Low Mem $0244

          LDX #$28		; Set Stack pointer
          TXS          
          CLD			; Clear decimal mode
          JSR CLS		; Clear the screen      
;
L_F87A:
          JSR PUTREG   
          JSR HOCUR		; Home the cursor
          JSR L_F2DD   
;
L_F883:
          JSR KBRD		; Get a key from Keyboard     
          STA ZP_EF		; Zero Page $EF
          JSR CHAROUT		; Output character in accumulator   
          JSR SPC1     
          LDA ZP_EF		; Zero Page $EF
 
          CMP #$52     		; Is it "R"?
          BNE L_F89D   
          JSR HOCUR		; Home the cursor    
          JSR L_F8F5   
          JMP L_F901   
;
L_F89D:
          CMP #$53		; Is it "S"?
          BNE L_F8A4   
          JMP L_FCBF   
;
L_F8A4:
          CMP #$4C     		; Is it "L"?
          BNE L_F8AB   
          JMP L_FCFD   
;
L_F8AB:
          JSR GETPAR		; Get parameter
          LDA ZP_EF		; Zero Page $EF
          CMP #$4D     		; Is it "M"?
          BNE L_F8CB   
          LDA ZP_E1		; Zero Page $E1
          BEQ L_F8C5   
          CMP #$03     
          BEQ L_F8C2   
          JSR L_F8F0   
          JMP L_F8AB   
;
L_F8C2:
          JMP L_FA5F   
;
L_F8C5:
          JSR CRLF     		; Print a <CR><LF>     
          JMP L_FE07   
;
L_F8CB:
          CMP #$46		; Is it "F"? (Fill block)
          BNE L_F8D2   
          JMP L_FAA0   
;
L_F8D2:
          CMP #$42		; Is it "B"? (Block)
          BNE L_F8D9   
          JMP L_FAEC   
;
L_F8D9:
          CMP #$56		; Is it "V"? (Verify/View Block)
          BNE L_F8E0   
          JMP L_FA21   
;
L_F8E0:
          CMP #$47		; Is it "G"? (Go)
          BNE L_F8E7   
          JMP L_FABA   
;
L_F8E7:
          JSR USERVEC		; User Vector
;
MONCRLF:
          JSR CRLF     		; Print a <CR><LF>     
          JMP L_F883   
;
L_F8F0:
          LDA #$0D		; <CR> character     
          JSR CHAROUT		; Output character in accumulator   
;
L_F8F5:
          JSR L_F2DD   
          LDA ZP_EF		; Zero Page $EF
          JSR CHAROUT		; Output character in accumulator   
          JSR SPC1     
          RTS          
;
;
L_F901:
          LDA #$00     
          STA ZP_EA		; Zero Page $EA
;
L_F905:
          JSR L_FA1B   
          BNE L_F910   
          JSR CHAROUT		; Output character in accumulator   
          JMP L_F87A   
;
L_F910:
          CMP #$30		; Is it "0"?
          BCS L_F926   
          CMP #$20     		; Is it <SPACE>?
          BEQ L_F920   
          CMP #$08     		;
          BEQ L_F920   
          CMP #$18     
          BNE L_F923   
;
L_F920:
          JSR CHAROUT		; Output character in accumulator   
;
L_F923:
          JMP L_F901   
;
L_F926:
          STA ZP_E9		; Zero Page $E9
          LDA ZP_EA		; Zero Page $EA
          BNE L_F940   
          LDA CURPSL		; Cursor Position LO byte
          SEC          
          SBC #$40     
L_F932:
          LDY #$06     
L_F934:
          CMP TABLE3,Y 
          BEQ L_F93E   
          DEY          
          BPL L_F934   
          BMI L_F901   
L_F93E:
          STY $EB      
L_F940:
          LDA ZP_E9		; Zero Page $E9
          JSR HEXCHECK		; Is it 0 to 0 or A to F?  
          BMI L_F905   
 
          PHA          
          LDA ZP_E9		; Zero Page $E9
          JSR CHAROUT		; Output character in accumulator   
          PLA          
          LDX ZP_EA		; Zero Page $EA
          BNE L_F956   
          LDX #$02     
          STX ZP_EA		; Zero Page $EA

L_F956:
          LDX #$00     
          JSR ROTCHR   
          DEC ZP_EA		; Zero Page $EA
          BNE L_F905   
          LDX $EB      
          LDA ZP_FA		; Zero Page $FA
          STA REGMAP,X		; Register map
          JMP L_F901   

;================================================================================
; [$F969] PUTREG - Put REGISTERS Header to Top of Screen
;================================================================================
; Writes the contents of the register map across the top of the screen
; Set Y register to offset from Top of Screen.

PUTREG:
          LDY #$0C		; Original ROM ##########################
;         LDY #$8C		; Alternate ROM #########################

          LDX #$00
L_F96D:
          LDA TABLE4,X 
          BEQ L_F979   
          STA SCREEN,Y		; Video Character RAM
          INY          
          INX          
          BNE L_F96D		; Loop for More
L_F979:
          LDX #$00     
L_F97B:
          LDY TABLE3,X 
          BEQ L_F986   
          JSR L_F987   
          INX          
          BNE L_F97B   
L_F986:
          RTS          


L_F987:
          LDA REGMAP,X		; Register map
          LSR          
          LSR          
          LSR          
          LSR          
          JSR L_F999   
          LDA REGMAP,X		; Register map
          INY          
          JSR L_F999   
          RTS          

L_F999:
          AND #$0F     
          ORA #$30     
          CMP #$3A     
          BMI L_F9A4   
          CLC          
          ADC #$07     
L_F9A4:
          STA SCREEN,Y		; Video Character RAM
          RTS          


;================================================================================
; [$F9A8] Monitor RESISTERS Header
;================================================================================

TABLE4:
          !TEXT "PC      FR    SP    ACC    XR    YR ",$00     ;MCM header

;================================================================================
; [$F9CD] ?? Monitor Table
;================================================================================

TABLE3:
          !TEXT $0F,$11,$17,$1D,$24,$2A,$30,$00 ; Original ROM ###################
;         !TEXT $8F,$91,$97,$9D,$A4,$AA,$B0,$00	; Alternate ROM ##################
 

;================================================================================
; [$F9D5] GETPAR - Get Parameter
;================================================================================
; Gets up to 3 16-bit parameters into
;    FA,FB,LSB,MSB,P1
;    FC,FF,LSB,MSB,P2
;    FE,FF,LSB,MSB,P3
; On return $E1 contains the parameter count.

GETPAR:
          LDX #$05     
          LDA #$00     
L_F9D9:
          STA ZP_FA,X		; Zero Page $FA
          DEX          
          BPL L_F9D9   
          INX          
          STX ZP_E1		; Zero Page $E1
          STX ZP_E3		; Zero Page $E3
          JSR L_FA1B   
          BEQ L_FA1A   
          INC ZP_E1		; Zero Page $E1
          BNE L_F9F1   
L_F9EC:
          JSR L_FA1B   
          BEQ L_FA1A   
L_F9F1:
          CMP #$2D     
          BEQ L_FA0A   
          STA ZP_E4		; Zero Page $E4
          JSR HEXCHECK		; Is it 0 to 0 or A to F?  
          BMI L_F9EC   
          PHA          
          LDA ZP_E4		; Zero Page $E4
          JSR CHAROUT		; Output character in accumulator   
          PLA          
          LDX ZP_E3		; Zero Page $E3
          JSR ROTCHR   
          BEQ L_F9EC   
L_FA0A:
          JSR CHAROUT		; Output character in accumulator   
          LDA ZP_E1		; Zero Page $E1
          ASL          
          STA ZP_E3		; Zero Page $E3
          INC ZP_E1		; Zero Page $E1
          LDX ZP_E1		; Zero Page $E1
          CPX #$04     
          BNE L_F9EC   
L_FA1A:
          RTS          

L_FA1B:
          JSR KBRD		; Get a key from Keyboard     
          CMP #$0D     
          RTS          

L_FA21:
          JSR L_FD38   
          LDX ZP_E1		; Zero Page $E1
          CPX #$02     
          BEQ L_FA30   
          JSR L_F8F0   
          JMP L_FA21   
L_FA30:
          LDX #$07     
          STX ZP_E4		; Zero Page $E4
          JSR CRLF     		; Print a <CR><LF>     
          LDX #$03     
          JSR NXTDIG   
          DEX          
          JSR NXTDIG   
          JSR SPC1     
L_FA43:
          LDY #$00     
          LDA (ZP_FC),Y		; Zero Page $FC
          STA ZP_FA		; Zero Page $FA
          LDX #$00     
          JSR NXTDIG   
          JSR SPC1     
          JSR L_FF33   
          BCS L_FA5C   
          DEC ZP_E4		; Zero Page $E4
          BPL L_FA43   
          BMI L_FA30   
L_FA5C:
          JMP MONCRLF   
L_FA5F:
          JSR CRLF     		; Print a <CR><LF>     
          LDX #$00     
          STX ZP_DE		; Zero Page $DE
          STX ZP_DF		; Zero Page $DF
L_FA68:
          LDY #$00     
          LDA ZP_FA		; Zero Page $FA
          CMP (ZP_FC),Y		; Zero Page $FC
          BNE L_FA87   
          JSR MONPRINT 
          JSR CRLF     		; Print a <CR><LF>     
          SED          
          CLC          
          LDA ZP_00DE		; Zero Page $DE ################## Original did NOT use ZP addressing!
          ADC #$01     
          STA ZP_DE		; Zero Page $DE
          BCC L_FA87   
          LDA ZP_DF		; Zero Page $DF
          ADC #$00     
          STA ZP_DF		; Zero Page $DF
L_FA87:
          CLD          
          JSR L_FF33   
          BCC L_FA68   
          LDA ZP_DE		; Zero Page $DE
          STA ZP_FA		; Zero Page $FA
          LDA ZP_DF		; Zero Page $DF
          STA ZP_FB		; Zero Page $FB
          LDX #$01     
L_FA97:
          JSR NXTDIG   
          DEX          
          BPL L_FA97   
          JMP MONCRLF   
L_FAA0:
          LDA ZP_E1		; Zero Page $E1
          CMP #$03     
          BEQ L_FAAC   
          JSR L_F8F0   
          JMP L_FAA0   
L_FAAC:
          LDY #$00     
          LDA ZP_FA		; Zero Page $FA
          STA (ZP_FC),Y		; Zero Page $FC
          JSR L_FF33   
          BCC L_FAAC   
          JMP MONCRLF   
L_FABA:
          LDA ZP_E1		; Zero Page $E1
          BEQ L_FAC8   
          LDA ZP_FA		; Zero Page $FA
          STA MEM0242		; Low Mem $0242
          LDA ZP_FB		; Zero Page $FB
          STA REGMAP		; Register map
L_FAC8:
          LDA REGMAP		; Register map
          PHA          
          LDA MEM0242		; Low Mem $0242
          STA REGMAP		; Register map
          PLA          
          STA MEM0242		; Low Mem $0242
          LDX MEM0244		; Low Mem $0244
          TXS          
          LDX MEM0246		; Low Mem $0246
          LDY $0247    
          LDA MEM0243		; Low Mem $0243
          PHA          
          LDA MEM0245		; Low Mem $0245
          PLP          
          CLI          
          JMP (REGMAP)		; Register map
L_FAEC:
          LDA ZP_E1		; Zero Page $E1
          CMP #$03     
          BEQ L_FAF8   
          JSR L_F8F0   
          JMP L_FAEC   
L_FAF8:
          LDA ZP_FD		; Zero Page $FD
          CMP ZP_FB		; Zero Page $FB
          BCC L_FB18   
          LDA ZP_FC		; Zero Page $FC
          CMP ZP_FA		; Zero Page $FA
          BCC L_FB18   
          LDY #$00     
L_FB06:
          LDA (ZP_FC),Y		; Zero Page $FC
          STA (ZP_FA),Y		; Zero Page $FA
          INC ZP_FA		; Zero Page $FA
          BNE L_FB10   
          INC ZP_FB		; Zero Page $FB
L_FB10:
          JSR L_FF33   
          BCC L_FB06   
L_FB15:
          JMP MONCRLF   
L_FB18:
          JSR ADJPTR   
          LDX #$00     
          JSR L_FB42   
          LDX #$01     
L_FB22:
          LDA ZP_F8,X		; Zero Page $F8
          EOR #$FF     
          STA ZP_F8,X		; Zero Page $F8
          DEX          
          BPL L_FB22   
L_FB2B:
          DEC ZP_FF		; Zero Page $FF
          DEC ZP_FB		; Zero Page $FB
          LDY #$FF     
L_FB31:
          LDA (ZP_FE),Y		; Zero Page $FE
          STA (ZP_FA),Y		; Zero Page $FA
          INC ZP_F8		; Zero Page $F8
          BNE L_FB3D   
          INC ZP_F9		; Zero Page $F9
          BEQ L_FB15   
L_FB3D:
          DEY          
          BNE L_FB31   
          BEQ L_FB2B   
L_FB42:
          LDA ZP_F8		; Zero Page $F8
          CLC          
          ADC ZP_FA		; Zero Page $FA
          STA ZP_FA,X		; Zero Page $FA
          LDA ZP_F9		; Zero Page $F9
          ADC ZP_FB		; Zero Page $FB
          STA ZP_FB,X		; Zero Page $FB
          RTS          

;================================================================================
; [$FB50] SAVEIT - Handle Save
;================================================================================
; Save Vector jumpe to $FF96, which re-directs to here

SAVEIT:
          PHA          
          TYA          
          PHA          
          TXA          
          PHA          
          JSR GETNAM   
          JSR L_FCB2   
          JSR L_FBCB   
L_FB5E:
          LDA (ZP_FC),Y		; Zero Page $FC
          BNE L_FB71   
          DEX          
          BNE L_FB73   
          JSR TAPOUT   
          JSR FINTAPE		; Turn off tape
          PLA          
          TAX          
          PLA          
          TAY          
          PLA          
          RTS          

L_FB71:
          LDX #$03     
L_FB73:
          JSR TAPOUT   
          JSR L_FBC4   
          BNE L_FB5E   

;================================================================================
; [$FB7B] DELAY5S - Delay 5 seconds
;================================================================================
; Delays approx 5 seconds. Corrupts A,Y,X.

DELAY5S:
          LDA #$14     
          STA $DD      
L_FB7F:
          LDY #$FF     
          JSR DELAY    
          DEC $DD      
          BNE L_FB7F   
          RTS          

;================================================================================
; [$FB89] ???
;================================================================================

LOADIT:
          JSR GETNAM   
L_FB8B:				; Incorrect Target of jump to middle of above ######
L_FB8C:
          JSR L_FBD6   
          LDX ZP_E2		; Zero Page $E2
          BNE L_FB97   
          DEC LDFLAG		; Load Flag
          RTS          

L_FB97:
          JSR L_FBCB   
L_FB9A:
          JSR L_FFC7   
          BNE L_FBBB   
          DEX          
          BNE L_FBBD   
          STA (ZP_FC),Y		; Zero Page $FC
          JSR L_FBC4   
          LDA ZP_FC		; Zero Page $FC
          STA ZP_7B		; Zero Page $7B
          LDA ZP_FD		; Zero Page $FD
          STA ZP_7C		; Zero Page $7C
          LDA #$01     
          STA ZP_79		; Zero Page $79
          LDA #$03     
          STA ZP_7A		; Zero Page $7A
          JSR FINTAPE		; Turn off tape
          RTS          

L_FBBB:
          LDX #$03     
L_FBBD:
          STA (ZP_FC),Y		; Zero Page $FC
          JSR L_FBC4   
          BNE L_FB9A   
L_FBC4:
          INC ZP_FC		; Zero Page $FC
          BNE L_FBCA   
          INC ZP_FD		; Zero Page $FD
L_FBCA:
          RTS          

L_FBCB:
          LDA #$03     
          STA ZP_FD		; Zero Page $FD
          TAX          
          LDA #$00     
          STA ZP_FC		; Zero Page $FC
          TAY          
          RTS          

L_FBD6:
          JSR L_F732   
          LDX MCLOADFLG		; Zero Page $E0
          BNE L_FBE1   
          LDX ZP_E2		; Zero Page $E2
          BEQ L_FBF8   
L_FBE1:
          JSR L_FC13   
          JSR GETNAM1  
          LDX #(MSG2-BANNER)	; $1C - Offset of message "FOUND"   
          JSR MSGOUT		; Print the message   

          JSR L_FF23   
          LDX ZP_E2		; Zero Page $E2
          BEQ L_FBF8   
          JSR COMPNAME 
          BCS L_FBE1   
L_FBF8:
          LDX #(MSG3-BANNER)	; $25 - Offset of message "LOADING" 
          JSR MSGOUT		; Print the message

          STA MCLOADFLG		; Zero Page $E0
          JSR CRLF     		; Print a <CR><LF>     
          RTS          


;================================================================================
; [$FC03] ???
;================================================================================

L_FC03:
          LDX #$20     
          LDA #$16     
L_FC07:
          JSR TAP1     
          DEX          
          BPL L_FC07   
          LDA #$2A     
          JSR TAP1     
          RTS          

L_FC13:
          LDX #$00     
L_FC15:
          JSR L_FFC7   
          CMP #$16     
          BNE L_FC1F   
          INX          
          BNE L_FC15   
L_FC1F:
          CMP #$2A     
          BNE L_FC13   
          CPX #$09     
          BCC L_FC13   
          RTS          

;================================================================================
; [$FC2A] COMPNAME - Compare Names
;================================================================================
; Compares two 6-character strings in NAM1 and NAM2.
; Returns carry clear if equal, or carry set if not equal.
; If one of the charactes in NAM1 is a "*" then the test terminates and carry clear

COMPNAME:
          LDX #$00     
L_FC2A:
          LDA NAM1,X		; Filename to search for or to write
          CMP #$2A     
          BEQ L_FC3D   
          CMP NAM2,X		; Filename that was found
          BEQ L_FC38   
          SEC          
          RTS          

L_FC38:
          INX          
          CPX #$06     
          BCC L_FC2A   
L_FC3D:
          CLC          
          RTS          

;================================================================================
; [$FC3F] GETNAM1 - Get Name 1
;================================================================================
; Reads 6 characters from tape and stores in NAM2

GETNAM1:
          LDX #$00     
L_FC41:
          JSR L_FFC7   
          STA NAM2,X		; Filename that was found
          INX          
          CPX #$06     
          BCC L_FC41   
          RTS          

;================================================================================
; [$FC4D] ???
;================================================================================

L_FC4D:
          JSR L_FC8B   
          TAX          
L_FC51:
          JSR L_FA1B   
          BEQ L_FC70   
          CMP #$2A     
          BNE L_FC5F   
          STA NAM1,X		; Filename to search for or to write
          BNE L_FC70   
L_FC5F:
          CMP #$20     
          PHP          
          JSR CHAROUT		; Output character in accumulator   
          PLP          
          BCC L_FC51   
          STA NAM1,X		; Filename to search for or to write
          INX          
          CPX #$06     
          BNE L_FC51   
L_FC70:
          STX ZP_E2		; Zero Page $E2
          RTS          

;================================================================================
; [$FC73] PUTNAM - Put Name
;================================================================================
; Puts contents of NAM2 out to tape.

PUTNAM:
          LDX #(MSG4-BANNER)	; $2F - Offset of message "SAVING"     
          JSR MSGOUT   		; Print the message
          TAX          
L_FC79:
          LDA NAM1,X		; Filename to search for or to write
          JSR CHAROUT		; Output character in accumulator   
          JSR TAP1     
          INX          
          CPX #$06     
          BCC L_FC79   
          JSR CRLF     		; Print a <CR><LF>     
          RTS          

L_FC8B:
          LDX #$0B     
          LDA #$00     
L_FC8F:
          STA NAM1,X		; Filename to search for or to write
          DEX          
          BPL L_FC8F   
          RTS          


;================================================================================
; [$FC96] GETNAM - Get Name
;================================================================================
; Gets 6 characters from keyboard and puts in NAM1.
; On return $E2=Number of characters upto but not including RETURN.
; Zeros NAM1 and NAM2 first. Only accept characters $30 to $5A.

GETNAM:
          JSR L_FC8B   
          TAX          
          LDA ZP_14,X		; Zero Page $14
          BEQ L_FCAB   
          INX          
L_FC9F:
          LDA ZP_14,X		; Zero Page $14
          BEQ L_FCAB   
          STA $0233,X  
          INX          
          CPX #$07     
          BNE L_FC9F   
L_FCAB:
          STX ZP_E2		; Zero Page $E2
          LDA #$61     
          STA INBUF		; Zero Page $13
          RTS          


;================================================================================
; [$FCB2] ???
;================================================================================

L_FCB2:
          JSR L_F732   
          JSR DELAY5S  
          JSR L_FC03   
          JSR PUTNAM   
          RTS          

;================================================================================
; [$FCBF] ???
;================================================================================

L_FCBF:
          JSR L_FC4D   
          JSR SPC1     
          JSR GETPAR		; Get parameter   
          CPX #$02     
          BEQ L_FCDF   
          JSR L_F8F0   
          JMP L_FCBF   
          BRK          

;================================================================================
; [$FCD3] ???
;================================================================================

L_FCD3:
          !BYTE $9D,$34		; Unknown could $9D be $AD - LDA $34
 
          CMP #$1C     
          BEQ L_FCDC   
          JMP BROMA374
L_FCDC:
          JMP BROMA359
L_FCDF:
          JSR L_FD38   
          JSR L_FCB2   
          LDX #$03     
L_FCE7:
          LDA ZP_FC,X		; Zero Page $FC
          JSR TAP1     
          DEX          
          BPL L_FCE7   
L_FCEF:
          LDY #$00     
          LDA (ZP_FC),Y		; Zero Page $FC
          JSR TAP1     
          JSR L_FF33   
          BCC L_FCEF   
          BCS L_FD32   
L_FCFD:
          JSR L_FC4D   
          JSR SPC1     
          JSR GETPAR		; Get parameter   
          DEC MCLOADFLG		; Zero Page $E0
          JSR L_FBD6   
          LDX #$03     
L_FD0D:
          JSR L_FFC7   
          STA ZP_FC,X		; Zero Page $FC
          DEX          
          BPL L_FD0D   
          LDX ZP_E1		; Zero Page $E1
          BEQ L_FD26   
          JSR ADJPTR   
          LDX #$04     
          JSR L_FB42   
          LDX #$01     
          JSR L_FD3A   
L_FD26:
          LDY #$00     
          JSR L_FFC7   
          STA (ZP_FC),Y		; Zero Page $FC
          JSR L_FF33   
          BCC L_FD26   
L_FD32:
          JSR L_F73D   
          JMP MONCRLF   

L_FD38:
          LDX #$03     
L_FD3A:
          LDA ZP_FA,X		; Zero Page $FA
          STA ZP_FC,X		; Zero Page $FC
          DEX          
          BPL L_FD3A   
          RTS          

;================================================================================
; [$FD42] ADJPTR - Adjust Pointers
;================================================================================
; Subtracts FC,FD from FE,FF and returns with the difference in F8,F9

ADJPTR:
          SEC          
          LDA ZP_FE		; Zero Page $FE
          SBC ZP_FC		; Zero Page $FC
          STA ZP_F8		; Zero Page $F8
          LDA ZP_FF		; Zero Page $FF
          SBC ZP_FD		; Zero Page $FD
          STA ZP_F9		; Zero Page $F9
          RTS          


;================================================================================
; [$FD50] ???
;================================================================================

          JSR L_FD79   
          LDX #$04     
          JSR $FB8B		; WAS: L_FB8B (MIDDLE OF INSTRUCTION) #####################
          LDX #$01     
          JSR L_FD71   
L_FD5D:
          LDY #$00		; Zero out
          JSR L_FFC7   
          STA (ZP_FC),Y		; Zero Page $FC
          JSR L_FF33   
          BCC L_FD5D   
          JSR $F784		; WAS: L_F784 (MIDDLE OF INSTRUCTION) #####################
          JMP $F931		; WAS: L_F931 (MIDDLE OF INSTRUCTION) #####################
          LDX #$03     
L_FD71:
          LDA ZP_FA,X		; Zero Page $FA
          STA ZP_FC,X		; Zero Page $FC
          DEX          
          BPL L_FD71   
          RTS          

;================================================================================
; [$FD79] ADJUST POINTERS 2
;================================================================================

L_FD79:
          SEC          
          LDA ZP_FE		; Zero Page $FE
          SBC ZP_FC		; Zero Page $FC
          STA ZP_F8		; Zero Page $F8
          LDA ZP_FF		; Zero Page $FF
          SBC ZP_FD		; Zero Page $FD
          STA ZP_F9		; Zero Page $F9
          RTS          

;================================================================================
; [$FD87] Filler
;================================================================================

	 !FILL $FE00-*,$AA	; Fill with $AA (79 bytes)
 

;================================================================================
; [$FE00] POLLED KEYBOARD INPUT ROUTINE
;================================================================================
; This directly interfaces to the keyboard matrix, scans the ROWs
; and reads the COLUMNs to determine which keys are pressed and
; translates them into ASCII to be used by the rest of the system.

* = $FE00

GETKEY    LDX #$28     
          TXS          
          CLD          
          JSR CLS		; Clear the screen      
L_FE07:
          LDA #$00     
          STA ZP_FC		; Zero Page $FC
          STA ZP_FD		; Zero Page $FD
          TAY          
          BEQ L_FE3E   
L_FE10:
          JSR MINPUT   
          AND #$7F     		; Strip off high bit
          CMP #$2F		; Is it "/"?
          BEQ L_FE4E   
          CMP #$47		; Is it "G"?
          BEQ L_FE4B   
          CMP #$4C		; Is it "L"?     
          BEQ L_FE86   
          CMP #$58		; Is it "X"?     
          BNE L_FE2F
   
          LDA #$FE     
          PHA          
          LDA #$25     
          PHA          
          PHP          
          JMP MONBRK   

L_FE2F:
          JSR HEXCHECK		; Is it 0 to 0 or A to F? 
          BMI L_FE10   
          LDX #$02     
          JSR ROTCHR   
          LDA #$1A     
          JSR WRITESCRN		; Write it to the screen
;
L_FE3E:
          LDA (ZP_FC),Y		; Zero Page $FC
          STA ZP_FA		; Zero Page $FA
          JSR MONPRINT 
          JSR CRLF     		; Print a <CR><LF>     
          JMP L_FE10   
L_FE4B:
          JMP (ZP_FC)		; Zero Page $FC
L_FE4E:
          JSR MINPUT   
          AND #$7F		; Strip off high bit     
          CMP #$2E		; Is it "."?     
          BEQ L_FE10   
          CMP #$0D		; Is it <CR>?
          BNE L_FE6A   
          INC ZP_FC		; Zero Page $FC
          BNE L_FE61   
          INC ZP_FD		; Zero Page $FD
L_FE61:
          LDY #$00     
          LDA (ZP_FC),Y		; Zero Page $FC
          STA ZP_FA		; Zero Page $FA
          JMP L_FE7D   
L_FE6A:
          JSR HEXCHECK		; Is it 0 to 0 or A to F?  
          BMI L_FE4E   
          LDX #$00     
          JSR ROTCHR   
          LDA ZP_FA		; Zero Page $FA
          STA (ZP_FC),Y		; Zero Page $FC
          LDA #$1A     
          JSR WRITESCRN		; Write it to the screen
L_FE7D:
          JSR MONPRINT 
          JSR CRLF     		; Print a <CR><LF>
          JMP L_FE4E   
L_FE86:
          STA MCLOADFLG		; Zero Page $E0
          BEQ L_FE4E   
          JMP L_FFC2   

;================================================================================
; [$FE8D] KEYBOARD MATRIX
;================================================================================
; The keyboard is organized as an 8x8 matrix of keys. In the main
; matrix only 7 keys are used on each ROW.
; The special modifier keys (RPT,CTRL,ESC,Both SHIFTs, and ShiftLock)
; are not included in the matrix and are handled separately.
; Because of this, the table below is a matrix of 7x7.

KEYMATRIX
KMATRIX
	!TEXT "P;/ ZAQ"		; Row 1
	!TEXT ",MNBVCX"		; Row 2
	!TEXT "KJHGFDS"		; Row 3
	!TEXT "IUYTREW"		; Row 4
	!BYTE $00,$00,$0D,$0A	; Row 5
	!TEXT "OL."
	!BYTE $00		; Row 6
	!BYTE $0E		; 		WAS: $5F
	!TEXT "-:098"
	!TEXT "7654321"		; Row 7

;================================================================================
; [$FEBE] Keyboard Token Offset Table
;================================================================================
; 49 bytes... keyboard translation

KEYTOKEN:
	!BYTE $54,$B2,$13,$75,$80,$C9,$A3	; Row 1
	!BYTE $B8,$DB,$06,$2D,$C6,$CC,$97	; Row 2
	!BYTE $BB,$9A,$16,$1D,$03,$0A,$C2	; Row 3
	!BYTE $0E,$41,$A0,$79,$D5,$00,$45	; Row 4
	!BYTE $00,$00,$32,$88,$3F,$BF,$94	; Row 5
	!BYTE $00,$65,$A9,$AC,$A6,$B5,$21	; Row 6
	!BYTE $61,$6D,$7D,$26,$38,$4D,$49	; Row 7
 

;================================================================================
; [$FEEF] PRINTINIT - Initialize Printing
;================================================================================
; Initializes a 6520 at $8800-$8803 to operate as a parallel port
; with handshake.

PRINTINIT:
          LDA #$2A     
          STA PARPORT+1		; Parallel Port $8801
          LDA #$FF     
          STA PARPORT		; Parallel Port $8800
          LDA #$2E     
          STA PARPORT+1		; Parallel Port $8801
          LDA #$2C     
          STA PARPORT+3		; Parallel Port $8803
          RTS          


;================================================================================
; [$FF04] PRINTOUT - Send Character to Printer Port
;================================================================================
; Writes character in .A to PIA at $8800 configured as output port.
; PA0-PA7 = D0-D7
; CA2     = DS Active LO
; RBQ     - off line/empty  Active HI
; PB1     - ACK
; PB6     - Busy Active HI

PRINTOUT:
          PHA          
          LDA PARPORT+1		; Parallel Port $8801
          ORA #$04     
          STA PARPORT+1		; Parallel Port $8801
          PLA          
          STA PARPORT		; Parallel Port $8800
          LDA PARPORT+2		; Parallel Port $8802
          LSR          
          BCS L_FF22   
          NOP          
          NOP          
          NOP          
L_FF1A:
          BIT PARPORT+2		; Parallel Port $8802
          BMI L_FF1A   
          LDA PARPORT		; Parallel Port $8800
;
L_FF22:
          RTS          

L_FF23:
          LDX #$00     
L_FF25:
          LDA NAM2,X		; Filename that was found
          BEQ L_FF32   
          JSR CHAROUT		; Output character in accumulator   
          INX          
          CPX #$06     
          BNE L_FF25   
L_FF32:
          RTS          

L_FF33:
          INC ZP_FC		; Zero Page $FC
          BNE L_FF39   
          INC ZP_FD		; Zero Page $FD
L_FF39:
          LDA ZP_FC		; Zero Page $FC
          CMP ZP_FE		; Zero Page $FE
          BNE L_FF47   
          LDA ZP_FD		; Zero Page $FD
          CMP ZP_FF		; Zero Page $FF
          BNE L_FF47   
          SEC          
          RTS          

L_FF47:
          CLC          
          RTS          

;================================================================================
; [$FF49] TABLE
;================================================================================
; This table doesn't seem to be referenced anywhere

L_FF49:
          !BYTE $09,$08,$01,$0C,$1A,$1E,$0A,$0E ;unknown


;================================================================================
; [$FF51] ???
;================================================================================
 
NULL10:
          TXA          
          PHA          
          LDX #$0A		; 10 bytes     
          JSR TAPNULLA		; Send 10 Nulls
          PLA          
          TAX          
          PLA          
          RTS          

;================================================================================
; [$FF5C] TABLE
;================================================================================

L_FF5C:
          !BYTE $AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA	;Filler
          !BYTE $AA,$AA,$AA,$AA,$AA		;Filler
 

;================================================================================
; [$FF69] CHAROUT - Output a Character
;================================================================================
; Sends the character in A to screen, tape or ACIA

CHAROUT:
          JSR L_FFEE   
CHAROUT2:
          PHA          
          BIT PRFLAG		; Is Parallel Printer Output ON?
          BPL NOPRNTR		; No, skip ahead
          PLA          
          PHA          
          JSR PRINTOUT		; Send to Parallel Printer
NOPRNTR:
          BIT SCRSPD		; Scroll delay?
          BPL NODELAY		; No, skip ahead   
          PLA          
          PHA          
          JSR TAPOUT		; Send out to Tape
          CMP #$0D		; Is it <CR>?     
          BEQ NULL10		; Send out 10 NULLS to TAPE
NODELAY:
          PLA          
          RTS          


;================================================================================
; [$FF87] Filler
;================================================================================

          TAX          
          TAX          
          TAX          
          TAX          


;================================================================================
; [$FF8B] LDVECROM - Load Vector Handler
;================================================================================

LDVECROM:
          JMP LOADIT		; Do the Load


;================================================================================
; [$FF8E] Filler
;================================================================================
   
          TAX          
          TAX          
          TAX          
          TAX          
          TAX          
          TAX          
          TAX          
          TAX          


;================================================================================
; [$FF96] SAVVECROM - Save Vector Handler
;================================================================================
; Save Vector points here

SAVVECROM:
          JMP SAVEIT		; Jump to real Save Routine

L_FF99    TAX			; Filler

   
;================================================================================
; [$FF9A] CCVECROM - CTRL-C Handler
;================================================================================

CCVECROM
CTRLC	  LDA CCFLAG		; Get Control-C Flag
          BNE CCNONE		; Is it enabled? No, skip ahead

          LDA #$FE		; Keyboard ROW#     
          STA KEYBD		; Keyboard Port
          BIT KEYBD		; Keyboard Port
          BVS CCNONE		; No keys, skip ahead

          LDA #$FB		; Keyboard ROW#          
          STA KEYBD		; Keyboard Port
          BIT KEYBD		; Keyboard Port
          BVS CCNONE   
          LDA #$03		; 3 = STOP?
          JMP BROMA636		; Break execution?
CCNONE:
          RTS          

;------------------------------ 

L_FFB9:
          !BYTE $2C		; Unknown- probably to confuse disassemblers

;================================================================================
; [$FFBA] INVECROM - Input Vector Handler
;================================================================================
 
INVECROM:
          BIT LDFLAG		; Is Load Flag set
          BMI L_FFC2		; Yes, skip ahead
          JMP INVECN		; No, do normal output
L_FFC2:
          JSR FCHAR    
          BEQ L_FFD1   
L_FFC7:
          LDA ACIA     		; Relocated ACIA
          LSR          
          BCC L_FFC2   
          LDA ACIA+1		; Relocated ACIA offset 1
          RTS          
L_FFD1:
          JSR FINTAPE		; Turn off tape
          JMP INVECN

;================================================================================
; [$FFD1] TABLE
;================================================================================

L_FFD7:
          !BYTE $C2,$C0,$A8,$CE,$10,$DA,$14,$D8	;unknown
 
;================================================================================
; [$FFDF] Filler
;================================================================================

L_FFDF:
          !BYTE $AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA	;Filler
          !BYTE $AA,$AA,$AA,$AA			;Filler

;================================================================================
; [$FFEE] VECTORS
;================================================================================
 
          JMP (INVEC)		; Input Vector
L_FFEE:   JMP (OUTVEC)		; Output Vector
          JMP (CCVEC)		; Control-C Vector
          JMP (LDVEC)		; Load Vector
          JMP (SAVVEC)		; Save Vector

;================================================================================
; [$FFFA] 6502 RESET, IRQ, and NMI Vectors
;================================================================================
; This is where the CPU jumps when powered on, or an interrupt is
; triggered. These must be located as the last 6 bytes of the ROM
; so that the CPU can start properly

*=$FFFA

L_FFFA:   !WORD NMI		; NMI   (normally not used)
L_FFFC:   !WORD RESET		; RESET
L_FFFE:   !WORD IRQ		; IRQ   (normally not used) 

; The End.