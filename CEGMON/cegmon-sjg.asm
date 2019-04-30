;=================================================================
; CEGMON MONITOR REVERSE ENGINEERED SOURCE CODE
;=================================================================

; BASED ON BINARY IMAGE FILE: "CEGMONC1.ROM"
;
; This disassembly started by unknown author ("Dave"?).
; Found on OSIWEB.ORG site.
; Adapted, Commented, and Enhanced by Steve J. Gray, Apr 25, 2019
; Formatted for ACME assembler.
;
;-----------------------------------------------------------------
; Features
;-----------------------------------------------------------------

; CHR$(10) - Cursor DOWN (line feed)
; CHR$(11) - Cursor RIGHT
; CHR$(12) - Cursor HOME (top left of window)
; CHR$(13) - Cursor to start of next line (carriage return)
; CHR$(26) - Clear Entire Screen (CTRL-Z)
; CHR$(30) - Clear Window (CTRL-SHIFT-N)
; -----
; NORMAL EMACS
; CTRL-E CTRL-E - Toggle Editor
; CTRL-A CTRL-B - Edit Cursor LEFT (back)
; CTRL-S CTRL-P - Edit Cursor UP (previous line)
; CTRL-D CTRL-F - Edit Cursor RIGHT (forward)
; CTRL-F CTRL-N - Edit Cursor DOWN (next line)
; ESC    CTRL-Y - Copy character at edit cursor (CTRL-Q on UK-101) (yank)


;=================================================================
; CONFIGURE MACHINE AND DISPLAY TYPE
;=================================================================
; MACHINE  NAME      NMI   IRQ   
; 0        C1P/SBII  $130  $1C0
; 1        UK101     $130  $1C0
; 2        C2/C4     $130  $1C0
;-----------------------------------------------------------------
; DISPLAY  DESCRIPTION  WIDTH  SIZE  START  COLS  ROWS NOTES
; 0        Std  24x24   32     0     133    24    24   C1/SB 
; 1	   Std  48x14   64     0     140    48    14   C1-II/600 RevD/UK101
; 2        Std  64x28   64     1     128    64    28   C2/C4
; 3        Tall 24x26   32     0     69     24    26   C1/SB
; 4        Full 32x28   32     0     64     32    28   Modified C1/SB
;-----------------------------------------------------------------   
; Edit the following for your desired feature/options:

MACHINE = 0	; 0 to 2      - Determines Machine hardware config
DISPLAY = 3	; 0 to 4      - Determines Video Display Parameters
EMACS   = 0     ; 0=No, 1=Yes - Enable EMACS-like Editing keys


;=================================================================
; Symbols for ROM and IO space
;=================================================================

BASIC	= $A000			; BASIC ROM
DISK	= $C000			; DISK CONTROLLER (PIA = +$00, ACIA = +$10)
SCREEN	= $D000			; SCREEN RAM
KEYBD	= $DF00			; KEYBOARD
MONITOR = $F800   		; MONITOR ROM

!IF MACHINE=0 { ACIA = $F000 }	; SERIAL PORT (MC6850 ACIA) FOR C1/Superboard
!IF MACHINE=1 { ACIA = $F000 }	; SERIAL PORT (MC6850 ACIA) FOR UK101
!IF MACHINE=2 { ACIA = $FC00 }	; SERIAL PORT (MC6850 ACIA) FOR C2/C4


;=================================================================
; BASIC ROM ROUTINES
;=================================================================
; ROM BASIC provides ACIA I/O for the C2/4, which has the ACIA at
; $FC00.  The C1 must reproduce these in the monitor for the ACIA
; at $F000.

LA34B	= BASIC + $034B		; UK101 BASIC RUBOUT KEY RETURN
LA374	= BASIC + $0374		; UK101 BASIC RUBOUT KEY RETURN
LA636	= BASIC + $0636		; CTRL-C HANDLER
LBD11	= BASIC + $1D11		; BASIC COLD START
LBF2D	= BASIC + $1F2D		; CRT SIMULATOR

LBF15	= BASIC + $1F15		; OUTPUT CHAR TO ACIA (C2/C4)
LBF22	= BASIC + $1F22		; INIT ACIA (C2/C4)


;=================================================================
; DISPLAY PARAMETERS
;=================================================================

;------ DISPLAY 0 - C1/SB

!IF DISPLAY=0 {
WIDTH	= 32			; SCREEN WIDTH
SIZE	=  0			; SCREEN SIZE 0=1K 1=2K
START	= 133			; SET SCREEN OFFSET  (Row 2, Col 5)
COLS	= 24			; SET COLUMNS TO DISPLAY
ROWS	= 24			; SET ROWS TO DISPLAY
}

;------ DISPLAY 1

!IF DISPLAY=1 {
WIDTH	=  64			; SCREEN WIDTH
SIZE	=   0			; SCREEN SIZE 0=1K 1=2K
START	= 140			; SET SCREEN OFFSET  (Row 2, Col 12)
COLS	=  48			; SET COLUMNS TO DISPLAY
ROWS	=  14			; SET ROWS TO DISPLAY
}

;------ DISPLAY 2

!IF DISPLAY=2 {
WIDTH	=  64			; SCREEN WIDTH
SIZE	=   1			; SCREEN SIZE 0=1K 1=2K
START	= 128			; SET SCREEN OFFSET (Row 2, Col 0)
COLS	=  64			; SET COLUMNS TO DISPLAY
ROWS	=  28			; SET ROWS TO DISPLAY
}

;------ DISPLAY 3

!IF DISPLAY=3 {
WIDTH	= 32			; SCREEN WIDTH
SIZE	=  0			; SCREEN SIZE 0=1K 1=2K
START	= 69			; SET SCREEN OFFSET  (Row 2, Col 5)
COLS	= 24			; SET COLUMNS TO DISPLAY
ROWS	= 26			; SET ROWS TO DISPLAY
}

;------ DISPLAY 4

!IF DISPLAY=4 {
WIDTH	= 32			; SCREEN WIDTH
SIZE	=  0			; SCREEN SIZE 0=1K 1=2K
START	= 64			; SET SCREEN OFFSET  (Row 2, Col 0)
COLS	= 32			; SET COLUMNS TO DISPLAY
ROWS	= 28			; SET ROWS TO DISPLAY
}

;------ Calculate screen parameters based on DISPLAY setting

LWIDTH	= COLS-1		; Logical line width -1
TOP	= SCREEN+START		; "HOME" position
BOT	= SCREEN+(SIZE+1)*1024	; End of physical screen RAM
BASE	= TOP+(ROWS-1)*WIDTH	; Start of Last Line of screen

;=================================================================
; Symbols for Zero Page Storage
;=================================================================

; $0F     - Terminal Width
; $13-$?? - Keyboard Buffer
; $BC-$D3 - BASIC Character Get routine
; $E4-$E5 - CEGMON Temp (ie: edit cursor, GO address, new block)
; $E7
; $F9-$FA - CEGMON Temp (screen clear)
; $FB     - LOAD Flag
; $FC-$FF - SYNMON usage


;=================================================================
; Symbols for Stack Page
;=================================================================

NMI	= $130			; NMI ADDRESS
IRQ	= $1C0			; IRQ ADDRESS

;=================================================================
; Symbols for Monitor ROM Storage
;=================================================================

CURDIS  = $0200			; Cursor Displacement on current line
OLDCHR  = $0201			; Character under Edit Cursor
NEWCHR  = $0202			; New character for
LDFLAG  = $0203			; BASIC Load Flag (0=none,FF=ACIA)
EDFLAG  = $0204			; Edit Flag (0=Off, 255=On)
SVFLAG  = $0205			; Save Flag (0=Off, 1=on)
SDELAY  = $0206			; Printing speed delay (0-255)

CCFLAG  = $0212			; CTRL-C Flag (0=enabled,1=disabled)

COUNTR  = $0214			; Auto Repeat counter for GETKEY
SCRTCH  = $0215			; ASCII value from GETKEY
LSTCHR  = $0216			; Last KEY value (for repeat test)

;------------------------------ VECTORS

INVECP  = $0218			; INPUT  Vector pointer ($FFEB/$FB46-INPUT)
OUTVEC  = $021A			; OUTPUT Vector pointer ($FFEE/$FF9B-OUTPUT)
CCVEC   = $021C			; CTRL-C Vector pointer ($FFF1/$FB94-CTRLC)
LDVEC   = $021E			; LOAD   Vector pointer ($FFF4/$FE70-SETLOD)
SVVEC   = $0220			; SAVE   Vector pointer ($FFF7/$FE7B-SETSAV)

;------------------------------ Screen WINDOW definition

SWIDTH  = $0222			; SWIDTH (column width -1)
SLTOP   = $0223			; Lo byte of TOP
SHTOP   = SLTOP+1		; Hi byte of TOP
SLBASE  = $0225			; Lo byte of BASE
SHBASE  = SLBASE+1		; Hi byte of BASE

;------------------------------ 6502 CODE
; Code is placed here and the addresses are modified

XLDA	= $0227			; $0227 > LDA TOP,X
LSRC    = $0228			; $0228 > TOP LO BYTE
HSRC    = $0229			; $0229 > TOP HI BYTE
XSTA    = $022A			; $022A > STA TOP,X
LTEXT   = $022B			; $022B > LO BYTE of text-line start
HTEXT   = LTEXT+1		; $022C > HI BYTE of text-line start
XDEX    = $022D			; $022D > DEX
XRTS    = $022E			; $022E > RTS

;------------------------------

TEMP22F = $022F			; ??? appear to not be used
TEMP230 = $0230			; ??? appear to not be used

CURSLO  = $0231			; Lo byte of cursor position
CURSHI  = CURSLO+1		; Hi byte of cursor position
USERLO  = $0233			; Lo byte of Monitor U command jump vector
USERHI  = USERLO+1		; Hi byte of Monitor U command jump vector

; $0234-$02FF is un-allocated.
; $0300 is Normal start of BASIC workspace.

;=================================================================
; Set Output File
;=================================================================

!TO "CEGMON-SJG.BIN",plain


;=================================================================
; ROM RE-MAPPING OPTION
;=================================================================
; NOTE: The original text indicated that the FCXX segment should be
; relocated to $F700 for C2/4.  To simplify the address circuitry, the
; ROM is wired to $F000-$FFFF, with A11 ignored, and the chip disabled
; when $FC00 (ACIA) is enabled.  This scheme maps the $FCXX segment to
; $F400 in hardware, so the offset is calculated as $FCXX-$F4XX.  If
; you follow the original CEGMON wiring instructions (I don't have
; these), then change the offest back to $FC00-$F700.
;

!IF MACHINE=0 { OFFSET = $0 }		; no offset needed for C1/Superboard
!IF MACHINE=1 { OFFSET = $0 }		; no offset needed for UK-101
!IF MACHINE=2 { OFFSET = $FC00-$F400 }	; $FC00-FCFF ROM MAPPED TO $F400 for C2/C4


;=================================================================
; Start of Monitor ROM
;=================================================================
; This is the target ROM address:

*=MONITOR


;=================================================================
; RUBOUT
;=================================================================

RUBOUT	LDA	$E
	BEQ	LF80A
	DEC	$E
	BEQ	LF80A
	DEC	$E
LF80A	LDA	#32				;$20 - Width of physical line
	STA	OLDCHR
	JSR	SCOUT2
	BPL	LF82D
	SEC
	LDA	LTEXT
	SBC	#32				;$20 - Width of physical line
	STA	LTEXT
	LDA	LTEXT+1
	SBC	#0
	STA	LTEXT+1
	JSR	ENDCHK
	BCS	LF82D
	JSR	CURHOME
LF82D	STX	CURDIS
	JSR	LFF88
	JMP	PULLYXA


;=================================================================
; NEW SCREEN HANDLER
;=================================================================

NSCREEN	STA	NEWCHR
	PHA
	TXA
	PHA
	TYA
	PHA
	LDA	NEWCHR
	BNE	LF846			; NOT NULL
	JMP	PULLYXA

LF846	LDY	SDELAY			; SCREEN DELAY
	BEQ	LF84E
	JSR	LFCE1-OFFSET

LF84E	CMP	#$5F			; RUBOUT character
	BEQ	RUBOUT
	CMP	#$0C			; CTRL-L ?
	BNE	LF861

;------ CTRL-L

	JSR	SCOUT
	JSR	CURHOME
	STX	CURDIS
	BEQ	LF8CF

;------ More checking

LF861	CMP	#$A			; CTRL-J
	BEQ	LINEFD

	CMP	#$1E			; CTRL Up-Arrow
	BEQ	CTRLUA

	CMP	#$B			; CTRL-K
	BEQ	CTRLK

	CMP	#$1A			; CTRL-Z (Clear Screen)
	BEQ	CTRLZ
	CMP	#$D			; CARRIAGE RETURN
	BNE	STORKEY

;------ CARRIAGE RETURN

	JSR	MODIFYX
	BNE	PULLYXA

;------ Store character for repeat comparison

STORKEY	STA	OLDCHR			; Save it

;------ CTRL-K

CTRLK	JSR	SCOUT
	INC	CURDIS
	INX
	CPX	SWIDTH
	BMI	LF8CF
	JSR	LFF70

;------ LINE FEED

LINEFD	JSR	SCOUT
	LDY	#2
	JSR	ENDCHK2
	BCS	LF89E
	LDX	#3
	JSR	NEXTLINE
	JMP	LF8CF

LF89E	JSR	LFE28
	JSR	CURHOME
	JSR	NEXTLINE
	LDX	SWIDTH
LF8AA	JSR	XLDA
	BPL	LF8AA
	INX
	JSR	NEXTLINE
	LDX	#3
	JSR	NEXTLINE
	JSR	ENDCHK
	BCC	LF8AA
	LDA	#' '
LF8BF	JSR	XSTA
	BPL	LF8BF
	LDX	#1
LF8C6	LDA	SLTOP,X
	STA	LSRC,X
	DEX
	BPL	LF8C6
LF8CF	JSR	LFF75


PULLYXA	PLA
	TAY
	PLA
	TAX
	PLA
	RTS

;------ CTRL-Z (CLEAR ENTIRE SCREEN)

CTRLZ	JSR	LFE59
	STA	OLDCHR
	BEQ	LF904

;------ CTRL-UP_ARROW / CTRL-SHIFT-N  (WINDOW CLEAR)

CTRLUA	LDA	#' '				; SPACE CHAR
	JSR	SCOUT2
	JSR	CURHOME
LF8E8	LDX	SWIDTH
	LDA	#' '				; SPACE CHAR
LF8ED	JSR	XSTA				; Write it to screen
	BPL	LF8ED
	STA	OLDCHR
	LDY	#2
	JSR	ENDCHK2
	BCS	LF904
	LDX	#3
	JSR	NEXTLINE
	JMP	LF8E8

LF904	JSR	CURHOME
	STX	CURDIS
	BEQ	PULLYXA

LF90C	JSR	TWPQAD
LF90F	JSR	CTRLF
	JSR	QDDATD
	JSR	SPCOUT
	JSR	GRTOUT
	LDX	#8				; # BYTES DISPLAYED
	STX	$FD
LF91F	JSR	SPCOUT
	JSR	LFEF0
	JSR	NOTEND
	BCS	LF97B
	JSR	BUMP
	DEC	$FD
	BNE	LF91F
	BEQ	LF90F

;------ 'M' (MOVE MEMORY)

LF933	JSR	TRIQAD
	JSR	SWAPMEM
	BCS	MSTART

;------ 'R' (RESTART FROM BREAKPOINT)

LF93B	LDX	$E4
	TXS
	LDA	$E6
	PHA
	LDA	$E5
	PHA
	LDA	$E3
	PHA
	LDA	$E0
	LDX	$E1
	LDY	$E2
	RTI

;------ 'Z' (SET BREAKPOINT)

LF94E	LDX	#3
LF950	LDA	LFA4C-1,X
	STA	IRQ-1,X
	DEX
	BNE	LF950
	JSR	LFE8D
	JSR	GETQDE
	LDA	($FE),Y
	STA	$E7
	TYA
	STA	($FE),Y
	BEQ	MSTART

;------ 'S' (SAVE)

LF968	JMP	SAVEMC

;------ 'L' (LOAD)

LF96B	DEC	$FB
	BNE	LF9E8

;------ 'T' (TABULAR DISPLAY)

LF96F	BEQ	LF90C
LF971	RTS

LF972	LDA	$FB
	BNE	LF971
	LDA	#'?'
	JSR	JUMPOUT

LF97B	LDX	#$28
	TXS

MSTART	JSR	CTRLF
	LDY	#0
	STY	$FB
	JSR	GRTOUT

;------ '.' (COMMAND/ADDRESS MODE)

LF988	JSR	LFE8D
	CMP	#'M'
	BEQ	LF933
	CMP	#'R'
	BEQ	LF93B
	CMP	#'Z'
	BEQ	LF94E
	CMP	#'S'
	BEQ	LF968
	CMP	#'L'
	BEQ	LF96B
	CMP	#'U'
	BNE	LF9D6
	JMP	($233)

;------ TWOQAD - Collect two addresses. First to FE/FF, second to F9/FA

TWPQAD	JSR	LFE8D
	JSR	GETQDE
	JSR	CMAOUT
	LDX	#0
LF9B1	JSR	LFE8D
	!BYTE	$2C

;------ GETQDE - Collect address, store in FE. NOTE: Call GETNEW first!

GETQDE
GETQDE	LDX	#5
	JSR	LF9C0
	JSR	LFE8D
	!BYTE	$2C

;------ Collect Hex Pair for data byte, store in FC. NOTE: Call GETNEW first!

GETPRC	LDX	#3
LF9C0	JSR	LF9C6
	JSR	LFE8D
LF9C6	CMP	#'.'
	BEQ	LF988
	CMP	#'/'
	BEQ	LF9E8
	JSR	ASCHEX
	BMI	LF972
	JMP	ROLSTR

LF9D6	CMP	#'T'
	BEQ	LF96F
	JSR	GETQDE

LF9DD	LDA	#'/'
	JSR	JUMPOUT
	JSR	LFEF0
	JSR	SPCOUT

;------ '/' (DATA MODE)

LF9E8	JSR	LFE8D
	CMP	#'G'
	BNE	LF9F2
	JMP	($FE)

LF9F2	CMP	#','
	BNE	LF9FC
	JSR	BUMP
	JMP	LF9E8

LF9FC	CMP	#$A
	BEQ	LFA16
	CMP	#$D
	BEQ	LFA1B
	CMP	#'^'
	BEQ	LFA21
	CMP	#$27
	BEQ	LFA3A
	JSR	GETPRC
	LDA	$FC
	STA	($FE),Y
LFA13	JMP	LF9E8

LFA16	LDA	#$D
	JSR	JUMPOUT

LFA1B	JSR	BUMP
	JMP	LFA31

;------ '^'

LFA21	SEC
	LDA	$FE
	SBC	#1
	STA	$FE
	LDA	$FF
	SBC	#0
	STA	$FF

DATALN	JSR	CTRLF

LFA31	JSR	QDDATD
	JMP	LF9DD

LFA37	JSR	LFEF7

;------ "'"

LFA3A	JSR	LFE8D
	CMP	#$27
	BNE	LFA46
	JSR	CMAOUT
	BNE	LFA13
LFA46	CMP	#$D
	BEQ	DATALN
	BNE	LFA37

LFA4C	JMP	LFA4F

LFA4F	STA	$E0
	PLA
	PHA
	AND	#$10
	BNE	LFA5A
	LDA	$E0
	RTI

;------ SAVE REGISTERS ON BREAK

LFA5A	STX	$E1
	STY	$E2
	PLA
	STA	$E3
	CLD
	SEC
	PLA
	SBC	#2
	STA	$E5
	PLA
	SBC	#0
	STA	$E6
	TSX
	STX	$E4			; **BUG FIX** (ORIGINAL VALUE WAS $E1)
	LDY	#0
	LDA	$E7
	STA	($E5),Y
	LDA	#$E0
	STA	$FE
	STY	$FF
	BNE	DATALN

SAVEMC	JSR	TRIQAD
	JSR	JUMPSV
	JSR	LFEE9
	JSR	JUMPOUT
	JSR	PERIOD
	LDA	#'/'
	JSR	JUMPOUT
	BNE	LFA97
LFA94	JSR	BUMP

LFA97	JSR	LFEF0
	LDA	#$D
	JSR	ACIAOUT-OFFSET
	JSR	NOTEND
	BCC	LFA94
	LDA	$E4
	LDX	$E5
	STA	$FE
	STX	$FF
	JSR	PERIOD
	LDA	#'G'
	JSR	JUMPOUT
	JSR	TENULL
	STY	SVFLAG
	JMP	MSTART

;=================================================================
; ENTRY TO SCREEN EDITOR
;=================================================================

EDITOR	TXA
	PHA
	TYA
	PHA
	LDA	EDFLAG
	BPL	LFB1F
EDLOOP	LDY	TEMP22F
	LDA	CURSLO
	STA	$E4
	LDA	CURSLO+1
	STA	$E5
	LDA	($E4),Y
	STA	TEMP230
	LDA	#$A1
	STA	($E4),Y
	JSR	GETKEY
	LDA	TEMP230
	STA	($E4),Y

;------ Test for Cursor Movement A/S/D/F and Q/ESC to Copy

CHKEDIT	LDA	SCRTCH

!IF EMACS=0 {
!IF MACHINE=0 {	CMP #$1B }			; ESC    (copy)?
!IF MACHINE=1 {	CMP #$11 }			; CTRL-Q (copy)?
!IF MACHINE=2 {	CMP #$1B }			; ESC    (copy)?
	BEQ	LFB13

	CMP	#1				; CTRL-A (left)?
	BEQ	LFB0D
	CMP	#4				; CTRL-D (right)?
	BEQ	LFB07
	CMP	#$13				; CTRL-S (up)?
	BEQ	LFB01
	CMP	#6				; CTRL-F (down)?
	BNE	LFB22
} ELSE {
	CMP	#$19				; CTRL-Y (yank)
	BEQ	LFB13
	CMP	#2				; CTRL-B (backwards)?
	BEQ	LFB0D
	CMP	#6				; CTRL-F (forwards)?
	BEQ	LFB07
	CMP	#$10				; CTRL-P (up)?
	BEQ	LFB01
	CMP	#E				; CTRL-N (next line)?
	BNE	LFB22
}
;------ CTRL-F (cursor down)

	JSR	LFB7C
	JMP	EDLOOP

;------ CTRL-S (cursor up)

LFB01	JSR	LFE28
	JMP	EDLOOP

;------ CTRL-D (cursor right)

LFB07	JSR	LFB6B
	JMP	EDLOOP

;------ CTRL-A (cursor left)

LFB0D	JSR	LFE19
	JMP	EDLOOP

;------ CTRL-Q / ESC (copy character at edit cursor)

LFB13	LDA	TEMP230
	STA	SCRTCH
	JSR	LFB6B
	JMP	LFB43

LFB1F	JSR	GETKEY

;------ Check for CTRL-E
LFB22	CMP	#5
	BNE	LFB43

;------ CTRL-E (toggle EDIT MODE)

	LDA	EDFLAG
	EOR	#$FF
	STA	EDFLAG
	BPL	LFB1F
	LDA	LTEXT
	STA	CURSLO
	LDA	LTEXT+1
	STA	CURSLO+1
	LDX	#0
	STX	TEMP22F
	BEQ	EDLOOP
LFB43	JMP	LFDD3

;=================================================================
; BASIC INPUT ROUTINE
;=================================================================
; Get a character from keyboard or ACIA

INPUT	BIT	LDFLAG
	BPL	LFB68	; LOAD FLAG CLR
LFB4B	LDA	#$FD
	STA	KEYBD
	LDA	#$10
	BIT	KEYBD
	BEQ	LFB61	; SPACE KEY PRESSED

;=================================================================
; INPUT FROM ACIA
;=================================================================

TAPIN	LDA	ACIA
	LSR	
	BCC	LFB4B
	LDA	ACIA+1
	RTS

LFB61	LDA	#0
	STA	$FB
	STA	LDFLAG
LFB68	JMP	EDITOR

LFB6B	LDX	SWIDTH
	CPX	TEMP22F
	BEQ	LFB77
	INC	TEMP22F
	RTS

LFB77	LDX	#0
	STX	TEMP22F
LFB7C	CLC
	LDA	CURSLO
	ADC	#WIDTH 				; Physical line width
	STA	CURSLO
	LDA	CURSLO+1
	ADC	#0
	CMP	#>BOT 				; HI byte of BOTTOM OF SCREEN
	BNE	LFB90
	LDA	#>SCREEN			; HI Byte of start of SCREEN
LFB90	STA	CURSLO+1
LFB93	RTS

;=================================================================
; BASIC CTRL-C CHECK
;=================================================================

CTRLC	LDA	CCFLAG
	BNE	LFB93	; DISABLE FLAG SET
	LDA	#$FE
	STA	KEYBD
	BIT	KEYBD
	BVS	LFB93
	LDA	#$FB
	STA	KEYBD
	BIT	KEYBD
	BVS	LFB93
	LDA	#3	; CTRL-C PRESSED
	JMP	LA636

;=================================================================
; TABLE to setup screen parameters and vectors
;=================================================================

SETUPTBL	!WORD	INPUT	; 218 INPUT
	!WORD	OUTPUT	; 21A OUTPUT
	!WORD	CTRLC	; 21C CTRL-C
	!WORD	LFE70	; 21E LOAD
	!WORD	LFE7B	; 220 SAVE
	!BYTE	LWIDTH	; 222
	!WORD	TOP	; 223
	!WORD	BASE	; 225

	LDA	TOP,X	; 227  <-- Start of a small subroutine
	STA	TOP,X	; 22A  copied to low memory and executed
	DEX		; 22D  there.
	RTS		; 22E  <-- END

	!BYTE	$00	; 22F
	!BYTE	$20	; 230
	!WORD	TOP	; 231
	!WORD	LF988	; 233

;=================================================================
; Check if top or base of screen overshot
;=================================================================

ENDCHK	LDX	SWIDTH
ENDCHK2	SEC
	LDA	LTEXT
	SBC	SLTOP,Y
	LDA	LTEXT+1
	SBC	SLTOP+1,Y
	RTS

;=================================================================
; Output a ">" , "," or " "
;=================================================================

GRTOUT	LDA	#'>'
	!BYTE	$2C
CMAOUT	LDA	#','
	!BYTE	$2C
SPCOUT	LDA	#' '
	JMP	JUMPOUT			; Print it

;=================================================================
; NOTEND Compare FE with F9, Set CARRY CLEAR if FE is less
;=================================================================

NOTEND	SEC
	LDA	$FE
	SBC	$F9
	LDA	$FF
	SBC	$FA
	RTS

;=================================================================
; CRLF - Print CR and LF to display
;=================================================================

CTRLF	LDA	#$D
	JSR	JUMPOUT
	LDA	#$A
	JMP	JUMPOUT

	!BYTE	$40

;=================================================================
; FLOPPY DISK BOOTSTRAP
;=================================================================
; **** This entry point must not move!

BOOTSTRP
	JSR	LFC0C-OFFSET
	JMP	($FD)

	JSR	LFC0C-OFFSET
	JMP	NEWMON

LFC0C	LDY	#0
	STY	DISK+1
	STY	DISK
	LDX	#4
	STX	DISK+1
	STY	DISK+3
	DEY
	STY	DISK+2
	STX	DISK+3
	STY	DISK+2
	LDA	#$FB
	BNE	LFC33

LFC2A	LDA	#2
	BIT	DISK
	BEQ	LFC4D
	LDA	#$FF
LFC33	STA	DISK+2
	JSR	LFCA5-OFFSET
	AND	#$F7
	STA	DISK+2
	JSR	LFCA5-OFFSET
	ORA	#8
	STA	DISK+2
	LDX	#$18
	JSR	LFC91-OFFSET
	BEQ	LFC2A
LFC4D	LDX	#$7F
	STX	DISK+2
	JSR	LFC91-OFFSET
LFC55	LDA	DISK
	BMI	LFC55
LFC5A	LDA	DISK
	BPL	LFC5A
	LDA	#3
	STA	DISK+$10
	LDA	#$58
	STA	DISK+$10
	JSR	DISKIN-OFFSET
	STA	$FE
	TAX
	JSR	DISKIN-OFFSET
	STA	$FD
	JSR	DISKIN-OFFSET
	STA	$FF
	LDY	#0
LFC7B	JSR	DISKIN-OFFSET
	STA	($FD),Y
	INY
	BNE	LFC7B
	INC	$FE
	DEC	$FF
	BNE	LFC7B
	STX	$FE
	LDA	#$FF
	STA	DISK+2
	RTS

LFC91	LDY	#$F8
LFC93	DEY
	BNE	LFC93
	EOR	$FF,X
	DEX
	BNE	LFC91
	RTS

;------ INPUT CHAR FROM DISK

DISKIN	LDA	DISK+$10
	LSR
	BCC	DISKIN
	LDA	DISK+$11
LFCA5	RTS

;------ INIT ACIA

LFCA6	LDA	#3			; RESET ACIA
	STA	ACIA

!IF MACHINE=0 {	LDA #$11 }		; /16, 8BITS, 2STOP, RTS LOW
!IF MACHINE=1 { LDA #$11 }		; /16, 8BITS, 2STOP, RTS LOW
!IF MACHINE=2 { LDA #$B1 }		; /16, 8BITS, 2STOP, RTS LOW, RX INT

	STA	ACIA
	RTS

;------ OUTPUT CHAR TO ACIA

ACIAOUT	PHA
LFCB2	LDA	ACIA
	LSR	
	LSR	
	BCC	LFCB2
	PLA
	STA	ACIA+1
	RTS

;=================================================================
; KEYWRT - Write to KEYBOARD with INVERT 
;=================================================================
; ROW (A)  1=R0, 2=R1, 4=R2 ETC

KEYWRT	EOR	#$FF
	STA	KEYBD
	EOR	#$FF
	RTS

;=================================================================
; KEY2XR - Read KEYBOARD with INVERT
;=================================================================
;COL (X) 1=C0, 2=C1, 4=C2, 0=NONE

KEY2XR	PHA
	JSR	KYREAD-OFFSET
	TAX
	PLA
	DEX
	INX
	RTS

;=================================================================
; KYREAD - Read KEYBOARD with INVERT
;=================================================================

KYREAD	LDA	KEYBD
	EOR	#$FF
	RTS

;------ UK101 BASIC ROM RUBOUT KEY HANDLER

LFCD5	CMP	#$5F	; RUBOUT
	BEQ	LFCDC
	JMP	LA374

LFCDC	JMP	LA34B

;=================================================================
; KDELAY - Delay for keyboard read. Set X=0,Y=0
;=================================================================

KDELAY	LDY	#$10

DELAY2
LFCE1	LDX	#$40
LFCE3	DEX
	BNE	LFCE3
	DEY
	BNE	LFCE1
	RTS


;=================================================================
; START-UP BANNER and PROMPT
;=================================================================

BANNER	!TEXT "CEGMON(C)1980 D/C/W/M?"


;=================================================================
; POLLED KEYBOARD INPUT ROUTINE
;=================================================================
; This routine must start at $FD00 !

* = $FD00
GETKEY
GETKEY	TXA
	PHA
	TYA
	PHA

LFD04	LDA	#$80			; ROW 7
LFD06	JSR	KEYWRT			; SET ROW
	JSR	KEY2XR			; READ COL
	BNE	LFD13			; KEY PRESS

	LSR				; NEXT ROW
	BNE	LFD06
	BEQ	LFD3A

LFD13	LSR	
	BCC	LFD1F
	TXA
	AND	#$20
	BEQ	LFD3A
	LDA	#$1B
	BNE	LFD50

LFD1F	JSR	LFE86
	TYA
	STA	SCRTCH
	ASL	
	ASL	
	ASL	
	SEC
	SBC	SCRTCH
	STA	SCRTCH
	TXA
	LSR	
	ASL	
	JSR	LFE86
	BEQ	LFD47
	LDA	#0
LFD3A	STA	LSTCHR
LFD3D	STA	$213
	LDA	#2
	STA	COUNTR
	BNE	LFD04

LFD47	CLC
	TYA
	ADC	SCRTCH
	TAY
	LDA	KMATRIX-1,Y

LFD50	CMP	$213
	BNE	LFD3D
	DEC	COUNTR
	BEQ	LFD5F
	JSR	KDELAY-OFFSET
	BEQ	LFD04

LFD5F	LDX	#$64
	CMP	LSTCHR
	BNE	LFD68
	LDX	#$F
LFD68	STX	COUNTR
	STA	LSTCHR
	CMP	#$21
	BMI	LFDD0

	CMP	#$5F
	BEQ	LFDD0

	LDA	#1
	JSR	KEYWRT
	JSR	KYREAD
	STA	SCRTCH
	AND	#1
	TAX
	LDA	SCRTCH
	AND	#6
	BNE	LFDA2
	BIT	$213
	BVC	LFDBB
	TXA
	EOR	#1
	AND	#1
	BEQ	LFDBB
	LDA	#$20
	BIT	SCRTCH
	BVC	LFDC3
	LDA	#$C0
	BNE	LFDC3

LFDA2	BIT	$213
	BVC	LFDAA
	TXA
	BEQ	LFDBB
LFDAA	LDY	$213
	CPY	#$31
	BCC	LFDB9
	CPY	#$3C
	BCS	LFDB9
	LDA	#$F0
	BNE	LFDBB

LFDB9	LDA	#$10
LFDBB	BIT	SCRTCH
	BVC	LFDC3
	CLC
	ADC	#$C0
LFDC3	CLC
	ADC	$213
	AND	#$7F
	BIT	SCRTCH
	BPL	LFDD0
	ORA	#$80
LFDD0	STA	SCRTCH
LFDD3	PLA
	TAY
	PLA
	TAX
	LDA	SCRTCH
	RTS

LFDDB	JSR	BUMP
	INC	$E4
	BNE	SWAPMEM
	INC	$E5

;------ SWAP Memory block move
;SWAP
SWAPMEM	LDA	($FE),Y
	STA	($E4),Y
	JSR	NOTEND
	BCC	LFDDB
	RTS

;------ Move to next line on screen
NEXTLINE	CLC
	LDA	#WIDTH 			; Physical line width
	ADC	LSRC,X
	STA	LSRC,X
	LDA	#0
	ADC	LSRC+1,X
	STA	LSRC+1,X
	RTS

;=================================================================
; 65V MONITOR
;=================================================================
; Thir routine must start at $FE00

* = $FE00

NEWMON	LDX	#$28
	TXS
	CLD
	JSR	LFCA6
	JSR	LFE40
	NOP
	NOP
MENTRY
	JSR	LFE59
	STA	OLDCHR
	STY	$FE
	STY	$FF
	JMP	MSTART

LFE19	LDX	TEMP22F
	BEQ	LFE22
	DEC	TEMP22F
	RTS

LFE22	LDX	SWIDTH
	STX	TEMP22F

LFE28	SEC
	LDA	CURSLO
	SBC	#WIDTH 			; Physical line width
	STA	CURSLO
	LDA	CURSLO+1
	SBC	#0
	CMP	#>SCREEN-1 		; HI Byte of start of SCREEN
	BNE	LFE3C
	LDA	#>BOT-1 		; HI byte of BOTTOM OF SCREEN
LFE3C	STA	CURSLO+1
	RTS

LFE40	LDY	#$1C			; INIT 218-234
LFE42	LDA	SETUPTBL,Y		; Read from Screen and Vector SETUP Table
	STA	INVECP,Y		; Write it
	DEY
	BPL	LFE42
	LDY	#7			; ZERO 200-206, 212
	LDA	#0
	STA	CCFLAG			; ENABLE CTRL-C FLAG
LFE52	STA	CURDIS-1,Y
	DEY
	BNE	LFE52
	RTS

;------ CLEAR SCREEN

SCNCLR
LFE59	LDY	#0
	STY	$F9
	LDA	#>SCREEN		; HI Byte of start of SCREEN
	STA	$FA
	LDX	#(SIZE+1)*4 		;8
	LDA	#' '
LFE65	STA	($F9),Y
	INY
	BNE	LFE65
	INC	$FA
	DEX
	BNE	LFE65
	RTS

;------ LOAD

LFE70	PHA
	DEC	LDFLAG			; SET LOAD FLAG
	LDA	#0			; CLR SAVE FLAG
LFE76	STA	SVFLAG
	PLA
	RTS

;------ SAVE

LFE7B	PHA
	LDA	#1			; SET SAVE FLAG
	BNE	LFE76

;=================================================================
; INPUT CHAR FROM ACIA
;=================================================================

MCACIA	JSR	TAPIN
	AND	#$7F			; CLEAR BIT 7
	RTS

LFE86	LDY	#8
LFE88	DEY
	ASL	
	BCC	LFE88
	RTS

LFE8D	JSR	LFEE9
	JMP	JUMPOUT

;=================================================================
; CONVERT ASCII-HEX CHAR TO BINARY
;=================================================================

ASCHEX	CMP	#'0'
	BMI	LFEA9
	CMP	#'9'+1
	BMI	LFEA6
	CMP	#'A'
	BMI	LFEA9
	CMP	#'F'+1
	BPL	LFEA9
	SEC
	SBC	#7
LFEA6	AND	#$F
	RTS

LFEA9	LDA	#$80
	RTS

;------ Print address in FE, space, value in FC to display

ADVTOD	JSR	QDDATD
	NOP
	NOP
	JSR	SPCOUT
	BNE	PRDATD

;------ Print address in FE to display

QDDATD	LDX	#3
	JSR	PRDATD2
	DEX
	!BYTE	$2C

;------ Print Data Byte in FC to display

PRDATD	LDX	#0

PRDATD2	LDA	$FC,X
	LSR	
	LSR	
	LSR	
	LSR	
	JSR	HEXOUT
	LDA	$FC,X

;------ Strip byte in A to lower nibble, print it as ASCII hex to display

HEXOUT	AND	#$F
	ORA	#'0'
	CMP	#'9'+1
	BMI	LFED5
	CLC
	ADC	#7
LFED5	JMP	JUMPOUT

	!BYTE	$EA,$EA

;------ Roll new nibble into FE if X=2, or into FC if X=0

ROLSTR	LDY	#4
	ASL	
	ASL	
	ASL	
	ASL	
LFEE0	ROL	
	ROL	$F9,X
	ROL	$FA,X
	DEY
	BNE	LFEE0
	RTS

LFEE9	LDA	$FB
	BNE	MCACIA
	JMP	GETKEY

;------ Print data at current address, pointed to by FE to display. Assume Y=0

PRBYTE
LFEF0	LDA	($FE),Y
	STA	$FC
	JMP	PRDATD

LFEF7	STA	($FE),Y

;------ Increment current address at FE

BUMP	INC	$FE
	BNE	LFEFF
	INC	$FF
LFEFF	RTS


;=================================================================
; POWER ON RESET
;=================================================================

RESET	CLD
	LDX	#$28
	TXS
	JSR	LFCA6
	JSR	LFE40
	JSR	LFE59
	STY	CURDIS			;JSR	CURHOME

;------ Display Power-on Banner ("CEGMON D/C/W/M?")

BANLOOP	LDA	BANNER,Y		; Prompt
	JSR	JUMPOUT			; Print the character
	INY				; next character
	CPY	#$16			; Are we done?
	BNE	BANLOOP			; No, loop for more

;------ Wait for User Selection

	JSR	JUMPIN			; Accept a key
	AND	#$DF			; strip off uppercase bits

	CMP	#'D'			; Is it "D" (Disk Boot)?
	BNE	LFF27
	JMP	BOOTSTRP		; Yes, do it

LFF27	CMP	#'M'			; Is it "M" (Monitor)?
	BNE	LFF2E
	JMP	NEWMON			; Yes, do it

LFF2E	CMP	#'W'			; Is it "W" (Warm Start)?
	BNE	LFF35
	JMP	0			; Yes, do it

LFF35	CMP	#'C'			; Is it "C" (Cold Start)?
	BNE	RESET			; No, do a RESET
	JMP	LBD11			; Yes, do it


;=================================================================
; KEYBOARD MATRIX
;=================================================================
; The keyboard is organized as an 8x8 matrix of keys. In the main
; matrix only 7 keys are used on each ROW.
; The special modifier keys (RPT,CTRL,ESC,Shifts, and ShiftLock)
; are not included in the matrix and are handled separately.
; Because of this, the table below is a matrix of 7x7.

KMATRIX	!TEXT "P;/ ZAQ"
	!TEXT ",MNBVCX"
	!TEXT "KJHGFDS"
	!TEXT "IUYTREW"
	!BYTE $00,$00,$0D,$0A
	!TEXT "OL."
	!BYTE $00,$5F
	!TEXT "-:098"
	!TEXT "7654321"


;=================================================================
; MODIFY INSTRUCTIONS in XSTA routine
;=================================================================

MODIFYX	JSR	SCOUT

LFF70	LDX	#0
	STX	CURDIS
LFF75	LDX	CURDIS
	LDA	#$BD			; "LDA" instruction (LDA ABS,X)
	STA	XSTA
	JSR	XSTA
	STA	OLDCHR
	LDA	#$9D			; "STA" instruction (STA ABS,X)
	STA	XSTA
LFF88	LDA	#$5F
	BNE	SCOUT2

;=================================================================
; SCOUT - Print character at cursor position
;=================================================================

SCOUT	LDA	OLDCHR
SCOUT2	LDX	CURDIS
	JMP	XSTA

;=================================================================
; OLD SCREEN
;=================================================================
; Use BASIC's screen handler at $BF2D

OLDSCR	JSR	LBF2D
	JMP	LFF9E

;=================================================================
; OUTPUT
;=================================================================
; General output to screen or ACIA

OUTPUT	JSR	NSCREEN
LFF9E	PHA
	LDA	SVFLAG
	BEQ	LFFBB	; SAVE FLAG CLR
	PLA
	JSR	ACIAOUT	; CHAR TO ACIA
	CMP	#$D
	BNE	LFFBC	; NOT CR

;------ 10 NULLS

TENULL	PHA
	TXA
	PHA
	LDX	#$A
	LDA	#0
LFFB3	JSR	ACIAOUT-OFFSET
	DEX
	BNE	LFFB3
	PLA
	TAX
LFFBB	PLA
LFFBC	RTS

;=================================================================
; TRIQAD - Collect 3 addresses to FE/FF, F9/FA, E4/E5
;=================================================================

TRIQAD	JSR	TWPQAD
	JSR	GRTOUT
	LDX	#3
	JSR	LF9B1
	LDA	$FC
	LDX	$FD
	STA	$E4
	STX	$E5
	RTS

;=================================================================
; SET DEFAULT WINDOW AND DO CURSOR HOME
;=================================================================

CURHOME	LDX	#2
CHLOOP	LDA	SLTOP-1,X
	STA	LSRC-1,X
	STA	LTEXT-1,X
	DEX
	BNE	CHLOOP
	RTS

;=================================================================
; TABLE for BASIC ROM SCREEN PARAMETERS
;=================================================================
; These bytes must not moved! They are used for BASIC printing

LFFE0	!BYTE	<BASE		; CURSOR START
LFFE1	!BYTE	LWIDTH		; LINE LENGTH - 1
LFFE2	!BYTE	SIZE		; SCREEN SIZE (0=1K 1=2K)

;=================================================================
; Print a PERIOD
;=================================================================

PERIOD	LDA	#'.'
	JSR	JUMPOUT
	JMP	QDDATD

;=================================================================
; JUMP TABLE
;=================================================================

JUMPIN	JMP	(INVECP)		; INPUT  FB46
JUMPOUT	JMP	(OUTVEC)		; OUTPUT FF9B
JUMPCC	JMP	(CCVEC)			; CTRL-C FB94
JUMPLD	JMP	(LDVEC)			; LOAD   FE70
JUMPSV	JMP	(SVVEC)			; SAVE   FE7B

;=================================================================
; 6502 RESET, IRQ, and NMI Vectors
;=================================================================

!WORD	NMI			; NMI   (normally not used)
!WORD	RESET			; RESET
!WORD	IRQ			; IRQ   (normally not used)
