;=================================================================
; CEGMON MONITOR REVERSE ENGINEERED SOURCE CODE
;=================================================================
; CEGMON was written by George Chkiantz, Richard Elen, and Tom Graves
;
; CEGMON is a monitor ROM for Ohio Scientific and compatible
; computers, including C1/Superboard, C2/C4 and UK-101.
;
; BASED ON BINARY IMAGE FILE: "CEGMONC1.ROM"
;
; This disassembly started by unknown author ("Dave"?).
; Found on OSIWEB.ORG site.
; Adapted, Commented, and Enhanced by Steve J. Gray, Apr 25, 2019
; Formatted for ACME assembler.

;=================================================================
; CUSTOMIZABLE
;=================================================================
; This file "cegmon-main.asm" requires that specific VARIABLES be
; set to select different features. Creat an ASM file with your
; settings and then include this file at the bottom.

;=================================================================
; Features
;=================================================================
;
; CTRL-J CHR$(10) - Cursor DOWN (line feed)
; CTRL-K CHR$(11) - Cursor RIGHT
; CTRL-L CHR$(12) - Cursor HOME (top left of window)
; CTRL-M CHR$(13) - Cursor to start of next line (carriage return)
; CTRL-Z CHR$(26) - Clear Entire Screen
; CTRL-^ CHR$(30) - Clear Window (CTRL-SHIFT-N)
;
; ----- Editor
;
; NORMAL  EMACS
; CTRL-E  CTRL-E - Toggle Editor
; CTRL-A  CTRL-B - Edit Cursor LEFT/BACK
; CTRL-S  CTRL-P - Edit Cursor UP/PREVIOUS LIN
; CTRL-D  CTRL-F - Edit Cursor RIGHT/FORWARD
; CTRL-F  CTRL-N - Edit Cursor DOWN/NEXT LINE
; ESC     CTRL-Y - COPY/YANK character at Edit Cursor (CTRL-Q on UK-101)
;
; ----- Machine Code Monitor
;
; COMMAND/ADDRESS MODE		DATA MODE
; --------------------		----------
; / - Jump to Data Mode		. - Return to Address Mode
; L - Load			/ - Re-open current address
; S - Save			G - Start execution at current
; M - Memory Move		' - Enter Text entry loop
; T - Tabular memory display	LF- Increment current address
; Z - Set a breakpoint		CR- Increment on current line
; R - Restart from Breakpoint   ^ - Decrement current address
; U - Jump to User routine


;=================================================================
; Symbols for ROM and IO space
;=================================================================
; Assume Superboard Memory Map

XROM    = $8000			; EXTENDED ROM SPACE (BASIC EXTENSIONS etc)
BASIC	= $A000			; BASIC ROM
DISK	= $C000			; DISK CONTROLLER (PIA = +$00, ACIA = +$10)
SCREEN	= $D000			; SCREEN RAM
COLOUR  = $D400			; COLOUR RAM ($D400-D7FF - with 630 board)
KEYBD	= $DF00			; KEYBOARD
SCDREG  = $D800			; Screen, Colour, DAC Control Register
ACIA	= $F000			; SERIAL PORT (MC6850 ACIA) FOR C1/Superboard
I630REG = $F7E0			; 630 IO/Colour board Control Register
MONITOR = $F800   		; MONITOR ROM

; Change as appropriate for other machines

!IF MACHINE=2 {
ACIA	= $FC00			; SERIAL PORT (MC6850 ACIA) FOR C2/C4
COLOUR	= $E000			; 540B COLOUR RAM ($E000-$E7FF)
}


;=================================================================
; BASIC ROM ROUTINES
;=================================================================
; ROM BASIC provides ACIA I/O for the C2/4, which has the ACIA at
; $FC00.  The C1 must reproduce these in the monitor for the ACIA
; at $F000.

BROMWARM = BASIC + $0274	; UK101 BASIC WARM START
BROMRUB1 = BASIC + $034B	; UK101 BASIC RUBOUT KEY RETURN
BROMRUB2 = BASIC + $0374	; UK101 BASIC RUBOUT KEY RETURN
BROMCC   = BASIC + $0636	; CTRL-C HANDLER
BROMCOLD = BASIC + $1D11	; BASIC COLD START
BROMCRTC = BASIC + $1F2D	; CRT SIMULATOR

BROMA084 = BASIC + $0084	; ?
BROMA359 = BASIC + $0359	; ?
BROMA374 = BASIC + $0374	; ?
BROMA636 = BASIC + $0636	; ?
BROMA866 = BASIC + $0866	; ?

BROMAFC1 = BASIC + $0FC1     	; ?
BROMB96E = BASIC + $196E	; ?
BROMBCF7 = BASIC + $1CF7	; ?

; BROMAOUT = BASIC + $1F15	; OUTPUT CHAR TO ACIA (C2/C4)
; BROMAINI = BASIC + $1F22	; INIT ACIA (C2/C4)

;=================================================================
; Symbols for Zero Page Storage
;=================================================================

;NULLS	= $0D			; $0D - Number of NULL's
CHRCOUNT= $0E			; $0E - Terminal Character Count
TERMWID	= $0F			; $0F - BASIC Terminal Width (not used)
;ZP10	= $10			; $10 - BASIC Terminal Width columns
INBUF	= $13			; $13-$5A - Input Buffer

MLMREGA	= $E0			; $E0 - MLM "A" Register
MLMREGX	= $E1			; $E1 - MLM "X" Register
MLMREGY	= $E2			; $E2 - MLM "Y" Register
MLMREGP	= $E3			; $E3 - MLM "P" Register
MLMSP	= $E4			; $E4 - MLM Stack Pointer
MLMPCLO	= $E5			; $E5 - MLM PC Counter LO
MLMPCHI	= $E6			; $E6 - MLM PC Counter HI
;ZPE7	= $E7			; $E7 - not used
;ZPE8	= $E8			; $E8 - not used
CEGPTRLO= $F9			; $F9 - CEGMON Temp (screen clear)
CEGPTRHI= $FA			; $FA - CEGMON Temp (screen clear)
LOADFLAG= $FB     		; $FB - Monitor LOAD Flag
MLMBYTE	= $FC			; $FC - Monitor Contents of current memory location
GENSTORE= $FD			; $FD - General Storage
GENPTRLO= $FE			; $FE - General Pointer LO
GENPTRHI= $FF			; $FF - General Pointer HI

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

MEM213  = $0213			; Temp storage for keyboard routine
COUNTR  = $0214			; Auto Repeat counter for GETKEY
TMPCHR  = $0215			; ASCII value from GETKEY
LSTCHR  = $0216			; Last KEY value (for repeat test)

;------------------------------ VECTORS

INVECP  = $0218			; INPUT  Vector pointer ($FFEB/$FB46-INPUT)
OUTVEC  = $021A			; OUTPUT Vector pointer ($FFEE/$FF9B-OUTPUT)
CCVEC   = $021C			; CTRL-C Vector pointer ($FFF1/$FB94-CTRLC)
LDVEC   = $021E			; LOAD   Vector pointer ($FFF4/$FE70-SETLOD)
SVVEC   = $0220			; SAVE   Vector pointer ($FFF7/$FE7B-SETSAV)

;------------------------------ Screen WINDOW definition

SWIDTH  = $0222			; Column width -1
SLTOP   = $0223			; Lo byte of TOP
SHTOP   = SLTOP+1		; Hi byte of TOP
SLBASE  = $0225			; Lo byte of BASE
SHBASE  = SLBASE+1		; Hi byte of BASE

;------------------------------ 6502 CODE
; Code is placed here and the addresses are modified

XLDA	= $0227			; $0227 > LDA opcode - LDA TOP,X
LSRC    = $0228			; $0228 > TOP LO BYTE (this gets modified)
HSRC    = $0229			; $0229 > TOP HI BYTE (this gets modified)
XSTA    = $022A			; $022A > STA opcode - STA LTEXT,X
LTEXT   = $022B			; $022B > TEXT LO BYTE (this gets modified)
HTEXT   = LTEXT+1		; $022C > TEXT HI BYTE (this gets modified)
XDEX    = $022D			; $022D > DEX opcode
XRTS    = $022E			; $022E > RTS opcode

EDISP	= $022F			; "DISP" Edit Cursor displacement from start of current line
CURCHR	= $0230			; Character beneath Edit Curstor
CURSLO  = $0231			; Lo byte of cursor position
CURSHI  = CURSLO+1		; Hi byte of cursor position
USERLO  = $0233			; Lo byte of Monitor U command jump vector
USERHI  = USERLO+1		; Hi byte of Monitor U command jump vector

;------------------------------ Custom Locations
!IF OPTWIDTH=1 {
PWIDTH = $FD			; Pysical screen width
SHADOW = $0234			; Shadow SCD Register (Screen,Colour,DAC)
}

; $0234-$02FF is un-allocated.
; $0300 is Normal start of BASIC workspace.


;=================================================================
; DISPLAY PARAMETERS
;=================================================================

;------ DISPLAY 0

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
; [$8000] EXTERNAL ROM OPTION
;=================================================================
; This option allows you to put additional monitor ROM functions
; into a separate ROM. Typically this would be in the area from
; $8000-$9FFF which is where BASIC extensions would reside
; NOTE: When ACME assembles the code you will get ONE file with
; all code starting from $8000 and ending at $FFFF. You will need
; to manually split this file into two separate ROM binaries.

!IF OPTXROM>0 {
*=XROM
	!IF OPTXROM=1 { !SOURCE "cegmon-xdiskbootstrap.asm" }		; Include DISK BOOTSTRAP
	!IF OPTXROM=2 { !SOURCE "cegmon-xrevd.asm" }			; Include 600 REV D screen and keyboard code
	!IF OPTXROM=3 { !SOURCE "cegmon-xmonselect.asm" }		; Include Monitor ROM selection Menu
	!IF OPTXROM=4 { !SOURCE "cegmon-xbasselect.asm" }		; Include BASIC ROM selection Menu
}


;#################################################################
; [$F800] Start of Monitor ROM
;#################################################################

; CEGMON is a 2K Monitor ROM with start address of $F800.

*=MONITOR


;=================================================================
; [$F800] RUBOUT
;=================================================================

RUBOUT	LDA	CHRCOUNT	; Terminal Character Count
	BEQ	RUBOUT2		; Is it 0? Yes,
	DEC	CHRCOUNT	; Subtract 1
	BEQ	RUBOUT2		; Is it 0? Yes,
	DEC	CHRCOUNT	; Subtract 1

RUBOUT2	LDA	#32		; SPACE Character
	STA	OLDCHR		; Put the SPACE character
	JSR	SCOUT2		; where the cursor character was
	BPL	RUBDONE		; Are we at start of line? No, jump ahead

;------ Move to previous line

	SEC
	LDA	LTEXT		; Position of cursor LO

!IF OPTWIDTH=0 {	SBC #WIDTH }	; Subtract width of physical line (calculated constant)
!IF OPTWIDTH=1 {	SBC PWIDTH }	; Subtract width of physical line (from memory)

	STA	LTEXT		; Store it
	LDA	LTEXT+1		; Get position of cursor HI
	SBC	#0		; subtract the carry bit
	STA	LTEXT+1		; Store it
	JSR	ENDCHK		; Is it above top screen boundary?
	BCS	RUBDONE		; No, skip ahead
	JSR	CURHOME		; HOME the cursor
RUBDONE	STX	CURDIS
	JSR	MODIFY2		; Set pointers
	JMP	PULLYXA		; Restore registers and we're done


;=================================================================
; [$F836] NEW SCREEN HANDLER
;=================================================================

NSCREEN	STA	NEWCHR		; Save character
	PHA			; Save registers to stack
	TXA
	PHA
	TYA
	PHA
	LDA	NEWCHR		; Load character
	BNE	NSCR1		; Check if key pressed. If so jump ahead
	JMP	PULLYXA		; Restore registers and we're done


;=================================================================
; [$F846] NEW SCREEN KEY HANDLER
;=================================================================
; This is where the special keys are handled for NORMAL printing mode
; (ie: NOT Edit Mode). If no special keys are found then the key is
; assumed to be printable.

NSCR1	LDY	SDELAY		; SCREEN DELAY
	BEQ	RUBCHK		; Is there a delay? No, skip ahead
	JSR	DELAY3-OFFSET	; Yes, do the delay

;------ Check for RUBOUT

RUBCHK	CMP	#$5F		; RUBOUT character
	BEQ	RUBOUT		; Perform RUBOUT

	CMP	#$0C		; CTRL-L (Home)?
	BNE	KEYCHK

;------ CTRL-L (HOME)

	JSR	SCOUT		; Print character at cursor position
	JSR	CURHOME		; HOME the cursor
	STX	CURDIS		; Update cursor position
	BEQ	PDONE

;------ More checking

KEYCHK	CMP	#$0A		; CTRL-J (Cursor Down)
	BEQ	LINEFD

	CMP	#$1E		; CTRL-^ (Clear Window)
	BEQ	CTRLUA

	CMP	#$0B		; CTRL-K (Cursor Right)
	BEQ	CTRLK

	CMP	#$1A		; CTRL-Z (Clear Screen)
	BEQ	CTRLZ

;------ Check for CR
; This is the last check. It checks for CR and if so does it, if not
; it assumes a normal printable character. For our customized ROM
; we patch here to jump to our own CUSTKEYS key handler. It must process
; the keys INCLUDING Carriage RETURN and jump to STORKEY if the key
; is not recognized.

!IF OPTKEYS=0 {
	CMP	#$0D		; CTRL-M (CARRIAGE RETURN)
	BNE	STORKEY		; We are done testing for special KEYs
} ELSE {
	!IF OPTXJMP=0 {	JMP CUSTKEYS }	; Process our custom keys
	!IF OPTXJMP=1 {	JMP XCUSTKEYS }	; Process our custom keys
}

;------ CARRIAGE RETURN

DOCR	JSR	MODIFYX
	BNE	PULLYXA		; Restore registers and we're done

;------ Store character for repeat comparison

STORKEY	STA	OLDCHR		; Save it

;------ CTRL-K (Cursor Right)

CTRLK	JSR	SCOUT		; Print character at cursor position
	INC	CURDIS		; Move to next cursor position
	INX
	CPX	SWIDTH		; Are we at end of window line?
	BMI	PDONE		; No, skip ahead
	JSR	MODIFY0		; Yes

;------ LINE FEED (Cursor Down)

LINEFD	JSR	SCOUT		; Print character at cursor position
	LDY	#2		; Offset from SLTOP to compare
	JSR	ENDCHK2		; Are we at end of screen?
	BCS	SCROLLUP	; Yes, scroll up
	LDX	#3
	JSR	NEXTLINE		
	JMP	PDONE

;------ Scroll Screen UP?

SCROLLUP
	JSR	CRSRUP		; Cursor UP
	JSR	CURHOME		; HOME the cursor
	JSR	NEXTLINE	; Start at line 2

	LDX	SWIDTH		; Copy #bytes in Window width

SULOOP	JSR	XLDA		; Copy a character
	BPL	SULOOP		; Loop for more
	INX
	JSR	NEXTLINE	; Move to next screen line
	LDX	#3
	JSR	NEXTLINE
	JSR	ENDCHK		; Are we at the end of the screen?
	BCC	SULOOP		; No, loop back for more
	LDA	#32		; SPACE character
SULOOP2	JSR	XSTA		; Copy character to line
	BPL	SULOOP2		; Loop back for more

	LDX	#1		; Copy 2 bytes
SULOOP3	LDA	SLTOP,X		; Get byte
	STA	LSRC,X		; Copy it to source
	DEX			; Next byte
	BPL	SULOOP3		; Loop back for more

PDONE	JSR	MODIFY1

;------ Pull Registers from the stack in YXA order.

PULLYXA	PLA
	TAY
	PLA
	TAX
	PLA
	RTS

;------ CTRL-Z (CLEAR ENTIRE SCREEN)

CTRLZ	JSR	SCNCLR		; Clear the screen
	STA	OLDCHR
	BEQ	CUADONE

;------ CTRL-UP_ARROW / CTRL-SHIFT-N  (WINDOW CLEAR)

CTRLUA	LDA	#32		; SPACE CHAR
	JSR	SCOUT2
	JSR	CURHOME		; HOME the cursor
CUALOOP	LDX	SWIDTH
	LDA	#32		; SPACE CHAR
CUALOO2	JSR	XSTA		; Write it to screen
	BPL	CUALOO2		; Loop back
	STA	OLDCHR		; Set Last Chr as SPACE
	LDY	#2		; Offset from SLTOP to compare
	JSR	ENDCHK2		; Are we at end of screen?
	BCS	CUADONE		; Yes, we're done
	LDX	#3		; No
	JSR	NEXTLINE	; Move to next line on screen
	JMP	CUALOOP		; Loop back for more

CUADONE	JSR	CURHOME		; HOME the cursor
	STX	CURDIS
	BEQ	PULLYXA		; Restore registers and we're done

;------ ?

LF90C	JSR	TWPQAD
LF90F	JSR	CTRLF
	JSR	QDDATD
	JSR	SPCOUT
	JSR	GRTOUT
	LDX	#8		; # BYTES DISPLAYED
	STX	GENSTORE
LF91F	JSR	SPCOUT		; Output a SPACE
	JSR	PRBYTE
	JSR	NOTEND
	BCS	LF97B
	JSR	BUMP
	DEC	GENSTORE
	BNE	LF91F
	BEQ	LF90F

;------ 'M' (MOVE MEMORY)

MLM_M	JSR	TRIQAD
	JSR	SWAPMEM
	BCS	MSTART

;------ 'R' (RESTART FROM BREAKPOINT)

MLM_R	LDX	MLMSP
	TXS
	LDA	MLMPCHI
	PHA
	LDA	MLMPCLO
	PHA
	LDA	MLMREGP
	PHA
	LDA	MLMREGA
	LDX	MLMREGX
	LDY	MLMREGY
	RTI

;------ 'Z' (SET BREAKPOINT)

MLM_Z	LDX	#3
LF950	LDA	LFA4C-1,X
	STA	IRQ-1,X
	DEX
	BNE	LF950
	JSR	CHKLOAD2
	JSR	GETQDE
	LDA	(GENPTRLO),Y
	STA	MLMPCLO
	TYA
	STA	(GENPTRLO),Y
	BEQ	MSTART

;------ 'S' (SAVE)

MLM_S	JMP	SAVEMC

;------ 'L' (LOAD)

MLM_L	DEC	LOADFLAG
	BNE	DATAMODE

;------ 'T' (TABULAR DISPLAY)

LF96F	BEQ	LF90C
LF971	RTS

LF972	LDA	LOADFLAG
	BNE	LF971
	LDA	#'?'
	JSR	JUMPOUT		; Print It

LF97B	LDX	#$28
	TXS

MSTART	JSR	CTRLF
	LDY	#0
	STY	LOADFLAG
	JSR	GRTOUT

;------ '.' (COMMAND/ADDRESS MODE)

COMMAND	JSR	CHKLOAD2
	CMP	#'M'
	BEQ	MLM_M
	CMP	#'R'
	BEQ	MLM_R
	CMP	#'Z'
	BEQ	MLM_Z
	CMP	#'S'
	BEQ	MLM_S
	CMP	#'L'
	BEQ	MLM_L
	CMP	#'U'
	BNE	MLM_U
	JMP	(USERLO)

;------ TWOQAD - Collect two addresses. First to FE/FF, second to F9/FA

TWPQAD	JSR	CHKLOAD2
	JSR	GETQDE
	JSR	CMAOUT
	LDX	#0
LF9B1	JSR	CHKLOAD2
	!BYTE	$2C

;------ GETQDE - Collect address, store in FE. NOTE: Call GETNEW first!

GETQDE	LDX	#5
	JSR	GETPRC2
	JSR	CHKLOAD2
	!BYTE	$2C

;------ Collect Hex Pair for data byte, store in FC. NOTE: Call GETNEW first!

GETPRC	LDX	#3
GETPRC2	JSR	GETPRC3
	JSR	CHKLOAD2
GETPRC3	CMP	#'.'
	BEQ	COMMAND
	CMP	#'/'
	BEQ	DATAMODE
	JSR	ASCHEX
	BMI	LF972
	JMP	ROLSTR

MLM_U	CMP	#'T'
	BEQ	LF96F
	JSR	GETQDE

LF9DD	LDA	#'/'
	JSR	JUMPOUT		; Print It
	JSR	PRBYTE
	JSR	SPCOUT

;------ '/' (DATA MODE)

DATAMODE
	JSR	CHKLOAD2
	CMP	#'G'		; G=GO - Execute code
	BNE	LF9F2
	JMP	(GENPTRLO)

LF9F2	CMP	#','
	BNE	LF9FC
	JSR	BUMP
	JMP	DATAMODE

LF9FC	CMP	#$A		; LINEFEED
	BEQ	LFA16
	CMP	#$D
	BEQ	LFA1B

	CMP	#'^'
	BEQ	LFA21

	CMP	#$27
	BEQ	LFA3A
	JSR	GETPRC
	LDA	MLMBYTE
	STA	(GENPTRLO),Y
LFA13	JMP	DATAMODE

LFA16	LDA	#$D
	JSR	JUMPOUT		; Print It

LFA1B	JSR	BUMP
	JMP	LFA31

;------ '^'

LFA21	SEC
	LDA	GENPTRLO
	SBC	#1
	STA	GENPTRLO
	LDA	GENPTRHI
	SBC	#0
	STA	GENPTRHI

DATALN	JSR	CTRLF

LFA31	JSR	QDDATD
	JMP	LF9DD

LFA37	JSR	LFEF7

;------ "'"

LFA3A	JSR	CHKLOAD2
	CMP	#$27
	BNE	LFA46
	JSR	CMAOUT
	BNE	LFA13
LFA46	CMP	#$D
	BEQ	DATALN
	BNE	LFA37

LFA4C	JMP	LFA4F

LFA4F	STA	MLMREGA
	PLA
	PHA
	AND	#$10
	BNE	LFA5A
	LDA	MLMREGA
	RTI

;------ SAVE REGISTERS ON BREAK

LFA5A	STX	MLMREGX
	STY	MLMREGY
	PLA
	STA	MLMREGP
	CLD
	SEC
	PLA
	SBC	#2
	STA	MLMPCLO
	PLA
	SBC	#0
	STA	MLMPCHI
	TSX
	STX	MLMSP		; **BUG FIX** (ORIGINAL VALUE WAS $E1)
	LDY	#0
	LDA	MLMPCLO
	STA	(MLMPCLO),Y
	LDA	#$E0
	STA	GENPTRLO
	STY	GENPTRHI
	BNE	DATALN

SAVEMC	JSR	TRIQAD
	JSR	JUMPSV
	JSR	CHKLOAD
	JSR	JUMPOUT		; Print It
	JSR	PERIOD
	LDA	#'/'
	JSR	JUMPOUT		; Print It
	BNE	LFA97
LFA94	JSR	BUMP

LFA97	JSR	PRBYTE
	LDA	#$D
	JSR	ACIAOUT-OFFSET
	JSR	NOTEND
	BCC	LFA94
	LDA	MLMSP
	LDX	MLMPCLO
	STA	GENPTRLO
	STX	GENPTRHI
	JSR	PERIOD
	LDA	#'G'
	JSR	JUMPOUT		; Print It
	JSR	TENULL
	STY	SVFLAG
	JMP	MSTART


;=================================================================
; [$FABD] ENTRY TO SCREEN EDITOR
;=================================================================

EDITOR	TXA			; Push X,Y
	PHA
	TYA
	PHA
	LDA	EDFLAG		; Are we in EDIT mode?
	BPL	KEYLOOP

EDLOOP	LDY	EDISP		; Edit Cursor displacement from start of current line
	LDA	CURSLO
	STA	MLMSP
	LDA	CURSLO+1
	STA	MLMPCLO
	LDA	(MLMSP),Y
	STA	CURCHR		; Character under edit cursor
	LDA	#$A1
	STA	(MLMSP),Y
	JSR	GETKEY		; Poll the keyboard
	LDA	CURCHR		; Character under edit cursor
	STA	(MLMSP),Y

;------ Test for Cursor Movement A/S/D/F and Q/ESC to Copy

CHKEDIT	LDA	TMPCHR

!IF OPTEMACS=0 {
	!IF MACHINE=0 {	CMP #$1B } ; ESC    (copy)?
	!IF MACHINE=1 {	CMP #$11 } ; CTRL-Q (copy)?  UK-101 does not have an ESC key!
	!IF MACHINE=2 {	CMP #$1B } ; ESC    (copy)?

	BEQ	DOCTRLQ

	CMP	#1		; CTRL-A (left)?
	BEQ	DOCTRLA
	CMP	#4		; CTRL-D (right)?
	BEQ	DOCTRLD
	CMP	#$13		; CTRL-S (up)?
	BEQ	DOCTRLS
	CMP	#6		; CTRL-F (down)?
	BNE	LFB22
} ELSE {
	CMP	#$19		; CTRL-Y (yank)
	BEQ	DOCTRLQ
	CMP	#2		; CTRL-B (backwards)?
	BEQ	DOCTRLA
	CMP	#6		; CTRL-F (forwards)?
	BEQ	DOCTRLD
	CMP	#$10		; CTRL-P (up)?
	BEQ	DOCTRLS
	CMP	#E		; CTRL-N (next line)?
	BNE	LFB22
}
;------ CTRL-F (cursor down)

	JSR	LFB7C
	JMP	EDLOOP

;------ CTRL-S (cursor up)

DOCTRLS	JSR	CRSRUP
	JMP	EDLOOP

;------ CTRL-D (cursor right)

DOCTRLD	JSR	CRSRFWD
	JMP	EDLOOP

;------ CTRL-A (cursor left)

DOCTRLA	JSR	LFE19
	JMP	EDLOOP

;------ CTRL-Q / ESC (copy character at edit cursor)

DOCTRLQ	LDA	CURCHR		; Character under edit cursor
	STA	TMPCHR
	JSR	CRSRFWD
	JMP	CHKEDONE

;------ Get another key

KEYLOOP	JSR	GETKEY		; Poll the keyboard

;------ Check for CTRL-E
LFB22	CMP	#5		; Is it CTRL-E?
	BNE	CHKEDONE	; No, we're done

;------ CTRL-E (toggle EDIT MODE)

DOCTRLE	LDA	EDFLAG		; Get EDIT Flag
	EOR	#$FF		; Toggle it
	STA	EDFLAG		; Save it back
	BPL	KEYLOOP		; Is it enabled? No, get more keys

;------ Enable EDIT mode

EDITON	LDA	LTEXT		; Get Current cursor position
	STA	CURSLO		; and move the edit cursor there too
	LDA	LTEXT+1
	STA	CURSLO+1
	LDX	#0
	STX	EDISP		; Edit Cursor displacement from start of current line
	BEQ	EDLOOP

CHKEDONE
	JMP	PULLYX

;=================================================================
; [$FB46] BASIC INPUT ROUTINE
;=================================================================
; Get a character from keyboard or ACIA

INPUT	BIT	LDFLAG
	BPL	SKIP2ED		; LOAD FLAG CLR
INPUT2	LDA	#$FD
	STA	KEYBD
	LDA	#$10
	BIT	KEYBD
	BEQ	LOADOFF		; SPACE KEY PRESSED

;=================================================================
; [$FB57] INPUT FROM ACIA
;=================================================================

TAPIN	LDA	ACIA
	LSR	
	BCC	INPUT2
	LDA	ACIA+1
	RTS

;=================================================================
; [$FB61] CANCEL TAPE LOAD
;=================================================================

LOADOFF	LDA	#0
	STA	LOADFLAG
	STA	LDFLAG

SKIP2ED	JMP	EDITOR		; Check for Editor Keys


;=================================================================
; [$FBxx] MOVE CURSOR FORWARD
;=================================================================

CRSRFWD	LDX	SWIDTH
	CPX	EDISP		; Edit Cursor displacement from start of current line
	BEQ	LFB77
	INC	EDISP		; Edit Cursor displacement from start of current line
	RTS

LFB77	LDX	#0
	STX	EDISP		; Edit Cursor displacement from start of current line
LFB7C	CLC
	LDA	CURSLO

!IF OPTWIDTH=0 {	ADC #WIDTH }	; Subtract width of physical line (calculated constant)
!IF OPTWIDTH=1 {	ADC PWIDTH }	; Subtract width of physical line (from memory)

	STA	CURSLO
	LDA	CURSLO+1
	ADC	#0
	CMP	#>BOT 		; HI byte of BOTTOM OF SCREEN
	BNE	LFB90
	LDA	#>SCREEN	; HI Byte of start of SCREEN
LFB90	STA	CURSLO+1
LFB93	RTS

;=================================================================
; [$FB94] BASIC CTRL-C CHECK
;=================================================================

CTRLC	LDA	CCFLAG
	BNE	LFB93		; DISABLE FLAG SET
	LDA	#$FE
	STA	KEYBD
	BIT	KEYBD
	BVS	LFB93
	LDA	#$FB
	STA	KEYBD
	BIT	KEYBD
	BVS	LFB93
	LDA	#3		; CTRL-C PRESSED
	JMP	BROMCC

;=================================================================
; [$FBB2] TABLE to setup screen parameters and vectors
;=================================================================

SETUPTBL
	!WORD	INPUT		; 218 [$FBB2] INPUT  ($FB94)
	!WORD	OUTPUT		; 21A [$FBB4] OUTPUT ($FF93)
	!WORD	CTRLC		; 21C [$FBB6] CTRL-C ($FB70)
	!WORD	LOADIT		; 21E [$FBB8] LOAD   ($FE7B)
	!WORD	SAVEIT		; 220 [$FBBA] SAVE   ($FE17)

	!BYTE	LWIDTH		; 222 [$FBBC] Logical Screen Line Width (ie: 24)
	!WORD	TOP		; 223 [$FBBD] Top-Left (Home) position of screen (ie: $D085)
	!WORD	BASE		; 225 [$FBBF] Bottom-Left position of screen (ie: $D365)

	LDA	TOP,X		; 227 [$FBC1]  <-- Start of a small subroutine
	STA	TOP,X		; 22A [$FBC4]  copied to low memory and executed
	DEX			; 22D [$FBC7]  there.
	RTS			; 22E [$FBC8]  <-- END

	!BYTE	$00		; 22F [$FBC9] 
	!BYTE	$20		; 230 [$FBCA] 
	!WORD	TOP		; 231 [$FBCB] 
	!WORD	COMMAND		; 233 [$FBCD] 

;=================================================================
; [$FBCF] Check if base of screen overshot
;=================================================================
; This checks if the cursor goes below the screen BASE.
; Called with Y set to the offset. Returns CARRY FLAG SET

ENDCHK	LDX	SWIDTH		; Screen Width
ENDCHK2	SEC
	LDA	LTEXT		; Text Pointer LO
	SBC	SLTOP,Y		; Screen Parameter LO
	LDA	LTEXT+1		; Text Pointer HI
	SBC	SLTOP+1,Y	; Screen Parameter HI
	RTS

;=================================================================
; [$FBE0] Output a ">" , "," or " "
;=================================================================

GRTOUT	LDA	#'>'
	!BYTE	$2C
CMAOUT	LDA	#','
	!BYTE	$2C
SPCOUT	LDA	#' '
	JMP	JUMPOUT		; Print It

;=================================================================
; [$FBEB] NOTEND Compare FE with F9, Set CARRY CLEAR if FE is less
;=================================================================

NOTEND	SEC
	LDA	GENPTRLO
	SBC	CEGPTRLO
	LDA	GENPTRHI
	SBC	CEGPTRHI
	RTS

;=================================================================
; [$FBF5] CRLF - Print CR and LF to display
;=================================================================

CTRLF	LDA	#$0D		; CARRIAGE RETURN
	JSR	JUMPOUT		; Print It
	LDA	#$0A		; LINE FEED
	JMP	JUMPOUT		; Print It

	!BYTE	$40

;=================================================================
; [$FC00] FLOPPY DISK BOOTSTRAP
;=================================================================
; OPTDISK variable determines what code is placed here. 0=None.

* = $FC00

!IF OPTDISK=1 { !SOURCE "cegmon-diskbootstrap.asm" }		; Include DISK BOOTSTRAP
!IF OPTDISK=2 { !SOURCE "cegmon-revd.asm" }			; Include 600 REV D screen and keyboard code
!IF OPTDISK=3 { !SOURCE "cegmon-monselect.asm" }		; Include Monitor ROM selection Menu
!IF OPTDISK=4 { !SOURCE "cegmon-basselect.asm" }		; Include BASIC ROM selection Menu

;=================================================================
; [$FCA6] INIT ACIA
;=================================================================

* = $FCA6

INITACIA
	LDA	#3		; RESET ACIA
	STA	ACIA

!IF MACHINE=0 {	LDA #$11 }	; /16, 8BITS, 2STOP, RTS LOW
!IF MACHINE=1 { LDA #$11 }	; /16, 8BITS, 2STOP, RTS LOW
!IF MACHINE=2 { LDA #$B1 }	; /16, 8BITS, 2STOP, RTS LOW, RX INT

	STA	ACIA
	RTS

;=================================================================
; [$FCB1] OUTPUT CHAR TO ACIA
;=================================================================

ACIAOUT
	PHA
ACIAOUT2
	LDA	ACIA
	LSR	
	LSR	
	BCC	ACIAOUT2
	PLA
	STA	ACIA+1
	RTS

;=================================================================
; [$FCBE] KEYWRT - Write to KEYBOARD with INVERT 
;=================================================================
; ROW (A)  1=R0, 2=R1, 4=R2 ETC

KEYWRT	EOR	#$FF
	STA	KEYBD
	EOR	#$FF
	RTS

;=================================================================
; [$FCC6] KEY2XR - Read KEYBOARD with INVERT
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
; [$FCCF] KYREAD - Read KEYBOARD with INVERT
;=================================================================

KYREAD	LDA	KEYBD
	EOR	#$FF
	RTS


;=================================================================
; [$FCD5] UK101 BASIC ROM RUBOUT KEY HANDLER
;=================================================================

UKRUBOUT
	CMP	#$5F			; RUBOUT
	BEQ	UKRUB2
	JMP	BROMRUB2
UKRUB2	JMP	BROMRUB1


;=================================================================
; [$FCDF] KDELAY - Delay for keyboard read. Set X=0,Y=0
;=================================================================

KDELAY	LDY	#$10
DELAY3	LDX	#$40
DLOOP	DEX
	BNE	DLOOP
	DEY
	BNE	DELAY3
	RTS


;=================================================================
; [$FCEA] START-UP BANNER and PROMPT
;=================================================================

BANNER
!IF OPTBANNR = 0 { !TEXT "CEGMON(C)1980   C/W/M?" }
!IF OPTBANNR = 1 { !TEXT "CEGMON(C)1980 D/C/W/M?" }
!IF OPTBANNR = 2 { !TEXT "SJGMON 600D.17  C/W/M?" }
!IF OPTBANNR = 3 { !TEXT "CEGMON-MON-SEL  C/W/M?" }
!IF OPTBANNR = 4 { !TEXT "CEGMON-BAS-SEL  C/W/M?" }
!IF OPTBANNR = 5 { !TEXT "SJGMON-EXT-01 D/C/W/M?" }


;=================================================================
; [$FD00] POLLED KEYBOARD INPUT ROUTINE
;=================================================================
; This directly interfaces to the keyboard matrix, scans the ROWs
; and reads the COLUMNs to determine which keys are pressed and
; translates them into ASCII to be used by the rest of the system.
;
; This routine must start at $FD00 !
;
; Some LABELS and comments taken from PEEK65-V5#6 (1984) disassembly
;
; $0213 =
; $0214 = COUNTR
; $0215 = TMPCHR
; $0216 = LSTCHR

* = $FD00

GETKEY	TXA			; Push X and Y to the stack
	PHA
	TYA
	PHA

NEWSCN	LDA	#$80		; ROW 7
ROWLUP	JSR	KEYWRT		; SET ROW
	JSR	KEY2XR		; READ COL
	BNE	KEYFND		; KEY PRESS

	LSR			; NEXT ROW
	BNE	ROWLUP
	BEQ	LFD3A

KEYFND	LSR	
	BCC	TRUCHR
	TXA
	AND	#$20
	BEQ	LFD3A
	LDA	#$1B
	BNE	LFD50

TRUCHR	JSR	BITSHIFT
	TYA
	STA	TMPCHR		; Save it temporarily
	ASL	
	ASL	
	ASL	
	SEC
	SBC	TMPCHR
	STA	TMPCHR		; Save it temporarily
	TXA
	LSR	
	ASL	
	JSR	BITSHIFT
	BEQ	LFD47
	LDA	#0
LFD3A	STA	LSTCHR
LFD3D	STA	MEM213
	LDA	#2
	STA	COUNTR
	BNE	NEWSCN

LFD47	CLC
	TYA
	ADC	TMPCHR
	TAY
	LDA	KMATRIX-1,Y	; Get Key value from keyboard table

LFD50	CMP	MEM213
	BNE	LFD3D
	DEC	COUNTR
	BEQ	LFD5F
	JSR	KDELAY-OFFSET
	BEQ	NEWSCN

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
	STA	TMPCHR
	AND	#1
	TAX
	LDA	TMPCHR
	AND	#6
	BNE	LFDA2
	BIT	MEM213
	BVC	LFDBB
	TXA
	EOR	#1
	AND	#1
	BEQ	LFDBB
	LDA	#$20
	BIT	TMPCHR
	BVC	LFDC3
	LDA	#$C0
	BNE	LFDC3

LFDA2	BIT	MEM213
	BVC	LFDAA
	TXA
	BEQ	LFDBB
LFDAA	LDY	MEM213
	CPY	#$31
	BCC	LFDB9
	CPY	#$3C
	BCS	LFDB9
	LDA	#$F0
	BNE	LFDBB

LFDB9	LDA	#$10
LFDBB	BIT	TMPCHR
	BVC	LFDC3
	CLC
	ADC	#$C0
LFDC3	CLC
	ADC	MEM213
	AND	#$7F
	BIT	TMPCHR
	BPL	LFDD0
	ORA	#$80
LFDD0	STA	TMPCHR

;------ Pull Y and X from stack and load A with TMPCHR

PULLYX	PLA
	TAY
	PLA
	TAX
	LDA	TMPCHR		; Results of Keyboard Polling
	RTS

;------ ????

LFDDB	JSR	BUMP
	INC	MLMSP
	BNE	SWAPMEM
	INC	MLMPCLO

;------ SWAP Memory block move

SWAPMEM	LDA	(GENPTRLO),Y
	STA	(MLMSP),Y
	JSR	NOTEND
	BCC	LFDDB
	RTS


;=================================================================
; [$FDEE] Move to next line on screen
;=================================================================
; Sets the LSRC pointer to the next physical line
; X must be set to the proper pointer.

NEXTLINE	CLC

!IF OPTWIDTH=0 {	LDA #WIDTH }	; Subtract width of physical line (calculated constant)
!IF OPTWIDTH=1 {	LDA PWIDTH }	; Subtract width of physical line (from memory)

	ADC	LSRC,X		; Add the current position LO
	STA	LSRC,X		; Write it back
	LDA	#0
	ADC	LSRC+1,X	; Add the CARRY to update the
	STA	LSRC+1,X	; current position HI byte
	RTS


;=================================================================
; [$FE00] 65V MONITOR
;=================================================================
; This is the machine language monitor, for displaying and modifying
; memory, loading and saving binary code, and executing code.
;
; This routine must start at $FE00

* = $FE00

NEWMON	LDX	#$28
	TXS
	CLD
	JSR	INITACIA
	JSR	INITMEM
	NOP
	NOP

MENTRY	JSR	SCNCLR		; Clear the screen
	STA	OLDCHR
	STY	GENPTRLO
	STY	GENPTRHI
	JMP	MSTART

LFE19	LDX	EDISP		; Edit Cursor displacement from start of current line
	BEQ	LFE22
	DEC	EDISP		; Edit Cursor displacement from start of current line
	RTS

LFE22	LDX	SWIDTH
	STX	EDISP		; Edit Cursor displacement from start of current line

;------ Cursor UP

CRSRUP	SEC
	LDA	CURSLO

!IF OPTWIDTH=0 {	SBC #WIDTH }	; Subtract width of physical line (calculated constant)
!IF OPTWIDTH=1 {	SBC PWIDTH }	; Subtract width of physical line (from memory)

	STA	CURSLO
	LDA	CURSLO+1
	SBC	#0
	CMP	#>SCREEN-1	; HI Byte of start of SCREEN
	BNE	LFE3C
	LDA	#>BOT-1		; HI byte of BOTTOM OF SCREEN
LFE3C	STA	CURSLO+1
	RTS


;=================================================================
; [$FE40] Init Low Memory / Storage and Vectors area
;=================================================================
; This initialized lower memory used for the operating system.
; CEGMON must initialize it's screen windowing system parameters and
; copy a small routine that gets used for screen operations. CEGMON
; modifies the code in this routine which is why it must be in RAM.

INITMEM	LDY	#$1C		; INIT 218-234
ILOOP1	LDA	SETUPTBL,Y	; Read from Screen and Vector SETUP Table
	STA	INVECP,Y	; Write it
	DEY
	BPL	ILOOP1
	LDY	#7		; ZERO 200-206, 212
	LDA	#0
	STA	CCFLAG		; ENABLE CTRL-C FLAG
ILOOP2	STA	CURDIS-1,Y
	DEY
	BNE	ILOOP2
	RTS


;=================================================================
; [$FE59] CLEAR SCREEN
;=================================================================
; Clears the entire video screen (ignores current WINDOW).

SCNCLR	LDY	#<SCREEN	; LO Byte of start of SCREEN
	STY	CEGPTRLO
	LDA	#>SCREEN	; HI Byte of start of SCREEN
	STA	CEGPTRHI

	LDX	#(SIZE+1)*4 	; How many pages to clear?
	LDA	#' '		; SPACE character

SCLOOP	STA	(CEGPTRLO),Y	; Store it
	INY			; Next position
	BNE	SCLOOP		; Loop back for more
	INC	CEGPTRHI	; Increment HI byte of address
	DEX			; Done one screen page
	BNE	SCLOOP		; Are we done? No, loop back
	RTS


;=================================================================
; [$FE70] LOAD
;=================================================================

LOADIT	PHA
	DEC	LDFLAG		; SET LOAD FLAG
	LDA	#0		; CLR SAVE FLAG
LFE76	STA	SVFLAG
	PLA
	RTS


;=================================================================
; [$FE7B] SAVE
;=================================================================

SAVEIT	PHA
	LDA	#1		; SET SAVE FLAG
	BNE	LFE76


;=================================================================
; [$FE80] INPUT CHAR FROM ACIA
;=================================================================

MCACIA	JSR	TAPIN
	AND	#$7F		; CLEAR BIT 7
	RTS


;=================================================================
; [$FE86] BIT SHIFTER - LEFT 
;=================================================================

BITSHIFT
	LDY	#8
BITSHFT2
	DEY
	ASL	
	BCC	BITSHFT2
	RTS


;=================================================================
; [$FE8D] CHECK LOAD 2
;=================================================================

CHKLOAD2
	JSR	CHKLOAD
	JMP	JUMPOUT		; Print It


;=================================================================
; [$FE93] CONVERT ASCII-HEX CHAR TO BINARY
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


;=================================================================
; [$FEAC] Print address in FE, space, value in FC to display
;=================================================================

ADVTOD	JSR	QDDATD
	NOP
	NOP
	JSR	SPCOUT
	BNE	PRDATD

;------ Print address in FE to display

QDDATD	LDX	#3
	JSR	PRDATD2
	DEX
	!BYTE	$2C		; BIT

;------ Print Data Byte in FC to display

PRDATD	LDX	#0

PRDATD2	LDA	MLMBYTE,X
	LSR	
	LSR	
	LSR	
	LSR	
	JSR	HEXOUT
	LDA	MLMBYTE,X

;------ Strip byte in A to lower nibble, print it as ASCII hex to display

HEXOUT	AND	#$F
	ORA	#'0'
	CMP	#'9'+1
	BMI	LFED5
	CLC
	ADC	#7
LFED5	JMP	JUMPOUT		; Print It

	!BYTE	$EA,$EA		; Wow, 2 unused bytes!

;------ Roll new nibble into FE if X=2, or into FC if X=0

ROLSTR	LDY	#4
	ASL	
	ASL	
	ASL	
	ASL	
LFEE0	ROL	
	ROL	CEGPTRLO,X
	ROL	CEGPTRHI,X
	DEY
	BNE	LFEE0
	RTS


;=================================================================
; [$FEE9] Check LOADFLAG
;=================================================================
; 0=Get Key, 1=Get from ACIA

CHKLOAD	LDA	LOADFLAG	; Check the LOAD FLAG
	BNE	MCACIA		; >0? Yes, do ACIA
	JMP	GETKEY		; =0, do keyboard


;=================================================================
; [$FEF0] Print data at Current Address at $FE to display. Assume Y=0
;=================================================================

PRBYTE	LDA	(GENPTRLO),Y
	STA	MLMBYTE
	JMP	PRDATD


;=================================================================
; [$FEF7] Increment current address at FE
;=================================================================

LFEF7	STA	(GENPTRLO),Y
BUMP	INC	GENPTRLO
	BNE	LFEFF
	INC	GENPTRHI
LFEFF	RTS


;=================================================================
; [$FF00] POWER ON RESET
;=================================================================
; This is where the CPU jumps (via the RESET vector) when the machine
; is powered on. The hardware must be initialized just enough to
; display the power-on BANNER and accept user input. We don't want to
; mess things up too much so that the user can select "W" for warm
; start and still have their BASIC program intact.

* = $FF00

RESET	CLD
	LDX	#$28		; Reserve some area in
	TXS			; the stack
	JSR	INITACIA	; Initialize the ACIA chip

!IF OPTINIT=0 {
	JSR	INITMEM		; Initialize low memory
} ELSE {
	!IF OPTXJMP=0 {	JSR CUSTOM }	; Run CUSTOM code
	!IF OPTXJMP=1 {	JSR XCUSTOM }	; Run CUSTOM code via Jump table
}
	JSR	SCNCLR		; Clear the screen
	STY	CURDIS		; was: JSR CURHOME


;=================================================================
; [$FF10] Display Power-on Banner ("CEGMON D/C/W/M?")
;=================================================================
; This prints the power-on banner prompt

BANLOOP	LDA	BANNER,Y	; Prompt
	JSR	JUMPOUT		; Print It
	INY			; next character
	CPY	#$16		; Are we done?
	BNE	BANLOOP		; No, loop for more


;=================================================================
; [$FF1A] Get User's BOOT option
;=================================================================
; This waits for the User to make a selection then jumps to the
; appropriate code to start the machine.

BOOTMENU
	JSR	JUMPIN		; Accept a key
	AND	#$DF		; strip off uppercase bits

!IF OPTDISK=1 {
	CMP	#'D'		; Is it "D" (Disk Boot)?
	BNE	LFF27
	JMP	BOOTSTRP	; Yes, do it
}

LFF27	CMP	#'M'		; Is it "M" (Monitor)?
	BNE	LFF2E
	JMP	NEWMON		; Yes, do it

LFF2E	CMP	#'W'		; Is it "W" (Warm Start)?
	BNE	LFF35
	JMP	0		; Yes, do it

LFF35	CMP	#'C'		; Is it "C" (Cold Start)?
	BNE	RESET		; No, do a RESET
	JMP	BROMCOLD	; Yes, do it


;=================================================================
; [$FF3C] KEYBOARD MATRIX
;=================================================================
; The keyboard is organized as an 8x8 matrix of keys. In the main
; matrix only 7 keys are used on each ROW.
; The special modifier keys (RPT,CTRL,ESC,Both SHIFTs, and ShiftLock)
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
; [$FF6D] MODIFY INSTRUCTIONS in XSTA routine
;=================================================================

MODIFYX	JSR	SCOUT		;Print character at cursor position

MODIFY0	LDX	#0
	STX	CURDIS
MODIFY1	LDX	CURDIS
	LDA	#$BD		; "LDA" instruction (LDA ABS,X)
	STA	XSTA
	JSR	XSTA
	STA	OLDCHR
	LDA	#$9D		; "STA" instruction (STA ABS,X)
	STA	XSTA
MODIFY2	LDA	#$5F
	BNE	SCOUT2


;=================================================================
; [$FF8C] SCOUT - Print character at cursor position
;=================================================================

SCOUT	LDA	OLDCHR
SCOUT2	LDX	CURDIS
	JMP	XSTA


;=================================================================
; [$FF95] OLD SCREEN
;=================================================================
; Use BASIC's screen handler at $BF2D

OLDSCR	JSR	BROMCRTC
	JMP	OUTPUT2


;=================================================================
; [$FF9B] OUTPUT
;=================================================================
; General output to screen or ACIA

OUTPUT	JSR	NSCREEN		; Handle new CTRL keys for Editor
OUTPUT2	PHA
	LDA	SVFLAG		; ARE WE SAVING?
	BEQ	PULLA		; SAVE FLAG CLR
	PLA
	JSR	ACIAOUT		; CHAR TO ACIA
	CMP	#$D		; IS IT CARRIAGE RETURN?
	BNE	LFFBC		; NO, SKIP


;=================================================================
; [$FFAC] OUTPUT 10 NULLS
;=================================================================

TENULL	PHA
	TXA
	PHA
	LDX	#$A		; Count to 10
	LDA	#0		; NULL

TENLOOP	JSR	ACIAOUT-OFFSET	; Output it
	DEX				
	BNE	TENLOOP		; Loop back up if not done

	PLA			; Pull X from stack
	TAX
PULLA	PLA			; Pull A from stack
LFFBC	RTS


;=================================================================
; [$FFBD] TRIQAD - Collect 3 addresses to FE/FF, F9/FA, E4/E5
;=================================================================

TRIQAD	JSR	TWPQAD
	JSR	GRTOUT
	LDX	#3
	JSR	LF9B1
	LDA	MLMBYTE
	LDX	GENSTORE
	STA	MLMSP
	STX	MLMPCLO
	RTS


;=================================================================
; [$FFD1] HOME CURSOR - COPY WINDOW TOP to CURSOR POINTER
;=================================================================

CURHOME	LDX	#2		; We want to copy 2 bytes
CHLOOP	LDA	SLTOP-1,X	; From Window Parameters
	STA	LSRC-1,X	; To copy routine pointer
	STA	LTEXT-1,X	; and this pointer
	DEX			; next byte
	BNE	CHLOOP		; go back for more
	RTS


;=================================================================
; [$FFE0] TABLE for BASIC ROM SCREEN PARAMETERS
;=================================================================
; These bytes must not move! They are used for BASIC printing

*=$FFE0

LFFE0	!BYTE	<BASE		; CURSOR START
LFFE1	!BYTE	LWIDTH		; LINE LENGTH - 1
LFFE2	!BYTE	SIZE		; SCREEN SIZE (0=1K 1=2K)


;=================================================================
; [$FFE3] Print a PERIOD
;=================================================================

PERIOD	LDA	#'.'
	JSR	JUMPOUT		; Print It
	JMP	QDDATD


;=================================================================
; [$FFEB] JUMP TABLE
;=================================================================

JUMPIN	JMP	(INVECP)	; INPUT  FB46
JUMPOUT	JMP	(OUTVEC)	; OUTPUT FF9B
JUMPCC	JMP	(CCVEC)		; CTRL-C FB94
JUMPLD	JMP	(LDVEC)		; LOAD   FE70
JUMPSV	JMP	(SVVEC)		; SAVE   FE7B


;=================================================================
; [$FFFA] 6502 RESET, IRQ, and NMI Vectors
;=================================================================
; This is where the CPU jumps when powered on, or an interrupt is
; triggered. These must be located as the last 6 bytes of the ROM
; so that the CPU can start properly

*=$FFFA

!WORD	NMI			; NMI   (normally not used)
!WORD	RESET			; RESET
!WORD	IRQ			; IRQ   (normally not used)
