DISK=$C000
ACIA=$F000

*=$F800
SF800    STA $0202
         PHA
         TXA
         PHA
         TYA
         PHA
         LDA $0202
         AND #$FF
         BEQ LF81C
         LDY $0206
         BEQ LF81C
LF814    LDX #$FF
LF816    DEX
         BNE LF816
         DEY
         BNE LF814
LF81C    CMP #$0A	;LF
         BNE LF823
         JSR LF8D5
LF823    CMP #$0D	;CR
         BNE LF82A
         JSR SF9B9
LF82A    CMP #$5F	; '_' (RUBOUT)
         BNE LF831
         JSR SF9E0
LF831    CMP #$03
         BNE LF838
         JSR SFA1B	; CTRL-C clear screen
LF838    CMP #$02
         BNE LF83F
         JSR SF993	; CTRL-B beginning
LF83F    CMP #$01
         BNE LF846
         JSR LF954	; CTRL-A erase to end of screen
LF846    CMP #$05
         BNE LF84D
         JSR SF9C5
LF84D    CMP #$12	; CTRL-R right
         BNE LF854
         JSR SFA38
LF854    CMP #$15	; CTRL-U up
         BNE LF85B
         JSR SFA27
LF85B    CMP #$0C
         BNE LF862
         JSR SFA21	; CTRL-L left
LF862    CMP #$04
         BNE LF869
         JSR LF8D5	; CTRL-D
LF869    CMP #$07
         BNE LF870
         JSR SFBD0	; BELL
LF870    CMP #$09
         BNE LF877
         JSR SFA44
LF877    CMP #$7F
         BNE LF87E
         JSR SFA7C	; ??
LF87E    CMP #$20
         BCC LF885
         JSR SFA3E	; OTHER CTRL CHAR
LF885    PLA
         TAY
         PLA
         TAX
         PLA
         RTS
         
SF88B    LDA #$00
         STA $022D
         LDA $022A
         STA $022C
         LDX #$05
LF898    ASL $022C
         ROL $022D
         DEX
         BNE LF898
         LDA $0226
         BEQ LF8AC
         ASL $022C
         ROL $022D
LF8AC    CLC
         LDA $0229
         ADC $022C
         STA $022C
         LDA #$D0
         ADC $022D
         STA $022D
         RTS
         
SF8BF    LDA $0201
SF8C2    LDY #$8D
LF8C4    STY $022B
         LDY #$60
         STY $022E
         LDY #$00
         JMP $022B
         
SF8D1    LDY #$AD
         BNE LF8C4
LF8D5    JSR SF8BF
         LDA $022A
         CMP $0225
         BCS LF8E6
         INC $022A
         JMP LF8E9
         
LF8E6    JMP LF907
         
LF8E9    LDA #$5F	;underscore
         LDX $0227
         BMI SF8F2
         LDA #$BB	;halftone block
SF8F2    STA $0201
SF8F5    JSR SF88B
         JSR SF8D1
         TAX
         LDA $0201
         JSR SF8C2
         STX $0201
         TYA
         RTS
         
LF907    CLC
         LDX #$07

LF90A    LDA $BFF3,X	;BFF3 B9 00 D0 LDA $D000,Y
         STA $0207,X    ;BFF6 99 00 D0 STA $D000,Y
         DEX            ;BFF9 C8       INY
         BPL LF90A      ;BFFA 60       RTS
         LDA $0224
         STA $022A
LF919    LDA $0222
         STA $0229
         JSR SF88B
         LDA $022C
         LDX $022D
         STA $020B
         STX $020C
         LDA $022A
         CMP $0225
         BCS LF94E
         INC $022A
         JSR SF88B
         LDA $022C
         LDX $022D
         STA $0208
         STX $0209
         JSR SF982
         JMP LF919
         
LF94E    LDA $0222
         STA $0229
LF954    LDA $022A
         PHA
         LDA $0229
         PHA
LF95C    LDA #$20
         JSR SF8F2
         LDA $0229
         CMP $0223
         BCC LF971
         LDA $022A
         CMP $0225
         BCS LF977
LF971    JSR SF9A5
         JMP LF95C
         
LF977    PLA
         STA $0229
         PLA
         STA $022A
         JMP LF8E9
         
SF982    LDY #$00
LF984    JSR $0207
         INC $0229
         LDA $0223
         CMP $0229
         BCS LF984
         RTS
         
SF993    JSR SF8BF
SF996    LDA $0224
         LDX $0222
         STA $022A
         STX $0229
         JMP LF8E9
         
SF9A5    LDA $0229
         CMP $0223
         BCS LF9B3
         INC $0229
         JMP LF8E9
         
LF9B3    JMP LFCD5
         
LF9B6    JMP LF8D5

; CR         
SF9B9    JSR SF8BF	
SF9BC    LDA $0222
         STA $0229
         JMP LF8E9

; CTRL-E         
SF9C5    LDA $022A
         PHA
         LDA $0229
         PHA
LF9CD    LDA #$20
         JSR SF8F2
         LDA $0229
         CMP $0223
         BCS LF977
         JSR SF9A5
         JMP LF9CD
         
; RUBOUT ('_')
SF9E0    LDA #$20
         JSR SF8F2
         JSR SF9F0
         LDA #$20
         JSR SF8F2
         JMP LF8E9
         
SF9F0    LDA $0222
         CMP $0229
         BCS LF9FE
         DEC $0229
         JMP LF8E9
         
LF9FE    LDA $0223
         STA $0229
         LDA $0224
         CMP $022A
         BCS LFA12
         DEC $022A
         JMP LF8E9
         
LFA12    LDA $0222
         STA $0229
         JMP LF8E9

; CTRL-C
SFA1B    JSR SF993
         JMP LF954

; CTRL-L
SFA21    JSR SF8BF
         JMP SF9F0

; CTRL-U
SFA27    JSR SF8BF
         LDA $0224
         CMP $022A
         BCS LFA35
         DEC $022A
LFA35    JMP LF8E9

; CTRL-R
SFA38    JSR SF8BF
         JMP SF9A5

; ASCII CHAR $20-
SFA3E    JSR SF8F2
         JMP SF9A5

; TAB
SFA44    LDA #$20
         JSR SF8C2
         LDA $022A
         PHA
         LDA $0229
         PHA
LFA51    LDA $0229
         CMP $0223
         BCS LFA62
         INC $0229
         JSR SF8F5
         JMP LFA51
         
LFA62    LDA $0222
         STA $0229
         LDA $022A
         CMP $0225
         BCS LFA79
         INC $022A
         DEC $0229
         JMP LFA51
         
LFA79    JMP LF977

; CTRL-D
SFA7C    LDA $022A
         PHA
         LDA $0229
         PHA
LFA84    LDA $0229
         CMP $0223
         BCS LFAA1
         INC $0229
LFA8F    JSR SF8F5
         TXA
         PHA
         JSR SF9E0
         PLA
         JSR SF8C2
         JSR SF9A5
         JMP LFA84
         
LFAA1    LDA $022A
         CMP $0225
         BCS LFAB5
         INC $022A
         LDA $0222
         STA $0229
         JMP LFA8F
         
LFAB5    JSR SF8BF
         JMP LF977
         
LFABB    LDA $0227
         BPL LFAC8
         JSR SFCF7
         CMP #$0D
         BEQ LFB3D
         RTS
         
LFAC8    JSR LFD00
         CMP #$0D
         BEQ LFAD9
         CMP #$1B
         BEQ LFADE
         JSR SF800
         JMP LFAC8
         
LFAD9    LDA $0227
         BNE LFAE3
LFADE    LDA #$FF	;disable editor
         STA $0227
LFAE3    PLA
         PLA
         PLA
         PLA
         PLA
         PLA
         JSR SF993
LFAEC    CPX #$20
         BNE LFB08
         JSR SF9A5
         LDA $0229
         CMP $0223
         BNE LFAEC
         LDA $022A
         CMP $0225
         BNE LFAEC
         LDX #$00
         JMP LFB2C
         
LFB08    LDA #$00
         PHA
LFB0B    PLA
         TAX
         LDA $0201
         CPX #$47
         BCS LFB22
         STA $13,X
         INX
         TXA
         PHA
         JSR SF8BF
         JSR SF9A5
         JMP LFB0B
         
LFB22    DEX
         BEQ LFB2B
         LDY $13,X
         CPY #$20
         BEQ LFB22
LFB2B    INX
LFB2C    LDY #$00
         STY $13,X
         JSR SFA1B
         JSR SFBC7
         LDX #$12
         LDY #$00
         JMP $A280
         
LFB3D    PLA
         PLA
         PLA
         PLA
         PLA
         PLA
         JSR $A866
         STX $C3
         STY $C4
         JMP LFCE7
         
LFB4D    BNE LFB52
LFB4F    JMP $A27D
         
LFB52    LDX #$FF
         STX $88
         BCS LFB5B
         JMP $A295
         
LFB5B    JSR $A3A6
         JSR $00BC
         CMP #$99
         BEQ LFBA0
         CMP #$89
         BEQ LFBA0
         CMP #$9B
         BEQ LFBA0
         CMP #$45
         BNE LFBA3
         JSR $00BC
         BNE LFB80
         LDA #$01
         STA $0227	;enable editor
         JSR SFA1B
         BEQ LFB4F
LFB80    BCS LFBA3
         CMP #$30
         BEQ LFBA3
LFB86    JSR $00BC
         BEQ LFB8F
         BCC LFB86
         BCS LFBA3
LFB8F    LDA #$00
         STA $0227
         LDA #$FB
         LDY #$B5
         STA $05
         STY $04
         LDA #$99
         STA $13
LFBA0    JSR SFA1B
LFBA3    LDY #$00
         LDX #$12
         STX $C3
         STY $C4
         JMP $A5F6
         
LFBAE    LDA #$BA
         LDY #$FB
         JSR $A8C3
         LDA #$FF
         STA $88
         RTS
         
LFBBA    .BYTE $0D,$0A,'ready',$0D
LFBC2    .BYTE $0A,$07,$07,$00,$00 

SFBC7    LDA #<LFBAE
         STA $04
         LDA #>LFBAE
         STA $05
         RTS
         
SFBD0    LDA $0205
         BEQ LFBD6
         RTS
		 
;BELL
LFBD6    LDY #$64
LFBD8    LDX #$60
         LDA #$51	; /16, 8BITS, 2STOP, RTS=HIGH
         STA ACIA
LFBDF    DEX
         BNE LFBDF
         LDX #$60
         LDA #$11	; /16, 8BITS, 2STOP, RTS=LOW
         STA ACIA
LFBE9    DEX
         BNE LFBE9
         DEY
         BNE LFBD8
         TYA
         RTS
         
LFBF1    LDY $88
         INY
         BNE LFBF9
         JMP LFABB
         
LFBF9    JMP SFCF7
         
         .BYTE $3B,$0D,$04,$06 

; FLOPPY DISK BOOTSTRAP 
LFC00    JSR SFC0C
         JMP ($00FD)
         
LFC06    JSR SFC0C
         JMP LFE00
         
SFC0C    LDY #$00
         STY DISK+1
         STY DISK
         LDX #$04
         STX DISK+1
         STY DISK+3
         DEY
         STY DISK+2
         STX DISK+3
         STY DISK+2
         LDA #$FB
         BNE LFC33
LFC2A    LDA #$02
         BIT DISK
         BEQ LFC4D
         LDA #$FF
LFC33    STA DISK+2
         JSR SFCA5
         AND #$F7
         STA DISK+2
         JSR SFCA5
         ORA #$08
         STA DISK+2
         LDX #$18
         JSR SFC91
         BEQ LFC2A
LFC4D    LDX #$7F
         STX DISK+2
         JSR SFC91
LFC55    LDA DISK
         BMI LFC55
LFC5A    LDA DISK
         BPL LFC5A
         LDA #$03
         STA DISK+$10
         LDA #$58
         STA DISK+$10
         JSR SFC9C
         STA $FE
         TAX
         JSR SFC9C
         STA $FD
         JSR SFC9C
         STA $FF
         LDY #$00
LFC7B    JSR SFC9C
         STA ($FD),Y
         INY
         BNE LFC7B
         INC $FE
         DEC $FF
         BNE LFC7B
         STX $FE
         LDA #$FF
         STA DISK+2
         RTS
         
SFC91    LDY #$F8
LFC93    DEY
         BNE LFC93
         EOR $FF,X
         DEX
         BNE SFC91
         RTS
         
SFC9C    LDA DISK+$10
         LSR A
         BCC SFC9C
         LDA DISK+$11
SFCA5    RTS
         
SFCA6    LDA #$03	;reset ACIA
         STA ACIA
         LDA #$11
         STA ACIA
         RTS
         
LFCB1    PHA		;write to ACIA
LFCB2    LDA ACIA
         LSR A
         LSR A
         BCC LFCB2
         PLA
         STA ACIA+1
         RTS
         
SFCBE    EOR #$FF	;invert write to KB
         STA $DF00
         EOR #$FF
         RTS
         
SFCC6    PHA
         JSR SFCCF
         TAX
         PLA
         DEX
         INX
         RTS
         
SFCCF    LDA $DF00	;invert read from KB
         EOR #$FF
         RTS
         
LFCD5    LDA $0203
         BMI LFCDD
         BEQ LFCE1
LFCDC    RTS
         
LFCDD    CMP #$FF
         BNE LFCDC
LFCE1    JSR SF9BC
         JMP LF8D5
         
LFCE7    JSR SFBC7
         JSR $00BC
         JMP LFB4D
         
LFCF0    DEC $0203
         JMP LFF8B
         
         .BYTE $06 
		 
SFCF7    CPX #$00
         BNE LFD00
         LDA #$07
         JSR SF800

; POLLED KEYBOARD INPUT ROUTINE
LFD00    TXA
         PHA
         TYA
         PHA
LFD04    LDA #$01	; ROW 0
LFD06    JSR SFCBE	; SET ROW
         JSR SFCC6	; READ COL
         BNE LFD20	; KEY PRESSED
LFD0E    ASL A		; NEXT ROW
         BNE LFD06	; UNTIL DONE
LFD11    LDA #$00
         STA $0216
LFD16    STA $0215
         LDA #$02
         STA $0214
         BNE LFD04
LFD20    LSR A
         BCC LFD2C
         ROL A
         CPX #$21
         BNE LFD0E
         LDA #$1B
         BNE LFD4D
LFD2C    JSR SFDC6
         TYA
         STA $0213
         ASL A
         ASL A
         ASL A
         SEC
         SBC $0213
         STA $0213
         TXA
         LSR A
         JSR SFDC6
         BNE LFD11
         CLC
         TYA
         ADC $0213
         TAY
         LDA LFDCD,Y
LFD4D    CMP $0215
         BNE LFD16
         DEC $0214
         BEQ LFD63
         LDY #$05
LFD59    LDX #$C8
LFD5B    DEX
         BNE LFD5B
         DEY
         BNE LFD59
         BEQ LFD04
LFD63    LDX #$96
         CMP $0216
         BNE LFD6C
         LDX #$14
LFD6C    STX $0214
         STA $0216
         LDA #$01
         JSR SFCBE
         JSR SFCCF
         TAX
         LDY #$A0
         LSR A
         CMP #$20
         BEQ LFDAB
         LDY #$00
         CMP #$10
         BEQ LFDAB
         TXA
         LSR A
         BCC LFD97
LFD8C    AND #$FF
         BNE LFDA9
         LDA $0215
         BMI LFDAB
         BPL LFDA2
LFD97    LDX $0215
         CPX #$51
         BCC LFD8C
         AND #$FF
         BEQ LFDAB
LFDA2    LDY #$E0
         LDA $0215
         BPL LFDAB
LFDA9    LDY #$F0
LFDAB    LDA $0215
         CMP #$21
         BCC LFDBB
         STY $0213
         CLC
         ADC $0213
         AND #$7F
LFDBB    STA $0213
         PLA
         TAY
         PLA
         TAX
         LDA $0213
         RTS
         
SFDC6    LDY #$08
LFDC8    DEY
         ASL A
         BCC LFDC8
         RTS

; KEYBOARD MATRIX

LFDCD
		.BYTE	'p',$BB,'O',$20,'z','a','q'
		.BYTE	'L','m','n','b','v','c','x'
		.BYTE	'k','j','h','g','f','d','s'
		.BYTE	'i','u','y','t','r','e','w'
		.BYTE	$00,$00,$0D,$0A,'o','l','N'
		.BYTE	$00,$FF,'M',$BA,'P',$B9,$B8
		.BYTE	$B7,$B6,$B5,$B4,$B3,$B2,$B1

		NOP
		NOP

; 65V MONITOR
LFE00    LDX #$28
         TXS
         CLD
         BNE LFE0C
SFE06    LDX #$D8
         LDA #$D0
         BNE LFE2D
LFE0C    JSR SFE06
         BEQ LFE43
LFE11    JSR SFEE9
         CMP #$2F
         BEQ LFE4F
         CMP #$47
         BEQ LFE4C
         CMP #$4C
         BEQ LFE7C
         JSR SFE93
         BMI LFE11
         LDX #$02
         JSR SFEDA
         JMP LFE43
         
LFE2D    STA $FF
         LDA #$00
         STA $FE
         STA $FB
         TAY
         LDA #$20
LFE38    STA ($FE),Y
         INY
         BNE LFE38
         INC $FF
         JMP LFE8C
         
LFE42    NOP
LFE43    LDA ($FE),Y
         STA $FC
         JSR SFEAC
         BNE LFE11
LFE4C    JMP ($00FE)
         
LFE4F    JSR SFEE9
         CMP #$2E
         BEQ LFE11
         CMP #$0D
         BNE LFE69
         INC $FE
         BNE LFE60
         INC $FF
LFE60    LDY #$00
         LDA ($FE),Y
         STA $FC
         JMP LFE77
         
LFE69    JSR SFE93
         BMI LFE4F
         LDX #$00
         JSR SFEDA
         LDA $FC
         STA ($FE),Y
LFE77    JSR SFEAC
         BNE LFE4F
LFE7C    STA $FB
         BEQ LFE4F
LFE80    LDA ACIA
         LSR A
         BCC LFE80
         LDA ACIA+1
         AND #$7F
         RTS
         
LFE8C    CPX $FF
         BNE LFE38
         STY $FF
         RTS
         
SFE93    CMP #$30
         BMI LFEA9
         CMP #$3A
         BMI LFEA6
         CMP #$41
         BMI LFEA9
         CMP #$47
         BPL LFEA9
         SEC
         SBC #$07
LFEA6    AND #$0F
         RTS
         
LFEA9    LDA #$80
         RTS
         
SFEAC    LDX #$03
         LDY #$00
LFEB0    LDA $FC,X
         LSR A
         LSR A
         LSR A
         LSR A
         JSR SFECA
         LDA $FC,X
         JSR SFECA
         DEX
         BPL LFEB0
         LDA #$20
         STA $D153
         STA $D154
         RTS
         
SFECA    AND #$0F
         ORA #$30
         CMP #$3A
         BMI LFED5
         CLC
         ADC #$07
LFED5    STA $D14F,Y
         INY
         RTS
         
SFEDA    LDY #$04
         ASL A
         ASL A
         ASL A
         ASL A
LFEE0    ROL A
         ROL $FC,X
         ROL $FD,X
         DEY
         BNE LFEE0
         RTS
         
SFEE9    LDA $FB
         BNE LFE80
         JMP LFD00

LFEF0
	.WORD	SFFBA	; 218	INPUT
	.WORD	LFF69	; 21A	OUTPUT
	.WORD	LFF9B	; 21C	CTRL-C
	.WORD	LFF8B	; 21E	LOAD
	.WORD	LFF96	; 220	SAVE
	.BYTE	$05	; 222
	.BYTE	$1C	; 223
	.BYTE	$04	; 224
	.BYTE	$1C	; 225
	.BYTE	$00	; 226
	.BYTE	$FF	; 227 editor enable flag
 
RESET_HANDLER
LFF00       
         CLD
         LDX #$28
         TXS
         LDY #$10
LFF06    LDA $FEEF,Y
         STA $0217,Y
         DEY
         BNE LFF06
         JSR SFCA6
         STY $0212
         STY $0203
         STY $0205
         STY $0206
         STY $0228
         JSR SFE06
         JSR SF996
LFF27    LDA LFF5B,Y ;print D/C/W/M
         BEQ LFF32
         JSR SF800
         INY
         BNE LFF27
LFF32    JSR SFFBA
         CMP #$4D	''M
         BNE LFF3C
         JMP LFE00
         
LFF3C    CMP #$57	;'W
         BNE LFF46
         STA $0228
         JMP $0000
         
LFF46    CMP #$43	;'C
         BNE LFF54
         STA $0228
         LDY #$00
         STY $88
         JMP $BD22
         
LFF54    CMP #$44	;'D
         BNE RESET_HANDLER
         JMP LFC00
LFF5B    .BYTE 'D/C/W/M',0

         ASL A
		 ADC #$05
LFF66    LDY #$00
         RTS

; OUTPUT ROUTINE
LFF69    JSR SF800
; CHAR TO ACIA WITH NULLS AFTER CR
LFF6C
         PHA
         LDA $0205
         BEQ LFF89	; SAVE FLAG OFF
         PLA
         JSR LFCB1	; CHAR TO ACIA
         CMP #$0D
         BNE LFF8A	; NOT CR
         PHA
         TXA
         PHA
         LDX #$0A	; 10 NULLS
         LDA #$00
LFF81    JSR LFCB1
         DEX
         BNE LFF81
         PLA
         TAX
LFF89    PLA
LFF8A    RTS

; LOAD ROUTINE
LFF8B    PHA
         DEC $0203	; SET LOAD FLAG
         LDA #$00	; CLR SAVE FLAG
LFF91    STA $0205
         PLA
         RTS

; SAVE ROUTINE

LFF96    PHA
         LDA #$01	; SET SAVE FLAG
         BNE LFF91
LFF9B
         LDA $0212
         BNE LFFB9	; DISABLE FLAG SET
         LDA #$FE
         STA $DF00
         BIT $DF00
         BVS LFFB9	;test CTRL
         LDA #$FB
         STA $DF00
         BIT $DF00
         BVS LFFB9	;test C
         LDA #$03
         JMP $A636	;ctrl-c pressed
         
LFFB9    RTS

; INPUT

SFFBA    BIT $0203
         BPL LFFD8	; LOAD FLAG CLR
LFFBF    LDA #$FD
         STA $DF00
         LDA #$10
         BIT $DF00	;test space bar pressed
         BEQ LFFD5
         LDA ACIA
         LSR A
         BCC LFFBF	; WAIT ACIA
         LDA ACIA+1	; GET CHAR
         RTS
         
LFFD5    INC $0203
LFFD8    LDA $0228
         BNE LFFE3
         JMP LFD00
         
LFFE0	.BYTE	$65	; LINE START (OFFSET FROM $D300)
LFFE1	.BYTE	$17	; LINE LENGTH - 1
LFFE2	.BYTE	0	; SCREEN RAM 0=1K, 1=2K
LFFE3    JMP LFBF1
         
         .BYTE $FA, $EA 
LFFE8    JMP SFCF7

LFFEB    JMP ($0218)  ; INPUT  FFBA
LFFEE    JMP ($021A)  ; OUTPUT FF69
LFFF1    JMP ($021C)  ; CTRL-C FF9B
LFFF4    JMP ($021E)  ; LOAD   FF8B
LFFF7    JMP ($0220)  ; SAVE   FF96
         
		.WORD	$130	; NMI
		.WORD	LFF00	; RESET
		.WORD	$1C0	; IRQ
		