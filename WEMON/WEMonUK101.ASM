*=$F000
;
; ---- Code
;
;================================================================================
; Cold Start Entry Point
;================================================================================
;
RESET:
F000: A2 28     LDX #$28                
F002: 9A        TXS                     
F003: D8        CLD                     
F004: A9 7F     LDA #$7F                
F006: 8D 2D 02  STA CURFL                ;Cursor Flash Rate/suppress
F009: A9 06     LDA #$06                
F00B: 8D 40 02  STA MEM0240              ;Low Mem $0240
F00E: A9 00     LDA #$00                
F010: 8D 03 02  STA LDFLAG               ;Load Flag
F013: 8D 4F 02  STA TPFlag               ;Tape on/off Flag
F016: 8D 05 02  STA SCRSPD               ;Scroll delay
F019: 8D 22 02  STA MOVFLAG              ;Move Flag for insert Routine
F01C: 8D 27 02  STA EDFLAG               ;Edit Flag. Get a line from the screen
F01F: 8D 23 02  STA INSFLAG              ;Insert Flag
F022: 8D 24 02  STA SKBFLAG              ;FF next character returns BASIC command
F025: 8D 26 02  STA QUOFLAG              ;Quotes Flag
F028: 8D 12 02  STA CCFLAG               ;Control-C Flag
F02B: 8D 04 02  STA ERRORFL              ;FF = Run error
F02E: 8D 4E 02  STA PRFLAG               ;Print on/off Flag
F031: 8D 4B 02  STA DISK                 ;Disk bootstrap Vector
F034: 8D 06 02  STA MEM0206              ;Low Mem $0206
F037: A9 98     LDA #$98                
F039: 8D 4C 02  STA $024C               
F03C: 20 EF FE  JSR PRINTINIT           
F03F: A0 09     LDY #$09                
;
VECLD:
F041: B9 78 F7  LDA TABLE5,Y            
F044: 99 18 02  STA INVEC,Y              ;Input Vector
F047: 88        DEY                     
F048: 10 F7     BPL VECLD               
F04A: A9 00     LDA #$00                
F04C: 85 F6     STA ZP_F6                ;Zero Page $F6
F04E: 85 E0     STA ZP_E0                ;Zero Page $E0
F050: 85 D6     STA ZP_D6                ;Zero Page $D6
F052: 20 94 F0  JSR ACIAINIT            
F055: A9 A1     LDA #$A1                
F057: 8D 2C 02  STA CURSYM               ;Cursor Symbol
F05A: 20 A2 F0  JSR CLS                 
;
RECALL:
F05D: A2 00     LDX #$00                
F05F: 20 82 F7  JSR MSGOUT              
;================================================================================
; Power ON Menu
;================================================================================
;
MENULOOP:
F062: 20 69 F3  JSR KBRD                
F065: 29 5F     AND #$5F                
F067: C9 4D     CMP #$4D                ; Is it "M"? (Monitor)
F069: D0 0A     BNE L_F075              
F06B: A9 F0     LDA #$F0                
F06D: 48        PHA                     
F06E: A9 5E     LDA #$5E                
F070: 48        PHA                     
F071: 08        PHP                     
F072: 4C 55 F8  JMP L_F855              
;
L_F075:
F075: C9 43     CMP #$43                ; Is it "C"?  (Cold Start ROM BASIC)
F077: D0 03     BNE L_F07C              
F079: 4C 11 BD  JMP BASICCOLD           
;
L_F07C:
F07C: C9 57     CMP #$57                ; Is it "W"? (Warm Start)
F07E: D0 03     BNE L_F083              
F080: 4C 74 A2  JMP BASICWARM           
;
L_F083:
F083: C9 44     CMP #$44                ; Is it "D"? (Disk Boot)
F085: D0 03     BNE L_F08A              
F087: 6C 4B 02  JMP (DISK)               ;Disk bootstrap Vector
;
L_F08A:
F08A: C9 55     CMP #$55                ; Is it "U"? (User Vector)
F08C: D0 D4     BNE MENULOOP            
F08E: 20 29 02  JSR USERVEC              ;User Vector
F091: 4C 62 F0  JMP MENULOOP            
;================================================================================
; Initialize ACIA
;================================================================================
;
ACIAINIT:
F094: A9 03     LDA #$03                
F096: 8D 00 E0  STA ACIA2                ;Relocated ACIA
F099: A9 11     LDA #$11                
F09B: 8D 00 E0  STA ACIA2                ;Relocated ACIA
F09E: 8D 4F 02  STA TPFlag               ;Tape on/off Flag
F0A1: 60        RTS                     
;
;================================================================================
; CLEARSCREEN
;================================================================================
;
CLS:
F0A2: A9 60     LDA #$60                
F0A4: A0 00     LDY #$00                
F0A6: 8D 01 02  STA CURCHR               ;Character under cursor
;
CLSLOOP:
F0A9: 99 00 D3  STA VIDRAM+768,Y         ;Video Character RAM page 4
F0AC: 99 00 D2  STA VIDRAM+512,Y         ;Video Character RAM page 3
F0AF: 99 00 D1  STA VIDRAM+256,Y         ;Video Character RAM page 2
F0B2: 99 00 D0  STA VIDRAM,Y             ;Video Character RAM
F0B5: C8        INY                     
F0B6: D0 F1     BNE CLSLOOP             
;================================================================================
; Home the Cursor
;================================================================================
;
HOCUR:
F0B8: A9 D0     LDA #$D0                
F0BA: 8D 25 02  STA CURPSH               ;Cursor Position HI byte
F0BD: 85 F7     STA ZP_F7                ;Zero Page $F7
F0BF: A0 4B     LDY #$4B                
F0C1: 8C 00 02  STY CURPSL               ;Cursor Position LO byte
F0C4: D0 48     BNE L_F10E              
;
SETCUR:
F0C6: A9 00     LDA #$00                
F0C8: 85 F6     STA ZP_F6                ;Zero Page $F6
F0CA: AD 25 02  LDA CURPSH               ;Cursor Position HI byte
F0CD: 85 F7     STA ZP_F7                ;Zero Page $F7
F0CF: AC 00 02  LDY CURPSL               ;Cursor Position LO byte
F0D2: 60        RTS                     
;
;================================================================================
; Write character to screen
;================================================================================
;
WRITESCRN:
F0D3: 8D 02 02  STA TEMCHR               ;Temporary character store
F0D6: 48        PHA                     
F0D7: 8A        TXA                     
F0D8: 48        PHA                     
F0D9: 98        TYA                     
F0DA: 48        PHA                     
F0DB: 20 C6 F0  JSR SETCUR              
F0DE: 20 ED F0  JSR WRITE               
F0E1: 20 29 F2  JSR PUTCUR              
F0E4: 4C 37 F2  JMP L_F237              
;================================================================================
; Restore character under cursor
;================================================================================
;
L_F0E7:
F0E7: AD 01 02  LDA CURCHR               ;Character under cursor
F0EA: 91 F6     STA (ZP_F6),Y            ;Zero Page $F6
F0EC: 60        RTS                     
;
;
WRITE:
F0ED: AD 02 02  LDA TEMCHR               ;Temporary character store
F0F0: C9 0D     CMP #$0D                ; Is it <CR>?
F0F2: D0 30     BNE L_F124              
;================================================================================
; Perform <CR>
;================================================================================
;
CR:
F0F4: 20 5C F2  JSR START               
F0F7: C0 CA     CPY #$CA                
F0F9: F0 09     BEQ L_F104              
F0FB: 20 88 F1  JSR L_F188              
F0FE: A9 60     LDA #$60                
F100: D1 F6     CMP (ZP_F6),Y            ;Zero Page $F6
F102: F0 07     BEQ L_F10B              
;
L_F104:
F104: 20 75 F1  JSR L_F175              
F107: A9 60     LDA #$60                
F109: 91 F6     STA (ZP_F6),Y            ;Zero Page $F6
;
L_F10B:
F10B: C8        INY                     
F10C: C6 D6     DEC ZP_D6                ;Zero Page $D6
;
L_F10E:
F10E: A9 00     LDA #$00                
F110: 85 F6     STA ZP_F6                ;Zero Page $F6
F112: 8D 26 02  STA QUOFLAG              ;Quotes Flag
F115: 8D 27 02  STA EDFLAG               ;Edit Flag. Get a line from the screen
F118: 8D 22 02  STA MOVFLAG              ;Move Flag for insert Routine
F11B: 8D 23 02  STA INSFLAG              ;Insert Flag
F11E: A9 A1     LDA #$A1                
F120: 8D 2C 02  STA CURSYM               ;Cursor Symbol
F123: 60        RTS                     
;
;
L_F124:
F124: C9 22     CMP #$22                ; Is is <QUOTE>?
F126: D0 03     BNE L_F12B              
F128: 20 01 F5  JSR TQUOTE              
;
L_F12B:
F12B: C9 20     CMP #$20                ; Is it <SPACE>?
F12D: 90 17     BCC L_F146              
F12F: C9 3F     CMP #$3F                ; Is is <?>?
F131: D0 10     BNE L_F143              
F133: 20 4A F2  JSR SOL                 
F136: D0 09     BNE L_F141              
F138: A9 FF     LDA #$FF                
F13A: C5 88     CMP ZP_88                ;Zero Page $88
F13C: F0 03     BEQ L_F141              
F13E: 8D 04 02  STA ERRORFL              ;FF = Run error
;
L_F141:
F141: A9 3F     LDA #$3F                
;
L_F143:
F143: 4C EF F1  JMP PUT1                
;
L_F146:
F146: C9 0E     CMP #$0E                
F148: F0 0B     BEQ L_F155              
F14A: 2C 26 02  BIT QUOFLAG              ;Quotes Flag
F14D: 10 06     BPL L_F155              
F14F: 20 D6 F1  JSR L_F1D6              
F152: 4C EF F1  JMP PUT1                
;
L_F155:
F155: C9 08     CMP #$08                
F157: D0 18     BNE LNFD                
F159: 88        DEY                     
F15A: 20 4A F2  JSR SOL                 
F15D: B0 11     BCS L_F170              
F15F: 20 3F F2  JSR SOP                 
F162: B0 09     BCS L_F16D              
F164: A9 D0     LDA #$D0                
F166: 85 F3     STA ZP_F3                ;Zero Page $F3
F168: A0 7B     LDY #$7B                
F16A: 4C B0 F2  JMP SCRDN               
;
L_F16D:
F16D: 20 50 F2  JSR OLDLN               
;
L_F170:
F170: 60        RTS                     
;
;================================================================================
; Perform Line Feed
;================================================================================
;
LNFD:
F171: C9 0A     CMP #$0A                
F173: D0 21     BNE L_F196              
;
L_F175:
F175: A5 D6     LDA ZP_D6                ;Zero Page $D6
F177: D0 18     BNE L_F191              
F179: C0 CA     CPY #$CA                
F17B: 90 0B     BCC L_F188              
F17D: A9 D3     LDA #$D3                
F17F: C5 F7     CMP ZP_F7                ;Zero Page $F7
F181: D0 05     BNE L_F188              
F183: 85 F3     STA ZP_F3                ;Zero Page $F3
F185: 4C 65 F2  JMP SCROLL              
;
L_F188:
F188: 98        TYA                     
F189: 18        CLC                     
F18A: 69 40     ADC #$40                
F18C: A8        TAY                     
F18D: 90 02     BCC L_F191              
F18F: E6 F7     INC ZP_F7                ;Zero Page $F7
;
L_F191:
F191: A9 00     LDA #$00                
F193: 85 D6     STA ZP_D6                ;Zero Page $D6
F195: 60        RTS                     
;
;
L_F196:
F196: C9 09     CMP #$09                
F198: F0 57     BEQ L_F1F1              
;
VTAB:
F19A: C9 1A     CMP #$1A                
F19C: D0 1B     BNE L_F1B9              
F19E: C0 8B     CPY #$8B                
F1A0: B0 0B     BCS L_F1AD              
F1A2: A9 D0     LDA #$D0                
F1A4: C5 F7     CMP ZP_F7                ;Zero Page $F7
F1A6: D0 05     BNE L_F1AD              
F1A8: 85 F3     STA ZP_F3                ;Zero Page $F3
F1AA: 4C B0 F2  JMP SCRDN               
;
L_F1AD:
F1AD: 48        PHA                     
F1AE: 98        TYA                     
F1AF: 38        SEC                     
F1B0: E9 40     SBC #$40                
F1B2: A8        TAY                     
F1B3: B0 02     BCS L_F1B7              
F1B5: C6 F7     DEC ZP_F7                ;Zero Page $F7
;
L_F1B7:
F1B7: 68        PLA                     
F1B8: 60        RTS                     
;
;
L_F1B9:
F1B9: C9 0C     CMP #$0C                
F1BB: D0 03     BNE L_F1C0              
F1BD: 4C A2 F0  JMP CLS                 
;
L_F1C0:
F1C0: C9 01     CMP #$01                
F1C2: D0 03     BNE L_F1C7              
F1C4: 4C B8 F0  JMP HOCUR               
;
L_F1C7:
F1C7: C9 0E     CMP #$0E                
F1C9: D0 03     BNE L_F1CE              
F1CB: 4C AB F6  JMP DELETE              
;
L_F1CE:
F1CE: C9 1E     CMP #$1E                
F1D0: D0 03     BNE BELL                
F1D2: 4C E7 F5  JMP INSERT              
;
BELL:
F1D5: 60        RTS                     
;
;
L_F1D6:
F1D6: 85 EC     STA ZP_EC                ;Zero Page $EC
F1D8: 98        TYA                     
F1D9: 48        PHA                     
F1DA: 20 CB F5  JSR L_F5CB              
F1DD: 20 D4 F5  JSR L_F5D4              
F1E0: C9 FF     CMP #$FF                
F1E2: F0 06     BEQ L_F1EA              
F1E4: 20 C2 F5  JSR L_F5C2              
F1E7: 20 E2 F5  JSR L_F5E2              
;
L_F1EA:
F1EA: 68        PLA                     
F1EB: A8        TAY                     
F1EC: A5 EC     LDA ZP_EC                ;Zero Page $EC
F1EE: 60        RTS                     
;
;
PUT1:
F1EF: 91 F6     STA (ZP_F6),Y            ;Zero Page $F6
;
L_F1F1:
F1F1: C8        INY                     
F1F2: 20 1A F2  JSR EOL                 
F1F5: 90 0D     BCC L_F204              
F1F7: 20 0C F2  JSR EOP                 
F1FA: B0 09     BCS L_F205              
F1FC: 20 22 F2  JSR NEWLN               
;
L_F1FF:
F1FF: A9 20     LDA #$20                
F201: 91 F6     STA (ZP_F6),Y            ;Zero Page $F6
F203: C8        INY                     
;
L_F204:
F204: 60        RTS                     
;
;
L_F205:
F205: A0 CA     LDY #$CA                
F207: 20 65 F2  JSR SCROLL              
F20A: D0 F3     BNE L_F1FF              
;
EOP:
F20C: 48        PHA                     
F20D: 98        TYA                     
F20E: C9 FB     CMP #$FB                
F210: B0 02     BCS L_F214              
F212: 68        PLA                     
F213: 60        RTS                     
;
;
L_F214:
F214: A5 F7     LDA ZP_F7                ;Zero Page $F7
F216: C9 D3     CMP #$D3                
F218: 68        PLA                     
F219: 60        RTS                     
;
;================================================================================
; Move to End of Line
;================================================================================
;
EOL:
F21A: 48        PHA                     
F21B: 98        TYA                     
F21C: 29 3F     AND #$3F                
F21E: C9 3B     CMP #$3B                
F220: 68        PLA                     
F221: 60        RTS                     
;
;
NEWLN:
F222: 20 5C F2  JSR START               
F225: 20 88 F1  JSR L_F188              
F228: 60        RTS                     
;
;================================================================================
; Put Cursor On screen
;================================================================================
;
PUTCUR:
F229: B1 F6     LDA (ZP_F6),Y            ;Zero Page $F6
;
L_F22B:
F22B: 8D 01 02  STA CURCHR               ;Character under cursor
;
L_F22E:
F22E: A5 F7     LDA ZP_F7                ;Zero Page $F7
F230: 8D 25 02  STA CURPSH               ;Cursor Position HI byte
F233: 8C 00 02  STY CURPSL               ;Cursor Position LO byte
F236: 60        RTS                     
;
;
L_F237:
F237: 68        PLA                     
F238: A8        TAY                     
F239: 68        PLA                     
F23A: AA        TAX                     
F23B: 68        PLA                     
F23C: 4C 6C FF  JMP L_FF6C              
;
SOP:
F23F: C0 4B     CPY #$4B                
F241: B0 06     BCS L_F249              
F243: 48        PHA                     
F244: A5 F7     LDA ZP_F7                ;Zero Page $F7
F246: C9 D1     CMP #$D1                
F248: 68        PLA                     
;
L_F249:
F249: 60        RTS                     
;
;
SOL:
F24A: 98        TYA                     
F24B: 29 3F     AND #$3F                
F24D: C9 0B     CMP #$0B                
F24F: 60        RTS                     
;
;
OLDLN:
F250: 48        PHA                     
F251: 98        TYA                     
F252: 38        SEC                     
F253: E9 10     SBC #$10                
F255: A8        TAY                     
F256: B0 02     BCS L_F25A              
F258: C6 F7     DEC ZP_F7                ;Zero Page $F7
;
L_F25A:
F25A: 68        PLA                     
F25B: 60        RTS                     
;
;
START:
F25C: 48        PHA                     
F25D: 98        TYA                     
F25E: 29 C0     AND #$C0                
F260: 09 0A     ORA #$0A                
F262: A8        TAY                     
F263: 68        PLA                     
F264: 60        RTS                     
;
;================================================================================
; Scroll
;================================================================================
;
SCROLL:
F265: 20 8A F2  JSR L_F28A              
F268: AC 06 02  LDY MEM0206              ;Low Mem $0206
F26B: F0 03     BEQ L_F270              
F26D: 20 0D F7  JSR DELAY               
;
L_F270:
F270: 20 3E F8  JSR FCHAR               
F273: D0 12     BNE L_F287              
F275: A9 03     LDA #$03                
F277: 85 E5     STA ZP_E5                ;Zero Page $E5
F279: A0 C0     LDY #$C0                
;
L_F27B:
F27B: 20 0D F7  JSR DELAY               
F27E: 20 3E F8  JSR FCHAR               
F281: D0 04     BNE L_F287              
F283: C6 E5     DEC ZP_E5                ;Zero Page $E5
F285: D0 F4     BNE L_F27B              
;
L_F287:
F287: 4C C6 F0  JMP SETCUR              
;
L_F28A:
F28A: 20 2E F2  JSR L_F22E              
;
L_F28D:
F28D: A0 40     LDY #$40                
F28F: A2 CF     LDX #$CF                
F291: A9 40     LDA #$40                
F293: 85 F8     STA ZP_F8                ;Zero Page $F8
;
L_F295:
F295: E8        INX                     
F296: 86 F9     STX ZP_F9                ;Zero Page $F9
F298: 86 F7     STX ZP_F7                ;Zero Page $F7
;
L_F29A:
F29A: B1 F8     LDA (ZP_F8),Y            ;Zero Page $F8
F29C: 91 F6     STA (ZP_F6),Y            ;Zero Page $F6
F29E: C8        INY                     
F29F: D0 F9     BNE L_F29A              
F2A1: E0 D3     CPX #$D3                
F2A3: D0 F0     BNE L_F295              
F2A5: A9 60     LDA #$60                
F2A7: 88        DEY                     
;
L_F2A8:
F2A8: 91 F6     STA (ZP_F6),Y            ;Zero Page $F6
F2AA: 88        DEY                     
F2AB: C0 C0     CPY #$C0                
F2AD: D0 F9     BNE L_F2A8              
F2AF: 60        RTS                     
;
;================================================================================
; Screen Down
;================================================================================
;
SCRDN:
F2B0: 20 B5 F2  JSR L_F2B5              
F2B3: 30 D2     BMI L_F287              
;
L_F2B5:
F2B5: 20 2E F2  JSR L_F22E              
;
L_F2B8:
F2B8: 98        TYA                     
F2B9: 29 C0     AND #$C0                
F2BB: 85 F4     STA ZP_F4                ;Zero Page $F4
F2BD: A0 BF     LDY #$BF                
F2BF: A9 40     LDA #$40                
F2C1: 85 F8     STA ZP_F8                ;Zero Page $F8
F2C3: A2 D4     LDX #$D4                
;
L_F2C5:
F2C5: CA        DEX                     
F2C6: 86 F7     STX ZP_F7                ;Zero Page $F7
F2C8: 86 F9     STX ZP_F9                ;Zero Page $F9
;
L_F2CA:
F2CA: B1 F6     LDA (ZP_F6),Y            ;Zero Page $F6
F2CC: 91 F8     STA (ZP_F8),Y            ;Zero Page $F8
F2CE: C4 F4     CPY ZP_F4                ;Zero Page $F4
F2D0: D0 04     BNE L_F2D6              
F2D2: E4 F3     CPX ZP_F3                ;Zero Page $F3
F2D4: F0 07     BEQ L_F2DD              
;
L_F2D6:
F2D6: 88        DEY                     
F2D7: C0 FF     CPY #$FF                
F2D9: D0 EF     BNE L_F2CA              
F2DB: F0 E8     BEQ L_F2C5              
;
L_F2DD:
F2DD: A9 3F     LDA #$3F                
F2DF: 85 F8     STA ZP_F8                ;Zero Page $F8
F2E1: AD 00 02  LDA CURPSL               ;Cursor Position LO byte
F2E4: 29 C0     AND #$C0                
F2E6: A8        TAY                     
F2E7: A9 60     LDA #$60                
;
L_F2E9:
F2E9: 91 F8     STA (ZP_F8),Y            ;Zero Page $F8
F2EB: C6 F8     DEC ZP_F8                ;Zero Page $F8
F2ED: 10 FA     BPL L_F2E9              
F2EF: 60        RTS                     
;
;
L_F2F0:
F2F0: AD 04 02  LDA ERRORFL              ;FF = Run error
F2F3: F0 16     BEQ L_F30B              
F2F5: 30 08     BMI L_F2FF              
F2F7: A9 00     LDA #$00                
F2F9: 8D 04 02  STA ERRORFL              ;FF = Run error
F2FC: 4C 74 A2  JMP BASICWARM           
;
L_F2FF:
F2FF: A9 00     LDA #$00                
F301: C5 8C     CMP $8C                 
F303: D0 03     BNE L_F308              
F305: 4C D2 F4  JMP PRERRL              
;
L_F308:
F308: 8D 04 02  STA ERRORFL              ;FF = Run error
;
L_F30B:
F30B: 98        TYA                     
F30C: 48        PHA                     
F30D: 8A        TXA                     
F30E: 48        PHA                     
;
L_F30F:
F30F: 20 69 F3  JSR KBRD                
F312: C9 0D     CMP #$0D                
F314: F0 17     BEQ L_F32D              
F316: C9 0E     CMP #$0E                
F318: F0 09     BEQ L_F323              
F31A: 2C 26 02  BIT QUOFLAG              ;Quotes Flag
F31D: 30 1E     BMI L_F33D              
F31F: C9 20     CMP #$20                
F321: B0 1A     BCS L_F33D              
;
L_F323:
F323: 2C 27 02  BIT EDFLAG               ;Edit Flag. Get a line from the screen
F326: 30 27     BMI L_F34F              
F328: CE 27 02  DEC EDFLAG               ;Edit Flag. Get a line from the screen
F32B: D0 22     BNE L_F34F              
;
L_F32D:
F32D: 20 C6 F0  JSR SETCUR              
F330: AD 01 02  LDA CURCHR               ;Character under cursor
F333: 91 F6     STA (ZP_F6),Y            ;Zero Page $F6
F335: 2C 27 02  BIT EDFLAG               ;Edit Flag. Get a line from the screen
F338: 10 1E     BPL L_F358              
F33A: 4C 7D F5  JMP L_F57D              
;
L_F33D:
F33D: 68        PLA                     
F33E: 48        PHA                     
F33F: AA        TAX                     
F340: AD 13 02  LDA MEM0213              ;Low Mem $0213
F343: 95 13     STA ZP_13,X              ;Zero Page $13
F345: 86 DF     STX ZP_DF                ;Zero Page $DF
F347: E8        INX                     
F348: 68        PLA                     
F349: 8A        TXA                     
F34A: 48        PHA                     
F34B: E0 47     CPX #$47                
F34D: F0 09     BEQ L_F358              
;
L_F34F:
F34F: AD 13 02  LDA MEM0213              ;Low Mem $0213
F352: 20 D3 F0  JSR WRITESCRN           
F355: 4C 0F F3  JMP L_F30F              
;
L_F358:
F358: 68        PLA                     
F359: AA        TAX                     
F35A: 68        PLA                     
F35B: A8        TAY                     
;
L_F35C:
F35C: 86 F8     STX ZP_F8                ;Zero Page $F8
F35E: BA        TSX                     
F35F: E8        INX                     
F360: E8        INX                     
F361: E8        INX                     
F362: E8        INX                     
F363: 9A        TXS                     
F364: A6 F8     LDX ZP_F8                ;Zero Page $F8
F366: 4C 66 A8  JMP BASICA866           
;================================================================================
; Keyboard Scanner
;================================================================================
;
KBRD:
F369: 98        TYA                     
F36A: 48        PHA                     
F36B: 8A        TXA                     
F36C: 48        PHA                     
;
KEYLOOP:
F36D: A9 02     LDA #$02                
;
L_F36F:
F36F: 20 16 F7  JSR INVWR               
F372: 20 1E F7  JSR RDCOL               
F375: D0 1A     BNE L_F391              
F377: 0A        ASL                     
F378: D0 F5     BNE L_F36F              
F37A: A9 01     LDA #$01                
F37C: 20 16 F7  JSR INVWR               
F37F: 20 27 F7  JSR RDCOLA              
F382: 85 EC     STA ZP_EC                ;Zero Page $EC
F384: 29 FE     AND #$FE                
F386: C9 44     CMP #$44                
F388: D0 3A     BNE L_F3C4              
F38A: A9 FF     LDA #$FF                
F38C: 8D 24 02  STA SKBFLAG              ;FF next character returns BASIC command
F38F: D0 DC     BNE KEYLOOP             
;
L_F391:
F391: 4A        LSR                     
F392: 20 FE F3  JSR L_F3FE              
F395: 98        TYA                     
F396: 8D 13 02  STA MEM0213              ;Low Mem $0213
F399: 0A        ASL                     
F39A: 0A        ASL                     
F39B: 0A        ASL                     
F39C: 38        SEC                     
F39D: ED 13 02  SBC MEM0213              ;Low Mem $0213
F3A0: 8D 13 02  STA MEM0213              ;Low Mem $0213
F3A3: 8A        TXA                     
F3A4: 4A        LSR                     
F3A5: 20 FE F3  JSR L_F3FE              
F3A8: D0 1A     BNE L_F3C4              
F3AA: 98        TYA                     
F3AB: 18        CLC                     
F3AC: 6D 13 02  ADC MEM0213              ;Low Mem $0213
F3AF: A8        TAY                     
F3B0: B9 8D FE  LDA KEYMATRIX,Y         ; Convert keyboard matrix position to ASCII code
F3B3: CD 15 02  CMP MEM0215              ;Low Mem $0215 (Last keypress?)
F3B6: D0 3B     BNE L_F3F3              
F3B8: CE 14 02  DEC MEM0214              ;Low Mem $0214
F3BB: F0 48     BEQ L_F405              
F3BD: A0 05     LDY #$05                
F3BF: 20 0D F7  JSR DELAY               
F3C2: F0 A9     BEQ KEYLOOP             
;
L_F3C4:
F3C4: 24 D7     BIT $D7                 
F3C6: 30 21     BMI L_F3E9              
F3C8: C6 D7     DEC $D7                 
F3CA: D0 22     BNE L_F3EE              
F3CC: CE 40 02  DEC MEM0240              ;Low Mem $0240
F3CF: D0 18     BNE L_F3E9              
F3D1: 20 C6 F0  JSR SETCUR              
F3D4: AD 01 02  LDA CURCHR               ;Character under cursor
F3D7: D1 F6     CMP (ZP_F6),Y            ;Zero Page $F6
F3D9: F0 04     BEQ L_F3DF              
F3DB: 91 F6     STA (ZP_F6),Y            ;Zero Page $F6
F3DD: D0 05     BNE L_F3E4              
;
L_F3DF:
F3DF: AD 2C 02  LDA CURSYM               ;Cursor Symbol
F3E2: 91 F6     STA (ZP_F6),Y            ;Zero Page $F6
;
L_F3E4:
F3E4: A9 06     LDA #$06                
F3E6: 8D 40 02  STA MEM0240              ;Low Mem $0240
;
L_F3E9:
F3E9: AD 2D 02  LDA CURFL                ;Cursor Flash Rate/suppress
F3EC: 85 D7     STA $D7                 
;
L_F3EE:
F3EE: A9 00     LDA #$00                
F3F0: 8D 16 02  STA MEM0216              ;Low Mem $0216
;
L_F3F3:
F3F3: 8D 15 02  STA MEM0215              ;Low Mem $0215 (Last keypress?)
F3F6: A9 02     LDA #$02                
F3F8: 8D 14 02  STA MEM0214              ;Low Mem $0214
F3FB: 4C 6D F3  JMP KEYLOOP             
;
L_F3FE:
F3FE: A0 08     LDY #$08                
;
L_F400:
F400: 88        DEY                     
F401: 0A        ASL                     
F402: 90 FC     BCC L_F400              
F404: 60        RTS                     
;
;
L_F405:
F405: 48        PHA                     
F406: 98        TYA                     
F407: 48        PHA                     
F408: 20 C6 F0  JSR SETCUR              
F40B: AD 01 02  LDA CURCHR               ;Character under cursor
F40E: 91 F6     STA (ZP_F6),Y            ;Zero Page $F6
F410: 68        PLA                     
F411: A8        TAY                     
F412: 68        PLA                     
F413: 2C 24 02  BIT SKBFLAG              ;FF next character returns BASIC command
F416: 10 03     BPL L_F41B              
F418: 4C 17 F5  JMP L_F517              
;
L_F41B:
F41B: A2 96     LDX #$96                
F41D: CD 16 02  CMP MEM0216              ;Low Mem $0216
F420: D0 02     BNE L_F424              
F422: A2 14     LDX #$14                
;
L_F424:
F424: 8E 14 02  STX MEM0214              ;Low Mem $0214
F427: 8D 16 02  STA MEM0216              ;Low Mem $0216
F42A: A5 EC     LDA ZP_EC                ;Zero Page $EC
F42C: 29 40     AND #$40                
F42E: D0 36     BNE L_F466              
F430: A5 EC     LDA ZP_EC                ;Zero Page $EC
F432: 4A        LSR                     
F433: 29 03     AND #$03                
F435: D0 12     BNE L_F449              
F437: 90 05     BCC L_F43E              
F439: AD 15 02  LDA MEM0215              ;Low Mem $0215 (Last keypress?)
F43C: D0 5E     BNE L_F49C              
;
L_F43E:
F43E: AD 15 02  LDA MEM0215              ;Low Mem $0215 (Last keypress?)
F441: C9 41     CMP #$41                
F443: 90 57     BCC L_F49C              
F445: 09 20     ORA #$20                
F447: D0 53     BNE L_F49C              
;
L_F449:
F449: AD 15 02  LDA MEM0215              ;Low Mem $0215 (Last keypress?)
F44C: C9 41     CMP #$41                
F44E: B0 4C     BCS L_F49C              
F450: C9 20     CMP #$20                
F452: F0 48     BEQ L_F49C              
F454: C9 0D     CMP #$0D                
F456: F0 44     BEQ L_F49C              
F458: C9 30     CMP #$30                
F45A: F0 40     BEQ L_F49C              
F45C: B0 04     BCS L_F462              
F45E: 09 10     ORA #$10                
F460: D0 3A     BNE L_F49C              
;
L_F462:
F462: 29 2F     AND #$2F                
F464: D0 36     BNE L_F49C              
;
L_F466:
F466: AD 15 02  LDA MEM0215              ;Low Mem $0215 (Last keypress?)
F469: 29 1F     AND #$1F                
F46B: C9 10     CMP #$10                
F46D: D0 08     BNE L_F477              
F46F: A9 FF     LDA #$FF                
F471: 4D 4E 02  EOR PRFLAG               ;Print on/off Flag
F474: 4C 6D F3  JMP KEYLOOP             
;
L_F477:
F477: C9 11     CMP #$11                
F479: D0 05     BNE L_F480              
F47B: 20 01 F5  JSR TQUOTE              
F47E: D0 10     BNE L_F490              
;
L_F480:
F480: C9 14     CMP #$14                
F482: D0 05     BNE L_F489              
F484: 20 2D F7  JSR TSW                 
F487: D0 07     BNE L_F490              
;
L_F489:
F489: C9 04     CMP #$04                
F48B: D0 06     BNE L_F493              
F48D: 20 50 F5  JSR OPEN                
;
L_F490:
F490: 4C 6D F3  JMP KEYLOOP             
;
L_F493:
F493: C9 05     CMP #$05                
F495: D0 05     BNE L_F49C              
F497: 20 A7 F4  JSR EXTEND              
F49A: 30 F4     BMI L_F490              
;
L_F49C:
F49C: 8D 13 02  STA MEM0213              ;Low Mem $0213
F49F: 68        PLA                     
F4A0: AA        TAX                     
F4A1: 68        PLA                     
F4A2: A8        TAY                     
F4A3: AD 13 02  LDA MEM0213              ;Low Mem $0213
F4A6: 60        RTS                     
;
;================================================================================
; Extend
;================================================================================
;
EXTEND:
F4A7: A2 03     LDX #$03                
;
L_F4A9:
F4A9: BD B2 F4  LDA TABLE1,X            
F4AC: 95 C5     STA ZP_C5,X              ;Zero Page $C5
F4AE: CA        DEX                     
F4AF: 10 F8     BPL L_F4A9              
F4B1: 60        RTS                     
;
;
TABLE1:
F4B2: 4C B6 F4  JMP L_F4B6              
F4B5: EA        NOP                     
;
L_F4B6:
F4B6: 08        PHP                     
F4B7: 48        PHA                     
F4B8: A5 C4     LDA ZP_C4                ;Zero Page $C4
F4BA: D0 11     BNE L_F4CD              
F4BC: A6 DF     LDX ZP_DF                ;Zero Page $DF
F4BE: D0 0D     BNE L_F4CD              
F4C0: 68        PLA                     
F4C1: 48        PHA                     
F4C2: C9 41     CMP #$41                
F4C4: 90 07     BCC L_F4CD              
F4C6: C9 5B     CMP #$5B                
F4C8: B0 03     BCS L_F4CD              
F4CA: 20 48 02  JSR EXTVEC               ;BASIC command external Vector
;
L_F4CD:
F4CD: 68        PLA                     
F4CE: 28        PLP                     
F4CF: 4C F7 BC  JMP BASICBCF7           
;================================================================================
; Print Error Line
;================================================================================
;
PRERRL:
F4D2: A5 88     LDA ZP_88                ;Zero Page $88
F4D4: A4 87     LDY ZP_87                ;Zero Page $87
F4D6: 20 C1 AF  JSR BASICAFC1           
F4D9: 20 6E B9  JSR BASICB96E           
F4DC: A2 00     LDX #$00                
F4DE: A0 39     LDY #$39                
;
L_F4E0:
F4E0: B9 8E F7  LDA BANNER,Y            
F4E3: F0 06     BEQ L_F4EB              
F4E5: 95 13     STA ZP_13,X              ;Zero Page $13
F4E7: C8        INY                     
F4E8: E8        INX                     
F4E9: D0 F5     BNE L_F4E0              
;
L_F4EB:
F4EB: A0 00     LDY #$00                
;
L_F4ED:
F4ED: B9 01 01  LDA STACK+1,Y            ;Stack offset 1
F4F0: F0 07     BEQ L_F4F9              
F4F2: 9D 13 00  STA ZP_13,X              ;Zero Page $13
F4F5: C8        INY                     
F4F6: E8        INX                     
F4F7: D0 F4     BNE L_F4ED              
;
L_F4F9:
F4F9: A9 7F     LDA #$7F                
F4FB: 8D 04 02  STA ERRORFL              ;FF = Run error
F4FE: 4C 5C F3  JMP L_F35C              
;================================================================================
; Toggle Quote Mode
;================================================================================
;
TQUOTE:
F501: 48        PHA                     
F502: A9 FF     LDA #$FF                
F504: 4D 26 02  EOR QUOFLAG              ;Quotes Flag
F507: 8D 26 02  STA QUOFLAG              ;Quotes Flag
F50A: 10 04     BPL L_F510              
F50C: A9 17     LDA #$17                ; <NE Arrow> symbol Cursor
F50E: D0 02     BNE L_F512              
;
L_F510:
F510: A9 A1     LDA #$A1                ; Reverse <SPACE> Cursor
;
L_F512:
F512: 8D 2C 02  STA CURSYM               ;Cursor Symbol
F515: 68        PLA                     
F516: 60        RTS                     
;
;
L_F517:
F517: B9 BE FE  LDA TABLE7,Y            
F51A: A8        TAY                     
F51B: BA        TSX                     
F51C: BD 05 01  LDA STACK+5,X            ;Stack offset 5
F51F: AA        TAX                     
;
L_F520:
F520: B9 84 A0  LDA BASICTBLE1,Y        
F523: 48        PHA                     
F524: 29 7F     AND #$7F                
F526: 20 69 FF  JSR L_FF69              
F529: 68        PLA                     
F52A: 30 06     BMI L_F532              
F52C: 95 13     STA ZP_13,X              ;Zero Page $13
F52E: C8        INY                     
F52F: E8        INX                     
F530: D0 EE     BNE L_F520              
;
L_F532:
F532: 29 7F     AND #$7F                
F534: 95 13     STA ZP_13,X              ;Zero Page $13
F536: E8        INX                     
F537: C9 24     CMP #$24                
F539: D0 08     BNE L_F543              
F53B: A9 28     LDA #$28                
F53D: 20 69 FF  JSR L_FF69              
F540: 95 13     STA ZP_13,X              ;Zero Page $13
F542: E8        INX                     
;
L_F543:
F543: 8A        TXA                     
F544: BA        TSX                     
F545: 9D 05 01  STA STACK+5,X            ;Stack offset 5
F548: A9 00     LDA #$00                
F54A: 8D 24 02  STA SKBFLAG              ;FF next character returns BASIC command
F54D: 4C 6D F3  JMP KEYLOOP             
;
OPEN:
F550: 20 C6 F0  JSR SETCUR              
F553: AD 25 02  LDA CURPSH               ;Cursor Position HI byte
F556: C0 CB     CPY #$CB                
F558: 90 05     BCC L_F55F              
F55A: C9 D3     CMP #$D3                
F55C: 90 01     BCC L_F55F              
;
L_F55E:
F55E: 60        RTS                     
;
;
L_F55F:
F55F: 85 F3     STA ZP_F3                ;Zero Page $F3
F561: 20 B8 F2  JSR L_F2B8              
F564: A9 60     LDA #$60                
F566: 8D 01 02  STA CURCHR               ;Character under cursor
F569: D0 F3     BNE L_F55E              
;
GETST:
F56B: 20 5C F2  JSR START               
F56E: B1 F6     LDA (ZP_F6),Y            ;Zero Page $F6
F570: C9 60     CMP #$60                
F572: F0 08     BEQ L_F57C              
F574: 20 3F F2  JSR SOP                 
F577: 90 03     BCC L_F57C              
F579: 20 AD F1  JSR L_F1AD              
;
L_F57C:
F57C: 60        RTS                     
;
;
L_F57D:
F57D: A2 00     LDX #$00                
F57F: 20 6B F5  JSR GETST               
;
L_F582:
F582: C8        INY                     
F583: 20 1A F2  JSR EOL                 
F586: 90 0D     BCC L_F595              
F588: E0 31     CPX #$31                
F58A: B0 2E     BCS L_F5BA              
F58C: 20 0C F2  JSR EOP                 
F58F: B0 29     BCS L_F5BA              
F591: 20 22 F2  JSR NEWLN               
F594: C8        INY                     
;
L_F595:
F595: B1 F6     LDA (ZP_F6),Y            ;Zero Page $F6
F597: C9 60     CMP #$60                
F599: F0 1F     BEQ L_F5BA              
F59B: 85 EC     STA ZP_EC                ;Zero Page $EC
F59D: 98        TYA                     
F59E: 48        PHA                     
F59F: 20 C2 F5  JSR L_F5C2              
F5A2: 20 D4 F5  JSR L_F5D4              
F5A5: C9 FF     CMP #$FF                
F5A7: F0 06     BEQ L_F5AF              
F5A9: 20 CB F5  JSR L_F5CB              
F5AC: 20 E2 F5  JSR L_F5E2              
;
L_F5AF:
F5AF: 68        PLA                     
F5B0: A8        TAY                     
F5B1: A5 EC     LDA ZP_EC                ;Zero Page $EC
F5B3: 95 13     STA ZP_13,X              ;Zero Page $13
F5B5: E8        INX                     
F5B6: E0 47     CPX #$47                
F5B8: D0 C8     BNE L_F582              
;
L_F5BA:
F5BA: 8A        TXA                     
F5BB: BA        TSX                     
F5BC: 9D 01 01  STA STACK+1,X            ;Stack offset 1
F5BF: 4C 58 F3  JMP L_F358              
;
L_F5C2:
F5C2: A9 D7     LDA #$D7                
F5C4: 85 FE     STA ZP_FE                ;Zero Page $FE
F5C6: A9 FF     LDA #$FF                
F5C8: 85 FF     STA ZP_FF                ;Zero Page $FF
F5CA: 60        RTS                     
;
;
L_F5CB:
F5CB: A9 49     LDA #$49                
F5CD: 85 FE     STA ZP_FE                ;Zero Page $FE
F5CF: A9 FF     LDA #$FF                
F5D1: 85 FF     STA ZP_FF                ;Zero Page $FF
F5D3: 60        RTS                     
;
;
L_F5D4:
F5D4: A5 EC     LDA ZP_EC                ;Zero Page $EC
F5D6: A0 06     LDY #$06                
;
L_F5D8:
F5D8: D1 FE     CMP (ZP_FE),Y            ;Zero Page $FE
F5DA: F0 05     BEQ L_F5E1              
F5DC: 88        DEY                     
F5DD: 10 F9     BPL L_F5D8              
F5DF: A9 FF     LDA #$FF                
;
L_F5E1:
F5E1: 60        RTS                     
;
;
L_F5E2:
F5E2: B1 FE     LDA (ZP_FE),Y            ;Zero Page $FE
F5E4: 85 EC     STA ZP_EC                ;Zero Page $EC
F5E6: 60        RTS                     
;
;
INSERT:
F5E7: 2C 23 02  BIT INSFLAG              ;Insert Flag
F5EA: 30 41     BMI L_F62D              
F5EC: 20 6B F5  JSR GETST               
F5EF: A2 00     LDX #$00                
F5F1: C8        INY                     
;
L_F5F2:
F5F2: A9 60     LDA #$60                
F5F4: D1 F6     CMP (ZP_F6),Y            ;Zero Page $F6
F5F6: D0 0D     BNE L_F605              
F5F8: E0 00     CPX #$00                
F5FA: F0 31     BEQ L_F62D              
F5FC: E0 47     CPX #$47                
F5FE: 90 32     BCC L_F632              
;
L_F600:
F600: CE 23 02  DEC INSFLAG              ;Insert Flag
F603: D0 28     BNE L_F62D              
;
L_F605:
F605: E0 47     CPX #$47                
F607: B0 F7     BCS L_F600              
F609: E8        INX                     
F60A: C8        INY                     
F60B: 20 1A F2  JSR EOL                 
F60E: 90 E2     BCC L_F5F2              
F610: E0 31     CPX #$31                
F612: B0 19     BCS L_F62D              
F614: 20 0C F2  JSR EOP                 
F617: B0 18     BCS L_F631              
F619: 20 22 F2  JSR NEWLN               
F61C: B1 F6     LDA (ZP_F6),Y            ;Zero Page $F6
F61E: C8        INY                     
F61F: C9 60     CMP #$60                
F621: F0 05     BEQ L_F628              
F623: CE 22 02  DEC MOVFLAG              ;Move Flag for insert Routine
F626: D0 CA     BNE L_F5F2              
;
L_F628:
F628: 20 50 F2  JSR OLDLN               
F62B: D0 05     BNE L_F632              
;
L_F62D:
F62D: 20 C6 F0  JSR SETCUR              
F630: 60        RTS                     
;
;
L_F631:
F631: 88        DEY                     
;
L_F632:
F632: A5 F7     LDA ZP_F7                ;Zero Page $F7
F634: 48        PHA                     
F635: 98        TYA                     
F636: 48        PHA                     
F637: B1 F6     LDA (ZP_F6),Y            ;Zero Page $F6
F639: 48        PHA                     
F63A: C8        INY                     
F63B: 20 1A F2  JSR EOL                 
F63E: 90 44     BCC L_F684              
F640: 2C 22 02  BIT MOVFLAG              ;Move Flag for insert Routine
F643: 30 20     BMI L_F665              
F645: CE 22 02  DEC MOVFLAG              ;Move Flag for insert Routine
F648: 20 0C F2  JSR EOP                 
F64B: B0 1E     BCS L_F66B              
F64D: 20 22 F2  JSR NEWLN               
F650: A5 F7     LDA ZP_F7                ;Zero Page $F7
F652: 85 F3     STA ZP_F3                ;Zero Page $F3
F654: 48        PHA                     
F655: 98        TYA                     
F656: 48        PHA                     
F657: 20 B8 F2  JSR L_F2B8              
F65A: 68        PLA                     
F65B: A8        TAY                     
F65C: 68        PLA                     
F65D: 85 F7     STA ZP_F7                ;Zero Page $F7
F65F: A9 20     LDA #$20                
F661: 91 F6     STA (ZP_F6),Y            ;Zero Page $F6
F663: D0 03     BNE L_F668              
;
L_F665:
F665: 20 22 F2  JSR NEWLN               
;
L_F668:
F668: C8        INY                     
F669: D0 19     BNE L_F684              
;
L_F66B:
F66B: AD 00 02  LDA CURPSL               ;Cursor Position LO byte
F66E: 38        SEC                     
F66F: E9 40     SBC #$40                
F671: 8D 00 02  STA CURPSL               ;Cursor Position LO byte
F674: BA        TSX                     
F675: A9 BA     LDA #$BA                
F677: 9D 02 01  STA $0102,X             
F67A: 20 8D F2  JSR L_F28D              
F67D: A0 CA     LDY #$CA                
F67F: A9 20     LDA #$20                
F681: 91 F6     STA (ZP_F6),Y            ;Zero Page $F6
F683: C8        INY                     
;
L_F684:
F684: 68        PLA                     
F685: 91 F6     STA (ZP_F6),Y            ;Zero Page $F6
F687: 68        PLA                     
F688: A8        TAY                     
F689: 68        PLA                     
F68A: 85 F7     STA ZP_F7                ;Zero Page $F7
F68C: CC 00 02  CPY CURPSL               ;Cursor Position LO byte
F68F: F0 0B     BEQ L_F69C              
F691: 88        DEY                     
F692: 20 4A F2  JSR SOL                 
F695: B0 9B     BCS L_F632              
F697: 20 50 F2  JSR OLDLN               
F69A: D0 96     BNE L_F632              
;
L_F69C:
F69C: AD 01 02  LDA CURCHR               ;Character under cursor
F69F: C8        INY                     
F6A0: 91 F6     STA (ZP_F6),Y            ;Zero Page $F6
F6A2: A9 20     LDA #$20                
F6A4: 88        DEY                     
F6A5: 91 F6     STA (ZP_F6),Y            ;Zero Page $F6
F6A7: 8D 01 02  STA CURCHR               ;Character under cursor
F6AA: 60        RTS                     
;
;
DELETE:
F6AB: 20 E7 F0  JSR L_F0E7              
F6AE: 20 6B F5  JSR GETST               
F6B1: C8        INY                     
F6B2: CC 00 02  CPY CURPSL               ;Cursor Position LO byte
F6B5: F0 3F     BEQ L_F6F6              
F6B7: 20 C6 F0  JSR SETCUR              
F6BA: CE 00 02  DEC CURPSL               ;Cursor Position LO byte
F6BD: AD 00 02  LDA CURPSL               ;Cursor Position LO byte
F6C0: 29 3F     AND #$3F                
F6C2: C9 0A     CMP #$0A                
F6C4: D0 0E     BNE L_F6D4              
F6C6: AD 00 02  LDA CURPSL               ;Cursor Position LO byte
F6C9: 38        SEC                     
F6CA: E9 10     SBC #$10                
F6CC: 8D 00 02  STA CURPSL               ;Cursor Position LO byte
F6CF: B0 03     BCS L_F6D4              
F6D1: CE 25 02  DEC CURPSH               ;Cursor Position HI byte
;
L_F6D4:
F6D4: A5 F7     LDA ZP_F7                ;Zero Page $F7
F6D6: 48        PHA                     
F6D7: 98        TYA                     
F6D8: 48        PHA                     
F6D9: B1 F6     LDA (ZP_F6),Y            ;Zero Page $F6
F6DB: 48        PHA                     
F6DC: 88        DEY                     
F6DD: 20 4A F2  JSR SOL                 
F6E0: B0 08     BCS L_F6EA              
F6E2: 20 3F F2  JSR SOP                 
F6E5: 90 21     BCC L_F708              
F6E7: 20 50 F2  JSR OLDLN               
;
L_F6EA:
F6EA: 68        PLA                     
F6EB: 91 F6     STA (ZP_F6),Y            ;Zero Page $F6
F6ED: C9 60     CMP #$60                
F6EF: D0 06     BNE L_F6F7              
F6F1: 68        PLA                     
F6F2: 68        PLA                     
F6F3: 20 C6 F0  JSR SETCUR              
;
L_F6F6:
F6F6: 60        RTS                     
;
;
L_F6F7:
F6F7: 68        PLA                     
F6F8: A8        TAY                     
F6F9: 68        PLA                     
F6FA: 85 F7     STA ZP_F7                ;Zero Page $F7
F6FC: C8        INY                     
F6FD: 20 1A F2  JSR EOL                 
F700: 90 D2     BCC L_F6D4              
F702: 20 22 F2  JSR NEWLN               
F705: C8        INY                     
F706: D0 CC     BNE L_F6D4              
;
L_F708:
F708: 68        PLA                     
F709: 68        PLA                     
F70A: 4C 2B F2  JMP L_F22B              
;
DELAY:
F70D: A2 C8     LDX #$C8                
;
L_F70F:
F70F: CA        DEX                     
F710: D0 FD     BNE L_F70F              
F712: 88        DEY                     
F713: D0 F8     BNE DELAY               
F715: 60        RTS                     
;
;================================================================================
; Invert Keyboard Data and Write to Keyboard Port
;================================================================================
;
INVWR:
F716: 49 FF     EOR #$FF                
F718: 8D 00 DF  STA KEYBOARD             ;Keyboard Port
F71B: 49 FF     EOR #$FF                
F71D: 60        RTS                     
;
;================================================================================
; Read Keyboard COLUMN
;================================================================================
;
RDCOL:
F71E: 48        PHA                     
F71F: 20 27 F7  JSR RDCOLA              
F722: AA        TAX                     
F723: 68        PLA                     
F724: CA        DEX                     
F725: E8        INX                     
F726: 60        RTS                     
;
;================================================================================
; Read Keyboard Port and Invert Data
;================================================================================
;
RDCOLA:
F727: AD 00 DF  LDA KEYBOARD             ;Keyboard Port
F72A: 49 FF     EOR #$FF                
F72C: 60        RTS                     
;
;
TSW:
F72D: 2C 4F 02  BIT TPFlag               ;Tape on/off Flag
F730: 30 0B     BMI L_F73D              
;
L_F732:
F732: A9 51     LDA #$51                
F734: 8D 00 E0  STA ACIA2                ;Relocated ACIA
F737: 0A        ASL                     
F738: 8D 4F 02  STA TPFlag               ;Tape on/off Flag
F73B: D0 08     BNE L_F745              
;
L_F73D:
F73D: A9 11     LDA #$11                
F73F: 8D 00 E0  STA ACIA2                ;Relocated ACIA
F742: 8D 4F 02  STA TPFlag               ;Tape on/off Flag
;
L_F745:
F745: 60        RTS                     
;
;
FINTAPE:
F746: A9 00     LDA #$00                
F748: 8D 05 02  STA SCRSPD               ;Scroll delay
F74B: 8D 03 02  STA LDFLAG               ;Load Flag
F74E: 85 E0     STA ZP_E0                ;Zero Page $E0
F750: 20 3D F7  JSR L_F73D              
F753: 60        RTS                     
;
F754: A2 06     LDX #$06                
;
L_F756:
F756: A9 00     LDA #$00                
;
L_F758:
F758: 20 5F F7  JSR TAPOUT              
F75B: CA        DEX                     
F75C: D0 FA     BNE L_F758              
F75E: 60        RTS                     
;
;
TAPOUT:
F75F: 48        PHA                     
F760: 20 3E F8  JSR FCHAR               
F763: F0 0E     BEQ L_F773              
F765: 68        PLA                     
;
TAP1:
F766: 48        PHA                     
;
L_F767:
F767: AD 00 E0  LDA ACIA2                ;Relocated ACIA
F76A: 4A        LSR                     
F76B: 4A        LSR                     
F76C: 90 F9     BCC L_F767              
F76E: 68        PLA                     
F76F: 8D 01 E0  STA ACIA2+1              ;Relocated ACIA offset 1
F772: 60        RTS                     
;
;
L_F773:
F773: 20 46 F7  JSR FINTAPE             
F776: 68        PLA                     
F777: 60        RTS                     
;
;
TABLE5:
F778:           !BYTE $BA,$FF,$D3,$F0,$9A,$FF,$8B,$FF ;unknown
F780:           !BYTE $96,$FF           ;unknown
 
;================================================================================
; Show Power On Banner
;================================================================================
;
MSGOUT:
F782: BD 8E F7  LDA BANNER,X            
F785: F0 06     BEQ L_F78D              
F787: 20 69 FF  JSR L_FF69              
F78A: E8        INX                     
F78B: D0 F5     BNE MSGOUT              
;
L_F78D:
F78D: 60        RTS                     
;
;
BANNER:
F78E:           !TEXT "WEMON (C)1981.",$0D,$0A,"M/C/W/D/U ?",$00 ;Power-on Prompt
 
;================================================================================
; Messages
;================================================================================
;
L_F7AA:
F7AA:           !TEXT $0D,$0A,"FOUND ",$00 ;Tape 'found '
 
;
L_F7B3:
F7B3:           !TEXT $0D,$0A,"LOADING",$00 ;Tape 'Loading' message
 
;
L_F7BD:
F7BD:           !TEXT $0D,$0A,"SAVING ",$00 ;Tape 'Saving' message
 
;
L_F7C7:
F7C7:           !TEXT "LIST",$00        ;List message
 
;================================================================================
; Check for valid HEX digit
;================================================================================
;
HEXCHECK:
F7CC: C9 30     CMP #$30                
F7CE: 30 12     BMI L_F7E2              
F7D0: C9 3A     CMP #$3A                
F7D2: 30 0B     BMI L_F7DF              
F7D4: C9 41     CMP #$41                
F7D6: 30 0A     BMI L_F7E2              
F7D8: C9 47     CMP #$47                
F7DA: 10 06     BPL L_F7E2              
F7DC: 38        SEC                     
F7DD: E9 07     SBC #$07                
;
L_F7DF:
F7DF: 29 0F     AND #$0F                
F7E1: 60        RTS                     
;
;
L_F7E2:
F7E2: A9 80     LDA #$80                
F7E4: 60        RTS                     
;
;
MONPRINT:
F7E5: A2 03     LDX #$03                
;
L_F7E7:
F7E7: 20 F6 F7  JSR NXTDIG              
F7EA: E0 02     CPX #$02                
F7EC: D0 04     BNE L_F7F2              
F7EE: 20 13 F8  JSR SPC1                
F7F1: CA        DEX                     
;
L_F7F2:
F7F2: CA        DEX                     
F7F3: 10 F2     BPL L_F7E7              
F7F5: 60        RTS                     
;
;
NXTDIG:
F7F6: B5 FA     LDA ZP_FA,X              ;Zero Page $FA
F7F8: 4A        LSR                     
F7F9: 4A        LSR                     
F7FA: 4A        LSR                     
F7FB: 4A        LSR                     
F7FC: 20 18 F8  JSR HEXIT               
F7FF: B5 FA     LDA ZP_FA,X              ;Zero Page $FA
F801: 4C 18 F8  JMP HEXIT               
;
CRLF:
F804: A9 0D     LDA #$0D                
F806: 20 69 FF  JSR L_FF69              
F809: A9 0A     LDA #$0A                
F80B: 4C 69 FF  JMP L_FF69              
;
SPC2:
F80E: A9 20     LDA #$20                
F810: 20 69 FF  JSR L_FF69              
;
SPC1:
F813: A9 20     LDA #$20                
;
L_F815:
F815: 4C 69 FF  JMP L_FF69              
;
HEXIT:
F818: 29 0F     AND #$0F                
F81A: 09 30     ORA #$30                
F81C: C9 3A     CMP #$3A                
F81E: 30 03     BMI L_F823              
F820: 18        CLC                     
F821: 69 07     ADC #$07                
;
L_F823:
F823: D0 F0     BNE L_F815              
;
ROTCHR:
F825: A0 04     LDY #$04                
F827: 0A        ASL                     
F828: 0A        ASL                     
F829: 0A        ASL                     
F82A: 0A        ASL                     
;
L_F82B:
F82B: 2A        ROL                     
F82C: 36 FA     ROL ZP_FA,X              ;Zero Page $FA
F82E: 36 FB     ROL ZP_FB,X              ;Zero Page $FB
F830: 88        DEY                     
F831: D0 F8     BNE L_F82B              
F833: 60        RTS                     
;
;
MINPUT:
F834: A5 E0     LDA ZP_E0                ;Zero Page $E0
F836: F0 03     BEQ L_F83B              
F838: 4C C2 FF  JMP L_FFC2              
;
L_F83B:
F83B: 4C 69 F3  JMP KBRD                
;
FCHAR:
F83E: A9 FD     LDA #$FD                
F840: 8D 00 DF  STA KEYBOARD             ;Keyboard Port
F843: A9 10     LDA #$10                
F845: 2C 00 DF  BIT KEYBOARD             ;Keyboard Port
F848: 60        RTS                     
;
;================================================================================
; NMI Entry Point
;================================================================================
;
MONBRK:
F849: 8D 45 02  STA MEM0245              ;Low Mem $0245
F84C: 68        PLA                     
F84D: 48        PHA                     
F84E: 29 10     AND #$10                
F850: D0 06     BNE L_F858              
F852: 6C 32 02  JMP ($0232)             
;================================================================================
; Monitor Entry Point
;================================================================================
;
L_F855:
F855: 8D 45 02  STA MEM0245              ;Low Mem $0245
;
L_F858:
F858: 8E 46 02  STX MEM0246              ;Low Mem $0246
F85B: 8C 47 02  STY $0247               
F85E: 68        PLA                     
F85F: 8D 43 02  STA MEM0243              ;Low Mem $0243
F862: 68        PLA                     
F863: 38        SEC                     
F864: E9 01     SBC #$01                
F866: 8D 42 02  STA MEM0242              ;Low Mem $0242
F869: 68        PLA                     
F86A: E9 00     SBC #$00                
F86C: 8D 41 02  STA REGMAP               ;Register map
F86F: BA        TSX                     
F870: 8E 44 02  STX MEM0244              ;Low Mem $0244
F873: A2 28     LDX #$28                
F875: 9A        TXS                     
F876: D8        CLD                     
F877: 20 A2 F0  JSR CLS                 
;
L_F87A:
F87A: 20 69 F9  JSR PUTREG              
F87D: 20 B8 F0  JSR HOCUR               
F880: 20 DD F2  JSR L_F2DD              
;
L_F883:
F883: 20 69 F3  JSR KBRD                
F886: 85 EF     STA ZP_EF                ;Zero Page $EF
F888: 20 69 FF  JSR L_FF69              
F88B: 20 13 F8  JSR SPC1                
F88E: A5 EF     LDA ZP_EF                ;Zero Page $EF
F890: C9 52     CMP #$52                
F892: D0 09     BNE L_F89D              
F894: 20 B8 F0  JSR HOCUR               
F897: 20 F5 F8  JSR L_F8F5              
F89A: 4C 01 F9  JMP L_F901              
;
L_F89D:
F89D: C9 53     CMP #$53                
F89F: D0 03     BNE L_F8A4              
F8A1: 4C BF FC  JMP L_FCBF              
;
L_F8A4:
F8A4: C9 4C     CMP #$4C                
F8A6: D0 03     BNE L_F8AB              
F8A8: 4C FD FC  JMP L_FCFD              
;
L_F8AB:
F8AB: 20 D5 F9  JSR GETPAR              
F8AE: A5 EF     LDA ZP_EF                ;Zero Page $EF
F8B0: C9 4D     CMP #$4D                
F8B2: D0 17     BNE L_F8CB              
F8B4: A5 E1     LDA ZP_E1                ;Zero Page $E1
F8B6: F0 0D     BEQ L_F8C5              
F8B8: C9 03     CMP #$03                
F8BA: F0 06     BEQ L_F8C2              
F8BC: 20 F0 F8  JSR L_F8F0              
F8BF: 4C AB F8  JMP L_F8AB              
;
L_F8C2:
F8C2: 4C 5F FA  JMP L_FA5F              
;
L_F8C5:
F8C5: 20 04 F8  JSR CRLF                
F8C8: 4C 07 FE  JMP L_FE07              
;
L_F8CB:
F8CB: C9 46     CMP #$46                
F8CD: D0 03     BNE L_F8D2              
F8CF: 4C A0 FA  JMP L_FAA0              
;
L_F8D2:
F8D2: C9 42     CMP #$42                
F8D4: D0 03     BNE L_F8D9              
F8D6: 4C EC FA  JMP L_FAEC              
;
L_F8D9:
F8D9: C9 56     CMP #$56                
F8DB: D0 03     BNE L_F8E0              
F8DD: 4C 21 FA  JMP L_FA21              
;
L_F8E0:
F8E0: C9 47     CMP #$47                
F8E2: D0 03     BNE L_F8E7              
F8E4: 4C BA FA  JMP L_FABA              
;
L_F8E7:
F8E7: 20 29 02  JSR USERVEC              ;User Vector
;
L_F8EA:
F8EA: 20 04 F8  JSR CRLF                
F8ED: 4C 83 F8  JMP L_F883              
;
L_F8F0:
F8F0: A9 0D     LDA #$0D                
F8F2: 20 69 FF  JSR L_FF69              
;
L_F8F5:
F8F5: 20 DD F2  JSR L_F2DD              
F8F8: A5 EF     LDA ZP_EF                ;Zero Page $EF
F8FA: 20 69 FF  JSR L_FF69              
F8FD: 20 13 F8  JSR SPC1                
F900: 60        RTS                     
;
;
L_F901:
F901: A9 00     LDA #$00                
F903: 85 EA     STA ZP_EA                ;Zero Page $EA
;
L_F905:
F905: 20 1B FA  JSR L_FA1B              
F908: D0 06     BNE L_F910              
F90A: 20 69 FF  JSR L_FF69              
F90D: 4C 7A F8  JMP L_F87A              
;
L_F910:
F910: C9 30     CMP #$30                
F912: B0 12     BCS L_F926              
F914: C9 20     CMP #$20                
F916: F0 08     BEQ L_F920              
F918: C9 08     CMP #$08                
F91A: F0 04     BEQ L_F920              
F91C: C9 18     CMP #$18                
F91E: D0 03     BNE L_F923              
;
L_F920:
F920: 20 69 FF  JSR L_FF69              
;
L_F923:
F923: 4C 01 F9  JMP L_F901              
;
L_F926:
F926: 85 E9     STA ZP_E9                ;Zero Page $E9
F928: A5 EA     LDA ZP_EA                ;Zero Page $EA
F92A: D0 14     BNE L_F940              
F92C: AD 00 02  LDA CURPSL               ;Cursor Position LO byte
F92F: 38        SEC                     
F930: E9 40     SBC #$40                
F932: A0 06     LDY #$06                
;
L_F934:
F934: D9 CD F9  CMP TABLE4,Y            
F937: F0 05     BEQ L_F93E              
F939: 88        DEY                     
F93A: 10 F8     BPL L_F934              
F93C: 30 C3     BMI L_F901              
;
L_F93E:
F93E: 84 EB     STY $EB                 
;
L_F940:
F940: A5 E9     LDA ZP_E9                ;Zero Page $E9
F942: 20 CC F7  JSR HEXCHECK            
F945: 30 BE     BMI L_F905              
F947: 48        PHA                     
F948: A5 E9     LDA ZP_E9                ;Zero Page $E9
F94A: 20 69 FF  JSR L_FF69              
F94D: 68        PLA                     
F94E: A6 EA     LDX ZP_EA                ;Zero Page $EA
F950: D0 04     BNE L_F956              
F952: A2 02     LDX #$02                
F954: 86 EA     STX ZP_EA                ;Zero Page $EA
;
L_F956:
F956: A2 00     LDX #$00                
F958: 20 25 F8  JSR ROTCHR              
F95B: C6 EA     DEC ZP_EA                ;Zero Page $EA
F95D: D0 A6     BNE L_F905              
F95F: A6 EB     LDX $EB                 
F961: A5 FA     LDA ZP_FA                ;Zero Page $FA
F963: 9D 41 02  STA REGMAP,X             ;Register map
F966: 4C 01 F9  JMP L_F901              
;
PUTREG:
F969: A0 0C     LDY #$0C                
F96B: A2 00     LDX #$00                
;
L_F96D:
F96D: BD A8 F9  LDA TABLE3,X            
F970: F0 07     BEQ L_F979              
F972: 99 00 D0  STA VIDRAM,Y             ;Video Character RAM
F975: C8        INY                     
F976: E8        INX                     
F977: D0 F4     BNE L_F96D              
;
L_F979:
F979: A2 00     LDX #$00                
;
L_F97B:
F97B: BC CD F9  LDY TABLE4,X            
F97E: F0 06     BEQ L_F986              
F980: 20 87 F9  JSR L_F987              
F983: E8        INX                     
F984: D0 F5     BNE L_F97B              
;
L_F986:
F986: 60        RTS                     
;
;
L_F987:
F987: BD 41 02  LDA REGMAP,X             ;Register map
F98A: 4A        LSR                     
F98B: 4A        LSR                     
F98C: 4A        LSR                     
F98D: 4A        LSR                     
F98E: 20 99 F9  JSR L_F999              
F991: BD 41 02  LDA REGMAP,X             ;Register map
F994: C8        INY                     
F995: 20 99 F9  JSR L_F999              
F998: 60        RTS                     
;
;
L_F999:
F999: 29 0F     AND #$0F                
F99B: 09 30     ORA #$30                
F99D: C9 3A     CMP #$3A                
F99F: 30 03     BMI L_F9A4              
F9A1: 18        CLC                     
F9A2: 69 07     ADC #$07                
;
L_F9A4:
F9A4: 99 00 D0  STA VIDRAM,Y             ;Video Character RAM
F9A7: 60        RTS                     
;
;
TABLE3:
;
TABLE4:
F9A8:           !TEXT "PC      FR    SP    ACC    XR    YR ",$00	;MCM header
F9CD:		!TEXT $0F,$11,$17,$1D,"$*0",$00				;MCM header
 
;
GETPAR:
F9D5: A2 05     LDX #$05                
F9D7: A9 00     LDA #$00                
;
L_F9D9:
F9D9: 95 FA     STA ZP_FA,X              ;Zero Page $FA
F9DB: CA        DEX                     
F9DC: 10 FB     BPL L_F9D9              
F9DE: E8        INX                     
F9DF: 86 E1     STX ZP_E1                ;Zero Page $E1
F9E1: 86 E3     STX ZP_E3                ;Zero Page $E3
F9E3: 20 1B FA  JSR L_FA1B              
F9E6: F0 32     BEQ L_FA1A              
F9E8: E6 E1     INC ZP_E1                ;Zero Page $E1
F9EA: D0 05     BNE L_F9F1              
;
L_F9EC:
F9EC: 20 1B FA  JSR L_FA1B              
F9EF: F0 29     BEQ L_FA1A              
;
L_F9F1:
F9F1: C9 2D     CMP #$2D                
F9F3: F0 15     BEQ L_FA0A              
F9F5: 85 E4     STA ZP_E4                ;Zero Page $E4
F9F7: 20 CC F7  JSR HEXCHECK            
F9FA: 30 F0     BMI L_F9EC              
F9FC: 48        PHA                     
F9FD: A5 E4     LDA ZP_E4                ;Zero Page $E4
F9FF: 20 69 FF  JSR L_FF69              
FA02: 68        PLA                     
FA03: A6 E3     LDX ZP_E3                ;Zero Page $E3
FA05: 20 25 F8  JSR ROTCHR              
FA08: F0 E2     BEQ L_F9EC              
;
L_FA0A:
FA0A: 20 69 FF  JSR L_FF69              
FA0D: A5 E1     LDA ZP_E1                ;Zero Page $E1
FA0F: 0A        ASL                     
FA10: 85 E3     STA ZP_E3                ;Zero Page $E3
FA12: E6 E1     INC ZP_E1                ;Zero Page $E1
FA14: A6 E1     LDX ZP_E1                ;Zero Page $E1
FA16: E0 04     CPX #$04                
FA18: D0 D2     BNE L_F9EC              
;
L_FA1A:
FA1A: 60        RTS                     
;
;
L_FA1B:
FA1B: 20 69 F3  JSR KBRD                
FA1E: C9 0D     CMP #$0D                
FA20: 60        RTS                     
;
;
L_FA21:
FA21: 20 38 FD  JSR L_FD38              
FA24: A6 E1     LDX ZP_E1                ;Zero Page $E1
FA26: E0 02     CPX #$02                
FA28: F0 06     BEQ L_FA30              
FA2A: 20 F0 F8  JSR L_F8F0              
FA2D: 4C 21 FA  JMP L_FA21              
;
L_FA30:
FA30: A2 07     LDX #$07                
FA32: 86 E4     STX ZP_E4                ;Zero Page $E4
FA34: 20 04 F8  JSR CRLF                
FA37: A2 03     LDX #$03                
FA39: 20 F6 F7  JSR NXTDIG              
FA3C: CA        DEX                     
FA3D: 20 F6 F7  JSR NXTDIG              
FA40: 20 13 F8  JSR SPC1                
;
L_FA43:
FA43: A0 00     LDY #$00                
FA45: B1 FC     LDA (ZP_FC),Y            ;Zero Page $FC
FA47: 85 FA     STA ZP_FA                ;Zero Page $FA
FA49: A2 00     LDX #$00                
FA4B: 20 F6 F7  JSR NXTDIG              
FA4E: 20 13 F8  JSR SPC1                
FA51: 20 33 FF  JSR L_FF33              
FA54: B0 06     BCS L_FA5C              
FA56: C6 E4     DEC ZP_E4                ;Zero Page $E4
FA58: 10 E9     BPL L_FA43              
FA5A: 30 D4     BMI L_FA30              
;
L_FA5C:
FA5C: 4C EA F8  JMP L_F8EA              
;
L_FA5F:
FA5F: 20 04 F8  JSR CRLF                
FA62: A2 00     LDX #$00                
FA64: 86 DE     STX ZP_DE                ;Zero Page $DE
FA66: 86 DF     STX ZP_DF                ;Zero Page $DF
;
L_FA68:
FA68: A0 00     LDY #$00                
FA6A: A5 FA     LDA ZP_FA                ;Zero Page $FA
FA6C: D1 FC     CMP (ZP_FC),Y            ;Zero Page $FC
FA6E: D0 17     BNE L_FA87              
FA70: 20 E5 F7  JSR MONPRINT            
FA73: 20 04 F8  JSR CRLF                
FA76: F8        SED                     
FA77: 18        CLC                     
FA78: AD DE 00  LDA ZP_00DE              ;Zero Page $DE #####################
FA7B: 69 01     ADC #$01                
FA7D: 85 DE     STA ZP_DE                ;Zero Page $DE
FA7F: 90 06     BCC L_FA87              
FA81: A5 DF     LDA ZP_DF                ;Zero Page $DF
FA83: 69 00     ADC #$00                
FA85: 85 DF     STA ZP_DF                ;Zero Page $DF
;
L_FA87:
FA87: D8        CLD                     
FA88: 20 33 FF  JSR L_FF33              
FA8B: 90 DB     BCC L_FA68              
FA8D: A5 DE     LDA ZP_DE                ;Zero Page $DE
FA8F: 85 FA     STA ZP_FA                ;Zero Page $FA
FA91: A5 DF     LDA ZP_DF                ;Zero Page $DF
FA93: 85 FB     STA ZP_FB                ;Zero Page $FB
FA95: A2 01     LDX #$01                
;
L_FA97:
FA97: 20 F6 F7  JSR NXTDIG              
FA9A: CA        DEX                     
FA9B: 10 FA     BPL L_FA97              
FA9D: 4C EA F8  JMP L_F8EA              
;
L_FAA0:
FAA0: A5 E1     LDA ZP_E1                ;Zero Page $E1
FAA2: C9 03     CMP #$03                
FAA4: F0 06     BEQ L_FAAC              
FAA6: 20 F0 F8  JSR L_F8F0              
FAA9: 4C A0 FA  JMP L_FAA0              
;
L_FAAC:
FAAC: A0 00     LDY #$00                
FAAE: A5 FA     LDA ZP_FA                ;Zero Page $FA
FAB0: 91 FC     STA (ZP_FC),Y            ;Zero Page $FC
FAB2: 20 33 FF  JSR L_FF33              
FAB5: 90 F5     BCC L_FAAC              
FAB7: 4C EA F8  JMP L_F8EA              
;
L_FABA:
FABA: A5 E1     LDA ZP_E1                ;Zero Page $E1
FABC: F0 0A     BEQ L_FAC8              
FABE: A5 FA     LDA ZP_FA                ;Zero Page $FA
FAC0: 8D 42 02  STA MEM0242              ;Low Mem $0242
FAC3: A5 FB     LDA ZP_FB                ;Zero Page $FB
FAC5: 8D 41 02  STA REGMAP               ;Register map
;
L_FAC8:
FAC8: AD 41 02  LDA REGMAP               ;Register map
FACB: 48        PHA                     
FACC: AD 42 02  LDA MEM0242              ;Low Mem $0242
FACF: 8D 41 02  STA REGMAP               ;Register map
FAD2: 68        PLA                     
FAD3: 8D 42 02  STA MEM0242              ;Low Mem $0242
FAD6: AE 44 02  LDX MEM0244              ;Low Mem $0244
FAD9: 9A        TXS                     
FADA: AE 46 02  LDX MEM0246              ;Low Mem $0246
FADD: AC 47 02  LDY $0247               
FAE0: AD 43 02  LDA MEM0243              ;Low Mem $0243
FAE3: 48        PHA                     
FAE4: AD 45 02  LDA MEM0245              ;Low Mem $0245
FAE7: 28        PLP                     
FAE8: 58        CLI                     
FAE9: 6C 41 02  JMP (REGMAP)             ;Register map
;
L_FAEC:
FAEC: A5 E1     LDA ZP_E1                ;Zero Page $E1
FAEE: C9 03     CMP #$03                
FAF0: F0 06     BEQ L_FAF8              
FAF2: 20 F0 F8  JSR L_F8F0              
FAF5: 4C EC FA  JMP L_FAEC              
;
L_FAF8:
FAF8: A5 FD     LDA ZP_FD                ;Zero Page $FD
FAFA: C5 FB     CMP ZP_FB                ;Zero Page $FB
FAFC: 90 1A     BCC L_FB18              
FAFE: A5 FC     LDA ZP_FC                ;Zero Page $FC
FB00: C5 FA     CMP ZP_FA                ;Zero Page $FA
FB02: 90 14     BCC L_FB18              
FB04: A0 00     LDY #$00                
;
L_FB06:
FB06: B1 FC     LDA (ZP_FC),Y            ;Zero Page $FC
FB08: 91 FA     STA (ZP_FA),Y            ;Zero Page $FA
FB0A: E6 FA     INC ZP_FA                ;Zero Page $FA
FB0C: D0 02     BNE L_FB10              
FB0E: E6 FB     INC ZP_FB                ;Zero Page $FB
;
L_FB10:
FB10: 20 33 FF  JSR L_FF33              
FB13: 90 F1     BCC L_FB06              
;
L_FB15:
FB15: 4C EA F8  JMP L_F8EA              
;
L_FB18:
FB18: 20 42 FD  JSR ADJPTR              
FB1B: A2 00     LDX #$00                
FB1D: 20 42 FB  JSR L_FB42              
FB20: A2 01     LDX #$01                
;
L_FB22:
FB22: B5 F8     LDA ZP_F8,X              ;Zero Page $F8
FB24: 49 FF     EOR #$FF                
FB26: 95 F8     STA ZP_F8,X              ;Zero Page $F8
FB28: CA        DEX                     
FB29: 10 F7     BPL L_FB22              
;
L_FB2B:
FB2B: C6 FF     DEC ZP_FF                ;Zero Page $FF
FB2D: C6 FB     DEC ZP_FB                ;Zero Page $FB
FB2F: A0 FF     LDY #$FF                
;
L_FB31:
FB31: B1 FE     LDA (ZP_FE),Y            ;Zero Page $FE
FB33: 91 FA     STA (ZP_FA),Y            ;Zero Page $FA
FB35: E6 F8     INC ZP_F8                ;Zero Page $F8
FB37: D0 04     BNE L_FB3D              
FB39: E6 F9     INC ZP_F9                ;Zero Page $F9
FB3B: F0 D8     BEQ L_FB15              
;
L_FB3D:
FB3D: 88        DEY                     
FB3E: D0 F1     BNE L_FB31              
FB40: F0 E9     BEQ L_FB2B              
;
L_FB42:
FB42: A5 F8     LDA ZP_F8                ;Zero Page $F8
FB44: 18        CLC                     
FB45: 65 FA     ADC ZP_FA                ;Zero Page $FA
FB47: 95 FA     STA ZP_FA,X              ;Zero Page $FA
FB49: A5 F9     LDA ZP_F9                ;Zero Page $F9
FB4B: 65 FB     ADC ZP_FB                ;Zero Page $FB
FB4D: 95 FB     STA ZP_FB,X              ;Zero Page $FB
FB4F: 60        RTS                     
;
;
L_FB50:
FB50: 48        PHA                     
FB51: 98        TYA                     
FB52: 48        PHA                     
FB53: 8A        TXA                     
FB54: 48        PHA                     
FB55: 20 96 FC  JSR GETNAM              
FB58: 20 B2 FC  JSR L_FCB2              
FB5B: 20 CB FB  JSR L_FBCB              
;
L_FB5E:
FB5E: B1 FC     LDA (ZP_FC),Y            ;Zero Page $FC
FB60: D0 0F     BNE L_FB71              
FB62: CA        DEX                     
FB63: D0 0E     BNE L_FB73              
FB65: 20 5F F7  JSR TAPOUT              
FB68: 20 46 F7  JSR FINTAPE             
FB6B: 68        PLA                     
FB6C: AA        TAX                     
FB6D: 68        PLA                     
FB6E: A8        TAY                     
FB6F: 68        PLA                     
FB70: 60        RTS                     
;
;
L_FB71:
FB71: A2 03     LDX #$03                
;
L_FB73:
FB73: 20 5F F7  JSR TAPOUT              
FB76: 20 C4 FB  JSR L_FBC4              
FB79: D0 E3     BNE L_FB5E              
;
DELAY5S:
FB7B: A9 14     LDA #$14                
FB7D: 85 DD     STA $DD                 
;
L_FB7F:
FB7F: A0 FF     LDY #$FF                
FB81: 20 0D F7  JSR DELAY               
FB84: C6 DD     DEC $DD                 
FB86: D0 F7     BNE L_FB7F              
FB88: 60        RTS                     
;
;
L_FB89:
FB89: 20 96 FC  JSR GETNAM              
FB8C: 20 D6 FB  JSR L_FBD6              
FB8F: A6 E2     LDX ZP_E2                ;Zero Page $E2
FB91: D0 04     BNE L_FB97              
FB93: CE 03 02  DEC LDFLAG               ;Load Flag
FB96: 60        RTS                     
;
;
L_FB97:
FB97: 20 CB FB  JSR L_FBCB              
;
L_FB9A:
FB9A: 20 C7 FF  JSR L_FFC7              
FB9D: D0 1C     BNE L_FBBB              
FB9F: CA        DEX                     
FBA0: D0 1B     BNE L_FBBD              
FBA2: 91 FC     STA (ZP_FC),Y            ;Zero Page $FC
FBA4: 20 C4 FB  JSR L_FBC4              
FBA7: A5 FC     LDA ZP_FC                ;Zero Page $FC
FBA9: 85 7B     STA ZP_7B                ;Zero Page $7B
FBAB: A5 FD     LDA ZP_FD                ;Zero Page $FD
FBAD: 85 7C     STA ZP_7C                ;Zero Page $7C
FBAF: A9 01     LDA #$01                
FBB1: 85 79     STA ZP_79                ;Zero Page $79
FBB3: A9 03     LDA #$03                
FBB5: 85 7A     STA ZP_7A                ;Zero Page $7A
FBB7: 20 46 F7  JSR FINTAPE             
FBBA: 60        RTS                     
;
;
L_FBBB:
FBBB: A2 03     LDX #$03                
;
L_FBBD:
FBBD: 91 FC     STA (ZP_FC),Y            ;Zero Page $FC
FBBF: 20 C4 FB  JSR L_FBC4              
FBC2: D0 D6     BNE L_FB9A              
;
L_FBC4:
FBC4: E6 FC     INC ZP_FC                ;Zero Page $FC
FBC6: D0 02     BNE L_FBCA              
FBC8: E6 FD     INC ZP_FD                ;Zero Page $FD
;
L_FBCA:
FBCA: 60        RTS                     
;
;
L_FBCB:
FBCB: A9 03     LDA #$03                
FBCD: 85 FD     STA ZP_FD                ;Zero Page $FD
FBCF: AA        TAX                     
FBD0: A9 00     LDA #$00                
FBD2: 85 FC     STA ZP_FC                ;Zero Page $FC
FBD4: A8        TAY                     
FBD5: 60        RTS                     
;
;
L_FBD6:
FBD6: 20 32 F7  JSR L_F732              
FBD9: A6 E0     LDX ZP_E0                ;Zero Page $E0
FBDB: D0 04     BNE L_FBE1              
FBDD: A6 E2     LDX ZP_E2                ;Zero Page $E2
FBDF: F0 17     BEQ L_FBF8              
;
L_FBE1:
FBE1: 20 13 FC  JSR L_FC13              
FBE4: 20 3F FC  JSR GETNAM1             
FBE7: A2 1C     LDX #$1C                
FBE9: 20 82 F7  JSR MSGOUT              
FBEC: 20 23 FF  JSR L_FF23              
FBEF: A6 E2     LDX ZP_E2                ;Zero Page $E2
FBF1: F0 05     BEQ L_FBF8              
FBF3: 20 28 FC  JSR COMPNAME            
FBF6: B0 E9     BCS L_FBE1              
;
L_FBF8:
FBF8: A2 25     LDX #$25                
FBFA: 20 82 F7  JSR MSGOUT              
FBFD: 85 E0     STA ZP_E0                ;Zero Page $E0
FBFF: 20 04 F8  JSR CRLF                
FC02: 60        RTS                     
;
;
L_FC03:
FC03: A2 20     LDX #$20                
FC05: A9 16     LDA #$16                
;
L_FC07:
FC07: 20 66 F7  JSR TAP1                
FC0A: CA        DEX                     
FC0B: 10 FA     BPL L_FC07              
FC0D: A9 2A     LDA #$2A                
FC0F: 20 66 F7  JSR TAP1                
FC12: 60        RTS                     
;
;
L_FC13:
FC13: A2 00     LDX #$00                
;
L_FC15:
FC15: 20 C7 FF  JSR L_FFC7              
FC18: C9 16     CMP #$16                
FC1A: D0 03     BNE L_FC1F              
FC1C: E8        INX                     
FC1D: D0 F6     BNE L_FC15              
;
L_FC1F:
FC1F: C9 2A     CMP #$2A                
FC21: D0 F0     BNE L_FC13              
FC23: E0 09     CPX #$09                
FC25: 90 EC     BCC L_FC13              
FC27: 60        RTS                     
;
;
COMPNAME:
FC28: A2 00     LDX #$00                
;
L_FC2A:
FC2A: BD 34 02  LDA NAM1,X               ;Filename to search for or to write
FC2D: C9 2A     CMP #$2A                
FC2F: F0 0C     BEQ L_FC3D              
FC31: DD 3A 02  CMP NAM2,X               ;Filename that was found
FC34: F0 02     BEQ L_FC38              
FC36: 38        SEC                     
FC37: 60        RTS                     
;
;
L_FC38:
FC38: E8        INX                     
FC39: E0 06     CPX #$06                
FC3B: 90 ED     BCC L_FC2A              
;
L_FC3D:
FC3D: 18        CLC                     
FC3E: 60        RTS                     
;
;
GETNAM1:
FC3F: A2 00     LDX #$00                
;
L_FC41:
FC41: 20 C7 FF  JSR L_FFC7              
FC44: 9D 3A 02  STA NAM2,X               ;Filename that was found
FC47: E8        INX                     
FC48: E0 06     CPX #$06                
FC4A: 90 F5     BCC L_FC41              
FC4C: 60        RTS                     
;
;
L_FC4D:
FC4D: 20 8B FC  JSR L_FC8B              
FC50: AA        TAX                     
;
L_FC51:
FC51: 20 1B FA  JSR L_FA1B              
FC54: F0 1A     BEQ L_FC70              
FC56: C9 2A     CMP #$2A                
FC58: D0 05     BNE L_FC5F              
FC5A: 9D 34 02  STA NAM1,X               ;Filename to search for or to write
FC5D: D0 11     BNE L_FC70              
;
L_FC5F:
FC5F: C9 20     CMP #$20                
FC61: 08        PHP                     
FC62: 20 69 FF  JSR L_FF69              
FC65: 28        PLP                     
FC66: 90 E9     BCC L_FC51              
FC68: 9D 34 02  STA NAM1,X               ;Filename to search for or to write
FC6B: E8        INX                     
FC6C: E0 06     CPX #$06                
FC6E: D0 E1     BNE L_FC51              
;
L_FC70:
FC70: 86 E2     STX ZP_E2                ;Zero Page $E2
FC72: 60        RTS                     
;
;
PUTNAM:
FC73: A2 2F     LDX #$2F                
FC75: 20 82 F7  JSR MSGOUT              
FC78: AA        TAX                     
;
L_FC79:
FC79: BD 34 02  LDA NAM1,X               ;Filename to search for or to write
FC7C: 20 69 FF  JSR L_FF69              
FC7F: 20 66 F7  JSR TAP1                
FC82: E8        INX                     
FC83: E0 06     CPX #$06                
FC85: 90 F2     BCC L_FC79              
FC87: 20 04 F8  JSR CRLF                
FC8A: 60        RTS                     
;
;
L_FC8B:
FC8B: A2 0B     LDX #$0B                
FC8D: A9 00     LDA #$00                
;
L_FC8F:
FC8F: 9D 34 02  STA NAM1,X               ;Filename to search for or to write
FC92: CA        DEX                     
FC93: 10 FA     BPL L_FC8F              
FC95: 60        RTS                     
;
;
GETNAM:
FC96: 20 8B FC  JSR L_FC8B              
FC99: AA        TAX                     
FC9A: B5 14     LDA ZP_14,X              ;Zero Page $14
FC9C: F0 0D     BEQ L_FCAB              
FC9E: E8        INX                     
;
L_FC9F:
FC9F: B5 14     LDA ZP_14,X              ;Zero Page $14
FCA1: F0 08     BEQ L_FCAB              
FCA3: 9D 33 02  STA $0233,X             
FCA6: E8        INX                     
FCA7: E0 07     CPX #$07                
FCA9: D0 F4     BNE L_FC9F              
;
L_FCAB:
FCAB: 86 E2     STX ZP_E2                ;Zero Page $E2
FCAD: A9 61     LDA #$61                
FCAF: 85 13     STA ZP_13                ;Zero Page $13
FCB1: 60        RTS                     
;
;
L_FCB2:
FCB2: 20 32 F7  JSR L_F732              
FCB5: 20 7B FB  JSR DELAY5S             
FCB8: 20 03 FC  JSR L_FC03              
FCBB: 20 73 FC  JSR PUTNAM              
FCBE: 60        RTS                     
;
;
L_FCBF:
FCBF: 20 4D FC  JSR L_FC4D              
FCC2: 20 13 F8  JSR SPC1                
FCC5: 20 D5 F9  JSR GETPAR              
FCC8: E0 02     CPX #$02                
FCCA: F0 13     BEQ L_FCDF              
FCCC: 20 F0 F8  JSR L_F8F0              
FCCF: 4C BF FC  JMP L_FCBF              
FCD2: 00        BRK                     
;
L_FCD3:
FCD3:           !BYTE $9D,$34           ;unknown
 
FCD5: C9 1C     CMP #$1C                
FCD7: F0 03     BEQ L_FCDC              
FCD9: 4C 74 A3  JMP BASICA374           
;
L_FCDC:
FCDC: 4C 59 A3  JMP BASICA359           
;
L_FCDF:
FCDF: 20 38 FD  JSR L_FD38              
FCE2: 20 B2 FC  JSR L_FCB2              
FCE5: A2 03     LDX #$03                
;
L_FCE7:
FCE7: B5 FC     LDA ZP_FC,X              ;Zero Page $FC
FCE9: 20 66 F7  JSR TAP1                
FCEC: CA        DEX                     
FCED: 10 F8     BPL L_FCE7              
;
L_FCEF:
FCEF: A0 00     LDY #$00                
FCF1: B1 FC     LDA (ZP_FC),Y            ;Zero Page $FC
FCF3: 20 66 F7  JSR TAP1                
FCF6: 20 33 FF  JSR L_FF33              
FCF9: 90 F4     BCC L_FCEF              
FCFB: B0 35     BCS L_FD32              
;
L_FCFD:
FCFD: 20 4D FC  JSR L_FC4D              
FD00: 20 13 F8  JSR SPC1                
FD03: 20 D5 F9  JSR GETPAR              
FD06: C6 E0     DEC ZP_E0                ;Zero Page $E0
FD08: 20 D6 FB  JSR L_FBD6              
FD0B: A2 03     LDX #$03                
;
L_FD0D:
FD0D: 20 C7 FF  JSR L_FFC7              
FD10: 95 FC     STA ZP_FC,X              ;Zero Page $FC
FD12: CA        DEX                     
FD13: 10 F8     BPL L_FD0D              
FD15: A6 E1     LDX ZP_E1                ;Zero Page $E1
FD17: F0 0D     BEQ L_FD26              
FD19: 20 42 FD  JSR ADJPTR              
FD1C: A2 04     LDX #$04                
FD1E: 20 42 FB  JSR L_FB42              
FD21: A2 01     LDX #$01                
FD23: 20 3A FD  JSR L_FD3A              
;
L_FD26:
FD26: A0 00     LDY #$00                
FD28: 20 C7 FF  JSR L_FFC7              
FD2B: 91 FC     STA (ZP_FC),Y            ;Zero Page $FC
FD2D: 20 33 FF  JSR L_FF33              
FD30: 90 F4     BCC L_FD26              
;
L_FD32:
FD32: 20 3D F7  JSR L_F73D              
FD35: 4C EA F8  JMP L_F8EA              
;
L_FD38:
FD38: A2 03     LDX #$03                
;
L_FD3A:
FD3A: B5 FA     LDA ZP_FA,X              ;Zero Page $FA
FD3C: 95 FC     STA ZP_FC,X              ;Zero Page $FC
FD3E: CA        DEX                     
FD3F: 10 F9     BPL L_FD3A              
FD41: 60        RTS                     
;
;
ADJPTR:
FD42: 38        SEC                     
FD43: A5 FE     LDA ZP_FE                ;Zero Page $FE
FD45: E5 FC     SBC ZP_FC                ;Zero Page $FC
FD47: 85 F8     STA ZP_F8                ;Zero Page $F8
FD49: A5 FF     LDA ZP_FF                ;Zero Page $FF
FD4B: E5 FD     SBC ZP_FD                ;Zero Page $FD
FD4D: 85 F9     STA ZP_F9                ;Zero Page $F9
FD4F: 60        RTS                     
;
FD50: 20 79 FD  JSR L_FD79              
FD53: A2 04     LDX #$04                
FD55: 20 8B FB  JSR L_FB8B              
FD58: A2 01     LDX #$01                
FD5A: 20 71 FD  JSR L_FD71              
;
L_FD5D:
FD5D: A0 00     LDY #$00                
FD5F: 20 C7 FF  JSR L_FFC7              
FD62: 91 FC     STA (ZP_FC),Y            ;Zero Page $FC
FD64: 20 33 FF  JSR L_FF33              
FD67: 90 F4     BCC L_FD5D              
FD69: 20 84 F7  JSR L_F784              
FD6C: 4C 31 F9  JMP L_F931              
FD6F: A2 03     LDX #$03                
;
L_FD71:
FD71: B5 FA     LDA ZP_FA,X              ;Zero Page $FA
FD73: 95 FC     STA ZP_FC,X              ;Zero Page $FC
FD75: CA        DEX                     
FD76: 10 F9     BPL L_FD71              
FD78: 60        RTS                     
;
;
L_FD79:
FD79: 38        SEC                     
FD7A: A5 FE     LDA ZP_FE                ;Zero Page $FE
FD7C: E5 FC     SBC ZP_FC                ;Zero Page $FC
FD7E: 85 F8     STA ZP_F8                ;Zero Page $F8
FD80: A5 FF     LDA ZP_FF                ;Zero Page $FF
FD82: E5 FD     SBC ZP_FD                ;Zero Page $FD
FD84: 85 F9     STA ZP_F9                ;Zero Page $F9
FD86: 60        RTS                     
;
;
L_FD87:
FD87:           !BYTE $AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA ;filler
FD8F:           !BYTE $AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA ;filler
FD97:           !BYTE $AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA ;filler
FD9F:           !BYTE $AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA ;filler
FDA7:           !BYTE $AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA ;filler
FDAF:           !BYTE $AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA ;filler
FDB7:           !BYTE $AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA ;filler
FDBF:           !BYTE $AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA ;filler
FDC7:           !BYTE $AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA ;filler
FDCF:           !BYTE $AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA ;filler
FDD7:           !BYTE $AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA ;filler
FDDF:           !BYTE $AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA ;filler
FDE7:           !BYTE $AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA ;filler
FDEF:           !BYTE $AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA ;filler
FDF7:           !BYTE $AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA ;filler
FDFF:           !BYTE $AA                             ;filler
 
;================================================================================
; Polled Keyboard Entry Point
;================================================================================
FE00: A2 28     LDX #$28                
FE02: 9A        TXS                     
FE03: D8        CLD                     
FE04: 20 A2 F0  JSR CLS                 
;
L_FE07:
FE07: A9 00     LDA #$00                
FE09: 85 FC     STA ZP_FC                ;Zero Page $FC
FE0B: 85 FD     STA ZP_FD                ;Zero Page $FD
FE0D: A8        TAY                     
FE0E: F0 2E     BEQ L_FE3E              
;
L_FE10:
FE10: 20 34 F8  JSR MINPUT              
FE13: 29 7F     AND #$7F                
FE15: C9 2F     CMP #$2F                
FE17: F0 35     BEQ L_FE4E              
FE19: C9 47     CMP #$47                
FE1B: F0 2E     BEQ L_FE4B              
FE1D: C9 4C     CMP #$4C                
FE1F: F0 65     BEQ L_FE86              
FE21: C9 58     CMP #$58                
FE23: D0 0A     BNE L_FE2F              
FE25: A9 FE     LDA #$FE                
FE27: 48        PHA                     
FE28: A9 25     LDA #$25                
FE2A: 48        PHA                     
FE2B: 08        PHP                     
FE2C: 4C 49 F8  JMP MONBRK              
;
L_FE2F:
FE2F: 20 CC F7  JSR HEXCHECK            
FE32: 30 DC     BMI L_FE10              
FE34: A2 02     LDX #$02                
FE36: 20 25 F8  JSR ROTCHR              
FE39: A9 1A     LDA #$1A                
FE3B: 20 D3 F0  JSR WRITESCRN           
;
L_FE3E:
FE3E: B1 FC     LDA (ZP_FC),Y            ;Zero Page $FC
FE40: 85 FA     STA ZP_FA                ;Zero Page $FA
FE42: 20 E5 F7  JSR MONPRINT            
FE45: 20 04 F8  JSR CRLF                
FE48: 4C 10 FE  JMP L_FE10              
;
L_FE4B:
FE4B: 6C FC 00  JMP (ZP_FC)              ;Zero Page $FC
;
L_FE4E:
FE4E: 20 34 F8  JSR MINPUT              
FE51: 29 7F     AND #$7F                
FE53: C9 2E     CMP #$2E                
FE55: F0 B9     BEQ L_FE10              
FE57: C9 0D     CMP #$0D                
FE59: D0 0F     BNE L_FE6A              
FE5B: E6 FC     INC ZP_FC                ;Zero Page $FC
FE5D: D0 02     BNE L_FE61              
FE5F: E6 FD     INC ZP_FD                ;Zero Page $FD
;
L_FE61:
FE61: A0 00     LDY #$00                
FE63: B1 FC     LDA (ZP_FC),Y            ;Zero Page $FC
FE65: 85 FA     STA ZP_FA                ;Zero Page $FA
FE67: 4C 7D FE  JMP L_FE7D              
;
L_FE6A:
FE6A: 20 CC F7  JSR HEXCHECK            
FE6D: 30 DF     BMI L_FE4E              
FE6F: A2 00     LDX #$00                
FE71: 20 25 F8  JSR ROTCHR              
FE74: A5 FA     LDA ZP_FA                ;Zero Page $FA
FE76: 91 FC     STA (ZP_FC),Y            ;Zero Page $FC
FE78: A9 1A     LDA #$1A                
FE7A: 20 D3 F0  JSR WRITESCRN           
;
L_FE7D:
FE7D: 20 E5 F7  JSR MONPRINT            
FE80: 20 04 F8  JSR CRLF                
FE83: 4C 4E FE  JMP L_FE4E              
;
L_FE86:
FE86: 85 E0     STA ZP_E0                ;Zero Page $E0
FE88: F0 C4     BEQ L_FE4E              
FE8A: 4C C2 FF  JMP L_FFC2              
;
KEYMATRIX:
FE8D:           !TEXT "P;/ ZAQ,MNBVCXKJHGFDSIUYTREW",$00,$00,$0D,$0A ;Keyboard Matrix
FEAD:           !TEXT "OL.",$00,$0E,"-:0987654321" ;Keyboard Matrix
 
;
TABLE7:
FEBE:           !BYTE $54,$B2,$13,$75,$80,$C9,$A3,$B8 ;unknown
FEC6:           !BYTE $DB,$06,$2D,$C6,$CC,$97,$BB,$9A ;unknown
FECE:           !BYTE $16,$1D,$03,$0A,$C2,$0E,$41,$A0 ;unknown
FED6:           !BYTE $79,$D5,$00,$45,$00,$00,$32,$88 ;unknown
FEDE:           !BYTE $3F,$BF,$94,$00,$65,$A9,$AC,$A6 ;unknown
FEE6:           !BYTE $B5,$21,$61,$6D,$7D,$26,$38,$4D ;unknown
FEEE:           !BYTE $49               ;unknown
 
;
PRINTINIT:
FEEF: A9 2A     LDA #$2A                
FEF1: 8D 01 88  STA PARPORT1             ;Parallel Port $8801
FEF4: A9 FF     LDA #$FF                
FEF6: 8D 00 88  STA PARPORT              ;Parallel Port $8800
FEF9: A9 2E     LDA #$2E                
FEFB: 8D 01 88  STA PARPORT1             ;Parallel Port $8801
FEFE: A9 2C     LDA #$2C                
FF00: 8D 03 88  STA PARPORT3             ;Parallel Port $8803
FF03: 60        RTS                     
;
;
PRINTOUT:
FF04: 48        PHA                     
FF05: AD 01 88  LDA PARPORT1             ;Parallel Port $8801
FF08: 09 04     ORA #$04                
FF0A: 8D 01 88  STA PARPORT1             ;Parallel Port $8801
FF0D: 68        PLA                     
FF0E: 8D 00 88  STA PARPORT              ;Parallel Port $8800
FF11: AD 02 88  LDA PARPORT2             ;Parallel Port $8802
FF14: 4A        LSR                     
FF15: B0 0B     BCS L_FF22              
FF17: EA        NOP                     
FF18: EA        NOP                     
FF19: EA        NOP                     
;
L_FF1A:
FF1A: 2C 02 88  BIT PARPORT2             ;Parallel Port $8802
FF1D: 30 FB     BMI L_FF1A              
FF1F: AD 00 88  LDA PARPORT              ;Parallel Port $8800
;
L_FF22:
FF22: 60        RTS                     
;
;
L_FF23:
FF23: A2 00     LDX #$00                
;
L_FF25:
FF25: BD 3A 02  LDA NAM2,X               ;Filename that was found
FF28: F0 08     BEQ L_FF32              
FF2A: 20 69 FF  JSR L_FF69              
FF2D: E8        INX                     
FF2E: E0 06     CPX #$06                
FF30: D0 F3     BNE L_FF25              
;
L_FF32:
FF32: 60        RTS                     
;
;
L_FF33:
FF33: E6 FC     INC ZP_FC                ;Zero Page $FC
FF35: D0 02     BNE L_FF39              
FF37: E6 FD     INC ZP_FD                ;Zero Page $FD
;
L_FF39:
FF39: A5 FC     LDA ZP_FC                ;Zero Page $FC
FF3B: C5 FE     CMP ZP_FE                ;Zero Page $FE
FF3D: D0 08     BNE L_FF47              
FF3F: A5 FD     LDA ZP_FD                ;Zero Page $FD
FF41: C5 FF     CMP ZP_FF                ;Zero Page $FF
FF43: D0 02     BNE L_FF47              
FF45: 38        SEC                     
FF46: 60        RTS                     
;
;
L_FF47:
FF47: 18        CLC                     
FF48: 60        RTS                     
;
;
L_FF49:
FF49:           !BYTE $09,$08,$01,$0C,$1A,$1E,$0A,$0E ;unknown
 
;
L_FF51:
FF51: 8A        TXA                     
FF52: 48        PHA                     
FF53: A2 0A     LDX #$0A                
FF55: 20 56 F7  JSR L_F756              
FF58: 68        PLA                     
FF59: AA        TAX                     
FF5A: 68        PLA                     
FF5B: 60        RTS                     
;
;
L_FF5C:
FF5C:           !BYTE $AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA ;Filler
FF64:           !BYTE $AA,$AA,$AA,$AA,$AA ;Filler
 
;
L_FF69:
FF69: 20 EE FF  JSR L_FFEE              
;
L_FF6C:
FF6C: 48        PHA                     
FF6D: 2C 4E 02  BIT PRFLAG               ;Print on/off Flag
FF70: 10 05     BPL L_FF77              
FF72: 68        PLA                     
FF73: 48        PHA                     
FF74: 20 04 FF  JSR PRINTOUT            
;
L_FF77:
FF77: 2C 05 02  BIT SCRSPD               ;Scroll delay
FF7A: 10 09     BPL L_FF85              
FF7C: 68        PLA                     
FF7D: 48        PHA                     
FF7E: 20 5F F7  JSR TAPOUT              
FF81: C9 0D     CMP #$0D                
FF83: F0 CC     BEQ L_FF51              
;
L_FF85:
FF85: 68        PLA                     
FF86: 60        RTS                     
;
FF87: AA        TAX                     
FF88: AA        TAX                     
FF89: AA        TAX                     
FF8A: AA        TAX                     
FF8B: 4C 89 FB  JMP L_FB89              
FF8E: AA        TAX                     
FF8F: AA        TAX                     
FF90: AA        TAX                     
FF91: AA        TAX                     
FF92: AA        TAX                     
FF93: AA        TAX                     
FF94: AA        TAX                     
FF95: AA        TAX                     
FF96: 4C 50 FB  JMP L_FB50              
FF99: AA        TAX                     
FF9A: AD 12 02  LDA CCFLAG               ;Control-C Flag
FF9D: D0 19     BNE L_FFB8              
FF9F: A9 FE     LDA #$FE                
FFA1: 8D 00 DF  STA KEYBOARD             ;Keyboard Port
FFA4: 2C 00 DF  BIT KEYBOARD             ;Keyboard Port
FFA7: 70 0F     BVS L_FFB8              
FFA9: A9 FB     LDA #$FB                
FFAB: 8D 00 DF  STA KEYBOARD             ;Keyboard Port
FFAE: 2C 00 DF  BIT KEYBOARD             ;Keyboard Port
FFB1: 70 05     BVS L_FFB8              
FFB3: A9 03     LDA #$03                
FFB5: 4C 36 A6  JMP BASICA636           
;
L_FFB8:
FFB8: 60        RTS                     
;
;
L_FFB9:
FFB9:           !BYTE $2C               ;Unkown
 
FFBA: 2C 03 02  BIT LDFLAG               ;Load Flag
FFBD: 30 03     BMI L_FFC2              
FFBF: 4C F0 F2  JMP L_F2F0              
;
L_FFC2:
FFC2: 20 3E F8  JSR FCHAR               
FFC5: F0 0A     BEQ L_FFD1              
;
L_FFC7:
FFC7: AD 00 E0  LDA ACIA2                ;Relocated ACIA
FFCA: 4A        LSR                     
FFCB: 90 F5     BCC L_FFC2              
FFCD: AD 01 E0  LDA ACIA2+1              ;Relocated ACIA offset 1
FFD0: 60        RTS                     
;
;
L_FFD1:
FFD1:           !BYTE $20,$46,$F7,$4C,$F0,$F2,$C2,$C0 ;unknown
FFD9:           !BYTE $A8,$CE,$10,$DA,$14,$D8 ;unknown
 
;
L_FFDF:
FFDF:           !BYTE $AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA ;Filler
FFE7:           !BYTE $AA,$AA,$AA,$AA   ;Filler
 
FFEB: 6C 18 02  JMP (INVEC)              ;Input Vector
;
L_FFEE:
FFEE: 6C 1A 02  JMP (OUTVEC)             ;Output Vector
FFF1: 6C 1C 02  JMP (CCVEC)              ;Control-C Vector
FFF4: 6C 1E 02  JMP (LDVEC)              ;Load Vector
FFF7: 6C 20 02  JMP (SAVVEC)             ;Save Vector
;================================================================================
; 6502 CPU Vectors
;================================================================================
;
L_FFFA:
FFFA:           !WORD $022E             ;NMI Vector
 
;
L_FFFC:
FFFC:           !WORD $F000             ;Reset Vector
 
;
L_FFFE:
FFFE:           !WORD $F849             ;IRQ Vector
 
