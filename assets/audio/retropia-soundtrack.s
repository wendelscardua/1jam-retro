;this file for FamiTone2 library generated by FamiStudio

retropia_soundtrack_music_data:
	.byte 11
	.word @instruments
	.word @samples-3
	.word @song0ch0,@song0ch1,@song0ch2,@song0ch3,@song0ch4,307,256
	.word @song1ch0,@song1ch1,@song1ch2,@song1ch3,@song1ch4,307,256
	.word @song2ch0,@song2ch1,@song2ch2,@song2ch3,@song2ch4,307,256
	.word @song3ch0,@song3ch1,@song3ch2,@song3ch3,@song3ch4,307,256
	.word @song4ch0,@song4ch1,@song4ch2,@song4ch3,@song4ch4,307,256
	.word @song5ch0,@song5ch1,@song5ch2,@song5ch3,@song5ch4,307,256
	.word @song6ch0,@song6ch1,@song6ch2,@song6ch3,@song6ch4,307,256
	.word @song7ch0,@song7ch1,@song7ch2,@song7ch3,@song7ch4,307,256
	.word @song8ch0,@song8ch1,@song8ch2,@song8ch3,@song8ch4,307,256
	.word @song9ch0,@song9ch1,@song9ch2,@song9ch3,@song9ch4,307,256
	.word @song10ch0,@song10ch1,@song10ch2,@song10ch3,@song10ch4,307,256

@instruments:
	.byte $30 ;instrument 00 (BassDrum)
	.word @env1, @env0, @env0
	.byte $00
	.byte $b0 ;instrument 01 (Bassish)
	.word @env17, @env0, @env0
	.byte $00
	.byte $b0 ;instrument 02 (GKBacking)
	.word @env2, @env0, @env0
	.byte $00
	.byte $b0 ;instrument 03 (GKFanfare)
	.word @env5, @env0, @env0
	.byte $00
	.byte $b0 ;instrument 04 (GKLead)
	.word @env4, @env0, @env0
	.byte $00
	.byte $f0 ;instrument 05 (Lead)
	.word @env16, @env0, @env0
	.byte $00
	.byte $30 ;instrument 06 (Noise-Bumbo (F#2))
	.word @env11, @env10, @env0
	.byte $00
	.byte $30 ;instrument 07 (NoiseHiHat)
	.word @env3, @env0, @env0
	.byte $00
	.byte $30 ;instrument 08 (Noise-Prato (D#2))
	.word @env7, @env9, @env0
	.byte $00
	.byte $30 ;instrument 09 (NoiseSnare)
	.word @env12, @env0, @env0
	.byte $00
	.byte $70 ;instrument 0a (SimpleBacking)
	.word @env15, @env0, @env0
	.byte $00
	.byte $b0 ;instrument 0b (SimpleLead)
	.word @env14, @env0, @env0
	.byte $00
	.byte $30 ;instrument 0c (Tri-Bumbo)
	.word @env18, @env0, @env8
	.byte $00
	.byte $30 ;instrument 0d (Tri-Caixa)
	.word @env6, @env0, @env13
	.byte $00

@samples:
@env0:
	.byte $c0,$7f,$00,$00
@env1:
	.byte $c5,$cc,$c9,$c6,$c4,$c2,$00,$05
@env2:
	.byte $c7,$c3,$02,$c2,$c0,$00,$04
@env3:
	.byte $cf,$cd,$ca,$c0,$00,$03
@env4:
	.byte $c5,$c8,$ca,$c7,$c4,$c0,$00,$05
@env5:
	.byte $c1,$c3,$c5,$c6,$c7,$c8,$c7,$c5,$00,$07
@env6:
	.byte $cf,$02,$c0,$00,$02
@env7:
	.byte $c8,$c6,$02,$c0,$00,$03
@env8:
	.byte $c0,$f9,$00,$01
@env9:
	.byte $c0,$c3,$00,$01
@env10:
	.byte $c3,$be,$bf,$00,$02
@env11:
	.byte $cb,$ca,$c8,$c6,$c4,$c2,$c0,$00,$06
@env12:
	.byte $cf,$cd,$cb,$c9,$c7,$c6,$c4,$c2,$00,$07
@env13:
	.byte $c0,$84,$00,$01
@env14:
	.byte $c6,$c8,$c6,$c5,$c4,$c3,$00,$05
@env15:
	.byte $c5,$c6,$c4,$c3,$c2,$c1,$00,$05
@env16:
	.byte $c8,$17,$c7,$c6,$c5,$c4,$c3,$02,$c2,$02,$c1,$04,$c0,$00,$0c
@env17:
	.byte $c1,$c3,$c4,$c5,$c6,$c5,$c6,$c5,$c7,$c6,$c5,$c4,$c3,$c2,$c1,$c0,$00,$0f
@env18:
	.byte $cf,$05,$c0,$c0,$c0,$00,$04

@song0ch0:
	.byte $fb, $01
@song0ch0loop:
@ref0:
	.byte $a1,$8a,$30,$bb,$38,$9d,$46,$f9,$83
@ref1:
	.byte $48,$8d,$44,$8d,$48,$9d,$40,$9d,$3a,$9d,$40,$ed,$00,$8d
@ref2:
	.byte $a1,$30,$bb,$38,$9d,$46,$f9,$83
@ref3:
	.byte $46,$bd,$38,$9d,$3c,$9d,$40,$9b,$3c,$9d,$38,$af,$00,$8d
@ref4:
	.byte $9f,$38,$9d,$3c,$9d,$4a,$bd,$42,$dd
@ref5:
	.byte $46,$bd,$50,$8d,$4c,$8d,$46,$8d,$3c,$8d,$42,$8d,$34,$9d,$34,$9d,$3c,$ad
	.byte $ff,$08
	.word @ref2
	.byte $ff,$0e
	.word @ref3
	.byte $fd
	.word @song0ch0loop

@song0ch1:
@song0ch1loop:
@ref6:
	.byte $82,$12,$9d,$16,$9d,$1c,$9d,$16,$9d,$12,$9d,$16,$9d,$1c,$9d,$16,$9d
@ref7:
	.byte $1a,$9d,$20,$9d,$28,$9d,$20,$9d,$1a,$9d,$20,$9d,$28,$9d,$20,$9d
@ref8:
	.byte $12,$9d,$16,$9d,$1c,$9d,$16,$9d,$12,$9d,$16,$9d,$1c,$9d,$16,$9d
	.byte $ff,$10
	.word @ref7
	.byte $ff,$10
	.word @ref8
	.byte $ff,$10
	.word @ref7
	.byte $ff,$10
	.word @ref8
	.byte $ff,$10
	.word @ref7
	.byte $fd
	.word @song0ch1loop

@song0ch2:
@song0ch2loop:
@ref9:
	.byte $98,$26,$8d,$00,$ad,$26,$8d,$00,$ad,$26,$8d,$00,$8d,$9a,$2a,$9d,$98,$26,$8d,$00,$8d,$9a,$2a,$9d
	.byte $ff,$14
	.word @ref9
	.byte $ff,$14
	.word @ref9
	.byte $ff,$14
	.word @ref9
	.byte $ff,$14
	.word @ref9
	.byte $ff,$14
	.word @ref9
	.byte $ff,$14
	.word @ref9
	.byte $ff,$14
	.word @ref9
	.byte $fd
	.word @song0ch2loop

@song0ch3:
@song0ch3loop:
@ref10:
	.byte $8c,$3c,$9d,$90,$38,$8d,$38,$8d,$8c,$3c,$9d,$90,$38,$8d,$8c,$3c,$ad,$90,$38,$8d,$38,$8d,$8c,$3c,$9d,$90,$38,$9d
	.byte $ff,$14
	.word @ref10
	.byte $ff,$14
	.word @ref10
	.byte $ff,$14
	.word @ref10
	.byte $ff,$14
	.word @ref10
	.byte $ff,$14
	.word @ref10
	.byte $ff,$14
	.word @ref10
	.byte $ff,$14
	.word @ref10
	.byte $fd
	.word @song0ch3loop

@song0ch4:
@song0ch4loop:
@ref11:
	.byte $f9,$f9,$8b
@ref12:
	.byte $f9,$f9,$8b
@ref13:
	.byte $f9,$f9,$8b
@ref14:
	.byte $f9,$f9,$8b
@ref15:
	.byte $f9,$f9,$8b
@ref16:
	.byte $f9,$f9,$8b
@ref17:
	.byte $f9,$f9,$8b
@ref18:
	.byte $f9,$f9,$8b
	.byte $fd
	.word @song0ch4loop

@song1ch0:
	.byte $fb, $01
@ref19:
	.byte $96,$4a,$99,$52,$b5,$62,$b3,$00,$62,$b7,$00,$97
@song1ch0loop:
@ref20:
	.byte $96,$4a,$ed,$50,$99,$54,$99,$46,$a7,$4a,$8b
@ref21:
	.byte $4a,$d1,$4e,$99,$52,$99,$4e,$99,$48,$b5
@ref22:
	.byte $40,$99,$48,$99,$40,$8b,$4a,$8b,$44,$8b,$40,$a7,$48,$b5,$40,$99
@ref23:
	.byte $4a,$99,$52,$99,$4a,$8b,$54,$8b,$4e,$8b,$4a,$a7,$52,$b5,$4a,$99
@ref24:
	.byte $44,$99,$40,$99,$4a,$8b,$44,$8b,$48,$8b,$4a,$a7,$44,$b5,$40,$97,$00
@ref25:
	.byte $4a,$ed,$54,$99,$42,$99,$54,$99,$4a,$99
@ref26:
	.byte $50,$ed,$5a,$99,$48,$99,$5a,$99,$50,$99
@ref27:
	.byte $4a,$b3,$00,$9b,$48,$8b,$42,$8b,$46,$8b,$3e,$8b,$42,$8b,$3a,$8b,$3e,$8b,$36,$8b,$38,$97,$00
	.byte $fd
	.word @song1ch0loop

@song1ch1:
@ref28:
	.byte $94,$40,$99,$4a,$b5,$52,$b3,$00,$52,$b3,$00,$9b
@song1ch1loop:
@ref29:
	.byte $94,$40,$ed,$46,$99,$42,$99,$3c,$b5
@ref30:
	.byte $40,$d1,$44,$b5,$46,$99,$3e,$b5
@ref31:
	.byte $36,$99,$32,$99,$38,$8b,$30,$8b,$28,$8b,$28,$99,$2e,$8b,$36,$b5,$2e,$99
@ref32:
	.byte $32,$99,$2e,$99,$34,$8b,$2c,$8b,$24,$8b,$24,$99,$2a,$8b,$32,$b5,$2a,$99
@ref33:
	.byte $3c,$99,$38,$99,$42,$8b,$3a,$c3,$32,$b5,$3a,$97,$00
@ref34:
	.byte $40,$ed,$48,$99,$4a,$99,$40,$b5
@ref35:
	.byte $46,$ed,$3e,$99,$42,$99,$46,$b3,$00
@ref36:
	.byte $3e,$b3,$00,$9b,$3e,$8b,$3a,$8b,$42,$8b,$38,$8b,$3e,$8b,$32,$8b,$2e,$b3,$00
	.byte $fd
	.word @song1ch1loop

@song1ch2:
@ref37:
	.byte $80,$20,$97,$00,$94,$28,$97,$00,$28,$97,$00,$28,$99,$80,$20,$97,$00,$94,$32,$97,$00,$32,$97,$00,$32,$99
@song1ch2loop:
@ref38:
	.byte $80,$20,$97,$00,$94,$32,$97,$00,$32,$97,$00,$32,$97,$00,$80,$20,$97,$00,$94,$32,$97,$00,$32,$97,$00,$32,$99
	.byte $ff,$16
	.word @ref37
	.byte $ff,$17
	.word @ref38
	.byte $ff,$16
	.word @ref37
	.byte $ff,$17
	.word @ref38
	.byte $ff,$16
	.word @ref37
	.byte $ff,$17
	.word @ref38
	.byte $ff,$16
	.word @ref37
	.byte $fd
	.word @song1ch2loop

@song1ch3:
@ref39:
	.byte $8e,$7a,$b3,$00,$9b,$7a,$99,$00,$b5,$7a,$b3,$00
@song1ch3loop:
@ref40:
	.byte $8e,$7a,$b5,$92,$76,$99,$8e,$7a,$99,$92,$76,$b5,$8e,$7a,$b5
@ref41:
	.byte $7a,$b5,$92,$76,$99,$8e,$7a,$99,$92,$76,$b5,$8e,$7a,$b5
	.byte $ff,$0a
	.word @ref41
	.byte $ff,$0a
	.word @ref41
	.byte $ff,$0a
	.word @ref41
	.byte $ff,$0a
	.word @ref41
	.byte $ff,$0a
	.word @ref41
	.byte $ff,$0a
	.word @ref41
	.byte $fd
	.word @song1ch3loop

@song1ch4:
@ref42:
	.byte $f9,$e5
@song1ch4loop:
@ref43:
	.byte $f9,$e5
@ref44:
	.byte $f9,$e5
@ref45:
	.byte $f9,$e5
@ref46:
	.byte $f9,$e5
@ref47:
	.byte $f9,$e5
@ref48:
	.byte $f9,$e5
@ref49:
	.byte $f9,$e5
@ref50:
	.byte $f9,$e5
	.byte $fd
	.word @song1ch4loop

@song2ch0:
	.byte $fb, $01
@ref51:
	.byte $96,$1a,$8f,$00,$9d,$50,$87,$56,$87,$54,$87,$5a,$87,$64,$87,$60,$8f,$00
@song2ch0loop:
@ref52:
	.byte $f7
	.byte $fd
	.word @song2ch0loop

@song2ch1:
@ref53:
	.byte $b1,$94,$46,$87,$4a,$87,$46,$87,$4a,$87,$50,$87,$54,$8f,$00
@song2ch1loop:
@ref54:
	.byte $f7
	.byte $fd
	.word @song2ch1loop

@song2ch2:
@ref55:
	.byte $f7
@song2ch2loop:
@ref56:
	.byte $f7
	.byte $fd
	.word @song2ch2loop

@song2ch3:
@ref57:
	.byte $f7
@song2ch3loop:
@ref58:
	.byte $f7
	.byte $fd
	.word @song2ch3loop

@song2ch4:
@ref59:
	.byte $f7
@song2ch4loop:
@ref60:
	.byte $f7
	.byte $fd
	.word @song2ch4loop

@song3ch0:
	.byte $fb, $01
@ref61:
	.byte $88,$2e,$cd,$3a,$a5
@ref62:
	.byte $93,$26,$91,$3a,$91,$1a,$91,$26,$a5
@song3ch0loop:
@ref63:
	.byte $88,$2e,$a5,$1a,$91,$28,$91,$3a,$a5
@ref64:
	.byte $1a,$91,$26,$91,$3a,$91,$1a,$91,$26,$a5
@ref65:
	.byte $34,$a5,$20,$91,$2e,$91,$40,$a5
@ref66:
	.byte $20,$91,$2c,$91,$40,$91,$20,$91,$2c,$a5
	.byte $fd
	.word @song3ch0loop

@song3ch1:
@ref67:
	.byte $f7
@ref68:
	.byte $f7
@song3ch1loop:
@ref69:
	.byte $84,$26,$a5,$12,$91,$20,$91,$28,$a5
@ref70:
	.byte $12,$91,$2c,$91,$32,$91,$12,$91,$1e,$a5
@ref71:
	.byte $26,$a5,$12,$91,$20,$91,$28,$a5
	.byte $ff,$0a
	.word @ref70
	.byte $fd
	.word @song3ch1loop

@song3ch2:
@ref72:
	.byte $f7
@ref73:
	.byte $f7
@song3ch2loop:
@ref74:
	.byte $f7
@ref75:
	.byte $f7
@ref76:
	.byte $f7
@ref77:
	.byte $f7
	.byte $fd
	.word @song3ch2loop

@song3ch3:
@ref78:
	.byte $f7
@ref79:
	.byte $f7
@song3ch3loop:
@ref80:
	.byte $f7
@ref81:
	.byte $f7
@ref82:
	.byte $f7
@ref83:
	.byte $f7
	.byte $fd
	.word @song3ch3loop

@song3ch4:
@ref84:
	.byte $f7
@ref85:
	.byte $f7
@song3ch4loop:
@ref86:
	.byte $f7
@ref87:
	.byte $f7
@ref88:
	.byte $f7
@ref89:
	.byte $f7
	.byte $fd
	.word @song3ch4loop

@song4ch0:
	.byte $fb, $01
@ref90:
	.byte $86,$3a,$cd,$42,$cd,$3e,$cb,$00
@song4ch0loop:
@ref91:
	.byte $86,$42,$91,$44,$91,$40,$91,$40,$91,$46,$91,$46,$91,$4a,$91,$4a,$91,$40,$91,$3c,$91,$3c,$a5
@ref92:
	.byte $3c,$91,$3e,$91,$3a,$91,$3a,$91,$42,$91,$42,$91,$46,$91,$46,$91,$40,$91,$4a,$91,$4a,$a5
	.byte $fd
	.word @song4ch0loop

@song4ch1:
@ref93:
	.byte $82,$32,$cd,$2c,$cd,$32,$a5,$2e,$a3,$00
@song4ch1loop:
@ref94:
	.byte $82,$30,$a5,$32,$a5,$2e,$a5,$32,$a5,$2c,$a5,$2e,$a5
@ref95:
	.byte $30,$a5,$32,$a5,$2e,$a5,$32,$a5,$2c,$a5,$2e,$a5
	.byte $fd
	.word @song4ch1loop

@song4ch2:
@ref96:
	.byte $f9,$f5
@song4ch2loop:
@ref97:
	.byte $f9,$f5
@ref98:
	.byte $f9,$f5
	.byte $fd
	.word @song4ch2loop

@song4ch3:
@ref99:
	.byte $f9,$f5
@song4ch3loop:
@ref100:
	.byte $f9,$f5
@ref101:
	.byte $f9,$f5
	.byte $fd
	.word @song4ch3loop

@song4ch4:
@ref102:
	.byte $f9,$f5
@song4ch4loop:
@ref103:
	.byte $f9,$f5
@ref104:
	.byte $f9,$f5
	.byte $fd
	.word @song4ch4loop

@song5ch0:
	.byte $fb, $01
@song5ch0loop:
@ref105:
	.byte $f9,$f5
@ref106:
	.byte $f9,$f5
@ref107:
	.byte $f9,$f5
	.byte $fd
	.word @song5ch0loop

@song5ch1:
@song5ch1loop:
@ref108:
	.byte $f9,$f5
@ref109:
	.byte $f9,$f5
@ref110:
	.byte $f9,$f5
	.byte $fd
	.word @song5ch1loop

@song5ch2:
@song5ch2loop:
@ref111:
	.byte $f9,$f5
@ref112:
	.byte $f9,$f5
@ref113:
	.byte $f9,$f5
	.byte $fd
	.word @song5ch2loop

@song5ch3:
@song5ch3loop:
@ref114:
	.byte $f9,$f5
@ref115:
	.byte $f9,$f5
@ref116:
	.byte $f9,$f5
	.byte $fd
	.word @song5ch3loop

@song5ch4:
@song5ch4loop:
@ref117:
	.byte $f9,$f5
@ref118:
	.byte $f9,$f5
@ref119:
	.byte $f9,$f5
	.byte $fd
	.word @song5ch4loop

@song6ch0:
	.byte $fb, $01
@song6ch0loop:
@ref120:
	.byte $f9,$f5
@ref121:
	.byte $f9,$f5
@ref122:
	.byte $f9,$f5
	.byte $fd
	.word @song6ch0loop

@song6ch1:
@song6ch1loop:
@ref123:
	.byte $f9,$f5
@ref124:
	.byte $f9,$f5
@ref125:
	.byte $f9,$f5
	.byte $fd
	.word @song6ch1loop

@song6ch2:
@song6ch2loop:
@ref126:
	.byte $f9,$f5
@ref127:
	.byte $f9,$f5
@ref128:
	.byte $f9,$f5
	.byte $fd
	.word @song6ch2loop

@song6ch3:
@song6ch3loop:
@ref129:
	.byte $f9,$f5
@ref130:
	.byte $f9,$f5
@ref131:
	.byte $f9,$f5
	.byte $fd
	.word @song6ch3loop

@song6ch4:
@song6ch4loop:
@ref132:
	.byte $f9,$f5
@ref133:
	.byte $f9,$f5
@ref134:
	.byte $f9,$f5
	.byte $fd
	.word @song6ch4loop

@song7ch0:
	.byte $fb, $01
@ref135:
	.byte $86,$3a,$91,$42,$91,$4a,$91,$48,$91,$46,$91,$48,$91,$4a,$a3,$00,$93,$44,$91,$4a,$a3,$00
@song7ch0loop:
@ref136:
	.byte $f9,$f5
@ref137:
	.byte $f9,$f5
	.byte $fd
	.word @song7ch0loop

@song7ch1:
@ref138:
	.byte $82,$32,$91,$3a,$91,$42,$91,$40,$91,$3e,$91,$3c,$91,$3a,$a3,$00,$93,$3c,$91,$38,$a5
@song7ch1loop:
@ref139:
	.byte $f9,$f5
@ref140:
	.byte $f9,$f5
	.byte $fd
	.word @song7ch1loop

@song7ch2:
@ref141:
	.byte $f9,$f5
@song7ch2loop:
@ref142:
	.byte $f9,$f5
@ref143:
	.byte $f9,$f5
	.byte $fd
	.word @song7ch2loop

@song7ch3:
@ref144:
	.byte $f9,$f5
@song7ch3loop:
@ref145:
	.byte $f9,$f5
@ref146:
	.byte $f9,$f5
	.byte $fd
	.word @song7ch3loop

@song7ch4:
@ref147:
	.byte $f9,$f5
@song7ch4loop:
@ref148:
	.byte $f9,$f5
@ref149:
	.byte $f9,$f5
	.byte $fd
	.word @song7ch4loop

@song8ch0:
	.byte $fb, $01
@song8ch0loop:
@ref150:
	.byte $f9,$f5
@ref151:
	.byte $f9,$f5
@ref152:
	.byte $f9,$f5
	.byte $fd
	.word @song8ch0loop

@song8ch1:
@song8ch1loop:
@ref153:
	.byte $f9,$f5
@ref154:
	.byte $f9,$f5
@ref155:
	.byte $f9,$f5
	.byte $fd
	.word @song8ch1loop

@song8ch2:
@song8ch2loop:
@ref156:
	.byte $f9,$f5
@ref157:
	.byte $f9,$f5
@ref158:
	.byte $f9,$f5
	.byte $fd
	.word @song8ch2loop

@song8ch3:
@song8ch3loop:
@ref159:
	.byte $f9,$f5
@ref160:
	.byte $f9,$f5
@ref161:
	.byte $f9,$f5
	.byte $fd
	.word @song8ch3loop

@song8ch4:
@song8ch4loop:
@ref162:
	.byte $f9,$f5
@ref163:
	.byte $f9,$f5
@ref164:
	.byte $f9,$f5
	.byte $fd
	.word @song8ch4loop

@song9ch0:
	.byte $fb, $01
@song9ch0loop:
@ref165:
	.byte $f9,$e5
@ref166:
	.byte $f9,$e5
@ref167:
	.byte $f9,$e5
@ref168:
	.byte $f9,$e5
@ref169:
	.byte $f9,$e5
@ref170:
	.byte $f9,$e5
@ref171:
	.byte $f9,$e5
@ref172:
	.byte $f9,$e5
@ref173:
	.byte $f9,$e5
	.byte $fd
	.word @song9ch0loop

@song9ch1:
@song9ch1loop:
@ref174:
	.byte $f9,$e5
@ref175:
	.byte $f9,$e5
@ref176:
	.byte $f9,$e5
@ref177:
	.byte $f9,$e5
@ref178:
	.byte $f9,$e5
@ref179:
	.byte $f9,$e5
@ref180:
	.byte $f9,$e5
@ref181:
	.byte $f9,$e5
@ref182:
	.byte $f9,$e5
	.byte $fd
	.word @song9ch1loop

@song9ch2:
@song9ch2loop:
@ref183:
	.byte $f9,$e5
@ref184:
	.byte $f9,$e5
@ref185:
	.byte $f9,$e5
@ref186:
	.byte $f9,$e5
@ref187:
	.byte $f9,$e5
@ref188:
	.byte $f9,$e5
@ref189:
	.byte $f9,$e5
@ref190:
	.byte $f9,$e5
@ref191:
	.byte $f9,$e5
	.byte $fd
	.word @song9ch2loop

@song9ch3:
@song9ch3loop:
@ref192:
	.byte $f9,$e5
@ref193:
	.byte $f9,$e5
@ref194:
	.byte $f9,$e5
@ref195:
	.byte $f9,$e5
@ref196:
	.byte $f9,$e5
@ref197:
	.byte $f9,$e5
@ref198:
	.byte $f9,$e5
@ref199:
	.byte $f9,$e5
@ref200:
	.byte $f9,$e5
	.byte $fd
	.word @song9ch3loop

@song9ch4:
@song9ch4loop:
@ref201:
	.byte $f9,$e5
@ref202:
	.byte $f9,$e5
@ref203:
	.byte $f9,$e5
@ref204:
	.byte $f9,$e5
@ref205:
	.byte $f9,$e5
@ref206:
	.byte $f9,$e5
@ref207:
	.byte $f9,$e5
@ref208:
	.byte $f9,$e5
@ref209:
	.byte $f9,$e5
	.byte $fd
	.word @song9ch4loop

@song10ch0:
	.byte $fb, $01
@song10ch0loop:
@ref210:
	.byte $f9,$e5
@ref211:
	.byte $f9,$e5
@ref212:
	.byte $f9,$e5
@ref213:
	.byte $f9,$e5
@ref214:
	.byte $f9,$e5
@ref215:
	.byte $f9,$e5
@ref216:
	.byte $f9,$e5
@ref217:
	.byte $f9,$e5
@ref218:
	.byte $f9,$e5
	.byte $fd
	.word @song10ch0loop

@song10ch1:
@song10ch1loop:
@ref219:
	.byte $f9,$e5
@ref220:
	.byte $f9,$e5
@ref221:
	.byte $f9,$e5
@ref222:
	.byte $f9,$e5
@ref223:
	.byte $f9,$e5
@ref224:
	.byte $f9,$e5
@ref225:
	.byte $f9,$e5
@ref226:
	.byte $f9,$e5
@ref227:
	.byte $f9,$e5
	.byte $fd
	.word @song10ch1loop

@song10ch2:
@song10ch2loop:
@ref228:
	.byte $f9,$e5
@ref229:
	.byte $f9,$e5
@ref230:
	.byte $f9,$e5
@ref231:
	.byte $f9,$e5
@ref232:
	.byte $f9,$e5
@ref233:
	.byte $f9,$e5
@ref234:
	.byte $f9,$e5
@ref235:
	.byte $f9,$e5
@ref236:
	.byte $f9,$e5
	.byte $fd
	.word @song10ch2loop

@song10ch3:
@song10ch3loop:
@ref237:
	.byte $f9,$e5
@ref238:
	.byte $f9,$e5
@ref239:
	.byte $f9,$e5
@ref240:
	.byte $f9,$e5
@ref241:
	.byte $f9,$e5
@ref242:
	.byte $f9,$e5
@ref243:
	.byte $f9,$e5
@ref244:
	.byte $f9,$e5
@ref245:
	.byte $f9,$e5
	.byte $fd
	.word @song10ch3loop

@song10ch4:
@song10ch4loop:
@ref246:
	.byte $f9,$e5
@ref247:
	.byte $f9,$e5
@ref248:
	.byte $f9,$e5
@ref249:
	.byte $f9,$e5
@ref250:
	.byte $f9,$e5
@ref251:
	.byte $f9,$e5
@ref252:
	.byte $f9,$e5
@ref253:
	.byte $f9,$e5
@ref254:
	.byte $f9,$e5
	.byte $fd
	.word @song10ch4loop
