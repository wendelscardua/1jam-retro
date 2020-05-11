.include "constants.inc"
.include "header.inc"

.feature force_range

; famitone2 config
FT_PAL_SUPPORT=0
FT_NTSC_SUPPORT=1
FT_PITCH_FIX=0
FT_SFX_ENABLE=1
FT_THREAD=1
FT_DPCM_ENABLE=1
FT_SFX_STREAMS=4
FT_DPCM_OFF= $c000

; music/sfx constants
; MUSIC_TRACK_1 = 0

; SFX_SOME_SFX = 0

; workhouse keeper constants
.enum wk_symbols
  padding = $2D ; "-"
  empty   = $20 ; " "
  wall    = $23 ; "#"
  box     = $6F ; "o"
  goal    = $78 ; "x"
  player  = $40 ; "@"
.endenum

; game config

GAMEKID_DELAY = 60


.segment "ZEROPAGE"
FT_TEMP: .res 3
.segment "FAMITONE"
FT_BASE_ADR: .res 186

.segment "CODE"
.include "famitone2.s"

.segment "OAM"
.struct Sprite
  ycoord .byte
  tile .byte
  flag .byte
  xcoord .byte
.endstruct

oam_sprites:
  .repeat 64
    .tag Sprite
  .endrepeat

.zeropage

.enum game_states
  main_playing = 0
  ; wk = workhouse keeper (sokoban clone)
  wk_booting_gamekid
  wk_title
  wk_load_next_level
  wk_playing
  wk_win
  ; gi = galaxy intruders (pew pew)
  gi_booting_gamekid
  gi_title
  gi_playing
  gi_win
  gi_lose
.endenum

.importzp rng_seed
.importzp buttons
.importzp last_frame_buttons
.importzp released_buttons
.importzp pressed_buttons
.importzp rle_ptr

addr_ptr: .res 2 ; generic address pointer
ppu_addr_ptr: .res 2 ; temporary address for PPU_ADDR
nmis: .res 1
old_nmis: .res 1
args: .res 5
game_state: .res 1
current_nametable: .res 1
current_level: .res 1
current_sub_level: .res 1
frame_counter: .res 1
sprite_counter: .res 1
temp_a: .res 1
temp_b: .res 1
temp_x: .res 1
temp_y: .res 1

.segment "BSS"
; non-zp RAM goes here
gamekid_ram: .res $100

.struct wk_var
  table .res 9*16
  player_xy .byte
  box_xy .res 4
  old_xy .res 5
  start_xy .res 5
.endstruct

GI_MAX_ENEMIES=8
GI_TOTAL_ENEMIES=30
.struct gi_var
  player_x .byte
  player_lives .byte
  total_enemies .byte
  num_enemies .byte
  enemy_x .res 8
  enemy_y .res 8
  enemy_direction .res 8
  bullet_x .byte
  bullet_y .byte
.endstruct

.segment "CODE"

.import reset_handler
.import readjoy
.import rand
.import unrle

; .import music_data

.macro KIL ; pseudo instruction to kill the program
  .byte $12
.endmacro

.macro save_regs
  PHA
  TXA
  PHA
  TYA
  PHA
.endmacro

.macro restore_regs
  PLA
  TAY
  PLA
  TAX
  PLA
.endmacro

.macro copy_word_to_pointer word, pointer
  LDA #<word
  STA pointer
  LDA #>word
  STA pointer + 1
.endmacro

.macro add_byte_to_word byte, word
  .local skip
  CLC
  LDA byte
  ADC word
  STA word
  BCC skip
  INC word+1
skip:
.endmacro

.proc irq_handler
  RTI
.endproc

.proc nmi_handler
  save_regs

  ; Fix Scroll
  LDA PPUSTATUS
  LDA current_nametable
  ASL
  ASL
  ORA #$20
  STA PPUADDR
  LDA #$00
  STA PPUADDR
  LDA #$00 ; horizontal scroll
  STA PPUSCROLL
  STA PPUSCROLL

  ; Refresh OAM
  LDA #$00
  STA OAMADDR
  LDA #$02
  STA OAMDMA
  restore_regs

  INC nmis
  RTI
.endproc

.export main
.proc main
  SEI         ; ignore IRQs
  CLD         ; disable decimal mode
  LDX #$40
  STX $4017   ; disable APU frame IRQ
  LDX #$ff
  TXS         ; Set up stack
  INX         ; now X = 0
  STX PPUCTRL ; disable NMI
  STX PPUMASK ; disable rendering
  STX $4010   ; disable DMC IRQs

  LDX #0
clear_ram:
  LDA #$00
  STA $0000,X
  STA $0100,X
  STA $0300,X
  STA $0400,X
  STA $0500,X
  STA $0600,X
  STA $0700,X
  LDA #$fe
  STA $0200,X
  INX
  BNE clear_ram

  ; load palettes
  LDX PPUSTATUS
  LDX #$3f
  STX PPUADDR
  LDX #$00
  STX PPUADDR
load_palettes:
  LDA palettes,X
  STA PPUDATA
  INX
  CPX #$20
  BNE load_palettes

  LDA #$23
  STA rng_seed
  LDA #$C1
  STA rng_seed+1

vblankwait:       ; wait for another vblank before continuing
  BIT PPUSTATUS
  BPL vblankwait

  LDA #%10010000  ; turn on NMIs, sprites use first pattern table
  STA PPUCTRL
  LDA #%00011110  ; turn on screen
  STA PPUMASK

  ; LDX #<some_music_data
  ; LDY #>some_music_data
  ; LDA #1
  ; JSR FamiToneInit

  ; LDX #<some_sfx_data
  ; LDY #>some_sfx_data
  ; LDA #1
  ; JSR FamiToneSfxInit

  ; LDA #game_states::wk_booting_gamekid
  LDA #game_states::gi_booting_gamekid
  STA game_state
  LDA #$00
  STA frame_counter

forever:
  LDA nmis
  CMP old_nmis
  BEQ etc
  STA old_nmis
  ; new frame code
  JSR rand
  JSR game_state_handler
  ; JSR FamiToneUpdate

etc:
  JMP forever
.endproc

.proc load_level
; expects rle_ptr to already point to rle data
; if addr_ptr is present, uses it to load second bg
  BIT PPUSTATUS
  LDA #%00010000  ; turn off NMIs
  STA PPUCTRL
  LDA #%00000000  ; turn off screen
  STA PPUMASK

  ; clear sprite ram
  LDX #0
  LDA #$fe
:
  STA $0200,X
  INX
  BNE :-

  ; read bg rle pointer and uncompress it
  save_regs
:  ; wait for another vblank before continuing
  BIT PPUSTATUS
  BPL :-
  LDA #$20
  STA PPUADDR
  LDA #$00
  STA PPUADDR
  JSR unrle
  LDA addr_ptr
  BEQ :++
:  ; wait for another vblank before continuing
  BIT PPUSTATUS
  BPL :-
  LDA addr_ptr
  STA rle_ptr
  LDA addr_ptr+1
  STA rle_ptr+1
  LDA #$24
  STA PPUADDR
  LDA #$00
  STA PPUADDR
  JSR unrle
:
  restore_regs

vblankwait:       ; wait for another vblank before continuing
  BIT PPUSTATUS
  BPL vblankwait

  BIT PPUSTATUS
  LDA #%10010000  ; turn of NMIs
  STA PPUCTRL
  LDA #%00011110  ; turn on screen
  STA PPUMASK

  RTS
.endproc

.proc player_input
  JSR readjoy
  RTS
.endproc

.proc DEBUG_start
  LDA #%00010000
  STA PPUCTRL

  BIT PPUSTATUS
  LDA #%00000000  ; turn off screen
  STA PPUMASK

  LDA PPUSTATUS
  LDA #$29
  STA PPUADDR
  LDA #$00
  STA PPUADDR
  RTS
.endproc

.proc DEBUG_end
vblankwait:       ; wait for another vblank before continuing
  BIT PPUSTATUS
  BPL vblankwait

  BIT PPUSTATUS
  LDA #%00011110  ; turn on screen
  STA PPUMASK
  LDA #$20
  STA PPUADDR
  LDA #$00
  STA PPUADDR
  RTS
.endproc

.proc DEBUG_print_byte
  PHA
  AND #%11110000
  .repeat 4
  LSR
  .endrepeat
  JSR print_hex
  PLA
  AND #%00001111
  JSR print_hex
  RTS
.endproc

.proc print_hex
  CMP #$0A
  BCS :+
  CLC
  ADC #$10
  STA PPUDATA
  RTS
:
  ADC #$36
  STA PPUDATA
  RTS
.endproc

.macro print string
  LDA #<string
  STA addr_ptr
  LDA #>string
  STA addr_ptr+1
  JSR write_tiles
.endmacro

; these act like printf, displaying the corresponding digit instead
WRITE_X_SYMBOL = $FE

.proc write_tiles
  ; write tiles on background
  ; addr_ptr - point to string starting point (strings end with $00)
  ; ppu_addr_ptr - PPU target
  ; When the tile is #WRITE_X_SYMBOL, the current value for X
  ; is written instead (e.g. '2' tile for X = 2)
  LDA PPUSTATUS
  LDA ppu_addr_ptr+1
  STA PPUADDR
  LDA ppu_addr_ptr
  STA PPUADDR
  LDY #0
writing_loop:
  LDA (addr_ptr), Y
  BEQ exit
  CMP #WRITE_X_SYMBOL
  BNE write_tile
  TXA
  JSR print_hex
  JMP next
write_tile:
  STA PPUDATA
next:
  INY
  JMP writing_loop
exit:
  RTS
.endproc

.proc game_state_handler
  ; Uses RTS Trick
  LDX game_state
  LDA game_state_handlers_h, X
  PHA
  LDA game_state_handlers_l, X
  PHA
  RTS
.endproc

.proc main_playing
  JSR player_input
  RTS
.endproc

.proc gk_booting_gamekid
  ; generic "state"

  LDA frame_counter
  BNE wait_for_title
  ; load gamekid screen
  LDA #<nametable_gamekid_boot
  STA rle_ptr
  LDA #>nametable_gamekid_boot
  STA rle_ptr+1
  ; ... and also wk title
  LDX game_state
  LDA subgame_by_game_state,X
  TAX
  LDA subgame_nametables_l,X
  STA addr_ptr
  LDA subgame_nametables_h,X
  STA addr_ptr+1
  JSR load_level

wait_for_title:
  ; TODO: optimize
  INC frame_counter
  LDA #GAMEKID_DELAY ; wait a second
  CMP frame_counter
  BNE :+
  INC game_state ; XXX assume ??-title comes after ??-booting
  LDA #$00
  STA frame_counter
  RTS
:
  RTS
.endproc

.proc wk_booting_gamekid
  JSR gk_booting_gamekid
  RTS
.endproc

.proc wk_title
  LDA frame_counter
  BNE wait_for_level

  ; insta scroll to title
  LDA #$01
  STA current_nametable
  LDA #$00
  STA current_sub_level
  JSR wk_load_level_data

wait_for_level:
  ; TODO: optimize
  INC frame_counter
  LDA #GAMEKID_DELAY ; wait a second
  CMP frame_counter
  BNE :+
  LDA #$00
  STA current_nametable
  LDA #game_states::wk_playing
  STA game_state
:
  ; use the wait to draw the level bg, row by row (gotta go fast)
  JSR wk_partial_draw_level
  RTS
.endproc

.proc wk_load_level_data
  LDA current_sub_level
  ASL
  TAX
  LDA wk_levels,X
  STA addr_ptr
  LDA wk_levels+1,X
  STA addr_ptr+1

  LDA #$00
  STA gamekid_ram+wk_var::box_xy+0
  STA gamekid_ram+wk_var::box_xy+1
  STA gamekid_ram+wk_var::box_xy+2
  STA gamekid_ram+wk_var::box_xy+3
  LDX #$00
  LDY #(.sizeof(wk_var::table)-1)
loop:
  LDA (addr_ptr),Y
  STA gamekid_ram+wk_var::table,Y
  CMP #wk_symbols::box
  BNE :+
  TYA
  STA gamekid_ram+wk_var::box_xy,X
  INX
:
  CMP #wk_symbols::player
  BNE :+
  TYA
  STA gamekid_ram+wk_var::player_xy
:
  DEY
  CPY #$FF ; TODO optimize
  BNE loop

  LDX #$04
:
  LDA gamekid_ram+wk_var::player_xy,X
  STA gamekid_ram+wk_var::start_xy,X
  DEX
  BPL :-

  RTS
.endproc

.proc wk_partial_draw_level
  LDA frame_counter
  CMP #$06
  BCS :+
  RTS
:
  CMP #$18
  BCC :+
  RTS
:
  ; frame_counter = y-coordinate in nametable
  TAY
  LDA current_nametable
  EOR #%1
  ASL
  ASL
  ORA #$20
  STA ppu_addr_ptr+1
  LDA #$00
  STA ppu_addr_ptr

  ; adding y*$20 to ppu_addr_ptr
  ; 76543210         76543210 76543210
  ; 000edcba x $20 = 000000ed cba00000

  ; ed
  TYA
  .repeat 3
  LSR
  .endrepeat
  CLC
  ADC ppu_addr_ptr+1
  STA ppu_addr_ptr+1

  ; cba
  TYA
  .repeat 5
  ASL
  .endrepeat
  CLC
  ADC #$06 ; X offset
  ADC ppu_addr_ptr
  STA ppu_addr_ptr
  BCC :+
  INC ppu_addr_ptr+1
:

  LDA PPUSTATUS
  LDA ppu_addr_ptr+1
  STA PPUADDR
  LDA ppu_addr_ptr
  STA PPUADDR

  ; now let's write a row
  TYA
  SEC
  SBC #$06
  .repeat 3
  ASL
  .endrepeat
  AND #$F0
  ORA #$03
  TAY

  LDA frame_counter
  AND #%1
  BNE even_loop

odd_loop:
  LDA gamekid_ram+wk_var::table,Y
  CMP #wk_symbols::padding
  BNE :+
  LDA current_nametable
  ASL
  ASL
  ORA #$20
  STA PPUADDR
  LDA #$00
  STA PPUADDR
  LDA #$00 ; horizontal scroll
  STA PPUSCROLL
  STA PPUSCROLL
  RTS
:
  CMP #wk_symbols::empty
  BNE :+
draw_empty:
  LDA #$00
  STA PPUDATA
  STA PPUDATA
  INY
  JMP odd_loop
:
  CMP #wk_symbols::wall
  BNE :+
  LDA #$C0
  STA PPUDATA
  LDA #$C1
  STA PPUDATA
  INY
  JMP odd_loop
:
  CMP #wk_symbols::box
  BNE :+
  ; TODO create box object
  JMP draw_empty
:
  CMP #wk_symbols::goal
  BNE :+
  LDA #$C2
  STA PPUDATA
  LDA #$C3
  STA PPUDATA
  INY
  JMP odd_loop
:
  CMP #wk_symbols::player
  BNE return
  ; TODO create player
  JMP draw_empty


even_loop:
  LDA gamekid_ram+wk_var::table,Y
  CMP #wk_symbols::padding
  BNE :+
  LDA current_nametable
  ASL
  ASL
  ORA #$20
  STA PPUADDR
  LDA #$00
  STA PPUADDR
  LDA #$00 ; horizontal scroll
  STA PPUSCROLL
  STA PPUSCROLL
  RTS
:
  CMP #wk_symbols::empty
  BNE :+
even_draw_empty:
  LDA #$00
  STA PPUDATA
  STA PPUDATA
  INY
  JMP even_loop
:
  CMP #wk_symbols::wall
  BNE :+
  LDA #$D0
  STA PPUDATA
  LDA #$D1
  STA PPUDATA
  INY
  JMP even_loop
:
  CMP #wk_symbols::box
  BNE :+
  ; TODO create box object
  JMP even_draw_empty
:
  CMP #wk_symbols::goal
  BNE :+
  LDA #$D2
  STA PPUDATA
  LDA #$D3
  STA PPUDATA
  INY
  JMP even_loop
:
  CMP #wk_symbols::player
  BNE return
  ; TODO create player
  JMP even_draw_empty

return:
  RTS
.endproc

.proc wk_playing
  ; player input
  ; first, save old xys
  LDX #$4
:
  LDA gamekid_ram+wk_var::player_xy,X
  STA gamekid_ram+wk_var::old_xy,X
  DEX
  BPL :-

  JSR readjoy
  LDA pressed_buttons
  AND #BUTTON_B
  BEQ :++

  LDX #$04
:
  LDA gamekid_ram+wk_var::start_xy,X
  STA gamekid_ram+wk_var::player_xy,X
  DEX
  BPL :-

:
  LDA pressed_buttons
  AND #BUTTON_UP
  BEQ :+

  SEC
  LDA gamekid_ram+wk_var::player_xy
  SBC #$10
  STA gamekid_ram+wk_var::player_xy
  JMP post_move
:
  LDA pressed_buttons
  AND #BUTTON_DOWN
  BEQ :+
  CLC
  LDA gamekid_ram+wk_var::player_xy
  ADC #$10
  STA gamekid_ram+wk_var::player_xy
  JMP post_move
:
  LDA pressed_buttons
  AND #BUTTON_LEFT
  BEQ :+
  DEC gamekid_ram+wk_var::player_xy
  JMP post_move
:
  LDA pressed_buttons
  AND #BUTTON_RIGHT
  BEQ :+
  INC gamekid_ram+wk_var::player_xy
  JMP post_move
:
  ; no key pressed
  JMP after_move

post_move:
  LDX gamekid_ram+wk_var::player_xy
  LDA gamekid_ram+wk_var::table,X
  CMP #wk_symbols::wall
  BEQ undo_move

  ; check box collision
  LDX #$03
  LDA gamekid_ram+wk_var::player_xy
:
  CMP gamekid_ram+wk_var::box_xy,X
  BEQ push_box
  DEX
  BPL :-
  JMP after_move
push_box:
  ; move Xth box by same direction as player
  ; A = player xy
  SEC
  SBC gamekid_ram+wk_var::old_xy
  CLC
  ADC gamekid_ram+wk_var::box_xy,X
  STA gamekid_ram+wk_var::box_xy,X

  ; check if box-box collision happened
  ; first we swap first box with Xth box
  LDA gamekid_ram+wk_var::box_xy,X
  PHA
  LDA gamekid_ram+wk_var::box_xy
  STA gamekid_ram+wk_var::box_xy,X
  PLA
  STA gamekid_ram+wk_var::box_xy

  ; now we check if first box matches any other
  ; (we can cobble X now)
  LDX #$03
:
  CMP gamekid_ram+wk_var::box_xy,X
  BEQ undo_move
  DEX
  BNE :-

  ; check if box-wall collision happened
  LDA gamekid_ram+wk_var::box_xy
  TAX
  LDA gamekid_ram+wk_var::table,X
  CMP #wk_symbols::wall
  BEQ undo_move

  JMP after_move

undo_move:
  LDX #$04
:
  LDA gamekid_ram+wk_var::old_xy,X
  STA gamekid_ram+wk_var::player_xy,X
  DEX
  BPL :-

after_move:

  ; draw elements
  LDA #0
  STA sprite_counter

  LDA #<wk_player_sprite
  STA addr_ptr
  LDA #>wk_player_sprite
  STA addr_ptr+1
  LDA gamekid_ram+wk_var::player_xy
  JSR gamekid_xy_to_coordinates
  JSR display_metasprite

  LDA #<wk_box_sprite
  STA addr_ptr
  LDA #>wk_box_sprite
  STA addr_ptr+1

  ; TODO optimize?
  LDA #$00
  STA temp_a
  STA temp_b
draw_loop:
  LDA temp_a
  TAX
  LDA gamekid_ram+wk_var::box_xy,X
  BEQ after_drawing

  JSR gamekid_xy_to_coordinates
  JSR display_metasprite

  LDX temp_a
  LDA gamekid_ram+wk_var::box_xy,X
  TAX
  LDA gamekid_ram+wk_var::table,X
  CMP #wk_symbols::goal
  BNE :+
  INC temp_b
:

  INC temp_a
  LDA #$04
  CMP temp_a
  BNE draw_loop

after_drawing:
  LDA temp_a
  CMP temp_b
  BNE return

  ; win the level
  INC current_sub_level
  LDA #((wk_levels_end - wk_levels)/2)
  CMP current_sub_level
  BEQ win
  JSR wk_load_level_data
  LDA #$00
  STA frame_counter
  LDA #game_states::wk_load_next_level
  STA game_state
  JMP return
win:
  LDA #game_states::wk_win
  STA game_state
  LDA #$00
  STA frame_counter
return:
  RTS
.endproc

.proc wk_load_next_level
  INC frame_counter
  LDA #GAMEKID_DELAY
  CMP frame_counter
  BNE :+
  LDA current_nametable
  EOR #%1
  STA current_nametable
  LDA #game_states::wk_playing
  STA game_state
  RTS
:
  JSR wk_partial_draw_level
  RTS
.endproc

.proc wk_win
  LDA #$AC
  STA ppu_addr_ptr
  LDA current_nametable
  ASL
  ASL
  ORA #$21
  PHA
  STA ppu_addr_ptr+1
  print string_you_win
  PLA
  STA PPUADDR
  LDA #$00
  STA PPUADDR
  KIL ; TODO - return to main game (with push power)
  RTS
.endproc

.proc gi_booting_gamekid
  LDA #game_states::gi_title
  JSR gk_booting_gamekid
  RTS
.endproc

.proc gi_title
  LDA frame_counter
  BNE wait_for_level

  ; insta scroll to title
  LDA #$01
  STA current_nametable

wait_for_level:
  ; TODO: optimize
  INC frame_counter
  LDA #GAMEKID_DELAY ; wait a second
  CMP frame_counter
  BNE :+
  LDA #$00
  STA current_nametable

  ; gi setup
  LDA #game_states::gi_playing
  STA game_state
  LDA #$78
  STA gamekid_ram+gi_var::player_x
  LDA #$05
  STA gamekid_ram+gi_var::player_lives
  TAX

  LDA #$22
  STA ppu_addr_ptr+1
  LDA #$E6
  STA ppu_addr_ptr
  print string_lives
  LDA #$20
  STA PPUADDR
  LDA #$00
  STA PPUADDR

  LDA #GI_TOTAL_ENEMIES
  STA gamekid_ram+gi_var::total_enemies
  LDA #$00
  STA gamekid_ram+gi_var::bullet_y
  STA gamekid_ram+gi_var::num_enemies

:
  ; use the wait to draw the level bg, row by row (gotta go fast)
  JSR gi_partial_draw_level
  RTS
.endproc

.proc gi_partial_draw_level
  LDA frame_counter
  CMP #$06
  BCS :+
  RTS
:
  CMP #$18
  BCC :+
  RTS
:

  TAY
  LDA current_nametable
  EOR #%1
  ASL
  ASL
  ORA #$20
  STA ppu_addr_ptr+1
  LDA #$00
  STA ppu_addr_ptr

  ; adding y*$20 to ppu_addr_ptr
  ; 76543210         76543210 76543210
  ; 000edcba x $20 = 000000ed cba00000

  ; ed
  TYA
  .repeat 3
  LSR
  .endrepeat
  CLC
  ADC ppu_addr_ptr+1
  STA ppu_addr_ptr+1

  ; cba
  TYA
  .repeat 5
  ASL
  .endrepeat
  CLC
  ADC #$06 ; X offset
  ADC ppu_addr_ptr
  STA ppu_addr_ptr
  BCC :+
  INC ppu_addr_ptr+1
:

  LDA PPUSTATUS
  LDA ppu_addr_ptr+1
  STA PPUADDR
  LDA ppu_addr_ptr
  STA PPUADDR

  LDX #$09
row_loop:
  JSR rand
  LDA rng_seed
  AND #%1100
  BEQ :+
  LDA #$C8
  JMP :++
:
  LDA rng_seed
  AND #%11
  CLC
  ADC #$C8
:
  STA PPUDATA


  LDA rng_seed+1
  AND #%1100
  BEQ :+
  LDA #$C8
  JMP :++
:
  LDA rng_seed+1
  AND #%11
  CLC
  ADC #$C8
:
  STA PPUDATA
  DEX
  BPL row_loop

  LDA current_nametable
  ASL
  ASL
  ORA #$20
  STA PPUADDR
  LDA #$00
  STA PPUADDR
  LDA #$00 ; horizontal scroll
  STA PPUSCROLL
  STA PPUSCROLL

  RTS
.endproc

.proc gi_lose_life
  ; subroutine to cause and deal with life loss
  TXA
  PHA
  DEC gamekid_ram+gi_var::player_lives
  LDX gamekid_ram+gi_var::player_lives

  LDA #$22
  STA ppu_addr_ptr+1
  LDA #$E6
  STA ppu_addr_ptr
  print string_lives
  LDA #$20
  STA PPUADDR
  LDA #$00
  STA PPUADDR

  LDA gamekid_ram+gi_var::player_lives
  BNE no_death

  LDA #game_states::gi_lose
  STA game_state

  LDA #$21
  STA ppu_addr_ptr+1
  LDA #$AB
  STA ppu_addr_ptr
  print string_game_over
  LDA #$20
  STA PPUADDR
  LDA #$00
  STA PPUADDR

  STA frame_counter

no_death:
  PLA
  TAX
  RTS
.endproc

.proc gi_collisions
  ; subroutine for player-enemy and bullet-enemy collisions
  ; (mostly to avoid long jmp)
  LDX gamekid_ram+gi_var::num_enemies
  DEX
collision_loop:
  LDA gamekid_ram+gi_var::enemy_y, X

  ; enemy-player collision
  CMP #$A2
  BNE check_bullet_collision

  ; if player_center >= enemy_x && player_center <= enemy_x + 16 then collided
  ; player_center = player_x + 8
  ; if player_x + 8 >= enemy_x && player_x + 8 <= enemy_x + 16 then collided
  ; CMP compares A >= M, so rewriting all calculations on the "A" side...
  ; if player_x + 8 >= enemy_x && enemy_x + 8 >= player_x then collided
  LDA gamekid_ram+gi_var::player_x
  CLC
  ADC #$08
  CMP gamekid_ram+gi_var::enemy_x, X
  BCC next_collision_iteration

  LDA gamekid_ram+gi_var::enemy_x, X
  CLC
  ADC #$08
  CMP gamekid_ram+gi_var::player_x
  BCC next_collision_iteration

  ; reduce player lives
  JSR gi_lose_life

  JMP delete_enemy

check_bullet_collision:
  ; enemy-bullet collision
  ; if bullet_center >= enemy_x && bullet_center <= enemy_x + 16 then collided
  ; bullet_center = bullet_x + 4
  ; if bullet_x + 4 >= enemy_x && bullet_x + 4 <= enemy_x + 16 then collided
  ; CMP compares A >= M, so rewriting all calculations on the "A" side...
  ; if bullet_x + 4 >= enemy_x && enemy_x + 12 >= bullet_x then collided
  LDA gamekid_ram+gi_var::bullet_x
  CLC
  ADC #$04
  CMP gamekid_ram+gi_var::enemy_x, X
  BCC next_collision_iteration

  LDA gamekid_ram+gi_var::enemy_x, X
  CLC
  ADC #$0C
  CMP gamekid_ram+gi_var::bullet_x
  BCC next_collision_iteration

  ; if bullet_center >= enemy_y && bullet_center <= enemy_y + 8 then collided
  ; bullet_center = bullet_y + 4
  ; if bullet_y + 4 >= enemy_y && bullet_y + 4 <= enemy_y + 8 then collided
  ; CMP compares A >= M, so rewriting all calculations on the "A" side...
  ; if bullet_y + 4 >= enemy_y && enemy_y + 4 >= bullet_y then collided
  LDA gamekid_ram+gi_var::bullet_y
  CLC
  ADC #$04
  CMP gamekid_ram+gi_var::enemy_y, X
  BCC next_collision_iteration

  LDA gamekid_ram+gi_var::enemy_y, X
  CLC
  ADC #$04
  CMP gamekid_ram+gi_var::bullet_y
  BCC next_collision_iteration

  ; delete bullet
  LDA #$00
  STA gamekid_ram+gi_var::bullet_y

delete_enemy:
  ; delete the enemy
  LDY gamekid_ram+gi_var::num_enemies
  DEY
  LDA gamekid_ram+gi_var::enemy_x, Y
  STA gamekid_ram+gi_var::enemy_x, X
  LDA gamekid_ram+gi_var::enemy_y, Y
  STA gamekid_ram+gi_var::enemy_y, X
  LDA gamekid_ram+gi_var::enemy_direction, Y
  STA gamekid_ram+gi_var::enemy_direction, X
  STY gamekid_ram+gi_var::num_enemies

next_collision_iteration:
  DEX
  BPL collision_loop
  RTS
.endproc

.proc gi_set_enemy_speed
  ; input X = current enemy index
  ; if there's few enemies left it can be faster, randomly
  ; else it'll be simply +1 / -1
  CLC
  LDA gamekid_ram+gi_var::total_enemies
  ADC gamekid_ram+gi_var::num_enemies
  CLC
  CMP #(GI_TOTAL_ENEMIES/2)
  BCC maybe_faster
normal_speed:
  LDA gamekid_ram+gi_var::enemy_direction, X
  AND #$F0
  BEQ normal_negative
normal_positive:
  LDA #$01
  JMP return
normal_negative:
  LDA #$FF
  JMP return
maybe_faster:
  LDA rng_seed+1
  AND #%1
  BEQ normal_speed
  LDA gamekid_ram+gi_var::enemy_direction, X
  AND #$F0
  BEQ faster_negative
faster_positive:
  LDA #$02
  JMP return
faster_negative:
  LDA #$FE
  ; implicit JMP return
return:
  STA gamekid_ram+gi_var::enemy_direction, X
  RTS
.endproc

.proc gi_playing
  JSR readjoy
  LDA buttons
  AND #BUTTON_LEFT
  BEQ :+

  LDA gamekid_ram+gi_var::player_x
  CMP #$32
  BEQ :+
  DEC gamekid_ram+gi_var::player_x
:
  LDA buttons
  AND #BUTTON_RIGHT
  BEQ :+

  LDA gamekid_ram+gi_var::player_x
  CMP #$BD
  BEQ :+
  INC gamekid_ram+gi_var::player_x
:
  LDA pressed_buttons
  AND #BUTTON_A
  BEQ :+
  LDA gamekid_ram+gi_var::bullet_y
  BNE :+
  LDA #$A8
  STA gamekid_ram+gi_var::bullet_y
  LDA gamekid_ram+gi_var::player_x
  CLC
  ADC #$04
  STA gamekid_ram+gi_var::bullet_x
:

  ; update bullet
  LDA gamekid_ram+gi_var::bullet_y
  BEQ :++
  CMP #$30
  BNE :+
  LDA #$00
  STA gamekid_ram+gi_var::bullet_y
  JMP :++
:
  .repeat 2
  DEC gamekid_ram+gi_var::bullet_y
  .endrepeat
:

  ; update enemies
  LDA gamekid_ram+gi_var::num_enemies
  CMP #GI_MAX_ENEMIES
  BEQ move_enemies

  ; maybe add an enemy
  LDA gamekid_ram+gi_var::total_enemies
  BEQ move_enemies

  LDA nmis
  AND #%11111
  BNE move_enemies
  JSR rand

  LDA gamekid_ram+gi_var::num_enemies
  BNE unlikely
  LDA #%0
  JMP :+
unlikely:
  CMP #$04
  BCS less_likely
  LDA #%1
  JMP :+
less_likely:
  LDA #%11
:
  AND rng_seed
  BNE move_enemies

  LDX gamekid_ram+gi_var::num_enemies
  INC gamekid_ram+gi_var::num_enemies

  LDA #$32
  STA gamekid_ram+gi_var::enemy_x, X
  STA gamekid_ram+gi_var::enemy_y, X

  LDA #$FF
  STA gamekid_ram+gi_var::enemy_direction, X
  JSR gi_set_enemy_speed
  DEC gamekid_ram+gi_var::total_enemies

move_enemies:
  LDX gamekid_ram+gi_var::num_enemies
  BEQ skip_loop
  DEX
move_loop:
  LDA gamekid_ram+gi_var::enemy_direction, X
  CLC
  ADC gamekid_ram+gi_var::enemy_x, X
  STA gamekid_ram+gi_var::enemy_x, X
  CMP #$BB
  BCC check_left
  LDA #$BB
  STA gamekid_ram+gi_var::enemy_x, X
  LDA gamekid_ram+gi_var::enemy_y, X
  CLC
  ADC #$10
  STA gamekid_ram+gi_var::enemy_y, X

  JSR gi_set_enemy_speed

  JMP next_move
check_left:
  LDA #$31
  CMP gamekid_ram+gi_var::enemy_x, X
  BCC next_move
  STA gamekid_ram+gi_var::enemy_x, X
  LDA gamekid_ram+gi_var::enemy_y, X
  CLC
  ADC #$10
  STA gamekid_ram+gi_var::enemy_y, X

  JSR gi_set_enemy_speed

next_move:
  DEX
  BPL move_loop

  JSR gi_collisions

skip_loop:

  ; draw elements
  LDA #0
  STA sprite_counter
  LDA #<gi_player_sprite
  STA addr_ptr
  LDA #>gi_player_sprite
  STA addr_ptr+1
  LDA gamekid_ram+gi_var::player_x
  STA temp_x
  LDA #$A8
  STA temp_y
  JSR display_metasprite

  LDA gamekid_ram+gi_var::num_enemies
  BEQ skip_draw_loop
  TAX
  DEX

  LDA #<gi_enemy_sprite
  STA addr_ptr
  LDA #>gi_enemy_sprite
  STA addr_ptr+1

draw_loop:
  LDA gamekid_ram+gi_var::enemy_x, X
  STA temp_x
  LDA gamekid_ram+gi_var::enemy_y, X
  STA temp_y
  TXA
  PHA
  JSR display_metasprite
  PLA
  TAX
  DEX
  BPL draw_loop

skip_draw_loop:

  LDA gamekid_ram+gi_var::bullet_y
  BEQ :+

  LDA #<gi_bullet_sprite
  STA addr_ptr
  LDA #>gi_bullet_sprite
  STA addr_ptr+1
  LDA gamekid_ram+gi_var::bullet_x
  STA temp_x
  LDA gamekid_ram+gi_var::bullet_y
  STA temp_y
  JSR display_metasprite
:

  ; ensure we erase sprites if we lost a metasprite before
  LDX sprite_counter
  LDA #$F0
:
  STA oam_sprites+Sprite::ycoord, X
  .repeat .sizeof(Sprite)
  INX
  .endrepeat
  BNE :-

  ; win?
  LDA gamekid_ram+gi_var::player_lives
  BEQ :+
  CLC
  LDA gamekid_ram+gi_var::num_enemies
  ADC gamekid_ram+gi_var::total_enemies
  BNE :+

  LDA #game_states::gi_win
  STA game_state

:
  RTS
.endproc

.proc gi_win
  LDA #$21
  STA ppu_addr_ptr+1
  LDA #$AC
  STA ppu_addr_ptr
  print string_you_win
  LDA #$20
  STA PPUADDR
  LDA #$00
  STA PPUADDR
  KIL ; TODO - return to main game (with fireball power)
  RTS
.endproc

.proc gi_lose
  INC frame_counter
  LDA frame_counter
  CMP #GAMEKID_DELAY
  BNE return

  ; erase sprites
  LDA #$F0
  LDX #$00
:
  STA oam_sprites+Sprite::ycoord,X
  .repeat .sizeof(Sprite)
  INX
  .endrepeat
  BNE :-

  ; back to title
  LDA #$00
  STA frame_counter
  LDA #game_states::gi_title
  STA game_state

return:
  RTS
.endproc

.proc gamekid_xy_to_coordinates
  ; input: A = gamekid xy coordinates (high nibble y, low nibble x)
  ; output: temp_x and temp_y = screen xy coordinates
  STA temp_y ; save A

  ; compute X
  AND #%00001111
  .repeat 4
  ASL
  .endrepeat
  STA temp_x

  ; compute Y
  LDA temp_y
  AND #%11110000
  CLC
  ADC #$30 ; y coordinate skips some unused rows in order to save some ram
  STA temp_y
  RTS
.endproc

.proc display_metasprite
  ; input: (addr_ptr) = metasprite pointer
  ;        temp_x and temp_y = screen position for metasprite origin

  LDY #0
  LDX sprite_counter
loop:
  LDA (addr_ptr),Y ; delta x
  CMP #128
  BEQ return
  INY
  CLC
  ADC temp_x
  STA oam_sprites+Sprite::xcoord,X
  LDA (addr_ptr),Y ; delta y
  INY
  SEC
  SBC #$01
  CLC
  ADC temp_y
  STA oam_sprites+Sprite::ycoord,X
  LDA (addr_ptr),Y ; tile
  INY
  STA oam_sprites+Sprite::tile,X
  LDA (addr_ptr),Y ; flags
  INY
  STA oam_sprites+Sprite::flag,X
  .repeat .sizeof(Sprite)
  INX
  .endrepeat
  JMP loop
return:
  STX sprite_counter
  RTS
.endproc

.segment "VECTORS"
.addr nmi_handler, reset_handler, irq_handler

.segment "RODATA"

game_state_handlers_l:
  .byte <(main_playing-1)
  .byte <(wk_booting_gamekid-1)
  .byte <(wk_title-1)
  .byte <(wk_load_next_level-1)
  .byte <(wk_playing-1)
  .byte <(wk_win-1)
  .byte <(gi_booting_gamekid-1)
  .byte <(gi_title-1)
  .byte <(gi_playing-1)
  .byte <(gi_win-1)
  .byte <(gi_lose-1)

game_state_handlers_h:
  .byte >(main_playing-1)
  .byte >(wk_booting_gamekid-1)
  .byte >(wk_title-1)
  .byte >(wk_load_next_level-1)
  .byte >(wk_playing-1)
  .byte >(wk_win-1)
  .byte >(gi_booting_gamekid-1)
  .byte >(gi_title-1)
  .byte >(gi_playing-1)
  .byte >(gi_win-1)
  .byte >(gi_lose-1)

palettes:
.incbin "../assets/bg-palettes.pal"
.incbin "../assets/sprite-palettes.pal"

sprites:
.include "../assets/metasprites.s"

wk_box_sprite = metasprite_0_data
wk_player_sprite = metasprite_1_data
gi_player_sprite = metasprite_2_data
gi_bullet_sprite = metasprite_3_data
gi_enemy_sprite = metasprite_4_data

strings:
string_game_over: .byte "GAME", $5B, "OVER", $00
string_lives: .byte "LIVES", $5B, WRITE_X_SYMBOL, $00
string_you_win: .byte "YOU", $5B, "WIN", $00

levels:
        .word level_0_data

        ; level data format:
        ; pointer to rle bg nametable
level_0_data:
        .word nametable_level_0



wk_levels:
        .word wk_level_1_data
        .word wk_level_2_data
        .word wk_level_3_data
wk_levels_end:

; format:
; 9-rows, 16-column matrix of bytes
; cell values tell if it's padding, space, wall, box, goal, player
       ;                     '-'    ' '    '#'   'o'   'x'  '@'
; 3 columns each side are just padding, makes math easier

wk_level_1_data:
        .byte "---##########---"
        .byte "---#        #---"
        .byte "---#   @    #---"
        .byte "---#        #---"
        .byte "---#x o  o x#---"
        .byte "---#        #---"
        .byte "---#        #---"
        .byte "---#        #---"
        .byte "---##########---"

wk_level_2_data:
        .byte "---##########---"
        .byte "---#   x#   #---"
        .byte "---# #      #---"
        .byte "---# # ##   #---"
        .byte "---#@   oo ##---"
        .byte "---# # ##   #---"
        .byte "---# #      #---"
        .byte "---#   x#   #---"
        .byte "---##########---"

wk_level_3_data:
        .byte "---##########---"
        .byte "---#    o  x#---"
        .byte "---#   oo   #---"
        .byte "---#    o  x#---"
        .byte "---#####  ###---"
        .byte "---#       x#---"
        .byte "---#   @    #---"
        .byte "---#       x#---"
        .byte "---##########---"

nametable_level_0: .incbin "../assets/level/level-0.rle"
nametable_gamekid_boot: .incbin "../assets/gamekid-boot.rle"
nametable_wk_title: .incbin "../assets/wk-level/title.rle"
nametable_gi_title: .incbin "../assets/gi-level/title.rle"

subgame_by_game_state:
        .byte $00 ; main
        .byte $01, $01, $01, $01, $01 ; WK
        .byte $02, $02, $02, $02 ; GI

subgame_nametables_l:
        .byte $00
        .byte <nametable_wk_title
        .byte <nametable_gi_title
subgame_nametables_h:
        .byte $00
        .byte >nametable_wk_title
        .byte >nametable_gi_title

; music and sfx data
;.include "../assets/music/some-music.s"
;.include "../assets/music/some-sfx.s"

.segment "CHR"
.incbin "../assets/graphics.chr"
