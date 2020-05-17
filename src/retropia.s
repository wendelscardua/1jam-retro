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
  ; main game stuff
  main_playing
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
  ; mf = mine finder (minesweeper clone)
  mf_booting_gamekid
  mf_title
  mf_playing
  mf_win
  mf_lose
  ; rr = river ray (finite swimmer)
  rr_booting_gamekid
  rr_title
  rr_playing
  rr_win
  rr_lose
.endenum

.struct Box
  x1 .byte
  y1 .byte
  x2 .byte
  y2 .byte
.endstruct

.struct AnimData
  up_sprite_1 .word
  up_sprite_2 .word
  down_sprite_1 .word
  down_sprite_2 .word
  left_sprite_1 .word
  left_sprite_2 .word
  right_sprite_1 .word
  right_sprite_2 .word
.endstruct

.enum object_type
  player
.endenum

.enum direction
  up
  down
  left
  right
.endenum

.struct Exit
  up    .byte
  down  .byte
  left  .byte
  right .byte
.endstruct

MAX_OBJECTS=10
.struct Object ; used as struct of arrays
  type            .res 10 ; enum object_type
  xcoord          .res 10
  ycoord          .res 10
  direction       .res 10 ; enum direction
  sprite_toggle   .res 10 ; which of the 2 available metasprites to use
.endstruct

.importzp rng_seed
.importzp buttons
.importzp last_frame_buttons
.importzp released_buttons
.importzp pressed_buttons
.importzp rle_ptr

addr_ptr: .res 2 ; generic address pointer
ppu_addr_ptr: .res 2 ; temporary address for PPU_ADDR
second_rle_ptr: .res 2 ; secondary nametable pointer
palette_ptr: .res 2 ; pointer to palettes
nmis: .res 1
old_nmis: .res 1
args: .res 5
game_state: .res 1
current_nametable: .res 1
current_level: .res 1
current_exits: .res 4 ; up, down, left, right
current_sub_level: .res 1
frame_counter: .res 1
sprite_counter: .res 1
temp_a: .res 1
temp_b: .res 1
temp_x: .res 1
temp_y: .res 1
num_objects: .res 1
objects: .tag Object

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

MF_BOMBS=8
.struct mf_var
  player_x .byte ; table coordinates (0..7)
  player_y .byte ; idem
  ready .byte
  opened_cells .byte
  table .res 64
  bomb_table .res 64
  status .res 64
.endstruct

.enum mf_tile_indices
  number_0 = 0
  number_1 = 4
  number_2 = 8
  number_3 = 12
  number_4 = 16
  flag = 20
  closed = 24
  bomb = 28
.endenum

.enum mf_cell_status
  closed = 0
  flagged
  opened
.endenum

RR_MAX_BARRIERS=$1F
RR_MIN_DELAY=20
RR_INITIAL_DELAY=120
RR_DELTA_DELAY=20
RR_SPEEDUP_DELAY=15 * 60 ; 15 seconds
RR_FLAG_DELAY=180
.struct rr_var
  player_y .byte
  player_lives .byte
  speedup_timer .word
  barrier_pattern .byte
  next_barrier_counter .byte
  barrier_delay .byte
  barrier_x .res $1F
  barrier_y .res $1F
  flag_x .byte
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
  JSR load_palettes

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

  ; TODO: change to title screen when available
  LDA #game_states::main_playing
  STA game_state
  LDA #1
  STA current_level
  STA num_objects
  LDA #$80
  STA objects+Object::xcoord
  STA objects+Object::ycoord
  LDA #object_type::player
  STA objects+Object::type
  LDA #direction::down
  STA objects+Object::direction
  LDA #$00
  STA objects+Object::sprite_toggle
  JSR load_level

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

.proc load_palettes
  ; input: palette_ptr points to palettes
  ; cobbles Y
  LDY PPUSTATUS
  LDY #$3f
  STY PPUADDR
  LDY #$00
  STY PPUADDR
:
  LDA (palette_ptr),Y
  STA PPUDATA
  INY
  CPY #$20
  BNE :-
  RTS
.endproc

.proc load_level
  ; loads current level for main game
  LDX current_level
  LDA levels_l, X
  STA addr_ptr
  LDA levels_h, X
  STA addr_ptr+1

  LDY #0

  ; load nametable pointer
  LDA (addr_ptr),Y
  INY
  STA rle_ptr
  LDA (addr_ptr),Y
  INY
  STA rle_ptr+1

  ; load exits
  LDA (addr_ptr),Y
  INY
  STA current_exits+Exit::up

  LDA (addr_ptr),Y
  INY
  STA current_exits+Exit::down

  LDA (addr_ptr),Y
  INY
  STA current_exits+Exit::left

  LDA (addr_ptr),Y
  INY
  STA current_exits+Exit::right

  LDA #<palettes
  STA palette_ptr
  LDA #>palettes
  STA palette_ptr+1
  JSR load_nametable
  RTS
.endproc

.proc load_nametable
; expects rle_ptr to already point to rle data
; if second_rle_ptr is present, uses it to load second bg
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
  LDA second_rle_ptr
  BEQ :++
:  ; wait for another vblank before continuing
  BIT PPUSTATUS
  BPL :-
  LDA second_rle_ptr
  STA rle_ptr
  LDA #$00
  STA second_rle_ptr
  LDA second_rle_ptr+1
  STA rle_ptr+1
  LDA #$00
  STA second_rle_ptr+1
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

  JSR load_palettes

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

.proc load_next_screen
  LDX objects+Object::direction
  CPX #direction::up
  BEQ wrap_up
  CPX #direction::down
  BEQ wrap_down
  CPX #direction::left
  BEQ wrap_left
  CPX #direction::right
  BEQ wrap_right
  KIL ; never happens (?)
wrap_up:
  LDA #$E0
  STA objects+Object::ycoord
  JMP load
wrap_down:
  LDA #$00
  STA objects+Object::ycoord
  JMP load
wrap_left:
  LDA #$F0
  STA objects+Object::xcoord
  JMP load
wrap_right:
  LDA #$00
  STA objects+Object::xcoord
  JMP load

load:
  LDA current_exits, X
  BNE :+
  KIL ; should never try to load zeroth level
:
  STA current_level
  JSR load_level
  RTS
.endproc

.proc main_playing
  JSR player_input

  LDA buttons
  AND #BUTTON_UP
  BEQ :+
  LDA objects+Object::ycoord
  BNE move_up
  JSR load_next_screen
move_up:
  DEC objects+Object::ycoord
  INC objects+Object::sprite_toggle
  LDA #direction::up
  STA objects+Object::direction
:
  LDA buttons
  AND #BUTTON_DOWN
  BEQ :+
  LDA objects+Object::ycoord
  CMP #$E0
  BNE move_down
  JSR load_next_screen
move_down:
  INC objects+Object::ycoord
  INC objects+Object::sprite_toggle
  LDA #direction::down
  STA objects+Object::direction
:
  LDA buttons
  AND #BUTTON_LEFT
  BEQ :+
  LDA objects+Object::xcoord
  BNE move_left
  JSR load_next_screen
move_left:
  DEC objects+Object::xcoord
  INC objects+Object::sprite_toggle
  LDA #direction::left
  STA objects+Object::direction
:
  LDA buttons
  AND #BUTTON_RIGHT
  BEQ :+
  LDA objects+Object::xcoord
  CMP #$F0
  BNE move_right
  JSR load_next_screen
move_right:
  INC objects+Object::xcoord
  INC objects+Object::sprite_toggle
  LDA #direction::right
  STA objects+Object::direction
:

  ; draw elements
  LDA #0
  STA sprite_counter
  LDX num_objects
  DEX

draw_elements_loop:
  ; Save X
  TXA
  PHA
  ; first we get the pointer to anim_data stuff
  LDY objects+Object::type, X
  LDA anim_data_ptr_l, Y
  STA addr_ptr
  LDA anim_data_ptr_h, Y
  STA addr_ptr+1
  ; then we find the right index to metasprite inside anim_data
  ; Y = 4 * direction + 2 * sprite_toggle_relevant_bit
  CLC
  LDA objects+Object::sprite_toggle, X
  AND #%1000 ; toggle sprite every 8 frames
  LSR
  LSR
  .repeat 4
  ADC objects+Object::direction, X
  .endrepeat
  TAY

  LDA (addr_ptr),Y
  PHA
  INY
  LDA (addr_ptr),Y
  STA addr_ptr+1
  PLA
  STA addr_ptr

  LDA objects+Object::xcoord, X
  STA temp_x
  LDA objects+Object::ycoord, X
  STA temp_y
  JSR display_metasprite
  ; restore X
  PLA
  TAX
  DEX
  BPL draw_elements_loop

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
  ; ... and also subgame title
  LDX game_state
  LDA subgame_by_game_state,X
  TAX
  LDA subgame_nametables_l,X
  STA second_rle_ptr
  LDA subgame_nametables_h,X
  STA second_rle_ptr+1

  LDA #<gamekid_palettes
  STA palette_ptr
  LDA #>gamekid_palettes
  STA palette_ptr+1

  JSR load_nametable

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

.proc mf_booting_gamekid
  JSR gk_booting_gamekid
  RTS
.endproc

.proc mf_title
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

  ; mf setup
  LDA #game_states::mf_playing
  STA game_state
  LDA #$03
  STA gamekid_ram+mf_var::player_x
  STA gamekid_ram+mf_var::player_y
  LDA #$00
  STA gamekid_ram+mf_var::ready
:
  ; use the wait to draw the level bg, row by row (gotta go fast)
  JSR mf_partial_draw_level
  RTS
.endproc

.proc mf_partial_draw_level
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

  LDA #$00
  STA PPUDATA
  STA PPUDATA

  LDX #$08
row_loop:
  LDA frame_counter
  CMP #$06
  BEQ margin
  CMP #$17
  BEQ margin
  AND #%1
  BEQ even_row

odd_row:
  LDA #$E2
  STA PPUDATA
  LDA #$E3
  STA PPUDATA
  JMP next
even_row:
  LDA #$F2
  STA PPUDATA
  LDA #$F3
  STA PPUDATA
  JMP next

margin:
  LDA #$00
  STA PPUDATA
  STA PPUDATA
next:
  DEX
  BNE row_loop

  LDA #$00
  STA PPUDATA
  STA PPUDATA

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

.proc mf_randomize_board
  ; randomize board
  LDA #0
  STA gamekid_ram+mf_var::opened_cells
  LDA #$00
  LDX #63
:
  STA gamekid_ram+mf_var::table,X
  STA gamekid_ram+mf_var::bomb_table,X
  STA gamekid_ram+mf_var::status,X
  DEX
  BPL :-

  LDY #MF_BOMBS
new_bomb_loop:
  TYA
  PHA
  JSR rand
  PLA
  TAY
  LDA rng_seed
  AND #%111111
  CMP temp_a
  BEQ new_bomb_loop
  TAX
  LDA gamekid_ram+mf_var::bomb_table,X
  BNE new_bomb_loop
  INC gamekid_ram+mf_var::bomb_table,X

  TXA
  AND #%111
  STA temp_x
  TXA
  LSR
  LSR
  LSR
  AND #%111
  STA temp_y

  LDA temp_y
  BEQ skip_previous_row

  LDA temp_x
  BEQ :+
  INC gamekid_ram+mf_var::table-9,X
:
  INC gamekid_ram+mf_var::table-8,X

  CMP #7
  BEQ :+
  INC gamekid_ram+mf_var::table-7,X
:

skip_previous_row:
  LDA temp_x
  BEQ :+
  INC gamekid_ram+mf_var::table-1,X
:
  CMP #7
  BEQ :+
  INC gamekid_ram+mf_var::table+1,X
:

  LDA temp_y
  CMP #7
  BEQ skip_next_row

  LDA temp_x
  BEQ :+
  INC gamekid_ram+mf_var::table+7,X
:
  INC gamekid_ram+mf_var::table+8,X

  CMP #7
  BEQ :+
  INC gamekid_ram+mf_var::table+9,X
:

skip_next_row:
  DEY
  BNE new_bomb_loop
  RTS
.endproc

.proc mf_open_cell
  ; convert 8x8 coordinate to array index in temp_a
  LDA gamekid_ram+mf_var::player_y
  .repeat 3
  ASL
  .endrepeat
  CLC
  ADC gamekid_ram+mf_var::player_x
  STA temp_a

  LDA gamekid_ram+mf_var::ready
  BNE open_cell
  INC gamekid_ram+mf_var::ready
:
  JSR mf_randomize_board
  LDX #63
:
  LDA gamekid_ram+mf_var::table,X
  CMP #5
  BCS :--
  DEX
  BPL :-

open_cell:
  LDX temp_a
  LDA gamekid_ram+mf_var::status,X
  BEQ is_closed
  RTS
is_closed:
  LDA #mf_cell_status::opened
  STA gamekid_ram+mf_var::status,X
  LDA gamekid_ram+mf_var::bomb_table,X
  BEQ safe
bomb:
  LDY #mf_tile_indices::bomb
  JSR mf_draw_tile
  LDA #game_states::mf_lose
  STA game_state

  LDA #$20
  STA ppu_addr_ptr+1
  LDA #$CB
  STA ppu_addr_ptr
  print string_game_over
  LDA #$20
  STA PPUADDR
  LDA #$00
  STA PPUADDR
  STA frame_counter
  RTS
safe:
  INC gamekid_ram+mf_var::opened_cells
  ; draw new megatile

  LDA gamekid_ram+mf_var::table,X
  .repeat 2
  ASL
  .endrepeat
  TAY
  JSR mf_draw_tile
  RTS
.endproc

.proc mf_toggle_flag
  ; convert 8x8 coordinate to array index in X
  LDA gamekid_ram+mf_var::player_y
  .repeat 3
  ASL
  .endrepeat
  CLC
  ADC gamekid_ram+mf_var::player_x
  TAX

  LDA gamekid_ram+mf_var::status,X
  BEQ is_closed
  CMP #mf_cell_status::flagged
  BEQ is_flagged
is_opened:
  ; can't flag opened cells
  RTS
is_flagged:
  LDA #mf_cell_status::closed
  STA gamekid_ram+mf_var::status,X
  LDY #mf_tile_indices::closed
  JMP draw_tile
is_closed:
  LDA #mf_cell_status::flagged
  STA gamekid_ram+mf_var::status,X
  LDY #mf_tile_indices::flag
  ; JMP draw_tile
draw_tile:
  JSR mf_draw_tile
  RTS
.endproc

.proc mf_draw_tile
  ; displays a tile (indexed by Y)
  ; at position X (X = 0yyy0xxx)
    LDA #$20
  STA ppu_addr_ptr+1
  LDA #$00
  STA ppu_addr_ptr
  CLC
  TXA
  AND #%100
  BEQ :+
  LDA #$8
  ADC ppu_addr_ptr
  STA ppu_addr_ptr
:
  TXA
  AND #%010
  BEQ :+
  LDA #$4
  ADC ppu_addr_ptr
  STA ppu_addr_ptr
:
  TXA
  AND #%001
  BEQ :+
  LDA #$2
  ADC ppu_addr_ptr
  STA ppu_addr_ptr
:
  LDA #$E8
  ADC ppu_addr_ptr
  STA ppu_addr_ptr
  BCC :+
  INC ppu_addr_ptr+1
:
  TXA
  AND #%100000
  BEQ :+
  INC ppu_addr_ptr+1
:
  TXA
  AND #%010000
  BEQ :+
  LDA #$80
  CLC
  ADC ppu_addr_ptr
  STA ppu_addr_ptr
  BCC :+
  INC ppu_addr_ptr+1
:
  TXA
  AND #%001000
  BEQ :+
  LDA #$40
  CLC
  ADC ppu_addr_ptr
  STA ppu_addr_ptr
  BCC :+
  INC ppu_addr_ptr+1
:

vblankwait:       ; wait for another vblank before continuing
  BIT PPUSTATUS
  BPL vblankwait

  LDA ppu_addr_ptr+1
  STA PPUADDR
  LDA ppu_addr_ptr
  STA PPUADDR

  LDA mf_tiles,Y
  STA PPUDATA
  INY
  LDA mf_tiles,Y
  STA PPUDATA
  INY
  CLC
  LDA #$20
  ADC ppu_addr_ptr
  STA ppu_addr_ptr
  BCC :+
  INC ppu_addr_ptr+1
:
  LDA ppu_addr_ptr+1
  STA PPUADDR
  LDA ppu_addr_ptr
  STA PPUADDR
  LDA mf_tiles,Y
  STA PPUDATA
  INY
  LDA mf_tiles,Y
  STA PPUDATA
  INY

  LDA #$20
  STA PPUADDR
  LDA #$00
  STA PPUADDR
  STA PPUSCROLL
  STA PPUSCROLL
  RTS
.endproc

.proc mf_playing
  JSR readjoy
  LDA pressed_buttons
  AND #BUTTON_LEFT
  BEQ :+

  LDA gamekid_ram+mf_var::player_x
  SEC
  SBC #$01
  AND #%111
  STA gamekid_ram+mf_var::player_x
:
  LDA pressed_buttons
  AND #BUTTON_RIGHT
  BEQ :+

  LDA gamekid_ram+mf_var::player_x
  CLC
  ADC #$01
  AND #%111
  STA gamekid_ram+mf_var::player_x
:
  LDA pressed_buttons
  AND #BUTTON_UP
  BEQ :+

  LDA gamekid_ram+mf_var::player_y
  SEC
  SBC #$01
  AND #%111
  STA gamekid_ram+mf_var::player_y
:
  LDA pressed_buttons
  AND #BUTTON_DOWN
  BEQ :+

  LDA gamekid_ram+mf_var::player_y
  CLC
  ADC #$01
  AND #%111
  STA gamekid_ram+mf_var::player_y
:
  LDA pressed_buttons
  AND #BUTTON_A
  BEQ :+
  JSR mf_open_cell
:
  LDA pressed_buttons
  AND #BUTTON_B
  BEQ :+
  JSR mf_toggle_flag
:

  ; check win
  LDA gamekid_ram+mf_var::opened_cells
  CMP #(.sizeof(mf_var::table) - MF_BOMBS)
  BNE :+
  LDA #game_states::mf_win
  STA game_state
:


  ; draw elements
  LDA #0
  STA sprite_counter
  LDA #<mf_cursor_sprite
  STA addr_ptr
  LDA #>mf_cursor_sprite
  STA addr_ptr+1

  ; if game ended, hide cursor, else compute actual position
  LDA game_state
  CMP #game_states::mf_playing
  BEQ draw_cursor
hide_cursor:
  LDA #$F0
  STA temp_y
  JSR display_metasprite
  RTS
draw_cursor:
  LDA gamekid_ram+mf_var::player_x
  ; X = x * $10 + $40
  .repeat 4
  ASL
  .endrepeat
  CLC
  ADC #$40
  STA temp_x
  LDA gamekid_ram+mf_var::player_y
  ; Y = y * $10 + $38
  .repeat 4
  ASL
  .endrepeat
  CLC
  ADC #$38
  STA temp_y
  JSR display_metasprite

  RTS
.endproc

.proc mf_win
  LDA #$20
  STA ppu_addr_ptr+1
  LDA #$CC
  STA ppu_addr_ptr
  print string_you_win
  LDA #$20
  STA PPUADDR
  LDA #$00
  STA PPUADDR
  KIL ; TODO - return to main game (with bomb)
  RTS
.endproc

.proc mf_lose
  INC frame_counter
  LDA frame_counter
  CMP #GAMEKID_DELAY
  BNE return

  ; back to title
  LDA #$00
  STA frame_counter
  LDA #game_states::mf_title
  STA game_state

return:
  RTS
.endproc

.proc rr_booting_gamekid
  JSR gk_booting_gamekid
  RTS
.endproc

.proc rr_title
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
  BNE skip_setup
  LDA #$00
  STA current_nametable

  ; rr setup
  LDA #game_states::rr_playing
  STA game_state
  LDA #$70
  STA gamekid_ram+rr_var::player_y
  LDA #$00
  STA gamekid_ram+rr_var::flag_x
  LDX #(RR_MAX_BARRIERS-1)
:
  STA gamekid_ram+rr_var::barrier_x, X
  DEX
  BPL :-

  LDA #<RR_SPEEDUP_DELAY
  STA gamekid_ram+rr_var::speedup_timer
  LDA #>RR_SPEEDUP_DELAY
  STA gamekid_ram+rr_var::speedup_timer+1
  LDA #$05
  STA gamekid_ram+rr_var::player_lives

  TAX

  LDA #$20
  STA ppu_addr_ptr+1
  LDA #$C6
  STA ppu_addr_ptr
  print string_lives
  LDA #$20
  STA PPUADDR
  LDA #$00
  STA PPUADDR

:
  JSR rand
  LDA rng_seed
  AND #%11111
  BEQ :-
  CMP #%11111
  BEQ :-
  STA gamekid_ram+rr_var::barrier_pattern
  LDA #RR_INITIAL_DELAY
  STA gamekid_ram+rr_var::next_barrier_counter
  STA gamekid_ram+rr_var::barrier_delay

skip_setup:
  ; use the wait to draw the level bg, row by row (gotta go fast)
  JSR rr_partial_draw_level
  RTS
.endproc

.proc rr_partial_draw_level
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

  LDX #$0A
row_loop:
  LDA frame_counter
  CMP #$0A
  BCC margin
  CMP #$14
  BCS margin

  ; draw river
  LDA #$60
  STA PPUDATA
  STA PPUDATA
  JMP next

margin:
  LDA #$70
  STA PPUDATA
  STA PPUDATA
next:
  DEX
  BNE row_loop

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

.proc rr_check_collision
  ; input: X = index of barrier_{x,y}
  ; reduces player lives if player collided with barrier
  ; changes game state to rr_lose if player has no life left
  LDA gamekid_ram+rr_var::barrier_x, X
  BEQ return
  CMP #$40
  BCS return
  LDA gamekid_ram+rr_var::barrier_y, X
  CMP gamekid_ram+rr_var::player_y
  BNE return
  LDA #$00
  STA gamekid_ram+rr_var::barrier_x, X ; "remove" barrier
  DEC gamekid_ram+rr_var::player_lives

  TXA
  PHA
  LDX gamekid_ram+rr_var::player_lives

  LDA #$20
  STA ppu_addr_ptr+1
  LDA #$C6
  STA ppu_addr_ptr
  print string_lives
  LDA #$20
  STA PPUADDR
  LDA #$00
  STA PPUADDR
  STA PPUSCROLL
  STA PPUSCROLL
  PLA
  TAX
  LDA gamekid_ram+rr_var::player_lives
  BNE return
  LDA #game_states::rr_lose
  STA game_state

  LDA #$21
  STA ppu_addr_ptr+1
  LDA #$8B
  STA ppu_addr_ptr
  print string_game_over
  LDA #$20
  STA PPUADDR
  LDA #$00
  STA PPUADDR
  STA frame_counter
return:
  RTS
.endproc

.proc rr_playing
  JSR readjoy
  LDA pressed_buttons
  AND #BUTTON_UP
  BEQ :+
  LDA gamekid_ram+rr_var::player_y
  CMP #$50
  BEQ :+
  SEC
  SBC #$10
  STA gamekid_ram+rr_var::player_y
:
  LDA pressed_buttons
  AND #BUTTON_DOWN
  BEQ :+
  LDA gamekid_ram+rr_var::player_y
  CMP #$90
  BEQ :+
  CLC
  ADC #$10
  STA gamekid_ram+rr_var::player_y
:

  ; timer for speedup/endgame
  DEC gamekid_ram+rr_var::speedup_timer
  LDA gamekid_ram+rr_var::speedup_timer
  CMP #$FF
  BNE no_speedup
  DEC gamekid_ram+rr_var::speedup_timer+1
  LDA gamekid_ram+rr_var::speedup_timer+1
  CMP #$FF
  BPL no_speedup

  ; speedup time
  LDA #<RR_SPEEDUP_DELAY
  STA gamekid_ram+rr_var::speedup_timer
  LDA #>RR_SPEEDUP_DELAY
  STA gamekid_ram+rr_var::speedup_timer+1
  LDA gamekid_ram+rr_var::barrier_delay
  CMP #RR_MIN_DELAY
  BEQ begin_endgame
  SEC
  SBC #RR_DELTA_DELAY
  STA gamekid_ram+rr_var::barrier_delay
  JMP no_speedup

begin_endgame:
  LDA #RR_FLAG_DELAY
  STA gamekid_ram+rr_var::barrier_delay
  STA gamekid_ram+rr_var::next_barrier_counter
  LDA #%11111
  STA gamekid_ram+rr_var::barrier_pattern

no_speedup:
  ; check for new barriers
  LDA gamekid_ram+rr_var::flag_x
  BNE no_new_barrier
  DEC gamekid_ram+rr_var::next_barrier_counter
  BNE no_new_barrier
  ; reset barrier counter
  LDA gamekid_ram+rr_var::barrier_delay
  STA gamekid_ram+rr_var::next_barrier_counter

  ; TODO add barrier
  LDX #$00 ; index of barrier_{x,y} array
  LDA #$50 ; y position of new barrier
  STA temp_y

  ; check if it's time to end

  LDA gamekid_ram+rr_var::barrier_pattern
  STA temp_a
  CMP #%11111
  BNE barrier_loop
  LDA #$C0
  STA gamekid_ram+rr_var::flag_x
  JMP no_new_barrier
barrier_loop:
  LDA temp_a
  BEQ end_barrier_loop
  CLC
  ROR temp_a
  BCC next_iteration

free_x_loop:
  LDA gamekid_ram+rr_var::barrier_x, X
  BEQ found_free_x
  INX
  JMP free_x_loop
found_free_x:
  LDA #$C8
  STA gamekid_ram+rr_var::barrier_x, X
  LDA temp_y
  STA gamekid_ram+rr_var::barrier_y, X
  INX

next_iteration:
  LDA #$10
  CLC
  ADC temp_y
  STA temp_y
  JMP barrier_loop
end_barrier_loop:


  ; select next barrier in sequence
  LDX gamekid_ram+rr_var::barrier_pattern
  LDA rr_barrier_transitions,X
  STA gamekid_ram+rr_var::barrier_pattern

no_new_barrier:

  ; TODO: update all barriers
  LDA gamekid_ram+rr_var::flag_x
  BEQ :+
  .repeat 2
  DEC gamekid_ram+rr_var::flag_x
  .endrepeat
  BNE :+
  ; flag is gone, game over, you win!
  LDA #game_states::rr_win
  STA game_state
:

  LDX #(RR_MAX_BARRIERS-1)
update_barrier_loop:
  LDA gamekid_ram+rr_var::barrier_x, X
  BEQ next_barrier_to_update
  CMP #$30
  BNE move_barrier

; erase barrier
  LDA #$00
  STA gamekid_ram+rr_var::barrier_x, X
  JMP next_barrier_to_update

move_barrier:
  .repeat 2
  DEC gamekid_ram+rr_var::barrier_x, X
  .endrepeat
  JSR rr_check_collision
next_barrier_to_update:
  DEX
  BPL update_barrier_loop

  ; draw elements
  LDA #0
  STA sprite_counter

  LDA #<rr_player_sprite
  STA addr_ptr
  LDA #>rr_player_sprite
  STA addr_ptr+1
  LDA #$30
  STA temp_x
  LDA gamekid_ram+rr_var::player_y
  STA temp_y
  JSR display_metasprite

  LDA #<rr_barrier_sprite
  STA addr_ptr
  LDA #>rr_barrier_sprite
  STA addr_ptr+1

  LDX #(RR_MAX_BARRIERS-1)
draw_barrier_loop:
  LDA gamekid_ram+rr_var::barrier_x, X
  BEQ skip_draw_barrier
  STA temp_x
  LDA gamekid_ram+rr_var::barrier_y, X
  STA temp_y
  TXA
  PHA
  JSR display_metasprite
  PLA
  TAX
skip_draw_barrier:
  DEX
  BPL draw_barrier_loop

  LDA gamekid_ram+rr_var::flag_x
  BEQ :+
  CMP #$30
  BCC :+
  STA temp_x
  LDA #<rr_flag_sprite
  STA addr_ptr
  LDA #>rr_flag_sprite
  STA addr_ptr+1
  LDA #$50
  STA temp_y
  JSR display_metasprite
  LDA #$60
  STA temp_y
  JSR display_metasprite
  LDA #$70
  STA temp_y
  JSR display_metasprite
  LDA #$80
  STA temp_y
  JSR display_metasprite
  LDA #$90
  STA temp_y
  JSR display_metasprite
:

  LDA #<rr_barrier_sprite
  STA addr_ptr
  LDA #>rr_barrier_sprite
  STA addr_ptr+1


  ; ensure we erase sprites if we lost a metasprite before
  LDX sprite_counter
  LDA #$F0
:
  STA oam_sprites+Sprite::ycoord, X
  .repeat .sizeof(Sprite)
  INX
  .endrepeat
  BNE :-

  RTS
.endproc

.proc rr_win
  LDA #$21
  STA ppu_addr_ptr+1
  LDA #$8C
  STA ppu_addr_ptr
  print string_you_win
  LDA #$20
  STA PPUADDR
  LDA #$00
  STA PPUADDR
  KIL ; TODO - return to main game (with swimming)
  RTS
.endproc

.proc rr_lose
  INC frame_counter
  LDA frame_counter
  CMP #GAMEKID_DELAY
  BNE return

  ; back to title
  LDA #$00
  STA frame_counter
  LDA #game_states::rr_title
  STA game_state

  ; also erase sprites
  LDX 0
  LDA #$F0
:
  STA oam_sprites+Sprite::ycoord, X
  .repeat .sizeof(Sprite)
  INX
  .endrepeat
  BNE :-

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
  ; cobbles X, Y
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
  .byte <(mf_booting_gamekid-1)
  .byte <(mf_title-1)
  .byte <(mf_playing-1)
  .byte <(mf_win-1)
  .byte <(mf_lose-1)
  .byte <(rr_booting_gamekid-1)
  .byte <(rr_title-1)
  .byte <(rr_playing-1)
  .byte <(rr_win-1)
  .byte <(rr_lose-1)

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
  .byte >(mf_booting_gamekid-1)
  .byte >(mf_title-1)
  .byte >(mf_playing-1)
  .byte >(mf_win-1)
  .byte >(mf_lose-1)
  .byte >(rr_booting_gamekid-1)
  .byte >(rr_title-1)
  .byte >(rr_playing-1)
  .byte >(rr_win-1)
  .byte >(rr_lose-1)

palettes:
.incbin "../assets/bg-palettes.pal"
.incbin "../assets/sprite-palettes.pal"
gamekid_palettes:
.incbin "../assets/bg-gk-palettes.pal"
.incbin "../assets/sprite-gk-palettes.pal"

sprites:
.include "../assets/metasprites.s"

wk_box_sprite = metasprite_0_data
wk_player_sprite = metasprite_1_data
gi_player_sprite = metasprite_2_data
gi_bullet_sprite = metasprite_3_data
gi_enemy_sprite = metasprite_4_data
mf_cursor_sprite = metasprite_5_data
rr_player_sprite = metasprite_6_data
rr_barrier_sprite = metasprite_7_data
rr_tree_sprite = metasprite_8_data
rr_flag_sprite = metasprite_9_data

; data fitting AnimData struct
player_anim_data:
        .word metasprite_10_data, metasprite_11_data ; walking up
        .word metasprite_12_data, metasprite_13_data ; walking down
        .word metasprite_14_data, metasprite_15_data ; walking left
        .word metasprite_16_data, metasprite_17_data ; walking right

; indexed by object type
anim_data_ptr_l:
        .byte <player_anim_data
anim_data_ptr_h:
        .byte >player_anim_data

; indexed by object type
hitbox_x1:
        .byte $05
hitbox_y1:
        .byte $00
hitbox_x2:
        .byte $0A
hitbox_y2:
        .byte $0F


strings:
string_game_over: .byte "GAME", $5B, "OVER", $00
string_lives: .byte "LIVES", $5B, WRITE_X_SYMBOL, $00
string_you_win: .byte "YOU", $5B, "WIN", $00

levels_l:
        .byte $00 ; padding
        .byte <level_1_data
        .byte <level_2_data
        .byte <level_3_data
        .byte <level_4_data
        .byte <level_5_data
levels_h:
        .byte $00 ; padding
        .byte >level_1_data
        .byte >level_2_data
        .byte >level_3_data
        .byte >level_4_data
        .byte >level_5_data

        ; level data format:
        ; pointer to rle bg nametable
        ; index of level exits (up,down,left,right)
level_1_data:
        .word nametable_screen_grass_oooo
        .byte $02, $03, $04, $05
level_2_data:
        .word nametable_screen_grass_cocc
        .byte $00, $01, $00, $00
level_3_data:
        .word nametable_screen_grass_occc
        .byte $01, $00, $00, $00
level_4_data:
        .word nametable_screen_grass_ccco
        .byte $00, $00, $00, $01
level_5_data:
        .word nametable_screen_grass_ccoc
        .byte $00, $00, $01, $00

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

mf_tiles:
        .byte $E0, $E1, $F0, $F1 ; 0
        .byte $C4, $C5, $D4, $D5 ; 1
        .byte $C6, $C7, $D6, $D7 ; 2
        .byte $E4, $E5, $F4, $F5 ; 3
        .byte $E6, $E7, $F6, $F7 ; 4
        .byte $CC, $CD, $DC, $DD ; flag
        .byte $E2, $E3, $F2, $F3 ; closed
        .byte $CE, $CF, $DE, $DF ; bomb

rr_barrier_transitions:
        ; computed with transitions.rb
        .byte %00000 ; (endgame)
        .byte %10001 ; from 00001
        .byte %01000 ; from 00010
        .byte %10111 ; from 00011
        .byte %01101 ; from 00100
        .byte %10110 ; from 00101
        .byte %10000 ; from 00110
        .byte %01111 ; from 00111
        .byte %00101 ; from 01000
        .byte %10011 ; from 01001
        .byte %10101 ; from 01010
        .byte %10100 ; from 01011
        .byte %01001 ; from 01100
        .byte %10010 ; from 01101
        .byte %00100 ; from 01110
        .byte %00011 ; from 01111
        .byte %11001 ; from 10000
        .byte %01010 ; from 10001
        .byte %00010 ; from 10010
        .byte %11011 ; from 10011
        .byte %01100 ; from 10100
        .byte %01011 ; from 10101
        .byte %11010 ; from 10110
        .byte %01110 ; from 10111
        .byte %11101 ; from 11000
        .byte %11100 ; from 11001
        .byte %11000 ; from 11010
        .byte %00111 ; from 11011
        .byte %11110 ; from 11100
        .byte %00110 ; from 11101
        .byte %00001 ; from 11110
        .byte %00000 ; from 11111 (victory flag)

nametable_screen_grass_oooo: .incbin "../assets/nametables/screens/grass-oooo.rle"
nametable_screen_grass_occc: .incbin "../assets/nametables/screens/grass-occc.rle"
nametable_screen_grass_cocc: .incbin "../assets/nametables/screens/grass-cocc.rle"
nametable_screen_grass_ccoc: .incbin "../assets/nametables/screens/grass-ccoc.rle"
nametable_screen_grass_ccco: .incbin "../assets/nametables/screens/grass-ccco.rle"

nametable_gamekid_boot: .incbin "../assets/nametables/gamekid-titles/boot.rle"
nametable_wk_title: .incbin "../assets/nametables/gamekid-titles/wk.rle"
nametable_gi_title: .incbin "../assets/nametables/gamekid-titles/gi.rle"
nametable_mf_title: .incbin "../assets/nametables/gamekid-titles/mf.rle"
nametable_rr_title: .incbin "../assets/nametables/gamekid-titles/rr.rle"

subgame_by_game_state:
        .byte $00 ; main
        .byte $01, $01, $01, $01, $01 ; WK
        .byte $02, $02, $02, $02, $02 ; GI
        .byte $03, $03, $03, $03, $03 ; MF
        .byte $04, $04, $04, $04, $04 ; RR

subgame_nametables_l:
        .byte $00
        .byte <nametable_wk_title
        .byte <nametable_gi_title
        .byte <nametable_mf_title
        .byte <nametable_rr_title
subgame_nametables_h:
        .byte $00
        .byte >nametable_wk_title
        .byte >nametable_gi_title
        .byte >nametable_mf_title
        .byte >nametable_rr_title

; music and sfx data
;.include "../assets/music/some-music.s"
;.include "../assets/music/some-sfx.s"

.segment "CHR"
.incbin "../assets/graphics.chr"
