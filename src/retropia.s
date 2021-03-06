.include "constants.inc"
.include "header.inc"

.feature force_range

; famitone2 config
FT_PAL_SUPPORT=0
FT_NTSC_SUPPORT=1
FT_SFX_ENABLE=1
FT_THREAD=1
FT_DPCM_ENABLE=0
FT_SFX_STREAMS=4
FT_DPCM_OFF= $c000

; music/sfx constants
.enum music_track
  Retropian_Jingle
  Saccharine_Saga
  Gamekid_Booting
  Sokowhat
  Invasion
  Sweeping_Mines
  Log_a_rhythm
  Gamekid_Victory
  Gamekid_Defeat
  Glitching
  Free_Faerie
.endenum

.enum sfx
  Menu_Open
  Menu_Close
  Cartridge_Get
  Typing_Text
  Read_More
  Select_Cartridge
  Choose_Cartridge
  Shoot_Fireball
  Fireball_Collision
  Ignited_Bomb
  Explosion
  Pushed_Block
  Swimming
  Damage
.endenum

; debug - macros for NintendulatorDX interaction
.ifdef DEBUG
.macro debugOut str
    .local over
    sta $4040
    jmp over
        .byte str, 0
    over:
.endmacro

.define fHex8( addr ) 1, 0, <(addr), >(addr)
.define fDec8( addr ) 1, 1, <(addr), >(addr)
.define fHex16( addr ) 1, 2, <(addr), >(addr)
.define fDec16( addr ) 1, 3, <(addr), >(addr)
.endif

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

GAMEKID_DELAY = 120
HAS_WK =      %00000001
HAS_GI =      %00000010
HAS_MF =      %00000100
HAS_RR =      %00001000
FINISHED_WK = %00010000
FINISHED_GI = %00100000
FINISHED_MF = %01000000
FINISHED_RR = %10000000

MAIN_EXPLANATION = %00010000
FINAL_QUEST_EXPLANATION = %00100000

BOSS_SCREEN = $0B

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
  main_title
  main_playing
  main_dialog
  main_inventory
  main_dying
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
  enemy_vrissy
  cartridge_wk
  cartridge_gi
  cartridge_mf
  cartridge_rr
  pushable_block
  breakable_wall
  glitch_boss
  faerie
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
  rom_ptr_l       .res 10 ; up to object_type to implement meaning for these
  rom_ptr_h       .res 10
  ram             .res 10
.endstruct

; object rom/ram definition per object_type:
; - vrissy -> walks along a path
; rom: (target-x-or-y new-direction)+ (ends with a 0)
;      - target type depends on current direction
; ram: current rom index
; - pushable block -> can be pushed between two locations
; rom: (target-x-left-or-y-up target-x-right-or-y-down)
; ram: boolean-pushed

.importzp rng_seed
.importzp buttons
.importzp last_frame_buttons
.importzp released_buttons
.importzp pressed_buttons
.importzp rle_ptr

; zp vars
addr_ptr: .res 2 ; generic address pointer
ppu_addr_ptr: .res 2 ; temporary address for PPU_ADDR
second_rle_ptr: .res 2 ; secondary nametable pointer
palette_ptr: .res 2 ; pointer to palettes
nmis: .res 1
old_nmis: .res 1
args: .res 5
game_state: .res 1
lives: .res 1
inventory: .res 1
inventory_selection: .res 1
current_nametable: .res 1
current_screen: .res 1
current_exits: .res 4 ; up, down, left, right
current_screen_soundtrack: .res 1
next_screen_direction: .res 1
current_sub_level: .res 1
frame_counter: .res 1
sprite_counter: .res 1
old_player_x: .res 1
old_player_y: .res 1
entrance_player_x: .res 1
entrance_player_y: .res 1
temp_a: .res 1
temp_b: .res 1
temp_x: .res 1
temp_y: .res 1
temp_hitbox_a: .tag Box
temp_hitbox_b: .tag Box
num_objects: .res 1
objects: .tag Object
dialog_string_ptr: .res 2
dialog_ppu_ptr: .res 2
dialog_callback: .res 2
dialog_current_row: .res 1
fireball_x: .res 1
fireball_y: .res 1
fireball_direction: .res 1
bomb_x: .res 1
bomb_y: .res 1
bomb_countdown: .res 1
explosion_x: .res 1
explosion_y: .res 1
explosion_progress: .res 1
swimming: .res 1
boss_index: .res 1
boss_horizontal: .res 1
boss_vertical: .res 1
boss_h_speed: .res 1
boss_v_speed: .res 1
boss_lives: .res 1
faerie_checklist: .res 1

.segment "BSS"
; non-zp RAM goes here
gamekid_ram: .res $100
wall_x1: .res $20
wall_y1: .res $20
wall_x2: .res $20
wall_y2: .res $20
wall_watery: .res $20
num_walls: .res 1

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

.import music_data
.import sfx_data

.macro KIL ; pseudo instruction to kill the program
  .byte $12
.endmacro

.macro VBLANK
  .local vblankwait
vblankwait:
  BIT PPUSTATUS
  BPL vblankwait
.endmacro

.macro NC_VBLANK
  .local vblankwait
vblankwait:
  LDA nmis
  CMP old_nmis
  BEQ vblankwait
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

  VBLANK

  LDA #%10010000  ; turn on NMIs, sprites use first pattern table
  STA PPUCTRL
  LDA #%00011110  ; turn on screen
  STA PPUMASK

  LDX #<music_data
  LDY #>music_data
  LDA #1
  JSR FamiToneInit

  LDX #<sfx_data
  LDY #>sfx_data
  LDA #1
  JSR FamiToneSfxInit

  ; TODO: change to title screen when available
  JSR title_setup

forever:
  LDA nmis
  CMP old_nmis
  BEQ etc
  STA old_nmis
  ; new frame code
  JSR rand
  JSR game_state_handler
  JSR FamiToneUpdate

etc:
  JMP forever
.endproc

.proc title_setup
  LDA #game_states::main_title
  STA game_state
  JSR load_title_screen
  LDA #music_track::Retropian_Jingle
  JSR FamiToneMusicPlay
  RTS
.endproc

.proc start_game_setup
  LDA #music_track::Saccharine_Saga
  STA current_screen_soundtrack
  JSR FamiToneMusicPlay
  LDA #game_states::main_playing
  STA game_state
  LDA #$01
  STA current_screen
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
  LDA #$00
  STA inventory
  LDA #$01
  STA inventory_selection
  LDA #$03
  STA lives
  LDA #$00
  STA current_nametable

  LDA #$00
  STA faerie_checklist

  ; only useful for boss level
  LDA #direction::left
  STA boss_horizontal
  LDA #2
  STA boss_h_speed
  LDA #direction::down
  STA boss_vertical
  LDA #1
  STA boss_v_speed
  LDA #3
  STA boss_lives

  JSR load_screen
  RTS
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

.macro add_wall x1, y1, x2, y2, watery
  LDA x1
  STA wall_x1, X
  LDA y1
  STA wall_y1, X
  LDA x2
  STA wall_x2, X
  LDA y2
  STA wall_y2, X
  .ifnblank watery
  LDA watery
  STA wall_watery, X
  .endif
  INX
.endmacro

.proc load_screen
  ; save player entrance position
  LDA objects+Object::xcoord
  STA entrance_player_x
  LDA objects+Object::ycoord
  STA entrance_player_y

  ; hide any lost fireballs / bombs
  LDA #$00
  STA fireball_x
  STA fireball_y
  STA fireball_direction
  STA bomb_x
  STA bomb_y
  STA bomb_countdown
  STA explosion_x
  STA explosion_y
  STA explosion_progress

  ; loads current screen for main game
  LDX current_screen
  LDA screens_l, X
  STA addr_ptr
  LDA screens_h, X
  STA addr_ptr+1

  LDY #0

  ; load nametable pointer
  LDA (addr_ptr),Y
  INY
  STA rle_ptr
  LDA (addr_ptr),Y
  INY
  STA rle_ptr+1

  ; load exits / create outer walls
  LDX #$00

  LDA (addr_ptr),Y
  STA current_exits+Exit::up
  BEQ closed_up
opened_up:
  add_wall #$00, #$00, #$57, #$17
  add_wall #$A8, #$00, #$FF, #$17
  JMP :+
closed_up:
  add_wall #$00, #$00, #$FF, #$17
:
  INY
  LDA (addr_ptr),Y
  STA current_exits+Exit::down
  BEQ closed_down
opened_down:
  add_wall #$00, #$D8, #$57, #$EF
  add_wall #$A8, #$D8, #$FF, #$EF
  JMP :+
closed_down:
  add_wall #$00, #$D8, #$FF, #$EF
:
  INY
  LDA (addr_ptr),Y
  STA current_exits+Exit::left
  BEQ closed_left
opened_left:
  add_wall #$00, #$00, #$17, #$47
  add_wall #$00, #$A8, #$17, #$EF
  JMP :+
closed_left:
  add_wall #$00, #$00, #$17, #$EF
:
  INY
  LDA (addr_ptr),Y
  STA current_exits+Exit::right
  BEQ closed_right
opened_right:
  add_wall #$E8, #$00, #$FF, #$47
  add_wall #$E8, #$A8, #$FF, #$EF
  JMP :+
closed_right:
  add_wall #$E8, #$00, #$FF, #$EF
:
  INY

screen_walls_loop:
  LDA (addr_ptr), Y
  BEQ end_of_screen_walls
  STA wall_x1, X
  INY
  LDA (addr_ptr), Y
  STA wall_y1, X
  INY
  LDA (addr_ptr), Y
  STA wall_x2, X
  INY
  LDA (addr_ptr), Y
  STA wall_y2, X
  INY
  LDA (addr_ptr), Y
  STA wall_watery, X
  INY

  INX
  JMP screen_walls_loop
end_of_screen_walls:
  INY
  STX num_walls

  LDX #1
objects_loop:
  LDA (addr_ptr), Y
  BEQ end_of_objects_loop
  STA objects+Object::type, X
  INY

  LDA (addr_ptr), Y
  BEQ end_of_objects_loop
  STA objects+Object::xcoord, X
  INY

  LDA (addr_ptr), Y
  STA objects+Object::ycoord, X
  INY

  LDA (addr_ptr), Y
  STA objects+Object::direction, X
  INY

  LDA (addr_ptr), Y
  STA objects+Object::rom_ptr_l, X
  INY

  LDA (addr_ptr), Y
  STA objects+Object::rom_ptr_h, X
  INY

  LDA (addr_ptr), Y
  STA objects+Object::ram, X
  INY

  INX
  JMP objects_loop
end_of_objects_loop:
  INY

  STX num_objects

  LDA #<palettes
  STA palette_ptr
  LDA #>palettes
  STA palette_ptr+1
  JSR load_nametable
  LDA #$00
  STA PPUSCROLL
  STA PPUSCROLL

  ; if coming from/to boss screen, change soundtrack
  LDA current_screen
  CMP #BOSS_SCREEN
  BNE no_boss_screen
boss_screen:
  LDA #music_track::Glitching
  CMP current_screen_soundtrack
  BEQ no_change
  STA current_screen_soundtrack
  JSR FamiToneMusicPlay
  RTS
no_boss_screen:
  LDA #music_track::Saccharine_Saga
  CMP current_screen_soundtrack
  BEQ no_change
  STA current_screen_soundtrack
  JSR FamiToneMusicPlay
  RTS
no_change:
  RTS
.endproc

.proc load_title_screen
  LDA #<nametable_title
  STA rle_ptr
  LDA #>nametable_title
  STA rle_ptr+1
  LDA #<palettes
  STA palette_ptr
  LDA #>palettes
  STA palette_ptr+1
  JSR load_nametable
  LDA #$00
  STA PPUSCROLL
  STA PPUSCROLL
  RTS
.endproc

.proc load_nametable
; expects rle_ptr to already point to rle data
; if second_rle_ptr is present, uses it to load second bg
; if during main_playing, both bg are the same, but we draw a window on second one
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


  LDA game_state
  CMP #game_states::main_playing
  BNE skip_bg_doubling
  LDA rle_ptr
  STA second_rle_ptr
  LDA rle_ptr+1
  STA second_rle_ptr+1
skip_bg_doubling:

  ; read bg rle pointer and uncompress it
  save_regs
  VBLANK
  LDA #$20
  STA PPUADDR
  LDA #$00
  STA PPUADDR
  JSR unrle

  LDA second_rle_ptr
  BEQ skip_second_bg
  LDA second_rle_ptr
  STA rle_ptr
  LDA #$00
  STA second_rle_ptr
  LDA second_rle_ptr+1
  STA rle_ptr+1
  LDA #$00
  STA second_rle_ptr+1

  VBLANK
  LDA #$24
  STA PPUADDR
  LDA #$00
  STA PPUADDR
  JSR unrle

  LDA game_state
  CMP #game_states::main_playing
  BNE skip_second_bg

  VBLANK

  LDY #$7
rows_loop:
  LDA window_ppu_addrs_h, Y
  STA PPUADDR
  LDA window_ppu_addrs_l, Y
  STA PPUADDR

  LDA window_ppu_left_tile, Y
  STA PPUDATA

  LDX #$15
  LDA window_ppu_center_tile, Y
tile_loop:
  STA PPUDATA
  DEX
  BPL tile_loop

  LDA window_ppu_right_tile, Y
  STA PPUDATA

  DEY
  BPL rows_loop

; color window tiles
  LDA #$27
  STA PPUADDR
  LDA #$E9
  STA PPUADDR
  LDX #$5
:
  LDA #%01010101
  STA PPUDATA
  DEX
  BPL :-

  LDA #$27
  STA PPUADDR
  LDA #$F1
  STA PPUADDR
  LDX #$5
:
  LDA #%01010101
  STA PPUDATA
  DEX
  BPL :-

skip_second_bg:
  restore_regs

  VBLANK

  JSR load_palettes

  BIT PPUSTATUS
  LDA #%10010000  ; turn of NMIs
  STA PPUCTRL
  LDA #%00011110  ; turn on screen
  STA PPUMASK

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

.macro DIALOG string_pointer, callback
  LDA #<string_pointer
  STA dialog_string_ptr
  LDA #>string_pointer
  STA dialog_string_ptr+1
  .ifblank callback
  LDA #<(dialog_to_playing-1)
  STA dialog_callback
  LDA #>(dialog_to_playing-1)
  STA dialog_callback+1
  .else
  LDA #<(callback-1)
  STA dialog_callback
  LDA #>(callback-1)
  STA dialog_callback+1
  .endif
  JSR begin_display_dialog
.endmacro

; these act like printf, displaying the corresponding digit instead
WRITE_X_SYMBOL = $FE

LINEBREAK_SYMBOL = $0A

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
  CMP #LINEBREAK_SYMBOL
  BNE :+
  LDA #$20
  CLC
  ADC ppu_addr_ptr
  STA ppu_addr_ptr
  LDA #$00
  ADC ppu_addr_ptr+1
  STA ppu_addr_ptr+1
  STA PPUADDR
  LDA ppu_addr_ptr
  STA PPUADDR
:
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
  LDX next_screen_direction
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
  STA current_screen
  JSR load_screen
  RTS
.endproc

.proc prepare_player_hitbox
  CLC
  LDA hitbox_x1
  ADC objects+Object::xcoord
  STA temp_hitbox_a+Box::x1
  CLC
  LDA hitbox_y1
  ADC objects+Object::ycoord
  STA temp_hitbox_a+Box::y1
  CLC
  LDA hitbox_x2
  ADC objects+Object::xcoord
  STA temp_hitbox_a+Box::x2
  CLC
  LDA hitbox_y2
  ADC objects+Object::ycoord
  STA temp_hitbox_a+Box::y2
  RTS
.endproc

.proc check_wall_collision
  ; returns 1 in A if player hitbox intersect with any wall
  ; returns 1 in Y if player is swimming
  JSR prepare_player_hitbox
  LDA #0
  STA swimming
  LDX num_walls
  DEX
loop:
  LDA wall_x1, X
  STA temp_hitbox_b+Box::x1
  LDA wall_y1, X
  STA temp_hitbox_b+Box::y1
  LDA wall_x2, X
  STA temp_hitbox_b+Box::x2
  LDA wall_y2, X
  STA temp_hitbox_b+Box::y2
  JSR temp_hitbox_collision
  BEQ next
  LDA wall_watery, X
  BEQ normal_collision
  ; on water, check if player can swim
  LDA inventory
  AND #FINISHED_RR
  BEQ normal_collision
  LDA #1
  STA swimming ; start swimming
  LDA nmis
  AND #%111
  BNE :+
  save_regs
  LDA #sfx::Swimming
  LDX #FT_SFX_CH3
  JSR FamiToneSfxPlay
  restore_regs
:
  JMP next ; but may be colliding with something else
normal_collision:
  LDA #1
  RTS
next:
  DEX
  BPL loop
  LDA #$00
  RTS
.endproc

.proc main_title
  JSR readjoy
  LDA pressed_buttons
  AND #(BUTTON_START | BUTTON_A)
  BEQ :+
  JSR start_game_setup
:

  ; nice title animation
  LDA #$00
  STA sprite_counter

  LDA nmis
  AND #%10000
  BEQ even_frame

odd_frame:

  LDA #$30
  STA temp_x
  LDA #$80
  STA temp_y

  LDA #<metasprite_16_data
  STA addr_ptr
  LDA #>metasprite_16_data
  STA addr_ptr+1

  JSR display_metasprite

  LDA #$B0
  STA temp_x
  LDA #$80
  STA temp_y

  LDA #<metasprite_26_data
  STA addr_ptr
  LDA #>metasprite_26_data
  STA addr_ptr+1

  JSR display_metasprite

  LDA #$38
  STA temp_x
  LDA #$B0
  STA temp_y

  LDA #<text_cursor_sprite
  STA addr_ptr
  LDA #>text_cursor_sprite
  STA addr_ptr+1

  JSR display_metasprite

  RTS

even_frame:
  LDA #$30
  STA temp_x
  LDA #$80
  STA temp_y

  LDA #<metasprite_17_data
  STA addr_ptr
  LDA #>metasprite_17_data
  STA addr_ptr+1

  JSR display_metasprite


  LDA #$B0
  STA temp_x
  LDA #$78
  STA temp_y

  LDA #<metasprite_26_data
  STA addr_ptr
  LDA #>metasprite_26_data
  STA addr_ptr+1

  JSR display_metasprite

  LDA #$40
  STA temp_x
  LDA #$B0
  STA temp_y

  LDA #<text_cursor_sprite
  STA addr_ptr
  LDA #>text_cursor_sprite
  STA addr_ptr+1

  JSR display_metasprite

  RTS
.endproc

.proc open_inventory
  LDA #sfx::Menu_Open
  LDX #FT_SFX_CH1
  JSR FamiToneSfxPlay
  LDA #game_states::main_inventory
  STA game_state
  LDA #$1
  STA current_nametable

  ; hide all sprites (TODO: hide only above window?)
  LDX #$00
  LDA #$F0
:
  STA oam_sprites+Sprite::ycoord, X
  .repeat .sizeof(Sprite)
  INX
  .endrepeat
  BNE :-

  RTS
.endproc

.proc close_inventory
  LDA #sfx::Menu_Close
  LDX #FT_SFX_CH1
  JSR FamiToneSfxPlay
  LDA #game_states::main_playing
  STA game_state
  LDA #$0
  STA current_nametable

  ; hide all sprites
  LDX #$00
  LDA #$F0
:
  STA oam_sprites+Sprite::ycoord, X
  .repeat .sizeof(Sprite)
  INX
  .endrepeat
  BNE :-

  RTS
.endproc

.proc start_selected_game
  LDA inventory_selection
  AND inventory
  BEQ return
  LDA inventory_selection
  CMP #HAS_WK
  BNE :+
  LDX #game_states::wk_booting_gamekid
:
  CMP #HAS_GI
  BNE :+
  LDX #game_states::gi_booting_gamekid
:
  CMP #HAS_MF
  BNE :+
  LDX #game_states::mf_booting_gamekid
:
  CMP #HAS_RR
  BNE :+
  LDX #game_states::rr_booting_gamekid
:
  STX game_state
  LDA #$00
  STA current_nametable
  LDA #$00
  STA frame_counter
  LDA #sfx::Choose_Cartridge
  LDX #FT_SFX_CH2
  JSR FamiToneSfxPlay
  LDA #music_track::Gamekid_Booting
  JSR FamiToneMusicPlay
return:
  RTS
.endproc

.proc main_playing
  ; save player position
  LDA objects+Object::xcoord
  STA old_player_x
  LDA objects+Object::ycoord
  STA old_player_y

  JSR readjoy
  .ifdef DEBUG
  ; press A + B in debug mode to unlock all power ups
  LDA buttons
  AND #(BUTTON_A | BUTTON_B)
  CMP #(BUTTON_A | BUTTON_B)
  BNE :+
  LDA #$FF
  STA inventory
  RTS
:
  .endif
  LDA pressed_buttons
  AND #BUTTON_SELECT
  BEQ :+
  JSR open_inventory
  RTS
:
  LDA pressed_buttons
  AND #BUTTON_A
  BEQ :+
  JSR shoot_fireball
  RTS
:
  LDA pressed_buttons
  AND #BUTTON_B
  BEQ :+
  JSR drop_bomb
  RTS
:
  LDA buttons
  AND #BUTTON_UP
  BEQ :+
  LDA objects+Object::ycoord
  BNE move_up
  LDA #direction::up
  STA next_screen_direction
  JSR load_next_screen
move_up:
  DEC objects+Object::ycoord
  INC objects+Object::sprite_toggle
  LDA #direction::up
  STA objects+Object::direction
  JSR check_wall_collision
  BEQ :+
  INC objects+Object::ycoord ; undo move
:
  LDA buttons
  AND #BUTTON_DOWN
  BEQ :+
  LDA objects+Object::ycoord
  CMP #$E0
  BNE move_down
  LDA #direction::down
  STA next_screen_direction
  JSR load_next_screen
move_down:
  INC objects+Object::ycoord
  INC objects+Object::sprite_toggle
  LDA #direction::down
  STA objects+Object::direction
  JSR check_wall_collision
  BEQ :+
  DEC objects+Object::ycoord ; undo move
:
  LDA buttons
  AND #BUTTON_LEFT
  BEQ :+
  LDA objects+Object::xcoord
  BNE move_left
  LDA #direction::left
  STA next_screen_direction
  JSR load_next_screen
move_left:
  DEC objects+Object::xcoord
  INC objects+Object::sprite_toggle
  LDA #direction::left
  STA objects+Object::direction
  JSR check_wall_collision
  BEQ :+
  INC objects+Object::xcoord ; undo move
:
  LDA buttons
  AND #BUTTON_RIGHT
  BEQ :+
  LDA objects+Object::xcoord
  CMP #$F0
  BNE move_right
  LDA #direction::right
  STA next_screen_direction
  JSR load_next_screen
move_right:
  INC objects+Object::xcoord
  INC objects+Object::sprite_toggle
  LDA #direction::right
  STA objects+Object::direction
  JSR check_wall_collision
  BEQ :+
  DEC objects+Object::xcoord ; undo move
:

  ; update elements
  LDA #$0
  STA boss_index
  LDX num_objects
  DEX
  BEQ skip_update_elements
update_elements_loop:
  LDA objects+Object::type, X
  CMP #object_type::enemy_vrissy
  BEQ @animate_vrissy
  CMP #object_type::cartridge_wk
  BEQ @animate_cartridge
  CMP #object_type::cartridge_gi
  BEQ @animate_cartridge
  CMP #object_type::cartridge_mf
  BEQ @animate_cartridge
  CMP #object_type::cartridge_rr
  BEQ @animate_cartridge
  CMP #object_type::pushable_block
  BEQ @animate_block
  CMP #object_type::glitch_boss
  BEQ @update_boss
  INC objects+Object::sprite_toggle, X
  JMP next
@animate_vrissy:
  JSR update_enemy_vrissy
  JMP next
@animate_cartridge:
  JSR animate_cartridge
  JMP next
@animate_block:
  JSR animate_block
  JMP next
@update_boss:
  STX boss_index
  JSR update_boss
  ; JMP next
next:
  DEX
  BNE update_elements_loop ; X = 0 is player object
skip_update_elements:

  ; draw elements
draw_elements:
  ; maybe update fireball
  LDA fireball_x
  BEQ :+
  JSR update_fireball
:

  ; maybe update bomb
  LDA bomb_x
  BEQ :+
  JSR update_bomb
:

  ; maybe update explosion
  LDA explosion_x
  BEQ :+
  JSR update_explosion
:

  LDA #0
  STA sprite_counter
  LDX num_objects
  DEX

draw_elements_loop:
  ; Save X
  TXA
  PHA

  ; hide object if in inventory
  LDY objects+Object::type, X
  LDA inventory_mask_per_type, Y
  AND inventory
  BNE skip_drawing

  ; get the pointer to anim_data stuff
  LDA anim_data_ptr_l, Y
  STA addr_ptr
  LDA anim_data_ptr_h, Y
  STA addr_ptr+1

  CPY #object_type::glitch_boss
  BNE no_boss

  ; boss has 4 sprites animation for left/right directions
  CLC
  LDA objects+Object::sprite_toggle, X
  AND #%11000
  LSR
  LSR
  TAY
  LDA objects+Object::direction, X
  CMP #direction::left
  BEQ load_sprite
  TYA
  CLC
  ADC #8
  TAY
  JMP load_sprite

no_boss:

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
  LDA swimming
  BEQ load_sprite
  TXA
  BNE load_sprite
  TYA
  CLC
  ADC #$10
  TAY
load_sprite:

  LDA (addr_ptr),Y
  PHA
  INY
  LDA (addr_ptr),Y
  STA addr_ptr+1
  PLA
  STA addr_ptr

  ; check collision before drawing
  CPX #$00
  BEQ skip_collision
  LDA game_state
  CMP #game_states::main_playing
  BNE skip_collision

  JSR handle_object_player_collision
  ; if collision changed stuff, stop drawing
  LDA game_state
  CMP #game_states::main_playing
  BEQ skip_collision
  PLA
  RTS

skip_collision:
  ; end collision check

  LDA objects+Object::xcoord, X
  STA temp_x
  LDA objects+Object::ycoord, X
  STA temp_y
  JSR display_metasprite

skip_drawing:
  ; restore X
  PLA
  TAX

  DEX
  BPL draw_elements_loop

  ; maybe draw fireball
  LDA fireball_x
  BEQ :+
  JSR draw_fireball
:

  ; maybe draw bomb
  LDA bomb_x
  BEQ :+
  JSR draw_bomb
:

  ; maybe draw explosion
  LDA explosion_x
  BEQ :+
  JSR draw_explosion
:

  ; display lives
  LDA #<heart_sprite
  STA addr_ptr
  LDA #>heart_sprite
  STA addr_ptr+1

  LDA lives
  BEQ skip_lives
  STA temp_b
  LDA #$0C
  STA temp_x
lives_loop:
  LDA #$0C
  STA temp_y
  LDA nmis
  AND #%11000
  LSR
  LSR
  LSR
  EOR #%11
  CMP temp_b
  BNE :+
  LDA #$09
  STA temp_y
  :
  JSR display_metasprite
  CLC
  LDA #$0A
  ADC temp_x
  STA temp_x
  DEC temp_b
  BNE lives_loop
skip_lives:

  ; draw boss lives
  LDX boss_index
  BEQ skip_boss_lives
  LDA boss_lives
  BEQ skip_boss_lives
  BMI skip_boss_lives
  STA temp_b

  LDA objects+Object::xcoord, X
  STA temp_x
boss_lives_loop:
  LDA objects+Object::ycoord, X
  SEC
  SBC #$08
  STA temp_y
  LDA nmis
  AND #%11000
  LSR
  LSR
  LSR
  EOR #%11
  CMP temp_b
  BNE :+
  LDA temp_y
  SEC
  SBC #$03
  STA temp_y
  :
  TXA
  PHA
  JSR display_metasprite
  PLA
  TAX
  CLC
  LDA #$0A
  ADC temp_x
  STA temp_x
  DEC temp_b
  BNE boss_lives_loop

skip_boss_lives:

  ; ensure we erase sprites if we lost a metasprite before
  LDX sprite_counter
  LDA #$F0
:
  STA oam_sprites+Sprite::ycoord, X
  .repeat .sizeof(Sprite)
  INX
  .endrepeat
  BNE :-

  LDA boss_lives
  BPL no_victory

  LDA #music_track::Free_Faerie
  JSR FamiToneMusicPlay

  DIALOG string_dialog_victory, title_setup

no_victory:

  RTS
.endproc

.proc shoot_fireball
  LDA inventory
  AND #FINISHED_GI
  BNE :+
  RTS
:
  LDA #sfx::Shoot_Fireball
  LDX #FT_SFX_CH1
  JSR FamiToneSfxPlay

  LDA objects+Object::xcoord
  CLC
  ADC #$08
  STA fireball_x
  LDA objects+Object::ycoord
  CLC
  ADC #$08
  STA fireball_y
  LDA objects+Object::direction
  STA fireball_direction
  RTS
.endproc

.proc drop_bomb
  LDA inventory
  AND #FINISHED_MF
  BNE :+
  RTS
:
  LDA bomb_x
  BEQ :+
  RTS
:
  LDA objects+Object::xcoord
  STA bomb_x
  LDA objects+Object::ycoord
  STA bomb_y
  LDA #180
  STA bomb_countdown
  RTS
.endproc

.proc update_fireball
  LDA fireball_direction
  CMP #direction::up
  BEQ @move_up
  CMP #direction::down
  BEQ @move_down
  CMP #direction::left
  BEQ @move_left
  CMP #direction::right
  BEQ @move_right
  KIL ; should never happen

@move_up:
  DEC fireball_y
  DEC fireball_y
  JMP @collisions

@move_down:
  INC fireball_y
  INC fireball_y
  JMP @collisions

@move_left:
  DEC fireball_x
  DEC fireball_x
  JMP @collisions

@move_right:
  INC fireball_x
  INC fireball_x
  ; JMP @collisions

@collisions:
  ; disappear if out of bounds
  LDA fireball_x
  CMP #$08
  BCC delete_fireball
  LDA fireball_y
  CMP #$08
  BCC delete_fireball

  LDA fireball_x
  CMP #$F8
  BCS delete_fireball
  LDA fireball_y
  CMP #$E8
  BCS delete_fireball

  ; collide with walls
  JSR fireball_wall_collision

  ; collide with enemy
  JSR fireball_enemy_collision
  RTS
delete_fireball:
  LDA #$00
  STA fireball_x
  RTS
.endproc

.proc fireball_wall_collision
  LDX num_walls
  DEX
loop:
  LDA wall_watery, X
  BNE next
  LDA fireball_x
  CMP wall_x1, X
  BCC next
  LDA fireball_y
  CMP wall_y1, X
  BCC next
  LDA fireball_x
  CMP wall_x2, X
  BCS next
  LDA fireball_y
  CMP wall_y2, X
  BCS next
  JMP delete_fireball
next:
  DEX
  BPL loop

  RTS
delete_fireball:
  LDA #sfx::Fireball_Collision
  LDX #FT_SFX_CH2
  JSR FamiToneSfxPlay
  LDA #$00
  STA fireball_x
  RTS
.endproc

.proc fireball_enemy_collision
  LDX num_objects
  BNE :+
  RTS
:
  DEX
@loop:
  LDY objects+Object::type, X
  LDA is_enemy_per_type, Y
  BEQ @next

  CLC
  LDA hitbox_x1, Y
  ADC objects+Object::xcoord, X
  SEC
  SBC #$04
  STA temp_hitbox_b+Box::x1
  CLC
  LDA hitbox_y1, Y
  ADC objects+Object::ycoord, X
  SEC
  SBC #$04
  STA temp_hitbox_b+Box::y1
  CLC
  LDA hitbox_x2, Y
  ADC objects+Object::xcoord, X
  ADC #$04
  STA temp_hitbox_b+Box::x2
  CLC
  LDA hitbox_y2, Y
  ADC objects+Object::ycoord, X
  ADC #$04
  STA temp_hitbox_b+Box::y2

  LDA fireball_x
  CMP temp_hitbox_b+Box::x1
  BCC @next
  CMP temp_hitbox_b+Box::x2
  BCS @next
  LDA fireball_y
  CMP temp_hitbox_b+Box::y1
  BCC @next
  CMP temp_hitbox_b+Box::y2
  BCS @next

  TXA
  PHA
  LDA #sfx::Fireball_Collision
  LDX #FT_SFX_CH2
  JSR FamiToneSfxPlay
  PLA
  TAX

  ; delete fireball and object
  CPX boss_index
  BNE @skip_boss_check
  DEC boss_lives
  BPL @skip_enemy_deleted
@skip_boss_check:
  JSR delete_nth_object
@skip_enemy_deleted:
  JMP delete_fireball
@next:
  DEX
  BPL @loop

  RTS
delete_fireball:
  LDA #$00
  STA fireball_x
  RTS
.endproc

.proc delete_nth_object
  ; assumes X is the object index
  ; cobbles Y
  DEC num_objects
  CPX num_objects
  BEQ :+
  LDY num_objects
  LDA objects+Object::type, Y
  STA objects+Object::type, X
  LDA objects+Object::xcoord, Y
  STA objects+Object::xcoord, X
  LDA objects+Object::ycoord, Y
  STA objects+Object::ycoord, X
  LDA objects+Object::direction, Y
  STA objects+Object::direction, X
  LDA objects+Object::sprite_toggle, Y
  STA objects+Object::sprite_toggle, X
  LDA objects+Object::rom_ptr_l, Y
  STA objects+Object::rom_ptr_l, X
  LDA objects+Object::rom_ptr_h, Y
  STA objects+Object::rom_ptr_h, X
  LDA objects+Object::ram, Y
  STA objects+Object::ram, X
:
  RTS
.endproc

.proc update_bomb
  LDA bomb_countdown
  BEQ kaboom
  DEC bomb_countdown
  LDA #sfx::Ignited_Bomb
  LDX #FT_SFX_CH3
  JSR FamiToneSfxPlay
  RTS
kaboom:
  LDA #sfx::Explosion
  LDX #FT_SFX_CH0
  JSR FamiToneSfxPlay
  ; prepare to draw explosion
  LDA bomb_x
  STA explosion_x
  LDA bomb_y
  STA explosion_y
  LDA #$00
  STA explosion_progress

  ; check if any breakable wal / enemy / player  was caught by explosion

  ; first we make an explosion hitbox
  LDA bomb_x
  CLC
  ADC #$F7
  STA temp_hitbox_a+Box::x1
  LDA bomb_y
  CLC
  ADC #$F7
  STA temp_hitbox_a+Box::y1
  LDA bomb_x
  CLC
  ADC #$18
  STA temp_hitbox_a+Box::x2
  LDA bomb_y
  CLC
  ADC #$18
  STA temp_hitbox_a+Box::y2

  ; now we check for breakable wall hitboxes

  LDX num_objects
  BEQ return

  DEX
@loop:
  LDY objects+Object::type, X

  CLC
  LDA hitbox_x1, Y
  ADC objects+Object::xcoord, X
  SEC
  SBC #$04
  STA temp_hitbox_b+Box::x1
  CLC
  LDA hitbox_y1, Y
  ADC objects+Object::ycoord, X
  SEC
  SBC #$04
  STA temp_hitbox_b+Box::y1
  CLC
  LDA hitbox_x2, Y
  ADC objects+Object::xcoord, X
  ADC #$04
  STA temp_hitbox_b+Box::x2
  CLC
  LDA hitbox_y2, Y
  ADC objects+Object::ycoord, X
  ADC #$04
  STA temp_hitbox_b+Box::y2

  JSR temp_hitbox_collision
  BEQ @next

  ; check if object should be damaged
  CPY #object_type::player
  BNE :+
  JSR damage_player
:
  CPY #object_type::enemy_vrissy
  BNE :+
  JSR delete_nth_object
:
  CPY #object_type::breakable_wall
  BNE :+
  JSR delete_nth_object
:
  CPY #object_type::glitch_boss
  BNE :+
  DEC boss_lives
  BPL :+
  JSR delete_nth_object
:
@next:
  DEX
  BPL @loop

  ; end check
return:
  LDA #$00
  STA bomb_x
  RTS
.endproc

.proc update_explosion
  INC explosion_progress
  LDA explosion_progress
  CMP #%1011
  BCC :+
  LDA #$00
  STA explosion_x
:
  RTS
.endproc

.proc draw_fireball
  ; LDA fireball_x ; XXX done by caller
  STA temp_x
  LDA fireball_y
  STA temp_y

  LDA nmis
  AND #%1100
  LSR
  LSR
  TAX

  LDA fireball_sprites_l, X
  STA addr_ptr
  LDA fireball_sprites_h, X
  STA addr_ptr+1

  JSR display_metasprite
  RTS
.endproc

.proc draw_bomb
  ; LDA bomb_x ; XXX done by caller
  STA temp_x
  LDA bomb_y
  STA temp_y
  LDA #<bomb_sprite
  STA addr_ptr
  LDA #>bomb_sprite
  STA addr_ptr+1

  JSR display_metasprite
  RTS
.endproc

.proc draw_explosion
  ; LDA explosion_x ; XXX done by caller
  STA temp_x
  LDA explosion_y
  STA temp_y
  LDA explosion_progress
  LSR
  LSR
  TAX
  LDA explosion_sprites_l, X
  STA addr_ptr
  LDA explosion_sprites_h, X
  STA addr_ptr+1

  JSR display_metasprite
  RTS
.endproc

.proc dialog_to_playing
  LDA #game_states::main_playing
  STA game_state
  RTS
.endproc

.proc begin_display_dialog
  LDX #$1
  STX dialog_current_row
  LDA window_ppu_addrs_l, X
  CLC
  ADC #$02
  STA dialog_ppu_ptr
  LDA window_ppu_addrs_h, X
  STA dialog_ppu_ptr+1
  LDA #$00
  STA frame_counter
  LDA #$01
  STA current_nametable
  LDA #game_states::main_dialog
  STA game_state

  ; erase sprites
  LDX #$00
  LDA #$F0
:
  STA oam_sprites+Sprite::ycoord, X
  .repeat .sizeof(Sprite)
  INX
  .endrepeat
  BNE :-

  RTS
.endproc

.proc end_display_dialog
  LDA #$00
  STA current_nametable
  LDA dialog_callback+1
  PHA
  LDA dialog_callback
  PHA
  RTS
.endproc

DIALOG_DELAY=$02
DIALOG_LAST_ROW=$07
.proc main_dialog
  LDA dialog_current_row
  CMP #DIALOG_LAST_ROW
  BEQ dialog_interaction
  LDA dialog_current_row
  BMI erasing_rows
  LDY #0
  LDA (dialog_string_ptr), Y
  BEQ dialog_interaction

  INC frame_counter
  LDA frame_counter
  CMP #DIALOG_DELAY
  BEQ :+
  RTS
:
  LDA #$00
  STA frame_counter
  LDA (dialog_string_ptr), Y
  CMP #$0A
  BEQ linebreak
  ; display single char (TODO - maybe SFX)
  LDA PPUSTATUS
  LDA dialog_ppu_ptr+1
  STA PPUADDR
  LDA dialog_ppu_ptr
  STA PPUADDR
  LDA (dialog_string_ptr), Y
  STA PPUDATA
  LDA #$24
  STA PPUADDR
  LDA #$00
  STA PPUADDR
  STA PPUSCROLL
  STA PPUSCROLL
  LDA #sfx::Typing_Text
  LDX #FT_SFX_CH3
  JSR FamiToneSfxPlay

increment_pointers:
  INC dialog_string_ptr
  BNE increment_ppu
  INC dialog_string_ptr+1
increment_ppu:
  INC dialog_ppu_ptr
  BNE :+
  INC dialog_ppu_ptr+1
:
  RTS
linebreak:
  INC dialog_current_row
  LDX dialog_current_row
  LDA window_ppu_addrs_l, X
  CLC
  ADC #$01
  STA dialog_ppu_ptr
  LDA window_ppu_addrs_h, X
  STA dialog_ppu_ptr+1
  JSR increment_pointers
  JMP main_dialog
dialog_interaction:
  JSR readjoy
  LDA pressed_buttons
  BEQ no_buttons
  JMP continue_dialog
  RTS
erasing_rows:
  LDA dialog_current_row
  EOR #$80
  TAX
  LDA PPUSTATUS
  LDA window_ppu_addrs_h, X
  STA PPUADDR
  LDA window_ppu_addrs_l, X
  CLC
  ADC #$02
  STA PPUADDR
  LDX #$14
  LDA #$60
@erase_loop:
  STA PPUDATA
  DEX
  BPL @erase_loop
  LDA #$24
  STA PPUADDR
  LDA #$00
  STA PPUADDR
  STA PPUSCROLL
  STA PPUSCROLL
  INC dialog_current_row
  LDA dialog_current_row
  CMP #$80 + DIALOG_LAST_ROW
  BEQ @erased_last_row
  RTS
@erased_last_row:
  LDY #0
  LDA (dialog_string_ptr), Y
  BNE @continue_drawing

  JSR end_display_dialog

@continue_drawing:
  LDA #$00
  STA dialog_current_row
  JMP linebreak

continue_dialog:
  LDA #$81
  STA dialog_current_row
  LDA #sfx::Read_More
  LDX #FT_SFX_CH2
  JSR FamiToneSfxPlay
  RTS
no_buttons:
  LDA #$00
  STA sprite_counter
  LDA nmis
  AND #%10000
  LSR
  LSR
  CLC
  ADC #$CE
  STA temp_x
  LDA #$D0
  STA temp_y
  LDA #<text_cursor_sprite
  STA addr_ptr
  LDA #>text_cursor_sprite
  STA addr_ptr+1
  JSR display_metasprite
  RTS
.endproc

.proc main_inventory
  ; player input
  JSR readjoy

  LDA pressed_buttons
  AND #(BUTTON_SELECT | BUTTON_START | BUTTON_B)
  BEQ :+
  JSR close_inventory
  RTS
:
  LDA pressed_buttons
  AND #BUTTON_A
  BEQ :+
  JSR start_selected_game
  RTS
:
  LDA pressed_buttons
  AND #(BUTTON_UP | BUTTON_LEFT)
  BEQ :+
  LDA #sfx::Select_Cartridge
  LDX #FT_SFX_CH2
  JSR FamiToneSfxPlay
  LSR inventory_selection
  BCC :+
  LDA #%1000
  STA inventory_selection
:
  LDA pressed_buttons
  AND #(BUTTON_DOWN | BUTTON_RIGHT)
  BEQ :+
  LDA #sfx::Select_Cartridge
  LDX #FT_SFX_CH2
  JSR FamiToneSfxPlay
  ASL inventory_selection
  LDA inventory_selection
  CMP #%10000
  BNE :+
  LDA #%0001
  STA inventory_selection
:

  LDA #$00
  STA sprite_counter
  ; draw elements
  LDY #object_type::cartridge_wk
loop:
  TYA
  PHA

  ASL
  ASL
  ASL
  ASL
  PHA
  ASL

  CLC
  ADC #$08
  STA temp_x

  PLA
  AND #%10000
  CLC
  ADC #$B0
  STA temp_y

  LDA inventory_mask_per_type, Y
  AND inventory
  BEQ nextish

  LDA anim_data_ptr_l, Y
  STA addr_ptr
  LDA anim_data_ptr_h, Y
  STA addr_ptr+1

  LDY #$00
  LDA (addr_ptr),Y
  PHA
  INY
  LDA (addr_ptr),Y
  STA addr_ptr+1
  PLA
  STA addr_ptr

  JSR display_metasprite

  PLA
  TAY
  PHA

nextish:

  LDA inventory_mask_per_type, Y
  AND inventory_selection
  BEQ skip_cursor

  LDA temp_x
  CLC
  ADC #$4
  STA temp_x

  LDA #$C0
  CMP temp_y
  BEQ cursor_down
cursor_up:
  LDA #<cursor_up_sprite
  STA addr_ptr
  LDA #>cursor_up_sprite
  STA addr_ptr+1
  LDA #$C8
  STA temp_y
  JMP draw_cursor
cursor_down:
  LDA #<cursor_down_sprite
  STA addr_ptr
  LDA #>cursor_down_sprite
  STA addr_ptr+1
  LDA #$B0
  STA temp_y
draw_cursor:
  JSR display_metasprite

skip_cursor:

  PLA
  TAY

  INY
  CPY #(object_type::cartridge_rr+1)
  BNE loop

  RTS
.endproc

DYING_FRAMES = 60
.proc main_dying
  LDA frame_counter
  CMP #DYING_FRAMES
  BEQ stop_blinking
  INC frame_counter
  LDA frame_counter
  AND #%1000
  BNE show_player
hide_player:
  LDA objects+Object::ycoord
  CMP #$F0
  BEQ update_sprite
  STA old_player_y
  LDA #$F0
  STA objects+Object::ycoord
  JMP update_sprite
show_player:
  LDA old_player_y
  STA objects+Object::ycoord
update_sprite:
  ; XXX hijack main_playing drawing code
  JSR main_playing::draw_elements
  RTS
stop_blinking:
  LDA lives
  BEQ game_over
  LDA entrance_player_x
  STA objects+Object::xcoord
  LDA entrance_player_y
  STA objects+Object::ycoord
  LDA #game_states::main_playing
  STA game_state
  JSR load_screen
  RTS
game_over:
  LDA #music_track::Gamekid_Defeat
  JSR FamiToneMusicPlay
  DIALOG string_dialog_game_over, title_setup
  RTS
.endproc

.proc handle_object_player_collision
  ; input: X = index of object
  ; cobbles Y
  JSR prepare_player_hitbox

  LDY objects+Object::type, X

  CLC
  LDA hitbox_x1, Y
  ADC objects+Object::xcoord, X
  STA temp_hitbox_b+Box::x1
  CLC
  LDA hitbox_y1, Y
  ADC objects+Object::ycoord, X
  STA temp_hitbox_b+Box::y1
  CLC
  LDA hitbox_x2, Y
  ADC objects+Object::xcoord, X
  STA temp_hitbox_b+Box::x2
  CLC
  LDA hitbox_y2, Y
  ADC objects+Object::ycoord, X
  STA temp_hitbox_b+Box::y2

  JSR temp_hitbox_collision
  BNE collided
  RTS
collided:
  CPY #object_type::enemy_vrissy
  BEQ @enemy
  CPY #object_type::cartridge_wk
  BEQ @cartridge_wk
  CPY #object_type::cartridge_gi
  BEQ @cartridge_gi
  CPY #object_type::cartridge_mf
  BEQ @cartridge_mf
  CPY #object_type::cartridge_rr
  BEQ @cartridge_rr
  CPY #object_type::pushable_block
  BEQ @pushable_block
  CPY #object_type::breakable_wall
  BEQ @breakable_wall
  CPY #object_type::glitch_boss
  BEQ @enemy
  CPY #object_type::faerie
  BEQ @exposition
  KIL ; not yet implemented
@enemy:
  JSR damage_player
  RTS
@pushable_block:
  JSR push_block
  RTS
@breakable_wall:
  LDA old_player_x
  STA objects+Object::xcoord
  LDA old_player_y
  STA objects+Object::ycoord
  RTS
@exposition:
  JSR faerie_exposition
  RTS
@cartridge_wk:
  LDA inventory
  ORA #HAS_WK
  STA inventory
  DIALOG string_dialog_wk_cartridge
  JMP @cartridge_sfx
@cartridge_gi:
  LDA inventory
  ORA #HAS_GI
  STA inventory
  DIALOG string_dialog_gi_cartridge
  JMP @cartridge_sfx
@cartridge_mf:
  LDA inventory
  ORA #HAS_MF
  STA inventory
  DIALOG string_dialog_mf_cartridge
  JMP @cartridge_sfx
@cartridge_rr:
  LDA inventory
  ORA #HAS_RR
  STA inventory
  DIALOG string_dialog_rr_cartridge
  JMP @cartridge_sfx
@cartridge_sfx:
  LDA #sfx::Cartridge_Get
  LDX #FT_SFX_CH2
  JSR FamiToneSfxPlay
  RTS
.endproc

.proc faerie_exposition
  LDA faerie_checklist
  AND #MAIN_EXPLANATION
  BNE :+
  LDA #MAIN_EXPLANATION
  ORA faerie_checklist
  STA faerie_checklist
  DIALOG string_dialog_main_explanation
  RTS
:
  LDA faerie_checklist
  AND #HAS_WK
  BNE :+
  LDA inventory
  AND #(FINISHED_WK | HAS_WK)
  CMP #HAS_WK
  BNE :+
  LDA #HAS_WK
  ORA faerie_checklist
  STA faerie_checklist
  DIALOG string_dialog_explain_wk
  RTS
:
  LDA faerie_checklist
  AND #HAS_GI
  BNE :+
  LDA inventory
  AND #(FINISHED_GI | HAS_GI)
  CMP #HAS_GI
  BNE :+
  LDA #HAS_GI
  ORA faerie_checklist
  STA faerie_checklist
  DIALOG string_dialog_explain_gi
  RTS
:
  LDA faerie_checklist
  AND #HAS_MF
  BNE :+
  LDA inventory
  AND #(FINISHED_MF | HAS_MF)
  CMP #HAS_MF
  BNE :+
  LDA #HAS_MF
  ORA faerie_checklist
  STA faerie_checklist
  DIALOG string_dialog_explain_mf
  RTS
:
  LDA faerie_checklist
  AND #HAS_RR
  BNE :+
  LDA inventory
  AND #(FINISHED_RR | HAS_RR)
  CMP #HAS_RR
  BNE :+
  LDA #HAS_RR
  ORA faerie_checklist
  STA faerie_checklist
  DIALOG string_dialog_explain_rr
  RTS
:
  LDA faerie_checklist
  AND #FINAL_QUEST_EXPLANATION
  BNE :+
  LDA inventory
  AND #(FINISHED_WK | FINISHED_GI | FINISHED_MF | FINISHED_RR)
  CMP #(FINISHED_WK | FINISHED_GI | FINISHED_MF | FINISHED_RR)
  BNE :+
  LDA #FINAL_QUEST_EXPLANATION
  ORA faerie_checklist
  STA faerie_checklist
  DIALOG string_dialog_explain_quest
  RTS
:
  RTS
.endproc

.proc push_block
  LDA inventory
  AND #FINISHED_WK
  BEQ collide

  LDA objects+Object::ram, X
  BNE collide

  LDA objects+Object::direction, X
  CMP objects+Object::direction

  BNE collide

  INC objects+Object::ram, X

  LDA objects+Object::direction, X
  CMP #direction::up
  BEQ fix_x
  CMP #direction::down
  BEQ fix_x

fix_y:
  LDA objects+Object::ycoord, X
  STA old_player_y
  JMP collide

fix_x:
  LDA objects+Object::xcoord, X
  STA old_player_x
  ;JMP collide

collide:
  LDA old_player_x
  STA objects+Object::xcoord
  LDA old_player_y
  STA objects+Object::ycoord
  RTS
.endproc

.proc damage_player
  TXA
  PHA
  DEC lives
  LDA #sfx::Damage
  LDX #FT_SFX_CH3
  JSR FamiToneSfxPlay
  LDA #$00
  STA frame_counter
  LDA #game_states::main_dying
  STA game_state
  PLA
  TAX
  RTS
.endproc

.proc animate_cartridge
  INC objects+Object::sprite_toggle, X
  LDA objects+Object::sprite_toggle, X
  AND #%1111
  BNE :+
  LDA objects+Object::sprite_toggle, X
  AND #%10000
  BEQ up
  LDA objects+Object::ycoord, X
  CLC
  ADC #4
  STA objects+Object::ycoord, X
  JMP :+
up:
  LDA objects+Object::ycoord, X
  SEC
  SBC #4
  STA objects+Object::ycoord, X
:
  RTS
.endproc

.proc update_enemy_vrissy
  INC objects+Object::sprite_toggle, X
  ; load rom ptr and ram index
  LDY objects+Object::ram, X
  LDA objects+Object::rom_ptr_l, X
  STA addr_ptr
  LDA objects+Object::rom_ptr_h, X
  STA addr_ptr+1

  ; move
  LDA objects+Object::direction, X
  CMP #direction::up
  BEQ move_up
  CMP #direction::down
  BEQ move_down
  CMP #direction::left
  BEQ move_left
  CMP #direction::right
  BEQ move_right
  KIL
move_up:
  DEC objects+Object::ycoord, X
  LDA (addr_ptr),Y
  CMP objects+Object::ycoord, X
  BEQ update_direction
  JMP return
move_down:
  INC objects+Object::ycoord, X
  LDA (addr_ptr),Y
  CMP objects+Object::ycoord, X
  BEQ update_direction
  JMP return
move_left:
  DEC objects+Object::xcoord, X
  LDA (addr_ptr),Y
  CMP objects+Object::xcoord, X
  BEQ update_direction
  JMP return
move_right:
  INC objects+Object::xcoord, X
  LDA (addr_ptr),Y
  CMP objects+Object::xcoord, X
  BEQ update_direction
  JMP return
update_direction:
  INY
  LDA (addr_ptr),Y
  STA objects+Object::direction, X
  INY
  LDA (addr_ptr),Y
  BNE :+
  LDY #$00
:
  STY objects+Object::ram, X
return:
  RTS
.endproc

.proc rand_boss_speed
  JSR rand
  LDA rng_seed
  AND #%11
  STA boss_h_speed
  LDA #%11
  SEC
  SBC boss_h_speed
  STA boss_v_speed
  RTS
.endproc

.proc update_boss
  INC objects+Object::sprite_toggle, X
  LDA boss_horizontal
  CMP #direction::left
  BEQ @move_left
@move_right:
  LDA objects+Object::xcoord, X
  CLC
  ADC boss_h_speed
  STA objects+Object::xcoord, X
  CMP #$D0
  BCC @vertical_movement
  LDA #direction::left
  STA boss_horizontal
  JSR rand_boss_speed
  JMP @vertical_movement
@move_left:
  LDA objects+Object::xcoord, X
  SEC
  SBC boss_h_speed
  STA objects+Object::xcoord, X
  CMP #$18
  BCS @vertical_movement
  LDA #direction::right
  STA boss_horizontal
  JSR rand_boss_speed

  ; JMP @vertical_movement

@vertical_movement:
  LDA boss_vertical
  CMP #direction::up
  BEQ @move_up
@move_down:
  LDA objects+Object::ycoord, X
  CLC
  ADC boss_v_speed
  STA objects+Object::ycoord, X
  CMP #$C0
  BCC @return
  LDA #direction::up
  STA boss_vertical
  JSR rand_boss_speed
  JMP @return
@move_up:
  LDA objects+Object::ycoord, X
  SEC
  SBC boss_v_speed
  STA objects+Object::ycoord, X
  CMP #$18
  BCS @return
  LDA #direction::down
  STA boss_vertical
  JSR rand_boss_speed
  ; JMP @return

@return:
  LDA boss_horizontal
  STA objects+Object::direction, X
  RTS
.endproc

.proc animate_block
  LDA objects+Object::ram, X
  BNE moving
  RTS
moving:
  LDA nmis
  AND #%111
  BNE :+
  save_regs
  LDA #sfx::Pushed_Block
  LDX #FT_SFX_CH2
  JSR FamiToneSfxPlay
  restore_regs
:
  LDA objects+Object::direction, X
  CMP #direction::up
  BEQ move_up
  CMP #direction::down
  BEQ move_down
  CMP #direction::left
  BEQ move_left
  CMP #direction::right
  BEQ move_right
  KIL ; never happens (?)

move_up:
  LDY #$00
  LDA objects+Object::rom_ptr_l, X
  STA addr_ptr
  LDA objects+Object::rom_ptr_h, X
  STA addr_ptr+1
  LDA (addr_ptr), Y ; target y position
  CMP objects+Object::ycoord, X
  BEQ stop_up
  DEC objects+Object::ycoord, X
  RTS
stop_up:
  LDA #$00
  STA objects+Object::ram, X
  LDA #direction::down
  STA objects+Object::direction, X
  RTS

move_down:
  LDY #$01
  LDA objects+Object::rom_ptr_l, X
  STA addr_ptr
  LDA objects+Object::rom_ptr_h, X
  STA addr_ptr+1
  LDA (addr_ptr), Y ; target y position
  CMP objects+Object::ycoord, X
  BEQ stop_down
  INC objects+Object::ycoord, X
  RTS
stop_down:
  LDA #$00
  STA objects+Object::ram, X
  LDA #direction::up
  STA objects+Object::direction, X
  RTS

move_left:
  LDY #$00
  LDA objects+Object::rom_ptr_l, X
  STA addr_ptr
  LDA objects+Object::rom_ptr_h, X
  STA addr_ptr+1
  LDA (addr_ptr), Y ; target x position
  CMP objects+Object::xcoord, X
  BEQ stop_left
  DEC objects+Object::xcoord, X
  RTS
stop_left:
  LDA #$00
  STA objects+Object::ram, X
  LDA #direction::right
  STA objects+Object::direction, X
  RTS

move_right:
  LDY #$01
  LDA objects+Object::rom_ptr_l, X
  STA addr_ptr
  LDA objects+Object::rom_ptr_h, X
  STA addr_ptr+1
  LDA (addr_ptr), Y ; target x position
  CMP objects+Object::xcoord, X
  BEQ stop_right
  INC objects+Object::xcoord, X
  RTS
stop_right:
  LDA #$00
  STA objects+Object::ram, X
  LDA #direction::left
  STA objects+Object::direction, X
  RTS

.endproc

.proc temp_hitbox_collision
  ; returns 1 in A if temp_hitbox_a and temp_hitbox_b intersect
  ; Pseudo-code:
  ;  ((a.x1,a.y1),(a.x2,a.y2)) and ((b.x1,b.y1),(b.x2,b.y2))
  ;
  ;  if (a.x2<b.x1 or b.x2<a.x1 or a.y2<b.y1 or b.y2<a.y1):
  ;      don't intersect
  ;  else
  ;      intersect
  ;
  ;  Also: foo < bar ==> foo; CMP bar; carry is clear
  CLC
  LDA temp_hitbox_a+Box::x2
  CMP temp_hitbox_b+Box::x1
  BCS :+
  LDA #$00
  RTS
:
  CLC
  LDA temp_hitbox_b+Box::x2
  CMP temp_hitbox_a+Box::x1
  BCS :+
  LDA #$00
  RTS
:

  CLC
  LDA temp_hitbox_a+Box::y2
  CMP temp_hitbox_b+Box::y1
  BCS :+
  LDA #$00
  RTS
:

  CLC
  LDA temp_hitbox_b+Box::y2
  CMP temp_hitbox_a+Box::y1
  BCS :+
  LDA #$00
  RTS
:
  LDA #$01
  RTS
.endproc

.proc quit_gamekid
  LDA current_screen_soundtrack
  JSR FamiToneMusicPlay

  LDA game_state
  PHA

  LDA #$00
  STA current_nametable
  LDA #game_states::main_playing
  STA game_state

  JSR load_screen

  PLA
  CMP #game_states::wk_win
  BNE :+
  LDA inventory
  AND #FINISHED_WK
  BNE :+
  LDA inventory
  ORA #FINISHED_WK
  STA inventory
  DIALOG string_dialog_wk_win
  RTS
:
  CMP #game_states::gi_win
  BNE :+
  LDA inventory
  AND #FINISHED_GI
  BNE :+
  LDA inventory
  ORA #FINISHED_GI
  STA inventory
  DIALOG string_dialog_gi_win
  RTS
:
  CMP #game_states::mf_win
  BNE :+
  LDA inventory
  AND #FINISHED_MF
  BNE :+
  LDA inventory
  ORA #FINISHED_MF
  STA inventory
  DIALOG string_dialog_mf_win
  RTS
:
  CMP #game_states::rr_win
  BNE :+
  LDA inventory
  AND #FINISHED_RR
  BNE :+
  LDA inventory
  ORA #FINISHED_RR
  STA inventory
  DIALOG string_dialog_rr_win
:
  RTS
.endproc

.proc check_quitting_gamekid
  LDA pressed_buttons
  AND #(BUTTON_SELECT | BUTTON_START)
  BEQ :+
  JSR quit_gamekid
:
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
  LDA #music_track::Sokowhat
  JSR FamiToneMusicPlay

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
  JSR check_quitting_gamekid
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
  LDA #music_track::Gamekid_Victory
  JSR FamiToneMusicPlay
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
  LDA frame_counter
  BNE wait_to_return
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
  STA PPUSCROLL
  STA PPUSCROLL
wait_to_return:
  INC frame_counter
  LDA #GAMEKID_DELAY
  CMP frame_counter
  BNE :+
  JSR quit_gamekid
:
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
  LDA #music_track::Invasion
  JSR FamiToneMusicPlay

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

  LDA #music_track::Gamekid_Defeat
  JSR FamiToneMusicPlay

  LDA #$00
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
  JSR check_quitting_gamekid
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
  LDA #sfx::Shoot_Fireball
  LDX #FT_SFX_CH1
  JSR FamiToneSfxPlay
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

  LDA #$00
  STA frame_counter
  LDA #game_states::gi_win
  STA game_state
  LDA #music_track::Gamekid_Victory
  JSR FamiToneMusicPlay
:
  RTS
.endproc

.proc gi_win
  LDA frame_counter
  BNE wait_to_return
  LDA #$21
  STA ppu_addr_ptr+1
  LDA #$AC
  STA ppu_addr_ptr
  print string_you_win
  LDA #$20
  STA PPUADDR
  LDA #$00
  STA PPUADDR
  STA PPUSCROLL
  STA PPUSCROLL
wait_to_return:
  INC frame_counter
  LDA #GAMEKID_DELAY
  CMP frame_counter
  BNE :+
  JSR quit_gamekid
:
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
  LDA #music_track::Sweeping_Mines
  JSR FamiToneMusicPlay

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
  STA gamekid_ram+mf_var::opened_cells
  LDX #$63
@erase_loop:
  STA gamekid_ram+mf_var::table,X
  STA gamekid_ram+mf_var::bomb_table,X
  STA gamekid_ram+mf_var::status,X
  DEX
  BPL @erase_loop

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
  STA PPUSCROLL
  STA PPUSCROLL

  LDA #music_track::Gamekid_Defeat
  JSR FamiToneMusicPlay
  LDA #sfx::Explosion
  LDX #FT_SFX_CH1
  JSR FamiToneSfxPlay

  LDA #$00
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

  NC_VBLANK

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
  JSR check_quitting_gamekid
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
  LDA #$00
  STA frame_counter
  LDA #game_states::mf_win
  STA game_state
  LDA #music_track::Gamekid_Victory
  JSR FamiToneMusicPlay
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
  LDA frame_counter
  BNE wait_to_return
  NC_VBLANK
  LDA #$20
  STA ppu_addr_ptr+1
  LDA #$CC
  STA ppu_addr_ptr
  print string_you_win
  LDA #$20
  STA PPUADDR
  LDA #$00
  STA PPUADDR
  STA PPUSCROLL
  STA PPUSCROLL
wait_to_return:
  INC frame_counter
  LDA #GAMEKID_DELAY
  CMP frame_counter
  BNE :+
  JSR quit_gamekid
:
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
  LDA #music_track::Log_a_rhythm
  JSR FamiToneMusicPlay

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

  LDA #music_track::Gamekid_Defeat
  JSR FamiToneMusicPlay

  LDA #$00
  STA frame_counter
return:
  RTS
.endproc

.proc rr_playing
  JSR readjoy
  JSR check_quitting_gamekid
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
  LDA #$00
  STA frame_counter
  LDA #game_states::rr_win
  STA game_state
  LDA #music_track::Gamekid_Victory
  JSR FamiToneMusicPlay
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
  LDA frame_counter
  BNE wait_to_return
  LDA #$21
  STA ppu_addr_ptr+1
  LDA #$8C
  STA ppu_addr_ptr
  print string_you_win
  LDA #$20
  STA PPUADDR
  LDA #$00
  STA PPUADDR
  STA PPUSCROLL
  STA PPUSCROLL
wait_to_return:
  INC frame_counter
  LDA #GAMEKID_DELAY
  CMP frame_counter
  BNE :+
  JSR quit_gamekid
:
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
  .byte <(main_title-1)
  .byte <(main_playing-1)
  .byte <(main_dialog-1)
  .byte <(main_inventory-1)
  .byte <(main_dying-1)
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
  .byte >(main_title-1)
  .byte >(main_playing-1)
  .byte >(main_dialog-1)
  .byte >(main_inventory-1)
  .byte >(main_dying-1)
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

cursor_up_sprite = metasprite_30_data
cursor_down_sprite = metasprite_31_data

heart_sprite = metasprite_32_data

text_cursor_sprite = metasprite_33_data

fireball_sprite_1 = metasprite_34_data
fireball_sprite_2 = metasprite_35_data
fireball_sprite_3 = metasprite_36_data
fireball_sprite_4 = metasprite_37_data

bomb_sprite = metasprite_40_data

; data fitting AnimData struct
player_anim_data:
        .word metasprite_10_data, metasprite_11_data ; walking up
        .word metasprite_12_data, metasprite_13_data ; walking down
        .word metasprite_14_data, metasprite_15_data ; walking left
        .word metasprite_16_data, metasprite_17_data ; walking right

        .word metasprite_41_data, metasprite_41_data ; swimming up
        .word metasprite_42_data, metasprite_42_data ; swimming down
        .word metasprite_43_data, metasprite_43_data ; swimming left
        .word metasprite_44_data, metasprite_44_data ; swimming right

enemy_vrissy_anim_data:
        .word metasprite_18_data, metasprite_19_data ; walking up
        .word metasprite_20_data, metasprite_21_data ; walking down
        .word metasprite_22_data, metasprite_23_data ; walking left
        .word metasprite_24_data, metasprite_25_data ; walking right

cartridge_wk_anim_data:
        .word metasprite_26_data, metasprite_26_data ; neutral
cartridge_gi_anim_data:
        .word metasprite_27_data, metasprite_27_data ; neutral
cartridge_mf_anim_data:
        .word metasprite_28_data, metasprite_28_data ; neutral
cartridge_rr_anim_data:
        .word metasprite_29_data, metasprite_29_data ; neutral

pushable_block_anim_data:
        .word metasprite_38_data, metasprite_38_data ; moving up
        .word metasprite_38_data, metasprite_38_data ; moving down
        .word metasprite_38_data, metasprite_38_data ; moving left
        .word metasprite_38_data, metasprite_38_data ; moving right

breakable_wall_anim_data:
        .word metasprite_39_data, metasprite_39_data ; neutral

glitch_boss_anim_data:
        .word metasprite_45_data, metasprite_46_data, metasprite_47_data, metasprite_48_data ; moving left
        .word metasprite_49_data, metasprite_50_data, metasprite_51_data, metasprite_52_data ; moving right

faerie_anim_data:
        .word metasprite_53_data, metasprite_54_data ; neutral

; note: fireballs and bombs aren't really objects
fireball_sprites_l:
        .byte <fireball_sprite_1, <fireball_sprite_2, <fireball_sprite_3, <fireball_sprite_4
fireball_sprites_h:
        .byte >fireball_sprite_1, >fireball_sprite_2, >fireball_sprite_3, >fireball_sprite_4

explosion_sprite_1 = metasprite_55_data
explosion_sprite_2 = metasprite_56_data
explosion_sprite_3 = metasprite_57_data
explosion_sprites_l:
        .byte <explosion_sprite_1, <explosion_sprite_2, <explosion_sprite_3
explosion_sprites_h:
        .byte >explosion_sprite_1, >explosion_sprite_2, >explosion_sprite_3

; indexed by object type
anim_data_ptr_l:
        .byte <player_anim_data
        .byte <enemy_vrissy_anim_data
        .byte <cartridge_wk_anim_data
        .byte <cartridge_gi_anim_data
        .byte <cartridge_mf_anim_data
        .byte <cartridge_rr_anim_data
        .byte <pushable_block_anim_data
        .byte <breakable_wall_anim_data
        .byte <glitch_boss_anim_data
        .byte <faerie_anim_data
anim_data_ptr_h:
        .byte >player_anim_data
        .byte >enemy_vrissy_anim_data
        .byte >cartridge_wk_anim_data
        .byte >cartridge_gi_anim_data
        .byte >cartridge_mf_anim_data
        .byte >cartridge_rr_anim_data
        .byte >pushable_block_anim_data
        .byte >breakable_wall_anim_data
        .byte >glitch_boss_anim_data
        .byte >faerie_anim_data

; indexed by object type
;              pl,  vr,  cartridges        , blk, bw, gb, fae
hitbox_x1:
        .byte $03, $02, $00, $00, $00, $00, $00, $00, $07, $00
hitbox_y1:
        .byte $00, $02, $00, $00, $00, $00, $00, $00, $04, $00
hitbox_x2:
        .byte $0C, $0D, $0F, $0F, $0F, $0F, $0F, $0F, $11, $0F
hitbox_y2:
        .byte $0F, $0D, $0F, $0F, $0F, $0F, $0F, $0F, $16, $0F

inventory_mask_per_type:
        .byte $00 ; player
        .byte $00 ; vrissy
inventory_mask_per_index:
        .byte HAS_WK, HAS_GI, HAS_MF, HAS_RR
        .byte $00, $00 ; pushable block, breakable wall
        .byte $00 ; glitch boss
        .byte $00 ; faerie

is_enemy_per_type:
        .byte $00 ; player
        .byte $01 ; vrissy
        .byte $00, $00, $00, $00 ; cartridges
        .byte $00 ; pushable block
        .byte $00 ; breakable wall
        .byte $01 ; glitch boss
        .byte $00 ; faerie

window_ppu_addrs_l:
        .byte $84
        .byte $A4
        .byte $C4
        .byte $E4
        .byte $04
        .byte $24
        .byte $44
        .byte $64
window_ppu_addrs_h:
        .byte $26
        .byte $26
        .byte $26
        .byte $26
        .byte $27
        .byte $27
        .byte $27
        .byte $27
window_ppu_left_tile:
        .byte $84
        .byte $94
        .byte $94
        .byte $94
        .byte $94
        .byte $94
        .byte $94
        .byte $A4
window_ppu_center_tile:
        .byte $85
        .byte $5B
        .byte $5B
        .byte $5B
        .byte $5B
        .byte $5B
        .byte $5B
        .byte $A5
window_ppu_right_tile:
        .byte $86
        .byte $96
        .byte $96
        .byte $96
        .byte $96
        .byte $96
        .byte $96
        .byte $A6
window_ppu_palette_ptr_l:
        .byte $E9, $EA, $EB, $EC, $ED, $EE
        .byte $F1, $F2, $F3, $F4, $F5, $F6
strings:
.ifdef PTBR
string_game_over: .byte "_DERROTA_", $00
string_lives: .byte "VIDAS_", WRITE_X_SYMBOL, $00
string_you_win: .byte "VITORIA", $00
string_dialog_main_explanation: .byte "OI",$0A
                                .byte "VEJO_QUE_TENS_UM", $0A
                                .byte "NOTENDO_GAMEKID", $0A
                                .byte $0A, $0A, $0A, $0A
                                .byte "A_PROFECIA_DIZ_QUE", $0A
                                .byte "TU_NOS_SALVARA_DO", $0A
                                .byte "MONSTRO_CHAMADO", $0A
                                .byte "GLITCH", $0A
                                .byte $0A, $0A, $0A
                                .byte "PARA_ISSO_DEVES_ACHAR", $0A
                                .byte "OS_QUATRO_CARTUCHOS", $0A
                                .byte "LENDARIOS", $0A
                                .byte $0A, $0A, $0A, $0A
                                .byte "JOGUE_CADA_UM_DELES", $0A
                                .byte "EM_TEU_GAMEKID_PARA", $0A
                                .byte "GANHAR_PODERES", $0A
                                .byte $0A, $0A, $0A, $0A
                                .byte "SE_ENCONTRARES_ALGUM", $0A
                                .byte "DESTES_CARTUCHOS_PODE", $0A
                                .byte "FALAR_COM_UMA_FADA", $0A
                                .byte "COMO_EU_PARA_QUE", $0A
                                .byte "EXPLIQUEMOS_A_TI", $0A
                                .byte "COMO_JOGAR", $0A
                                .byte $0A
                                .byte "APERTAR_SELECT_ABRE", $0A
                                .byte "O_INVENTORIO_ONDE", $0A
                                .byte "PODES_VER_E_JOGAR", $0A
                                .byte "TODOS_OS_JOGOS_QUE", $0A
                                .byte "TIVERES_ENCONTRADO"
                                .byte $00
string_dialog_explain_wk: .byte "ENCONTRASTES", $0A
                          .byte "WORKHOUSE_KEEPER", $0A
                          .byte $0A
                          .byte "NESTE_JOGO_DEVES", $0A
                          .byte "EMPURRAR_BLOCOS_PARA", $0A
                          .byte "LUGARES_MARCADOS", $0A
                          .byte $0A
                          .byte "CASO_COMETAS_UM_ERRO", $0A
                          .byte "BASTA_APERTAR_B_PARA", $0A
                          .byte "REINICIAR_A_FASE", $0A
                          .byte $0A
                          .byte "BOA_SORTE", $0A
                          .byte $00
string_dialog_explain_gi: .byte "VEJO_QUE_ACHASTES", $0A
                          .byte "GALAXY_INTRUDERS", $0A
                          .byte $0A
                          .byte "NESTE_JOGO_TU_ES_UMA", $0A
                          .byte "NAVE_ESPACIAL_QUE", $0A
                          .byte "ATIRA_NOS_INIMIGOS", $0A
                          .byte $0A
                          .byte "APERTE_A_PARA_ATIRAR", $0A
                          .byte $0A
                          .byte "CUIDADO_POIS_NAVES", $0A
                          .byte "INIMIGAS_FICAM_MAIS", $0A
                          .byte "RAPIDAS_NA_SEGUNDA", $0A
                          .byte "METADE_DO_JOGO", $0A
                          .byte $0A
                          .byte "BOA_SORTE"
                          .byte $00
string_dialog_explain_mf: .byte "ACHASTES_MINE_FINDER", $0A
                          .byte $0A
                          .byte "NESTE_JOGO_DEVES", $0A
                          .byte "DEDUZIR_EM_QUE_CASAS", $0A
                          .byte "HA_OU_NAO_BOMBAS", $0A
                          .byte "ESCONDIDAS", $0A
                          .byte $0A
                          .byte "APERTE_A_PARA_ABRIR", $0A
                          .byte "APERTE_B_PARA_MARCAR", $0A
                          .byte "VENCES_ABRINDO_TODAS", $0A
                          .byte "AS_CASAS_SEM_BOMBA", $0A
                          .byte $0A, $0A, $0A
                          .byte "CASAS_SEM_BOMBA_SAO", $0A
                          .byte "VAZIAS_SE_NAO_HA", $0A
                          .byte "BOMBAS_AO_SEU_REDOR", $0A
                          .byte "OU_CONTEM_O_NUMERO", $0A
                          .byte "DE_BOMBAS_NAS_CASAS", $0A
                          .byte "AO_SEU_REDOR", $0A
                          .byte $0A
                          .byte "BOA_SORTE"
                          .byte $00
string_dialog_explain_rr: .byte "ACHASTES_RIVER_RAY", $0A
                          .byte $0A
                          .byte "NESTE_AQUI_TU_ES_UMA", $0A
                          .byte "ARRAIA_QUE_DEVE", $0A
                          .byte "DESVIAR_DE_TORAS", $0A
                          .byte "ESPINHENTAS", $0A
                          .byte $0A
                          .byte "AS_TORAS_ACELERAM", $0A
                          .byte "CONFORME_A_LINHA_DE", $0A
                          .byte "CHEGADA_SE_APROXIMA", $0A
                          .byte $0A
                          .byte "BOA_SORTE"
                          .byte $00
string_dialog_explain_quest: .byte "VALENTE_HEROI", $0A
                             .byte "AGORA_QUE_POSSUIS", $0A
                             .byte "OS_PODERES_DOS", $0A
                             .byte "CARTUCHOS_LENDARIOS", $0A
                             .byte "PODES_CONFRONTAR_O", $0A
                             .byte "GLITCH", $0A
                             .byte $0A
                             .byte "O_GLITCH_ESTA_NA", $0A
                             .byte "SALA_DOURADA_AO", $0A
                             .byte "NORTE_DE_NOSSO_REINO", $0A
                             .byte $0A
                             .byte "LA_FICA_O_TRONO_DOS", $0A
                             .byte "JOGOS"
                             .byte $00
string_dialog_wk_cartridge: .byte "OBTEVE_UM_NOVO", $0A
                            .byte "CARTUCHO_LENDARIO:", $0A
                            .byte $0A
                            .byte "WORKHOUSE_KEEPER", $00
string_dialog_gi_cartridge: .byte "OBTEVE_UM_NOVO", $0A
                            .byte "CARTUCHO_LENDARIO:", $0A
                            .byte $0A
                            .byte "GALAXY_INTRUDERS", $00
string_dialog_mf_cartridge: .byte "OBTEVE_UM_NOVO", $0A
                            .byte "CARTUCHO_LENDARIO:", $0A
                            .byte $0A
                            .byte "MINE_FINDER", $00
string_dialog_rr_cartridge: .byte "OBTEVE_UM_NOVO", $0A
                            .byte "CARTUCHO_LENDARIO:", $0A
                            .byte $0A
                            .byte "RIVER_RAY", $00
string_dialog_game_over: .byte "O_HEROI_DOS_JOGOS", $0A
                         .byte "FOI_DERROTADO", $0A
                         .byte $0A
                         .byte "NOSSA_ESPERANCA", $0A
                         .byte "SE_FOI", $00
string_dialog_victory: .byte "O_HEROI_DOS_JOGOS", $0A
                       .byte "VENCEU_O_GLITCH", $0A
                       .byte $0A
                       .byte "O_REINO_DE_RETROPIA", $0A
                       .byte "ESTA_LIVRE_NOVAMENTE", $0A
                       .byte $0A
                       .byte $0A
                       .byte $0A
                       .byte $0A
                       .byte "______FIM", $0A
                       .byte $00
string_dialog_wk_win: .byte "OBTEVE_UM_NOVO_PODER:", $0A
                      .byte $0A
                      .byte "EMPURRAR_BLOCOS", $0A
                      .byte $0A
                      .byte "AGORA_BLOCOS_PESADOS", $0A
                      .byte "PODEM_SER_EMPURRADOS", $0A
                      .byte $00
string_dialog_gi_win: .byte "OBTEVE_UM_NOVO_PODER:", $0A
                      .byte $0A
                      .byte "BOLA_DE_FOGO", $0A
                      .byte $0A
                      .byte "APERTE_A_PARA_ATIRAR", $00
string_dialog_mf_win: .byte "OBTEVE_UM_NOVO_PODER:", $0A
                      .byte $0A
                      .byte "CRIAR_BOMBAS", $0A
                      .byte $0A
                      .byte "APERTE_B_PARA_CRIAR", $0A
                      .byte $00
string_dialog_rr_win: .byte "OBTEVE_UM_NOVO_PODER:", $0A
                      .byte $0A
                      .byte "NADAR", $0A
                      .byte $0A
                      .byte "ATRAVESSE_A_AGUA", $0A
                      .byte "PARA_SAIR_NADANDO", $0A
                      .byte $00
.else
string_game_over: .byte "GAME_OVER", $00
string_lives: .byte "LIVES_", WRITE_X_SYMBOL, $00
string_you_win: .byte "YOU_WIN", $00
string_dialog_main_explanation: .byte "HELLO",$0A
                                .byte "I_SEE_YOU_HAVE_A", $0A
                                .byte "NOTENDO_GAMEKID", $0A
                                .byte $0A, $0A, $0A, $0A
                                .byte "THE_PROPHECY_SAYS", $0A
                                .byte "YOU_WILL_SAVE_US", $0A
                                .byte "FROM_THE_MONSTER", $0A
                                .byte "CALLED_THE_GLITCH", $0A
                                .byte $0A, $0A, $0A
                                .byte "IN_ORDER_TO_DO_THAT", $0A
                                .byte "YOU_MUST_FIND_FOUR", $0A
                                .byte "CARTRIDGES_OF_LEGEND", $0A
                                .byte $0A, $0A, $0A, $0A
                                .byte "PLAY_EACH_ONE_ON_YOUR", $0A
                                .byte "GAMEKID_TO_ACQUIRE", $0A
                                .byte "SPECIAL_POWERS", $0A
                                .byte $0A, $0A, $0A, $0A
                                .byte "IF_YOU_FIND_ANY_OF", $0A
                                .byte "THOSE_CARTRIDGES", $0A
                                .byte "YOU_CAN_TALK_TO_ONE", $0A
                                .byte "OF_US_FAERIES_TO_GET", $0A
                                .byte "AN_EXPLANATION_ON", $0A
                                .byte "HOW_TO_PLAY_IT", $0A
                                .byte $0A
                                .byte "PRESSING_SELECT", $0A
                                .byte "OPENS_THE", $0A
                                .byte "INVENTORY_WHERE", $0A
                                .byte "YOU_CAN_SEE_AND_PLAY", $0A
                                .byte "THE_GAMES_YOU_FOUND"
                                .byte $00
string_dialog_explain_wk: .byte "SO_YOU_FOUND", $0A
                          .byte "WORKHOUSE_KEEPER", $0A
                          .byte $0A
                          .byte "IN_THIS_GAME_YOU", $0A
                          .byte "MUST_PUSH_BLOCKS", $0A
                          .byte "ONTO_MARKED_PLACES", $0A
                          .byte $0A
                          .byte "IN_CASE_YOU_MAKE_A", $0A
                          .byte "MISTAKE_JUST_PRESS_B", $0A
                          .byte "TO_RESET_THE_LEVEL", $0A
                          .byte $0A
                          .byte "GOOD_LUCK", $0A
                          .byte $00
string_dialog_explain_gi: .byte "I_SEE_YOU_FOUND", $0A
                          .byte "GALAXY_INTRUDERS", $0A
                          .byte $0A
                          .byte "IN_THIS_GAME_YOU_ARE", $0A
                          .byte "A_SPACESHIP_THAT_CAN", $0A
                          .byte "SHOOT_AT_ENEMY_SHIPS", $0A
                          .byte $0A
                          .byte "PRESS_A_TO_FIRE", $0A
                          .byte $0A
                          .byte "BE_CAREFUL_AS_THE", $0A
                          .byte "ENEMIES_GET_FASTER", $0A
                          .byte "ON_THE_SECOND_HALF", $0A
                          .byte "OF_THE_GAME", $0A
                          .byte $0A
                          .byte "GOOD_LUCK"
                          .byte $00
string_dialog_explain_mf: .byte "YOU_GOT_MINE_FINDER", $0A
                          .byte $0A
                          .byte "IN_THIS_GAME_YOU", $0A
                          .byte "MUST_DEDUCE_WHICH", $0A
                          .byte "CELLS_HAVE_BOMBS_OR", $0A
                          .byte "NOT", $0A
                          .byte $0A
                          .byte "PRESS_A_TO_OPEN_CELL", $0A
                          .byte "PRESS_B_TO_PUT_FLAGS", $0A
                          .byte "YOU_WIN_IF_YOU_OPEN", $0A
                          .byte "ALL_THE_SAFE_CELLS", $0A
                          .byte $0A, $0A, $0A
                          .byte "SAFE_CELLS_ARE_EITHER", $0A
                          .byte "EMPTY_IF_THERE_ARE_NO", $0A
                          .byte "BOMBS_AROUND_OR_THEY", $0A
                          .byte "CONTAIN_THE_NUMBER_OF", $0A
                          .byte "BOMBS_ON_CELLS_AROUND", $0A
                          .byte "THEM", $0A
                          .byte $0A
                          .byte "GOOD_LUCK"
                          .byte $00
string_dialog_explain_rr: .byte "YOU_FOUND_RIVER_RAY", $0A
                          .byte $0A
                          .byte "IN_THIS_ONE_YOU_ARE", $0A
                          .byte "A_MANTA_RAY_AND_YOU", $0A
                          .byte "HAVE_TO_DODGE_SPIKY", $0A
                          .byte "LOGS", $0A
                          .byte $0A
                          .byte "THE_LOGS_GET_FASTER", $0A
                          .byte "AS_YOU_GET_NEARER", $0A
                          .byte "THE_FINISH_LINE", $0A
                          .byte $0A
                          .byte "GOOD_LUCK"
                          .byte $00
string_dialog_explain_quest: .byte "BRAVE_HERO", $0A
                             .byte "NOW_THAT_YOU_HAVE", $0A
                             .byte "THE_POWERS_FROM_THE", $0A
                             .byte "CARTRIDGES_OF_LEGEND", $0A
                             .byte "YOU_CAN_FIGHT_THE", $0A
                             .byte "GLITCH", $0A
                             .byte $0A
                             .byte "THE_GLITCH_IS_ON_THE", $0A
                             .byte "GOLDEN_ROOM_IN_THE", $0A
                             .byte "NORTH_OF_OUR_KINGDOM", $0A
                             .byte $0A
                             .byte "IT_IS_THE_LOCATION_OF", $0A
                             .byte "THE_THRONE_OF_GAMES"
                             .byte $00
string_dialog_wk_cartridge: .byte "YOU_GOT_A_NEW", $0A
                            .byte "CARTRIDGE_OF_LEGEND:", $0A
                            .byte $0A
                            .byte "WORKHOUSE_KEEPER", $00
string_dialog_gi_cartridge: .byte "YOU_GOT_A_NEW", $0A
                            .byte "CARTRIDGE_OF_LEGEND:", $0A
                            .byte $0A
                            .byte "GALAXY_INTRUDERS", $00
string_dialog_mf_cartridge: .byte "YOU_GOT_A_NEW", $0A
                            .byte "CARTRIDGE_OF_LEGEND:", $0A
                            .byte $0A
                            .byte "MINE_FINDER", $00
string_dialog_rr_cartridge: .byte "YOU_GOT_A_NEW", $0A
                            .byte "CARTRIDGE_OF_LEGEND:", $0A
                            .byte $0A
                            .byte "RIVER_RAY", $00
string_dialog_game_over: .byte "THE_HERO_OF_GAMES", $0A
                         .byte "WAS_DEFEATED", $0A
                         .byte $0A
                         .byte "OUR_HOPE_IS_GONE", $00
string_dialog_victory: .byte "THE_HERO_OF_GAMES", $0A
                       .byte "DEFEATED_THE_GLITCH", $0A
                       .byte $0A
                       .byte "THE_LAND_OF_RETROPIA", $0A
                       .byte "WAS_FINALLY_FREE", $0A
                       .byte $0A
                       .byte $0A
                       .byte $0A
                       .byte $0A
                       .byte "______THE_END", $0A
                       .byte $00
string_dialog_wk_win: .byte "YOU_GOT_A_NEW_POWER:", $0A
                      .byte $0A
                      .byte "PUSHING_BLOCKS", $0A
                      .byte $0A
                      .byte "HEAVY_BLOCKS_CAN_BE", $0A
                      .byte "PUSHED_NOW", $0A
                      .byte $00
string_dialog_gi_win: .byte "YOU_GOT_A_NEW_POWER:", $0A
                      .byte $0A
                      .byte "FIREBALL", $0A
                      .byte $0A
                      .byte "PRESS_A_TO_SHOOT", $00
string_dialog_mf_win: .byte "YOU_GOT_A_NEW_POWER:", $0A
                      .byte $0A
                      .byte "CREATING_BOMBS", $0A
                      .byte $0A
                      .byte "PRESS_B_TO_DROP_BOMB", $0A
                      .byte $00
string_dialog_rr_win: .byte "YOU_GOT_A_NEW_POWER:", $0A
                      .byte $0A
                      .byte "SWIMMING", $0A
                      .byte $0A
                      .byte "WALK_INTO_WATER_TO", $0A
                      .byte "SWIM_AROUND", $0A
                      .byte $00
.endif
screens_l:
        .byte $00 ; padding
        .byte <screen_1_data
        .byte <screen_2_data
        .byte <screen_3_data
        .byte <screen_4_data
        .byte <screen_5_data
        .byte <screen_6_data
        .byte <screen_7_data
        .byte <screen_8_data
        .byte <screen_9_data
        .byte <screen_A_data
        .byte <screen_B_data
screens_h:
        .byte $00 ; padding
        .byte >screen_1_data
        .byte >screen_2_data
        .byte >screen_3_data
        .byte >screen_4_data
        .byte >screen_5_data
        .byte >screen_6_data
        .byte >screen_7_data
        .byte >screen_8_data
        .byte >screen_9_data
        .byte >screen_A_data
        .byte >screen_B_data

        ; screen data format:
        ; pointer to rle bg nametable
        ; index of screen exits (up,down,left,right)
        ; array of:
        ;   [wall x1] [y1] [x2] [y2]
        ;   (ends with x1 == 0)
        ; array of:
        ;   [object type] [x] [y] [direction] [rom ptr] [ram value]
        ;   (ends with object type == 0)
screen_1_data:
        .word nametable_screen_oooo
        .byte $09, $06, $04, $02
        .byte $00
        .ifdef DEBUG
        .byte object_type::cartridge_wk, $30, $30, direction::up
        .word $0000
        .byte $00
        .byte object_type::cartridge_gi, $D0, $30, direction::up
        .word $0000
        .byte $00
        .byte object_type::cartridge_mf, $30, $C0, direction::up
        .word $0000
        .byte $00
        .byte object_type::cartridge_rr, $D0, $C0, direction::up
        .word $0000
        .byte $00
        .endif
        .byte object_type::faerie, $60, $60, direction::up
        .word $0000
        .byte $00

        .byte $00
screen_2_data:
        .word nametable_screen_2
        .byte $00, $00, $01, $03
        .byte $48, $38, $77, $77, $00
        .byte $88, $78, $B7, $B7, $00
        .byte $00 ; end of walls
        .byte object_type::enemy_vrissy, $58, $20, direction::right
        .word screen_2_vrissy_1_code
        .byte $00
        .byte object_type::enemy_vrissy, $58, $80, direction::left
        .word screen_2_vrissy_1_code
        .byte $04
        .byte object_type::enemy_vrissy, $98, $60, direction::right
        .word screen_2_vrissy_2_code
        .byte $00
        .byte object_type::enemy_vrissy, $98, $C0, direction::left
        .word screen_2_vrissy_2_code
        .byte $04
        .byte $00 ; end of objects
screen_2_vrissy_1_code:
        .byte $78, direction::down
        .byte $80, direction::left
        .byte $38, direction::up
        .byte $20, direction::right
        .byte $00
screen_2_vrissy_2_code:
        .byte $B8, direction::down
        .byte $C0, direction::left
        .byte $78, direction::up
        .byte $60, direction::right
        .byte $00

screen_3_data:
        .word nametable_screen_ccoc
        .byte $00, $00, $02, $00
        .byte $00 ; end of walls
        .byte object_type::cartridge_gi, $B0, $70, direction::up
        .word $0000
        .byte $00
        .byte object_type::faerie, $40, $40, direction::up
        .word $0000
        .byte $00

        .byte $00 ; end of objects

screen_4_data:
        .word nametable_screen_4
        .byte $00, $00, $05, $01
        .byte $00

        .byte object_type::pushable_block, $60, $90, direction::up
        .word screen_4_block_1_code
        .byte $00

        .byte object_type::pushable_block, $90, $90, direction::right
        .word screen_4_block_2_code
        .byte $00

        .byte $00 ; end of objects
screen_4_block_1_code:
        .byte $30, $90
screen_4_block_2_code:
        .byte $90, $D0

screen_5_data:
        .word nametable_screen_ccco
        .byte $00, $00, $00, $04
        .byte $00 ; end of walls
        .byte object_type::cartridge_wk, $50, $50, direction::up
        .word $0000
        .byte $00
        .byte object_type::faerie, $A0, $90, direction::up
        .word $0000
        .byte $00
        .byte $00 ; end of objects
screen_6_data:
        .word nametable_screen_6
        .byte $01, $00, $08, $07
        .byte $31, $21, $3E, $2E, $00
        .byte $51, $21, $5E, $4E, $00
        .byte $31, $41, $3E, $4E, $00
        .byte $31, $61, $5E, $8E, $00
        .byte $31, $A1, $5E, $CE, $00

        .byte $A1, $21, $CE, $4E, $00
        .byte $A1, $71, $CE, $7E, $00
        .byte $A1, $A1, $CE, $CE, $00

        .byte $A8, $80, $C7, $9F, $01 ; water

        .byte $00 ; end of walls

        .byte object_type::pushable_block, $50, $50, direction::left
        .word screen_6_block_1_code
        .byte $00

        .byte object_type::pushable_block, $40, $40, direction::up
        .word screen_6_block_2_code
        .byte $00

        .byte object_type::enemy_vrissy, $98, $58, direction::right
        .word screen_6_vrissy_code
        .byte $00

        .byte object_type::breakable_wall, $40, $90, direction::up
        .word $0000
        .byte $00

        .byte $00 ; end of objects
screen_6_block_1_code:
        .byte $30, $50
screen_6_block_2_code:
        .byte $20, $40
screen_6_vrissy_code:
        .byte $C8, direction::left
        .byte $98, direction::right
        .byte $00

screen_7_data:
        .word nametable_screen_ccoc
        .byte $00, $00, $06, $00
        .byte $00 ; end of walls
        .byte object_type::cartridge_rr, $48, $A0, direction::up
        .word $0000
        .byte $00
        .byte object_type::faerie, $30, $40, direction::up
        .word $0000
        .byte $00
        .byte $00 ; end of objects
screen_8_data:
        .word nametable_screen_ccco
        .byte $00, $00, $00, $06
        .byte $00 ; end of walls
        .byte object_type::cartridge_mf, $98, $C0, direction::up
        .word $0000
        .byte $00
        .byte object_type::faerie, $80, $40, direction::up
        .word $0000
        .byte $00
        .byte $00 ; end of objects
screen_9_data:
        .word nametable_screen_9
        .byte $0A, $01, $00, $00
        .byte $18, $C0, $7F, $CF, $00
        .byte $C0, $C0, $EF, $EF, $00
        .byte $D0, $70, $DF, $BF, $00
        .byte $B0, $70, $CF, $8F, $00
        .byte $C0, $90, $CF, $AF, $00
        .byte $70, $A0, $AF, $AF, $00
        .byte $38, $A0, $5F, $AF, $00
        .byte $70, $70, $8F, $8F, $00
        .byte $38, $38, $4F, $9F, $00
        .byte $50, $38, $EF, $4F, $00
        .byte $E0, $38, $EF, $BF, $00
        .byte $00 ; end of walls

        .byte object_type::pushable_block, $A0, $90, direction::left
        .word screen_9_block_1_code
        .byte $00

        .byte object_type::pushable_block, $70, $B0, direction::right
        .word screen_9_block_2_code
        .byte $00

        .byte object_type::enemy_vrissy, $18, $A0, direction::up
        .word screen_9_vrissy_code_a
        .byte $00

        .byte object_type::enemy_vrissy, $28, $90, direction::up
        .word screen_9_vrissy_code_b
        .byte $00

        .byte $00 ; end of objects

screen_9_block_1_code:
        .byte $60, $A0

screen_9_block_2_code:
        .byte $70, $C0

; 18 1D 22 28
screen_9_vrissy_code_a:
        .byte $20, direction::right
        .byte $1D, direction::down
        .byte $A0, direction::right
        .byte $22, direction::up
        .byte $20, direction::right
        .byte $28, direction::down
        .byte $A0, direction::left
        .byte $22, direction::up
        .byte $20, direction::left
        .byte $1D, direction::down
        .byte $A0, direction::left
        .byte $18, direction::up
        .byte $00
screen_9_vrissy_code_b:
        .byte $20, direction::left
        .byte $22, direction::down
        .byte $A0, direction::left
        .byte $1D, direction::up
        .byte $20, direction::left
        .byte $18, direction::down
        .byte $A0, direction::right
        .byte $1D, direction::up
        .byte $20, direction::right
        .byte $22, direction::down
        .byte $A0, direction::right
        .byte $28, direction::up
        .byte $00

screen_A_data:
        .word nametable_screen_A
        .byte $0B, $09, $00, $00

        ; rivers
        .byte $18, $A8, $E7, $C7, $01
        .byte $30, $30, $8F, $37, $01
        .byte $30, $30, $3F, $67, $01
        .byte $40, $60, $47, $67, $01

        ; normal walls

        .byte $90, $70, $BF, $9F, $00
        .byte $D0, $50, $E7, $9F, $00
        .byte $18, $18, $2F, $6F, $00
        .byte $18, $70, $1F, $9F, $00
        .byte $20, $90, $2F, $9F, $00
        .byte $40, $90, $8F, $9F, $00
        .byte $40, $80, $4F, $8F, $00
        .byte $30, $70, $4F, $7F, $00
        .byte $60, $70, $7F, $7F, $00
        .byte $60, $60, $6F, $6F, $00
        .byte $40, $40, $4F, $5F, $00
        .byte $50, $40, $7F, $4F, $00
        .byte $70, $50, $CF, $5F, $00
        .byte $30, $20, $8F, $2F, $00
        .byte $90, $30, $AF, $3F, $00
        .byte $C0, $30, $CF, $3F, $00

        .byte $00 ; end of walls

        .byte object_type::breakable_wall, $B0, $20, direction::up
        .word $0000
        .byte $00

        .byte object_type::breakable_wall, $C0, $40, direction::up
        .word $0000
        .byte $00

        .byte object_type::breakable_wall, $80, $70, direction::up
        .word $0000
        .byte $00

        .byte object_type::breakable_wall, $30, $90, direction::up
        .word $0000
        .byte $00

        .byte object_type::breakable_wall, $C0, $90, direction::up
        .word $0000
        .byte $00


        .byte $00 ; end of objects
screen_B_data:
        .word nametable_screen_B
        .byte $00, $0A, $00, $00
        .byte $00 ; end of walls

        .byte object_type::glitch_boss, $C0, $30, direction::left
        .word $0000
        .byte $00

        .byte $00 ; end of objects

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

.ifdef PTBR
nametable_title: .incbin "../assets/nametables/title-ptbr.rle"
.else
nametable_title: .incbin "../assets/nametables/title.rle"
.endif

nametable_screen_2: .incbin "../assets/nametables/screens/screen-2.rle"
nametable_screen_4: .incbin "../assets/nametables/screens/screen-4.rle"
nametable_screen_6: .incbin "../assets/nametables/screens/screen-6.rle"
nametable_screen_9: .incbin "../assets/nametables/screens/screen-9.rle"
nametable_screen_A: .incbin "../assets/nametables/screens/screen-a.rle"
nametable_screen_B: .incbin "../assets/nametables/screens/screen-b.rle"
nametable_screen_oooo: .incbin "../assets/nametables/screens/grass-oooo.rle"
nametable_screen_ccoc: .incbin "../assets/nametables/screens/grass-ccoc.rle"
nametable_screen_ccco: .incbin "../assets/nametables/screens/grass-ccco.rle"

nametable_gamekid_boot: .incbin "../assets/nametables/gamekid-titles/boot.rle"
nametable_wk_title: .incbin "../assets/nametables/gamekid-titles/wk.rle"
nametable_gi_title: .incbin "../assets/nametables/gamekid-titles/gi.rle"
nametable_mf_title: .incbin "../assets/nametables/gamekid-titles/mf.rle"
nametable_rr_title: .incbin "../assets/nametables/gamekid-titles/rr.rle"

subgame_by_game_state:
        .byte $00, $00, $00, $00, $00 ; main
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

.segment "CHR"
.incbin "../assets/graphics.chr"
