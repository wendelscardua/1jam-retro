	metasprite_0_data:

	.byte   0,  0,$c0,3
	.byte   8,  0,$c0,3|OAM_FLIP_H
	.byte   0,  8,$c0,3|OAM_FLIP_V
	.byte   8,  8,$c0,3|OAM_FLIP_H|OAM_FLIP_V
	.byte 128

	metasprite_1_data:

	.byte   0,  0,$c1,3
	.byte   8,  0,$c2,3
	.byte   0,  8,$d1,3
	.byte   8,  8,$d2,3
	.byte 128

	metasprite_2_data:

	.byte   0,  0,$c8,3
	.byte   8,  0,$c8,3|OAM_FLIP_H
	.byte   0,  8,$d8,3
	.byte   8,  8,$d9,3
	.byte 128

	metasprite_3_data:

	.byte   0,  0,$c9,3
	.byte 128

	metasprite_4_data:

	.byte   0,  0,$ca,3
	.byte   8,  0,$ca,3|OAM_FLIP_H
	.byte 128

	metasprite_5_data:

	.byte   0,  0,$c3,3
	.byte   8,  0,$c3,3|OAM_FLIP_H
	.byte   0,  8,$c3,3|OAM_FLIP_V
	.byte   8,  8,$c3,3|OAM_FLIP_H|OAM_FLIP_V
	.byte 128

	metasprite_6_data:

	.byte   0,  0,$e0,3
	.byte   8,  0,$e1,3
	.byte   8,  8,$f1,3
	.byte   0,  8,$f0,3
	.byte 128

	metasprite_7_data:

	.byte   0,  0,$e2,3
	.byte   0,  8,$f2,3
	.byte 128

	metasprite_8_data:

	.byte   0,  0,$e3,3
	.byte 128

	metasprite_9_data:

	.byte   0,  0,$f3,3
	.byte   8,  0,$f3,3
	.byte   0,  8,$f3,3
	.byte   8,  8,$f3,3
	.byte 128

	metasprite_10_data:

	.byte   0,  0,$00,0
	.byte   8,  0,$00,0|OAM_FLIP_H
	.byte   0,  8,$01,0
	.byte   8,  8,$02,0
	.byte 128

	metasprite_11_data:

	.byte   0,  0,$00,0
	.byte   8,  0,$00,0|OAM_FLIP_H
	.byte   0,  8,$02,0|OAM_FLIP_H
	.byte   8,  8,$01,0|OAM_FLIP_H
	.byte 128

	metasprite_12_data:

	.byte   0,  0,$03,0
	.byte   8,  0,$04,0
	.byte   0,  8,$01,0
	.byte   8,  8,$02,0
	.byte 128

	metasprite_13_data:

	.byte   0,  0,$03,0
	.byte   8,  0,$04,0
	.byte   0,  8,$02,0|OAM_FLIP_H
	.byte   8,  8,$01,0|OAM_FLIP_H
	.byte 128

	metasprite_14_data:

	.byte   0,  0,$06,0|OAM_FLIP_H
	.byte   8,  0,$05,0|OAM_FLIP_H
	.byte   0,  8,$08,0|OAM_FLIP_H
	.byte   8,  8,$07,0|OAM_FLIP_H
	.byte 128

	metasprite_15_data:

	.byte   8,  0,$0b,0|OAM_FLIP_H
	.byte   0,  0,$06,0|OAM_FLIP_H
	.byte   8,  8,$09,0|OAM_FLIP_H
	.byte   0,  8,$0a,0|OAM_FLIP_H
	.byte 128

	metasprite_16_data:

	.byte   0,  0,$05,0
	.byte   8,  0,$06,0
	.byte   0,  8,$07,0
	.byte   8,  8,$08,0
	.byte 128

	metasprite_17_data:

	.byte   0,  0,$0b,0
	.byte   8,  0,$06,0
	.byte   0,  8,$09,0
	.byte   8,  8,$0a,0
	.byte 128

	metasprite_18_data:

	.byte   0,  0,$0e,2|OAM_FLIP_V
	.byte   8,  0,$0f,2|OAM_FLIP_V
	.byte   0,  8,$0c,2|OAM_FLIP_V
	.byte   8,  8,$0d,2|OAM_FLIP_V
	.byte 128

	metasprite_19_data:

	.byte   0,  0,$0f,2|OAM_FLIP_H|OAM_FLIP_V
	.byte   8,  0,$0e,2|OAM_FLIP_H|OAM_FLIP_V
	.byte   0,  8,$0d,2|OAM_FLIP_H|OAM_FLIP_V
	.byte   8,  8,$0c,2|OAM_FLIP_H|OAM_FLIP_V
	.byte 128

	metasprite_20_data:

	.byte   0,  0,$0c,2
	.byte   8,  0,$0d,2
	.byte   0,  8,$0e,2
	.byte   8,  8,$0f,2
	.byte 128

	metasprite_21_data:

	.byte   0,  0,$0d,2|OAM_FLIP_H
	.byte   8,  0,$0c,2|OAM_FLIP_H
	.byte   0,  8,$0f,2|OAM_FLIP_H
	.byte   8,  8,$0e,2|OAM_FLIP_H
	.byte 128

	metasprite_22_data:

	.byte   0,  0,$12,2
	.byte   8,  0,$10,2
	.byte   0,  8,$13,2
	.byte   8,  8,$11,2
	.byte 128

	metasprite_23_data:

	.byte   0,  0,$13,2|OAM_FLIP_V
	.byte   8,  0,$11,2|OAM_FLIP_V
	.byte   0,  8,$12,2|OAM_FLIP_V
	.byte   8,  8,$10,2|OAM_FLIP_V
	.byte 128

	metasprite_24_data:

	.byte   0,  0,$10,2|OAM_FLIP_H
	.byte   8,  0,$12,2|OAM_FLIP_H
	.byte   0,  8,$11,2|OAM_FLIP_H
	.byte   8,  8,$13,2|OAM_FLIP_H
	.byte 128

	metasprite_25_data:

	.byte   0,  0,$11,2|OAM_FLIP_H|OAM_FLIP_V
	.byte   8,  0,$13,2|OAM_FLIP_H|OAM_FLIP_V
	.byte   0,  8,$10,2|OAM_FLIP_H|OAM_FLIP_V
	.byte   8,  8,$12,2|OAM_FLIP_H|OAM_FLIP_V
	.byte 128

	metasprite_26_data:

	.byte   0,  0,$14,1
	.byte   8,  0,$15,1
	.byte   0,  8,$16,1
	.byte   8,  8,$17,1
	.byte   4,  3,$1b,3
	.byte 128

	metasprite_27_data:

	.byte   0,  0,$14,1
	.byte   8,  0,$15,1
	.byte   0,  8,$16,1
	.byte   8,  8,$17,1
	.byte   4,  3,$18,3
	.byte 128

	metasprite_28_data:

	.byte   0,  0,$14,1
	.byte   8,  0,$15,1
	.byte   0,  8,$16,1
	.byte   8,  8,$17,1
	.byte   4,  3,$19,3
	.byte 128

	metasprite_29_data:

	.byte   0,  0,$14,1
	.byte   8,  0,$15,1
	.byte   0,  8,$16,1
	.byte   8,  8,$17,1
	.byte   4,  3,$1a,3
	.byte 128

	metasprite_30_data:

	.byte   0,  0,$1c,1
	.byte 128

	metasprite_31_data:

	.byte   0,  0,$1d,1
	.byte 128

	metasprite_32_data:

	.byte   0,  0,$1e,1
	.byte 128

	metasprite_33_data:

	.byte   0,  0,$1f,1
	.byte 128

	metasprite_pointers:

	.word metasprite_0_data
	.word metasprite_1_data
	.word metasprite_2_data
	.word metasprite_3_data
	.word metasprite_4_data
	.word metasprite_5_data
	.word metasprite_6_data
	.word metasprite_7_data
	.word metasprite_8_data
	.word metasprite_9_data
	.word metasprite_10_data
	.word metasprite_11_data
	.word metasprite_12_data
	.word metasprite_13_data
	.word metasprite_14_data
	.word metasprite_15_data
	.word metasprite_16_data
	.word metasprite_17_data
	.word metasprite_18_data
	.word metasprite_19_data
	.word metasprite_20_data
	.word metasprite_21_data
	.word metasprite_22_data
	.word metasprite_23_data
	.word metasprite_24_data
	.word metasprite_25_data
	.word metasprite_26_data
	.word metasprite_27_data
	.word metasprite_28_data
	.word metasprite_29_data
	.word metasprite_30_data
	.word metasprite_31_data
	.word metasprite_32_data
	.word metasprite_33_data
