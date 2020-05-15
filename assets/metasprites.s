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

