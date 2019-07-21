SUBROUTINE MY_SELE( SELE )
	USE ATOMS, ONLY : NATOMS
	USE ATOM_MANIPULATION, ONLY : ATOM_SELECTION
	LOGICAL, DIMENSION(1:NATOMS), INTENT(INOUT) :: SELE
	SELE = .FALSE.
	SELE = SELE .OR. ATOM_SELECTION( &
		SUBSYSTEM = (/ 'A' /), &
		RESIDUE_NUMBER = (/ 1, 5, 6, 8, 10, 12, 13, 14, 15, 16, 17, 18, 19,  &
20, 21, 22, 23, 24, 25, 31, 33, 34, 35, 36, 37, 38,  &
39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51,  &
52, 53, 54, 55, 56, 57, 58, 59, 65, 66, 67, 68, 69,  &
70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82,  &
83, 84, 85, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98,  &
99, 100, 101, 102, 103, 104, 105, 111, 112, 113, 114, 115, 116,  &
117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128, 139,  &
140, 141, 142, 143, 144, 145, 146, 147, 148, 149, 150, 151, 152,  &
153, 154, 155, 156, 157, 175, 176, 177, 178, 179, 180, 181, 182,  &
183, 184, 185, 186, 188, 189, 190, 191, 192, 193, 194, 195, 196,  &
197, 198, 199, 201, 202, 210, 216, 217, 218, 219, 220, 221, 222,  &
223, 224, 225, 226, 227, 228, 229, 230, 231, 232, 233, 234, 235,  &
237, 238, 239, 240, 241, 242, 243, 247, 248, 249, 250, 251, 252,  &
253, 254, 255, 256, 257, 258 /) )
	SELE = SELE .OR. ATOM_SELECTION( &
		SUBSYSTEM = (/ 'B' /), &
		RESIDUE_NUMBER = (/ 1 /) )
	SELE = SELE .OR. ATOM_SELECTION( &
		SUBSYSTEM = (/ 'C' /), &
		RESIDUE_NUMBER = (/ 1 /) )
	SELE = SELE .OR. ATOM_SELECTION( &
		SUBSYSTEM = (/ 'BOX' /), &
		RESIDUE_NUMBER = (/ 7, 15, 21, 29, 39, 50, 56, 61, 89, 111, 122, 135, 145,  &
148, 157, 159, 162, 165, 166, 183, 188, 194, 208, 209, 238, 251,  &
257, 261, 279, 288, 291, 299, 307, 311, 315, 339, 343, 348, 350,  &
363, 364, 366, 376, 385, 392, 410, 417, 421, 422, 423, 428, 430,  &
449, 457, 459, 460, 485, 487, 493, 495, 502, 512, 530, 538, 573,  &
574, 583, 584, 589, 600, 602, 608, 619, 626, 646, 647, 650, 653,  &
658, 673, 674, 684, 686, 696, 701, 704, 726, 728, 730, 733, 737,  &
768, 776, 782, 786, 789, 798, 805, 807, 811, 815, 816, 819, 820,  &
830, 833, 839, 840, 847, 857, 864, 865, 868, 873, 874, 884, 888,  &
891, 895, 901, 933, 938, 942, 943, 945, 946, 948, 949, 952, 955,  &
956, 968, 983, 1002, 1004, 1011, 1031, 1058, 1069, 1071, 1106, 1123, 1136,  &
1137, 1138, 1156, 1161, 1162, 1168, 1172, 1175, 1176, 1178, 1183, 1185, 1198,  &
1213, 1221, 1247, 1261, 1270, 1276, 1295, 1306, 1307, 1312, 1317, 1320, 1326,  &
1338, 1339, 1344, 1347, 1365, 1377, 1381, 1391, 1397, 1398, 1405, 1414, 1420,  &
1431, 1434, 1436, 1444, 1470, 1472, 1478, 1489, 1495, 1518, 1521, 1530, 1534,  &
1544, 1546, 1555, 1558, 1559, 1564, 1571, 1584, 1586, 1596, 1597, 1605, 1606,  &
1609, 1610, 1618, 1646, 1654, 1657, 1659, 1660, 1671, 1675, 1683, 1685, 1688,  &
1694, 1698, 1708, 1709, 1717, 1722, 1727, 1732, 1737, 1738, 1744, 1754, 1759,  &
1762, 1765, 1767, 1772, 1782, 1791, 1794, 1795, 1801, 1807, 1815, 1833, 1849,  &
1854, 1861, 1862, 1883, 1889, 1903, 1907, 1910, 1915, 1916, 1917, 1966, 1973,  &
1975, 1976, 2001, 2007, 2027, 2034, 2038, 2039, 2041, 2061, 2062, 2071, 2080,  &
2093, 2096, 2100, 2103, 2110, 2120, 2131, 2142, 2151, 2152, 2157, 2158, 2163,  &
2175, 2183, 2189, 2201, 2208, 2211, 2221, 2229, 2237, 2243, 2253, 2254, 2264,  &
2267, 2281, 2300, 2306, 2314, 2339, 2351, 2359, 2363, 2373, 2379, 2390, 2395,  &
2396, 2397, 2403, 2451, 2452, 2456, 2457, 2476, 2483, 2490, 2512, 2521, 2523,  &
2525, 2545, 2558, 2563, 2568, 2576, 2581, 2584, 2603, 2612, 2626, 2627, 2630,  &
2631, 2650, 2657, 2661, 2667, 2671, 2673, 2705, 2708, 2715, 2724, 2726, 2727,  &
2733, 2742, 2748, 2784, 2788, 2791, 2814, 2822, 2841, 2844, 2854, 2855, 2867,  &
2872, 2876, 2877, 2881, 2882, 2885, 2892, 2893, 2904, 2919, 2920, 2941, 2943,  &
2974, 2977, 2978, 2985, 2991, 3013, 3017, 3023, 3028, 3033, 3037, 3040, 3049,  &
3054, 3055, 3059, 3061, 3074, 3097, 3106, 3109, 3118, 3130, 3136, 3138, 3145,  &
3177, 3183, 3184, 3186, 3189, 3192, 3221, 3230, 3238, 3246, 3256, 3268, 3271,  &
3293, 3299, 3302, 3312, 3327, 3330, 3338, 3341, 3343, 3350, 3365, 3368, 3369,  &
3372, 3374, 3386, 3390, 3392, 3397, 3405, 3407, 3425, 3426, 3429, 3439, 3445,  &
3462, 3465, 3467, 3468, 3472, 3479, 3483, 3503, 3516, 3520, 3542, 3543, 3544,  &
3549, 3572, 3576, 3578, 3586, 3590, 3595, 3602, 3609, 3638, 3644, 3649, 3654,  &
3656, 3669, 3675, 3677, 3689, 3690, 3702, 3726, 3731, 3732, 3739, 3754, 3760,  &
3761, 3767, 3776, 3787, 3796, 3805, 3808, 3823, 3825, 3829, 3831, 3838, 3847,  &
3853, 3855, 3856, 3867, 3870, 3876, 3901, 3909, 3910, 3919, 3928, 3935, 3951,  &
3952, 3954, 3958, 3964, 3966, 3967, 3970, 3976, 3980, 3982, 3983, 3997, 3998,  &
4000, 4006, 4023, 4025, 4045, 4048, 4049, 4061, 4066, 4067, 4069, 4077, 4078,  &
4079, 4088, 4095, 4130, 4140, 4151, 4152, 4154, 4179, 4187, 4190, 4204, 4206,  &
4221, 4222, 4228, 4245, 4247, 4251, 4258, 4266, 4277, 4282, 4287, 4303, 4308,  &
4314, 4324, 4327, 4332, 4337, 4362, 4367, 4377, 4389, 4395, 4399, 4416, 4426,  &
4428, 4440, 4446, 4449, 4460, 4461, 4463, 4464, 4479, 4480, 4483, 4488, 4495,  &
4500, 4503, 4526, 4531, 4535, 4538, 4543, 4550, 4556, 4560, 4562, 4566, 4573,  &
4579, 4589, 4593, 4600, 4617, 4618, 4625, 4626, 4631, 4646, 4649, 4651, 4652,  &
4657, 4660, 4663, 4677, 4679, 4682, 4688, 4689, 4706, 4723, 4730, 4743, 4752,  &
4768, 4771, 4780, 4788, 4794, 4802, 4805, 4815, 4818, 4822, 4829, 4850, 4851,  &
4852, 4869, 4877, 4888, 4905, 4906, 4907, 4910, 4923, 4924, 4941, 4945, 4948,  &
4957, 4958, 4962, 4965, 4969, 4980, 4990, 4992, 4994, 5004, 5015, 5018, 5038,  &
5043, 5046, 5087, 5097, 5104, 5107, 5118, 5123, 5144, 5148, 5150, 5151, 5162,  &
5168, 5170, 5176, 5181, 5192, 5196, 5219, 5223, 5228, 5230, 5241, 5253, 5268,  &
5281, 5285, 5287, 5290, 5301, 5305, 5310, 5316, 5321, 5339, 5340, 5371, 5372,  &
5389, 5390, 5391, 5409, 5411, 5418, 5419, 5436, 5442, 5450, 5459, 5460, 5465,  &
5470, 5478, 5482, 5497, 5499, 5504, 5514, 5542, 5564, 5586, 5592, 5594, 5596,  &
5597, 5602, 5603, 5604, 5616, 5618, 5625, 5627, 5634, 5648, 5653, 5661, 5664,  &
5684, 5696, 5702, 5710, 5718, 5726, 5737, 5749, 5758, 5761, 5766, 5768, 5775,  &
5800, 5811, 5819, 5828, 5834, 5840, 5841, 5852, 5854, 5862, 5865, 5871, 5883,  &
5889, 5895, 5897, 5907, 5908, 5917, 5922, 5926, 5969, 5974, 5976, 5982, 5992,  &
6010, 6019, 6021, 6047, 6051, 6066, 6067, 6069, 6070, 6076, 6078, 6082, 6088,  &
6097, 6109, 6114, 6155, 6161, 6163, 6167, 6184, 6194, 6196, 6197, 6212, 6221,  &
6223, 6231, 6234, 6240, 6243, 6249, 6253, 6256, 6260, 6278, 6288, 6291, 6303,  &
6305, 6314, 6317, 6328, 6333, 6338, 6343, 6352, 6375, 6386, 6389, 6390, 6398,  &
6415, 6420, 6428, 6429, 6430, 6437, 6446, 6456, 6465, 6468, 6476, 6480, 6492,  &
6497, 6515, 6531, 6534, 6545, 6573, 6577, 6594, 6598, 6599, 6603, 6634, 6654,  &
6662, 6668, 6678, 6681, 6688, 6689, 6696, 6700, 6712, 6713, 6715, 6729, 6731,  &
6733, 6746, 6751, 6752, 6756, 6766, 6780, 6788, 6792, 6799, 6806, 6811, 6821,  &
6836, 6856, 6864, 6867, 6872, 6877, 6882, 6884, 6892, 6896, 6902, 6908, 6912,  &
6927, 6937, 6945, 6954, 6966, 6971, 6973, 6979, 6986, 6989, 6996, 7001, 7011,  &
7020, 7022, 7054, 7055, 7057, 7063, 7067, 7086, 7089, 7092, 7132, 7134, 7136,  &
7139, 7140, 7174, 7189, 7193, 7215, 7230, 7233, 7235, 7238, 7241, 7251, 7257,  &
7259, 7268, 7272, 7273, 7280, 7281, 7292, 7296, 7305, 7326, 7332, 7339, 7349,  &
7350, 7353, 7361, 7371, 7374, 7402, 7410, 7427, 7430, 7434, 7453, 7458, 7470,  &
7471, 7480, 7490, 7491, 7495, 7512, 7514, 7520, 7528, 7529, 7533, 7543, 7547,  &
7566, 7576, 7578, 7581, 7589, 7593, 7594, 7599, 7601, 7604, 7608, 7619, 7629,  &
7631, 7634, 7679, 7684, 7693, 7701, 7710, 7725, 7731, 7746, 7747, 7756, 7765,  &
7769, 7777, 7780, 7782, 7785, 7786, 7793, 7799, 7805, 7807, 7809, 7825, 7833,  &
7834, 7841, 7845, 7849, 7855, 7881, 7882, 7886, 7890, 7898, 7903, 7906, 7910,  &
7915, 7916, 7922, 7925, 7926, 7928, 7929, 7938, 7948, 7952, 7967, 7975, 7976,  &
7985, 7990, 7995, 7997, 8002, 8013, 8021, 8027, 8036, 8044, 8058, 8066, 8077,  &
8097, 8104, 8109, 8110, 8111, 8121, 8131, 8136, 8141, 8145, 8150, 8153, 8186,  &
8187, 8189, 8190, 8204, 8217, 8227, 8229, 8235, 8244, 8249, 8253, 8255, 8257,  &
8266, 8274, 8284, 8300, 8316, 8322, 8327, 8329, 8333, 8339, 8346, 8347, 8365,  &
8380, 8387, 8396, 8407, 8411, 8423, 8424, 8426, 8433, 8436, 8442, 8482, 8487,  &
8492, 8512, 8513, 8524, 8544, 8549, 8557, 8561, 8588, 8592, 8601, 8603, 8617,  &
8629, 8640, 8652, 8660, 8662, 8667, 8691, 8693, 8694, 8699, 8701, 8709, 8711,  &
8716, 8734, 8735, 8744, 8753, 8760, 8762, 8763, 8770, 8778, 8784, 8795, 8799,  &
8805, 8816, 8818, 8847, 8853, 8855, 8859, 8866, 8869, 8887, 8891, 8893, 8896,  &
8903, 8905, 8912, 8927, 8929, 8933, 8939, 8940, 8946, 8948, 8951, 8957, 8958,  &
8960, 8964, 8967, 8980, 8982, 8984, 8995, 9002, 9014, 9016, 9025, 9029, 9032,  &
9035, 9046, 9051, 9052, 9093, 9101, 9103, 9115, 9123, 9127, 9131, 9134, 9139,  &
9145, 9186, 9196, 9202, 9217, 9218, 9234, 9235, 9236, 9256, 9257, 9265, 9273,  &
9279, 9280, 9284, 9285, 9305, 9307, 9332, 9349, 9360, 9367, 9371, 9379, 9380,  &
9382, 9386, 9390, 9400, 9421, 9427, 9435, 9437, 9440, 9448, 9450, 9471, 9475,  &
9487, 9492, 9497, 9500, 9507, 9516, 9519, 9521, 9530, 9537, 9541, 9544, 9549,  &
9562, 9586, 9594, 9600, 9605, 9610, 9614, 9617, 9630, 9640, 9649, 9656, 9663,  &
9664, 9667, 9668, 9676, 9687, 9689, 9698, 9701, 9706, 9717, 9722, 9728, 9742,  &
9744, 9750, 9753, 9775, 9785, 9787, 9793, 9797, 9828, 9833, 9835, 9842, 9850,  &
9865, 9870, 9872, 9893, 9939, 9940, 9943, 9947, 9960, 9967, 9987, 10000, 10005,  &
10012, 10013, 10020, 10033, 10059, 10064, 10065, 10070, 10073, 10083, 10087, 10088, 10095,  &
10098, 10100, 10103, 10117, 10136, 10142, 10146, 10162, 10191, 10209, 10217, 10225, 10243,  &
10244, 10247, 10263, 10264, 10265, 10296, 10311, 10324, 10336, 10337, 10352, 10361, 10375,  &
10381, 10401, 10404, 10431, 10445, 10455, 10465, 10469, 10471, 10476, 10477, 10494, 10496,  &
10500, 10504, 10507, 10509, 10517, 10531, 10535, 10544, 10547, 10549, 10560, 10579, 10580,  &
10597, 10608, 10609, 10610, 10614, 10616, 10618, 10629, 10631, 10643, 10652, 10655, 10656,  &
10659, 10661, 10680, 10688, 10690, 10693, 10704, 10705, 10711, 10714, 10723, 10725, 10733,  &
10742, 10743, 10746, 10762, 10772, 10773, 10778, 10779, 10782, 10784, 10788, 10811, 10814,  &
10816, 10822, 10828, 10838, 10845, 10847, 10850, 10859, 10869, 10877, 10879, 10880, 10885,  &
10892, 10899, 10919, 10935, 10960, 10961, 10967, 10975, 10976, 10980, 10982, 10989, 11004,  &
11007, 11017, 11022, 11026, 11028 /) )
	SELE = SELE .OR. ATOM_SELECTION( &
		SUBSYSTEM = (/ 'SOD' /), &
		RESIDUE_NUMBER = (/ 2 /) )
END SUBROUTINE
