@256                 //0
D=A                  //1
@SP                  //2
M=D                  //3
@Sys.init$return251  //4
D=A                  //5
@SP                  //6
A=M                  //7
M=D                  //8
@SP                  //9
M=M+1                //10
@LCL                 //11
D=M                  //12
@SP                  //13
A=M                  //14
M=D                  //15
@SP                  //16
M=M+1                //17
@ARG                 //18
D=M                  //19
@SP                  //20
A=M                  //21
M=D                  //22
@SP                  //23
M=M+1                //24
@THIS                //25
D=M                  //26
@SP                  //27
A=M                  //28
M=D                  //29
@SP                  //30
M=M+1                //31
@THAT                //32
D=M                  //33
@SP                  //34
A=M                  //35
M=D                  //36
@SP                  //37
M=M+1                //38
@0                   //39
D=A                  //40
@5                   //41
D=D+A                //42
@SP                  //43
A=M                  //44
D=A-D                //45
@ARG                 //46
M=D                  //47
@SP                  //48
D=M                  //49
@LCL                 //50
M=D                  //51
@Sys.init            //52
0;JMP                //53
(Sys.init$return251)
//file: Sys.vm
//function Sys.init 0
(Sys.init)
@0                   //54
D=A                  //55
@SP                  //56
M=D+M                //57
//push constant 6
@6                   //58
D=A                  //59
@SP                  //60
A=M                  //61
M=D                  //62
@SP                  //63
M=M+1                //64
//push constant 8
@8                   //65
D=A                  //66
@SP                  //67
A=M                  //68
M=D                  //69
@SP                  //70
M=M+1                //71
//call Class1.set 2
@Class1.set$return252 //72
D=A                  //73
@SP                  //74
A=M                  //75
M=D                  //76
@SP                  //77
M=M+1                //78
@LCL                 //79
D=M                  //80
@SP                  //81
A=M                  //82
M=D                  //83
@SP                  //84
M=M+1                //85
@ARG                 //86
D=M                  //87
@SP                  //88
A=M                  //89
M=D                  //90
@SP                  //91
M=M+1                //92
@THIS                //93
D=M                  //94
@SP                  //95
A=M                  //96
M=D                  //97
@SP                  //98
M=M+1                //99
@THAT                //100
D=M                  //101
@SP                  //102
A=M                  //103
M=D                  //104
@SP                  //105
M=M+1                //106
@2                   //107
D=A                  //108
@5                   //109
D=D+A                //110
@SP                  //111
A=M                  //112
D=A-D                //113
@ARG                 //114
M=D                  //115
@SP                  //116
D=M                  //117
@LCL                 //118
M=D                  //119
@Class1.set          //120
0;JMP                //121
(Class1.set$return252)
//pop temp 0
@0                   //122
D=A                  //123
@R5                  //124
A=D+A                //125
D=A                  //126
@R13                 //127
M=D                  //128
@SP                  //129
M=M-1                //130
A=M                  //131
D=M                  //132
@R13                 //133
A=M                  //134
M=D                  //135
//push constant 23
@23                  //136
D=A                  //137
@SP                  //138
A=M                  //139
M=D                  //140
@SP                  //141
M=M+1                //142
//push constant 15
@15                  //143
D=A                  //144
@SP                  //145
A=M                  //146
M=D                  //147
@SP                  //148
M=M+1                //149
//call Class2.set 2
@Class2.set$return253 //150
D=A                  //151
@SP                  //152
A=M                  //153
M=D                  //154
@SP                  //155
M=M+1                //156
@LCL                 //157
D=M                  //158
@SP                  //159
A=M                  //160
M=D                  //161
@SP                  //162
M=M+1                //163
@ARG                 //164
D=M                  //165
@SP                  //166
A=M                  //167
M=D                  //168
@SP                  //169
M=M+1                //170
@THIS                //171
D=M                  //172
@SP                  //173
A=M                  //174
M=D                  //175
@SP                  //176
M=M+1                //177
@THAT                //178
D=M                  //179
@SP                  //180
A=M                  //181
M=D                  //182
@SP                  //183
M=M+1                //184
@2                   //185
D=A                  //186
@5                   //187
D=D+A                //188
@SP                  //189
A=M                  //190
D=A-D                //191
@ARG                 //192
M=D                  //193
@SP                  //194
D=M                  //195
@LCL                 //196
M=D                  //197
@Class2.set          //198
0;JMP                //199
(Class2.set$return253)
//pop temp 0
@0                   //200
D=A                  //201
@R5                  //202
A=D+A                //203
D=A                  //204
@R13                 //205
M=D                  //206
@SP                  //207
M=M-1                //208
A=M                  //209
D=M                  //210
@R13                 //211
A=M                  //212
M=D                  //213
//call Class1.get 0
@Class1.get$return254 //214
D=A                  //215
@SP                  //216
A=M                  //217
M=D                  //218
@SP                  //219
M=M+1                //220
@LCL                 //221
D=M                  //222
@SP                  //223
A=M                  //224
M=D                  //225
@SP                  //226
M=M+1                //227
@ARG                 //228
D=M                  //229
@SP                  //230
A=M                  //231
M=D                  //232
@SP                  //233
M=M+1                //234
@THIS                //235
D=M                  //236
@SP                  //237
A=M                  //238
M=D                  //239
@SP                  //240
M=M+1                //241
@THAT                //242
D=M                  //243
@SP                  //244
A=M                  //245
M=D                  //246
@SP                  //247
M=M+1                //248
@0                   //249
D=A                  //250
@5                   //251
D=D+A                //252
@SP                  //253
A=M                  //254
D=A-D                //255
@ARG                 //256
M=D                  //257
@SP                  //258
D=M                  //259
@LCL                 //260
M=D                  //261
@Class1.get          //262
0;JMP                //263
(Class1.get$return254)
//call Class2.get 0
@Class2.get$return255 //264
D=A                  //265
@SP                  //266
A=M                  //267
M=D                  //268
@SP                  //269
M=M+1                //270
@LCL                 //271
D=M                  //272
@SP                  //273
A=M                  //274
M=D                  //275
@SP                  //276
M=M+1                //277
@ARG                 //278
D=M                  //279
@SP                  //280
A=M                  //281
M=D                  //282
@SP                  //283
M=M+1                //284
@THIS                //285
D=M                  //286
@SP                  //287
A=M                  //288
M=D                  //289
@SP                  //290
M=M+1                //291
@THAT                //292
D=M                  //293
@SP                  //294
A=M                  //295
M=D                  //296
@SP                  //297
M=M+1                //298
@0                   //299
D=A                  //300
@5                   //301
D=D+A                //302
@SP                  //303
A=M                  //304
D=A-D                //305
@ARG                 //306
M=D                  //307
@SP                  //308
D=M                  //309
@LCL                 //310
M=D                  //311
@Class2.get          //312
0;JMP                //313
(Class2.get$return255)
//label WHILE
(Sys.init$WHILE)
//goto WHILE
@Sys.init$WHILE      //314
0;JMP                //315
//file: Class1.vm
//function Class1.set 0
(Class1.set)
@0                   //316
D=A                  //317
@SP                  //318
M=D+M                //319
//push argument 0
@0                   //320
D=A                  //321
@ARG                 //322
A=M                  //323
A=D+A                //324
D=M                  //325
@SP                  //326
A=M                  //327
M=D                  //328
@SP                  //329
M=M+1                //330
//pop static 0
@SP                  //331
M=M-1                //332
A=M                  //333
D=M                  //334
@Class1.0            //335
M=D                  //336
//push argument 1
@1                   //337
D=A                  //338
@ARG                 //339
A=M                  //340
A=D+A                //341
D=M                  //342
@SP                  //343
A=M                  //344
M=D                  //345
@SP                  //346
M=M+1                //347
//pop static 1
@SP                  //348
M=M-1                //349
A=M                  //350
D=M                  //351
@Class1.1            //352
M=D                  //353
//push constant 0
@0                   //354
D=A                  //355
@SP                  //356
A=M                  //357
M=D                  //358
@SP                  //359
M=M+1                //360
//return
@LCL                 //361
D=M                  //362
@R13                 //363
M=D                  //364
@5                   //365
D=A                  //366
@R13                 //367
A=M-D                //368
D=M                  //369
@R14                 //370
M=D                  //371
@SP                  //372
M=M-1                //373
A=M                  //374
D=M                  //375
@ARG                 //376
A=M                  //377
M=D                  //378
@ARG                 //379
D=M+1                //380
@SP                  //381
M=D                  //382
@R13                 //383
D=M                  //384
@1                   //385
A=D-A                //386
D=M                  //387
@THAT                //388
M=D                  //389
@R13                 //390
D=M                  //391
@2                   //392
A=D-A                //393
D=M                  //394
@THIS                //395
M=D                  //396
@R13                 //397
D=M                  //398
@3                   //399
A=D-A                //400
D=M                  //401
@ARG                 //402
M=D                  //403
@R13                 //404
D=M                  //405
@4                   //406
A=D-A                //407
D=M                  //408
@LCL                 //409
M=D                  //410
// 6-- go to return address
@R14                 //411
A=M                  //412
0;JEQ                //413
//function Class1.get 0
(Class1.get)
@0                   //414
D=A                  //415
@SP                  //416
M=D+M                //417
//push static 0
@Class1.0            //418
D=M                  //419
@SP                  //420
A=M                  //421
M=D                  //422
@SP                  //423
M=M+1                //424
//push static 1
@Class1.1            //425
D=M                  //426
@SP                  //427
A=M                  //428
M=D                  //429
@SP                  //430
M=M+1                //431
//sub
@SP                  //432
M=M-1                //433
A=M                  //434
D=M                  //435
A=A-1                //436
M=M-D                //437
//return
@LCL                 //438
D=M                  //439
@R13                 //440
M=D                  //441
@5                   //442
D=A                  //443
@R13                 //444
A=M-D                //445
D=M                  //446
@R14                 //447
M=D                  //448
@SP                  //449
M=M-1                //450
A=M                  //451
D=M                  //452
@ARG                 //453
A=M                  //454
M=D                  //455
@ARG                 //456
D=M+1                //457
@SP                  //458
M=D                  //459
@R13                 //460
D=M                  //461
@1                   //462
A=D-A                //463
D=M                  //464
@THAT                //465
M=D                  //466
@R13                 //467
D=M                  //468
@2                   //469
A=D-A                //470
D=M                  //471
@THIS                //472
M=D                  //473
@R13                 //474
D=M                  //475
@3                   //476
A=D-A                //477
D=M                  //478
@ARG                 //479
M=D                  //480
@R13                 //481
D=M                  //482
@4                   //483
A=D-A                //484
D=M                  //485
@LCL                 //486
M=D                  //487
// 6-- go to return address
@R14                 //488
A=M                  //489
0;JEQ                //490
//file: Class2.vm
//function Class2.set 0
(Class2.set)
@0                   //491
D=A                  //492
@SP                  //493
M=D+M                //494
//push argument 0
@0                   //495
D=A                  //496
@ARG                 //497
A=M                  //498
A=D+A                //499
D=M                  //500
@SP                  //501
A=M                  //502
M=D                  //503
@SP                  //504
M=M+1                //505
//pop static 0
@SP                  //506
M=M-1                //507
A=M                  //508
D=M                  //509
@Class2.0            //510
M=D                  //511
//push argument 1
@1                   //512
D=A                  //513
@ARG                 //514
A=M                  //515
A=D+A                //516
D=M                  //517
@SP                  //518
A=M                  //519
M=D                  //520
@SP                  //521
M=M+1                //522
//pop static 1
@SP                  //523
M=M-1                //524
A=M                  //525
D=M                  //526
@Class2.1            //527
M=D                  //528
//push constant 0
@0                   //529
D=A                  //530
@SP                  //531
A=M                  //532
M=D                  //533
@SP                  //534
M=M+1                //535
//return
@LCL                 //536
D=M                  //537
@R13                 //538
M=D                  //539
@5                   //540
D=A                  //541
@R13                 //542
A=M-D                //543
D=M                  //544
@R14                 //545
M=D                  //546
@SP                  //547
M=M-1                //548
A=M                  //549
D=M                  //550
@ARG                 //551
A=M                  //552
M=D                  //553
@ARG                 //554
D=M+1                //555
@SP                  //556
M=D                  //557
@R13                 //558
D=M                  //559
@1                   //560
A=D-A                //561
D=M                  //562
@THAT                //563
M=D                  //564
@R13                 //565
D=M                  //566
@2                   //567
A=D-A                //568
D=M                  //569
@THIS                //570
M=D                  //571
@R13                 //572
D=M                  //573
@3                   //574
A=D-A                //575
D=M                  //576
@ARG                 //577
M=D                  //578
@R13                 //579
D=M                  //580
@4                   //581
A=D-A                //582
D=M                  //583
@LCL                 //584
M=D                  //585
// 6-- go to return address
@R14                 //586
A=M                  //587
0;JEQ                //588
//function Class2.get 0
(Class2.get)
@0                   //589
D=A                  //590
@SP                  //591
M=D+M                //592
//push static 0
@Class2.0            //593
D=M                  //594
@SP                  //595
A=M                  //596
M=D                  //597
@SP                  //598
M=M+1                //599
//push static 1
@Class2.1            //600
D=M                  //601
@SP                  //602
A=M                  //603
M=D                  //604
@SP                  //605
M=M+1                //606
//sub
@SP                  //607
M=M-1                //608
A=M                  //609
D=M                  //610
A=A-1                //611
M=M-D                //612
//return
@LCL                 //613
D=M                  //614
@R13                 //615
M=D                  //616
@5                   //617
D=A                  //618
@R13                 //619
A=M-D                //620
D=M                  //621
@R14                 //622
M=D                  //623
@SP                  //624
M=M-1                //625
A=M                  //626
D=M                  //627
@ARG                 //628
A=M                  //629
M=D                  //630
@ARG                 //631
D=M+1                //632
@SP                  //633
M=D                  //634
@R13                 //635
D=M                  //636
@1                   //637
A=D-A                //638
D=M                  //639
@THAT                //640
M=D                  //641
@R13                 //642
D=M                  //643
@2                   //644
A=D-A                //645
D=M                  //646
@THIS                //647
M=D                  //648
@R13                 //649
D=M                  //650
@3                   //651
A=D-A                //652
D=M                  //653
@ARG                 //654
M=D                  //655
@R13                 //656
D=M                  //657
@4                   //658
A=D-A                //659
D=M                  //660
@LCL                 //661
M=D                  //662
// 6-- go to return address
@R14                 //663
A=M                  //664
0;JEQ                //665