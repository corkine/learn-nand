// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/04/Fill.asm

// Runs an infinite loop that listens to the keyboard input.
// When a key is pressed (any key), the program blackens the screen,
// i.e. writes "black" in every pixel;
// the screen should remain fully black as long as the key is pressed. 
// When no key is pressed, the program clears the screen, i.e. writes
// "white" in every pixel;
// the screen should remain fully clear as long as no key is pressed.

// Put your code here.
(LOOP)
@KBD
D=M
@DRAW_WHITE
D;JNE
@DRAW_BLACK
D;JEQ

(DRAW_BLACK)
@SCREEN
D=A
@current
M=D
@16400
D=A
(DRAW_START)
@current
A=M
M=-1
@current
M=M+1
D=D-1
@DRAW_START
D;JGT
@LOOP
0;JMP

(DRAW_WHITE)
@SCREEN
D=A
@current
M=D
@16400
D=A
(DRAW_START2)
@current
A=M
M=0
@current
M=M+1
D=D-1
@DRAW_START2
D;JGT
@LOOP
0;JMP