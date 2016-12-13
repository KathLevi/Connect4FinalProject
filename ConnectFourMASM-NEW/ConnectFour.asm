TITLE Connect Four Assembler Code

;Program Description: Connect Four written entirely in assembler, algorithms and all
;Author:			Kathleen, Andrew, Ryan, Jusice
;Creation Date:               12/01/2016
;Latest Revision:			  12/12/2016
 
; 32-bit assembly language template

INCLUDE Irvine32.inc

;----------------------------DATA----------------------------------------
.data

p1Prompt BYTE "Player 1: what is your name? ",0		;ask players names
p2Prompt BYTE "Player 2: what is your name? ",0
p1Name DWORD 80 DUP(0)					;holds player names
p2Name DWORD 80 DUP(0)

MM BYTE 1								;main menu user input
MMtitle BYTE "MAIN MENU",0				;main menu prompts and options
MMplay BYTE "1) Play game",0
MMinstructions BYTE "2) Instructions",0
MMstats BYTE "3) See stats",0
MMexit BYTE "4) Exit",0
MMprompt BYTE "Please chose a valid value from the main menu (1-4)",0
MMinvalid BYTE "Not a valid option, please enter a value 1 - 4",0		;if user chooses invalid menu input ask to try again
instruct BYTE "Be the first to place 4 tiles in a row! Switch off between player 1 and 2 until someone wins or the board fills up.",0
instructionsTitle BYTE "INSTRUCTIONS",0

anyKey BYTE "Press any key to continue...",0			;used for sysPause function similar to system("pause") in c++
colNumbers BYTE " 1   2   3   4   5   6   7",0

board DWORD 42 DUP (?)	;allocates space for a 6x7 board	;array for board
i DWORD 0							;used for display board function to keep track of the outside loop
blank DWORD " ",0						;blank space for display board
inarow BYTE 1							;keep track of the number of tiles in a row for check function
col BYTE ?							;separate col variable for check function
row BYTE ?							;separate row variable for check function
seven DWORD 7							;used for mul when accessing array
four DWORD 4							;used for mul when accessing array
validPlace DWORD ?						;used in place tile to ensure validity of column

colChoice DWORD 0						;users column choice for placing tile
curCol DWORD 0							;current column the program is on
placed DWORD 0							;keeps track of number of tiles places and when places == 42 game board is full
tileDropPrompt BYTE ", where would you like to drop your tile? (Choose columns 1 - 7)",0	;prompt to see where user wants to drop tilw
invalidCol BYTE "Column is full, please choose a different column"		;full column prompt
invalidB BYTE "Please enter a value between 1 and 7: "				;if colChoice is not between 1 and 7, try again

win BYTE 48				;win is going to be a kind of boolean set to either 1 or 0 in ascii
player DWORD 40h			;player 1 is 40h, player 2 is 23h
winnerTxt BYTE " won!",0	;playerX won!
drawTxt BYTE "It's a draw!",0	;its a tie!

scorePrompt BYTE " score is ",0		;display player score
p1Score DWORD 0		;holds scores of player 1
p2Score DWORD 0		;holds scores of player 2
againUser BYTE 49	;againUser is a aboolean that is either 1 or 0 in ascii
playAgain BYTE "Would you like to play again? Enter 1 for yes and 0 for no: ",0		;again? prompt
endGameMsg BYTE "Thank you for playing connect four, come back soon!",0			;thanks for coming prompt

;-------------------------------MAIN FUNCTION-------------------------------------
;executes at runtime
.code
main proc
	call askName				;ask users names at beginning and then not again while program is running

BeginGame:
	call mainMenu				;display main menu

	cmp MM, 49					;if MM == 1 play the game
		je Play
	cmp	MM, 52					;if MM == 4 end the game
		je EndGame	
	cmp MM, 50					;if MM == 2 give instructions and jmp to beginning 
		jne Next
		call instructions
		jmp BeginGame
	Next:
	cmp MM, 51					;if MM == 3 display stats and jmp to beginning 
		jne Next1
		call displayStats
		jmp BeginGame
	Next1:
		mov edx, offset MMinvalid		;if none of the above are called, then display invalidMM and jmp to beginning 
		call WriteString
	jmp BeginGame				
	

Play:
	call playGame
	
	mov eax, white+(black*16)			;reset screen colors to out of game mode
	call settextcolor
	call Clrscr

	call again						;sees if the user wants to play again
	cmp againUser, 49
	je BeginGame					;if yes, start the game over, if no then continue and thank the user for playing
EndGame:
	call Clrscr
	mov edx, offset endGameMsg		;ends program and thanks user for playing
	call WriteString
	call Crlf
	call sysPause
	
	call exitProcess
main endp

;---------------------------------FUNCTION TO ASK USER NAME-----------------------------------
;function executes at runtime
askName PROC USES EAX EDX ECX
	mov edx, offset p1Prompt		;asks P1 name
	call WriteString
	mov ecx, 80
	mov edx, offset p1Name			;reads P1 name
	call ReadString
	call Crlf

	mov edx, offset p2Prompt		;asks P2 name
	call Writestring
	mov ecx, 80
	mov edx, offset p2Name			;reads P2 name
	call ReadString
	call Clrscr

	ret
askName endp

;---------------------------------FUNCTION TO DISPLAY MAIN MENU-----------------------------------
;function executes at runtime or when user enters a 2 or 3, and after every game if the user chooses to play again
mainMenu PROC USES EDX EAX
	call Clrscr
	mov edx, offset MMtitle			;title
	call WriteString
	call Crlf
	mov edx, offset MMplay			;play
	call WriteString
	call Crlf
	mov edx, offset MMinstructions		;instructions
	call WriteString
	call Crlf
	mov edx, offset MMstats			;stats
	call WriteString
	call Crlf
	mov edx, offset MMexit			;exit
	call WriteString
	call Crlf
	call Crlf
	mov edx, offset MMprompt		;prompt
	call WriteString

	xor eax, eax
	call ReadChar
	mov MM, al

	ret
mainMenu endp

;---------------------------------FUNCTION FOR INSTRUCTIONS-----------------------------------
;user must enter a 2 in main menu function
instructions PROC USES EDX EAX
	call Clrscr
	mov edx, offset instructionsTitle		;display instruction title
	call WriteString
	call Crlf
	
	mov edx, offset instruct				;display instructions
	call WriteString
	call Crlf
	call Crlf
	
	call sysPause

	ret
instructions endp

;---------------------------------FUNCTION TO DISPLAY STATS-----------------------------------
;user must enter 3 in main menu function
displayStats PROC USES EDX EAX	
	call Clrscr

	mov edx, offset p1Name			;name of player --> scorePrompt --> score
	call WriteString
	mov edx, offset scorePrompt
	call WriteString
	mov eax, p1Score
	call WriteInt
	call Crlf

	mov edx, offset p2Name
	call WriteString
	mov edx, offset scorePrompt
	call WriteString
	mov eax, p2Score
	call WriteInt
	call Crlf

	call sysPause

    ret
displayStats endp

;---------------------------------FUNCTION TO PLAY GAME-----------------------------------
;user must enter 1 in main menu function
playGame PROC USES EDX EAX
	mov placed, 0
	mov win, 48
	call emptyBoard
	call displayBoard
Game:
	cmp curCol, -1
	je ChooseRow
		cmp player, 40h
		jne elseP2
			mov edx, offset p1Name
			call WriteString
			mov edx, offset tileDropPrompt
			call WriteString
			mov player, 23h
			jmp ChooseRow
		elseP2:
			mov edx, offset p2Name
			call WriteString
			mov edx, offset tileDropPrompt
			call WriteString
			mov player, 40h
	ChooseRow:
		cmp placed, 42		;if board is full then game over
		je Place
			call ReadInt	;otherwise read in char from user
			mov colChoice, eax	
			dec colChoice
			cmp colChoice, 7	;ensure row choice is valid
			jb Place
				mov edx, offset invalidB
				call WriteString
	jmp ChooseRow		;while (true)
	Place:
	cmp placed, 42		;if the board is full game over
		je GameOver
	call placeTile		;otherwise call placeTile to set curCol
	cmp curCol, -1
		jne checkWin		;if curCol is -1 then the column is full
		mov edx, offset invalidCol
		call WriteString
		jmp ChooseRow
	checkWin:				;otherwise check for a win
		cmp placed, 6
		jb Enough
			call check		;check for a win and change the value of win if someone won to exit loop
		Enough:
		inc placed
		call displayBoard
cmp win, 48
je Game						

GameOver:
	call updateStats			;announces winner and updates scores
	call crlf
	call sysPause				;system("Pause")

	ret
playGame endp

;-------------------------------FUNCTION TO EMPTY BOARD-------------------------------------
;empty at begining of game only before anyone places a tile
emptyBoard PROC USES ECX EAX EBX ESI					
	mov esi, offset board
	mov ebx, blank
	xor eax, eax
   	mov ecx, 42
    Loop1:
		mov [esi + eax * 4], ebx
		inc eax
	loop Loop1

	ret
emptyBoard endp

;---------------------------------FUNCTION TO DISPLAY BOARD----------------------------------- 
;display board at beginning and end of game and any time a change is made to the state of the baord (e.g. player drops a tile)
displayBoard PROC USES EAX ECX
	mov eax, white+(blue*16)
	call settextcolor
	call Clrscr
	
	mov i, 0
	mov ecx, 6
	Loop4:
		call setRow
	loop Loop4 
	mov edx, offset colNumbers
	call Writestring
	call crlf
	ret
displayBoard endp

;-------------------------------FUNCTION TO CREATE A ROW OF THE BOARD-------------------------------------
;called everytime display board is called
setRow PROC USES EAX ECX ESI           ;outputs a row of boxes using ascii characters
	mov ecx, 7
	Loop1:                                          ;ouputs first third of a row of 7 boxes
		mov al, 218
		call Writechar
		mov al, 196
		call Writechar
		mov al, 191
		call Writechar
		mov al, BYTE PTR blank
		call Writechar
	loop Loop1
	call crlf
 
	mov ecx, 7
	Loop2:                                          ;ouputs second third of a row of 7 boxes
		mov al, 179
		call Writechar
		xor eax, eax
		mov esi, i
		mov eax, board[esi * TYPE board]						
		call Writechar
		inc i
		mov al, 179
		call Writechar
		mov al, BYTE PTR blank
		call Writechar
	loop Loop2
	call crlf
 
	mov ecx, 7
	Loop3:                                          ;ouputs last third of a row of 7 boxes
		mov al, 192
		call Writechar
		mov al, 196
		call Writechar
		mov al, 217
		call Writechar
		mov al, BYTE PTR blank
		call Writechar
	loop Loop3
	call crlf
	ret
setRow endp

;--------------------------------FUNCTION TO PLACE TILE------------------------------------
;placce tile is called when the user choses a column IF placed != 42 and IF colChoice is valid (between 0 and 7)
placeTile PROC USES EAX EBX ECX EDX ESI
	mov validPlace, 0
	mov eax, colChoice
	.IF board[eax * TYPE board] == " "
		.WHILE validPlace < 6
			xor eax, eax
			mov eax, validPlace
			mul seven
			add eax, colChoice
			.IF board[eax * TYPE board] == " "			
				inc validPlace
			.ELSE
				jmp downAndOut
			.ENDIF
		.ENDW	
		
		.IF validPlace == 5
			inc validPlace
		.ENDIF
	
	downAndOut:	
		dec validPlace
		xor eax, eax
		mov eax, validPlace
		mul seven
		add eax, colChoice
		mov ebx, player
		mov esi, offset board
		mov [esi + eax * TYPE board], ebx
		mov edx, validPlace
		mov curCol, edx
		ret
			
	.ENDIF
	mov curCol, -1
	ret
placeTile endp

;---------------------------------FUNCTION TO CHECK FOR A WINNER-----------------------------------
;once placed > 6 everytime a tile is placed the function is called as long as placed != 42 and curCol != -1 (meaning the curCol is full)
check PROC USES EAX EBX EDX
	mov al, BYTE PTR colChoice
	mov col, al
	mov al, BYTE PTR curCol
	mov row, al

	;check horizontal
	;go left
	.WHILE col > 0
		movzx eax,row
		mul seven
		dec col
		add al,col
		inc col
		mov edx, board[eax * 4]
		.IF player == edx
			inc inarow
			.IF inarow == 4
				mov win,49
				ret
			.ENDIF
		.ELSE
			jmp next1
		.ENDIF
		dec col
	.ENDW
	next1:
	mov al, BYTE PTR colChoice
	mov col,al

	;go right
	.WHILE col < 6
		movzx eax,row
		mul seven
		inc col
		add al,col
		mov edx, board[eax * 4]
		.IF player == edx
			inc inarow
			.IF inarow == 4
				mov win,49
				ret
			.ENDIF
		.ELSE
			jmp next2
		.ENDIF
		inc col
	.ENDW
	next2:
	mov inarow,1
	mov al, BYTE PTR colChoice
	mov col,al

	;check vertical
	;go up
	.WHILE row > 0
		dec row
		movzx eax,row
		inc row
		mul seven
		add al,col
		mov edx,board[eax * 4]
		.IF player == edx
			inc inarow
			.IF inarow == 4
				mov win,49
				ret
			.ENDIF
		.ELSE
			jmp next3
		.ENDIF
		dec row
	.ENDW
	next3:
	mov al, BYTE PTR curCol
	mov row,al

	;go down
	.WHILE row < 5
		inc row
		movzx eax,row
		dec row
		mul seven
		add al,col
		mov edx,board[eax * 4]
		.IF player == edx
			inc inarow
			.IF inarow ==4
				mov win,49
				ret
			.ENDIF
		.ELSE
			jmp next4
		.ENDIF
		inc row
	.ENDW
	next4:
	mov inarow,1
	mov al, BYTE PTR curCol
	mov row,al

	;check diagonally (/)
	;go up-right
	.WHILE col < 6 && row >0
		dec row
		movzx eax,row
		inc row
		mul seven
		inc col
		add al,col
		dec col
		mov edx,board[eax * 4]
		.IF player == edx
			inc inarow
			.IF inarow == 4
				mov win,49
				ret
			.ENDIF
		.ELSE
			jmp next5
		.ENDIF
		dec row
		inc col
	.ENDW
	next5:
	mov al, BYTE PTR curCol
	mov row,al
	mov al,BYTE PTR colChoice
	mov col,al

	;go down-left
	.WHILE col > 0 && row < 5
		inc row
		movzx eax,row
		dec row
		mul seven
		dec col
		add al,col
		inc col
		mov edx,board[eax * 4]
		.IF player == edx
			inc inarow
			.IF inarow ==4
				mov win,49
				ret
			.ENDIF
		.ELSE
			jmp next6
		.ENDIF
		inc row
		dec col
	.ENDW
	next6:
	mov inarow,1
	mov al, BYTE PTR curCol
	mov row,al
	mov al, BYTE PTR colChoice
	mov col,al

	;check diagonally (\)
	;go up-left
	.WHILE col > 0 && row > 0
		dec row
		movzx eax,row
		inc row
		mul seven
		dec col
		add al,col
		inc col
		mov edx,board[eax * 4]
		.IF player == edx
			inc inarow
			.IF inarow ==4
				mov win,49
				ret
			.ENDIF
		.ELSE
			jmp next7
		.ENDIF
		dec row
		dec col
	.ENDW
	next7:
	mov al, BYTE PTR curCol
	mov row,al
	mov al, BYTE PTR colChoice
	mov col,al

	;go down-right
	.WHILE col < 6 && row < 5
		inc row
		movzx eax,row
		dec row
		mul seven
		inc col
		add al,col
		dec col
		mov edx,board[eax * 4]
		.IF player == edx
			inc inarow
			.IF inarow ==4
				mov win,49
				ret
			.ENDIF
		.ELSE
			jmp next8
		.ENDIF
		inc row
		inc col
	.ENDW
	next8:
	mov inarow,1
	mov al, BYTE PTR curCol
	mov row,al
	mov al, BYTE PTR colChoice
	mov col,al
    ret
check endp

;---------------------------------FUNCTION TO UPDATE STATS-----------------------------------
;called every time a game ends after the final board is displayed
updateStats PROC USES EDX
	cmp placed, 42					;if the board is full then its a draw
	jne Next
		mov edx, offset drawTxt		
		call Writestring
		mov edx, offset anyKey		;similar to system("pause") in c++
		call WriteString
		call ReadChar
		jmp statEnd
Next: 
	cmp player, 40h			;if player == 40h then player 2 won
	jne IfElse
		mov edx, offset p2Name
		call WriteString
		mov edx, offset winnerTxt		
		call Writestring
		inc p2Score
		jmp statEnd
	IfElse:					;otherwise player 1 wins
		mov edx, offset p1Name
		call WriteString
		mov edx, offset winnerTxt		
		call Writestring
		inc p1Score
statEnd:
    ret
updateStats endp

;---------------------------------FUNCTION TO ASK USER IF THEY WANT TO PLAY AGAIN-----------------------------------
;called at the end of game once a winner is announced
again PROC USES EDX EAX
	mov edx, offset playAgain
	call WriteString
	xor eax, eax
	call ReadChar
	mov againUser, al

	ret
again endp

;---------------------------------FUNCTION THAT MIMICS SYSTEM("PAUSE") IN C++-----------------------------------
;called at random points within the code, mostly when a message is the last thing to be displayed before clearing the screen or exiting the game 
sysPause PROC USES EDX
	mov edx, offset anyKey		
	call WriteString
	call ReadChar

	ret
sysPause endp
end main
