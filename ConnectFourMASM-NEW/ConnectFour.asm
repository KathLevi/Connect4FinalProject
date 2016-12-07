TITLE Connect Four Assembler Code

;Program Description: Functions written in assembler that will be linked to our C++ main function
;Author:			Kathleen, Andrew, Ryan, Jusice
;Creation Date:               12/01/2016
;Latest Revision:			  12/06/2016
 
; 32-bit assembly language template

INCLUDE Irvine32.inc

;----------------------------DATA----------------------------------------
.data

p1Prompt BYTE "Player 1: what is your name? ",0
p2Prompt BYTE "Player 2: what is your name? ",0
p1Name DWORD 80 DUP(0)
p2Name DWORD 80 DUP(0)

MM BYTE 1
MMtitle BYTE "MAIN MENU",0
MMplay BYTE "1) Play game",0
MMinstructions BYTE "2) Instructions",0
MMstats BYTE "3) See stats",0
MMexit BYTE "4) Exit",0
MMprompt BYTE "Please chose a valid value from the main menu (1-4)",0
MMinvalid BYTE "Not a valid option, please enter a value 1 - 4",0
instruct BYTE "Be the first to place 4 tiles in a row! Switch off between player 1 and 2 until someone wins or the board fills up.",0
instructionsTitle BYTE "INSTRUCTIONS",0

anyKey BYTE "Press any key to continue...",0

board DWORD 42 DUP(?)	;allocates space for a 6x7 board
column BYTE 0
row BYTE 0
blank BYTE " ",0
redBlue BYTE 14h		;red text on blue background
yellowBlue BYTE 30h		;yellow text on blue background
whiteBlue BYTE 31h		;white text on blue background
blueBlue BYTE 17h		;blue text on blue background

scorePrompt BYTE " score is ",0
p1Score DWORD 0
p2Score DWORD 0
againUser BYTE 49
playAgain BYTE "Would you like to play again? Enter 1 for yes and 0 for no: ",0
endGameMsg BYTE "Thank you for playing connect four, come back soon!",0

;-------------------------------MAIN FUNCTION------------------------------------- 
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
	call again						;sees if the user wants to play again
	cmp againUser, 49
	je BeginGame					;if yes, start the game over, if no then continue and thank the user for playing
EndGame:
	call Clrscr
	mov edx, offset endGameMsg		;ends program and thanks user for playing
	call WriteString
	call Crlf
	mov edx, offset anyKey
	call WriteString
	call ReadChar
	call exitProcess
main endp

;---------------------------------FUNCTION TO ASK USER NAME-----------------------------------
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
mainMenu PROC USES EDX EAX
	call Clrscr
	mov edx, offset MMtitle			;title
	call WriteString
	call Crlf
	mov edx, offset MMplay			;play
	call WriteString
	call Crlf
	mov edx, offset MMinstructions	;instructions
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
instructions PROC USES EDX EAX
	call Clrscr
	mov edx, offset instructionsTitle		;display instruction title
	call WriteString
	call Crlf
	
	mov edx, offset instruct				;display instructions
	call WriteString
	call Crlf
	call Crlf
	
	mov edx, offset anyKey
	call WriteString
	call ReadChar

	ret
instructions endp

;---------------------------------FUNCTION TO DISPLAY STATS-----------------------------------
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

	mov edx, offset anyKey
	call WriteString
	call ReadChar

    ret
displayStats endp


;---------------------------------FUNCTION TO PLAY GAME-----------------------------------
playGame PROC
	Call clrscr


	ret
playGame endp

;---------------------------------FUNCTION TO ASK USER IF THEY WANT TO PLAY AGAIN-----------------------------------
again PROC USES ECX EDX EAX
	mov edx, offset playAgain
	call WriteString
	xor eax, eax
	call ReadChar
	mov againUser, al

	ret
again endp
end main