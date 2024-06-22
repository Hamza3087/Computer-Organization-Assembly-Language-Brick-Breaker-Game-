comment!
MEMBER 1 : HAMZA TARIQ(I21-0707)
MEMBER 2 : AHMED DAWOOD AKRAM(I21-0615)
!

;--------------------------------------------------------------------------------
.model small
.stack 0100h

BRICKS struct 
	
	

BRICKS ends

.data

	PAUSEE db "GAME PAUSED ",'$'
	PAUSEER db "           ",'$'
	BALL_X DW 150                        ;current X position (column) of the ball
	BALL_Y DW 100                        ;current Y position (line) of the ball                     
	STRIKER_X DW 50
	STRIKER_X_ENDING dw 100
	STRIKER_Y DW 196
	STRIKER_Y_Temp DW 194
	BALL_VELOCITY_X DW 0               ;X (horizontal) velocity of the ball
	BALL_VELOCITY_Y DW 1               ;Y (vertical) velocity of the ball
	UPPER_BOUNDARY DW 40
	LEFT_BOUNDARY DW 5
	RIGHT_BOUNDARY DW 315
	LIVES DW 3
	Sco db "SCORE : ","$"
	S Dw 0	; SCORE
	sp_x dw 120
	sp_y dw 100
	SpecialLength dw 0
	count db (0)
	
	
	S1 db (0)
	s2 db (0)
	s3 db (0)
	
	S_F dw 0
	
	var db 6
	length_cx dw 100
		block_c dw 0
		block_d dw 0
		block_a dw 0
		block_b dw 0
		vR_C db 0Ah
	x dw 0
	y dw 0
	begin db 0
	colour db 0
	ch_c db 0
	string db "GAME OVER!!",'$'
	stringc db "		  CONGRATULATION'S YOU WON!!",'$'
	check2 db (0)
	
	strings db "-->   BRICK BREAKER      "
	stringn db "BRICK BREAKER"
	string1 db "PRESS ENTER TO CONTINUE..."
	Stringinput db "--> ENTER YOUR NAME: "
	ngame db "1.NEW GAME"
	rgame db "2.HIGH SCORE"
	inst db "3.INSTRUCTIONS"
	ex db "5.EXIT"
	Nam db 6 dup (0)
	var_B dW (0)
	levels db (0)
	skip db 10
	h_s db "--------------------------------- SCORE DISPLAY --------------------------------","$"
	temp db (0)
	
	BS dw 0
	BE dw 0
	checkexcept dw 0
	stringdisp db "------> SELECT LEVEL <------","$"
	stringlvl1 db "------> PRESS 1 FOR LEVEL 1","$"
	stringlvl2 db "------> PRESS 2 FOR LEVEL 2","$"
	stringlvl3 db "------> PRESS 3 FOR LEVEL 3","$"
	special db (0)
	colo db 6
	
	
	DIFFICULTY dw ?
	lvl1 db " LEVEL ","$"
	Bakwasecp db 0
	
	
	
	
;----------------------------------------------------------------------------

stringinst db "--------------------------------- INSTRUCTIONS ---------------------------------","$"
	stringdisplay db "------> WELCOME TO BRICK BREAKER GAME <------","$"
	inst1 db "-) Move Paddle With Left and Right Arrow Keys To Hit The Ball","$"
	inst2 db "-)The Game Objective Is To Eliminate All Bricks By Hitting Them With Ball","$"
	inst3 db "-)The Game Provides You With 3 Lives Every TIme Ball Hits Bottom You Lose a Life","$"
	inst4 db "-)Every TIme Ball Hits The Bottom You Lose a Life","$"
	inst5 db "-)There Are 3 Different Difficulties Levels","$"
	inst6 db "-)Press ESC To Exit The Game","$"
	inst7 db "PRESS ENTER TO RETURN","$"
	inst77 db "PRESS ESC TO RETURN","$"
	
	stringinstl db "--------------------------------------------------------------------------------","$"
	
;----------------------------------------------------------------------------
	
	endgame db (0)
	f1 db "file.txt", 0
	destroy db 11 dup ('$')
	fileinfo dw 0
	
	disp1 db "NAME : ","$"
	disp2 db "LEVEL : ","$"
	disp3 db "SCORE : ","$"
	
.code
call Main


levelSelect proc

mov ah,00
		mov al,03
		int 10h
		
		mov ah,09h
		mov bh, 00h
		mov al, 20h
		mov cx,1000h
		mov bl, 30h  ; This is Blue & Black.
		int 10h
		
		mov ah,02h
		mov bx,0
		mov dh,3
		mov dl,18
		mov al,3
		int 10h

		
		lea dx,offset stringdisplay
		mov ah,09h
		int 21h

;=======================================================


		mov ah,02h
		mov bx,0
		mov dh,6
		mov dl,27
		mov al,3
		int 10h

		
		lea dx,offset stringdisp
		mov ah,09h
		int 21h
		
;--------------------------------------------------------
;levels input display 	
		mov ah,02h
		mov bx,0
		mov dh,10
		mov dl,25
		mov al,3
		int 10h

		
		lea dx,offset stringlvl1
		mov ah,09h
		int 21h
		
		mov ah,02h
		mov bx,0
		mov dh,12
		mov dl,25
		mov al,3
		int 10h

		
		lea dx,offset stringlvl2
		mov ah,09h
		int 21h
		
		
		mov ah,02h
		mov bx,0
		mov dh,14
		mov dl,25
		mov al,3
		int 10h

		
		lea dx,offset stringlvl3
		mov ah,09h
		int 21h

;=======================================================			
;taking input and comparing for the levels
ln1:
			mov ah,00h
			int 16h
			mov levels,al
					
			cmp al,49
			je n
			
			cmp al,50
			je i
			
			cmp al,51
			je j
			
			cmp al,27
			je m
			
jmp ln1

		n:
			ret
		i:	
			;mov BALL_VELOCITY_X,2
			;mov BALL_VELOCITY_Y,2
			ret
		j:
			;mov BALL_VELOCITY_X,1
			;mov BALL_VELOCITY_Y,1
			ret
		m:
			ret




levelSelect endp

;print boundaries on the corners of the dispay screen

printboundries proc

	push ax
	push bx
	push cx
	push dx

	mov cx,0
	mov dx,199

	mov var_B,0

	l1:
		cmp var_B,319
		je exij

		mov ah,0ch
		mov al,0ch
		int 10h

		add var_B,1
		add cx,1

	jmp l1

	exij:

	mov cx,0
	mov dx,0
	mov var_B,0

	l2:
		cmp var_B,199
		je exiii

		mov ah,0ch
		mov al,0ch
		int 10h

		add var_B,1
		add dx,1

	jmp l2

	exiii:

	mov cx,0
	mov dx,0
	mov var_B,0

	l3:
		cmp var_B,319
		je exi

		mov ah,0ch
		mov al,0ch
		int 10h

		add var_B,1
		add cx,1

	jmp l3

	exi:

		mov cx,319
		mov dx,0
		mov var_B,0

	l4:
		cmp var_B,199
		je e

		mov ah,0ch
		mov al,0ch
		int 10h

		add var_B,1
		add dx,1

	jmp l4

	e:

	pop cx
	pop bx
	pop ax
	pop dx

	ret

printboundries endp

; print lives through live variable
printlives proc

	mov ah,02h
	mov bx,0
	mov dh,0
	mov dl,156
	int 10h
	
	push cx
;check the number of lives and print hearts
	mov cx,4
	
	REDUCELIVES:
		mov dl,32
		mov ah,02h
		int 21h
	
	loop REDUCELIVES
	
	mov dh,0
	mov dl,156
	mov ah,2
	int 10h
	
	mov ah,09h
	mov al,3
	mov bh,0
	mov bl,4
	mov cx,lives
	int 10h
	
	pop cx
	
	ret



printlives endp
;print the score on the game screen

prints proc

	mov ah,02h
	mov bx,0
	mov dh,1
	mov dl,1
	int 10h
	
	
	lea dx,offset sco
	mov ah,09h
	int 21h

	ret

prints endp

;print the score recursively when the brick is broken
printScore proc
    push ax
    push bx
    push cx
    push dx
    
    mov cx,0
    
    mov ax,S
    ll:
		mov bx,10
		mov dx,0
		div bx
		push dx
		inc cx
		cmp ax,0
    jne ll
	
	mov ah,02h
	mov bx,0
	mov dh,1
	mov dl,9
	int 10h
    
    l2:
		pop dx
		mov ah,02
		add dl,'0'
		int 21h
    loop l2
    
    pop dx
    pop cx
    pop bx
    pop ax
    
    ret
printScore endp

;displays the main page functionlaity


mainpage proc
		mov check2,0
	
		mov ah,00
		mov al,03
		int 10h
		
		mov ah,09h
		mov bh, 00h
		mov al, 20h
		mov cx,1000h
		mov bl, 3fh  ; This is Blue & White.
		int 10h
		
		mov ah,02h
		mov bx,0
		mov dh,7
		mov dl,28
		mov al,3
		int 10h

		
	mov cx,sizeof strings
	mov si,offset strings
	
	l11:
	
		mov dl,[si]
		int 21h
		inc si
		
	loop l11

;=======================================================		

	
	mov ah,02h
	mov bx,0
	mov dh,49
	mov dl,45
	int 10h
	
	
	mov cx,sizeof string1
	mov si,offset string1
	
	l1:
	
		mov dl,[si]
		int 21h
		inc si
		
	loop l1
	
;=======================================================		
	
	mov ah,02h
	mov bx,0
	mov dh,10
	mov dl,28
	mov al,3
	int 10h
	
	mov cx,sizeof stringinput
	mov si,offset stringinput
	
	input1:
	
		mov dl,[si]
		int 21h
		inc si
		
	loop input1
	
	mov si,offset Nam
	
	in1:
	mov cx, 2
	mov ah, 01h 		
	int 21h
	mov [si],al
	inc si
	cmp al,0dh	
	je menupage
	loop in1

;=======================================================		

mainpage endp





;-----------------------------------------------------------------------------
fileopen proc

;open a file

mov ah, 3dh
mov al, 2
lea dx,f1
int 21h

mov fileinfo,ax

ret

fileopen endp

filehandling proc
push ax
push bx
push cx
push dx

mov bx,fileinfo
mov cx,0
mov dx, 0
mov ah,42h
mov al,2
int 21h

mov ah, 40h ; service to write to a file
mov bx, fileinfo
mov cx, lengthof nam ;string length.
mov dx, offset nam
int 21h

mov bx,fileinfo
mov cx,0
mov dx, 0
mov ah,42h
mov al,2
int 21h

mov ah, 40h ; service to write to a file
mov bx, fileinfo
mov cx, 1 ;string length.
mov dx, offset '$'
int 21h

mov bx,fileinfo
mov cx,0
mov dx, 0
mov ah,42h
mov al,2
int 21h

mov ah, 40h ; service to write to a file
mov bx, fileinfo
mov cx, 1 ;string length.
mov dx, offset levels
int 21h

mov bx,fileinfo
mov cx,0
mov dx, 0
mov ah,42h
mov al,2
int 21h

mov ah, 40h ; service to write to a file
mov bx, fileinfo
mov cx, 1 ;string length.
mov dx, offset skip
int 21h

mov bx,fileinfo
mov cx,0
mov dx, 0
mov ah,42h
mov al,2
int 21h


mov ax,S
mov bl,10
div bl

mov S3,ah

mov ah,0
div bl
mov S2,ah

mov ah,0
div bl
mov S1,ah

add s2,48
add s3,48
add s1,48

;--------------------------------------------------------------------------------------------

mov ah, 40h ; service to write to a file
mov bx, fileinfo
mov cx, 1 ;string length.
mov dx, offset s1	
int 21h

mov bx,fileinfo
mov cx,0
mov dx, 0
mov ah,42h
mov al,2
int 21h


mov ah, 40h ; service to write to a file
mov bx, fileinfo
mov cx, 1 ;string length.
mov dx, offset s2	
int 21h

mov bx,fileinfo
mov cx,0
mov dx, 0
mov ah,42h
mov al,2
int 21h

mov ah, 40h ; service to write to a file
mov bx, fileinfo
mov cx, 1 ;string length.
mov dx, offset s3	
int 21h

;--------------------------------------------------------------------

mov bx,fileinfo
mov cx,0
mov dx, 0
mov ah,42h
mov al,2
int 21h

mov ah, 40h ; service to write to a file
mov bx, fileinfo
mov cx, 1 ;string length.
mov dx, offset skip
int 21h

mov fileinfo,ax

mov ah, 3eh
mov bx, fileinfo
int 21h

mov fileinfo,ax
 
pop dx
pop cx
pop bx
pop ax

ret 

filehandling endp


;-----------------------------------------------------------------------



;prints the menu page that decides that the next opertion to be performed
menupage proc

;call fileopen
;call filehandling

lll:
		mov ah,0
		mov al,13h
		int 10h
		
		call printboundries
		
		mov ah,02h
		mov bx,0
		mov dh,0
		mov dl,194
		mov al,3
		int 10h
	
	
	mov cx,sizeof Nam
	mov si,offset Nam
	
	na1:
	
		mov dl,[si]
		int 21h
		inc si
		
	loop na1

;=======================================================		

		mov ah,02h
		mov bx,0
		mov dh,2
		mov dl,53
		mov al,3
		int 10h

	mov cx,sizeof stringn
	mov si,offset stringn 
	
	l11:
	
		mov dl,[si]
		int 21h
		inc si
		
	loop l11
;=======================================================		
	mov ah,02h
	mov bx,0
	mov dh,6
	mov dl,55
	int 10h

	mov cx,sizeof ngame
	mov si,offset ngame 
	
	n1:
	
		mov dl,[si]
		int 21h
		inc si
		
	loop n1
;=======================================================		
	mov ah,02h
	mov bx,0
	mov dh,9
	mov dl,54
	int 10h

	mov cx,sizeof rgame
	mov si,offset rgame 
	
	r1:
	
		mov dl,[si]
		int 21h
		inc si
		
	loop r1
;=======================================================		
	mov ah,02h
	mov bx,0
	mov dh,12
	mov dl,54
	int 10h

	mov cx,sizeof inst
	mov si,offset inst
	
	instn1:
	
		mov dl,[si]
		int 21h
		inc si
		
	loop instn1
;=======================================================		

;=======================================================	

	mov ah,02h
	mov bx,0
	mov dh,15
	mov dl,58
	int 10h

	mov cx,sizeof ex
	mov si,offset ex 
	
	ex1:
	
		mov dl,[si]
		int 21h
		inc si
		
	loop ex1
;=======================================================

ln1:
			mov ah,00h
			int 16h
					
			cmp al,49
			je n
			
			cmp al,50
			je sk
			
			cmp al,53
			je m
			
			cmp al,51
			je i
			
			cmp al,27
			je m
			
		jmp ln1

		n:
			call levelSelect
			cmp al,27
			je lll
			ret
		i:	
			call instruction
			jmp lll
		m:
			mov ah,00h
			mov al,13h
			int 10h
			call exit2
		sk:
			call highscore
			cmp al,27
			je lll
		
		
menupage endp

highscore proc

call fileopen


mov ah,3fh
mov cx,11 
mov dx,offset destroy 
mov bx,fileinfo 
int 21h 

mov fileinfo,ax

mov ah, 3eh
mov bx, fileinfo
int 21h

		mov ah,00
		mov al,03
		int 10h
		
		mov ah,09h
		mov bh, 00h
		mov al, 20h
		mov cx,1000h
		mov bl, 30h  ; This is Blue & Black.
		int 10h
		
		
		mov ah,02h
		mov bx,0
		mov dh,2
		mov dl,0
		int 10h
	
	
		lea dx,offset h_s
		mov ah,09h
		int 21h
	
		
		mov ah,02h
		mov bx,0
		mov dh,4
		mov dl,18
		int 10h
	
	
		lea dx,offset stringdisplay
		mov ah,09h
		int 21h
		
		mov ah,02h
		mov bx,0
		mov dh,8
		mov dl,28
		mov al,3
		int 10h
		
		lea dx,offset disp1
		mov ah,09h
		int 21h
		
		mov ah,02h
		mov bx,0
		mov dh,8
		mov dl,38
		mov al,3
		int 10h
		
		mov al,destroy[5]
		mov temp,al
		mov bl,destroy[6]
		mov destroy[6],'$'
		
		lea dx,offset destroy
		mov ah,09h
		int 21h
		
		mov ah,02h
		mov bx,0
		mov dh,10
		mov dl,28
		mov al,3
		int 10h
		
		lea dx,offset disp2
		mov ah,09h
		int 21h
		
		mov ah,02h
		mov bx,0
		mov dh,10
		mov dl,40
		mov al,3
		int 10h
		
		add al,47
		
		mov destroy[0],al
		mov destroy[1],'$'
		
		lea dx,offset destroy
		mov ah,09h
		int 21h
		
		mov ah,02h
		mov bx,0
		mov dh,12
		mov dl,28
		mov al,3
		int 10h		
		
		lea dx,offset disp3
		mov ah,09h
		int 21h
		
		mov ah,02h
		mov bx,0
		mov dh,12
		mov dl,39
		mov al,3
		int 10h

		mov al,destroy[7]	
		
		mov Cl,destroy[8]
		
		mov destroy[0],bl
		mov destroy[1],al
		mov destroy[2],'0'
		mov destroy[3],'$'
		
		
		lea dx,offset destroy
		mov ah,09h
		int 21h
		
		mov ah,02h
	mov bx,0
	mov dh,21
	mov dl,25
	int 10h
	
	
	lea dx,offset inst77
	mov ah,09h
	int 21h
	

	mov ah,02h
	mov bx,0
	mov dh,22
	mov dl,0
	int 10h
	
	
	lea dx,offset stringinstl
	mov ah,09h
	int 21h
		
again:	
		mov ah,00h
		int 16h
				
		cmp al,27
		je new
		
		jmp again

new: 
	ret


highscore endp

;displays the instruction on the instruction screen
instruction proc

mov ah, 00h
	mov al,03
	int 10h
	
	mov ah,09h
	mov bh, 00h
	mov al, 20h
	mov cx,800h
	mov bl, 3fh  ; This is Blue & White.
	int 10h
	;------------------------------------------------------------------------------
	mov ah,02h
	mov bx,0
	mov dh,0
	mov dl,0
	int 10h
	
	
	lea dx,offset stringinst
	mov ah,09h
	int 21h
	
	;------------------------------------------------------------------------------
	
	mov ah,02h
	mov bx,0
	mov dh,2
	mov dl,18
	int 10h
	
	
	lea dx,offset stringdisplay
	mov ah,09h
	int 21h
	
	
	;-------------------------------------------------------------------------------
	
	mov ah,02h
	mov bx,5
	mov dh,9
	mov dl,10
	mov al,3
	int 10h
	
	
	lea dx,offset inst1
	mov ah,09h
	int 21h

	mov ah,02h
	mov bx,5
	mov dh,7
	mov dl,3
	mov al,3
	int 10h
	
	
	lea dx,offset inst2
	mov ah,09h
	int 21h

	mov ah,02h
	mov bx,5
	mov dh,5
	mov dl,0
	mov al,3
	int 10h
	
	
	lea dx,offset inst3
	mov ah,09h
	int 21h
	
	mov ah,02h
	mov bx,5
	mov dh,11
	mov dl,18
	mov al,3
	int 10h
	
	
	lea dx,offset inst5
	mov ah,09h
	int 21h
	
	mov ah,02h
	mov bx,5
	mov dh,13
	mov dl,25
	mov al,3
	int 10h
	
	
	lea dx,offset inst6
	mov ah,09h
	int 21h
	
;---------------------------------------------------------------------------------------------

	mov ah,02h
	mov bx,0
	mov dh,21
	mov dl,25
	int 10h
	
	
	lea dx,offset inst7
	mov ah,09h
	int 21h
	

	mov ah,02h
	mov bx,0
	mov dh,22
	mov dl,0
	int 10h
	
	
	lea dx,offset stringinstl
	mov ah,09h
	int 21h
	
again:	
		mov ah,00h
		int 16h
				
		cmp al,0dh
		je new
		
		jmp again

new: 
	ret

instruction endp


;------------------------------------------------------------------------

	beep proc
        push ax
        push bx
        push cx
        push dx
		
        mov     al, 0B6h        ; Prepare the speaker for the
        out     43h, al         ;  note.
        mov     ax, 400        ; Frequency number (in decimal)
                                ;  for middle C.
        out     42h, al         ; Output low byte.
        mov     al, ah          ; Output high byte.
        out     42h, al 
        in      al, 61h         ; Turn on note (get value from
                                ;  port 61h).
        or      al, 00000011b   ; Set bits 1 and 0.
        out     61h, al         ; Send new value.
        mov     bx, 2          ; Pause for duration of note.
p1:
        mov     cx, 65535
p2:
        dec     cx
        jne     p2
        dec     bx
        jne     p1
        in      al, 61h         ; Turn off note (get value from
                                ;  port 61h).
        and     al, 11111100b   ; Reset bits 1 and 0.
        out     61h, al         ; Send new value.

        pop dx
        pop cx
        pop bx
        pop ax

ret
beep endp

;----------------------------------------------------------------------------------


clear_screen proc
			MOV AH,00h                   ;set the configuration to video mode
			MOV AL,13h                   ;choose the video mode
			INT 10h    					 ;execute the configuration 
		
			MOV AH,0Bh 					 ;set the configuration
			MOV BH,00h 					 ;to the background color
			MOV BL,00h 					 ;choose black as background color
			INT 10h    					 ;execute the configuration
			
	RET
clear_screen endp

;---------------------------------------------------------------------------------
	
	checkbrick2 macro Q,R,S1,T
		
		local NotFF
		local doaddd
		local DONEEE
		
		mov ax,BALL_Y
		mov bx,BALL_X
		
		mov cx,S1
		mov dx,T
		add ax,5
		add bx,5
		
		mov cx,bx
		mov dx,ax
		mov ah,0Dh									; Checks if the colour of the brick is Black. It means that it has already been hit before. 
		int 10h
		cmp al,0	
		je NotFF
		
		mov dx,T
		cmp dx,BALL_Y								; Checks from brick bottom
		jl NotFF
		add dx,10
		cmp BALL_Y,dx								; If struck from Top							
		jg NotFF
		
		mov cx,S1
		cmp BALL_X,cx
		jl NotFF
		
		add cx,45
		cmp cx,BALL_X
		jl NotFF
		
		NEG BALL_VELOCITY_Y
		mov Colour,-1								; Because Colour is being incremented by 1 in brick funtion (So it will become zero ==> BLACK)
		
		add S,10
		
		mov ax,Q
		mov block_a,ax
		mov ax,T
		mov block_b,ax
		mov ax,S1
		mov block_c,ax
		mov ax,R
		mov block_d,ax
		call BRICK
		mov checkexcept,1
		call beep
		NotFF:
			
		
	endm
	
	
	checkbrick1 macro A,B,X,Y
		
		local NotF
		local doadd
		local DONEE
	
		mov ax,BALL_Y
		mov bx,BALL_X
		mov cx,X
		mov dx,Y
		
		cmp dx,BALL_Y					; Checks from brick bottom
		jl NotF
		
		cmp BALL_VELOCITY_Y,1
		je doadd
		
		sub dx,10						; If struck from Bottom
		cmp BALL_Y,dx												
		jl NotF
		jmp DONEE
		
		doadd:
			add dx,10
			cmp BALL_Y,dx				; If struck from Top							
			jg NotF
		
		DONEE:
		
		cmp BALL_X,cx
		jl NotF
		
		add cx,45
		cmp cx,BALL_X
		jl NotF
		
		mov cx,BALL_X
		mov dx,BALL_Y
		
		mov ah,0Dh									; Checks if the colour of the brick is Black. It means that it has already been hit before. 
		int 10h
		cmp al,0	
		je NotF
		
		NEG BALL_VELOCITY_Y
		
		mov Colour,-1								; Because Colour is being incremented by 1 in brick funtion (So it will become zero ==> BLACK)
		
		add S,10
		
		mov ax,A
		mov block_a,ax
		mov ax,Y
		mov block_b,ax
		mov ax,X
		mov block_c,ax
		mov ax,B
		mov block_d,ax
		call BRICK
		call beep
		
		
		NotF:	
	endm


	LEVEL2_checkbrick2 macro Q,R,S1,T
		
		local L2NotFF
		local L2doaddd
		local L2DONEEE
		local LastHit
		local moveover
		local FIXED
		local NOTFIXED
		local NOTLEVEL3
		local blockM2
		local ThirdHit
		local FirstHitOutofThree
		local MAGIC
		local NOMAGIC
		
		
		mov ax,BALL_Y
		mov bx,BALL_X
		
		mov cx,S1
		mov dx,T
		add ax,5
		add bx,5
		
		mov cx,bx
		mov dx,ax
		mov ah,0Dh									; Checks if the colour of the brick is Black. It means that it has already been hit before. 
		int 10h
		cmp al,0	
		je L2NotFF

		
		mov dx,T
		cmp dx,BALL_Y								; Checks from brick bottom
		jl L2NotFF
		add dx,10
		cmp BALL_Y,dx								; If struck from Top							
		jg L2NotFF
		
		mov cx,S1
		cmp BALL_X,cx
		jl L2NotFF
		
		add cx,45
		cmp cx,BALL_X
		jl L2NotFF
		
				
		cmp levels,51
		jne NOTLEVEL3
		mov cx,BALL_X
		mov dx,BALL_Y
		mov ah,0Dh									; Checks if the colour of the brick is Black. It means that it has already been hit before. 
		int 10h
		cmp al,15
		jne NOTFIXED
		NEG BALL_VELOCITY_Y
		mov Colour,14
		mov ax,Q
		mov block_a,ax
		mov ax,T
		mov block_b,ax
		mov ax,S1
		mov block_c,ax
		mov ax,R
		mov block_d,ax
		call BRICK
		call beep
		mov bakwasecp,1
		jmp L2NotFF
		
		NOTFIXED:
		cmp al,8
		jne NOMAGIC
		cmp levels,51
		jne NOMAGIC
		NEG BALL_VELOCITY_Y
		mov di,0
		mov block_a,60
		mov block_b,70
		mov block_c,15
		mov block_d,60
		blockM2:
		mov Colour,-1
		call brick
		
		add block_d,50
		add block_c,50
		inc di
		cmp di,5
		jbe blockM2
		call beep
		add S,50
		mov bakwasecp,1
		
		jmp L2NotFF
		
		
		NOMAGIC:
		
		mov cx,BALL_X
		mov dx,BALL_Y
		mov ah,0Dh									
		int 10h
		cmp al,1
		je ThirdHit
		cmp al,2	
		jne FirstHitOutofThree
		mov Colour,0								; Because Colour is being incremented by 1 in brick funtion (So it will become zero ==> BLACK)
		mov bakwasecp,1
		jmp moveover
		
		FirstHitOutofThree:
		mov Colour,1
		mov bakwasecp,1
		jmp moveover
		
		
		ThirdHit:
		mov colour,-1
		add s,10
		jmp moveover
		
		
		NOTLEVEL3:
		NEG BALL_VELOCITY_Y
		
		mov cx,BALL_X
		mov dx,BALL_Y
		mov ah,0Dh									; Checks if the colour of the brick is Black. It means that it has already been hit before. 
		int 10h
		cmp al,1	
		jne LastHit
		mov Colour,-1
		add S,10									; Because Colour is being incremented by 1 in brick funtion (So it will become zero ==> BLACK)
		jmp moveover
		
		LastHit:
		mov Colour,0
		
		moveover:
		mov ax,Q
		mov block_a,ax
		mov ax,T
		mov block_b,ax
		mov ax,S1
		mov block_c,ax
		mov ax,R
		mov block_d,ax
		call BRICK
		mov checkexcept,1
		call beep
		L2NotFF:
			
		
	endm
	
	
	LEVEL2_checkbrick1 macro A,B,X,Y
		
		local L2NotF
		local L2doadd
		local L2DONEE
		local L2LastHit
		local L2moveover
		local L3moveover
		local FirstHitOutofThree
		local ThirdHit
		local Hit2Lvl
		local NOMAGIC
		local blockM2
		local NOTFIXED
		
		
		mov ax,BALL_Y
		mov bx,BALL_X
		mov cx,X
		mov dx,Y
		
		cmp dx,BALL_Y					; Checks from brick bottom
		jl L2NotF
		
		cmp BALL_VELOCITY_Y,1
		je L2doadd
		
		sub dx,10						; If struck from Bottom
		cmp BALL_Y,dx												
		jl L2NotF
		jmp L2DONEE
		
		L2doadd:
			add dx,10
			cmp BALL_Y,dx				; If struck from Top							
			jg L2NotF
		
		L2DONEE:
		
		cmp BALL_X,cx
		jl L2NotF
		
		add cx,45
		cmp cx,BALL_X
		jl L2NotF
		
		mov cx,BALL_X
		mov dx,BALL_Y
		
		mov ah,0Dh									; Checks if the colour of the brick is Black. It means that it has already been hit before. 
		int 10h
		cmp al,0	
		je L2NotF
		
		cmp levels,50
		je Hit2Lvl
		
		mov cx,BALL_X
		mov dx,BALL_Y
		
		mov ah,0Dh									; Checks if the colour of the brick is Black. It means that it has already been hit before. 
		int 10h
		cmp al,15
		jne NOTFIXED
		NEG BALL_VELOCITY_Y
		mov Colour,14
		mov ax,A
		mov block_a,ax
		mov ax,Y
		mov block_b,ax
		mov ax,X
		mov block_c,ax
		mov ax,B
		mov block_d,ax
		call BRICK
		call beep
		mov bakwasecp,1
		jmp L2NotF
		
		NOTFIXED:
		cmp al,8
		jne NOMAGIC
		cmp levels,51
		jne NOMAGIC
		NEG BALL_VELOCITY_Y
		mov di,0
		mov block_a,60
		mov block_b,70
		mov block_c,15
		mov block_d,60
		mov special,1
		blockM2:
		mov Colour,-1
		call brick
		
		add block_d,50
		add block_c,50
		inc di
		cmp di,5
		jbe blockM2
		call beep
		add S,50
		mov bakwasecp,1
		
		jmp L2NotF
		
		
		NOMAGIC:
		
		mov cx,BALL_X
		mov dx,BALL_Y
		mov ah,0Dh									
		int 10h
		cmp al,1
		je ThirdHit
		cmp al,2	
		jne FirstHitOutofThree
		mov Colour,0								; Because Colour is being incremented by 1 in brick funtion (So it will become zero ==> BLACK)
		mov bakwasecp,1
		jmp L3Moveover
		
		FirstHitOutofThree:
		mov Colour,1
		jmp L3Moveover
		mov bakwasecp,1
		
		ThirdHit:
		mov colour,-1
		add s,10
		jmp L3Moveover
		
		Hit2Lvl:
		mov cx,BALL_X
		mov dx,BALL_Y
		mov ah,0Dh									
		int 10h
		cmp al,1	
		jne L2LastHit
		mov Colour,-1								; Because Colour is being incremented by 1 in brick funtion (So it will become zero ==> BLACK)
		add S,10
		jmp L2moveover
		L2LastHit:
		mov Colour,0
		mov bakwasecp,1
		
		L3Moveover:
		L2moveover:
		NEG BALL_VELOCITY_Y
		
	
		mov ax,A
		mov block_a,ax
		mov ax,Y
		mov block_b,ax
		mov ax,X
		mov block_c,ax
		mov ax,B
		mov block_d,ax
		call BRICK
		call beep
		mov bakwasecp,1
		
		
		L2NotF:	
		
	endm
	
	
	move_ball proc
		
		mov Colour,0											; Drawing black ball at previous co-ordinates 
		call draw_ball
		
		; Adding the Velocities to get New P 
		
		mov ax,BALL_VELOCITY_X
		add BALL_X,ax
		
		mov ax,BALL_VELOCITY_Y									
		add BALL_Y,ax
		
		
		mov ax,196												; Checks if the Ball has gone past down the Striker And Draws Life
		cmp ax,BALL_Y
		JB DLife
			
		
		mov ax,RIGHT_BOUNDARY									; Check If Ball's X coordinate is more than the window right side
		sub ax,2
		cmp BALL_X,ax
		JA CHGVEL_X
		
		mov ax,LEFT_BOUNDARY									; Check If Ball's X coordinate is less than the window left side
		add ax,2
		cmp BALL_X,ax
		JL CHGVEL_X
	
	
		;mov ax,0												; Check If Ball's Y coordinate is more than the window top side
		;add ax,10
		mov ax,BALL_Y
		cmp ax,UPPER_BOUNDARY
		JB CHGVEL_Y


		mov ax,BALL_Y
		add ax,5												; Size of BALL
		cmp ax,STRIKER_Y_Temp
		JNG NOTREBOUND											; If Ball IS Far Above the striker
		
		mov ax,STRIKER_Y
		add ax,5
		cmp BALL_Y,ax
		JNL NOTREBOUND
		
		mov ax,BALL_X											; If Ball is on the Left of the Striker
		add ax,5							
		cmp ax,STRIKER_X
		JNG NOTREBOUND
		
		mov ax,STRIKER_X_ENDING									; If Ball is on the Right of the Striker		
		cmp BALL_X,ax
		JNL NOTREBOUND
		
		mov ax,STRIKER_X
		add ax, 16
			
		mov bx,STRIKER_X_ENDING
		sub bx,16
		
		cmp levels,51
		je L3StrikerHit
		
		cmp BALL_X,ax
		jb LEFTPART	
			
		cmp BALL_X,bx
		ja RIGHTPART

			
			MIDDLE:
				mov BALL_VELOCITY_X,0
				NEG BALL_VELOCITY_Y
				jmp DR
			
			LEFTPART:
				cmp BALL_VELOCITY_X,0
				jne SIMPLE
				mov BALL_VELOCITY_X,-1
				jmp SIMPLE
			
			RIGHTPART:
				cmp BALL_VELOCITY_X,0
				jne SIMPLE
				mov BALL_VELOCITY_X,1
				jmp SIMPLE
			
			L3StrikerHit:
				mov ax,Striker_X
				add ax,10
				mov bx,STRIKER_X_ENDING
				sub bx,10
				cmp BALL_X,ax
				jb L3L
				
				cmp BALL_X,bx
				ja L3R
					
				mov BALL_VELOCITY_X,0
				NEG BALL_VELOCITY_Y
				jmp DR
				
					
				L3L:
					cmp BALL_VELOCITY_X,0
					jne SIMPLE
					mov BALL_VELOCITY_X,-1
					jmp SIMPLE
			
				L3R:
					cmp BALL_VELOCITY_X,0
					jne SIMPLE
					mov BALL_VELOCITY_X,1
					jmp SIMPLE
				
			SIMPLE:
				
				NEG BALL_VELOCITY_Y
			
		
		jmp DR
	
		
		NOTREBOUND:
			
		; CHECKS FOR ALL BRICKS
			
			cmp BALL_VELOCITY_X,1
			jne Excep 
			cmp BALL_VELOCITY_Y,1
			jne Excep
			; 1st row
			
			
			mov di,0
			mov block_a,40
			mov block_b,50
			mov block_c,15
			mov block_d,60
			
			cmp levels,50
			je ChkL2Bricks
			cmp levels,51
			je ChkL2Bricks
			
			block6:
			checkbrick2 block_a,block_d,block_c,block_b
			add block_d,50
			add block_c,50
			inc di
			cmp di,5
			jbe block6
			jmp L2_2
			
			ChkL2Bricks:
			L2block6:
			Level2_checkbrick2 block_a,block_d,block_c,block_b
			add block_d,50
			add block_c,50
			inc di
			cmp di,5
			jbe L2block6
			
			L2_2:
			
			
			;----------------------------------------
		;2nd row
		
			mov di,0
			mov block_a,60
			mov block_b,70
			mov block_c,15
			mov block_d,60
			
			cmp levels,50
			je ChkL2_R2Bricks
			cmp levels,51
			je ChkL2_R2Bricks
			
			block5:
			checkbrick2 block_a,block_d,block_c,block_b

			add block_d,50
			add block_c,50
			inc di
			cmp di,5
			jbe block5
			jmp L2_R3
			
			ChkL2_R2Bricks:
			L2block5:
			Level2_checkbrick2 block_a,block_d,block_c,block_b
			add block_d,50
			add block_c,50
			inc di
			cmp di,5
			jbe L2block5
			
			
			L2_R3:
			;----------------------------------------
		;3rd row
		
			mov di,0
			mov block_a,80
			mov block_b,90
			mov block_c,15
			mov block_d,60
			
			cmp levels,50
			je ChkL2_R3Bricks
			cmp levels,51
			je ChkL2_R3Bricks
			
			block4:
			checkbrick2 block_a,block_d,block_c,block_b
			
			add block_d,50
			add block_c,50
			inc di
			cmp di,5
			jbe block4
			cmp checkexcept,1
			je DR
			
			ChkL2_R3Bricks:
			L2block4:
			Level2_checkbrick2 block_a,block_d,block_c,block_b
			add block_d,50
			add block_c,50
			inc di
			cmp di,5
			jbe L2block4
			
			jmp DR
		
		Excep:
		; 1st row
		
			mov di,0
			mov block_a,40
			mov block_b,50
			mov block_c,15
			mov block_d,60
			
			cmp levels,50											; If Level Selected is 2
			je ChkL2_ER1Bricks
			
			cmp levels,51											; If Level Selected is 3
			je ChkL3_ER1Bricks
			
			block1:
			checkbrick1 block_a,block_d,block_c,block_b
			add block_d,50
			add block_c,50
			inc di
			cmp di,5
			jbe block1
			jmp L2_ER2
			
			ChkL2_ER1Bricks:
			blockE1:
			Level2_checkbrick1 block_a,block_d,block_c,block_b
			add block_d,50
			add block_c,50
			inc di
			cmp di,5
			jbe blockE1
			cmp bakwasecp,1
			je DR
			
			ChkL3_ER1Bricks:
				blockL3E1:
			LEVEL2_checkbrick1 block_a,block_d,block_c,block_b

			add block_d,50
			add block_c,50
			inc di
			cmp di,5
			jbe blockL3E1
			cmp bakwasecp,1
			je DR
			
			L2_ER2:
			
			;----------------------------------------
		;2nd row
		
			mov di,0
			mov block_a,60
			mov block_b,70
			mov block_c,15
			mov block_d,60
			
			cmp levels,50
			je ChkL2_ER2Bricks
			
			cmp levels,51
			je ChkL3_ER2Bricks
			
			
			block2:
			checkbrick1 block_a,block_d,block_c,block_b

			add block_d,50
			add block_c,50
			inc di
			cmp di,5
			jbe block2
			jmp L2_ER3
			
			ChkL2_ER2Bricks:
			blockE2:
			Level2_checkbrick1 block_a,block_d,block_c,block_b

			add block_d,50
			add block_c,50
			inc di
			cmp di,5
			jbe blockE2
			cmp bakwasecp,1
			je DR
			
			; LEVEL 3
			
			ChkL3_ER2Bricks:
				blockL3E2:
			LEVEL2_checkbrick1 block_a,block_d,block_c,block_b

			add block_d,50
			add block_c,50
			inc di
			cmp di,5
			jbe blockL3E2
			cmp bakwasecp,1
			je DR
			
			l2_ER3:
			
			;----------------------------------------
		;3rd row
		
			mov di,0
			mov block_a,80
			mov block_b,90
			mov block_c,15
			mov block_d,60
			
			cmp levels,50
			je ChkL2_ER3Bricks
			
			cmp levels,51
			je ChkL3_ER3Bricks
			
			block3:
			checkbrick1 block_a,block_d,block_c,block_b
			
			add block_d,50
			add block_c,50
			inc di
			cmp di,5
			jbe block3
			jmp DR
			
			ChkL2_ER3Bricks:
			blockL2E3:
			Level2_checkbrick1 block_a,block_d,block_c,block_b
			
			add block_d,50
			add block_c,50
			inc di
			cmp di,5
			jbe blockL2E3
			
			jmp DR
			
			
			; LEVEL 3
			
			ChkL3_ER3Bricks:
				blockL3E3:
			LEVEL2_checkbrick1 block_a,block_d,block_c,block_b

			add block_d,50
			add block_c,50
			inc di
			cmp di,5
			jbe blockL3E3
			jmp DR
			
			
			
		
		CHGVEL_X:
			NEG BALL_VELOCITY_X
			jmp DR
			
		CHGVEL_Y:
			NEG BALL_VELOCITY_Y
			jmp DR
			
		DLife:
			dec LIVES
			mov BALL_X,150
			mov BALL_Y,100
			cmp levels,51
			jne DR
			mov BALL_VELOCITY_X,0
		
		DR:
			mov bakwasecp,0
			mov checkexcept,0
			mov Colour,14
			call draw_ball
			RET
	move_ball endp


	
	; DRAW BALL FUNCTION
	
	DRAW_BALL PROC                  
		
		MOV DX, BALL_Y
	
		BALL1:
		
			MOV CX, BALL_X
			MOV Bx,6
			
			ball2:
			
				MOV AL, colour
				MOV AH, 0CH
				INT 10H
				
				INC CX
				dec bx
				cmp bx,0
			JNE ball2
			
			INC Dx
			dec var
			CMP var,0
		JNE ball1
			
			mov var,6
		
		RET
	DRAW_BALL ENDP
	
	drw_o proc
		MOV DX, sp_y
	
		BALL1:
		
			MOV CX, sp_x
			MOV Bx,6
			
			ball2:
			
				MOV AL, colo
				MOV AH, 0CH
				INT 10H
				
				INC CX
				dec bx
				cmp bx,0
			JNE ball2
			
			INC Dx
			dec var
			CMP var,0
		JNE ball1
			
			mov var,6
		
		RET
		
	drw_o endp
	
	; GRID FORMATION FUNCTION
	
	
	brick proc

		MOV DX, block_a
		inc colour

		L1:
			MOV CX, block_c
			L2:
				MOV AL, Colour
				MOV AH, 0CH
				INT 10H

				INC CX
				CMP CX,block_d
			JNE L2

			INC DX

			CMP DX,block_b
			JNE L1

			
		ret
	brick endp
	
	top proc
		
		push Dx
		push CX
	
		MOV DX, 00
		
		top1:
		
			MOV CX, 00
			
			L2:
			
					;MOV AL, 0
					;MOV AH, 0CH
					;INT 10H
				
				INC CX
				CMP CX,319
			JNE L2
			
			INC DX
			
			CMP DX,20
		JNE top1
		
		POP CX
		POP Dx
		
		
		ret
	
	top endp



main proc
	mov ax,@data
	mov ds,ax
	
	call mainpage
	
	MOV AH,00h                   ;set the configuration to video mode
	MOV AL,13h                   ;choose the video mode
	INT 10h 
	
	Mov colour,0
	call clear_screen
	
	mov cx,0
	mov dx,20

	mov var_B,0

	l1:
		cmp var_B,319
		je exij

		mov ah,0ch
		mov al,0ch
		int 10h

		add var_B,1
		add cx,1

	jmp l1

	exij:
	
		mov ah,02h
		mov bx,0
		mov dh,2
		mov dl,17
		mov al,3
		int 10h

		
		lea dx,offset lvl1
		mov ah,09h
		int 21h
		
		mov ah,02h
		mov bx,0
		mov dh,2
		mov dl,24
		mov al,3
		int 10h

		
		mov dl,levels
		mov ah,02h
		int 21h
		
		
	mov cx,141
	mov dx,25

	mov var_B,0

	lll1:
		cmp var_B,57
		je exijj

		mov ah,0ch
		mov al,0ch
		int 10h

		add var_B,1
		add cx,1

	jmp lll1

	exijj:
;--------------------------------------------------------
	
	
	; 1st row

		mov di,0
		mov block_a,40
		mov block_b,50
		mov block_c,15
		mov block_d,60
		
		block1:
		
		.if(ch_c == 0)
			mov colour,0eh
		.endif
		.if(ch_c == 1)
			mov colour,0ah
			mov ch_c,-6
		.endif
		call brick
		
		add block_d,50
		add block_c,50
		inc di
		inc ch_c
		
		cmp di,5
		jbe block1
		
		mov colour,5
		;----------------------------------------
	;2nd row

		mov di,0
		mov block_a,60
		mov block_b,70
		mov block_c,15
		mov block_d,60
		block2:
		call brick
		
		add block_d,50
		add block_c,50
		inc di
		cmp di,5
		jbe block2
		
		mov colour,9
		;----------------------------------------
	;3rd row

		mov di,0
		mov block_a,80
		mov block_b,90
		mov block_c,15
		mov block_d,60
		mov ch_c,0
		block3:
		
		.if(ch_c == 1)
			mov colour,0eh
		.endif
		.if(ch_c == 2)
			mov colour,0ah
			mov ch_c,-6
		.endif
		call brick
		
		add block_d,50
		add block_c,50
		inc di
		inc ch_c
		
		cmp di,5
		jbe block3
		
	
	call gameLoop
ml:	
	mov ah,4ch
    int 21h
main endp
	

	gameLoop:
	
		call CHECK_INPUT
		cmp LIVES,0
		jne GNO
		call exit
		GNO:
		call printlives
		call printboundries
		call move_ball
		call prints
		call printScore
		call top
		call delay
		call endkk
		cmp check2,1
		je kl
		.if (special > 0)
			mov colo,0
			call drw_o
			add sp_y,1
			mov colo,6
			call drw_o
				.if(sp_y>199)
					mov special,0
				.endif
				mov ax,sp_x
				.if(ax>STRIKER_X)
					.if(ax<STRIKER_X_ENDING)
						mov ax,sp_y
						.if(ax==STRIKER_Y_Temp)
							add LIVES,1
							add S,20
							mov special,0
							mov colo,0
							call drw_o
							mov SpecialLength,20
						.endif
					.endif
				.endif
		.endif
	    
	JMP gameLoop 
	
	
	exit proc
		
		cmp lives,0
		je EEE						; IF Exit proc is called with 0 lives Then it only displays GAME OVER 
		
		mov ah,02h
		mov bx,0
		mov dh,1
		mov dl,16
		mov al,3
		int 10h
		
		lea dx,PAUSEE						
		mov ah,09h
		int 21h
		
		
		mov ah,00					; If Escape is Pressed in Paused state then the game quits
		int 16h
		cmp al,27D
		jne GO
		
		EEE:
		
		jmp exit2
		

	GO:
	
		mov ah,02h
		mov bx,0
		mov dh,1
		mov dl,16
		mov al,3
		int 10h
		
		lea dx,PAUSEER
		mov ah,09h
		int 21h
	
		ret
	exit endp
	
	
exit2 proc
	
		mov ah,02h
		mov ah, 00h
		mov al,03
		int 10h

		mov ah,09h
		mov bh, 00h
		mov al, 20h
		mov cx,1000h
		mov bl, 3fh  ; This is Blue & White.
		int 10h

		mov ah,02h
		mov bx,0
		mov dh,12
		mov dl,35
		mov al,3
		int 10h
		
		lea dx,string
		mov ah,09h
		int 21h
		
		call fileopen
		call filehandling
	
		mov ah,04ch
		int 21h



exit2 endp




	
	CHECK_INPUT proc
	
		mov cx,0
		mov dx,196
		
		mov ah,01
		int 16h         	 
		jz noInput 					    		 			; no key is pressed
		mov ah,00
		int 16h
		cmp ah,4dh
		je right
		cmp ah,4bh
		je left
		cmp al,27
		jne GAMENOTEND
		call exit
		GAMENOTEND:
		noInput:
			ret  

		right:    		; When right key is pressed, the striker is replaced by black pixels
				mov bx,RIGHT_BOUNDARY
				cmp STRIKER_X_ENDING,bx
				ja noInput
				
				mov colour,0								; And then a new one is formed towards the right of previous one
				call draw_bar
				
				add STRIKER_X,5
				mov colour,7
				call draw_bar
				jmp noInput
		
		left:   											; When left key is pressed, the striker is replaced by black pixels
				mov bx,LEFT_BOUNDARY
				cmp STRIKER_X,bx
				jb noInput
				
				mov colour,0								; And then a new one is formed towards the left of previous one
				call draw_bar
				
				sub STRIKER_X,5
				mov colour,7
				call draw_bar
				jmp noInput
		
			
		
	CHECK_INPUT endp
	
	
	draw_bar proc											; FUNCTION to draw the striker 
															; It uses Striker's Starting and Ending points along x axis and the colour
		push bx
		mov al,colour
		mov cx,STRIKER_X
		mov dx,STRIKER_Y
		mov bx,STRIKER_X
		cmp levels,49
		je DL1
		
		cmp levels,50
		je DL2
		sub bx,10
		
		DL2:
			sub bx,10
		
		DL1:
		add bx,50
		add bx,SpecialLength
		mov STRIKER_X_ENDING,bx
		D:
			mov ah,0ch
			int 10h
			inc cx
			cmp cx,bx
			jne D
	
	
	
		pop bx
		ret
	draw_bar endp
	
	delay proc
		
		mov cx,35000
		cmp levels,49
		je D1
		
		cmp levels,50
		je D2
		sub cx,10000
		
		D2:
			sub cx,15000
		
		D1:
		
		Delayy:
			
		loop Delayy
	
	
		ret
	delay endp
		
		
	endkk proc
	
	push ax
	push bx
	push cx
	push dx
	
	mov cx,10
	mov dx,45
	
	L1:
	
		MOV CX, 10
		L2:
			mov ah,0dh
			mov bh,0
			INT 10H
			
			cmp al,0
			jne lr

			INC CX
			CMP CX,300
		JNE L2

		INC DX

		CMP DX,120
		JNE L1
		
		mov ah,00
		mov al,03
		int 10h
		
		mov ah,09h
		mov bh, 00h
		mov al, 20h
		mov cx,1000h
		mov bl, 3fh  ; This is Blue & White.
		int 10h

		mov ah,02h
		mov bx,0
		mov dh,12
		mov dl,13
		mov al,3
		int 10h
		
		lea dx,stringc
		mov ah,09h
		int 21h
		
		mov ah,02h
		mov bx,0
		mov dh,21
		mov dl,25
		int 10h
		
		
		lea dx,offset inst7
		mov ah,09h
		int 21h
	
aga:		

		mov ah,00h
		int 16h
				
		cmp al,0dh
		je kk
		
	jmp aga
	
kk:
		mov check2,1
		
lr:	
	pop dx
	pop cx
	pop bx
	pop ax
	
	ret
	
	
	endkk endp
	

kl:
mov ah,04ch
int 21h

end