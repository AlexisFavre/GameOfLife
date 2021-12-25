    ;;    game state memory location
    .equ CURR_STATE, 0x1000              ; current game state
    .equ GSA_ID, 0x1004                     ; gsa currently in use for drawing
    .equ PAUSE, 0x1008                     ; is the game paused or running
    .equ SPEED, 0x100C                      ; game speed
    .equ CURR_STEP,  0x1010              ; game current step
    .equ SEED, 0x1014              ; game seed
    .equ GSA0, 0x1018              ; GSA0 starting address
    .equ GSA1, 0x1038              ; GSA1 starting address
    .equ SEVEN_SEGS, 0x1198             ; 7-segment display addresses
    .equ CUSTOM_VAR_START, 0x1200 ; Free range of addresses for custom variable definition
    .equ CUSTOM_VAR_END, 0x1300
    .equ LEDS, 0x2000                       ; LED address
    .equ RANDOM_NUM, 0x2010          ; Random number generator address
    .equ BUTTONS, 0x2030                 ; Buttons addresses

    ;; states
    .equ INIT, 0
    .equ RAND, 1
    .equ RUN, 2

    ;; constants
    .equ N_SEEDS, 4
    .equ N_GSA_LINES, 8
    .equ N_GSA_COLUMNS, 12
    .equ MAX_SPEED, 10
    .equ MIN_SPEED, 1
    .equ PAUSED, 0x00
    .equ RUNNING, 0x01

;-------------------------------------------------------------------------------------------------
;---------------------------MAIN------------------------------------------------------------------
;-------------------------------------------------------------------------------------------------

main:
; while True do

	; set stack pointer to 2000
	addi sp, zero, LEDS

	; reset_game()
	call reset_game	

	; e <- get_input()
	call get_input	
	addi s0, v0, 0

	; done <- false
	addi s1, zero, 0

	; while !done do	
	main_while_not_done:
		
		; select_action(e)
		addi a0, s0, 0		
		call select_action
		
		; update_state(e)		
		addi a0, s0, 0
		call update_state

		; update_gsa()
		call update_gsa

		; mask()
		call mask

		; draw_gsa()
		call draw_gsa

		; wait()
		call wait
		
		; done <- decrement_steps()
		call decrement_step
		addi s1, v0, 0

		; e <- get_input()
		call get_input	
		addi s0, v0, 0

		beq s1, zero, main_while_not_done

	jmpi main


;--------------------------------------------------------------------------------------------
;---------------------------Helper-----------------------------------------------------------
;--------------------------------------------------------------------------------------------

; BEGIN:helper

done:
	ret

;------------------------------------------------------------------------------------------------------------

fill_gsa_with_given_seed:	;a3 is the desired seed to fill in current GSA   (helper meth)
							;(one could replace a3 by current seed)

	addi sp, sp, -16				; save the ra because we gonna make call inside loop
	stw ra, 0(sp)
	stw s3, 4(sp)
	stw s4, 8(sp)
	stw s5, 12(sp)

	slli a3, a3, 2					;words(ie seeds) in SEEDS are word aligned !
	ldw t0, SEEDS(a3)				; load begining address of our interested seed

	add a1, zero, zero				; counter : index the desired line in GSA
	add s3, zero, t0				; counter index of SEEDS(a3) word (are word aligned -> multiple of 4)
	addi s4, zero, N_GSA_LINES		; number of line in GSA (to determine if have completly initialized GSA)


	fill_gsa_with_given_seed_loop:

		ldw a0, 0(s3)			;load s3 th line of our interested seed
		add a1, s5, zero
		call set_gsa
		addi s5, s5, 1			;update the indexes (here by 1 because we are interested in line nb)
		addi s3, s3, 4			; here by 4 because words in SEEDS are word aligned

		bne s5, s4, fill_gsa_with_given_seed_loop ; if GSA line index = nb lines in GSA => fully initialized
	

	ldw ra, 0(sp)				; pop ra value from stack
	ldw s3, 4(sp)
	ldw s4, 8(sp)
	ldw s5, 12(sp)
	addi sp, sp, 16
	ret

;------------------------------------------------------------------------------------------------------------
get_current_gsa:			; store adrr of current GSA in v0
	ldw t0, GSA_ID(zero)	; load flag wich indicate current GSA (0 or 1)
	slli t0, t0, 5 			; size GSA0 (32 words)
	addi v0, t0, GSA0   	; calculate current GSA addr( GSA0 or (GSA0 +32)=GSA1)
	ret

;-----------------------------------------------------------------------------------------------------------
display_on_7seg: ;leftmost digit is always 0 we don't care about it
	;end of procedure is to display number of steps on 7-SEG display
		add t1, a1, zero
		srli t2, t1, 4					; shift to extract the 16 LSB into 4 bits' chunks of size 4
		srli t3, t1, 8

		addi t5, zero, 0x000F			; mask to keep only 4 least significants bits

		and t1, t1, t5					;apply mask to keep only the 4 LSB
		and t2, t2, t5
		and t3, t3, t5

		slli t1, t1, 2					;font data words are word aligned => index & 00
		slli t2, t2, 2					;could make smaller code (limit nb shift by 2 but is more clear like this
		slli t3, t3, 2

		ldw t1, font_data(t1)			;load how this t1 number is coded on the seven segs display
		ldw t2, font_data(t2)				; must be word aligned
		ldw t3, font_data(t3)
		ldw t4, font_data(zero)

		stw t4, SEVEN_SEGS(zero)		; SEVEN_SEGS[0] is for left most digit
		stw t3, (SEVEN_SEGS+4)(zero)	; they are word aligned (reason of increasing by 4)
		stw t2, (SEVEN_SEGS+8)(zero)
		stw t1, (SEVEN_SEGS+12)(zero) 	; SEVEN_SEGS[3] is for right most digit

		ret

; END:helper

;--------------------------------------------------------------------------------------------
;---------------------------Drawing using the LEDs-------------------------------------------
;--------------------------------------------------------------------------------------------

							; BEGIN:clear_leds
clear_leds:
	addi t0, zero, LEDS 	; LEDS Address
	stw zero, 0(t0) 		; Address 2000
	stw zero, 4(t0) 		; Address 2004
	stw zero, 8(t0) 		; Address 2008
	ret
							; END:clear_leds

;--------------------------------------------------------------------------------------------

							; BEGIN:set_pixel
set_pixel:
	addi t4, zero, LEDS 	; LEDS Address

	andi t0, a0, 3 			; x mod 4
	slli t0, t0, 3 			; (x mod 4) * 8
	add  t0, t0, a1 		; (x mod 4) * 8 + y (LED bit number)

	addi t2, zero, 1 		; A single bit to be shifted t0 times
	sll t2, t2, t0 			; LED bit to be activated

	addi t1, zero, 4 		; If x >= 4 then don't set bit t0 of LEDS[0] to 1  
	bge a0, t1, set_pixel_more_than_four

	ldw t5, 0(t4)
	or t5, t5, t2
	stw t5, 0(t4)
	jmpi done

	set_pixel_more_than_four:

	addi t1, zero, 8 		; If x >= 8 then don't set bit t0 of LEDS[1] to 1
	bge a0, t1, set_pixel_more_than_eight
	
	ldw t5, 4(t4)
	or t5, t5, t2
	stw t5, 4(t4)
	jmpi done

	set_pixel_more_than_eight:

	ldw t5, 8(t4)
	or t5, t5, t2
	stw t5, 8(t4)

	ret
							; END:set_pixel

;--------------------------------------------------------------------------------------------

							; BEGIN:wait
wait:
	addi t1, zero, 1
	slli t1, t1, 22 		; 2^22 (fit better to 1 sec than 2^19 (post on moodle)) TODO REMOVE________
;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

	ldw t2, SPEED(zero) 			; Game_speed

	wait_count_loop: 		; Count from 2^22 to 0 by sub speed 
		sub t1, t1, t2
		bge t1, zero, wait_count_loop
	ret
							; END:wait

;--------------------------------------------------------------------------------------------
;----------------------------GSA Handling Procedures-----------------------------------------
;--------------------------------------------------------------------------------------------

							; BEGIN:get_gsa
get_gsa:					; to get A LINE
	addi sp, sp, -4			; since make call must save ra
	stw ra, 0(sp)

	call get_current_gsa
	slli a0, a0, 2			; word alignment
	add  t0, v0, a0			; calculate current line location (GSA start + y)
	ldw v0, 0(t0)			; get line

	ldw ra, 0(sp)
	addi sp, sp, 4
	ret
							; END:get_gsa

;--------------------------------------------------------------------------------------------

							; BEGIN:set_gsa
set_gsa:					; to set a LINE in the CURRENT GSA !!!!!
	addi sp, sp, -4			; since make call must save ra
	stw ra, 0(sp)
						
	call get_current_gsa
	slli t1, a1, 2			; word alignment
	add  t0, v0, t1			; calculate current line location (GSA start + y)
	stw a0, 0(t0)			; set line

	ldw ra, 0(sp)
	addi sp, sp, 4
	ret
							; END:set_gsa

;--------------------------------------------------------------------------------------------
;----------------------------From the GSA to the LEDS----------------------------------------
;--------------------------------------------------------------------------------------------

							; BEGIN:draw_gsa
draw_gsa:
	; clear_leds

	addi sp, sp, -4			; since make call must save ra
	stw ra, 0(sp)

	call clear_leds

	call get_current_gsa
	add  t0, zero, v0		; adr of curr GSA line (initialized at fisrt line of curr GSA)

	addi a1, zero, 0		; y counter
	addi t4, zero, N_GSA_LINES ; y max
	draw_gsa_line_loop:
		ldw t1, 0(t0)		; load line from current gsa	

		addi a0, zero, 0		; x counter
		addi t5, zero, N_GSA_COLUMNS ; x max
		draw_gsa_col_loop:
			
			andi t2, t1, 1		; retrieve first bit of gsa line
			beq t2, zero, dont_set_pixel ; if its 0 dont draw

			addi sp, sp, -24
			stw t0, 20(sp)
			stw t1, 16(sp)
			stw t4, 12(sp)
			stw t5, 8(sp)
			stw a0, 4(sp)
			stw a1, 0(sp)
			
			call set_pixel		; set pixel at position (a0, a1)

			ldw t0, 20(sp)
			ldw t1, 16(sp)
			ldw t4, 12(sp)
			ldw t5, 8(sp)
			ldw a0, 4(sp)
			ldw a1, 0(sp)
			addi sp, sp, 24

			dont_set_pixel:			

			srli t1, t1, 1	; shift current gsa line
			addi a0, a0, 1	; increment x
			bne a0, t5, draw_gsa_col_loop

		addi t0, t0, 4		; increment adr + 4 to switch to next line of gsa
		addi a1, a1, 1		; increment y
		bne a1, t4, draw_gsa_line_loop

	ldw ra, 0(sp)
	addi sp, sp, 4
	ret
							; END:draw_gsa

;--------------------------------------------------------------------------------------------
;----------------------------Using a random GSA----------------------------------------------
;--------------------------------------------------------------------------------------------

							; BEGIN:random_gsa
random_gsa:
							; For each line 0 - 7
							; For each column 0 - 11
							; Generate a random number and *or* it to the temporary line
							; Set_gsa with the temp at line 
	
	addi t7, zero, RANDOM_NUM	; Random_num address

	addi a1, zero, 0		; Counter 0-7	 
	addi t4, zero, 8		; Max line loops

	random_gsa_line_loop: 
		addi a0, zero, 0	; Line to set

		addi t0, zero, 0	; Counter 0-11
		addi t3, zero, 12	; Max col loops

		random_gsa_col_loop:
			ldw t1, 0(t7)	; Random number
			andi t1, t1, 1	; Mod 2 (keep only LSB)
			sll t1, t1, t0	; Shift random number left by col number
			or a0, a0, t1	; Or to stored line

			addi t0, t0, 1	; Increment col_loop counter
			blt t0, t3, random_gsa_col_loop
		
							; PUSH ONTO STACK : a0, a1, ra, t0, t1, t3, t4, t7
		addi sp, sp, -32
		stw a0, 28(sp)
		stw a1, 24(sp)			
		stw ra, 20(sp)
		stw t0,	16(sp)	
		stw t1, 12(sp)
		stw t3, 8(sp)
		stw t4, 4(sp)
		stw t7, 0(sp)

		call set_gsa		; Set GSA

							; POP OFF STACK : t7, t4, t3, t1, t0, ra, a1, a0
		ldw t7, 0(sp)
		ldw t4, 4(sp)
		ldw t3, 8(sp)
		ldw t1, 12(sp)
		ldw t0, 16(sp)
		ldw ra, 20(sp)
		ldw a1, 24(sp)
		ldw a0, 28(sp)
		addi sp, sp, 32			

		addi a1, a1, 1		; Increment line_loop counter
		blt a1, t4, random_gsa_line_loop			
	
	ret
							; END:random_gsa


;--------------------------------------------------------------------------------------------
;----------------------------Action Functions------------------------------------------------
;--------------------------------------------------------------------------------------------

							; BEGIN:change_speed
change_speed:
	ldw t0, SPEED(zero)
	addi t1, zero, MAX_SPEED
	addi t2, zero, MIN_SPEED
	bne a0, zero, decrement	;if a0 == 0 then increment else decrement (a0 == 1)	
		

								
	increment:
		beq t0, t1, done
		addi t0, t0, 1
		stw t0, SPEED(zero)
		ret
							

							
	decrement:
		beq t0, t2, done
		addi t0, t0, -1
		stw t0, SPEED(zero)
		ret
							; END:change_speed

;--------------------------------------------------------------------------------------------

							; BEGIN:pause_game
pause_game:
	ldw t0, PAUSE(zero)
	xori t0, t0, 1			; inverse PAUSE word
	stw t0, PAUSE(zero)
	ret
							; END:pause_game

;--------------------------------------------------------------------------------------------
							
							; BEGIN:change_steps
change_steps:  ; if we are at FFFF and add 1 one will go to 10000 but since look only the 4 LSBytes -> no prob
	ldw t4, CURR_STEP(zero)
	slli t0, a2, 8			;a2 = 1 if button2 (for hundreds) pressed else 0 
	slli t1, a1, 4			;a1 = 1 if button3 (for tens) pressed else 0 
							;a0 = 1 if button4 (for units) pressed else 0 (then don't need to shift)
	add t0, t0, t1
	add t0, t0, a0			;sum = hundred + ten + unit + curr_step
	add t4, t4, t0
	andi t4, t4, 0xFFF		; to don't have hidden nb of thousands
	stw t4, CURR_STEP(zero)
	
	ret
							; END:change_steps

;--------------------------------------------------------------------------------------------

							; BEGIN:increment_seed
increment_seed:
	ldw t0, CURR_STATE(zero)	;load the value of the current state 
	ldw t1, SEED(zero)			;load the index of the currently used seed
	addi t2, zero, INIT			;load index corresponding to INIT state
	addi t3, zero, RAND			;load index corresponding to RAND state
	addi t4, zero, N_SEEDS		;number of predifiened seeds
	
	;compare index to determine state and then needed operations
	beq t0, t2, increment_seed_init
	beq t0, t3, increment_seed_rand		;useless since increment_seed called only in init state
	ret							; if curr state != RAND or Init => = Run -> do nothing

								
	increment_seed_init:
		addi t1, t1, 1
		stw t1, SEED(zero)			; store updated index of the Seed
		beq t1, t4, increment_seed_rand ;in currSeed is 4 = N -> fill gsa with random seed (no more predefined ones)
		add a3, zero, t1
		br fill_gsa_with_given_seed	;fill curr GSA with the curr seed
																
									
	increment_seed_rand:
	br random_gsa
								
							; END:increment_seed

;--------------------------------------------------------------------------------------------

							; BEGIN:update_state
update_state:
	ldw  t0, CURR_STATE(zero)	;load the value of the current state 
	addi t1, zero, INIT			;load index corresponding to INIT state
	addi t2, zero, RAND			;load index corresponding to RAND state
	addi t3, zero, RUN			;load index corresponding to RUN state
	addi t4, zero, N_SEEDS		;load the number of seed (the fisrt n correspond to predifinied ones)
	ldw  t5, SEED(zero)			;load the index of the currently used seed

	beq t0, t1, update_state_INIT
	beq t0, t2, update_state_RAND
	beq t0, t3, update_state_RUN
	ret								;might be useless if everything runs correctly

	update_state_INIT:
		andi t6, a0, 1				; 1 if b0 is pressed, 0 otherwise
		andi t7, a0, 2				; 1 if b1 is pressed, 0 otherwise
		bne t7, zero, update_state_INIT_to_RUN ; if b1 is pressed, switch to RUN state
		beq t6, zero, done			;else if b0 is not pressed do nothing
		blt t5, t4, done			;else if current seed index < n stay in INIT state
		stw t2, CURR_STATE(zero)	;else go to RAND state
		ret

		update_state_INIT_to_RUN:
			stw t3, CURR_STATE(zero)
			addi t7, zero, RUNNING	;if go to run state, automaticaly set game to run
			stw t7, PAUSE(zero) 
			ret

	update_state_RAND:
		andi t6, a0, 2				;1 if b1 is pressed, 0 otherwise
		beq t6, zero, done			;if b1 is pressed then go to RUN state
		stw t3, CURR_STATE(zero)	;set current state to RUN
		addi t7, zero, RUNNING	;if go to run state, automaticaly set game to run
		stw t7, PAUSE(zero)
		ret

	update_state_RUN:
		andi t6, a0, 8				;1 if b3 is pressed, 0 otherwise
		beq t6, zero, done			;if b3 is pressed then go to INIT state
		stw t1, CURR_STATE(zero)	;set current state to INIT
		br reset_game				;have changed of state so we need to call reset_game
		;ret function will be called at the end of reset_game procedure
		;don't use call here because we don't need to come back to this line after the reset procedure
							; END:update_state

;--------------------------------------------------------------------------------------------

							; BEGIN:select_action
select_action:
	addi sp, sp, -24					
	stw ra, 0(sp)					;save stack pointers and a few saved registers 
	stw s0, 4(sp)					;since gonna make several calls to other funct
	stw s1, 8(sp)
	stw s2, 12(sp)
	stw s3, 16(sp)
	stw s4, 20(sp)

	srli s0, a0, 0					;to extract bit corresponding to b0
	andi s0, s0, 1
	srli s1, a0, 1					;to extract bit corresponding to b1
	andi s1, s1, 1
	srli s2, a0, 2					;to extract bit corresponding to b2
	andi s2, s2, 1
	srli s3, a0, 3					;to extract bit corresponding to b3
	andi s3, s3, 1
	srli s4, a0, 4					;to extract bit corresponding to b4
	andi s4, s4, 1

	
	ldw t5, CURR_STATE(zero)		;load curr state and values corresponding to the states
	addi t6, zero, RAND
	addi t7, zero, RUN
	
	beq t5, t6, select_action_RAND ;depending on the state, the actions for each button will be differents
	beq t5, t7, select_action_RUN
	;if not in rand or run state => curr state = init



	select_action_INIT:
		beq s0, zero, select_action_INIT_b0_done
											;do nothing if b1 pressed, update_state call after select_action in main
			call increment_seed				; if b0 pushed -> increment seed

		select_action_INIT_b0_done:
			or t6, s2, s3					;3 buttons states -> need only 1 push
			or t6, t6, s4
			beq t6, zero, endPopStack			; if neither b2,b3,b4 pushed -> no more action to perfrom
		
			add a0, zero, s4	;a0 correspond to b4
			add a1, zero, s3	;a1 correspond to b3
			add a2, zero, s2	;a2 correspond to b2
			call change_steps	; if b2, b3 or b4 pushed -> change step
			br endPopStack
		
	

	select_action_RAND:
		beq s0, zero, select_action_RAND_b0_done
		
			call random_gsa					;if b0 pressed, upload gsa with new random seed

		select_action_RAND_b0_done:

			or t6, s2, s3
			or t6, t6, s4
			beq t6, zero, select_action_RAND_b234_done

			add a0, zero, s4	;a0 correspond to b4
			add a1, zero, s3	;a1 correspond to b3
			add a2, zero, s2	;a2 correspond to b2
			call change_steps				; if either b2,b3,b4 is pushed down -> change step

		select_action_RAND_b234_done:
			beq s1, zero, endPopStack
			call pause_game					;if b1 pressed -> launch game from the selected inital state, nb step, ...
			br endPopStack						; all operations that might be executed in rand state are over



	select_action_RUN:
		beq s0, zero, select_action_RUN_b0_done	;if b0 down, game toggles between play/pause
			call pause_game

		select_action_RUN_b0_done:
			or t6, s1, s2
			beq t6, zero, select_action_RUN_b12_done
			add a0, zero, s2					; if b2 push -> decrease -> a0 = 1 
					;else mean that it is b1 that has be pushed and we want a0 = 0 (then = b2)
			call change_speed					;if b1 or b2 down -> change speed

		select_action_RUN_b12_done:
			beq s4, zero, endPopStack
			call random_gsa						;if b4 down, change gsa with new random seed

			;if b3 down change state to INIT -> handle by update_state



	endPopStack:			; have to pop previously saved ra to use ret to quit funct
							
		ldw ra, 0(sp)					;save stack pointers since gonna make several calls to other funct
		ldw s0, 4(sp)
		ldw s1, 8(sp)
		ldw s2, 12(sp)
		ldw s3, 16(sp)
		ldw s4, 20(sp)
		addi sp, sp, 24
		ret
						; END:select_action

;--------------------------------------------------------------------------------------------
;---------------------------Updating the GSA-------------------------------------------------
;--------------------------------------------------------------------------------------------

							; BEGIN:cell_fate
cell_fate:
	addi v0, zero, 0		; Cell fate (dies by default)
	beq a1, zero, cell_dead_fate

	cell_living_fate:		; Living cell actually lives if neighbours = 2 or 3
		cmpeqi t0, a0, 2
		or v0, v0, t0
		cmpeqi t0, a0, 3
		or v0, v0, t0
		jmpi cell_fate_end


	cell_dead_fate:			; Dead cell actually lives if neighbours = 3
		cmpeqi t0, a0, 3
		or v0, v0, t0

	cell_fate_end:
	ret
							; END:cell_fate
	
;--------------------------------------------------------------------------------------------

							; BEGIN:find_neighbours
find_neighbours:
	addi t0, zero, 0		; Neighbour_memory (9 bits)
	addi t1, zero, 0		; Neighbour_sum

	addi t2, a1, -1			; Line counter y
	addi t3, a1, 2			; Line max	
	find_neighbours_outer_loop: ; For lines y - 1, to y + 1
							; PUSH ONTO STACK : t0, t1, t2, t3, a0, a1, ra
		addi sp, sp, -28			
		stw t0, 24(sp)
		stw t1,	20(sp)	
		stw t2, 16(sp)
		stw t3, 12(sp)
		stw a0, 8(sp)
		stw a1, 4(sp)
		stw ra, 0(sp)
		
		andi a0, t2, 7		; y mod 8
		call get_gsa		; a0 = y mod 8

							; POP OFF STACK : ra, a1, a0, t3, t2, t1, t0
		ldw ra, 0(sp)
		ldw a1, 4(sp)
		ldw a0, 8(sp)
		ldw t3, 12(sp)
		ldw t2, 16(sp)
		ldw t1, 20(sp)
		ldw t0, 24(sp)
		addi sp, sp, 28	
						
		addi t4, a0, -1 	; Column counter x
		addi t5, a0, 2		; Column max
		find_neighbours_inner_loop: ; For columns x - 1 to x + 1

			; PERFORM t6 <- t4 mod 12
			addi t7, zero, 12 							; t7 <- 12
			add t6, t4, zero							; t6 <- t4
			bge t4, zero, find_neighbours_x_positive	; if (x >= 0) break	 
			add t6, t6, t7								; else t6 <- x + 12
			jmpi find_neighbours_x_valid				; now valid x coordinate
			find_neighbours_x_positive:
				blt t4, t7, find_neighbours_x_valid		; if (x < 12) break
				sub t6, t6, t7							; else t6 <- x - 12
			find_neighbours_x_valid:					; t6 <- x mod 12
			
			srl t7, v0, t6  ; v0[0] is now gsa[x mod 12]
			andi t7, t7, 1  ; Keep only LSB
			slli t0, t0, 1  ; Shift neighbour_memory left
			or t0, t0, t7   ; Store in neighbour_memory
			add t1, t1, t7	; Add gsa[x mod 12] (either 1 or 0) to neighbour_sum		
				
			addi t4, t4, 1  ; Increment column counter
			blt t4, t5, find_neighbours_inner_loop

		addi t2, t2, 1		; Increment line counter
		blt t2, t3, find_neighbours_outer_loop
		
	srli t0, t0, 4			; Shift 4 (location of considered cell)
	andi v1, t0, 1			; v1 is neighbour_memory[4]	
	sub v0, t1, v1  		; v0 is neighbour_sum - v1
	ret
							; END:find_neighbours

;--------------------------------------------------------------------------------------------

							; BEGIN:update_gsa
update_gsa:
	ldw t0, PAUSE(zero)	; Do nothing if game is paused
	addi t7, zero, PAUSED ; PAUSED VALUE
	beq t0, t7, done

	; Get next_gsa address
	ldw t0, GSA_ID(zero)	; Load flag which indicate current GSA (0 or 1)
	xori t0, t0, 1			; Invert the GSA_ID bit
	slli t0, t0, 5 			; Size GSA0
	addi t0, t0, GSA0   	; t0 is next GSA adr (GSA0 or (GSA0 + 32) = GSA1)
	
	; Loop through cells
	addi a1, zero, 0		; Line counter and y coordinate
	addi t1, zero, 8		; Line max	
	update_gsa_outer_loop: ; For lines [0, 8[
		
		addi t3, zero, 0		; Temp line to store update
		addi a0, zero, 0		; Column counter and x coordinate
		addi t2, zero, 12		; Column max
		update_gsa_inner_loop:	; For cells [0, 12[
		
		addi sp, sp, -28		; PUSH ONTO STACK : t0, t1, t2, t3, a0, a1, ra
		stw t0, 24(sp)
		stw t1,	20(sp)	
		stw t2, 16(sp)
		stw t3, 12(sp)
		stw a0, 8(sp)
		stw a1, 4(sp)
		stw ra, 0(sp)
		
		call find_neighbours	; Find neighbours
		add a0, v0, zero		; Number of living neighbours
		add a1, v1, zero		; State of the examined cell
		call cell_fate			; Fate of the examined cell is in v0

		ldw ra, 0(sp)			; POP OFF STACK : ra, a1, a0, t3, t2, t1, t0
		ldw a1, 4(sp)
		ldw a0, 8(sp)
		ldw t3, 12(sp)
		ldw t2, 16(sp)
		ldw t1, 20(sp)
		ldw t0, 24(sp)
		addi sp, sp, 28	

		sll v0, v0, a0			; Shift (1 if the cell is alive 0 otherwise) x times
		or t3, t3, v0

		addi a0, a0, 1			; Increment column counter and x coordinate
		blt a0, t2, update_gsa_inner_loop

	slli t5, a1, 2			; must be word aligned
	add  t5, t0, t5			; Calculate next line location (GSA start + y)
	stw t3, 0(t5)			; Set line

	addi a1, a1, 1		; Increment line counter and y coordinate
	blt a1, t1, update_gsa_outer_loop

	; Inverts the GSA ID
	ldw t0, GSA_ID(zero)	; Load flag which indicate current GSA (0 or 1)
	xori t0, t0, 1			; Invert the GSA_ID bit
	stw t0, GSA_ID(zero)	; Store flag with inverted bit

	ret
							; END:update_gsa

;--------------------------------------------------------------------------------------------

							; BEGIN:mask
mask:
	ldw t0, SEED(zero)		; Load seed number
	slli t0, t0, 2			; Seed number 0 < s <= 4 -> Mask index 0 < i <= 16
	ldw t0, MASKS(t0)		; Get associated mask
	
	; Loop through lines
	addi a0, zero, 0		; Line counter and y coordinate
	addi t1, zero, 8		; Line max	
	mask_loop: 				; For lines [0, 8[
		
		slli t5, a0, 2
		add t2, t0, t5		; Calculate mask line
		ldw t2, 0(t2)		; Load mask line

		addi sp, sp, -20	; PUSH ONTO STACK : t0, t1, ra, a0, t2
		stw t0, 16(sp)
		stw t1,	12(sp)	
		stw ra, 8(sp)
		stw a0, 4(sp)
		stw t2, 0(sp)

		call get_gsa		 

		ldw t2, 0(sp)		; POP OFF STACK : t2, a0 (now a1)
		ldw a1, 4(sp)
		addi sp, sp, 8

		addi a0, v0, 0		; The line returned by get_gsa
		and a0, a0, t2		; Perform mask

		addi sp, sp, -8		; PUSH ONTO STACK : a1 (was a0), t2
		stw a1, 4(sp)
		stw t2, 0(sp)		

		call set_gsa

		ldw t2, 0(sp)			; POP OFF STACK : t2, a0, ra, t1, t0
		ldw a0, 4(sp)
		ldw ra, 8(sp)
		ldw t1, 12(sp)
		ldw t0, 16(sp)
		addi sp, sp, 20

		addi a0, a0, 1			; Increment line counter and y coordinate
		blt a0, t1, mask_loop

ret
							; END:mask


;---------------------------------------------------------------------------------------------------
;---------------------------------------INPUTS TO THE GAME------------------------------------------
;---------------------------------------------------------------------------------------------------
				; BEGIN:get_input
get_input:
	ldw v0, (BUTTONS+4)(zero)		;load values of edgecapture from memory to a register
	stw zero, (BUTTONS+4)(zero)		;reset (set to 0) the edgecapture in memory
	ret
				; END:get_input		

;----------------------------------------------------------------------------------------------------
;----------------------------------------Game step handling-----------------------------------------
;----------------------------------------------------------------------------------------------------

				; BEGIN:decrement_step
decrement_step:
	add v0, zero, zero				;initialize v0 to 0 (return value if curr state is notP RUN or curr step is not 0

	ldw a1, CURR_STEP(zero)
	ldw t7, CURR_STATE(zero)
	addi t2, zero, RUN
	bne t2, t7, display_on_7seg		; if curr state is Init or Random, do nothing and return 0


	ldw t0, PAUSE(zero)
	addi t1, zero, PAUSED
	beq t0, t1, display_on_7seg

	beq a1, zero, step_equ_0 ; if run && curr step is 0
									; else :
	addi a1, a1, -1					; decrement step
	stw a1, CURR_STEP(zero)			; save CURR_STEP
	br display_on_7seg


	step_equ_0:
		addi v0, zero, 1			; if in run state AND nb step = 0 return 1 (in all others cases return 0)
		ret							; don't call display_on_7seg since val on digits are still 0, have not changed	
	
				; END:decrement_step

;----------------------------------------------------------------------------------------------------
;--------------------------------------------RESET---------------------------------------------------
;----------------------------------------------------------------------------------------------------
				; BEGIN:reset_game
reset_game:
	addi sp, sp, -4				; PUSH ONTO STACK : ra
	stw ra, 0(sp)

	addi t1, zero, 1			;constant used to reset, 1 == currStep
	stw t1, CURR_STEP(zero)		;set current step to 1
	add a1, t1, zero
	call display_on_7seg		;display curr step on 7seg

	stw zero, SEED(zero)		;set curr seed to 0
	stw zero, GSA_ID(zero)		;set GSA id to 0
	
	addi t4, zero, INIT		
	stw t4, CURR_STATE(zero) 	; set curr state to INIT

	addi t3, zero, PAUSED
	stw t3, PAUSE(zero)			;set game to paused

	addi t1, zero, 1			;constant used to reset, 1 == MIN_SPEED
	stw t1, SPEED(zero)			;set Game speed to 1 (the min speed)

	add a3, zero, zero			;want to set GSA to seed0
	call fill_gsa_with_given_seed

	call mask					;mask with seed0

	call draw_gsa

	ldw ra, 0(sp)				; POP OFF STACK : ra
	addi sp, sp, 4
	
	ret
				; END:reset_game

;------------------------------------------------------------------------------------------------
font_data:
    .word 0xFC ; 0
    .word 0x60 ; 1
    .word 0xDA ; 2
    .word 0xF2 ; 3
    .word 0x66 ; 4
    .word 0xB6 ; 5
    .word 0xBE ; 6
    .word 0xE0 ; 7
    .word 0xFE ; 8
    .word 0xF6 ; 9
    .word 0xEE ; A
    .word 0x3E ; B
    .word 0x9C ; C
    .word 0x7A ; D
    .word 0x9E ; E
    .word 0x8E ; F


seed0:
    .word 0xC00
    .word 0xC00
    .word 0x000
    .word 0x060
    .word 0x0A0
    .word 0x0C6
    .word 0x006
    .word 0x000

seed1:
    .word 0x000
    .word 0x000
    .word 0x05C
    .word 0x040
    .word 0x240
    .word 0x200
    .word 0x20E
    .word 0x000

seed2:
    .word 0x000
    .word 0x010
    .word 0x020
    .word 0x038
    .word 0x000
    .word 0x000
    .word 0x000
    .word 0x000

seed3:
    .word 0x000
    .word 0x000
    .word 0x090
    .word 0x008
    .word 0x088
    .word 0x078
    .word 0x000
    .word 0x000


    ;; Predefined seeds
SEEDS:
    .word seed0
    .word seed1
    .word seed2
    .word seed3

mask0:
	.word 0xFFF
	.word 0xFFF
	.word 0xFFF
	.word 0xFFF
	.word 0xFFF
	.word 0xFFF
	.word 0xFFF
	.word 0xFFF

mask1:
	.word 0xFFF
	.word 0xFFF
	.word 0xFFF
	.word 0xFFF
	.word 0xFFF
	.word 0x1FF
	.word 0x1FF
	.word 0x1FF

mask2:
	.word 0x7FF
	.word 0x7FF
	.word 0x7FF
	.word 0x7FF
	.word 0x7FF
	.word 0x7FF
	.word 0x7FF
	.word 0x7FF

mask3:
	.word 0xFFF
	.word 0xFFF
	.word 0xFFF
	.word 0xFFF
	.word 0xFFF
	.word 0xFFF
	.word 0xFFF
	.word 0x000

mask4:
	.word 0xFFF
	.word 0xFFF
	.word 0xFFF
	.word 0xFFF
	.word 0xFFF
	.word 0xFFF
	.word 0xFFF
	.word 0x000

MASKS:
    .word mask0
    .word mask1
    .word mask2
    .word mask3
    .word mask4
