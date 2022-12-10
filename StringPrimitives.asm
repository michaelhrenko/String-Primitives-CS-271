

TITLE Designing Low-Level I/O Procedures	(Proj6_mhrenko.asm)

; --------------------------------------------------------------------------------------------------
;
; Author: Michael Hrenko
; Last Modified: 03/13/2022
; OSU email address: hrenkom@oregonstate.edu
; Course number/section: CS271 / Section 402
; Project Number: 6, Due Date: 03/13/2022
; Description: This program takes ten signed integers from a user, reads them in as strings,
;	converts them to integers, validates they are valid (not characters or too big for SDWORD), then
;	displays all the signed integers, the sum, and the average.
;
; Implementation notes:
;	This program is implemented using procedures and macros.
;	All global constant values are static.
;	All parameters are passed to the stack.
;	This program does not use the Irving procedures ReadInt, ReadDec, WriteInt, or WriteDec.
;	Per the requirements, I assumed that the total sum of the valid numbers will fit inside a 32-bit register.
;
; --------------------------------------------------------------------------------------------------

INCLUDE Irvine32.inc

; --------------------------------------------------------------------------------------------------
; Section: mGetString macro
;
; Purpose: Displays a string passed by another procedure, and gets the user value.
;
; Preconditions: 
;	somePrompt is populated with a valid string.
;	userString and userStringLength are passed as references.
;	maxStringLength is populated with a valid value. 
;
; Postconditions:
;	The value in the memory address that's associated with the userString parameter is updated.
;	The value in the memory address that's associated with the userStringLength parameter is updated.
;	eax, ecx, and edx are changed and restored.
;
; Receives:
;	somePrompt			= Placeholder for a string passed when calling the macro (input, reference).
;	maxStringLength		= Placeholder for the max allowable length of the user value (input, value).
;
; Returns: 
;  userString			= Placeholder for the user value string (output, reference).
;  userStringLength		= Placeholder for the actual length of the user value string (output, reference).
;
; --------------------------------------------------------------------------------------------------

mGetString MACRO somePrompt, userString, maxStringLength, userStringLength

	push	eax
	push	ecx
	push	edx

	mov		edx, somePrompt
	call	WriteString
	mov		ecx, maxStringLength									
	mov		edx, userString								
	call	ReadString
	mov		userStringLength, eax						; EAX holds number of characters entered

	pop		edx
	pop		ecx
	pop		eax

ENDM

; --------------------------------------------------------------------------------------------------
; Section: mDisplayString macro
;
; Purpose: Displays a string passed by another procedure. 
;
; Preconditions: 
;	someString is populated with a valid string.
;
; Postconditions: 
;	edx is changed and restored.
;
; Receives:
;	someString			= Placeholder for a string passed when calling the macro (input, reference).
;
; Returns: none
;
; --------------------------------------------------------------------------------------------------

mDisplayString MACRO someString

	push	EDX

	mov		EDX, someString
	call	WriteString

	pop		EDX

ENDM

; --------------------------------------------------------------------------------------------------
; Section: Setup
; Setup identifiers, their data types, and initialize values and strings.
; --------------------------------------------------------------------------------------------------

ARRAY_LENGTH = 10													; Count of user values.
MAX_STRING_LENGTH = 100												; Max length of characters that can be read in.
																	;	99 characters plus null 0 at the end.
MAX_VALID_DIGITS = 10												; Max length of valid numbers (doesn't include negative sign). 
SDWORD_SIZE_MIN = -2147483648										; Smallest valid SDWORD intege: -(2^31) = -2147483648.
SDWORD_SIZE_MAX = 2147483647										; Largest valid SDWORD integer: (2^31) - 1 = 2147483647.

.data
promptWelcome				BYTE	"Designing low-level I/O procedures by Michael Hrenko",13,10,13,10,0
promptIntroduction			BYTE	"This program takes 10 signed integers that are small enough to fit inside a 32 bit register ",13,10
							BYTE	"then displays a list of the integers, their sum, and their average value.",13,10,13,10,0
promptEnterValue			BYTE	"Please enter an signed integer: ",0
promptInvalidValue			BYTE	"ERROR: You did not enter an signed number or your number was too big.",0
promptAllUserValues			BYTE	"You entered the following numbers:",13,10,0
promptSum					BYTE	"The sum of these numbers is: ",0
promptAverage				BYTE	"The truncated average is: ",0
promptGoodbye				BYTE	"Goodbye, and thanks for using this program!",13,10,0

userValue					BYTE	MAX_STRING_LENGTH DUP(?)		; User value string; 99 characters plus the null 0 at the end.
userValueLength				DWORD	?								; Actual length of userValue.
userValueIntArray			SDWORD	ARRAY_LENGTH DUP(?)				; Array for all userValue's.
lengthUserValueIntArray		DWORD	LENGTHOF userValueIntArray		; Length of userValueIntArray.
displayStrArray				BYTE	12 DUP(?)						; Temporary memory for storing the string before displaying it.
																	;	11 characters plus null 0 at the end.
sum							SDWORD	?								; Stores the sum.
average						SDWORD	?								; Stores the average value.						
isNegative					DWORD	0								; Parameter to keep track of whether or not the value is negative. 

.code

; --------------------------------------------------------------------------------------------------
; Section: Main procedure to call other procedures.
; --------------------------------------------------------------------------------------------------

main PROC

; --------------------------------------------------------------------------------------------------
; Sub-section: Welcome and intro message.
; --------------------------------------------------------------------------------------------------

	mDisplayString	OFFSET promptWelcome					; "Designing low-level..."
	mDisplayString	OFFSET promptIntroduction				; "This program takes 10..."

; --------------------------------------------------------------------------------------------------
; Sub-section: Loop to call readVal procedure
;
; Purpose: Calls the readVal procedure which reads and transforms the strings to integers
;	and stores them in userValueIntArray.
;
; Preconditions: 
;	userValueIntArray is initialized as an array with ARRAY_LENGTH elements of size SDWORD.
;
; Postconditions: 
;	userValueIntArray is incremented by 4 (TYPE SDWORD) before each loop runs again.
;	edi and ecx are changed and restored.
;
; Receives:
;	lengthUserValueIntArray		= length of userValueIntArray (input, value).
;
; Returns:
;	[edi]						= userValueIntArray (output, reference), array to store integers
;
; --------------------------------------------------------------------------------------------------

	push	edi
	push	ecx

	mov		edi, OFFSET userValueIntArray					; Destination for valid user values.
	mov		ecx, lengthUserValueIntArray					; Loop counter.

	_readValLoop:
			push	edi
			push	ecx

			push	OFFSET isNegative
			push	edi										; Memory location for userValueIntArray.
			push	OFFSET userValueLength
			push	OFFSET userValue
			push	OFFSET promptInvalidValue
			push	OFFSET promptEnterValue
			call	readVal

			pop		ecx										; Restore ecx for the next loop. 
			pop		edi										; Restore edi for the next loop.
			add		edi, TYPE userValueIntArray				; Move to next memory location.

		loop	_readValLoop

	call    CrLf

	pop		ecx
	pop		edi

; --------------------------------------------------------------------------------------------------
; Sub-section: Calculate and store the sum.
; --------------------------------------------------------------------------------------------------

	push	lengthUserValueIntArray
	push	OFFSET sum
	push	OFFSET userValueIntArray
	call	calcSum

; --------------------------------------------------------------------------------------------------
; Sub-section: Calculate and store the average.
; --------------------------------------------------------------------------------------------------

	push	lengthUserValueIntArray
	push	OFFSET average	
	push	sum
	call	calcAverage

; --------------------------------------------------------------------------------------------------
; Sub-section: Display the user values (calls writeVal as a sub-procedure).
; --------------------------------------------------------------------------------------------------

	mDisplayString	OFFSET promptAllUserValues				; "You entered..."

	push	lengthUserValueIntArray		
	push	OFFSET displayStrArray
	push	OFFSET userValueIntArray
	call	displayAllUserValues
	call    CrLf

; --------------------------------------------------------------------------------------------------
; Sub-section: Display the sum.
; --------------------------------------------------------------------------------------------------

	mDisplayString	OFFSET promptSum						; "The sum of these numbers..."

	push	OFFSET displayStrArray
	push	sum
	call	writeVal
	call    CrLf
	call    CrLf

; --------------------------------------------------------------------------------------------------
; Sub-section: Display the average.
; --------------------------------------------------------------------------------------------------

	mDisplayString OFFSET promptAverage						; "The truncated average..."

	push	OFFSET displayStrArray
	push	average
	call	writeVal
	call    CrLf
	call    CrLf

; --------------------------------------------------------------------------------------------------
; Sub-section: Display the final greeting.
; --------------------------------------------------------------------------------------------------

	mDisplayString OFFSET promptGoodbye						; "Goodbye, and thanks..."

; --------------------------------------------------------------------------------------------------
; Sub-section: End the program. 
; --------------------------------------------------------------------------------------------------

	Invoke ExitProcess, 0									; exit to operating system

main ENDP

; --------------------------------------------------------------------------------------------------
; Section: Read, validate, transform, and store the user values.
;
; Purpose / Steps: 
;	(1) Call the mGetString macro to prompt user to enter a value.
;	(2) Store that value temporarily in memory as a string.
;	(3) Validate that all characters are in the range [0-9] with a possible leading '+' or '-'
;		symbol to denote a positive or negative value, respectively. 
;	(4) Remove any leading 0's.
;	(5) Convert the string to a SDOWRD integer.
;	(6) Validate that the integer fits into the size boundaries of a SDOWRD integer.
;	(7) Store the integer in userValueIntArray.
;	
; Preconditions: 
;	userValue is initialized as an array with 100 elements of size BYTE.
;	The current memory location of userValueIntArray is initialized and updated in the parent loop in main.
;	isNegative is initialized to 0.
;
; Postconditions: 
;	User is shown error message when the current value is invalid and prompted to enter a value until a valid 
;		value is entered. 
;	isNegative is reset to 0.
;	ebp, esi, edi, eax, ebx, ecx, and edx are changed and restored.
;
; Receives:
;	MAX_STRING_LENGTH			= Global constant for max length of userValue.
;	MAX_VALID_DIGITS			= Global constant for max length of valid numbers (EXCLUED negative sign).
;	SDWORD_SIZE_MIN				= Global constant for the smallest valid SDWORD integer.
;	SDWORD_SIZE_MAX				= Global constant for the largest valid SDWORD integer.
;	[ebp+8]						= promptEnterValue (input, reference), message.
;	[ebp+12]					= promptInvalidValue (input, reference), message.
;	[ebp+16]					= userValue (input, reference), array for temporarily storing user's string.
;	[ebp+20]					= userValueLength (input, reference), count of actual elements in userValue.
;	[ebp+28]					= isNegative (input, reference), boolean used as identifier for negative value.
;
; Returns:
;	[ebp+24]					= current memory location of userValueIntArray (output, reference), incremented in the parent loop
;	[ebp+28]					= isNegative (output, reference), boolean used as identifier for negative value
;
; Implementation notes:
;	EDX is used as a counter for valid integers. The program accepts 100 characters, but only 
;		keeps 11 valid integers (including the leading minus symbol).
;
; Invalid user values:
;	(1) Any characters not in the range [0, 9]. A leading plus or minus symbol is allowed.
;	(2)	Satisfies #1, but the total number of digits is greater than 10 (the max allowed for SDWORD).
;		Leading 0's and a leading minus symbol are not included in this count. 
;	(3) Satisfies #1 and #2, but the value is beyond the min or max allowed for SDWORD.
;
; --------------------------------------------------------------------------------------------------

readVal PROC

	push	ebp
	mov		ebp, esp

	push	esi
	push	edi
	push	eax
	push	ebx
	push	ecx
	push	edx	

_getValue:

	mov		edi, [ebp+24]									; Current memory location of userValueIntArray.
	mov		esi, [ebp+16]									; Memory address of userValue.
	
	mGetString [ebp+8], esi, MAX_STRING_LENGTH, [ebp+20]
 
	mov		ecx, [ebp+20]									; userValueLength value
	mov		SDWORD PTR [edi], 0								; Initialize value in userValueIntArray to 0. This is needed 
															;	since we're storing the accumulated value in edi and may
															;	already have an integer in there because the string was
															;	invalid. 		
	mov		eax, 0											; Initialize eax.
	mov		ebx, 0											; Initialize ebx.
	mov		edx, 0											; Initialize edx.

; Check for leading plus or minus symbol.
	cld														; Clear the direction so esi increments. 
	lodsb													
	cmp		al, 45											; 45 = '-' symbol in ASCII, implied subtraction (al - 45)
	je		_hasMinusSymbol									
	cmp		al, 43											; 43 = '+' symbol in ASCII, implied subtraction (al - 43)
	je		_hasPlusSymbol									
	
	dec		esi												; Making it to this step means there is no minus or plus symbol.  
															; Since lodsb already incremented esi 1 BYTE, decrement esi 
															;	so we don't eliminate the first character in the next step.
	jmp		_checkForLeadingZero

_hasMinusSymbol:
	; Has minus ('-') symbol so set the isNegative boolean to 1 for later.

	dec		ecx												; Decrease ecx because our counter is only for the remaining 
															;	characters after the first '-' symbol.
	push	edi
	mov		edi, [ebp+28]									; isNegative boolean.
	mov		DWORD PTR [edi], 1								; Set isNegative value to 1.  
	pop		edi

	jmp		_checkForLeadingZero

_hasPlusSymbol:
	; Has plus ('+') symbol. Don't show the + sign.

	dec		ecx												; Decrease ecx because our counter is only for the remaining 
															;	characters after the first '+' symbol.

_checkForLeadingZero:
	; leading zeros are allowed. Remove them by decrementing ecx.

	cmp		ecx, 1											; Don't remove if the only character is '0'
	je		_accumulateByteValues							
	cld														; Clear the direction so esi increments. 
	lodsb													
	cmp		al, 48											; 48 = integer 0 in ASCII, implied subtraction (al - 48)
	jne		_noLeadingZero
	dec		ecx
	jmp		_checkForLeadingZero

_noLeadingZero:
	dec		esi												; Since lodsb already incremented esi 1 BYTE, decrement esi 
															;	so we don't eliminate the first character in the next step.

	_accumulateByteValues:
		; Validate that the remaining characters are numbers in range [0, 9].
		; If any character is not in this range, the whole string is invalid.

			cld												; Clear the direction so esi increments. 
			lodsb													
			cmp		al, 48									; 48 = integer 0 in ASCII, implied subtraction (al - 48)
			jb		_invalid								; < 48, not a value in [0-9].
			cmp		al, 57									; 57 = integer 9 in ASCII, implied subtraction (al - 57)
			ja		_invalid								; > 57, not a value in [0-9].

		; Before seeking to add the value, make sure it doesn't cause an overflow.
		;	This must be done before passing to the DWORD register because the value could be truncated and 
		;	seem to fit within the bounds of SDWORD [-2^31, 2^31-1]. 

			inc		edx										; Add 1 for every valid value attempted to be added.
			push	edx										; Store value in stack since it's getting used below in the 
															;	multiplication step.
			cmp		edx, MAX_VALID_DIGITS
			ja		_tooManyDigits

			mov		edx, 0									; Reset value to 0 for multiplication step.

		; Transform the ASCII value to an integer using this recursive formula:
		;	for numberCharacter in userValue:
		;		numberInteger =  10 * numberInteger + (numberCharacter - 48)
		;	where numberInteger is initialized to 0.
		; For example, the string '123' is converted to 123 by these steps:
		;	iteration 1: numberInteger = 10 * 0 + (49 - 48) = 1
		;	iteration 2: numberInteger = 10 * 1 + (50 - 48) = 10 + 2 = 12
		;	iteration 3: numberInteger = 10 * 12 + (51 - 48) = 120 + 3 = 123

			sub		al, 48
			movzx	eax, al									; Zero extend the BYTE value and put it into eax.
			push	eax										; Store the current BTYE value in eax because we're going to add
															;	it to the prior value which will also be stored in eax.
			mov		eax, [edi]								; Move the accumulated integer into eax. 
			mul		ebx										; Multiply accumulated integer by 0, 10, 100,...
			mov		[edi], eax								; Move the new accumulated integer back into edi. 
			pop		eax										; Restore the current BTYE value. 
			add		[edi], eax								; Add the current BTYE value to the accumulated integer.
			mov		ebx, 10									; Update ebx to 10 after the first iteration where ebx is initialized to 0.
			pop		edx										; Restore valid value counter for next loop.
		loop 	_accumulateByteValues

	mov		eax, [edi]										; Accumulated valid value, before checking if it's too large for a
															;	SDWORD register. 

; Check if the integer if the integer needs to be negated.
	push	edi
	push	ebx
	mov		edi, [ebp+28]									; isNegative boolean.
	mov		ebx, [edi]
	cmp		ebx, 1											; Implied subtraction (ebx - 1)
	je		_convertToNegative
	pop		ebx
	pop		edi

	jmp		_checkPositiveSizeLimit							; Value isn't negative. 

_convertToNegative:
	; Value is negative so negate the integer.
	pop		ebx
	pop		edi
	neg		eax
	mov		[edi], eax										; Store negated value in [edi].

; These size checks below are different than the valid value counter above as this value
;	may have 11 digits, but be larger than the min/max valid SDWORD value. 

; Check if negative value fits in SDWORD register.
	cmp		eax, SDWORD_SIZE_MIN							;  -(2^31) = -2147483648. 
	jb		_invalid
	jmp		_finish

; Check if positive value fits in SDWORD register.
_checkPositiveSizeLimit:

	cmp		eax, SDWORD_SIZE_MAX							; (2^31) - 1 = 2147483647.
	ja		_invalid
	jmp		_finish

_tooManyDigits:
	pop		edx												; Need to balance stack.

_invalid:
	; Invalid value message for all the scenario's above.

	mDisplayString [ebp+12]									; "ERROR: You did not enter..."
	call    CrLf

	push	edi
	mov		edi, [ebp+28]									; isNegative boolean.
	mov		DWORD PTR [edi], 0								; Reset isNegative to 0 before evaluating next user value.
	pop		edi

	jmp		_getValue
	
_finish:

	push	edi
	mov		edi, [ebp+28]									; isNegative boolean.
	mov		DWORD PTR [edi], 0								; Reset isNegative to 0 before evaluating next user value.
	pop		edi

	pop		edx
	pop		ecx
	pop		ebx
	pop		eax
	pop		edi
	pop		esi

	pop		ebp
	ret		24												; Back to main and add 28 BYTEs to the stack.

readVal ENDP

; --------------------------------------------------------------------------------------------------
; Section: Display the integers as a string.
;
; Purpose / Steps:
;	(1) If the integer is negative, store a minus symbol in the displayStrArray array,
;	(2) Convert each integer to an ASCII character and push it to the stack.
;	(3) Pop each ASCII character from the stack and store in the displayStrArray array.
;	(4) Call the mDisplayString macro to display the value in displayStrArray.
;
; Preconditions: 
;	displayStrArray is initialized as an array with 12 element of size BYTE (11 values plus null character).
;	If displaying all the user values, the current memory location of userValueIntArray is updated 
;	 in the parent loop in displayAllUserValues.
;
; Postconditions:
;	mDisplayString displays the desired string. 
;	ebp, edi, eax, ebx, ecx, and edx are changed and restored.
;
; Receives:
;	If displaying all the user values: 
;		[ebp+8]				= Value in current memory location of userValueIntArray, (input, value)
;	If displaying the sum: 
;		[ebp+8]				= sum (input, value)
;	If displaying the average: 
;		[ebp+8]				= average (input, value)
;
; Returns:
;	[ebp+12]				= displayStrArray (output, reference), array for temporarily storing the string
;
; Implementation notes:
;	Pushing each integer to the stack was needed because the integer needed to be converted to 
;	 ASCII values by repeatedly dividing by 10 and adding 48. Doing this yields the last value in 
;	 the integer so 123 would print at '321'. By pushing the values to the stack, the program can 
;	 reverse the order to print in the desired order (yielding '123' in this example).
;
;	Ecx is used as a counter for the # of integers pushed to the stack. Values are popped off
;		the stack until ecx reverts back to 0. 
;
; --------------------------------------------------------------------------------------------------

writeVal PROC

	push	ebp
	mov		ebp, esp

	push	edi
	push	eax
	push	ebx
	push	ecx
	push	edx	
	
	mov		edi, [ebp+12]									; Memory address of displayStrArray																						
	mov		eax, [ebp+8]									; Value in userValueIntArray[n], sum, or average

	mov		ecx, 0											; Initialize ecx to 0 since we'll be incrementing ecx when 
															;	converting the integer to a string. 
	cmp		eax, 0											; Implied subtraction (eax - 1)
	jl		_displayMinusSymbol
	jmp		_convertToString

_displayMinusSymbol:
	push	eax
	mov		al, 45											; 45 = '-' symbol in ASCII.
	cld														; Clear the direction so edi increments. 	
	stosb													
	pop		eax
	neg		eax												; Make value positive for next step. 

; Part 1 of transforming the integer to an ASCII value takes this approach:
;	while eax <> 0:
;		eax mod 10 --> push edx + 48 to stack.
;	where eax is initialized to numberInteger.
; For example, the string 123 is converted to '123' by these steps:
;	iteration 1: numberASCII = 123 mod 10 --> edx + 48 = 3 + 48 = 51 --> push 51 to stack
;	iteration 2: numberASCII = 12 mod 10 --> edx + 48 = 2 + 48 = 50 --> push 50 to stack 
;	iteration 3: numberASCII = 1 mod 10 --> edx + 48 = 1 + 48 = 49 --> push 49 to stack

_convertToString:
	inc		ecx												; Counter used when popping the values off the stack.

	mov		ebx, 10
	mov		edx, 0
	div		ebx
	add		edx, 48
	push	edx												; Each value is in dl, so it's okay to push edx then
															;	pop off the 4 BYTE value later becuase it will still
															;	be in dl.
	cmp		eax, 0
	jne		_convertToString

	_popOffStack:
		; Part 2 pops off the values in reverse order (continued example below):
		;	iteration 1: pop 49 and load in edi[n]
		;	iteration 2: pop 50 and load in edi[n+1]
		;	iteration 3: pop 51 and load in edi[n+2]

			pop		eax										; It's okay to pop off eax because each value is in al.
			cld												; Clear the direction so edi increments.
			stosb													
		loop	_popOffStack

	mov		al, 0											; Add null character at the end of the string.
	stosb

	mDisplayString [ebp+12]									; Display value in displayStrArray.

	pop		edx
	pop		ecx
	pop		ebx
	pop		eax
	pop		edi

	pop		ebp
	ret		8												; Back to main or displayAllUserValues and add 12 BYTEs to the stack.

writeVal ENDP

; --------------------------------------------------------------------------------------------------
; Section/Purpose: Calculate and store the sum.
;
; Preconditions:
;	userValueIntArray is updated with all the user values stored as SDWORD.
;
; Postconditions:
;	sum is updated and stored as SDWORD.
;	ebp, esi, edi, eax, and ecx are changed and restored.
;
; Receives:
;	[ebp+8]				= userValueIntArray (input, reference)
;	[ebp+16]			= lengthUserValueIntArray (input, value)
;
; Returns:
;	[ebp+12]			= sum (output, reference)
;
; --------------------------------------------------------------------------------------------------

calcSum PROC

	push	ebp
	mov		ebp, esp

	push	esi
	push	edi
	push	eax
	push	ecx

	mov		esi, [ebp+8]									; Memory location for userValueIntArray.
	mov		edi, [ebp+12]									; Memory location for sum.
	mov		ecx, [ebp+16]									; Counter for loop.

	mov		eax, 0											; Initialize eax to 0. 

	_sumLoop:
		; Loop through value in userValueIntArray and add each integer to eax.

			add		eax, [esi]
			add		esi, 4

		loop	_sumLoop

	mov		[edi], eax										; Sum all values. 

	pop		ecx
	pop		eax
	pop		edi
	pop		esi

	pop		ebp
	ret		12												; Back to main and add 16 BYTEs to the stack.

calcSum ENDP


; --------------------------------------------------------------------------------------------------
; Section: Calculate and store the average.
;
; Purpose: Calculate and store the truncated average. This is equivalent to storing the integer
;	portion only when calculating the average by division.
;
; Preconditions: 
;	sum is updated and stored as SDWORD.
;
; Postconditions:
;	average is updated and stored as SDWORD.
;	ebp, edi, eax, and ecx are changed and restored.
;
; Receives:
;	[ebp+8]				= sum (input, value)
;	[ebp+16]			= lengthUserValueIntArray (input, value)
;
; Returns:
;	[ebp+12]			= average (output, reference)
;
; --------------------------------------------------------------------------------------------------

calcAverage PROC

	push	ebp
	mov		ebp, esp

	push	edi
	push	eax
	push	ecx

	mov		edi, [ebp+12]									; Memory location for average.
	mov		ecx, [ebp+16]									; Denominator for average.

	mov		eax, [ebp+8]									; sum
	cdq
	idiv	ecx												; eax mod ecx
	mov		[edi], eax										; Integer portion only.

	pop		ecx
	pop		eax
	pop		edi

	pop		ebp
	ret		12												; Back to main and add 16 BYTEs to the stack.

calcAverage ENDP

; --------------------------------------------------------------------------------------------------
; Section: Display all the user values.
;
; Purpose: Loop through userValueIntArray, calling writeVal to display each integer as a string.
;
; Preconditions: 
;	userValueIntArray is updated with all the user values stored as SDWORD.
;
; Postconditions:
;	Program calls writeVal and increments esi by 4 each time. 
;	writeVal displays the values as strings. 
;	ebp, esi, edi, eax, and ecx are changed and restored.
;
; Receives:
;	[ebp+8]				= userValueIntArray (input, reference)
;	[ebp+12]			= displayStrArray (input, reference)
;	[ebp+16]			= lengthUserValueIntArray (input, value)
;
; Returns: none
;
; --------------------------------------------------------------------------------------------------

displayAllUserValues PROC

	push	ebp
	mov		ebp, esp

	push	esi
	push	edi
	push	eax
	push	ecx

	mov		esi, [ebp+8]									; Memory location for userValueIntArray.
	mov		edi, [ebp+12]									; Memory location for displayStrArray.
	mov		ecx, [ebp+16]									; Counter for loop.
					
	_displayLoop:
		; Loop through userValueIntArray and push each value to writeVal.
		
			push	ecx

			push	edi										; Memory location for displayStrArray.
			push	[esi]									; Value in userValueIntArray.
			call	writeVal

			pop		ecx										; Restore ecx for loop count. 

			cmp		ecx, 1									; Implied subtraction (ecx - 1)
			je		_noDisplayComma							; Last value so don't add comma and space.
			mov     al, 44									; 44 = comma in ASCII
			call    WriteChar								
			MOV     al, 32									; 44 = space in ASCII
			call    WriteChar								

		_noDisplayComma:
			add		esi, 4

		loop	_displayLoop

	call    CrLf

	pop		ecx
	pop		eax
	pop		edi
	pop		esi

	pop		ebp
	ret		12												; Back to main and add 16 BYTEs to the stack.

displayAllUserValues ENDP

END main