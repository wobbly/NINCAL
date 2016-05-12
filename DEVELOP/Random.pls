. generates random number between 0 and 10 inclusive.
. stores number in RandomResult
///////////////////////////////////////////////////////////////////////////////
//
//   Program:   random.pls
//
//    Author:   bjackson@adjacecy.net
//
// Copyright:   © 2004 Adjacency Consulting Group, Inc.  All rights reserved.
//
//      Date:   21 DEC 2004
//
//   Purpose:   To demonstrate the use of FUNCTIONS in PLBWin9.0C
//
//  Revision:   ver01    21 DEC 2004     BJACKSON
//
///////////////////////////////////////////////////////////////////////////////
cwk1 dim 1
timestamp dim 16

randomResult form 3
cputime integer 4
iTimestamp integer 4
seconds integer 4
x integer 4
y integer 4
z integer 4
/////////////////////////////////////////////////////////////////////////////////
Password	dim	10
DimPtr		dim	^
Counter		form	2
Result		form	1
temp		dim	2
Result2		form 	2
alphabet	init	"ABCDEFGHIJKLMNOPQRSTUVWXYZ"
.

///////////////////////////////////////////////////////////////////////////////
rand function

    ///////////////////////////////////////////////////////////////////////////
    //
    //  FUNCTION: rand
    //
    //    INPUTS: multiplier (form 10.21), default value of 1
    //
    //   OUTPUTS: random value between 0 and multiplier
    //
    //  REQUIRES: must have called rand_seed to generate truly random numbers
    //
    //     NOTES: This function uses a combination of a Multiply with carry
    //             generator and a simple multiplicative generator. Simple but
    //             fast and good.
    //
    ///////////////////////////////////////////////////////////////////////////

// Input variable supplied by the USING clause on the originating call
multiplier form "         1.000000000000000000000" // input parameter
	entry
// Work variables, local the the rand function
v integer 4                 // temporary work variable while we carry our seed values
z2 integer 4                // work variable used to store the value of Z && 65535
z3 integer 4                // work variable used to store the value of Z << 16
result form 10.21           // this is our return value

	// "Carry" all of our seed values (x,y,z) using a pre-defined formula
	calc	v = (x*y)
	move	y to x
	move	v to y
	move	z to z2
	move	z to z3
	and	65535 with z2
	shiftleft z3 by 16
	calc	z = (z2 * 30903 + z3)
	calc	result = (y + z2)

	// We don't want an integer, we want a decimal value, find divide it by the max value for a
	//  4-byte integer
	divide	"4294967295" into result

	// Now, apply the multiplier we set at the entrypoint.  This is returned to
	//  the variable specified by the calling routine in the GIVING clause
	return using (result * multiplier)
	functionend
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
rand_seed function

    ///////////////////////////////////////////////////////////////////////////
    //
    //  FUNCTION: rand_seed
    //
    //    INPUTS: seed1 (integer 4) - a value with which to populate the random number generator
    //            seed2 (integer 4) - a value with which to populate the random number generator
    //            seed3 (integer 4) - a value with which to populate the random number generator
    //
    //   OUTPUTS: none, but modifies the public variables x, y, and z
    //
    //  REQUIRES: none
    //
    //     NOTES: Note that all three seeds will default to pre-set values if
    //             they are not specifically assigned.  This will cause the
    //             results to be repeatable.  Use a clock or series of clock
    //             statements to generate a more random starting value for each
    //             variable
    //
    ///////////////////////////////////////////////////////////////////////////

// Input variable supplied by the USING clause on the originating call
seed1 integer 4,"1893513180"
seed2 integer 4,"1791398085"
seed3 integer 4,"1557985959"
	entry
// Note there are no work variables in this function
	move	seed1 to x
	shiftleft x by 1
	or	1 with x
	calc	x = (x * 3 * x)

	move	seed2 to y
	shiftleft y by 1
	or	1 with y

	move	seed3 to z

	return
	functionend
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
RandomPassword Routine DimPtr
	// find out how many seconds have elapsed since 1970
	clock	seconds,seconds

	// find out how many CPU cycles have been used
	clock	cputime,cputime

	// get the rightmost 10 bytes of our timestamp and make it more unique
	clock	timestamp,timestamp
	bump	timestamp,6
	move	timestamp,iTimestamp
	shiftleft iTimestamp by 8

	// "Seed" our random number generator
	call	rand_seed using cputime,seconds,iTimestamp

	// the first result is not very random because of the seeding, so
	//  we're going to "throwaway" one random number
	call	rand giving randomResult using "100"

	call	GeneratePWD
	move	Password,DimPtr
	return
///////////////////////////////////////////////////////////////////
GeneratePWD
	move "", Password	// so we can append to Password
	for Counter, "1", "10", "1"	// Password is 10 characters

        	// when we call rand, we need to pass in a value telling it how big
        	//  we want our random number to be (in this case 1), and where
        	//  rand should store the return value (randomResult)
        	call rand giving randomResult using "1" // 0 numeric, 1 alphabetic
        	if (randomResult=0)	// generate numeric char
	        	call rand giving randomResult using "9"
			move randomResult,result
	        	append Result, Password
		else	// generate alpa char
			call rand giving randomResult using "25" // generate 0 through 25
	        	calc randomResult=randomResult+1
	        	move randomResult,Result2
.establish form pointer via random number generator - Result2 = form pointer value
			reset	alphabet,Result2
			move	alphabet,temp
			append	temp,Password
			reset	alphabet
        	endif
        repeat
        reset	Password
	return
/////////////////////////////////////////////////////////////////