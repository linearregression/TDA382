
//Test code
case 1: 
	int direction = getDirection();
	if (direction == Direction.DOWN) { //going downwards
		if (event.getStatus() == SensorEvent.INACTIVE) { //we have left the crossing point/previous track completely
			crit3.release(); 							//release previous track
		}
	} else { //going upwards
		if (event.getStatus() == SensorEvent.ACTIVE) { //we just came to the sensor
			if (crit3.availablePermits() == 0) { // if the track is occupied
				tsim.setSpeed(TRAIN_ID, 0);		// stop the train
				crit3.acquire();				// wait for the track to be free 
				tsim.setSpeed(TRAIN_ID, maxspeed); // and then accelerate to maxspeed
			} //else just keep going
			crit3.acquire(); //wait for the track to be free
			tsim.setSpeed(TRAIN_ID, maxspeed);
			
		} else { //sensor is inactive, we are leaving it
			//nothing happens
		}
		
	
	}
	
	//to avoid crash when both execute simoultaneously:
	//setSpeed to 0
	//acquire lock
	//setSpeed to max
	
	//results in potential speed bump for the trains
	
	
	
	/* See attached photo-file for how I labeled the track */
/***********************************************************************************/

//1)
	// DOWN
		// ACTIVE 	=> nothing
		// INACTIVE => crit3.release()
	
	// UP
		// ACTIVE
			// crit3 IS_AVAILABLE	=> crit3.acquire()
			// crit3 NOT_AVAILABLE 	=> stopTrain && crit3.acquire()
		// INACTIVE	=> nothing
//

//2)
	// DOWN
		// ACTIVE 	=> nothing
		// INACTIVE => crit3.release()
	
	// UP
		// ACTIVE
			// crit3 IS_AVAILABLE	=> crit3.acquire()
			// crit3 NOT_AVAILABLE 	=> stopTrain && crit3.acquire()
		// INACTIVE	=> nothing
//

//3)
	// DOWN
		// ACTIVE
			// t2 IS_AVAILABLE	=> t2.acquire()
			// t2 NOT_AVAILABLE => t1.acquire()
		// INACTIVE	=> nothing
	
	// UP
		// ACTIVE	=> nothing
		// INACTIVE => formerTrack.release()
//	

/**********************************************************/
	
//4)
	// DOWN
		// ACTIVE 	=> nothing
		// INACTIVE => formerTrack.release()
	
	// UP
		// ACTIVE
			// t5 IS_AVAILABLE	=> t5.acquire()
			// t5 NOT_AVAILABLE => t4.acquire()
		// INACTIVE	=> nothing
//	

//5)
	// DOWN
		// ACTIVE
			// crit3 IS_AVAILABLE	=> crit3.acquire()
			// crit3 NOT_AVAILABLE 	=> stopTrain && crit3.acquire()
		// INACTIVE => nothing
	
	// UP
		// ACTIVE	=> nothing
		// INACTIVE	=> crit3.release()
//

//6)
	// DOWN
		// ACTIVE
			// crit3 IS_AVAILABLE	=> crit3.acquire()
			// crit3 NOT_AVAILABLE 	=> stopTrain && crit3.acquire()
		// INACTIVE => nothing
	
	// UP
		// ACTIVE	=> nothing
		// INACTIVE	=> crit3.release()
//

/***********************************************************************/

//7)
	// DOWN
		// ACTIVE 	=> nothing
		// INACTIVE => crit6.release()
	
	// UP
		// ACTIVE
			// crit6 IS_AVAILABLE	=> crit6.acquire()
			// crit6 NOT_AVAILABLE 	=> stopTrain && crit6.acquire()
		// INACTIVE	=> nothing
//

//8)
	// DOWN
		// ACTIVE 	=> nothing
		// INACTIVE => crit6.release()
	
	// UP
		// ACTIVE
			// crit6 IS_AVAILABLE	=> crit6.acquire()
			// crit6 NOT_AVAILABLE 	=> stopTrain && crit6.acquire()
		// INACTIVE	=> nothing
//

//9)
	// DOWN
		// ACTIVE
			// t5 IS_AVAILABLE	=> t5.acquire()
			// t5 NOT_AVAILABLE => t4.acquire()
		// INACTIVE	=> nothing
	
	// UP
		// ACTIVE	=> nothing
		// INACTIVE => formerTrack.release()
//	
	
	
/********************************************************************/

//Pseudo code for the sensors on the top side of the track

/********************************************************************/
	
/*

10) 
	DOWN 
		ACTIVE => nothing 
		INACTIVE => formerTrack.release()			
	UP 
		ACTIVE 
			t8 IS_AVAILABLE  => t8.acquire()
			t8 NOT_AVAILABLE => t7.acquire()
		INACTIVE => nothing	

		
11)
	DOWN 
		ACTIVE 
			crit6 IS_AVAILABLE => crit6.acquire()
			crit6 NOT_AVAILABLE => stopTrain && crit6.acquire()
		INACTIVE => nothing
	UP
		ACTIVE => nothing
		INACTIVE => crit6.release()
		
12)
	DOWN 
		ACTIVE 
			crit6 IS_AVAILABLE => crit6.acquire()
			crit6 NOT_AVAILABLE => stopTrain && crit6.acquire()
		INACTIVE => nothing
	UP
		ACTIVE => nothing
		INACTIVE => crit6.release()
	
	
13)
	DOWN 
		ACTIVE => nothing
		INACTIVE => crit9.release()
	UP
		ACTIVE 
			crit9 IS_AVAILABLE => crit9.acquire()
			crit9 NOT_AVAILABLE => stopTrain && crit9.acquire()
		INACTIVE => nothing

14)		
	DOWN 
		ACTIVE => nothing
		INACTIVE => crit9.release()
	UP
		ACTIVE 
			crit9 IS_AVAILABLE => crit9.acquire()
			crit9 NOT_AVAILABLE => stopTrain && crit9.acquire()
		INACTIVE => nothing
		
15) 
	DOWN 
		ACTIVE 
			crit9 IS_AVAILABLE => crit9.acquire()
			crit9 NOT_AVAILABLE => stopTrain && crit9.acquire()
		INACTIVE => nothing
	UP	
		ACTIVE => nothing
		INACTIVE => crit9.release()
		
16) 
	DOWN 
		ACTIVE 
			crit9 IS_AVAILABLE => crit9.acquire()
			crit9 NOT_AVAILABLE => stopTrain && crit9.acquire()
		INACTIVE => nothing
	UP	
		ACTIVE => nothing
		INACTIVE => crit9.release()		
		
*/	
	
	
	
	
	
	
	