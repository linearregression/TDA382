import java.util.HashMap;
import java.util.Map;
import java.util.Random;
import java.util.concurrent.Semaphore;

import TSim.CommandException;
import TSim.SensorEvent;
import TSim.TSimInterface;

public class Lab1 {

	public static int simulation_speed_parameter;
	
	private enum Direction {DOWN, UP};
	private static final TSimInterface tsim = TSimInterface.getInstance();
	private static final Semaphore [] semaphores = {new Semaphore(1), new Semaphore(0), new Semaphore(1),
													new Semaphore(1), new Semaphore(1), new Semaphore(1),
													new Semaphore(1), new Semaphore(0), new Semaphore(1)};

	private static final Map<String, Integer> sensors;
	private static final Map<Integer, Integer> sensorSemaphoreMap;
    static {
        sensors = new HashMap<String, Integer>();
        sensorSemaphoreMap = new HashMap<Integer, Integer>();
        sensors.put("3 12", 1); sensorSemaphoreMap.put(1, 0);
        sensors.put("4 11", 2); sensorSemaphoreMap.put(2, 1);
        sensors.put("2 11", 3); 
        sensors.put("3 9", 4); 
        sensors.put("4 10", 5); sensorSemaphoreMap.put(5, 3);
        sensors.put("5 9", 6); sensorSemaphoreMap.put(6, 4);
        sensors.put("14 9", 7); sensorSemaphoreMap.put(7, 4);
        sensors.put("15 10", 8); sensorSemaphoreMap.put(8, 3);
        sensors.put("16 9", 9); 
        sensors.put("18 7", 10); 
        sensors.put("17 8", 11); sensorSemaphoreMap.put(11, 6);
        sensors.put("16 7", 12); sensorSemaphoreMap.put(12, 7);
        sensors.put("8 8", 13); 
        sensors.put("9 7", 14); 
        sensors.put("7 7", 15);
        sensors.put("8 6", 16);
        sensors.put("14 13", 17); //station sensors
        sensors.put("14 11", 18);
        sensors.put("14 5", 19);
        sensors.put("14 3", 20);
    }
	Switch sw1 = new Switch(3, 11);
	Switch sw2 = new Switch(4, 9);
	Switch sw3 = new Switch(15, 9);
	Switch sw4 = new Switch(17, 7);
	

	public static void main(String[] args) {
		new Lab1(args);
	}

	public Lab1(String[] args) {
		simulation_speed_parameter = Integer.parseInt(args[2]);
		Train train1 = new Train(1, Integer.parseInt(args[0]), Direction.DOWN);
		Train train2 = new Train(2, Integer.parseInt(args[1]), Direction.UP);
//		int speed = Integer.parseInt(args[2]);

		try {
			System.err.println("Something something");
			tsim.setSpeed(train1.TRAIN_ID, train1.maxspeed);
			tsim.setSpeed(train2.TRAIN_ID, train2.maxspeed);
			
			Thread t1 = new Thread(train1);
			Thread t2 = new Thread(train2);
			t1.start();t2.start();
		} catch (CommandException e) {
			e.printStackTrace(); // or only e.getMessage() for the error
			System.exit(1);
		}
	}

	public static String createSensorID(int x, int y) {
		return x + " " + y;
	}

	class Train implements Runnable {

		private final int TRAIN_ID;
		private int maxspeed;
		private int lastSensor;
		private Direction direction;
		

		public Train(int trainID, int maxspeed, Direction direction) {
			this.TRAIN_ID = trainID;
			this.maxspeed = maxspeed;
			this.direction = direction;
			
			if (TRAIN_ID == 1) {
				lastSensor = 17;
			} else {
				lastSensor = 0;
			}
		}
		
		@Override
		public void run() {
			TSimInterface tsim = TSimInterface.getInstance();
			while (true) {
				try {
					SensorEvent event = tsim.getSensor(TRAIN_ID);					
//					System.err.println("Received an event! " + event.toString());
					String sensorText = createSensorID(event.getXpos(), event.getYpos());
					int sensor = sensors.get(sensorText);
					passedSensor(sensor, event, direction);
					if (event.getStatus() == SensorEvent.INACTIVE) {
						lastSensor = sensor;
//						System.err.println(TRAIN_ID + " - last sensor is " + lastSensor);
					}
				} catch (InterruptedException ie) {
					System.err.println(ie.getMessage());
				} catch (CommandException ce) {
					System.err.println(ce.getMessage());
				}
			}
		}
		
		public void tryAcquire(Semaphore trackToAcquire) {
			System.err.println("tryAcquire()");
			if (!trackToAcquire.tryAcquire()) { // if the track is occupied
				try {
					tsim.setSpeed(TRAIN_ID, 0); // stop the train
					trackToAcquire.acquire(); // wait for the track to be free
					tsim.setSpeed(TRAIN_ID, maxspeed); // and then accelerate to maxspeed
				} catch (CommandException e) {
					System.err.println(e.getMessage());
				} catch (InterruptedException ie) {
					System.err.println(ie.getMessage());
				}
			} // else just keep going
		}
		
		public void chooseTrack(Semaphore shortTrack, Semaphore longTrack, Switch trainSwitch) {
			int direction;
			boolean gotShortTrack = shortTrack.tryAcquire();
			boolean leftIsShortestTrack = trainSwitch.equals(sw1) || trainSwitch.equals(sw2);
			
			if (trainSwitch.x == 17 && trainSwitch.y == 7) {
				System.err.println("gotShortTrack: " + gotShortTrack);
				System.err.println("leftIsShortest: " + leftIsShortestTrack);
			}
			
//			if (trainSwitch.x == 4 && trainSwitch.y == 9) {
//				System.err.println("gotShortTrack: " + gotShortTrack);
//				System.err.println("leftIsShortest: " + leftIsShortestTrack);
//			}
			
			if (gotShortTrack && leftIsShortestTrack || !gotShortTrack && !leftIsShortestTrack) {
				direction = tsim.SWITCH_LEFT;
			} else {
				direction = tsim.SWITCH_RIGHT;
			}
			trainSwitch.changeDirection(direction);
			System.err.println(direction == tsim.SWITCH_LEFT ? "LEFT\n" : "RIGHT\n");
			
			if (!gotShortTrack) { // if the track is occupied
				try {
					longTrack.acquire();
				} catch (InterruptedException e) {
					System.err.println(e.getMessage());
				}
			} 
		}
		
		public void releaseFormerTrack() {
			int trackIndex = sensorSemaphoreMap.get(lastSensor);
			semaphores[trackIndex].release();
			if (trackIndex == 7) {
				System.err.println("\nJust realeased track 8!!!!\n" + "\n");
			}
		}
		public int permits = 1;
		public void passedSensor(int sensor, SensorEvent event, Direction direction) {
			if (TRAIN_ID == 1) {
				if (direction != Direction.DOWN && maxspeed > 0) {
					System.err.println("Train 1 is stupid");
					System.err.println(direction + "\n");
				}
			} else {
				if (direction != Direction.UP && maxspeed > 0) {
					System.err.println("Train 2 is stupid");
					System.err.println(direction + "\n");
				}
			}
			
			
			if (semaphores[7].availablePermits() != permits) {
				System.err.print("\n\n WTF, HOW DID this HAPPEN???\n???\n\n");
				System.err.println("permits: " + semaphores[7].availablePermits());
			}
			permits = semaphores[7].availablePermits();
			
			switch (sensor) {
			case 1:
			case 2:
				if (direction == Direction.DOWN) { // going downwards
					if (event.getStatus() == SensorEvent.INACTIVE) { // we have left the crossing point/previous track completely
						semaphores[2].release(); // release previous track
					}
				} else { // going upwards
					if (event.getStatus() == SensorEvent.ACTIVE) { // we just came to the sensor
						tryAcquire(semaphores[2]);
						sw1.changeDirection(sensor == 1 ? tsim.SWITCH_RIGHT : tsim.SWITCH_LEFT);
					} 
				}
				break;
			case 3:
				if (direction == Direction.DOWN) { // going downwards
					if (event.getStatus() == SensorEvent.ACTIVE) { // we have left the crossing point/previous track completely
						chooseTrack(semaphores[1], semaphores[0], sw1); // release previous track
					}
				} else { // going upwards
					if (event.getStatus() == SensorEvent.INACTIVE) { // we just came to the sensor
						releaseFormerTrack();
					} 
				}
				break;
			case 4:
				if (direction == Direction.DOWN) { // going downwards
					if (event.getStatus() == SensorEvent.INACTIVE) { // we have left the crossing point/previous track completely
						releaseFormerTrack(); // release previous track
					}
				} else { // going upwards
					if (event.getStatus() == SensorEvent.ACTIVE) { // we just came to the sensor
						chooseTrack(semaphores[4], semaphores[3], sw2);
					} 
				}
				break;
			case 5:
			case 6:
				if (direction == Direction.DOWN) { // going downwards
					if (event.getStatus() == SensorEvent.ACTIVE) { // we have left the crossing point/previous track completely
						tryAcquire(semaphores[2]); // release previous track
						sw2.changeDirection(sensor == 5 ? tsim.SWITCH_RIGHT : tsim.SWITCH_LEFT);
					}
				} else { // going upwards
					if (event.getStatus() == SensorEvent.INACTIVE) { // we just came to the sensor
						semaphores[2].release();
					} 
				}
				break;
			case 7:
			case 8:
				if (direction == Direction.DOWN) { // going downwards
					if (event.getStatus() == SensorEvent.INACTIVE) { // we have left the crossing point/previous track completely
						semaphores[5].release(); // release previous track
					}
				} else { // going upwards
					if (event.getStatus() == SensorEvent.ACTIVE) { // we just came to the sensor
						tryAcquire(semaphores[5]);
						sw3.changeDirection(sensor == 7 ? tsim.SWITCH_RIGHT : tsim.SWITCH_LEFT);
					} 
				}
				break;
			case 9:
				if (direction == Direction.DOWN) { // going downwards
					if (event.getStatus() == SensorEvent.ACTIVE) { // we have left the crossing point/previous track completely
						chooseTrack(semaphores[4], semaphores[3], sw3); // release previous track
					}
				} else { // going upwards
					if (event.getStatus() == SensorEvent.INACTIVE) { // we just came to the sensor
						releaseFormerTrack();
					} 
				}
				break;
			case 10:
				if (direction == Direction.DOWN) { // going downwards
					if (event.getStatus() == SensorEvent.INACTIVE) { // we have left the crossing point/previous track completely
						releaseFormerTrack(); // release previous track
						System.err.println("Sensor 10 - direction down, former track released." + "\n");
					}
				} else { // going upwards
					if (event.getStatus() == SensorEvent.ACTIVE) { // we just came to the sensor
						chooseTrack(semaphores[7], semaphores[6], sw4);
					} 
				}
				break;
			case 11:
			case 12:
				if (direction == Direction.DOWN) { // going downwards
					if (event.getStatus() == SensorEvent.ACTIVE) { // we have left the crossing point/previous track completely
						tryAcquire(semaphores[5]); // release previous track
						sw4.changeDirection(sensor == 12 ? tsim.SWITCH_RIGHT : tsim.SWITCH_LEFT);
					}
				} else { // going upwards
					if (event.getStatus() == SensorEvent.INACTIVE) { // we just came to the sensor
						semaphores[5].release();
					} 
				}
				break;
			case 13:
			case 14:
				if (direction == Direction.DOWN) { // going downwards
					if (event.getStatus() == SensorEvent.INACTIVE) { // we have left the crossing point/previous track completely
						semaphores[8].release(); // release previous track
					}
				} else { // going upwards
					if (event.getStatus() == SensorEvent.ACTIVE) { // we just came to the sensor
						tryAcquire(semaphores[8]);
					} 
				}
				break;
			case 15:
			case 16:
				if (direction == Direction.DOWN) { // going downwards
					if (event.getStatus() == SensorEvent.ACTIVE) { // we have left the crossing point/previous track completely
						tryAcquire(semaphores[8]); // release previous track
					}
				} else { // going upwards
					if (event.getStatus() == SensorEvent.INACTIVE) { // we just came to the sensor
						semaphores[8].release();
					} 
				}
				break;
			case 17:
			case 18:
				if (direction == Direction.DOWN) { // going downwards
					if (event.getStatus() == SensorEvent.ACTIVE) { // we have left the crossing point/previous track completely
						turnAtStation();
					}
				} 
				break;
			case 19:
			case 20:
				if (direction == Direction.UP) { // going downwards
					if (event.getStatus() == SensorEvent.ACTIVE) { // we have left the crossing point/previous track completely
						turnAtStation();
					}
				} 
				break;
			}
			
		}
		
		public void turnAtStation() {
			try {
				tsim.setSpeed(TRAIN_ID, 0);
				int time_at_station = (int) (Math.random() * 1000 + 1000);
				int stopTimeMS = time_at_station + 2 * simulation_speed_parameter * Math.abs(maxspeed);
				System.err.println("stopTime: " + stopTimeMS);
				Thread.sleep(stopTimeMS);
				maxspeed = -maxspeed;
				direction = direction == Direction.DOWN ? Direction.UP : Direction.DOWN;
				tsim.setSpeed(TRAIN_ID, maxspeed);
			} catch (CommandException e) {
				System.err.println(e.getMessage());
			} catch (InterruptedException ie) {
				System.err.println(ie.getMessage());
			} 
			
		}
	}

	class Switch {
		private int x, y;

		public Switch(int x, int y) {
			this.x = x;
			this.y = y;
		}
		
		public void changeDirection(int switchDir) {
			try {
				tsim.setSwitch(x, y, switchDir);
			} catch (CommandException e) {
				System.err.println(e.getMessage());
			}
		}

	}

	class Sensor {
		private final String id;
		private int x, y;

		public Sensor(int x, int y) {
			this.x = x;
			this.y = y;
			this.id = createSensorID(x, y);
		}

		public String getID() {
			return this.id;
		}

		public boolean equals(SensorEvent event) {
			return this.x == event.getXpos() && this.y == event.getYpos();
		}
	}
}
