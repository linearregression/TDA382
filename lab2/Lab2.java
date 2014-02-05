//Jakob Svensson
//Florian Minges

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.Semaphore;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

import TSim.CommandException;
import TSim.SensorEvent;
import TSim.TSimInterface;

public class Lab2 {

	private static int simulation_speed_parameter;
	private static final int TRAIN_MAX_SPEED = 17;
	private static final int DEFAULT_SIM_SPEED_PARAMETER = 100;
	private enum Direction {SOUTH, NORTH};
	private static final TSimInterface tsim = TSimInterface.getInstance();
		
	private final Monitor [] monitors = {new Monitor(false),new Monitor(true),new Monitor(false),
											new Monitor(false),new Monitor(false),new Monitor(false),
											new Monitor(false),new Monitor(true),new Monitor(false)};
 	
	private static final Map<String, Integer> sensors;
	private static final Map<Integer, Integer> sensorTrackMap;
    static {
        sensors = new HashMap<String, Integer>();
        sensorTrackMap = new HashMap<Integer, Integer>();
        sensors.put("3 13", 1); sensorTrackMap.put(1, 0);
        sensors.put("5 11", 2); sensorTrackMap.put(2, 1);
        sensors.put("1 11", 3); 
        sensors.put("2 9", 4); 
        sensors.put("5 10", 5); sensorTrackMap.put(5, 3);
        sensors.put("6 9", 6); sensorTrackMap.put(6, 4);
        sensors.put("13 9", 7); sensorTrackMap.put(7, 4);
        sensors.put("14 10", 8); sensorTrackMap.put(8, 3);
        sensors.put("17 9", 9); 
        sensors.put("19 7", 10); 
        sensors.put("16 8", 11); sensorTrackMap.put(11, 6);
        sensors.put("15 7", 12); sensorTrackMap.put(12, 7);
        sensors.put("9 8", 13); 
        sensors.put("10 7", 14); 
        sensors.put("6 7", 15);
        sensors.put("8 5", 16);
        sensors.put("14 13", 17); //station sensors
        sensors.put("14 11", 18);
        sensors.put("14 5", 19);
        sensors.put("14 3", 20);
    }
    
	private Switch sw1 = new Switch(3, 11);
	private Switch sw2 = new Switch(4, 9);
	private Switch sw3 = new Switch(15, 9);
	private Switch sw4 = new Switch(17, 7);

	public static void main(String[] args) {
		new Lab2(args);
	}

	public Lab2(String[] args) {
		int train1Speed = args.length>0 ? Integer.parseInt(args[0]) : TRAIN_MAX_SPEED; 
		int train2Speed = args.length>1 ? Integer.parseInt(args[1]) : TRAIN_MAX_SPEED; 
		simulation_speed_parameter = args.length>2 ? Integer.parseInt(args[2]) : DEFAULT_SIM_SPEED_PARAMETER;
	
		Train train1 = new Train(1, train1Speed, Direction.SOUTH);
		Train train2 = new Train(2, train2Speed, Direction.NORTH);		

		Thread t1 = new Thread(train1);
		Thread t2 = new Thread(train2);
		t1.start();t2.start();
	}

	/** Returns the key we use in the sensorTrackMap. */
	public static String createSensorID(int x, int y) {
		return x + " " + y;
	}
	
	class Monitor {
		
		private final Lock lock = new ReentrantLock();
		private Condition notTaken = lock.newCondition();
		
		private boolean taken;
		
		public Monitor(boolean taken) {
			this.taken = taken;
		}
		
		public void enter(Train train) throws InterruptedException, CommandException {
			lock.lock();
			
			tsim.setSpeed(train.TRAIN_ID, 0); // stop the train
			while (taken) { notTaken.await(); } // wait for the track to be free
			
			tsim.setSpeed(train.TRAIN_ID, train.maxspeed); // and then accelerate to maxspeed
			taken = true;
			
			lock.unlock();
		}
	
		public void leave() {
			lock.lock();
			
			taken = false;
			notTaken.signal();
			
			lock.unlock();
		}
		
		public boolean tryEnter(){
			lock.lock();
			
			if(!taken){
				taken = true;
				lock.unlock();
				return true;				
			}
			lock.unlock();
			return false;
		}
		
	}

	class Train implements Runnable {

		private final int TRAIN_ID;
		private int maxspeed;
		private int lastSensor = -1;
		private Direction direction;		

		public Train(int trainID, int maxspeed, Direction direction) {
			this.TRAIN_ID = trainID;
			this.maxspeed = maxspeed;
			this.direction = direction;
		}
		
		
		
		@Override
		public void run() {
			try {
				tsim.setSpeed(TRAIN_ID, maxspeed);
				while (true) {
					SensorEvent event = tsim.getSensor(TRAIN_ID);
					String sensorText = createSensorID(event.getXpos(),	event.getYpos());
					int sensor = sensors.get(sensorText);
					passedSensor(sensor, event, direction);
					if (event.getStatus() == SensorEvent.INACTIVE) {
						lastSensor = sensor;
					}
				}
			} catch (InterruptedException ie) {
				System.err.println(ie.getMessage());
			} catch (CommandException ce) {
				System.err.println(ce.getMessage());
			}
		}
		
		public void acquireTrack(Semaphore trackToAcquire) throws  InterruptedException, CommandException {
			if (!trackToAcquire.tryAcquire()) { // if the track is occupied
				tsim.setSpeed(TRAIN_ID, 0); // stop the train
				trackToAcquire.acquire(); // wait for the track to be free
				tsim.setSpeed(TRAIN_ID, maxspeed); // and then accelerate to maxspeed
			} // else just keep going
		}
		
		public void chooseTrack(Monitor shortTrack, Monitor longTrack, Switch trainSwitch) throws InterruptedException, CommandException {
			boolean gotShortTrack = shortTrack.tryEnter();
			boolean leftIsShortestTrack = trainSwitch.equals(sw1) || trainSwitch.equals(sw2);
					
			int direction = (gotShortTrack == leftIsShortestTrack) ? TSimInterface.SWITCH_LEFT : TSimInterface.SWITCH_RIGHT;
			trainSwitch.changeDirection(direction);
						
			if (!gotShortTrack) { // if the track is occupied, then take the other
				longTrack.enter(this);
			} 
		}
		
		public void leavePreviousTrack() {
			int trackIndex = sensorTrackMap.get(lastSensor);
			monitors[trackIndex].leave();
		}
		
		public void passedSensor(int sensor, SensorEvent event, Direction direction) throws InterruptedException, CommandException {			
			switch (sensor) {
			case 1:
			case 2:
				if (direction == Direction.SOUTH) { 
					if (event.getStatus() == SensorEvent.INACTIVE) { // left the sensor completely
						monitors[2].leave();
					}
				} else { // heading north
					if (event.getStatus() == SensorEvent.ACTIVE) { // just came to the sensor
						monitors[2].enter(this);
						sw1.changeDirection(sensor == 1 ? TSimInterface.SWITCH_RIGHT : TSimInterface.SWITCH_LEFT);
					} 
				}
				break;
			case 3:
				if (direction == Direction.SOUTH) { 
					if (event.getStatus() == SensorEvent.ACTIVE) { 
						chooseTrack(monitors[1], monitors[0], sw1); 
					}
				} else { // heading north
					if (event.getStatus() == SensorEvent.INACTIVE) { 
						leavePreviousTrack();
					} 
				}
				break;
			case 4:
				if (direction == Direction.SOUTH) { 
					if (event.getStatus() == SensorEvent.INACTIVE) { 
						leavePreviousTrack(); 
					}
				} else { // heading north
					if (event.getStatus() == SensorEvent.ACTIVE) { 
						chooseTrack(monitors[4], monitors[3], sw2);
					} 
				}
				break;
			case 5:
			case 6:
				if (direction == Direction.SOUTH) { 
					if (event.getStatus() == SensorEvent.ACTIVE) {
						monitors[2].enter(this);
						sw2.changeDirection(sensor == 5 ? TSimInterface.SWITCH_RIGHT : TSimInterface.SWITCH_LEFT);
					}
				} else { // heading north
					if (event.getStatus() == SensorEvent.INACTIVE) { 
						monitors[2].leave();
					} 
				}
				break;
			case 7:
			case 8:
				if (direction == Direction.SOUTH) { 
					if (event.getStatus() == SensorEvent.INACTIVE) { 
						monitors[5].leave(); 
					}
				} else { // heading north
					if (event.getStatus() == SensorEvent.ACTIVE) { 
						monitors[5].enter(this);
						sw3.changeDirection(sensor == 7 ? TSimInterface.SWITCH_RIGHT : TSimInterface.SWITCH_LEFT);
					} 
				}
				break;
			case 9:
				if (direction == Direction.SOUTH) { 
					if (event.getStatus() == SensorEvent.ACTIVE) { 
						chooseTrack(monitors[4], monitors[3], sw3); 
					}
				} else { // heading north
					if (event.getStatus() == SensorEvent.INACTIVE) { 
						leavePreviousTrack();
					} 
				}
				break;
			case 10:
				if (direction == Direction.SOUTH) { 
					if (event.getStatus() == SensorEvent.INACTIVE) { 
						leavePreviousTrack(); 			
					}
				} else { // heading north
					if (event.getStatus() == SensorEvent.ACTIVE) { 
						chooseTrack(monitors[7], monitors[6], sw4);
					} 
				}
				break;
			case 11:
			case 12:
				if (direction == Direction.SOUTH) { 
					if (event.getStatus() == SensorEvent.ACTIVE) { 
						monitors[5].enter(this); 
						sw4.changeDirection(sensor == 12 ? TSimInterface.SWITCH_RIGHT : TSimInterface.SWITCH_LEFT);
					}
				} else { // heading north
					if (event.getStatus() == SensorEvent.INACTIVE) { 
						monitors[5].leave();
					} 
				}
				break;
			case 13:
			case 14:
				if (direction == Direction.SOUTH) { 
					if (event.getStatus() == SensorEvent.INACTIVE) { 
						monitors[8].leave(); 
					}
				} else { // heading north
					if (event.getStatus() == SensorEvent.ACTIVE) { 
						monitors[8].enter(this);
					} 
				}
				break;
			case 15:
			case 16:
				if (direction == Direction.SOUTH) { 
					if (event.getStatus() == SensorEvent.ACTIVE) { 
						monitors[8].enter(this); 
					}
				} else { // heading north
					if (event.getStatus() == SensorEvent.INACTIVE) { 
						monitors[8].leave();
					} 
				}
				break;
			case 17:
			case 18:
				if (direction == Direction.SOUTH) { 
					if (event.getStatus() == SensorEvent.ACTIVE) { 
						turnAtStation();
					}
				} 
				break;
			case 19:
			case 20:
				if (direction == Direction.NORTH) { 
					if (event.getStatus() == SensorEvent.ACTIVE) { 
						turnAtStation();
					}
				} 
				break;
			}
			
		}
		
		public void turnAtStation() throws CommandException, InterruptedException {
			tsim.setSpeed(TRAIN_ID, 0);
			int time_at_station = 1000;
			int stopTimeMS = time_at_station + 2 * simulation_speed_parameter * Math.abs(maxspeed);
			Thread.sleep(stopTimeMS);
			maxspeed = -maxspeed;
			direction = direction == Direction.SOUTH ? Direction.NORTH : Direction.SOUTH;
			tsim.setSpeed(TRAIN_ID, maxspeed);
		}
	}

	class Switch {
		private int x, y;

		public Switch(int x, int y) {
			this.x = x;
			this.y = y;
		}
		
		public void changeDirection(int switchDir) throws CommandException {			
			tsim.setSwitch(x, y, switchDir);			
		}
	}

	class Sensor {
		private int x, y;

		public Sensor(int x, int y) {
			this.x = x;
			this.y = y;
		}

		public boolean equals(SensorEvent event) {
			return this.x == event.getXpos() && this.y == event.getYpos();
		}
	}
}
