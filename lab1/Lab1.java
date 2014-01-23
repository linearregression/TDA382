import java.util.concurrent.Semaphore;

import TSim.*;

public class Lab1 {

	Semaphore semaphore1 = new Semaphore(1);
	
	Switch sw1 = new Switch(17, 7);
	Switch sw2 = new Switch(4, 9);
	Switch sw3 = new Switch(15, 9);
	Switch sw4 = new Switch(3, 11);
	

	public static void main(String[] args) {
		new Lab1(args);
	}

	public Lab1(String[] args) {
		TSimInterface tsi = TSimInterface.getInstance();
		Train train1 = new Train(1, Integer.parseInt(args[0]));
		Train train2 = new Train(2, Integer.parseInt(args[1]));

		try {
			System.err.println("Something something");
			tsi.setSpeed(train1.TRAIN_ID, train1.maxspeed);
			tsi.setSpeed(train2.TRAIN_ID, train2.maxspeed);
			
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
		private SensorEvent lastSensor;

		public Train(int trainID, int maxspeed) {
			this.TRAIN_ID = trainID;
			this.maxspeed = maxspeed;
		}

		@Override
		public void run() {
			TSimInterface tsim = TSimInterface.getInstance();
			while (true) {
				System.err.println("while start");
				try {
					System.err.println("Gonna wait for sensor now");
					SensorEvent event = tsim.getSensor(TRAIN_ID);
					//Compute direction
					lastSensor = event;
					System.err.println("Received an event!" + event.toString());
					String sensor = createSensorID(event.getXpos(),
							event.getYpos());
					switch (sensor) {
					case "16 7":
						if (event.getStatus() == SensorEvent.ACTIVE) {
							semaphore1.acquire();
							tsim.setSwitch(17, 7, tsim.SWITCH_RIGHT);
						}
						System.err.println("entered case");
						break;
					case "16 9":
						System.err.println("16 9 event");
						if (event.getStatus() == SensorEvent.INACTIVE) {
							System.err.println("semaphore1.release()");
							semaphore1.release();
						}
						System.err.println("16 9 done");
						break;
					case "13 9":
						if (semaphore1.availablePermits() == 0) {
							tsim.setSpeed(TRAIN_ID, 0);
						}
						System.err.println("semaphore1.acquire()");
						semaphore1.acquire();
						System.err.println("semaphore1 successfully aquired");
						tsim.setSwitch(15, 9, tsim.SWITCH_RIGHT);
						tsim.setSpeed(TRAIN_ID, maxspeed);
						break;
					default:
						break;
					}
				} catch (InterruptedException ie) {
				} catch (CommandException ce) {
					// TODO Auto-generated catch block
					ce.printStackTrace();
				}
				System.err.println("while end");
			}
		}
	}

	class Switch {
		private int x, y;

		public Switch(int x, int y) {
			this.x = x;
			this.y = y;
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
