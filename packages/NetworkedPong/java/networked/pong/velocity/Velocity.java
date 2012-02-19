package networked.pong.velocity;

public enum Velocity {

	STOPPED("Stopped"), UP("Up"), DOWN("Down");

	final private String networkString;

	public String getNetworkString() {
		return networkString;
	}
	
	private Velocity(String networkString) {
		this.networkString = networkString;
	}

}
