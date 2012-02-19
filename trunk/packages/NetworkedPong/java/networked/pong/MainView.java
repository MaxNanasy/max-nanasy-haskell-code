package networked.pong;

import java.awt.Dimension;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JSeparator;

import networked.pong.velocity.Velocity;

@SuppressWarnings("serial")
class MainView extends Box {

	final private IOHandler ioHandler;
	final private NotificationBar notificationBar;
	
	public MainView() {

		super(BoxLayout.Y_AXIS);

		ioHandler = new IOHandler(new IOHandlerListener());

		add(new ScoreBoard());
		add(new JSeparator());
		add(notificationBar = new NotificationBar());
		add(new JSeparator());
		final PlayingField playingField = new PlayingField(new SetVelocityListener());
			playingField.setPreferredSize(new Dimension(playingField.totalWidth, playingField.totalHeight));
		add(playingField);

	}

	private class SetVelocityListener implements networked.pong.velocity.SetVelocityListener {
		@Override
		public void setVelocity(Velocity velocity) {
			ioHandler.sendLine("SetVelocity " + velocity.getNetworkString());
		}
	}
	
	private class IOHandlerListener implements IOHandler.IOHandlerListener {
		@Override
		public void unableToConnectToServer() {
			notificationBar.addNotification("Unable to connect to server!");
		}
		@Override
		public void disconnectedFromServer() {
			notificationBar.addNotification("Disconnected from server!");
		}
		@Override
		public void getLine(String line) {
			notificationBar.addNotification("Line from server: " + line);
		}
	}

}
