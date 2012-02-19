package networked.pong;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JLabel;

@SuppressWarnings("serial")
class NotificationBar extends Box {

	final private JLabel label;
	
	public NotificationBar() {
		super(BoxLayout.X_AXIS);
		add(label = new JLabel());
	}

	public void addNotification(String notification) {
		label.setText(notification);
	}

}
