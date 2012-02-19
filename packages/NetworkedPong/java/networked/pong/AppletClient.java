package networked.pong;

import java.lang.reflect.InvocationTargetException;

import javax.swing.JApplet;
import javax.swing.SwingUtilities;

@SuppressWarnings("serial")
public class AppletClient extends JApplet {
	public AppletClient() throws InvocationTargetException, InterruptedException {
		SwingUtilities.invokeAndWait(new Runnable () {
			@Override
			public void run() {
				getContentPane().add(new MainView());
			}
		});
	}
}
