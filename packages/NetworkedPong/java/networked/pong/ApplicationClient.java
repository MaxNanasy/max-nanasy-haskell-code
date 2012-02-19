package networked.pong;

import javax.swing.JFrame;
import javax.swing.SwingUtilities;

public class ApplicationClient {
	public static void main(String[] args) {
		SwingUtilities.invokeLater(new Runnable() {
			@Override
			public void run() {
				final JFrame frame = new JFrame("Pong");
					frame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
					frame.getContentPane().add(new MainView());
					frame.pack();
					frame.setVisible(true);
			}
		});
	}
}
