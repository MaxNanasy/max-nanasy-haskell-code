package networked.pong;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JLabel;

@SuppressWarnings("serial")
class ScoreBoard extends Box {

	final private int myScore = 0, otherScore = 0;

	public ScoreBoard() {
		super(BoxLayout.X_AXIS);
		add(new JLabel(Integer.toString(myScore)));
		add(createGlue());
		add(new JLabel(Integer.toString(otherScore)));
	}

}
