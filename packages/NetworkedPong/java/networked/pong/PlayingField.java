package networked.pong;

import java.awt.Component;
import java.awt.Graphics;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

import javax.swing.SwingUtilities;

import networked.pong.velocity.SetVelocityListener;
import networked.pong.velocity.Velocity;

@SuppressWarnings("serial")
class PlayingField extends Component {

	final private SetVelocityListener setVelocityListener;
	
	final public int totalWidth = 400, totalHeight = 200;
	
	final private int ballWidth = 4, ballHeight = ballWidth;
	final private int ballX = totalWidth >> 1, ballY = totalHeight >> 1;

	final private int paddleWidth = ballWidth, paddleHeight = totalHeight >> 2;
	final private int myPaddleX = paddleWidth;
	private int myPaddleY = totalHeight >> 1;
	final private int otherPaddleX = totalWidth - paddleWidth;
	private int otherPaddleY = myPaddleY;

	public PlayingField(SetVelocityListener setVelocityListener) {
		this.setVelocityListener = setVelocityListener;
		setFocusable(true);
		addMouseListener(new MouseAdapter() {
			@Override
			public void mousePressed(MouseEvent e) {
				if (SwingUtilities.isLeftMouseButton(e)) {
					requestFocusInWindow();
				}
			}
		});
		addKeyListener(new KeyListener());
	}

	@Override
	public void paint(Graphics g) {
		super.paint(g);
		g.clearRect(0, 0, getWidth(), getHeight());
		g.fillRect(ballX, ballY, ballWidth, ballHeight);
		g.fillRect(myPaddleX, myPaddleY, paddleWidth, paddleHeight);
		g.fillRect(otherPaddleX, otherPaddleY, paddleWidth, paddleHeight);
	}
	
	private void setVelocity(Velocity velocity) {
		setVelocityListener.setVelocity(velocity);
	}
	
	private class KeyListener extends KeyAdapter {

		@Override
		public void keyPressed(KeyEvent e) {
			System.err.println("Key Pressed: " + e.getKeyChar());
			switch (e.getKeyCode()) {
				case KeyEvent.VK_UP:
					setVelocity(Velocity.UP);
				break;
				case KeyEvent.VK_DOWN:
					setVelocity(Velocity.DOWN);
				break;
			}
		}

		@Override
		public void keyReleased(KeyEvent e) {
			System.err.println("Key Released: " + e.getKeyChar());
			switch (e.getKeyCode()) {
			case KeyEvent.VK_UP:
			case KeyEvent.VK_DOWN:
				setVelocity(Velocity.STOPPED);
			}
		}

		@Override
		public void keyTyped(KeyEvent e) {
			System.err.println("Key Typed: " + e.getKeyChar());
		}
		
	}
	
}
