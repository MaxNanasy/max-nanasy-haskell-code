package networked.pong;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintStream;
import java.net.Socket;
import java.util.List;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

import javax.swing.SwingUtilities;
import javax.swing.SwingWorker;

import org.newsclub.net.unix.AFUNIXSocket;
import org.newsclub.net.unix.AFUNIXSocketAddress;

class IOHandler {
	
	final private IOHandlerListener ioHandlerListener;
	final private BlockingQueue<String> outputLineQueue = new LinkedBlockingQueue<String>();
	
	public IOHandler(IOHandlerListener l) {
		ioHandlerListener = l;
		final Thread thread = new Thread(new InitializeRunner());
			thread.setDaemon(true);
			thread.start();
	}

	public void sendLine(String line) {
		for (;;) {
			try {
				outputLineQueue.put(line);
				break;
			}
			catch (InterruptedException e) {
			}
		}
	}
	
	private class InitializeRunner implements Runnable {

		final private static String socketPath = "/tmp/pong.sock";
		final private File socketFile = new File(socketPath);		

		@Override
		public void run() {
			final BufferedReader inputStream;
			final PrintStream outputStream;
			try {
				final Socket socket = connectToSocket();
				inputStream = new BufferedReader(new InputStreamReader(socket.getInputStream()));
				outputStream = new PrintStream(socket.getOutputStream());
			}
			catch (IOException e) {
				SwingUtilities.invokeLater(new Runnable() {
					@Override
					public void run() {
						ioHandlerListener.unableToConnectToServer();
					}
				});
				return;
			}
			new HandleInputWorker(inputStream).execute();
			final Thread thread = new Thread(new HandleOutputRunner(outputStream));
				thread.setDaemon(true);
				thread.start();
		}

		private Socket connectToSocket() throws IOException {
			return AFUNIXSocket.connectTo(new AFUNIXSocketAddress(socketFile));
		}
		
	}

	public static interface IOHandlerListener {
		void unableToConnectToServer();
		void disconnectedFromServer();
		void getLine(String line);
	}
	
	private class HandleInputWorker extends SwingWorker<Void, String> {

		private final BufferedReader inputStream;

		public HandleInputWorker(BufferedReader inputStream) {
			this.inputStream = inputStream;
		}

		@Override
		protected Void doInBackground() throws IOException {
			for (;;) {
				final String stateString = inputStream.readLine();
				if (stateString == null) {
					break;
				}
				publish(stateString);
			}
			SwingUtilities.invokeLater(new Runnable() {
				@Override
				public void run() {
					ioHandlerListener.disconnectedFromServer();
				}
			});
			return null;
		}
		
		@Override
		protected void process(List<String> stateStrings) {
			ioHandlerListener.getLine(stateStrings.get(stateStrings.size() - 1));
		}

	}

	private class HandleOutputRunner implements Runnable {
		private final PrintStream outputStream;
		public HandleOutputRunner(PrintStream outputStream) {
			this.outputStream = outputStream;
		}
		private String uninterruptablyTakeLine() {
			for (;;) {
				try {
					return outputLineQueue.take();
				}
				catch (InterruptedException e) {
				}
			}
		}
		@Override
		public void run() {
			for (;;) {
				final String line = uninterruptablyTakeLine();
				outputStream.println(line);
			}
		}
	}

}
