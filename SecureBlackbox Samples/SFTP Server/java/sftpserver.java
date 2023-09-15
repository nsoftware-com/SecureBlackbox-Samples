/*
 * SecureBlackbox 2022 Java Edition - Sample Project
 *
 * This sample project demonstrates the usage of SecureBlackbox in a 
 * simple, straightforward way. It is not intended to be a complete 
 * application. Error handling and other checks are simplified for clarity.
 *
 * www.nsoftware.com/secureblackbox
 *
 * This code is subject to the terms and conditions specified in the 
 * corresponding product license agreement which outlines the authorized 
 * usage and restrictions.
 */

import java.io.*;
import java.awt.*;
import javax.swing.*;
import javax.swing.border.EmptyBorder;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.util.Date;

import secureblackbox.*;

public class sftpserver extends JDialog {
	
	private static final long serialVersionUID = 1L;
	private final JPanel contentPanel = new JPanel();
	private JTable tbConnections;
	private JTable tbLog;
	private JButton btnExit;
	private JButton btnStart;
	private JButton btnStop;
	private JButton btnSettings;
	
	private Sftpserver Server;
	private Usermanager usermanager;

	public static void main(String[] args)
	{
		sftpserver dialog = new sftpserver();
		dialog.setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);
		dialog.setVisible(true);
	}
	
	private void startServer()
	{
		try
		{
			Server.start();
			addLogEvent("Server started", false);
		}
		catch (Exception e)
		{
			e.printStackTrace();
		}
	}
	
	private void InitKeyStorage(){
		if (Server.getServerKeys().size() == 0)
		{
			/// NEVER USE THIS KEY IN REAL PROJECT. IT IS NOT SAFE. EVERYONE KNOWS IT !!!!!!!!!!!!!!
                        String S = "-----BEGIN RSA PRIVATE KEY-----\r\n" +
                            "MIIEpAIBAAKCAQEA1q/8c3VskNpxfeCN3PFhzOwWcjk6kRbu5eaSX9HwywLqRicK\r\n" +
                            "/1GhfXQbbp6KpTaibg0Zs01G3ll5nvjkD/+CHmLtfMW4HkjWDZ4++53iNJkT4GDr\r\n" +
                            "PAgeR+llxCNLYwSQhrqXr7jMcrh9/DRFiPd1Pxzu1fT/AvBcFh6P+YF4Ex7r8pdu\r\n" +
                            "fKRnZ33yER6xqecwN6prCADq7r8BeTwKwerv/3l2lbywqWq4xeZVEabLQWu1aiat\r\n" +
                            "9DahcJPBDaAMvTMQxNfY4kFsvBxDuLu7nsi4j/LZvovz4gibxy05aRtAoG3NyHCl\r\n" +
                            "DTXkytoro/vFQ2KTgk4VETn/rUGzTh8gGfymcwIDAQABAoIBAQC07vLDV+Zcmk1l\r\n" +
                            "+hd1atWzABHWyFaMqxhCF7WrHeNjJRzObN8+2MxST8VC0Ekm/kmcmFvdmjYs9Jmk\r\n" +
                            "mjyTIxLFizh9nKEUnOwR5BlOs/xNuelfhC8ck/b5QkIrMFe7psArXBy00vkzgjtj\r\n" +
                            "j6Y+zdc5jqfXGDjHgE0Ls0xBypSo/y4Z6NAQCTV5WjLXO7IOYc3vJmOcR/yN9ymI\r\n" +
                            "v9VqDTInd8XBYsagiLF73CZ/vgLkPGChHpFSLARUSwQyVbq+m4Ow7jbP9+qAPRGC\r\n" +
                            "hsaAH4DR8OZMm5cpSWLV21X0yEipahk/2HNMz/Z69Nm+yAJQP/Rt/n9Drh7v79/B\r\n" +
                            "rm4K7mEBAoGBAOLYUa6UOqEvCQ+M2ojeILxUO9AVcU6biIAOWW36uWIgk+Tu4PaD\r\n" +
                            "gw+a5ht9nuuCMY/fMiIM/w2mCQ5KUEnVkxIJrhRJt6DIcqb0e4pQU/vN+cEh+NjL\r\n" +
                            "OX79uH8MCiH4t+JlDmpDGdfNmRpkdwrXcQB4TSyda/xofS98Xhvf0dDzAoGBAPJH\r\n" +
                            "qGwgv89NZzvvnOhDhyzo26gTOZ2DBfiYarc+dM0XqSzPLyfMoWQPE8X3Las0WZ5K\r\n" +
                            "B01LyKgtwB0FcVwEeeF7libiBqnjnlUIsqIPdoIVEtnpr6c+pe9ydmlMZNx6sV6b\r\n" +
                            "kvltQKfpWa9M3j8V4tKsB/TeU7yApswbg1h+KzSBAoGARGDmhlRiM/IEri5MNZOX\r\n" +
                            "lI1jlj7Qb9yMUeWBaZh5Ry4AxrKHF2ffGtYbZNACFoWhQPgLmZ4HR+rzsd4Ow6Lg\r\n" +
                            "eUB3u9/YvVuOy+rITpIWLsIQ9fmylE72HuSGN62+ZC0f6NuDc96ULKvp6b288ISu\r\n" +
                            "qHCa+sbdJTGuAqFfkNFv4qcCgYEAm7uv6QEMCiaI6x/UTf/KDfYH63ugFz03Q1pF\r\n" +
                            "jlAZKBPDbMgoWX7RZs3COgfuVRQrXvHay7ag3mm6CW2MNkeySFv2Yjv7aIylI6eZ\r\n" +
                            "m04xMOB245ewjuJYKxf6QSkX1BkRleyOhsHRQ51dRq80VHyu9hgabS3TyWEw1hYB\r\n" +
                            "UYqYHwECgYAOHYzL98Vhl5o0XtTaciFHf8IaY+GE/+lWK8waISyvbRnCRTFDaJmL\r\n" +
                            "Fl4be/VS3KVnTnRssCTfVjwFb36XJDPZCuQZqAasFnGNcbz+5YVCIr7kbODXIshi\r\n" +
                            "kuqPupUahcTeR7jf63740ooTJqO7wQ8wMwgloWyL6EYpxwG6VH0IVg==\r\n" +
                            "-----END RSA PRIVATE KEY-----";

			try
			{
				Sshkeymanager keymanager = new Sshkeymanager();
				keymanager.importBytes(S.getBytes(StandardCharsets.US_ASCII), "");
				Server.getServerKeys().add(keymanager.getKey());
			}
			catch (Exception e)
			{
				e.printStackTrace();
			}
		}
	}
	
	private void Init()
	{
		try
		{
			Server = new Sftpserver();
			Server.setBaseDir("D:\\");//Current dir
			Server.addSftpserverEventListener(new DefaultSftpserverEventListener() {
				
				@Override
				public void writeFile(SftpserverWriteFileEvent e) {
					// TODO Auto-generated method stub
					
				}
				
				@Override
				public void supercoreIntercept(SftpserverSupercoreInterceptEvent e) {
					// TODO Auto-generated method stub
					
				}
				
				@Override
				public void setAttributes(SftpserverSetAttributesEvent e) {
					// TODO Auto-generated method stub
					
				}
				
				@Override
				public void sessionEstablished(SftpserverSessionEstablishedEvent e) {
					contablemodel model = (contablemodel) tbConnections.getModel();
					model.updateRowSession(e.connectionID, "connected");

					addLogEvent("Session " + e.connectionID + " established", false);
				}
				
				@Override
				public void sessionClosed(SftpserverSessionClosedEvent e) {
					contablemodel model = (contablemodel) tbConnections.getModel();
					model.updateRowSession(e.connectionID, "disconnected");

					addLogEvent("Session " + e.connectionID + " closed", false);
				}
				
				@Override
				public void requestAttributes(SftpserverRequestAttributesEvent e) {
					// TODO Auto-generated method stub
					
				}
				
				@Override
				public void renameFile(SftpserverRenameFileEvent e) {
					// TODO Auto-generated method stub
					
				}
				
				@Override
				public void remove(SftpserverRemoveEvent e) {
					// TODO Auto-generated method stub
					
				}
				
				@Override
				public void readFile(SftpserverReadFileEvent e) {
					// TODO Auto-generated method stub
					
				}
				
				@Override
				public void openFile(SftpserverOpenFileEvent e) {
					// TODO Auto-generated method stub
					
				}
				
				@Override
				public void findNext(SftpserverFindNextEvent e) {
					// TODO Auto-generated method stub
					
				}
				
				@Override
				public void findFirst(SftpserverFindFirstEvent e) {
					// TODO Auto-generated method stub
					
				}
				
				@Override
				public void findClose(SftpserverFindCloseEvent e) {
					// TODO Auto-generated method stub
					
				}
				
				@Override
				public void externalSign(SftpserverExternalSignEvent e) {
					// TODO Auto-generated method stub
					
				}
				
				@Override
				public void error(SftpserverErrorEvent e) {
					String S = "Error " + e.errorCode;
					if (e.description.length() > 0)
						S = S + ". Description: " + e.description;
					addLogEvent(S, true);
				}
				
				@Override
				public void disconnect(SftpserverDisconnectEvent e) {
					contablemodel model = (contablemodel) tbConnections.getModel();
					model.removeRowSession(e.connectionID);
				}
				
				@Override
				public void createDirectory(SftpserverCreateDirectoryEvent e) {
					// TODO Auto-generated method stub
					
				}
				
				@Override
				public void connect(SftpserverConnectEvent e) {
					contablemodel model = (contablemodel) tbConnections.getModel();
					model.addRow(e.connectionID, e.remoteAddress, "anonymous", "handshaking");

					addLogEvent("User from " + e.remoteAddress + " connected", false);
				}
				
				@Override
				public void closeFile(SftpserverCloseFileEvent e) {
					// TODO Auto-generated method stub
					
				}
				
				@Override
				public void beforeUploadFile(SftpserverBeforeUploadFileEvent e) {
					// TODO Auto-generated method stub
					
				}
				
				@Override
				public void beforeSetAttributes(SftpserverBeforeSetAttributesEvent e) {
					// TODO Auto-generated method stub
					
				}
				
				@Override
				public void beforeRequestAttributes(SftpserverBeforeRequestAttributesEvent e) {
					// TODO Auto-generated method stub
					
				}
				
				@Override
				public void beforeRenameFile(SftpserverBeforeRenameFileEvent e) {
					// TODO Auto-generated method stub
					
				}
				
				@Override
				public void beforeRemove(SftpserverBeforeRemoveEvent e) {
					// TODO Auto-generated method stub
					
				}
				
				@Override
				public void beforeFind(SftpserverBeforeFindEvent e) {
					// TODO Auto-generated method stub
					
				}
				
				@Override
				public void beforeDownloadFile(SftpserverBeforeDownloadFileEvent e) {
					// TODO Auto-generated method stub
					
				}
				
				@Override
				public void beforeCreateDirectory(SftpserverBeforeCreateDirectoryEvent e) {
					// TODO Auto-generated method stub
					
				}
				
				@Override
				public void authSucceeded(SftpserverAuthSucceededEvent e) {
					addLogEvent("Auth " + e.authType + " by User " + e.username + " succeeded", false);
				}
				
				@Override
				public void authPublicKey(SftpserverAuthPublicKeyEvent e) {
					e.accept = true;
				}
				
				@Override
				public void authPassword(SftpserverAuthPasswordEvent e) {
					e.accept = true;
				}
				
				@Override
				public void authFailed(SftpserverAuthFailedEvent e) {
					addLogEvent("Auth " + e.authType + " by User " + e.username + " failed", false);
				}
				
				@Override
				public void authAttempt(SftpserverAuthAttemptEvent e) {
					addLogEvent("Auth " + e.authType + " by User " + e.username, false);
					e.accept = true;
				}
				
				@Override
				public void afterSetAttributes(SftpserverAfterSetAttributesEvent e) {
					// TODO Auto-generated method stub
					
				}
				
				@Override
				public void afterRequestAttributes(SftpserverAfterRequestAttributesEvent e) {
					// TODO Auto-generated method stub
					
				}
				
				@Override
				public void afterRenameFile(SftpserverAfterRenameFileEvent e) {
					// TODO Auto-generated method stub
					
				}
				
				@Override
				public void afterRemove(SftpserverAfterRemoveEvent e) {
					// TODO Auto-generated method stub
					
				}
				
				@Override
				public void afterCreateDirectory(SftpserverAfterCreateDirectoryEvent e) {
					// TODO Auto-generated method stub
					
				}
				
				@Override
				public void accept(SftpserverAcceptEvent e) {
					addLogEvent("Try connection from " + e.remoteAddress, false);
					e.accept = true;
				}
			});

			usermanager = new Usermanager();
			usermanager.load("Users.dat", "dsf%^dfg444");

			for (int i = 0; i < usermanager.getUsers().size(); i++)
				Server.getUsers().add(usermanager.getUsers().item(i));

			InitKeyStorage();
		}
		catch (Exception e)
		{
			e.printStackTrace();
		}
	}
	
	private void stopServer(){
		try
		{
			Server.stop();
			addLogEvent("Server stoped", false);
		}
		catch (Exception e)
		{
			e.printStackTrace();
		}
	}
	
	public void addLogEvent(String msg, boolean eFlag) {
		logtablemodel model = (logtablemodel)tbLog.getModel();
		model.addRow(new Date(), msg, eFlag ? "Eror" : "Info");
	}	

	private void showSettings()
	{
		settingsfrm frm = new settingsfrm(Server);
		frm.setModal(true);
		frm.setVisible(true);

		try
		{
			usermanager.getUsers().clear();
			for (int i = 0; i < Server.getUsers().size(); i++)
				usermanager.getUsers().add(Server.getUsers().item(i));
			usermanager.save("Users.dat", "dsf%^dfg444");
		}
		catch (Exception e)
		{
			e.printStackTrace();
		}

		frm.dispose();
	}

	private Icon getIcon(String name) {
		URL resPath = getClass().getResource("/" + name + ".ico");
		if (resPath == null) return new ImageIcon();
		return new ImageIcon(resPath);
	}

	public sftpserver()
	{
		addWindowListener(new WindowAdapter() {
			@Override
			public void windowClosing(WindowEvent e) {
				stopServer();
			}
		});
		setTitle("SFTP Server demo");

		setBounds(100, 100, 720, 410);
		getContentPane().setLayout(new BorderLayout());
		contentPanel.setBorder(new EmptyBorder(5, 5, 5, 5));
		getContentPane().add(contentPanel, BorderLayout.CENTER);
		contentPanel.setLayout(null);

		JLabel lblCaption = new JLabel("This sample is a basic SFTP server. Tune up the server component as needed, and then press Start to activate the server. ");
		lblCaption.setBounds(10, 5, 710, 14);
		lblCaption.setForeground(new Color(49, 106, 197));
		contentPanel.add(lblCaption);

		JToolBar toolBar = new JToolBar();
		toolBar.setFloatable(false);
		toolBar.setBounds(5, 25, 695, 21);
		contentPanel.add(toolBar);

		btnStart = new JButton("");
		btnStart.setToolTipText("Connect");
		btnStart.setIcon(getIcon("connect"));
		btnStart.setFocusPainted(false);
		btnStart.setBorderPainted(false);
		btnStart.addMouseListener(new MouseAdapter() {
			@Override
			public void mousePressed(MouseEvent e) {
				startServer();
			}
		});
		toolBar.add(btnStart);

		btnStop = new JButton("");
		btnStop.setToolTipText("Disconnect");
		btnStop.setIcon(getIcon("disconnect"));
		btnStop.setFocusPainted(false);
		btnStop.setBorderPainted(false);
		btnStop.addMouseListener(new MouseAdapter() {
			@Override
			public void mousePressed(MouseEvent e) {
				stopServer();
			}
		});
		toolBar.add(btnStop);

		btnSettings = new JButton("");
		btnSettings.setToolTipText("Settings");
		btnSettings.setIcon(getIcon("settings"));
		btnSettings.setFocusPainted(false);
		btnSettings.setBorderPainted(false);
		btnSettings.addMouseListener(new MouseAdapter() {
			@Override
			public void mousePressed(MouseEvent e) {
				showSettings();
			}
		});
		toolBar.add(btnSettings);

		btnExit = new JButton("");
		btnExit.setToolTipText("Exit");
		btnExit.setIcon(getIcon("exit"));
		btnExit.setFocusPainted(false);
		btnExit.setBorderPainted(false);
		btnExit.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				stopServer();
				dispose();
			}
		});
		btnExit.addMouseListener(new MouseAdapter() {
			@Override
			public void mousePressed(MouseEvent e) {
				stopServer();
				dispose();
			}
		});
		
		toolBar.add(btnExit);
		
		JScrollPane scrollPane = new JScrollPane();
		scrollPane.setBounds(5, 50, 695, 183);
		contentPanel.add(scrollPane);
		
		tbConnections = new JTable(new contablemodel());
		tbConnections.setFillsViewportHeight(true);
		scrollPane.setViewportView(tbConnections);
		
		JScrollPane scrollPane_1 = new JScrollPane();
		scrollPane_1.setBounds(5, 235, 695, 133);
		contentPanel.add(scrollPane_1);
		
		tbLog = new JTable(new logtablemodel());
		tbLog.setFillsViewportHeight(true);
		scrollPane_1.setViewportView(tbLog);

		Init();
	}
	

}


















































































class ConsoleDemo {
  private static BufferedReader bf = new BufferedReader(new InputStreamReader(System.in));

  static String input() {
    try {
      return bf.readLine();
    } catch (IOException ioe) {
      return "";
    }
  }
  static char read() {
    return input().charAt(0);
  }

  static String prompt(String label) {
    return prompt(label, ":");
  }
  static String prompt(String label, String punctuation) {
    System.out.print(label + punctuation + " ");
    return input();
  }

  static String prompt(String label, String punctuation, String defaultVal)
  {
	System.out.print(label + " [" + defaultVal + "] " + punctuation + " ");
	String response = input();
	if(response.equals(""))
		return defaultVal;
	else
		return response;
  }

  static char ask(String label) {
    return ask(label, "?");
  }
  static char ask(String label, String punctuation) {
    return ask(label, punctuation, "(y/n)");
  }
  static char ask(String label, String punctuation, String answers) {
    System.out.print(label + punctuation + " " + answers + " ");
    return Character.toLowerCase(read());
  }

  static void displayError(Exception e) {
    System.out.print("Error");
    if (e instanceof SecureBlackboxException) {
      System.out.print(" (" + ((SecureBlackboxException) e).getCode() + ")");
    }
    System.out.println(": " + e.getMessage());
    e.printStackTrace();
  }
}



