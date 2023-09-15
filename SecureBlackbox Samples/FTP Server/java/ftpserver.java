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
import java.net.URL;
import java.util.Date;
import javax.swing.*;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;

import secureblackbox.*;

public class ftpserver extends JDialog {

	private static final long serialVersionUID = 1L;
	private JTable tableConnections;
	private static JTable tableLog;
	private JButton btnStart;
	private JButton btnStop;
	private conntablemodel cmodel;

	private Ftpserver Server;
	private Usermanager ftpusermanager;

	/**
	 * Launch the application.
	 */
	public static void main(String[] args) {
		EventQueue.invokeLater(new Runnable() {
			public void run() {
				try {
					ftpserver dialog = new ftpserver();
					dialog.setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);
					dialog.setVisible(true);
				} catch (Exception e) {
					e.printStackTrace();
				}
			}
		});
	}

	/**
	 * Create the dialog.
	 */
	public ftpserver() {
		addWindowListener(new WindowAdapter() {
			@Override
			public void windowClosing(WindowEvent e) {
				stopClick();
				System.exit(0);
			}
		});	
		
		setResizable(false);
		setTitle("FTPServer Java demo");
		setBounds(100, 100, 710, 540);

		JLabel lblCaption = new JLabel("This sample is a basic FTP server. Tune up the server component as needed, and then press Start to activate the server.");
		lblCaption.setBounds(10, 5, 700, 14);
		lblCaption.setForeground(new Color(49, 106, 197));
		getContentPane().add(lblCaption);

		getContentPane().setLayout(null);

		JToolBar toolBar = new JToolBar();
		toolBar.setFloatable(false);
		toolBar.setBounds(5, 30, 695, 27);
		getContentPane().add(toolBar);
		
		btnStart = new JButton("");
		btnStart.setFocusable(false);
		btnStart.setFocusPainted(false);
		btnStart.setRequestFocusEnabled(false);
		btnStart.setIcon(getIcon("start"));
		btnStart.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				startClick();
			}
		});
		toolBar.add(btnStart);
		
		btnStop = new JButton("");
		btnStop.setFocusable(false);
		btnStop.setFocusPainted(false);
		btnStop.setRequestFocusEnabled(false);
		btnStop.setIcon(getIcon("stop"));
		btnStop.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				stopClick();
			}
		});
		toolBar.add(btnStop);
		
		JButton btnSettings = new JButton("");
		btnSettings.setFocusable(false);
		btnSettings.setFocusPainted(false);
		btnSettings.setRequestFocusEnabled(false);
		btnSettings.setIcon(getIcon("settings"));
		btnSettings.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				if (settingsdialog.changeSettings(Server))
				{
					try
					{
						ftpusermanager.getUsers().clear();
						for (int i = 0; i < Server.getUsers().size(); i++)
							ftpusermanager.getUsers().add(Server.getUsers().item(i));
						ftpusermanager.save("Users.dat", "asd$%:sss");
					}
					catch (Exception ex)
					{
						ex.printStackTrace();
					}
				}
			}
		});
		toolBar.add(btnSettings);
		
		JButton btnExit = new JButton("");
		btnExit.setFocusable(false);
		btnExit.setFocusPainted(false);
		btnExit.setRequestFocusEnabled(false);
		btnExit.setIcon(getIcon("exit"));
		btnExit.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				dispose();
				System.exit(0);
			}
		});
		toolBar.add(btnExit);
		
		JScrollPane scrollPane = new JScrollPane();
		scrollPane.setBounds(5, 60, 695, 325);
		getContentPane().add(scrollPane);
		
		cmodel = new conntablemodel();
		tableConnections = new JTable(cmodel);
		tableConnections.setFillsViewportHeight(true);
		scrollPane.setViewportView(tableConnections);
		
		JScrollPane scrollPane_1 = new JScrollPane();
		scrollPane_1.setBounds(5, 395, 695, 110);
		getContentPane().add(scrollPane_1);
		
		tableLog = new JTable(new logtablemodel());
		tableLog.setFillsViewportHeight(true);
		scrollPane_1.setViewportView(tableLog);
		
		init();
	}

	private Icon getIcon(String name) {
		URL resPath = getClass().getResource("/" + name + ".ico");
		if (resPath == null) return new ImageIcon();
		return new ImageIcon(resPath);
	}

	protected void stopClick() {
        if (Server.isActive())
        {
			try
			{
				Server.stop();
				btnStop.setEnabled(false);
				btnStart.setEnabled(true);
				Log("Server stopped.", false);
			}
			catch (Exception ex)
			{
				ex.printStackTrace();
			}
        }
	}

	protected void startClick() {
        if (!Server.isActive())
        {
        	try
			{
				Server.setPort(demosettings.getSettings().getListeningPort());
				Server.setHost(demosettings.getSettings().getListeningAddress());
				Server.setPassiveModeHost(demosettings.getSettings().getPassiveModeHost());
				Server.setImplicitSSL(demosettings.getSettings().getImplicitSSL());
				Server.setAllowAnonymous(demosettings.getSettings().getAllowAnonymous());
				Server.setDataPortRangeFrom(demosettings.getSettings().getPortRangeFrom());
				Server.setDataPortRangeTo(demosettings.getSettings().getPortRangeTo());

				//int Opts = cfsoPlainLogin | cfsoEncryption | cfsoAuth | cfsoClearControlChannel | cfsoClearDataChannel | cfsoEncryptedDataChannel;
				int Opts = 1 | 2 | 4 | 8 | 16 | 32;
				if (demosettings.getSettings().getRequireTLS()) {
					Opts = Opts & (~1);//cfsoPlainLogin
					Opts = Opts & (~8);//cfsoClearControlChannel
				}
				if (demosettings.getSettings().getRequireTLSForData())
					Opts = Opts & (~16);//cfsoClearDataChannel
				Server.config("SecurityOptions=" + Opts);

				Server.getServerCertificates().clear();
				if (demosettings.getSettings().getServerCertificate() != null)
					Server.getServerCertificates().add(demosettings.getSettings().getServerCertificate());

				Server.start();
				btnStop.setEnabled(true);
				btnStart.setEnabled(false);
				Log("Server started.", false);
			}
			catch (Exception ex)
			{
				ex.printStackTrace();
			}
        }
	}

	void init()
	{
		Server = new Ftpserver();
		ftpusermanager = new Usermanager();
		try
		{
			ftpusermanager.load("Users.dat", "asd$%:sss");
			for (int i = 0; i < ftpusermanager.getUsers().size(); i++)
				Server.getUsers().add(ftpusermanager.getUsers().item(i));
		}
		catch (Exception ex)
		{
			ex.printStackTrace();
		}

        demosettings.getSettings().Load();
        Log("Settings loaded", false);

        if (demosettings.getSettings().getSettingsFound() == false)
		{
			showMessage("Welcome to the FTP Server Demo Application!\n" +
				"Thank you for your interest in our products.\n\n" +
				"First of all, you need to create at least one user account.\n\n" + 
				"After clicking the OK button you will be redirected to the\n" +
				"User settings window.");
			settingsdialog.changeSettings(Server);
		}

		initEvents();
	}

	private void initEvents() {
		try {
			Server.addFtpserverEventListener(new FtpserverEventListener() {
				
				@Override
				public void writeFile(FtpserverWriteFileEvent e) {
					// TODO Auto-generated method stub
					
				}
				
				@Override
				public void uploadFile(FtpserverUploadFileEvent e) {
					// TODO Auto-generated method stub
					
				}
				
				@Override
				public void transferCompleted(FtpserverTransferCompletedEvent e) {
					// TODO Auto-generated method stub
					
				}
				
				@Override
				public void supercoreIntercept(FtpserverSupercoreInterceptEvent e) {
					// TODO Auto-generated method stub
					
				}
				
				@Override
				public void requestAttributes(FtpserverRequestAttributesEvent e) {
					// TODO Auto-generated method stub
					
				}
				
				@Override
				public void renameFile(FtpserverRenameFileEvent e) {
					// TODO Auto-generated method stub
					
				}
				
				@Override
				public void removeFile(FtpserverRemoveFileEvent e) {
					// TODO Auto-generated method stub
					
				}
				
				@Override
				public void removeDirectory(FtpserverRemoveDirectoryEvent e) {
					// TODO Auto-generated method stub
					
				}
				
				@Override
				public void readFile(FtpserverReadFileEvent e) {
					// TODO Auto-generated method stub
					
				}
				
				@Override
				public void findNext(FtpserverFindNextEvent e) {
					// TODO Auto-generated method stub
					
				}
				
				@Override
				public void findInit(FtpserverFindInitEvent e) {
					// TODO Auto-generated method stub
					
				}
				
				@Override
				public void findClose(FtpserverFindCloseEvent e) {
					// TODO Auto-generated method stub
					
				}
				
				@Override
				public void externalSign(FtpserverExternalSignEvent e) {
					// TODO Auto-generated method stub
					
				}
				
				@Override
				public void error(FtpserverErrorEvent e) {
					// TODO Auto-generated method stub
					
				}
				
				@Override
				public void downloadFile(FtpserverDownloadFileEvent e) {
					// TODO Auto-generated method stub
					
				}
				
				@Override
				public void disconnect(FtpserverDisconnectEvent e) {
					cmodel.remove(e.connectionID);
					Log("Client disconnected.");
				}
				
				@Override
				public void createDirectory(FtpserverCreateDirectoryEvent e) {
					// TODO Auto-generated method stub
					
				}
								
				@Override
				public void connect(FtpserverConnectEvent e) {
					cmodel.add(e.connectionID, e.remoteAddress + ":" + e.port);
					Log("Client connected from " + e.remoteAddress + ":" + e.port);
				}
				
				@Override
				public void commandReceived(FtpserverCommandReceivedEvent e) {
					cmodel.change(e.connectionID, "-", "-", e.command + ' ' + e.parameters, "");

					e.ignore = false;
				}
				
				@Override
				public void commandProcessed(FtpserverCommandProcessedEvent e) {
					cmodel.change(e.connectionID, "-", e.currentDirectory, e.command, "Finished : " + e.replyCode);
				}
				
				@Override
				public void changeDirectory(FtpserverChangeDirectoryEvent e) {
					// TODO Auto-generated method stub
					
				}
				
				@Override
				public void beforeUploadFile(FtpserverBeforeUploadFileEvent e) {
					// TODO Auto-generated method stub
					
				}
				
				@Override
				public void beforeSendReply(FtpserverBeforeSendReplyEvent e) {
					// TODO Auto-generated method stub
					
				}
				
				@Override
				public void beforeRequestAttributes(FtpserverBeforeRequestAttributesEvent e) {
					// TODO Auto-generated method stub
					
				}
				
				@Override
				public void beforeRenameFile(FtpserverBeforeRenameFileEvent e) {
					// TODO Auto-generated method stub
					
				}
				
				@Override
				public void beforeRemoveFile(FtpserverBeforeRemoveFileEvent e) {
					// TODO Auto-generated method stub
					
				}
				
				@Override
				public void beforeRemoveDirectory(FtpserverBeforeRemoveDirectoryEvent e) {
					// TODO Auto-generated method stub
					
				}
				
				@Override
				public void beforeFind(FtpserverBeforeFindEvent e) {
					// TODO Auto-generated method stub
					
				}
				
				@Override
				public void beforeDownloadFile(FtpserverBeforeDownloadFileEvent e) {
					// TODO Auto-generated method stub
					
				}
				
				@Override
				public void beforeCreateDirectory(FtpserverBeforeCreateDirectoryEvent e) {
					// TODO Auto-generated method stub
					
				}
				
				@Override
				public void beforeChangeDirectory(FtpserverBeforeChangeDirectoryEvent e) {
					// TODO Auto-generated method stub
					
				}
				
				@Override
				public void authAttempt(FtpserverAuthAttemptEvent e) {
					if (e.allow)
						Log("Access granted to user " + e.username, false);
					else
						Log("Access denied for user " + e.username, true);

					cmodel.change(e.connectionID, e.username, "-", "-", "-");
				}
				
				@Override
				public void afterRequestAttributes(FtpserverAfterRequestAttributesEvent e) {
					// TODO Auto-generated method stub
					
				}
				
				@Override
				public void afterRenameFile(FtpserverAfterRenameFileEvent e) {
					// TODO Auto-generated method stub
					
				}
				
				@Override
				public void afterRemoveFile(FtpserverAfterRemoveFileEvent e) {
					// TODO Auto-generated method stub
					
				}
				
				@Override
				public void afterRemoveDirectory(FtpserverAfterRemoveDirectoryEvent e) {
					// TODO Auto-generated method stub
					
				}
				
				@Override
				public void afterCreateDirectory(FtpserverAfterCreateDirectoryEvent e) {
					// TODO Auto-generated method stub
					
				}
				
				@Override
				public void afterChangeDirectory(FtpserverAfterChangeDirectoryEvent e) {
					// TODO Auto-generated method stub
					
				}
				
				@Override
				public void accept(FtpserverAcceptEvent e) {
					e.accept = true;
				}
			});
					
		}
		catch (Exception ex)
		{
			ex.printStackTrace();
		}
	}

	public static void Log(String msg, boolean isError) 
	{
		if (tableLog == null){
			showMessage(msg);
		} else {
			logtablemodel model = (logtablemodel)tableLog.getModel();
			model.addRow(new Date(), msg, isError ? "Eror" : "Info");
			scrollToEndTable(tableLog);
		}
	}

	protected void Log(String msg) {
		Log(msg, false);
	}

	private static void scrollToEndTable(JTable tbl) {
		tbl.scrollRectToVisible(tbl.getCellRect(tbl.getRowCount() - 1, tbl.getColumnCount(), true));		
	}
	
	public static void showMessage(String msg) {
		JOptionPane.showMessageDialog(null, msg, "FTPSServer demo", JOptionPane.INFORMATION_MESSAGE);
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



