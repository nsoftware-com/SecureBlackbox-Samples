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
import java.io.File;
import java.net.URL;
import java.util.Date;

import secureblackbox.*;

public class kmipserver extends JDialog {
	
	private static final long serialVersionUID = 1L;
	private final JPanel contentPanel = new JPanel();
	private JTable tbConnections;
	private JTable tbLog;
	private JButton btnExit;
	private JButton btnStart;
	private JButton btnStop;
	private JButton btnSettings;
	
	private Kmipserver Server;

	public static void main(String[] args)
	{
		kmipserver dialog = new kmipserver();
		dialog.setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);
		dialog.setVisible(true);
	}

	public kmipserver()
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

		JLabel lblCaption = new JLabel("This sample is a basic KMIP server. Tune up the server component as needed, and then press Start to activate the server. ");
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

		tbConnections = new JTable(new reqtablemodel());
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

	private void startServer()
	{
		try
		{
			Server.start();
		}
		catch (Exception e)
		{
			e.printStackTrace();
		}
	}

	private void Init()
	{
		try
		{
			Server = new Kmipserver();

			Server.addKmipserverEventListener(new KmipserverEventListener() {
				
				@Override
				public void supercoreIntercept(KmipserverSupercoreInterceptEvent e) {
					// TODO Auto-generated method stub
					
				}
				
				@Override
				public void request(KmipserverRequestEvent e) {
					reqtablemodel model = (reqtablemodel) tbConnections.getModel();
					model.addRow(e.operation, e.username);
				}
				
				@Override
				public void externalSign(KmipserverExternalSignEvent e) {
					// TODO Auto-generated method stub
					
				}
				
				@Override
				public void error(KmipserverErrorEvent e) {
					String S = "Error " + e.errorCode;
					if (e.description.length() > 0)
						S = S + ". Description: " + e.description;
					addLogEvent(S, true);
				}

				@Override
				public void notification(KmipserverNotificationEvent e) {
					// TODO Auto-generated method stub
				}

				@Override
				public void destroyAction(KmipserverDestroyActionEvent e) {
					// TODO Auto-generated method stub
					
				}
				
				@Override
				public void beforeGenerateKey(KmipserverBeforeGenerateKeyEvent e) {
					// TODO Auto-generated method stub
					
				}
				
				@Override
				public void beforeGenerateCert(KmipserverBeforeGenerateCertEvent e) {
					// TODO Auto-generated method stub
					
				}
				
				@Override
				public void authAttempt(KmipserverAuthAttemptEvent e) {
					if (Server.getUsers().size() == 0) {
						e.allow = true;
					}
				}
				
				@Override
				public void afterGenerateKey(KmipserverAfterGenerateKeyEvent e) {
					// TODO Auto-generated method stub
					
				}
				
				@Override
				public void afterGenerateCert(KmipserverAfterGenerateCertEvent e) {
					// TODO Auto-generated method stub
					
				}
			});

			Usermanager usermanager = new Usermanager();
			usermanager.load("Users.dat", "dsf%^dfg444");

			for (int i = 0; i < usermanager.getUsers().size(); i++)
				Server.getUsers().add(usermanager.getUsers().item(i));
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

		if (frm.isOK())
		{
			try {
				Server.setStorageFileName(frm.getStorageFile());

				Server.setEncoderType(frm.getEncoderType());

				Server.setPort(frm.getListenPort());

				if (frm.getUseSSL())
					Server.config("SSLMode=true");
				else
					Server.config("SSLMode=false");

				if (frm.getUseCompression())
					Server.config("UseCompression=true");
				else
					Server.config("UseCompression=false");

				if (frm.getUseChunking())
					Server.config("UseChunkedTransfer=true");
				else
					Server.config("UseChunkedTransfer=false");

				if (frm.getAuthBasic())
					Server.config("AuthBasic=true");
				else
					Server.config("AuthBasic=false");

				if (frm.getAuthDigest())
					Server.config("AuthDigest=true");
				else
					Server.config("AuthDigest=false");

				Server.getUsers().clear();
				for (int i = 0; i < frm.manager.getUsers().size(); i++) {
					Server.getUsers().add(frm.manager.getUsers().item(i));
				}

				if (!frm.getCertFile().isEmpty())
				{
					File f = new File(frm.getCertFile());

					if(f.exists())
					{
						try
						{
							Certificatemanager certmanager = new Certificatemanager();

							certmanager.importFromFile(frm.getCertFile(), frm.getCertPass());

							Server.setCACertificate(certmanager.getCertificate());
						}
						catch (Exception ex)
						{
							showErrorMessage("Cannot load certificate!");
						}
					}
				}
			}
			catch (Exception ex)
			{
				showErrorMessage(ex.getMessage());
			}
		}

		frm.dispose();
	}

	static void showErrorMessage(String msg){
		JOptionPane.showMessageDialog(null, msg, "Error", JOptionPane.ERROR_MESSAGE);
	}

	private Icon getIcon(String name) {
		URL resPath = getClass().getResource("/" + name + ".ico");
		if (resPath == null) return new ImageIcon();
		return new ImageIcon(resPath);
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



