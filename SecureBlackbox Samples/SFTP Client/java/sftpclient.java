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
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JPanel;
import javax.swing.border.EmptyBorder;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JFileChooser;
import javax.swing.JMenuBar;
import javax.swing.JOptionPane;
import javax.swing.JTextField;
import javax.swing.JToolBar;
import javax.swing.JLabel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import java.io.File;
import java.net.URL;
import java.util.ArrayList;
import java.util.Date;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JSeparator;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.TooManyListenersException;

import secureblackbox.*;

public class sftpclient extends JDialog {
	private static final long serialVersionUID = 1L;
	private final JPanel contentPanel = new JPanel();
	private JTable tableFiles;
	private JMenuItem mntmExit;
	private Sftpclient client;
	private JTable logTable;
	private JLabel lPath;
	private progressdialog dlgProgress;
	ArrayList fileList = new ArrayList();
	java.lang.Thread t;
	/**
	 * Launch the application.
	 */
	public static void main(String[] args) {
		try {
			sftpclient dialog = new sftpclient();
			dialog.setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);
			dialog.setVisible(true);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	/**
	 * Create the dialog.
	 */
	public sftpclient() {
		addWindowListener(new WindowAdapter() {
			@Override
			public void windowClosing(WindowEvent e) {
				disconnect();
				System.exit(0);
			}
		});	
		setResizable(false);
		setTitle("SftpClient demo application");
		setBounds(100, 100, 746, 510);
		getContentPane().setLayout(new BorderLayout());
		contentPanel.setBorder(new EmptyBorder(5, 5, 5, 5));
		getContentPane().add(contentPanel, BorderLayout.CENTER);
		contentPanel.setLayout(null);

		JLabel lblCaption = new JLabel("This sample illustrates basic SFTP client operations.");
		lblCaption.setBounds(10, 5, 690, 14);
		lblCaption.setForeground(new Color(49, 106, 197));
		contentPanel.add(lblCaption);

		{
			JToolBar toolBar = new JToolBar();
			toolBar.setBounds(5, 27, 728, 18);
			toolBar.setFloatable(false);
			contentPanel.add(toolBar);
			{
				JButton btnConnect = new JButton("");
				btnConnect.setFocusPainted(false);
				btnConnect.addMouseListener(new MouseAdapter() {
					@Override
					public void mousePressed(MouseEvent e) {
						t = new java.lang.Thread(new Runnable() {
							public void run() {
								connect();
							}
						});
						
						t.start();
					}
				});
				btnConnect.setBorderPainted(false);
				btnConnect.setToolTipText("Connect");
				btnConnect.setIcon(getIcon("connect"));
				toolBar.add(btnConnect);
			}
			{
				JButton btnDisconnect = new JButton("");
				btnDisconnect.setFocusPainted(false);
				btnDisconnect.addMouseListener(new MouseAdapter() {
					@Override
					public void mousePressed(MouseEvent e) {
						t = new java.lang.Thread(new Runnable() {
							public void run() {
								disconnect();								
							}
						});
						
						t.start();
					}
				});				
				btnDisconnect.setBorderPainted(false);
				btnDisconnect.setToolTipText("Disconnect");
				btnDisconnect.setIcon(getIcon("disconnect"));
				toolBar.add(btnDisconnect);
			}
			{
				JButton btnMakedir = new JButton("");
				btnMakedir.setFocusPainted(false);
				btnMakedir.addMouseListener(new MouseAdapter() {
					@Override
					public void mousePressed(MouseEvent e) {
						t = new java.lang.Thread(new Runnable() {
							public void run() {
								makeDir();								
							}
						});
						
						t.start();
					}
				});				
				btnMakedir.setBorderPainted(false);
				btnMakedir.setToolTipText("Create directory");
				btnMakedir.setIcon(getIcon("makedir"));
				toolBar.add(btnMakedir);
			}
			{
				JButton btnDelete = new JButton("");
				btnDelete.setFocusPainted(false);
				btnDelete.addMouseListener(new MouseAdapter() {
					@Override
					public void mousePressed(MouseEvent e) {
						t = new java.lang.Thread(new Runnable() {
							public void run() {
								delete();								
							}
						});
						
						t.start();
					}
				});								
				btnDelete.setBorderPainted(false);
				btnDelete.setToolTipText("Delete");
				btnDelete.setIcon(getIcon("delete"));
				toolBar.add(btnDelete);
			}
			{
				JButton btnDownload = new JButton("");
				btnDownload.setFocusPainted(false);
				btnDownload.addMouseListener(new MouseAdapter() {
					@Override
					public void mousePressed(MouseEvent e) {
						t = new java.lang.Thread(new Runnable() {
							
							public void run() {						
								download();
							}
						});
						
						t.start();
					}
				});								
				btnDownload.setBorderPainted(false);
				btnDownload.setToolTipText("Download");
				btnDownload.setIcon(getIcon("download"));
				toolBar.add(btnDownload);
			}
			{
				JButton btnUpload = new JButton("");
				btnUpload.setFocusPainted(false);
				btnUpload.addMouseListener(new MouseAdapter() {
					@Override
					public void mousePressed(MouseEvent e) {
						t = new java.lang.Thread(new Runnable() {
							public void run() {
								upload();								
							}
						});
						
						t.start();
					}
				});								
				btnUpload.setBorderPainted(false);
				btnUpload.setToolTipText("Upload");
				btnUpload.setIcon(getIcon("upload"));
				toolBar.add(btnUpload);
			}
			{
				JButton btnRefresh = new JButton("");
				btnRefresh.setFocusPainted(false);
				btnRefresh.addMouseListener(new MouseAdapter() {
					@Override
					public void mousePressed(MouseEvent e) {
						refreshData();
					}
				});								
				btnRefresh.setBorderPainted(false);
				btnRefresh.setToolTipText("Refresh");
				btnRefresh.setIcon(getIcon("refresh"));
				toolBar.add(btnRefresh);
			}
		}
		{
			JScrollPane scrollPane = new JScrollPane();
			scrollPane.setBounds(5, 50, 728, 294);
			contentPanel.add(scrollPane);
			{
				tableFiles = new JTable(new filestablemodel());
				tableFiles.addMouseListener(new MouseAdapter(){
					public void mouseClicked(MouseEvent e){
						if (e.getClickCount() == 2){
							changeDir();
						}
					}
				} );
				tableFiles.addKeyListener(new KeyAdapter() {
					@Override
					public void keyPressed(KeyEvent e) {
						if (e.getKeyCode() == KeyEvent.VK_ENTER)
							changeDir();
					}
				});
				tableFiles.setFillsViewportHeight(true);
				scrollPane.setViewportView(tableFiles);
			}
		}
		{
			scrollLog = new JScrollPane();
			scrollLog.setBounds(5, 370, 728, 107);
			contentPanel.add(scrollLog);
			{
				logTable = new JTable(new logtablemodel());
				logTable.setFillsViewportHeight(true);
				tableFiles.setFillsViewportHeight(true);
				scrollLog.setViewportView(logTable);
			}
		}		
		
		lPath = new JLabel("Ready");
		lPath.setBounds(6, 351, 723, 14);
		contentPanel.add(lPath);

		init();
	}

	private Icon getIcon(String name) {
		URL resPath = getClass().getResource("/" + name + ".ico");
		if (resPath == null) return new ImageIcon();
		return new ImageIcon(resPath);
	}

	private void init()
	{
		client = new Sftpclient();

		initEvents();
	}
	
	private void connect()
	{
		conndialog dlg = new conndialog();
		if (client.isConnected())
		{
			showMessage("SFTP Connection Status", "Already connected");
			return;
		}
		dlg.setModal(true);
		dlg.setVisible(true);
		
		if (dlg.isOK()) 
		{
			try
			{
				client.setUsername(dlg.getUsername());
				client.setPassword(dlg.getPassword());

				if (!dlg.getPrivateKey().isEmpty())
				{
					try
					{
						Sshkeymanager keymanager = new Sshkeymanager();

						keymanager.importFromFile(dlg.getPrivateKey(), dlg.getPrivateKeyPassword());

						client.setKey(keymanager.getKey());
					}
					catch (Exception E)
					{
						showMessage(E.getMessage(), "Error");
					}
				}
				else
				{
					client.setKey(null);
				}

				client.setTrustedKeysFile(dlg.getTrustedKeysFile());

				client.connect(dlg.getHost(), dlg.getPort());

				log("SFTP connection established", false);
				loadObjects("/");
			}
			catch (Exception E)
			{
				showMessage(E.getMessage(), "Error");
			}
		};
	}

	static void showMessage(String caption, String msg) {
		JOptionPane.showMessageDialog(null, msg, caption, JOptionPane.INFORMATION_MESSAGE);
	}	

	private void disconnect()
	{
		log("Disconnecting", false);
		if (client.isConnected())
		{
			try
			{
				client.disconnect();
				clearFileList();
			}
			catch (Exception E)
			{
				showMessage(E.getMessage(), "Error");
			}
		}
	}

	private void loadObjects(String path)
	{
		if (!client.isConnected())
		{
			return;
		}

		try
		{
			clearFileList();

			client.changeDir(path);
			lPath.setText(path);

			log("Retrieving file list", false);

			fileList.clear();
			filestablemodel model = (filestablemodel) tableFiles.getModel();

			client.listDir(true, true);

			fileinfocomparer comparer = new fileinfocomparer();
			fileList.sort(comparer);
			for(int i = 0; i < fileList.size(); i++)
			{
				model.addItem((SFTPListEntry)fileList.get(i));
			}
		}
		catch (Exception e) {
			log("Failed to retrieve file list" + e.getMessage(), true);
		}
	}

	private void refreshData()
	{
		try
		{
			loadObjects(client.getCurrentDir());
		}
		catch (Exception e) {}
	}

	private void makeDir()
	{
		if (client.isConnected())
		{
			String DirName = inputDialog("Please specify the name for new directory");
			if (DirName.length() > 0) 
			{
				log("Creating directory " + DirName, false);
				try 
				{
					client.makeDir(DirName);

					refreshData();
				} 
				catch(Exception ex) 
				{
					log("Failed to create directory '" + DirName + "', " + ex.getMessage(), true);
				}
			}
		}
	}
	
	private String inputDialog(String msg) {
		JTextField jpf = new JTextField();
		JOptionPane.showConfirmDialog(this, jpf, msg, JOptionPane.OK_CANCEL_OPTION);
		return jpf.getText();
	}

	private void delete()
	{
		SFTPListEntry info;
		if (client.isConnected())
		{
			if (tableFiles.getSelectedRowCount() > 0)
			{
				filestablemodel model = (filestablemodel) tableFiles.getModel();
				info = model.getSelectedRowTag(tableFiles.getSelectedRow());

				if (info == null) {
					return;
				}

				if (showConfirmMessage("Please confirm that you want to delete " + 
					info.getName(), "Delete item")) 
				{
					log("Removing item " + info.getName(), false);
					try 
					{
						if (info.getFileType() == SFTPListEntry.cftDirectory)
						{
							client.deleteDir(info.getName());
						} 
						else 
						{
							client.deleteFile(info.getName());
						}
					} 
					catch(Exception ex) 
					{
						log("Failed to delete " + info.getName() + ", " + ex.getMessage(), true);
					}
					refreshData();
				}
			}
		}
	}

	private boolean showConfirmMessage(String msg, String cap) {
		return (JOptionPane.showConfirmDialog(this, msg, cap, JOptionPane.YES_NO_OPTION) == JOptionPane.YES_OPTION);
	}

	private void download()
	{
		SFTPListEntry info;
		dlgProgress = new progressdialog();
		
		if ((client.isConnected()) && (tableFiles.getSelectedRowCount() > 0))
		{
			filestablemodel model = (filestablemodel) tableFiles.getModel();
			info = model.getSelectedRowTag(tableFiles.getSelectedRow());

			String fileName = getSaveFileName();
			if (fileName.compareTo("") != 0) 
			{
				dlgProgress.setCaptionText("Download");
				dlgProgress.setSourceFilename(info.getName());
				dlgProgress.setDestFilename(fileName);
				dlgProgress.setProgressText("0 / " + info.getSize());
				dlgProgress.setProgressValue(0);
				dlgProgress.setVisible(true);
				try 
				{
					client.downloadFile(info.getName(), fileName);
				} 
				catch(Exception ex) 
				{
					log("Error during download: " + ex.getMessage(), true);
				}
				dlgProgress.setVisible(false);
				dlgProgress.dispose();
				dlgProgress = null;
				log("Download finished", false);
			}				
		}
	}

	String getSaveFileName(){
		JFileChooser fc = new JFileChooser();
	    int returnVal = fc.showSaveDialog(this);
		if (returnVal == JFileChooser.APPROVE_OPTION)
	        return fc.getSelectedFile().getPath();

	    return "";
	} 
	
	String getOpenFileName(){
		JFileChooser fc = new JFileChooser();
	    int returnVal = fc.showOpenDialog(this);
		if (returnVal == JFileChooser.APPROVE_OPTION)
	        return fc.getSelectedFile().getPath();

	    return "";
	} 
	
	private void upload()
	{
		long size;
		String shortName;
		String fileName = getOpenFileName();
		dlgProgress = new progressdialog();

		if (client.isConnected())
		{
			if (fileName.compareTo("") != 0) 
			{
				log("Uploading file " + fileName, false);
				File file = new File(fileName);
				shortName = file.getName();
				dlgProgress.setCaptionText("Upload");
				dlgProgress.setSourceFilename(fileName);
				dlgProgress.setDestFilename(shortName);
				size = file.length();
				dlgProgress.setProgressText("0 / " + size);
				dlgProgress.setProgressValue(0);
				dlgProgress.setVisible(true);
				try 
				{
					client.uploadFile(fileName, shortName);
				}
				catch (Exception ex)
				{
					ex.printStackTrace();
					log("Error during upload: " + ex.getMessage(), true);
				}

				dlgProgress.setVisible(false);
				dlgProgress.dispose();
				log("Upload finished", false);
				refreshData();
			}
		}
	}

	private void changeDir()
	{
		SFTPListEntry info;
		if ((client.isConnected()) && (tableFiles.getSelectedRowCount() > 0))
		{
			filestablemodel model = (filestablemodel) tableFiles.getModel();
			info = model.getSelectedRowTag(tableFiles.getSelectedRow());
			if (info.getFileType() == SFTPListEntry.cftDirectory)
			{
				log("Changing directory to " + info.getName(), false);
				try
				{
					client.changeDir(info.getName());
					refreshData();
				}
				catch(Exception ex)
				{
					log("Unable to change directory: " + ex.getMessage(), true);
					return;
				}
			}
		}
	}

	private void log(String msg, boolean isError) 
	{
		logtablemodel model = (logtablemodel)logTable.getModel();
		model.addRow(new Date(), msg, isError ? "Eror" : "Info");
		scrollToEndTable(logTable);
	}

	private void scrollToEndTable(JTable tbl) {
		tbl.scrollRectToVisible(tbl.getCellRect(tbl.getRowCount() - 1, tbl.getColumnCount(), true));		
	}

	private void clearFileList()
	{
		((filestablemodel)tableFiles.getModel()).clear();
	}


	private String OperationName(int operation)
	{
		switch (operation)
		{
			case 0://csfoDownloadFile
				return "Download file";
			case 1://csfoUploadFile
				return "Upload file";
			case 2://csfoDeleteFile
				return "Delete file";
			case 3://csfoMakeDir
				return "Make directory";
			default:
				return "Unknown";
		}
	}

	private void initEvents() {
		try
		{
			client.addSftpclientEventListener(new SftpclientEventListener() {
				
				@Override
				public void unknownKeyReceived(SftpclientUnknownKeyReceivedEvent e) {
					e.action = 2;//catAcceptPermanently;
				}
				
				@Override
				public void supercoreIntercept(SftpclientSupercoreInterceptEvent e) {
					// TODO Auto-generated method stub
				}
				
				@Override
				public void progress(SftpclientProgressEvent e) {
					dlgProgress.setProgressValue((int) (e.current * 100 / e.total));
					dlgProgress.setProgressText(e.current + " / " + e.total);
					e.cancel = false;
				}
				
				@Override
				public void privateKeyNeeded(SftpclientPrivateKeyNeededEvent e) {
					// TODO Auto-generated method stub
				}
				
				@Override
				public void passwordChangeRequest(SftpclientPasswordChangeRequestEvent e) {
					// TODO Auto-generated method stub
				}
				
				@Override
				public void listEntry(SftpclientListEntryEvent e) {
					fileList.add(client.getCurrentListEntry());
				}

				@Override
				public void notification(SftpclientNotificationEvent e) {
					// TODO Auto-generated method stub
				}
				
				@Override
				public void knownKeyReceived(SftpclientKnownKeyReceivedEvent e) {
					log("KnownKey Received", false);
				}
				
				@Override
				public void fileOperationResult(SftpclientFileOperationResultEvent e) {
					String S = "";
					if (e.errorCode != 0)
						S = S + "Error " + e.errorCode + ".";
					S = S + "Result of" + OperationName(e.operation) + ".";
					if (e.localPath.length() != 0)
						S = S + " Local path: " + e.localPath;
					if (e.remotePath.length() != 0)
						S = S + " Remote path: " + e.remotePath;
					if (e.comment.length() != 0)
						S = S + " Comment: " + e.comment;

					log(S, false);
				}
				
				@Override
				public void fileOperation(SftpclientFileOperationEvent e) {
					String S = OperationName(e.operation) + ".";
					if (e.localPath.length() != 0)
						S = S + " Local path: " + e.localPath;
					if (e.remotePath.length() != 0)
						S = S + " Remote path: " + e.remotePath;

					log(S, false);
				}
				
				@Override
				public void externalSign(SftpclientExternalSignEvent e) {
					// TODO Auto-generated method stub
					
				}
				
				@Override
				public void error(SftpclientErrorEvent e) {
					String S = "Error " + e.errorCode;
					if (e.description.length() > 0)
						S = S + ". Description: " + e.description;
					log(S, true);
				}
				
				@Override
				public void disconnect(SftpclientDisconnectEvent e) {
					// TODO Auto-generated method stub
					
				}
				
				@Override
				public void banner(SftpclientBannerEvent e) {
					// TODO Auto-generated method stub
					
				}
				
				@Override
				public void authSucceeded(SftpclientAuthSucceededEvent e) {
					// TODO Auto-generated method stub
					
				}
				
				@Override
				public void authFailed(SftpclientAuthFailedEvent e) {
					// TODO Auto-generated method stub
					
				}
				
				@Override
				public void authAttempt(SftpclientAuthAttemptEvent e) {
					// TODO Auto-generated method stub
					
				}
			});
		}
		catch (TooManyListenersException ex)
		{

		}
	}

	private JScrollPane scrollLog;
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



