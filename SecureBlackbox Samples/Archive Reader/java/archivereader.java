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
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import javax.swing.table.DefaultTableModel;
import java.io.File;

import secureblackbox.*;

public class archivereader {

	private JFrame frmArchiveReader;
	private JTable table;

	String currentPath;
	String extractToPath;
	Archivereader reader;

	JMenuItem mnCloseArchive;
	private JLabel lblCurrentPath;
	String openedArchiveName;

	progressdialog progressForm;
	
	/**
	 * Launch the application.
	 */
	public static void main(String[] args) {

		EventQueue.invokeLater(new Runnable() {
			public void run() {
				try {
					archivereader window = new archivereader();
					window.frmArchiveReader.setVisible(true);
				} catch (Exception e) {
					showErrorMessage("Error", e.getMessage());
				}
			}
		});
	}

	/**
	 * Create the application.
	 */
	public archivereader() {
		initialize();
		initArchiveReaderEvents();
	}

	/**
	 * Initialize the contents of the frame.
	 */
	private void initialize()
	{
		reader = new Archivereader();

		frmArchiveReader = new JFrame();
		frmArchiveReader.setTitle("Archive reader demo");
		frmArchiveReader.setBounds(100, 100, 450, 300);
		frmArchiveReader.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

		JLabel lblCaption = new JLabel("This sample shows how to work with existing archive.");
		lblCaption.setBounds(10, 5, 600, 14);
		lblCaption.setForeground(new Color(49, 106, 197));
		frmArchiveReader.getContentPane().add(lblCaption, BorderLayout.NORTH);
		
		JMenuBar menuBar = new JMenuBar();
		frmArchiveReader.setJMenuBar(menuBar);
		
		JMenu mnFile = new JMenu("File");
		menuBar.add(mnFile);
		
		JMenuItem mntmOpenArchive = new JMenuItem("Open archive...");
		mntmOpenArchive.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				openArchive();
			}
		});
		mnFile.add(mntmOpenArchive);

		mnCloseArchive = new JMenuItem("Close archive");
		mnCloseArchive.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				closeArchive();
			}
		});
		mnCloseArchive.setEnabled(false);
		mnFile.add(mnCloseArchive);

		JMenuItem mntmExit = new JMenuItem("Exit");
		mntmExit.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent arg0) {
				frmArchiveReader.dispose();
			}
		});
		mnFile.add(mntmExit);
		
		table = new JTable();
		JScrollPane scrollPane = new JScrollPane(table);
		frmArchiveReader.getContentPane().add(scrollPane, BorderLayout.CENTER);
		
		lblCurrentPath = new JLabel(" ");
		frmArchiveReader.getContentPane().add(lblCurrentPath, BorderLayout.SOUTH);
		
		initPopup();
		initDoubleClick();
	}

	private void initDoubleClick() {
		table.addMouseListener(new MouseAdapter() {
			public void mouseClicked(MouseEvent e){
			      if (e.getClickCount() == 2){
			    	  openItem();
			      }
			}
		});
	}

	private String pathCutLastComponent(String path)
	{
		int poz = path.lastIndexOf('/');

		if (poz < 0)
		{
			poz = path.lastIndexOf('\\');

			if (poz < 0)
				return "";
			else
			{
				if (poz == 2)
					return path.substring(0, poz + 1);
				else
					return path.substring(0, poz);
			}
		}
		else
			return path.substring(0, poz);
	}

	private void openItem() {
		if (table.getSelectedRowCount() > 0){
			ArchivedFile entry = ((filetablemodel)table.getModel()).getFirstSelectedItem(table.getSelectedRow());
			
			if (entry == null)
				currentPath = pathCutLastComponent(currentPath);
			else if (entry.getDirectory())
				currentPath = entry.getPath();
			else
				return;

			refreshFilesList();			
		}
	}
	
	private void initPopup() {
        JPopupMenu popup = new JPopupMenu();
		JMenuItem popupExtract = new JMenuItem("Extract");
		JMenuItem popupExtractTo = new JMenuItem("Extract to...");
		JMenuItem popupExtractAll = new JMenuItem("Extract All");
		JMenuItem popupExtractAllTo = new JMenuItem("Extract All to...");

		popupExtract.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				extractSelectedFile(pathCutLastComponent(openedArchiveName));
			}
		});

		popupExtractTo.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				String directory = selectDirectory();
				extractSelectedFile(directory);
			}
		});

		popupExtractAll.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				extractAllFiles(pathCutLastComponent(openedArchiveName));
			}
		});

		popupExtractAllTo.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				String directory = selectDirectory();
				extractAllFiles(directory);
			}
		});

        popup.add(popupExtract);
        popup.add(popupExtractTo);
		popup.add(popupExtractAll);
		popup.add(popupExtractAllTo);
        
        MouseListener listener=new TablePopupListener(popup, table);  
        table.addMouseListener(listener);  
	}

	private void initArchiveReaderEvents() {
		try {
			reader.addArchivereaderEventListener(new ArchivereaderEventListener() {
				@Override
				public void afterExtractFile(ArchivereaderAfterExtractFileEvent e) {
					progressForm.addToLog("File extracted : " + e.path);
				}

				@Override
				public void beforeExtractFile(ArchivereaderBeforeExtractFileEvent e) {
					progressForm.setCurrentFileName(e.path);
					progressForm.setProgressBarCurrentFile(0);
				}

				@Override
				public void decryptionPasswordNeeded(ArchivereaderDecryptionPasswordNeededEvent e) {
					JPasswordField jpf = new JPasswordField();
					int result = JOptionPane.showConfirmDialog(null, jpf, "Passphrase is needed for secret key", JOptionPane.OK_CANCEL_OPTION);
					try {
						if (result == 0) {
							reader.setDecryptionPassword(new String(jpf.getPassword()));
							e.cancel = false;
						} else {

							reader.setDecryptionPassword("");
							e.cancel = true;
						}
					}
					catch (Exception ex){}
				}

				@Override
				public void error(ArchivereaderErrorEvent e) {
					// TODO Auto-generated method stub
				}

				@Override
				public void notification(ArchivereaderNotificationEvent e) {
					// TODO Auto-generated method stub
				}

				@Override
				public void progress(ArchivereaderProgressEvent e) {
					long processedNorm;
					if (e.total > 0)
						processedNorm = ((e.processed * 100000) / e.total);
					else
						processedNorm = 100000;
					progressForm.setProgressBarCurrentFile((int) processedNorm);
					if (e.overallTotal > 0)
						processedNorm = (e.overallProcessed * 100000 / e.overallTotal);
					else
						processedNorm = 100000;
					progressForm.setProgressBarTotal((int) processedNorm);
					e.cancel = progressForm.getCancelOperation();
				}

				@Override
				public void recipientFound(ArchivereaderRecipientFoundEvent e) {
					certificatedialog certificateForm = new certificatedialog();
					certificateForm.setModal(true);
					certificateForm.setVisible(true);
					if (certificateForm.getCertificate() != null)
						reader.getDecryptionCertificates().add(certificateForm.getCertificate());
				}

				@Override
				public void signatureFound(ArchivereaderSignatureFoundEvent e) {
					if (!e.certFound)
					{
						signdialog frmSign = new signdialog(reader, e.issuerRDN, e.serialNumber);
						try
						{
							frmSign.setModal(true);
							frmSign.setVisible(true);

						}
						finally
						{
							frmSign.dispose();
						}
					}
				}

				@Override
				public void supercoreIntercept(ArchivereaderSupercoreInterceptEvent e) {

				}
			});
		}
		catch (Exception ex)
		{}
	}

	private void extractSelectedFile(String outputPath)
	{
		progressForm = new progressdialog();
		progressForm.setButtonOKEnabled(false);
		progressForm.setButtonCancelEnabled(true);
		progressForm.setCancelOperation(false);
		progressForm.clearLog();
		progressForm.setCaptionText("Extracting...");
		progressForm.setVisible(true);

		extractToPath = outputPath;

		java.lang.Thread t = new java.lang.Thread(new Runnable() {
			public void run() {
				try
				{
					ArchivedFile entry = ((filetablemodel)table.getModel()).getFirstSelectedItem(table.getSelectedRow());
					reader.extract(entry.getPath(), extractToPath, false);
				}
				catch (Exception ex)
				{
					progressForm.addToLog("Error extracting file: " + ex.getMessage());
				}
				finally
				{
					progressForm.setButtonCancelEnabled(false);
					progressForm.setButtonOKEnabled(true);
				}
			}
		});

		t.start();
	}

	private void extractAllFiles(String outputPath)
	{
		progressForm = new progressdialog();
		progressForm.setButtonOKEnabled(false);
		progressForm.setButtonCancelEnabled(true);
		progressForm.setCancelOperation(false);
		progressForm.clearLog();
		progressForm.setCaptionText("Extracting...");
		progressForm.setVisible(true);

		extractToPath = outputPath;

		java.lang.Thread t = new java.lang.Thread(new Runnable() {
			public void run() {
				try
				{
					reader.extractAll(extractToPath, true);
				}
				catch (Exception ex)
				{
					progressForm.addToLog("Error extracting file: " + ex.getMessage());
				}
				finally
				{
					progressForm.setButtonCancelEnabled(false);
					progressForm.setButtonOKEnabled(true);
				}
			}
		});

		t.start();
	}

	private String selectDirectory() {
		JFileChooser fc = new JFileChooser(); 
	    fc.setCurrentDirectory(new java.io.File("."));
	    fc.setDialogTitle("Select directory to extract files to");
	    fc.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
	    fc.setAcceptAllFileFilterUsed(false);

	    if (fc.showOpenDialog(frmArchiveReader) == JFileChooser.APPROVE_OPTION)
	    	return fc.getSelectedFile().getPath();
	    
	    return "";
	}

	private void openArchive()
	{
		closeArchive();

		openarchivedialog dlg = new openarchivedialog();
		dlg.setModal(true);
		dlg.setVisible(true);
		if (dlg.getIsOpenArch())
		{
			try
			{
				String fileName = dlg.getOpenArchiveFullPath();
				if (fileName.length() > 0)
				{
					openedArchiveName = fileName;

					reader.open(dlg.getArchiveType(), fileName);

					if (reader.isOpened())
					{
						currentPath = "";
						mnCloseArchive.setEnabled(true);

						refreshFilesList();
					}
					else
					{
						mnCloseArchive.setEnabled(false);
					}
				}
			}
			catch (Exception ex)
			{
				showErrorMessage("Error opening archive", ex.getMessage());
			}
		}
	}
	
	private void closeArchive() {
		if (reader.isOpened())
		{
			try
			{
				reader.close();
				clearTable();
				mnCloseArchive.setEnabled(false);
			}
			catch (Exception ex)
			{
				showErrorMessage("Error closing archive", ex.getMessage());
			}
		}
	}

	private void refreshFilesList()
	{
		if (!reader.isOpened())
			return;

		clearTable();

		table.setModel(new filetablemodel(reader, currentPath));

		lblCurrentPath.setText(currentPath);
	}

	private void clearTable() {
		Object model = table.getModel();
		if (model instanceof filetablemodel)
		{
			((filetablemodel) model).dataVector.removeAllElements();
			((filetablemodel) model).fireTableDataChanged();
		}
		else
			((DefaultTableModel) model).setNumRows(0);
	}

	static void showErrorMessage(String caption, String msg){
		JOptionPane.showMessageDialog(null, msg, caption, JOptionPane.ERROR_MESSAGE);
	}

	static void showMessage(String caption, String msg) {
		JOptionPane.showMessageDialog(null, msg, caption, JOptionPane.INFORMATION_MESSAGE);
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



