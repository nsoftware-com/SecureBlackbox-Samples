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
/*
 * SecureBlackbox 2022 Java Edition- Demo Application
 *
 * Copyright (c) 2022 /n software inc. - All rights reserved. - www.nsoftware.com
 *
 */

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

import static secureblackbox.Archivewriter.*;

public class archivewriter {

	private JFrame frmArchiveWriter;
	private JTable table;

	String currentPath;
	Archivewriter writer;

	JMenuItem mnCloseArchive;
	JMenuItem mntmSaveArchive;
	JMenuItem mntmSaveArchiveAs;
	private JLabel lblCurrentPath;
	String newArchiveName;
	String openedArchiveName;

	progressdialog progressForm;
	
	/**
	 * Launch the application.
	 */
	public static void main(String[] args) {

		EventQueue.invokeLater(new Runnable() {
			public void run() {
				try {
					archivewriter window = new archivewriter();
					window.frmArchiveWriter.setVisible(true);
				} catch (Exception e) {
					showErrorMessage("Error", e.getMessage());
				}
			}
		});
	}

	/**
	 * Create the application.
	 */
	public archivewriter() {
		initialize();
		initArchiveWriterEvents();
	}

	/**
	 * Initialize the contents of the frame.
	 */
	private void initialize()
	{
		writer = new Archivewriter();

		frmArchiveWriter = new JFrame();
		frmArchiveWriter.setTitle("Archive writer demo");
		frmArchiveWriter.setBounds(100, 100, 450, 300);
		frmArchiveWriter.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

		JLabel lblCaption = new JLabel("This sample shows how to create new and modify existing archive.");
		lblCaption.setBounds(10, 5, 600, 14);
		lblCaption.setForeground(new Color(49, 106, 197));
		frmArchiveWriter.getContentPane().add(lblCaption, BorderLayout.NORTH);
		
		JMenuBar menuBar = new JMenuBar();
		frmArchiveWriter.setJMenuBar(menuBar);
		
		JMenu mnFile = new JMenu("File");
		menuBar.add(mnFile);
		
		JMenuItem mntmOpenArchive = new JMenuItem("Open archive...");
		mntmOpenArchive.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				openArchive();
			}
		});
		mnFile.add(mntmOpenArchive);
		
		JMenuItem mntmNewArchive = new JMenuItem("New archive...");
		mntmNewArchive.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				newArchive();
			}
		});
		mnFile.add(mntmNewArchive);
		
		mnCloseArchive = new JMenuItem("Close archive");
		mnCloseArchive.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				closeArchive();
			}
		});
		mnCloseArchive.setEnabled(false);
		mnFile.add(mnCloseArchive);
		
		mntmSaveArchive = new JMenuItem("Save archive");
		mntmSaveArchive.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				saveArchive();
			}
		});
		mntmSaveArchive.setEnabled(false);
		mnFile.add(mntmSaveArchive);
		

		mntmSaveArchiveAs = new JMenuItem("Save archive as...");
		mntmSaveArchiveAs.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				saveArchiveAs();
			}
		});
		mntmSaveArchiveAs.setEnabled(false);
		mnFile.add(mntmSaveArchiveAs);
		
		JMenuItem mntmExit = new JMenuItem("Exit");
		mntmExit.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent arg0) {
				frmArchiveWriter.dispose();
			}
		});
		mnFile.add(mntmExit);
		
		table = new JTable();
		JScrollPane scrollPane = new JScrollPane(table);
		frmArchiveWriter.getContentPane().add(scrollPane, BorderLayout.CENTER);
		
		lblCurrentPath = new JLabel(" ");
		frmArchiveWriter.getContentPane().add(lblCurrentPath, BorderLayout.SOUTH);
		
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
        JMenuItem popupAddFiles = new JMenuItem("Add files...");
        JMenuItem popupAddDirectory = new JMenuItem("Add directory...");
        JMenuItem popupDelete = new JMenuItem("Delete");

        popupAddFiles.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				addFilesToArchive();
			}
		});
        
        popupAddDirectory.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				String directory = selectDirectory();

				try
				{
					writer.addFiles(currentPath, directory, true);

					refreshFilesList();
				}
				catch (Exception ex)
				{
					showErrorMessage("Error on added directory", ex.getMessage());
				}
			}
		});

        popupDelete.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e)
			{
				((filetablemodel)table.getModel()).removeSelectedItems(writer, table.getSelectedRows());
			}
		});

        popup.add(popupAddFiles);
        popup.add(popupAddDirectory);
        popup.add(popupDelete);
        
        MouseListener listener=new tablepopuplistener(popup, table);
        table.addMouseListener(listener);  
	}

	private void initArchiveWriterEvents() {
		try {
			writer.addArchivewriterEventListener(new ArchivewriterEventListener() {
				@Override
				public void afterCompressFile(ArchivewriterAfterCompressFileEvent e) {
					progressForm.addToLog("File compressed : " + e.path);
				}

				@Override
				public void beforeCompressFile(ArchivewriterBeforeCompressFileEvent e) {
					progressForm.setCurrentFileName(e.path);
					progressForm.setProgressBarCurrentFile(0);
				}

				@Override
				public void decryptionPasswordNeeded(ArchivewriterDecryptionPasswordNeededEvent e) {
					JPasswordField jpf = new JPasswordField();
					int result = JOptionPane.showConfirmDialog(null, jpf, "Passphrase is needed for secret key", JOptionPane.OK_CANCEL_OPTION);
					try
					{
					if (result == 0) {
						writer.setDecryptionPassword(new String(jpf.getPassword()));
						e.cancel = false;
					} else {

						writer.setDecryptionPassword("");
						e.cancel = true;
					}
					}
					catch (Exception ex){}
				}

				@Override
				public void error(ArchivewriterErrorEvent e) {
					// TODO Auto-generated method stub
				}

				@Override
				public void notification(ArchivewriterNotificationEvent e) {
					// TODO Auto-generated method stub
				}

				@Override
				public void prepareFile(ArchivewriterPrepareFileEvent e) {
					// TODO Auto-generated method stub
				}

				@Override
				public void progress(ArchivewriterProgressEvent e) {
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
				public void recipientFound(ArchivewriterRecipientFoundEvent e) {
					certificatedialog certificateForm = new certificatedialog();
					certificateForm.setModal(true);
					certificateForm.setVisible(true);
					if (certificateForm.getCertificate() != null)
						writer.getDecryptionCertificates().add(certificateForm.getCertificate());
				}

				@Override
				public void supercoreIntercept(ArchivewriterSupercoreInterceptEvent e) {

				}
			});
		}
		catch (Exception ex)
		{}
	}

	private void addFilesToArchive() {
		File[] files = getFileNames();

		for (int i = 0; i < files.length; i++)
		{
			try {
				writer.addFile(currentPath + "/" + files[i].getName(), files[i].getPath());
			}
			catch (Exception ex)
			{}
		}

		refreshFilesList();
	}
	
	private String selectDirectory() {
		JFileChooser fc = new JFileChooser(); 
	    fc.setCurrentDirectory(new java.io.File("."));
	    fc.setDialogTitle("Select directory to extract files to");
	    fc.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
	    fc.setAcceptAllFileFilterUsed(false);

	    if (fc.showOpenDialog(frmArchiveWriter) == JFileChooser.APPROVE_OPTION)
	    	return fc.getSelectedFile().getPath();
	    
	    return "";
	}

	private File[] getFileNames() {
		JFileChooser chooser = new JFileChooser();
		chooser.setDialogTitle("Add files to archive...");
		chooser.setMultiSelectionEnabled(true);

		chooser.showOpenDialog(frmArchiveWriter);

		return chooser.getSelectedFiles();			
	}

	String getFileSaveAsName(){
		JFileChooser fc = new JFileChooser();

	    int returnVal = fc.showSaveDialog(frmArchiveWriter);
		if (returnVal == JFileChooser.APPROVE_OPTION)
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

					writer.open(dlg.getArchiveType(), fileName);

					if (writer.isOpened())
					{
						currentPath = "";
						mnCloseArchive.setEnabled(true);
						mntmSaveArchive.setEnabled(true);
						mntmSaveArchiveAs.setEnabled(true);

						refreshFilesList();
					}
					else
					{
						mnCloseArchive.setEnabled(false);
						mntmSaveArchive.setEnabled(false);
						mntmSaveArchiveAs.setEnabled(false);
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
		if (writer.isOpened())
		{
			try
			{
				writer.close();
				clearTable();
				mnCloseArchive.setEnabled(false);
				mntmSaveArchive.setEnabled(false);
				mntmSaveArchiveAs.setEnabled(false);
			}
			catch (Exception ex)
			{
				showErrorMessage("Error closing archive", ex.getMessage());
			}
		}
	}

	private void createNewArchive(newarchivedialog src)
	{
		newArchiveName = src.getNewArchiveFullPath();

		try
		{
			writer.setCompressionLevel(src.getCompressionLevel());

			writer.setEncryptionType(src.getEncryptionType());

			if (src.getEncryptionType() == aetWinZip)
			{
				writer.config("EncryptionKeyLength="+src.getEncryptionKeyLength());
			}
			else if (src.getEncryptionType() == aetStrong)
			{
				writer.setEncryptionAlgorithm(src.getEncryptionAlg());
			}

			if (src.getEncryptionType() != aetNoEncryption) {
				writer.setEncryptionPassword(src.getPassword());
			}

			writer.createNew(src.getArchiveType());

			mnCloseArchive.setEnabled(true);
			mntmSaveArchive.setEnabled(true);
			mntmSaveArchiveAs.setEnabled(true);
			currentPath = "";

			addFilesToArchive();
		}
		catch (Exception ex)
		{
			showErrorMessage("Error on create new archive", ex.getMessage());
		}
	}

	private void newArchive()
	{
		closeArchive();

		newarchivedialog dlg = new newarchivedialog();
		dlg.setModal(true);
		dlg.setVisible(true);
		if (dlg.getIsCreateArch())
			createNewArchive(dlg);
	}

	private void saveArchiveAs() {
		if (writer.isNewArchive())
			newArchiveName = getFileSaveAsName();
		else
			openedArchiveName = getFileSaveAsName();

		saveArchive();
	}
	
	private void saveArchive() {
		
		progressForm = new progressdialog();

		progressForm.setCaptionText("Compressing...");
		progressForm.setButtonOKEnabled(false);
		progressForm.setButtonCancelEnabled(true);
		progressForm.setCancelOperation(false);
		
		progressForm.setVisible(true);
		
		java.lang.Thread t = new java.lang.Thread(new Runnable() {
			public void run() {
				try
				{
					if (writer.isNewArchive())
						writer.save(newArchiveName);
			        else
						writer.save(openedArchiveName);
				}
				catch (Exception ex)
				{
					showErrorMessage("Compression error", ex.getMessage());
				}
				finally
				{
					progressForm.setButtonCancelEnabled(false);
					progressForm.setButtonOKEnabled(true);
				}
				refreshFilesList();							
			}
		});
		
		t.start();
	}

	private void refreshFilesList()
	{
		if (!writer.isOpened())
			return;

		clearTable();

		table.setModel(new filetablemodel(writer, currentPath));

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



