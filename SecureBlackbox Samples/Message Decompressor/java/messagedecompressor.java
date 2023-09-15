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
import javax.swing.border.EtchedBorder;
import javax.swing.border.TitledBorder;
import javax.swing.table.DefaultTableModel;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;

import secureblackbox.*;

class messagedecompressor extends JDialog {
	private static final long serialVersionUID = -8938687539889635131L;
	private final JPanel contentPanel = new JPanel();
	private JTextField edInputFile;
	private JTextField edOutputFile;
	private JTable table;

	Messagedecompressor decompressor;

	/**
	 * Launch the application.
	 */
	public static void main(String[] args) {
		try {
			messagedecompressor dialog = new messagedecompressor();
			dialog.setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);
			dialog.setVisible(true);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	/**
	 * Create the dialog.
	 */
	public messagedecompressor()
	{
		decompressor = new Messagedecompressor();


		setTitle("Message Decompressor");

		setBounds(100, 100, 500, 200);
		getContentPane().setLayout(new BorderLayout());
		contentPanel.setBorder(new EmptyBorder(5, 5, 5, 5));
		getContentPane().add(contentPanel, BorderLayout.CENTER);
		contentPanel.setLayout(null);

		JLabel lblCaption = new JLabel("This sample performs decompress of compressed PKCS#7 messages.");
		lblCaption.setBounds(10, 5, 490, 14);
		lblCaption.setForeground(new Color(49, 106, 197));
		contentPanel.add(lblCaption);

		JLabel lblInputFile = new JLabel("Input File");
		lblInputFile.setBounds(10, 33, 70, 14);
		contentPanel.add(lblInputFile);

		edInputFile = new JTextField();
		edInputFile.setBounds(80, 30, 310, 20);
		contentPanel.add(edInputFile);
		edInputFile.setColumns(10);

		JButton sbBrowseInputFile = new JButton("Browse");
		sbBrowseInputFile.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				edInputFile.setText(getFileName());
			}
		});
		sbBrowseInputFile.setBounds(397, 28, 80, 25);
		contentPanel.add(sbBrowseInputFile);
		contentPanel.setLayout(null);

		JLabel lblOutputPath = new JLabel("Output File");
		lblOutputPath.setBounds(10, 63, 70, 14);
		contentPanel.add(lblOutputPath);

		edOutputFile = new JTextField();
		edOutputFile.setBounds(80, 60, 310, 20);
		contentPanel.add(edOutputFile);
		edOutputFile.setColumns(10);

		JButton sbBrowseOutputFile = new JButton("Browse");
		sbBrowseOutputFile.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				edOutputFile.setText(getSaveFileName());
			}
		});
		sbBrowseOutputFile.setBounds(397, 58, 80, 25);
		contentPanel.add(sbBrowseOutputFile);
		contentPanel.setLayout(null);


		JButton btnDecompress = new JButton("Decompress");
		btnDecompress.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				decrypt();
			}
		});
		btnDecompress.setBounds(367, 130, 110, 25);
		contentPanel.add(btnDecompress);
	}

	protected void decrypt() {
		try
		{
			decompressor.setInputFile(edInputFile.getText());
			decompressor.setOutputFile(edOutputFile.getText());

			decompressor.decompress();

			ShowResults();
		}
		catch (Exception ex) {
			showErrorMessage(ex.getMessage());
		}
	}

	private void ShowResults()
	{
		decompressresults verifierResults = new decompressresults();
		try
		{
			try
			{
			StringBuilder sb = new StringBuilder();
			sb.append("Successfully decompressed!\r\n");
			sb.append("Content Type: ");
			sb.append(decompressor.config("ContentType"));

			verifierResults.setResult(sb.toString());

			verifierResults.setLocationRelativeTo(this);

			verifierResults.setModal(true);
			verifierResults.setVisible(true);
			}
			catch (Exception ex) {
				showErrorMessage(ex.getMessage());
			}
		}
		finally
		{
			verifierResults.dispose();
		}
	}

	static void showErrorMessage(String msg){
		JOptionPane.showMessageDialog(null, msg, "Error", JOptionPane.ERROR_MESSAGE);
	}

	String getFileName(){
		JFileChooser fc = new JFileChooser();

		int returnVal = fc.showOpenDialog(this);
		if (returnVal == JFileChooser.APPROVE_OPTION)
			return fc.getSelectedFile().getPath();

		return "";
	}

	String getSaveFileName(){
		JFileChooser fc = new JFileChooser();

		int returnVal = fc.showSaveDialog(this);
		if (returnVal == JFileChooser.APPROVE_OPTION)
			return fc.getSelectedFile().getPath();

		return "";
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



