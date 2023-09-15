import java.awt.BorderLayout;
import java.awt.FlowLayout;

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;

import java.io.*;

import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;

import static secureblackbox.Archivewriter.*;


public class newarchivedialog extends JDialog implements ActionListener {
	private static final long serialVersionUID = 9082014456767725674L;
	private final JPanel contentPanel = new JPanel();
	private JTextField textField;

	private boolean isNeedCreate;

	JLabel labelArchiveType;
	JLabel labelCompressionLevel;
	JComboBox<String> cbArchiveType;
	JComboBox<String> cbCompressionLevel;

	JPanel panelSecurity;
	JLabel lEncryption;
	JComboBox<String> cbEncryptionType;
	JLabel lEncryptionAlgorithm;
	JComboBox<String> cbEncryptionAlgorithm;
	JLabel lPassword;
	JPasswordField tbPassword;
	JLabel lPasswordConfirmation;
	JPasswordField tbPasswordConfirmation;

	JButton btnOK;
	
	/**
	 * Launch the application.
	 */
	public static void main(String[] args) {
		try {
			newarchivedialog dialog = new newarchivedialog();
			dialog.setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);
			dialog.setVisible(true);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	/**
	 * Create the dialog.
	 */
	public newarchivedialog() {
		setTitle("Archive properties");
		setBounds(100, 100, 550, 250);
		getContentPane().setLayout(new BorderLayout());
		contentPanel.setBorder(new EmptyBorder(5, 5, 5, 5));
		getContentPane().add(contentPanel, BorderLayout.CENTER);
		contentPanel.setLayout(null);
		
		JLabel lblNewLabel = new JLabel("Archive File");
		lblNewLabel.setHorizontalAlignment(SwingConstants.RIGHT);
		lblNewLabel.setBounds(10, 11, 75, 14);
		contentPanel.add(lblNewLabel);
		
		textField = new JTextField("archive.zip");
		textField.setBounds(95, 8, 340, 20);
		contentPanel.add(textField);
		textField.setColumns(10);
		
		JButton btnChoose = new JButton("Choose...");
		btnChoose.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent arg0) {
				textField.setText(getFileName());
			}
		});
		btnChoose.setBounds(442, 7, 90, 23);
		contentPanel.add(btnChoose);

		JPanel panel = new JPanel();
		panel.setBounds(10, 36, 235, 100);
		panel.setBorder(BorderFactory.createTitledBorder("Compression"));
		contentPanel.add(panel);
		panel.setLayout(null);

		labelArchiveType = new JLabel("Archive type");
		labelArchiveType.setHorizontalAlignment(SwingConstants.RIGHT);
		labelArchiveType.setBounds(10, 45, 75, 14);
		panel.add(labelArchiveType);

		cbArchiveType = new JComboBox<String>();
		cbArchiveType.setBounds(95, 41, 125, 22);
		cbArchiveType.setModel(new DefaultComboBoxModel<String>(new String[] {"Zip", "Tar Gzip", "Tar Bzip2", "Gzip", "Bzip2"}));
		panel.add(cbArchiveType);

		labelCompressionLevel = new JLabel("Level");
		labelCompressionLevel.setHorizontalAlignment(SwingConstants.RIGHT);
		labelCompressionLevel.setBounds(10, 75, 75, 14);
		panel.add(labelCompressionLevel);

		cbCompressionLevel = new JComboBox<String>();
		cbCompressionLevel.setBounds(95, 71, 126, 22);
		cbCompressionLevel.setModel(new DefaultComboBoxModel<String>(new String[] {"Fastest", "Low", "Normal", "High"}));
		panel.add(cbCompressionLevel);

		panelSecurity = new JPanel();
		panelSecurity.setBounds(245, 36, 287, 140);
		panelSecurity.setBorder(BorderFactory.createTitledBorder("Security"));
		contentPanel.add(panelSecurity);
		panelSecurity.setLayout(null);

		lEncryption = new JLabel("Encryption");
		lEncryption.setBounds(14, 25, 81, 14);
		panelSecurity.add(lEncryption);

		cbEncryptionType = new JComboBox<String>();
		cbEncryptionType.setBounds(124, 21, 153, 22);
		cbEncryptionType.setModel(new DefaultComboBoxModel<String>(new String[] {"None", "Generic", "WinZip", "Strong"}));
		panelSecurity.add(cbEncryptionType);

		lEncryptionAlgorithm = new JLabel("Algorithm");
		lEncryptionAlgorithm.setBounds(14, 50, 81, 14);
		panelSecurity.add(lEncryptionAlgorithm);

		cbEncryptionAlgorithm = new JComboBox<String>();
		cbEncryptionAlgorithm.setBounds(124, 46, 153, 22);
		cbEncryptionAlgorithm.setModel(new DefaultComboBoxModel<String>(new String[] {"AES128", "AES192", "AES256", "Blowfish", "Twofish"}));
		panelSecurity.add(cbEncryptionAlgorithm);

		lPassword = new JLabel("Password");
		lPassword.setBounds(14, 83, 60, 14);
		panelSecurity.add(lPassword);

		tbPassword = new JPasswordField();
		tbPassword.setBounds(98, 80, 124, 20);
		panelSecurity.add(tbPassword);

		lPasswordConfirmation = new JLabel("Confirmation");
		lPasswordConfirmation.setBounds(14, 111, 80, 14);
		panelSecurity.add(lPasswordConfirmation);

		tbPasswordConfirmation = new JPasswordField();
		tbPasswordConfirmation.setBounds(98, 108, 124, 20);
		panelSecurity.add(tbPasswordConfirmation);
		{
			JPanel buttonPane = new JPanel();
			buttonPane.setLayout(new FlowLayout(FlowLayout.RIGHT));
			getContentPane().add(buttonPane, BorderLayout.SOUTH);
			{
				btnOK = new JButton("OK");
				btnOK.setEnabled(false);
				btnOK.addActionListener(new ActionListener() {
					public void actionPerformed(ActionEvent e) {
						isNeedCreate = true;
						setVisible(false);
					}
				});
				btnOK.setActionCommand("OK");
				buttonPane.add(btnOK);
				getRootPane().setDefaultButton(btnOK);
			}
			{
				JButton cancelButton = new JButton("Cancel");
				cancelButton.addActionListener(new ActionListener() {
					public void actionPerformed(ActionEvent e) {
						dispose();
					}
				});
				cancelButton.setActionCommand("Cancel");
				buttonPane.add(cancelButton);
			}
		}
		addFieldsListener(this);
		refreshControlsVisibility();
	}
	
	public void addFieldsListener(final ActionListener l)
	{
		cbEncryptionType.addActionListener(l);
		cbArchiveType.addActionListener(l);
		addPasswordDocumentLisner(tbPassword, l);
		addPasswordDocumentLisner(tbPasswordConfirmation, l);
		addTextDocumentLisner(textField, l);
	}		 
	
	public void addTextDocumentLisner(final JTextField txt, final ActionListener l) {
		txt.getDocument().addDocumentListener(new DocumentListener() {
			public void changedUpdate(DocumentEvent e) {
				l.actionPerformed(new ActionEvent(txt, 0, "changed"));
			}

			public void insertUpdate(DocumentEvent arg0) {
				l.actionPerformed(new ActionEvent(txt, 1, "insert"));
			}

			public void removeUpdate(DocumentEvent e) {			
				l.actionPerformed(new ActionEvent(txt, 2, "remove"));
			}
		});
	}

	public void addPasswordDocumentLisner(final JPasswordField txt, final ActionListener l) {
		txt.getDocument().addDocumentListener(new DocumentListener() {
			public void changedUpdate(DocumentEvent e) {
				l.actionPerformed(new ActionEvent(txt, 0, "changed"));
			}

			public void insertUpdate(DocumentEvent arg0) {
				l.actionPerformed(new ActionEvent(txt, 1, "insert"));
			}

			public void removeUpdate(DocumentEvent e) {
				l.actionPerformed(new ActionEvent(txt, 2, "remove"));
			}
		});
	}

	String getNewArchiveFullPath() {
		return textField.getText();
	}
	
	String getFileName(){
		JFileChooser fc = new JFileChooser();
		File f = new File("archive.zip");
		fc.setSelectedFile(f);

	    int returnVal = fc.showSaveDialog(this);
		if (returnVal == JFileChooser.APPROVE_OPTION)
	        return fc.getSelectedFile().getPath();

	    return "";
	}

	public boolean getIsCreateArch() {
		return isNeedCreate && textField.getText().length() > 0;
	}

	public int getArchiveType() {
		switch (cbArchiveType.getSelectedIndex())
		{
			case 1 : return aftTarGzip;
			case 2 : return aftTarBzip2;
			case 3 : return aftGzip;
			case 4 : return aftBzip2;
			default : return aftZip;
		}
	}

	public int getCompressionLevel() {
		switch (cbCompressionLevel.getSelectedIndex())
		{
			case 0 : return 1;
			case 1 : return 3;
			case 2 : return 6;
			default : return 9;
		}
	}

	public int getEncryptionType() {
		if (getArchiveType() == aftZip) {
			switch (cbEncryptionType.getSelectedIndex()) {
				case 1 : return aetGeneric;
				case 2 : return aetWinZip;
				case 3 : return aetStrong;
				default : return aetNoEncryption;
			}
		}
		else
			return aetNoEncryption;
	}

	public String getEncryptionAlg() {
		return cbEncryptionAlgorithm.getItemAt(cbEncryptionAlgorithm.getSelectedIndex());
	}

	public int getEncryptionKeyLength()
	{
		switch (cbEncryptionAlgorithm.getSelectedIndex())
		{
			case 0 : return 128;
			case 1 : return 192;
			default : return 256;
		}
	}

	public String getPassword() {
		return new String(tbPassword.getPassword());
	}

	public void actionPerformed(ActionEvent e) {
		refreshControlsVisibility();

		if (getNewArchiveFullPath().length() == 0)
		{
			btnOK.setEnabled(false);
			return;
		} else
		{
			btnOK.setEnabled(true);
		}

		if ((cbEncryptionType.getSelectedIndex() != 0) && getPassword().length() == 0) {
			btnOK.setEnabled(false);
			return;
		}

		if ((cbEncryptionType.getSelectedIndex() != 0))
			btnOK.setEnabled(getPassword().compareTo(new String (tbPasswordConfirmation.getPassword())) == 0);
	}

	private void refreshControlsVisibility()
	{
		if (cbArchiveType.getSelectedIndex() == 0)
		{
			panelSecurity.setEnabled(true);
			lEncryption.setEnabled(true);
			cbEncryptionType.setEnabled(true);
		}
		else
		{
			panelSecurity.setEnabled(false);
			lEncryption.setEnabled(false);
			cbEncryptionType.setEnabled(false);
			cbEncryptionType.setSelectedIndex(0);
		}

		if (cbEncryptionType.getSelectedIndex() == 0)
		{
			// no encryption
			lEncryptionAlgorithm.setVisible(false);
			cbEncryptionAlgorithm.setVisible(false);
			lPassword.setVisible(false);
			tbPassword.setVisible(false);
			lPasswordConfirmation.setVisible(false);
			tbPasswordConfirmation.setVisible(false);
		}
		else
		{
			lPassword.setVisible(true);
			tbPassword.setVisible(true);
			lPasswordConfirmation.setVisible(true);
			tbPasswordConfirmation.setVisible(true);

			if (cbEncryptionType.getSelectedIndex() == 1)
			{
				// generic encryption
				lEncryptionAlgorithm.setVisible(false);
				cbEncryptionAlgorithm.setVisible(false);
			}
			else if (cbEncryptionType.getSelectedIndex() == 2)
			{
				// WinZip encryption
				lEncryptionAlgorithm.setVisible(true);
				cbEncryptionAlgorithm.setVisible(true);
				cbEncryptionAlgorithm.removeAllItems();
				cbEncryptionAlgorithm.addItem("AES128");
				cbEncryptionAlgorithm.addItem("AES192");
				cbEncryptionAlgorithm.addItem("AES256");
				cbEncryptionAlgorithm.setSelectedIndex(0);
			}
			else if (cbEncryptionType.getSelectedIndex() == 3)
			{
				// Strong encryption
				lEncryptionAlgorithm.setVisible(true);
				cbEncryptionAlgorithm.setVisible(true);
				cbEncryptionAlgorithm.removeAllItems();
				cbEncryptionAlgorithm.addItem("AES128");
				cbEncryptionAlgorithm.addItem("AES192");
				cbEncryptionAlgorithm.addItem("AES256");
				cbEncryptionAlgorithm.addItem("Blowfish");
				cbEncryptionAlgorithm.addItem("Twofish");
				cbEncryptionAlgorithm.setSelectedIndex(0);
			}
		}
	}
}
