import javax.swing.*;
import javax.swing.border.EmptyBorder;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;

import static secureblackbox.Archivewriter.*;


public class openarchivedialog extends JDialog implements ActionListener {
	private static final long serialVersionUID = 9082014456767725674L;
	private final JPanel contentPanel = new JPanel();
	private JTextField textField;

	private boolean isNeedOpen;

	JLabel labelArchiveType;
	JComboBox<String> comboBoxArchiveType;

	JButton btnOK;

	/**
	 * Launch the application.
	 */
	public static void main(String[] args) {
		try {
			openarchivedialog dialog = new openarchivedialog();
			dialog.setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);
			dialog.setVisible(true);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	/**
	 * Create the dialog.
	 */
	public openarchivedialog() {
		setTitle("Open archive");
		setBounds(100, 100, 550, 170);
		getContentPane().setLayout(new BorderLayout());
		contentPanel.setBorder(new EmptyBorder(5, 5, 5, 5));
		getContentPane().add(contentPanel, BorderLayout.CENTER);
		contentPanel.setLayout(null);
		
		JLabel lblNewLabel = new JLabel("Archive File");
		lblNewLabel.setHorizontalAlignment(SwingConstants.RIGHT);
		lblNewLabel.setBounds(10, 11, 75, 14);
		contentPanel.add(lblNewLabel);
		
		textField = new JTextField();
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

		labelArchiveType = new JLabel("Archive type");
		labelArchiveType.setHorizontalAlignment(SwingConstants.RIGHT);
		labelArchiveType.setBounds(10, 45, 75, 14);
		contentPanel.add(labelArchiveType);

		comboBoxArchiveType = new JComboBox<String>();
		comboBoxArchiveType.setBounds(95, 41, 125, 22);
		comboBoxArchiveType.setModel(new DefaultComboBoxModel<String>(new String[] {"Zip", "Tar Gzip", "Tar Bzip2", "Gzip", "Bzip2"}));
		contentPanel.add(comboBoxArchiveType);

		{
			JPanel buttonPane = new JPanel();
			buttonPane.setLayout(new FlowLayout(FlowLayout.RIGHT));
			getContentPane().add(buttonPane, BorderLayout.SOUTH);
			{
				btnOK = new JButton("OK");
				btnOK.setEnabled(false);
				btnOK.addActionListener(new ActionListener() {
					public void actionPerformed(ActionEvent e) {
						isNeedOpen = true;
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
	}
	
	public void addFieldsListener(final ActionListener l) {
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

	String getOpenArchiveFullPath() {
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

	public boolean getIsOpenArch() {
		return isNeedOpen && textField.getText().length() > 0;
	}

	public int getArchiveType() {
		switch (comboBoxArchiveType.getSelectedIndex())
		{
			case 1 : return aftTarGzip;
			case 2 : return aftTarBzip2;
			case 3 : return aftGzip;
			case 4 : return aftBzip2;
			default : return aftZip;
		}
	}

	public void actionPerformed(ActionEvent e) {
		if (getOpenArchiveFullPath().length() == 0)
		{
			btnOK.setEnabled(false);
			return;
		} else
		{
			btnOK.setEnabled(true);
		}
	}
}
