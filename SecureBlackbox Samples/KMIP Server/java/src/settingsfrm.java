import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.*;
import javax.swing.border.EmptyBorder;
import javax.swing.border.EtchedBorder;
import javax.swing.border.TitledBorder;

import secureblackbox.*;

public class settingsfrm extends JDialog {

	private static final long serialVersionUID = 1L;
	private final JPanel contentPanel = new JPanel();
	private JTable table;

	private boolean isOk;
	private JTextField tbStorageFile;
	private JButton btnBrowsePath;
	private final ButtonGroup buttonGroup = new ButtonGroup();
	private JRadioButton rbTTLV;
	private JRadioButton rbXML;
	private JRadioButton rbJSON;
	private JSpinner sListenPort;
	private JCheckBox cbUseSSL;
	private JTextField tbCertFile;
	private JPasswordField tbCertPasw;
	private JCheckBox cbUseCompression;
	private JCheckBox cbUseChunking;
	private JCheckBox cbAuthBasic;
	private JCheckBox cbAuthDigest;

	public Usermanager manager;

	/**
	 * Create the dialog.
	 */
	public settingsfrm(Kmipserver serv) {
		setTitle("Server Settings");
		setBounds(100, 100, 550, 540);
		getContentPane().setLayout(new BorderLayout());
		contentPanel.setBorder(new EmptyBorder(5, 5, 5, 5));
		getContentPane().add(contentPanel, BorderLayout.CENTER);
		contentPanel.setLayout(null);

		JPanel paneMain = new JPanel();
		paneMain.setBorder(new TitledBorder(new EtchedBorder(EtchedBorder.LOWERED, null, null), "Server settings  ", TitledBorder.LEADING, TitledBorder.TOP, null, null));
		paneMain.setBounds(10, 10, 520, 440);
		contentPanel.add(paneMain);
		paneMain.setLayout(null);

		JLabel lblBasePath = new JLabel("Storage file");
		lblBasePath.setBounds(10, 28, 80, 14);
		paneMain.add(lblBasePath);

		tbStorageFile = new JTextField("default.db");
		tbStorageFile.setBounds(100, 25, 320, 20);
		paneMain.add(tbStorageFile);
		tbStorageFile.setColumns(10);

		btnBrowsePath = new JButton("Browse");
		btnBrowsePath.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				tbStorageFile.setText(getSaveFileName());
			}
		});
		btnBrowsePath.setBounds(430, 23, 80, 25);
		paneMain.add(btnBrowsePath);

		JPanel panel_4 = new JPanel();
		panel_4.setBorder(new TitledBorder(null, "Encoder type", TitledBorder.LEADING, TitledBorder.TOP, null, null));
		panel_4.setBounds(10, 55, 350, 45);
		paneMain.add(panel_4);
		panel_4.setLayout(null);

		rbTTLV = new JRadioButton("TTLV");
		buttonGroup.add(rbTTLV);
		rbTTLV.setHorizontalAlignment(SwingConstants.LEFT);
		rbTTLV.setBounds(10, 20, 60, 17);
		panel_4.add(rbTTLV);

		rbXML = new JRadioButton("XML");
		buttonGroup.add(rbXML);
		rbXML.setBounds(110, 20, 60, 17);
		panel_4.add(rbXML);

		rbJSON = new JRadioButton("JSON");
		buttonGroup.add(rbJSON);
		rbJSON.setBounds(210, 20, 60, 17);
		panel_4.add(rbJSON);

		JLabel lblListenOnPort = new JLabel("Listen on port");
		lblListenOnPort.setBounds(10, 118, 80, 14);
		paneMain.add(lblListenOnPort);

		sListenPort = new JSpinner();
		sListenPort.setModel(new SpinnerNumberModel(new Integer(5696), null, null, new Integer(1)));
		sListenPort.setBounds(100, 115, 70, 20);
		paneMain.add(sListenPort);

		cbUseSSL = new JCheckBox("Use SSL/TLS");
		cbUseSSL.setBounds(210, 113, 130, 23);
		paneMain.add(cbUseSSL);

		JPanel panelCertificate = new JPanel();
		panelCertificate.setBorder(new TitledBorder(new EtchedBorder(EtchedBorder.LOWERED, null, null), "CA Certificate ", TitledBorder.LEADING, TitledBorder.TOP, null, null));
		panelCertificate.setBounds(5, 150, 510, 90);
		paneMain.add(panelCertificate);
		panelCertificate.setLayout(null);

		JLabel lblCertificateFile = new JLabel("File name");
		lblCertificateFile.setBounds(10, 28, 60, 14);
		panelCertificate.add(lblCertificateFile);

		tbCertFile = new JTextField();
		tbCertFile.setColumns(10);
		tbCertFile.setBounds(80, 25, 335, 20);
		panelCertificate.add(tbCertFile);

		JButton buttonBrowseCert = new JButton("Browse");
		buttonBrowseCert.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				tbCertFile.setText(getFileName());
			}
		});
		buttonBrowseCert.setBounds(420, 23, 80, 25);
		panelCertificate.add(buttonBrowseCert);

		JLabel lblCertificatePassword = new JLabel("Certificate password");
		lblCertificatePassword.setBounds(10, 58, 120, 14);
		panelCertificate.add(lblCertificatePassword);

		tbCertPasw = new JPasswordField();
		tbCertPasw.setBounds(140, 55, 200, 20);
		panelCertificate.add(tbCertPasw);

		cbUseCompression = new JCheckBox("Use compression");
		cbUseCompression.setBounds(10, 250, 160, 23);
		paneMain.add(cbUseCompression);

		cbUseChunking = new JCheckBox("Use chunked encoding");
		cbUseChunking.setBounds(200, 250, 160, 23);
		paneMain.add(cbUseChunking);

		cbAuthBasic = new JCheckBox("Basic authentication");
		cbAuthBasic.setBounds(10, 280, 160, 23);
		paneMain.add(cbAuthBasic);

		cbAuthDigest = new JCheckBox("Digest authentication");
		cbAuthDigest.setBounds(200, 280, 160, 23);
		paneMain.add(cbAuthDigest);

		JPanel panel = new JPanel();
		panel.setBorder(new TitledBorder(new EtchedBorder(EtchedBorder.LOWERED, null, null), "Autorized users", TitledBorder.LEADING, TitledBorder.TOP, null, null));
		panel.setBounds(5, 310, 510, 125);
		paneMain.add(panel);
		panel.setLayout(null);

		JScrollPane scrollPane = new JScrollPane();
		scrollPane.setBounds(10, 20, 400, 97);
		panel.add(scrollPane);
		{
			table = new JTable(new usertablemodel());
			table.setFillsViewportHeight(true);
			scrollPane.setViewportView(table);
		}

		JButton btnAdd = new JButton("Add");
		btnAdd.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				addUserClick();
			}
		});
		btnAdd.setBounds(420, 20, 80, 25);
		panel.add(btnAdd);


		JButton btnRemove = new JButton("Remove");
		btnRemove.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				removeUserClick();
			}
		});
		btnRemove.setBounds(420, 50, 80, 25);
		panel.add(btnRemove);


		JButton btnOk = new JButton("Ok");
		btnOk.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				if (tbStorageFile.getText().isEmpty()) {
					showErrorMessage("Please, enter storage file");
					return;
				}

				try
				{
					manager.save("Users.dat", "dsf%^dfg444");
				}
				catch (Exception ex)
				{
					ex.printStackTrace();
				}

				isOk = true;
				setVisible(false);
			}
		});
		btnOk.setBounds(350, 465, 80, 25);
		contentPanel.add(btnOk);

		JButton btnCancel = new JButton("Cancel");
		btnCancel.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				isOk = false;
				setVisible(false);
			}
		});
		btnCancel.setBounds(445, 465, 80, 25);
		contentPanel.add(btnCancel);

		tbStorageFile.setText(serv.getStorageFileName());

		switch (serv.getEncoderType())
		{
			case Kmipserver.etXML: rbXML.setSelected(true); break;

			case Kmipserver.etJSON: rbJSON.setSelected(true); break;

			default: rbTTLV.setSelected(true); break;
		}

		sListenPort.setValue(serv.getPort());

		String confValue;
		try
		{
			confValue = serv.config("SSLMode");
		}
		catch (Exception ex)
		{
			confValue = "false";
		}
		if (confValue.equalsIgnoreCase("true"))
			cbUseSSL.setSelected(true);
		else
			cbUseSSL.setSelected(false);

		try
		{
			confValue = serv.config("UseCompression");
		}
		catch (Exception ex)
		{
			confValue = "false";
		}
		if (confValue.equalsIgnoreCase("true"))
			cbUseCompression.setSelected(true);
		else
			cbUseCompression.setSelected(false);

		try
		{
			confValue = serv.config("UseChunkedTransfer");
		}
		catch (Exception ex)
		{
			confValue = "false";
		}
		if (confValue.equalsIgnoreCase("true"))
			cbUseChunking.setSelected(true);
		else
			cbUseChunking.setSelected(false);

		try
		{
			confValue = serv.config("AuthBasic");
		}
		catch (Exception ex)
		{
			confValue = "false";
		}
		if (confValue.equalsIgnoreCase("true"))
			cbAuthBasic.setSelected(true);
		else
			cbAuthBasic.setSelected(false);

		try
		{
			confValue = serv.config("AuthDigest");
		}
		catch (Exception ex)
		{
			confValue = "false";
		}
		if (confValue.equalsIgnoreCase("true"))
			cbAuthDigest.setSelected(true);
		else
			cbAuthDigest.setSelected(false);

		manager = new Usermanager();
		for (int i = 0; i < serv.getUsers().size(); i++)
		{
			manager.getUsers().add(serv.getUsers().item(i));
		}

		FillUsers();
	}

	protected void addUserClick() {
		adduserdialog dlg = new adduserdialog();
		dlg.setModal(true);
		dlg.setVisible(true);
		if (dlg.IsOk)
			try
			{
				UserAccount user = new UserAccount();

				user.setUsername(dlg.edUserName.getText());
				user.setPassword(dlg.tbPass.getText());

				manager.getUsers().add(user);

				FillUsers();
			}
			catch (Exception e)
			{
				e.printStackTrace();
			}
	}

	protected void removeUserClick() {
		usertablemodel model = (usertablemodel)table.getModel();
		manager.getUsers().remove(table.getSelectedRow());

		FillUsers();
	}

	
	// sets up users list view according to user accounts settings
	private void FillUsers()
	{
		clearUserTable();

		for (int i = 0; i < manager.getUsers().size(); i++)
		{
			usertablemodel model = (usertablemodel) table.getModel();
			model.addRow(manager.getUsers().item(i));
		}
	}

	private void clearUserTable() {
		usertablemodel model = (usertablemodel) table.getModel();
		model.clear();
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

	static void showErrorMessage(String msg){
		JOptionPane.showMessageDialog(null, msg, "Error", JOptionPane.ERROR_MESSAGE);
	}

	public boolean isOK() {
		return isOk;
	}

	public int getListenPort() {
		return (Integer)sListenPort.getValue();
	}

	public boolean getUseSSL() {
		return cbUseSSL.isSelected();
	}

	public boolean getUseCompression() {
		return cbUseCompression.isSelected();
	}

	public boolean getUseChunking() {
		return cbUseChunking.isSelected();
	}

	public boolean getAuthBasic() {
		return cbAuthBasic.isSelected();
	}

	public boolean getAuthDigest() {
		return cbAuthDigest.isSelected();
	}

	public String getCertFile() {
		return tbCertFile.getText();
	}

	public String getCertPass() {
		return tbCertPasw.getText();
	}

	public String getStorageFile() {
		return tbStorageFile.getText();
	}

	public int getEncoderType()
	{
		if (rbJSON.isSelected())
		{
			return Kmipserver.etJSON;
		}
		else
		if (rbXML.isSelected())
		{
			return Kmipserver.etXML;
		}
		else
		{
			return Kmipserver.etTTLV;
		}
	}
}