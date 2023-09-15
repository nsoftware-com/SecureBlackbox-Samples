import java.awt.BorderLayout;
import java.awt.FlowLayout;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JPanel;
import javax.swing.border.EmptyBorder;
import javax.swing.border.TitledBorder;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JTextField;
import javax.swing.JSpinner;
import javax.swing.JPasswordField;
import javax.swing.JCheckBox;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.SpinnerNumberModel;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;

import secureblackbox.*;

public class settingsdialog extends JDialog {

	private static final long serialVersionUID = 1L;
	private final JPanel contentPanel = new JPanel();
	private JTextField tbIP;
	private JTextField tbCert;
	private JPasswordField passwordField;
	private JTable table;
	private JTextField tbPassive;
	private JSpinner spPort;
	private JButton btnBrowse;
	private JCheckBox chckbxAllowAnonymousAccess;
	private JCheckBox chckbxImplicitSsl;
	private JCheckBox chckbxRequireTlsForControl;
	private JCheckBox chckbxRequireTlsForData;
	private usertablemodel umodel;
	private boolean isOk;
	private JTextField tbPortRangeFrom;
	private JTextField tbPortRangeTo;
	private Ftpserver _server;

	/**
	 * Launch the application.
	 */
	public static void main(String[] args) {
		try
		{
			settingsdialog dialog = new settingsdialog(null);
			dialog.setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);
			dialog.setVisible(true);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	/**
	 * Create the dialog.
	 */
	public settingsdialog(Ftpserver server) {
		_server = server;

		setTitle("Server settings");
		setBounds(100, 100, 569, 416);
		getContentPane().setLayout(new BorderLayout());
		contentPanel.setBorder(new EmptyBorder(5, 5, 5, 5));
		getContentPane().add(contentPanel, BorderLayout.CENTER);
		contentPanel.setLayout(null);
		
		JPanel panel = new JPanel();
		panel.setBorder(new TitledBorder(null, "Server settings", TitledBorder.LEADING, TitledBorder.TOP, null, null));
		panel.setBounds(0, 0, 550, 214);
		contentPanel.add(panel);
		panel.setLayout(null);
		
		JLabel lblIpAddress = new JLabel("IP Address");
		lblIpAddress.setBounds(10, 23, 65, 14);
		panel.add(lblIpAddress);
		
		tbIP = new JTextField();
		tbIP.setText("127.0.0.1");
		tbIP.setBounds(96, 20, 175, 20);
		panel.add(tbIP);
		tbIP.setColumns(10);
		
		spPort = new JSpinner();
		spPort.setModel(new SpinnerNumberModel(new Integer(22), null, null, new Integer(1)));
		spPort.setBounds(323, 20, 53, 20);
		panel.add(spPort);
		
		JLabel lblPort = new JLabel("Port");
		lblPort.setBounds(286, 23, 46, 14);
		panel.add(lblPort);
		
		JLabel lblCertificateWithPrivate = new JLabel("Certificate with private key file (required for TLS/SSL)");
		lblCertificateWithPrivate.setBounds(10, 54, 326, 14);
		panel.add(lblCertificateWithPrivate);
		
		tbCert = new JTextField();
		tbCert.setBounds(10, 79, 275, 20);
		panel.add(tbCert);
		tbCert.setColumns(10);
		
		btnBrowse = new JButton("Browse");
		btnBrowse.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				tbCert.setText(getCertificateFileName());
			}
		});
		btnBrowse.setBounds(296, 76, 80, 25);
		panel.add(btnBrowse);
		
		JLabel lblPassword = new JLabel("Password");
		lblPassword.setBounds(396, 54, 110, 14);
		panel.add(lblPassword);
		
		passwordField = new JPasswordField();
		passwordField.setBounds(395, 79, 119, 20);
		panel.add(passwordField);
		
		JLabel lblPassiveModeHost = new JLabel("Passive mode host (if NAT is used):");
		lblPassiveModeHost.setBounds(296, 110, 236, 14);
		panel.add(lblPassiveModeHost);
		
		chckbxAllowAnonymousAccess = new JCheckBox("Allow anonymous access");
		chckbxAllowAnonymousAccess.setBounds(6, 106, 206, 23);
		panel.add(chckbxAllowAnonymousAccess);
		
		chckbxImplicitSsl = new JCheckBox("Implicit SSL");
		chckbxImplicitSsl.setBounds(6, 132, 118, 23);
		panel.add(chckbxImplicitSsl);
		
		chckbxRequireTlsForControl = new JCheckBox("Require TLS for control channel");
		chckbxRequireTlsForControl.setBounds(6, 158, 226, 23);
		panel.add(chckbxRequireTlsForControl);
		
		chckbxRequireTlsForData = new JCheckBox("Require TLS for data channel");
		chckbxRequireTlsForData.setBounds(6, 184, 202, 23);
		panel.add(chckbxRequireTlsForData);
		
		tbPassive = new JTextField();
		tbPassive.setBounds(296, 133, 168, 20);
		panel.add(tbPassive);
		tbPassive.setColumns(10);
		
		JLabel lblPortRangeFor = new JLabel("Port range for incomming connections:");
		lblPortRangeFor.setBounds(296, 162, 236, 14);
		panel.add(lblPortRangeFor);
		
		tbPortRangeFrom = new JTextField();
		tbPortRangeFrom.setBounds(296, 184, 53, 20);
		panel.add(tbPortRangeFrom);
		tbPortRangeFrom.setColumns(10);
		
		tbPortRangeTo = new JTextField();
		tbPortRangeTo.setBounds(370, 184, 53, 20);
		panel.add(tbPortRangeTo);
		tbPortRangeTo.setColumns(10);
		
		JLabel label = new JLabel("-");
		label.setBounds(358, 188, 14, 14);
		panel.add(label);
		
		JPanel panel_1 = new JPanel();
		panel_1.setBorder(new TitledBorder(null, "Authorised users", TitledBorder.LEADING, TitledBorder.TOP, null, null));
		panel_1.setBounds(0, 219, 550, 122);
		contentPanel.add(panel_1);
		panel_1.setLayout(null);
		
		JScrollPane scrollPane = new JScrollPane();
		scrollPane.setBounds(10, 21, 440, 95);
		panel_1.add(scrollPane);
	
		umodel = new usertablemodel();
		table = new JTable(umodel);
		table.setFillsViewportHeight(true);
		scrollPane.setViewportView(table);
		
		JButton btnAdd = new JButton("Add");
		btnAdd.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				addClick();
			}
		});
		btnAdd.setBounds(460, 19, 80, 25);
		panel_1.add(btnAdd);
		
		JButton btnRemove = new JButton("Remove");
		btnRemove.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				removeClick();
			}
		});
		btnRemove.setBounds(460, 53, 80, 25);
		panel_1.add(btnRemove);
		
		JButton btnEdit = new JButton("Edit");
		btnEdit.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				editClick();
			}
		});
		btnEdit.setBounds(460, 87, 80, 25);
		panel_1.add(btnEdit);
		{
			JPanel buttonPane = new JPanel();
			buttonPane.setLayout(new FlowLayout(FlowLayout.RIGHT));
			getContentPane().add(buttonPane, BorderLayout.SOUTH);
			{
				JButton okButton = new JButton("OK");
				okButton.addActionListener(new ActionListener() {
					public void actionPerformed(ActionEvent e) {
						isOk = true;
						setVisible(false);
					}
				});
				okButton.setActionCommand("OK");
				buttonPane.add(okButton);
				getRootPane().setDefaultButton(okButton);
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
		
		fillUsers();
		init();
	}

	protected String getCertificateFileName() {
		JFileChooser fc = new JFileChooser();
		
		int returnVal = fc.showOpenDialog(this);

        if (returnVal == JFileChooser.APPROVE_OPTION)
            return fc.getSelectedFile().getAbsolutePath();
        
        return "";
	}

	private void init() {
		demosettings settings = demosettings.getSettings();
		settings.Load();
		
        tbIP.setText(settings.getListeningAddress());
        spPort.setValue(settings.getListeningPort());
        tbCert.setText(settings.getCertificateFile());
        passwordField.setText(settings.getCertificatePassword());
        chckbxImplicitSsl.setSelected(settings.getImplicitSSL());
        chckbxAllowAnonymousAccess.setSelected(settings.getAllowAnonymous());
        chckbxRequireTlsForControl.setSelected(settings.getRequireTLS());
        chckbxRequireTlsForData.setSelected(settings.getRequireTLSForData());
        tbPassive.setText(settings.getPassiveModeHost());
        tbPortRangeFrom.setText(settings.getPortRangeFrom() + "");
        tbPortRangeTo.setText(settings.getPortRangeTo() + "");        
    }

	protected void editClick() {
        userdialog.editUserParameters(_server.getUsers(), getUserName());
        fillUsers();
	}

	private String getUserName()
	{
		usertablemodel model = (usertablemodel)table.getModel();
		return model.dataVector.get(table.getSelectedRow()).Name;
	}
	
	protected void removeClick() {
		_server.getUsers().remove(table.getSelectedRow());
		fillUsers();		
	}

	protected void addClick() {
        userdialog.AddUserParameters(_server);
        fillUsers();
	}

	private void fillUsers()
	{
		tableUsersClear();
		for (int i = 0; i < _server.getUsers().size(); i++)
		{
			umodel.addRow(_server.getUsers().item(i));
		}
	}

	private void tableUsersClear() {
		umodel.clear();
	}

	public static boolean changeSettings(Ftpserver server)
	{
		settingsdialog frm = new settingsdialog(server);
		frm.setModal(true);
		frm.setVisible(true);
		if (frm.isOK())
		{
			try
			{
				demosettings s = demosettings.getSettings();
				s.setListeningAddress(frm.getIP());
                s.setListeningPort(frm.getPort());
                s.setAllowAnonymous(frm.getAllowAnonymous());
                s.setImplicitSSL(frm.getImplicitSSL());
                s.setRequireTLS(frm.getRequireTLS());
                s.setRequireTLSForData(frm.getRequireTLSForData());
                s.setCertificateFile(frm.getCertificateFile());
                s.setCertificatePassword(frm.getCertificatePassword());
                s.setPassiveModeHost(frm.getPassiveModeHost());
                s.setPortRangeFrom(frm.getPortRangeFrom());
                s.setPortRangeTo(frm.getPortRangeTo());
                s.Save();
                if (s.getCertificateFile().length() > 0)
                    s.loadCertificate();
			}
			catch(Exception exc)
			{
                ftpserver.Log("ChangeSettings : " + exc.getMessage(),true);
			}
		}
		return frm.isOK();
	}

	private String getPassiveModeHost() {
		return tbPassive.getText();
	}

	private String getCertificatePassword() {
		return new String(passwordField.getPassword());
	}

	private String getCertificateFile() {
		return tbCert.getText();
	}

	private boolean getRequireTLSForData() {
		return chckbxRequireTlsForData.isSelected();
	}

	private boolean getRequireTLS() {
		return chckbxRequireTlsForControl.isSelected();
	}

	private boolean getImplicitSSL() {
		return chckbxImplicitSsl.isSelected();
	}

	private boolean getAllowAnonymous() {
		return chckbxAllowAnonymousAccess.isSelected();
	}

	private int getPort() {
		return (Integer)spPort.getValue();
	}

	private int getPortRangeFrom() {
		return Integer.parseInt(tbPortRangeFrom.getText());
	}

	private int getPortRangeTo() {
		return Integer.parseInt(tbPortRangeFrom.getText());
	}
	
	private boolean isOK() {
		return isOk;
	}

	private String getIP() {
		return tbIP.getText();
	}
}
