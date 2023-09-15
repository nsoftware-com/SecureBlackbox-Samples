import java.awt.BorderLayout;
import java.awt.FlowLayout;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JPanel;
import javax.swing.border.EmptyBorder;
import javax.swing.border.TitledBorder;
import javax.swing.JLabel;
import javax.swing.JTextField;
import javax.swing.JSpinner;
import javax.swing.SpinnerNumberModel;
import javax.swing.JPasswordField;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.DefaultComboBoxModel;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;

import secureblackbox.*;

public class conndialog extends JDialog {

	private static final long serialVersionUID = 1L;
	private final JPanel contentPanel = new JPanel();
	private JTextField tbUser;
	private JPasswordField tbPassword;
	protected boolean isOk;
	private JTextField tbHost;
	private JSpinner tbPort;
	private JComboBox<String> cmbSecurityMode;
	private JCheckBox chckbxPassiveFtpMode;
	private JButton okButton;
	private JButton cancelButton;

	/**
	 * Launch the application.
	 */
	public static void main(String[] args) {
		try {
			conndialog dialog = new conndialog();
			dialog.setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);
			dialog.setVisible(true);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	/**
	 * Create the dialog.
	 */
	public conndialog() {
		setTitle("Connection properties");
		setBounds(100, 100, 387, 265);
		getContentPane().setLayout(new BorderLayout());
		contentPanel.setBorder(new EmptyBorder(5, 5, 5, 5));
		getContentPane().add(contentPanel, BorderLayout.CENTER);
		contentPanel.setLayout(null);
		{
			JPanel panel = new JPanel();
			panel.setBorder(new TitledBorder(null, "Connection properties", TitledBorder.LEADING, TitledBorder.TOP, null, null));
			panel.setBounds(5, 11, 364, 180);
			contentPanel.add(panel);
			panel.setLayout(null);
			{
				JLabel lblHost = new JLabel("Host");
				lblHost.setBounds(10, 21, 46, 14);
				panel.add(lblHost);
			}
			{
				tbHost = new JTextField();
				tbHost.setText("127.0.0.1");
				tbHost.setBounds(10, 40, 255, 20);
				panel.add(tbHost);
				tbHost.setColumns(10);
			}

			tbPort = new JSpinner();
			tbPort.setModel(new SpinnerNumberModel(new Integer(21), null, null, new Integer(1)));
			tbPort.setBounds(275, 40, 74, 20);
			panel.add(tbPort);

			JLabel lblPort = new JLabel("Port");
			lblPort.setBounds(275, 21, 46, 14);
			panel.add(lblPort);

			JLabel lblUsername = new JLabel("Username");
			lblUsername.setBounds(10, 71, 84, 14);
			panel.add(lblUsername);

			tbUser = new JTextField();
			tbUser.setText("anonymous");
			tbUser.setBounds(8, 88, 158, 20);
			panel.add(tbUser);
			tbUser.setColumns(10);

			JLabel lblPassword = new JLabel("Password");
			lblPassword.setBounds(191, 71, 98, 14);
			panel.add(lblPassword);

			tbPassword = new JPasswordField();
			tbPassword.setBounds(191, 88, 158, 20);
			panel.add(tbPassword);


			JLabel lblAuthCommand = new JLabel("Security Mode");
			lblAuthCommand.setBounds(10, 125, 101, 14);
			panel.add(lblAuthCommand);

			cmbSecurityMode = new JComboBox<String>();
			cmbSecurityMode.setModel(new DefaultComboBoxModel<String>(new String[] {"NoTLS", "ImplicitTLS", "ExplicitTLS"}));
			cmbSecurityMode.setBounds(129, 121, 98, 22);
			panel.add(cmbSecurityMode);

			chckbxPassiveFtpMode = new JCheckBox("Passive FTP mode");
			chckbxPassiveFtpMode.setBounds(10, 150, 158, 23);
			panel.add(chckbxPassiveFtpMode);
		}
		{
			JPanel buttonPane = new JPanel();
			buttonPane.setLayout(new FlowLayout(FlowLayout.RIGHT));
			getContentPane().add(buttonPane, BorderLayout.SOUTH);
			{
				okButton = new JButton("OK");
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
				cancelButton = new JButton("Cancel");
				cancelButton.addActionListener(new ActionListener() {
					public void actionPerformed(ActionEvent e) {
						dispose();
					}
				});
				cancelButton.setActionCommand("Cancel");
				buttonPane.add(cancelButton);
			}
		}
	}

	public boolean isOK() {
		return isOk;
	}

	public String getHost() {
		return tbHost.getText();
	}

	public int getPort() {
		return (Integer)tbPort.getValue();
	}

	public String getUsername() {
		return tbUser.getText();
	}

	public String getPassword() {
		return new String(tbPassword.getPassword());
	}

	public short getSecurityMode() {
		if (cmbSecurityMode.getSelectedIndex() == -1)
			return TLSSettings.smNoTLS;

		return (short) cmbSecurityMode.getSelectedIndex();
	}

	public boolean getPassive() {
		return chckbxPassiveFtpMode.isSelected();
	}
}
