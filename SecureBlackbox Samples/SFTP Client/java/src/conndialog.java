import java.awt.BorderLayout;
import java.awt.FlowLayout;
import javax.swing.*;
import javax.swing.border.EmptyBorder;
import javax.swing.border.TitledBorder;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;

public class conndialog extends JDialog {

	private static final long serialVersionUID = 1L;
	private final JPanel contentPanel = new JPanel();
	private JTextField tbUser;
	private JPasswordField tbPassword;
	protected boolean isOk;
	private JTextField tbHost;
	private JSpinner tbPort;
	private JTextField tbPrivateKeyFile;
	private JPasswordField tbKeyPassword;
	private JButton btnBrowse;
	private JTextField tbTrustedKeysFile;
	private JButton btnBrowseKey;
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
		setBounds(100, 100, 387, 365);
		getContentPane().setLayout(new BorderLayout());
		contentPanel.setBorder(new EmptyBorder(5, 5, 5, 5));
		getContentPane().add(contentPanel, BorderLayout.CENTER);
		contentPanel.setLayout(null);
		{
			JPanel panel = new JPanel();
			panel.setBorder(new TitledBorder(null, "Connection properties", TitledBorder.LEADING, TitledBorder.TOP, null, null));
			panel.setBounds(10, 11, 359, 280);
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
			tbPort.setModel(new SpinnerNumberModel(new Integer(22), null, null, new Integer(1)));
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

			JLabel lblPrivateKeyFile = new JLabel("Private key file for PUBLICKEY authentication type");
			lblPrivateKeyFile.setBounds(10, 128, 284, 14);
			panel.add(lblPrivateKeyFile);

			tbPrivateKeyFile = new JTextField();
			tbPrivateKeyFile.setBounds(10, 148, 250, 20);
			panel.add(tbPrivateKeyFile);
			tbPrivateKeyFile.setColumns(10);

			btnBrowse = new JButton("Browse");
			btnBrowse.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					tbPrivateKeyFile.setText(getOpenFileName());
				}
			});
			btnBrowse.setBounds(270, 145, 80, 25);
			panel.add(btnBrowse);

			JLabel lblPrivateKeyPassword = new JLabel("Private key password");
			lblPrivateKeyPassword.setBounds(10, 174, 250, 14);
			panel.add(lblPrivateKeyPassword);

			tbKeyPassword = new JPasswordField();
			tbKeyPassword.setBounds(10, 192, 190, 20);
			panel.add(tbKeyPassword);

			JLabel lblTrustedKeysFile = new JLabel("Trusted keys file");
			lblTrustedKeysFile.setBounds(10, 228, 284, 14);
			panel.add(lblTrustedKeysFile);

			tbTrustedKeysFile = new JTextField();
			tbTrustedKeysFile.setBounds(10, 248, 250, 20);
			panel.add(tbTrustedKeysFile);
			tbTrustedKeysFile.setColumns(10);

			btnBrowseKey = new JButton("Browse");
			btnBrowseKey.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					tbTrustedKeysFile.setText(getOpenFileName());
				}
			});
			btnBrowseKey.setBounds(270, 245, 80, 25);
			panel.add(btnBrowseKey);
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

	String getOpenFileName(){
		JFileChooser fc = new JFileChooser();
		int returnVal = fc.showOpenDialog(this);
		if (returnVal == JFileChooser.APPROVE_OPTION)
			return fc.getSelectedFile().getPath();

		return "";
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

	public String getPrivateKey() {
		return tbPrivateKeyFile.getText();
	}

	public String getPrivateKeyPassword() {
		return new String(tbKeyPassword.getPassword());
	}

	public String getTrustedKeysFile() {
		return tbTrustedKeysFile.getText();
	}
}
