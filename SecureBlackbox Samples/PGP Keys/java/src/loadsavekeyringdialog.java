import java.awt.BorderLayout;
import java.awt.FlowLayout;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JFileChooser;
import javax.swing.JPanel;
import javax.swing.border.EmptyBorder;
import javax.swing.JLabel;
import javax.swing.JTextField;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;

public class loadsavekeyringdialog extends JDialog {

	private static final long serialVersionUID = 1L;
	private final JPanel contentPanel = new JPanel();
	private JTextField tbPub;
	private JTextField tbSec;
	private boolean isOk;
	private boolean isOpen;

	/**
	 * Launch the application.
	 */
	public static void main(String[] args) {
		try {
			loadsavekeyringdialog dialog = new loadsavekeyringdialog();
			dialog.setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);
			dialog.setVisible(true);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	/**
	 * Create the dialog.
	 */
	public loadsavekeyringdialog() {
		setResizable(false);
		setTitle("Load keyring");
		setBounds(100, 100, 465, 190);
		getContentPane().setLayout(new BorderLayout());
		contentPanel.setBorder(new EmptyBorder(5, 5, 5, 5));
		getContentPane().add(contentPanel, BorderLayout.CENTER);
		contentPanel.setLayout(null);
		{
			JLabel lblPublicKeyring = new JLabel("Public keyring");
			lblPublicKeyring.setBounds(10, 11, 341, 14);
			contentPanel.add(lblPublicKeyring);
		}
		
		tbPub = new JTextField();
		tbPub.setBounds(10, 30, 355, 20);
		contentPanel.add(tbPub);
		tbPub.setColumns(10);
		
		JButton btnBrowsePub = new JButton("Browse");
		btnBrowsePub.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				pubBrowseClick();
			}
		});
		btnBrowsePub.setBounds(375, 28, 80, 25);
		contentPanel.add(btnBrowsePub);
		
		JLabel lblSecretKeyring = new JLabel("Secret keyring");
		lblSecretKeyring.setBounds(10, 67, 341, 14);
		contentPanel.add(lblSecretKeyring);
		
		tbSec = new JTextField();
		tbSec.setColumns(10);
		tbSec.setBounds(10, 85, 355, 20);
		contentPanel.add(tbSec);
		
		JButton btnBrowseSec = new JButton("Browse");
		btnBrowseSec.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				secBrowseClick();
			}
		});
		btnBrowseSec.setBounds(375, 83, 80, 25);
		contentPanel.add(btnBrowseSec);
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
	}

	protected void secBrowseClick() {
		if (isOpen) 
		{
			String fileName = getOpenFileName();
			if (fileName.length() > 0) 
				tbSec.setText(fileName);
		} 
		else 
		{
			String fileName = getSaveFileName();
			if (fileName.length() > 0) 
				tbSec.setText(fileName);
		}
	}

	protected void pubBrowseClick() {
		if (isOpen) 
		{
			String fileName = getOpenFileName();
			if (fileName.length() > 0) 
				tbPub.setText(fileName);
		} 
		else 
		{
			String fileName = getSaveFileName();
			if (fileName.length() > 0) 
				tbPub.setText(fileName);
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

	public void setOpenDialog(boolean b) {
		isOpen = b;
	}

	public boolean isOK() {
		return isOk;
	}

	public String getPublicKeyringText() {
		return tbPub.getText();
	}

	public String getSecretKeyringText() {
		return tbSec.getText();
	}

	public void setPublicKeyringText(String pubKeyringFile) {
		tbPub.setText(pubKeyringFile);
	}

	public void setSecretKeyringText(String secKeyringFile) {
		tbSec.setText(secKeyringFile);
	}
}
