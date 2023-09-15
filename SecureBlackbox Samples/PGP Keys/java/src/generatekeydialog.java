import java.awt.BorderLayout;
import java.awt.FlowLayout;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JPanel;
import javax.swing.border.EmptyBorder;
import javax.swing.border.TitledBorder;
import javax.swing.JLabel;
import java.awt.Font;
import java.awt.Color;
import javax.swing.JOptionPane;
import javax.swing.SwingConstants;
import javax.swing.JRadioButton;
import javax.swing.ButtonGroup;
import javax.swing.JComboBox;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JTextField;
import javax.swing.JPasswordField;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;

import secureblackbox.*;

public class generatekeydialog extends JDialog {

	private static final long serialVersionUID = 1L;
	private final JPanel contentPanel = new JPanel();
	private final ButtonGroup buttonGroup = new ButtonGroup();
	private JTextField tbName;
	private JTextField tbEmail;
	private JPasswordField tbPassphrase;
	private JPasswordField tbPassphraseConf;
	
	private final int STATE_ALGORITHM_SELECT	= 1;
	private final int STATE_USERNAME_SELECT		= 2;
	private final int STATE_PASSPHRASE_SELECT	= 3;
	private final int STATE_GENERATION			= 4;
	private final int STATE_FINISH				= 5;	
	private final int STATE_INVALID				= -1;
	private JLabel lFinish;
	private JLabel lStep;
	private JLabel lStepComment;
	private JButton btnBack;
	private JButton btnNext;
	private JPanel pAlgorithmSelect;
	private JPanel pPassphrase;
	private JPanel pUserSelect;
	private JPanel pGeneration;
	private JPanel pFinish;
	private JButton btnCancel;
		
	private int state = STATE_ALGORITHM_SELECT;
	private int bits;
	private boolean useRSA;
	private String username;
	private String passphrase;
	private JRadioButton rbRsa;
	private JComboBox<Integer> cbBits;
	public boolean Success = false;
	public PGPKey NewKey;


	/**
	 * Launch the application.
	 */
	public static void main(String[] args) {
		try {
			generatekeydialog dialog = new generatekeydialog();
			dialog.setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);
			dialog.setVisible(true);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	/**
	 * Create the dialog.
	 */
	public generatekeydialog() {
		setTitle("Generate new key");
		setResizable(false);
		setBounds(100, 100, 576, 371);
		getContentPane().setLayout(new BorderLayout());
		contentPanel.setBorder(new EmptyBorder(5, 5, 5, 5));
		getContentPane().add(contentPanel, BorderLayout.CENTER);
		contentPanel.setLayout(null);
		{
			JPanel panel = new JPanel();
			panel.setBackground(Color.LIGHT_GRAY);
			panel.setBorder(new TitledBorder(null, "", TitledBorder.LEADING, TitledBorder.TOP, null, null));
			panel.setBounds(0, 0, 570, 61);
			contentPanel.add(panel);
			panel.setLayout(null);
			{
				lStep = new JLabel("Step 1 of 5");
				lStep.setFont(new Font("Tahoma", Font.BOLD, 11));
				lStep.setBounds(10, 11, 205, 14);
				panel.add(lStep);
			}
			{
				lStepComment = new JLabel("Please select public key algorithm");
				lStepComment.setFont(new Font("Tahoma", Font.PLAIN, 11));
				lStepComment.setBounds(10, 36, 272, 14);
				panel.add(lStepComment);
			}
		}
		{
			pAlgorithmSelect = new JPanel();
			pAlgorithmSelect.setBounds(0, 62, 570, 251);
			contentPanel.add(pAlgorithmSelect);
			pAlgorithmSelect.setLayout(null);
			{
				JLabel lblPleaseSelectPublic = new JLabel("Please select public key algorithm and key length in bits:");
				lblPleaseSelectPublic.setBounds(29, 22, 328, 14);
				pAlgorithmSelect.add(lblPleaseSelectPublic);
			}
			
			rbRsa = new JRadioButton("RSA");
			buttonGroup.add(rbRsa);
			rbRsa.setBounds(130, 55, 109, 23);
			pAlgorithmSelect.add(rbRsa);
			
			JRadioButton rbElgamaldssrecommended = new JRadioButton("Elgamal/DSS (recommended)");
			rbElgamaldssrecommended.setSelected(true);
			buttonGroup.add(rbElgamaldssrecommended);
			rbElgamaldssrecommended.setBounds(130, 81, 303, 23);
			pAlgorithmSelect.add(rbElgamaldssrecommended);
			
			cbBits = new JComboBox<Integer>();
			cbBits.setModel(new DefaultComboBoxModel<Integer>(new Integer[] {512, 1024, 1536, 2048}));
			cbBits.setBounds(130, 111, 167, 20);
			pAlgorithmSelect.add(cbBits);
		}
		{
			pPassphrase = new JPanel();
			pPassphrase.setVisible(false);
			pPassphrase.setLayout(null);
			pPassphrase.setBounds(0, 62, 570, 251);
			contentPanel.add(pPassphrase);
			{
				JLabel lblPleaseSpecifyA = new JLabel("Please specify a passphrase for the new key:");
				lblPleaseSpecifyA.setBounds(23, 22, 320, 14);
				pPassphrase.add(lblPleaseSpecifyA);
			}
			{
				JLabel lblPassphrase = new JLabel("Passphrase");
				lblPassphrase.setBounds(106, 62, 208, 14);
				pPassphrase.add(lblPassphrase);
			}
			{
				tbPassphrase = new JPasswordField();
				tbPassphrase.setColumns(10);
				tbPassphrase.setBounds(106, 80, 330, 20);
				pPassphrase.add(tbPassphrase);
			}
			{
				JLabel lblConfirm = new JLabel("Confirm");
				lblConfirm.setBounds(106, 111, 148, 14);
				pPassphrase.add(lblConfirm);
			}
			{
				tbPassphraseConf = new JPasswordField();
				tbPassphraseConf.setColumns(10);
				tbPassphraseConf.setBounds(106, 132, 330, 20);
				pPassphrase.add(tbPassphraseConf);
			}
		}
		{
			pUserSelect = new JPanel();
			pUserSelect.setVisible(false);
			pUserSelect.setLayout(null);
			pUserSelect.setBounds(0, 62, 570, 251);
			contentPanel.add(pUserSelect);
			{
				JLabel lblPleaseSpecifyYour = new JLabel("Please specify your name and e-mail address:");
				lblPleaseSpecifyYour.setBounds(23, 22, 320, 14);
				pUserSelect.add(lblPleaseSpecifyYour);
			}
			{
				JLabel lblName = new JLabel("Name");
				lblName.setBounds(106, 62, 46, 14);
				pUserSelect.add(lblName);
			}
			{
				tbName = new JTextField();
				tbName.setBounds(106, 80, 330, 20);
				pUserSelect.add(tbName);
				tbName.setColumns(10);
			}
			{
				JLabel lblEmail = new JLabel("E-Mail address");
				lblEmail.setBounds(106, 111, 148, 14);
				pUserSelect.add(lblEmail);
			}
			{
				tbEmail = new JTextField();
				tbEmail.setBounds(106, 132, 330, 20);
				pUserSelect.add(tbEmail);
				tbEmail.setColumns(10);
			}
		}
		{
			pGeneration = new JPanel();
			pGeneration.setVisible(false);
			pGeneration.setBounds(0, 62, 570, 251);
			contentPanel.add(pGeneration);
			pGeneration.setLayout(null);
			{
				JLabel lblGgg = new JLabel("<html><body><center>Please wait while the key is being generated... The generation process might take a long time depending on a key size you selected.</center></body></html>");
				lblGgg.setHorizontalAlignment(SwingConstants.CENTER);
				lblGgg.setHorizontalTextPosition(SwingConstants.CENTER);
				lblGgg.setBounds(77, 66, 398, 77);
				pGeneration.add(lblGgg);
			}
		}
		{
			pFinish = new JPanel();
			pFinish.setVisible(false);
			pFinish.setBounds(0, 62, 570, 251);
			contentPanel.add(pFinish);
			pFinish.setLayout(null);
			{
				lFinish = new JLabel("Generation finished");
				lFinish.setBounds(219, 97, 244, 14);
				pFinish.add(lFinish);
			}
		}
		{
			JPanel buttonPane = new JPanel();
			buttonPane.setLayout(new FlowLayout(FlowLayout.RIGHT));
			getContentPane().add(buttonPane, BorderLayout.SOUTH);
			{
				btnBack = new JButton("< Back");
				btnBack.addActionListener(new ActionListener() {
					public void actionPerformed(ActionEvent e) {
						Back();
					}
				});
				buttonPane.add(btnBack);
			}
			{
				btnNext = new JButton("Next >");
				btnNext.addActionListener(new ActionListener() {
					public void actionPerformed(ActionEvent e) {
						Next();
					}
				});
				btnNext.setActionCommand("OK");
				buttonPane.add(btnNext);
				getRootPane().setDefaultButton(btnNext);
			}
			{
				btnCancel = new JButton("Cancel");
				btnCancel.addActionListener(new ActionListener() {
					public void actionPerformed(ActionEvent e) {
						setVisible(false);
					}
				});
				btnCancel.setActionCommand("Cancel");
				buttonPane.add(btnCancel);
			}
		}
	}
	
	private void EnableView(JPanel p)
	{
		p.setVisible(true);
	}

	private void SetCaption(String Step, String Comment)
	{
		lStep.setText(Step);
		lStepComment.setText(Comment);
	}

	private void EnableButtons(boolean Back, boolean Next)
	{
		btnBack.setEnabled(Back);
		btnNext.setEnabled(Next);
	}

	private void ChangeState(int newState)
	{
		pAlgorithmSelect.setVisible(false);
		pUserSelect.setVisible(false);
		pPassphrase.setVisible(false);
		pGeneration.setVisible(false);
		pFinish.setVisible(false);
		switch(newState) 
		{
			case STATE_ALGORITHM_SELECT:
				SetCaption("Step 1 of 4", "Public key algorithm selection");
				EnableButtons(false, true);
				EnableView(pAlgorithmSelect);
				break;
			case STATE_USERNAME_SELECT:
				SetCaption("Step 2 of 4", "Username setup");
				EnableButtons(true, true);
				EnableView(pUserSelect);
				break;
			case STATE_PASSPHRASE_SELECT:
				SetCaption("Step 3 of 4", "Passphrase setup");
				EnableButtons(true, true);
				EnableView(pPassphrase);
				break;
			case STATE_GENERATION:
				SetCaption("Step 4 of 4", "Key generation");
				EnableButtons(false, false);
				EnableView(pGeneration);
				btnCancel.setEnabled(false);
				java.lang.Thread t = new Thread(new Runnable() {					
					public void run() {
						GenerateKey();
						btnCancel.setEnabled(true);
						ChangeState(STATE_FINISH);
					}
				});
				t.start();
				break;
			case STATE_FINISH:
				SetCaption("Finish", "End of work");
				EnableButtons(false, false);
				btnCancel.setText("Finish");
				EnableView(pFinish);
				break;
		}
		state = newState;
	}

	private int GetPrevState(int currState)
	{
		int result;
		switch(currState) 
		{
			case STATE_ALGORITHM_SELECT:
				result = STATE_INVALID;
				break;
			case STATE_USERNAME_SELECT:
				result = STATE_ALGORITHM_SELECT;
				break;
			case STATE_PASSPHRASE_SELECT:
				result = STATE_USERNAME_SELECT;
				break;
			default:
				result = STATE_INVALID;
				break;
		}
		return result;
	}

	private void Next()
	{
		switch(state) 
		{
			case STATE_ALGORITHM_SELECT:
				useRSA = rbRsa.isSelected();
				bits = 512 + 512 * cbBits.getSelectedIndex();
				ChangeState(STATE_USERNAME_SELECT);
				break;
			case STATE_USERNAME_SELECT:
				if ((tbName.getText().compareTo("") == 0) && (tbEmail.getText().compareTo("") == 0)) 
				{
					showErroMessage("Please specify non-empty user name");
				} 
				else 
				{
					username = tbName.getText() + " <" + tbEmail.getText() + ">";
					ChangeState(STATE_PASSPHRASE_SELECT);
				}
				break;
			case STATE_PASSPHRASE_SELECT:
				if (new String(tbPassphrase.getPassword()).compareTo(new String(tbPassphraseConf.getPassword())) != 0) 
				{
					showErroMessage("Confirmation does not match passphrase");
				} 
				else 
				{
					passphrase = new String(tbPassphrase.getPassword());
					ChangeState(STATE_GENERATION);
				}
				break;
			
		}
	}

	private void showErroMessage(String msg) {
		JOptionPane.showMessageDialog(null, msg, "Error", JOptionPane.ERROR_MESSAGE);		
	}

	private void Back()
	{
		ChangeState(GetPrevState(state));
	}
	
	private void GenerateKey()
	{
		lFinish.setText("Generation completed");
		Success = true;
		try 
		{
			Pgpkeymanager keymanager = new Pgpkeymanager();
			if (useRSA)
			{
				keymanager.generateLegacy(username, bits, passphrase, 0);
			}
			else
			{
				keymanager.generatePair(username, "DSA", bits, "Elgamal-encrypt", bits, passphrase, 0);
			}
			NewKey = (PGPKey)keymanager.getKey().clone();
		} 
		catch(Exception ex) 
		{
			lFinish.setText(ex.getMessage());
			Success = false;
		}
	}

	public PGPKey getNewKey() {
		return NewKey;
	}

	public boolean isSuccess() {
		return Success;
	}	
}
