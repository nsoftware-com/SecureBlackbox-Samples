import java.awt.BorderLayout;
import javax.swing.*;
import javax.swing.border.EmptyBorder;
import javax.swing.border.TitledBorder;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;

public class createkeydialog extends JDialog {

    private static final long serialVersionUID = 1L;
    private final JPanel contentPanel = new JPanel();
    private JComboBox cmbPublicAlgorithm;
    private JSpinner sKeyLength;
    private JLabel lblCurve;
    private JComboBox cmbCurve;
    private JTextField tbId;

    protected boolean isOk;

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
    public createkeydialog() {
        setTitle("Create key");
        setBounds(100, 100, 455, 205);
        getContentPane().setLayout(new BorderLayout());
        contentPanel.setBorder(new EmptyBorder(5, 5, 5, 5));
        getContentPane().add(contentPanel, BorderLayout.CENTER);
        contentPanel.setLayout(null);
        {
            JPanel panel = new JPanel();
            panel.setBorder(new TitledBorder(null, "Parameters", TitledBorder.LEADING, TitledBorder.TOP, null, null));
            panel.setBounds(10, 10, 420, 110);
            contentPanel.add(panel);
            panel.setLayout(null);

            JLabel lblPublicAlgorithm = new JLabel("Public algorithm");
            lblPublicAlgorithm.setBounds(10, 23, 100, 14);
            panel.add(lblPublicAlgorithm);

            cmbPublicAlgorithm = new JComboBox<String>();
            cmbPublicAlgorithm.setModel(new DefaultComboBoxModel<String>(new String[] {"RSA", "DSA", "EC", "DES", "3DES", "AES"}));
            cmbPublicAlgorithm.addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(ActionEvent e) {
                    if (cmbPublicAlgorithm.getSelectedIndex() == 2)
                    {
                        lblCurve.setEnabled(true);
                        cmbCurve.setEnabled(true);
                    }
                    else
                    {
                        lblCurve.setEnabled(false);
                        cmbCurve.setEnabled(false);
                    }

                    switch (cmbPublicAlgorithm.getSelectedIndex())
                    {
                        case 3: sKeyLength.setValue(64); break;
                        case 4: sKeyLength.setValue(192); break;
                        case 5: sKeyLength.setValue(256); break;
                        default: sKeyLength.setValue(1024); break;
                    }
                }
            });
            cmbPublicAlgorithm.setBounds(120, 20, 200, 22);
            panel.add(cmbPublicAlgorithm);

            JLabel lblPort = new JLabel("Port");
            lblPort.setBounds(10, 53, 30, 14);
            panel.add(lblPort);

            sKeyLength = new JSpinner();
            sKeyLength.setModel(new SpinnerNumberModel(new Integer(1024), new Integer(0), null, new Integer(1)));
            sKeyLength.setBounds(50, 50, 70, 20);
            panel.add(sKeyLength);

            lblCurve = new JLabel("Curve (for EC)");
            lblCurve.setEnabled(false);
            lblCurve.setBounds(160, 53, 80, 14);
            panel.add(lblCurve);

            cmbCurve = new JComboBox<String>();
            cmbCurve.setModel(new DefaultComboBoxModel<String>(new String[] {"", "SECP112R1", "SECT113R1", "SECP128R1", "SECT131R1", "SECP160K1", "SECT163K1", "C2PNB176W1", "C2TNB191V1", "SECP192K1", "SECT193R1", "C2PNB208W1", "SECP224K1", "SECT233K1", "SECT239K1", "SECP256K1", "C2PNB272W1", "SECT283K1", "C2PNB304W1", "C2TNB359V1", "C2PNB368W1", "SECP384R1", "SECT409K1", "C2TNB431R1", "BRAINPOOLP512R1", "SECP521R1", "SECT571K1"}));
            cmbCurve.setEnabled(false);
            cmbCurve.setBounds(250, 50, 150, 22);
            panel.add(cmbCurve);

            JLabel lblId = new JLabel("Id");
            lblId.setBounds(10, 83, 20, 14);
            panel.add(lblId);

            tbId = new JTextField();
            tbId.setText("");
            tbId.setBounds(40, 80, 158, 20);
            panel.add(tbId);
        }

        JButton btnOk = new JButton("Ok");
        btnOk.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                isOk = true;
                setVisible(false);
            }
        });
        btnOk.setBounds(260, 135, 80, 25);
        contentPanel.add(btnOk);

        JButton btnCancel = new JButton("Cancel");
        btnCancel.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                isOk = false;
                setVisible(false);
            }
        });
        btnCancel.setBounds(350, 135, 80, 25);
        contentPanel.add(btnCancel);
    }

    public boolean isOK() {
        return isOk;
    }

    public String getPublicAlgorithm() {
        return (String)cmbPublicAlgorithm.getItemAt(cmbPublicAlgorithm.getSelectedIndex());
    }

    public int getKeyLength() {
        return (Integer)sKeyLength.getValue();
    }

    public String getCurve() {
        return (String)cmbCurve.getItemAt(cmbCurve.getSelectedIndex());
    }

    public String getId() {
        return tbId.getText();
    }
}
