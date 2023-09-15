import java.awt.BorderLayout;
import javax.swing.*;
import javax.swing.border.EmptyBorder;
import javax.swing.border.TitledBorder;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;

public class createcertdialog extends JDialog {

    private static final long serialVersionUID = 1L;
    private final JPanel contentPanel = new JPanel();
    private JComboBox cmbPublicAlgorithm;
    private JComboBox cmbHashAlgorithm;
    private JSpinner sKeyLength;
    private JLabel lblCurve;
    private JComboBox cmbCurve;
    private JTextField tbId;
    private JTextField tbCountryS;
    private JTextField tbStateS;
    private JTextField tbLocalityS;
    private JTextField tbOrganizationS;
    private JTextField tbOrgUnitS;
    private JTextField tbCommonNameS;

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
    public createcertdialog() {
        setTitle("Create certificate");
        setBounds(100, 100, 455, 505);
        getContentPane().setLayout(new BorderLayout());
        contentPanel.setBorder(new EmptyBorder(5, 5, 5, 5));
        getContentPane().add(contentPanel, BorderLayout.CENTER);
        contentPanel.setLayout(null);
        {
            JPanel panel = new JPanel();
            panel.setBorder(new TitledBorder(null, "Parameters", TitledBorder.LEADING, TitledBorder.TOP, null, null));
            panel.setBounds(10, 10, 420, 410);
            contentPanel.add(panel);
            panel.setLayout(null);

            JLabel lblPublicAlgorithm = new JLabel("Public algorithm");
            lblPublicAlgorithm.setBounds(10, 23, 100, 14);
            panel.add(lblPublicAlgorithm);

            cmbPublicAlgorithm = new JComboBox<String>();
            cmbPublicAlgorithm.setModel(new DefaultComboBoxModel<String>(new String[] {"RSA", "DSA", "EC"}));
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
                }
            });
            cmbPublicAlgorithm.setBounds(120, 20, 200, 22);
            panel.add(cmbPublicAlgorithm);

            JLabel lblHashAlgorithm = new JLabel("Hash algorithm");
            lblHashAlgorithm.setBounds(10, 53, 100, 14);
            panel.add(lblHashAlgorithm);

            cmbHashAlgorithm = new JComboBox<String>();
            cmbHashAlgorithm.setModel(new DefaultComboBoxModel<String>(new String[] {"MD2", "MD5", "SHA1", "SHA224", "SHA256", "SHA384", "SHA512"}));
            cmbHashAlgorithm.setSelectedIndex(2);
            cmbHashAlgorithm.setBounds(120, 50, 200, 22);
            panel.add(cmbHashAlgorithm);

            JLabel lblKeyLength = new JLabel("Key length");
            lblKeyLength.setBounds(10, 83, 70, 14);
            panel.add(lblKeyLength);

            sKeyLength = new JSpinner();
            sKeyLength.setModel(new SpinnerNumberModel(new Integer(1024), new Integer(0), null, new Integer(1)));
            sKeyLength.setBounds(80, 80, 70, 20);
            panel.add(sKeyLength);

            lblCurve = new JLabel("Curve (for EC)");
            lblCurve.setEnabled(false);
            lblCurve.setBounds(165, 83, 80, 14);
            panel.add(lblCurve);

            cmbCurve = new JComboBox<String>();
            cmbCurve.setModel(new DefaultComboBoxModel<String>(new String[] {"", "SECP112R1", "SECT113R1", "SECP128R1", "SECT131R1", "SECP160K1", "SECT163K1", "C2PNB176W1", "C2TNB191V1", "SECP192K1", "SECT193R1", "C2PNB208W1", "SECP224K1", "SECT233K1", "SECT239K1", "SECP256K1", "C2PNB272W1", "SECT283K1", "C2PNB304W1", "C2TNB359V1", "C2PNB368W1", "SECP384R1", "SECT409K1", "C2TNB431R1", "BRAINPOOLP512R1", "SECP521R1", "SECT571K1"}));
            cmbCurve.setEnabled(false);
            cmbCurve.setBounds(250, 80, 150, 22);
            panel.add(cmbCurve);

            JLabel lblId = new JLabel("Id");
            lblId.setBounds(10, 113, 20, 14);
            panel.add(lblId);

            tbId = new JTextField();
            tbId.setText("");
            tbId.setBounds(40, 110, 158, 20);
            panel.add(tbId);

            JPanel panelSubject = new JPanel();
            panelSubject.setBorder(new TitledBorder(null, "Subject parameters  ", TitledBorder.LEADING, TitledBorder.TOP, null, null));
            panelSubject.setBounds(8, 140, 405, 265);
            panel.add(panelSubject);
            panelSubject.setLayout(null);

            JLabel lblCountryS = new JLabel("Country");
            lblCountryS.setBounds(40, 20, 100, 14);
            panelSubject.add(lblCountryS);

            tbCountryS = new JTextField();
            tbCountryS.setText("");
            tbCountryS.setBounds(40, 36, 320, 20);
            panelSubject.add(tbCountryS);

            JLabel lblStateS = new JLabel("State or Province");
            lblStateS.setBounds(40, 60, 100, 14);
            panelSubject.add(lblStateS);

            tbStateS = new JTextField();
            tbStateS.setText("");
            tbStateS.setBounds(40, 76, 320, 20);
            panelSubject.add(tbStateS);

            JLabel lblLocalityS = new JLabel("Locality");
            lblLocalityS.setBounds(40, 100, 100, 14);
            panelSubject.add(lblLocalityS);

            tbLocalityS = new JTextField();
            tbLocalityS.setText("");
            tbLocalityS.setBounds(40, 116, 320, 20);
            panelSubject.add(tbLocalityS);

            JLabel lblOrganizationS= new JLabel("Organization");
            lblOrganizationS.setBounds(40, 140, 100, 14);
            panelSubject.add(lblOrganizationS);

            tbOrganizationS = new JTextField();
            tbOrganizationS.setText("");
            tbOrganizationS.setBounds(40, 156, 320, 20);
            panelSubject.add(tbOrganizationS);

            JLabel lblOrgUnitS = new JLabel("Organization Unit");
            lblOrgUnitS.setBounds(40, 180, 100, 14);
            panelSubject.add(lblOrgUnitS);

            tbOrgUnitS = new JTextField();
            tbOrgUnitS.setText("");
            tbOrgUnitS.setBounds(40, 196, 320, 20);
            panelSubject.add(tbOrgUnitS);

            JLabel lblCommonNameS = new JLabel("Common Name");
            lblCommonNameS.setBounds(40, 220, 100, 14);
            panelSubject.add(lblCommonNameS);

            tbCommonNameS = new JTextField();
            tbCommonNameS.setText("");
            tbCommonNameS.setBounds(40, 236, 320, 20);
            panelSubject.add(tbCommonNameS);
        }

        JButton btnOk = new JButton("Ok");
        btnOk.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                if ((tbCountryS.getText().isEmpty()) || (tbLocalityS.getText().isEmpty()) || (tbOrganizationS.getText().isEmpty()) || (tbCommonNameS.getText().isEmpty()))
                {
                    showErrorMessage("One or several subject fields are empty. Correct, please.");
                    return;
                }

                isOk = true;
                setVisible(false);
            }
        });
        btnOk.setBounds(260, 435, 80, 25);
        contentPanel.add(btnOk);

        JButton btnCancel = new JButton("Cancel");
        btnCancel.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                isOk = false;
                setVisible(false);
            }
        });
        btnCancel.setBounds(350, 435, 80, 25);
        contentPanel.add(btnCancel);
    }

    static void showErrorMessage(String msg) {
        JOptionPane.showMessageDialog(null, msg, "Error", JOptionPane.ERROR_MESSAGE);
    }

    public boolean isOK() {
        return isOk;
    }

    public String getPublicAlgorithm() {
        return (String)cmbPublicAlgorithm.getItemAt(cmbPublicAlgorithm.getSelectedIndex());
    }

    public String getHashAlgorithm() {
        return (String)cmbHashAlgorithm.getItemAt(cmbHashAlgorithm.getSelectedIndex());
    }

    public int getKeyLength() {
        return (Integer)sKeyLength.getValue();
    }

    public String getCurve() {
        return (String)cmbCurve.getItemAt(cmbCurve.getSelectedIndex());
    }

    public String getSubject() {
        return "/C=" + tbCountryS.getText() + "/ST=" + tbStateS.getText() +
                "/L=" + tbLocalityS.getText() + "/O=" + tbOrganizationS.getText() +
                "/OU=" + tbOrgUnitS.getText() + "/CN=" + tbCommonNameS.getText();
    }

    public String getId() {
        return tbId.getText();
    }
}
