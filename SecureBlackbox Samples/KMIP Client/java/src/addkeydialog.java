import java.awt.BorderLayout;
import javax.swing.*;
import javax.swing.border.EmptyBorder;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;

public class addkeydialog extends JDialog {

    private static final long serialVersionUID = 1L;
    private final JPanel contentPanel = new JPanel();
    private JTextField tbKeyFile;
    private JTextField tbId;
    private JComboBox cmbCurve;

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
    public addkeydialog() {
        setTitle("Adding key");
        setBounds(100, 100, 455, 205);
        getContentPane().setLayout(new BorderLayout());
        contentPanel.setBorder(new EmptyBorder(5, 5, 5, 5));
        getContentPane().add(contentPanel, BorderLayout.CENTER);
        contentPanel.setLayout(null);

        JLabel lblKeyFile = new JLabel("Key file");
        lblKeyFile.setBounds(10, 28, 50, 14);
        contentPanel.add(lblKeyFile);

        tbKeyFile = new JTextField("");
        tbKeyFile.setBounds(60, 25, 280, 20);
        contentPanel.add(tbKeyFile);
        tbKeyFile.setColumns(10);

        JButton btnKeyFile = new JButton("Browse");
        btnKeyFile.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                tbKeyFile.setText(getOpenFileName());
            }
        });
        btnKeyFile.setBounds(350, 23, 80, 25);
        contentPanel.add(btnKeyFile);

        JLabel lblCurve = new JLabel("Curve (for EC)");
        lblCurve.setBounds(10, 58, 80, 14);
        contentPanel.add(lblCurve);

        cmbCurve = new JComboBox<String>();
        cmbCurve.setModel(new DefaultComboBoxModel<String>(new String[] {"", "SECP112R1", "SECT113R1", "SECP128R1", "SECT131R1", "SECP160K1", "SECT163K1", "C2PNB176W1", "C2TNB191V1", "SECP192K1", "SECT193R1", "C2PNB208W1", "SECP224K1", "SECT233K1", "SECT239K1", "SECP256K1", "C2PNB272W1", "SECT283K1", "C2PNB304W1", "C2TNB359V1", "C2PNB368W1", "SECP384R1", "SECT409K1", "C2TNB431R1", "BRAINPOOLP512R1", "SECP521R1", "SECT571K1"}));
        cmbCurve.setBounds(100, 55, 150, 22);
        contentPanel.add(cmbCurve);

        JLabel lblId = new JLabel("Id");
        lblId.setBounds(10, 88, 20, 14);
        contentPanel.add(lblId);

        tbId = new JTextField();
        tbId.setText("");
        tbId.setBounds(30, 85, 158, 20);
        contentPanel.add(tbId);


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

    public String getKeyFile() {
        return tbKeyFile.getText();
    }

    public String getCurve() {
        return (String)cmbCurve.getItemAt(cmbCurve.getSelectedIndex());
    }

    public String getId() {
        return tbId.getText();
    }
}
