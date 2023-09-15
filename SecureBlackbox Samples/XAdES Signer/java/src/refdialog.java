import java.awt.BorderLayout;
import java.awt.FlowLayout;
import javax.swing.*;
import javax.swing.border.EmptyBorder;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import java.nio.charset.StandardCharsets;

import secureblackbox.*;

import static secureblackbox.XMLReference.*;

public class refdialog extends JDialog {
    private static final long serialVersionUID = 1L;
    private final JPanel contentPanel = new JPanel();
    private JTextField edID;
    private JTextField edType;
    private JTextField edTargetXMLElement;
    private JCheckBox cbAutoGenerateElementId;
    private JComboBox<String> cmbHashAlgorithm;
    private JComboBox<String> cmbCanonMethod;
    private JTextArea mmData;
    protected boolean isOk = false;

    /**
     * Launch the application.
     */
    public static void main(String[] args) {
        try {
            refdialog dialog = new refdialog();
            dialog.setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);
            dialog.setVisible(true);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /**
     * Create the dialog.
     */
    public refdialog() {
        setTitle("Reference Options");
        setBounds(100, 100, 330, 450);
        getContentPane().setLayout(new BorderLayout());
        contentPanel.setBorder(new EmptyBorder(5, 5, 5, 5));
        getContentPane().add(contentPanel, BorderLayout.CENTER);
        contentPanel.setLayout(null);

        JLabel lblId = new JLabel("ID");
        lblId.setBounds(10, 13, 111, 14);
        contentPanel.add(lblId);

        edID = new JTextField();
        edID.setBounds(100, 10, 200, 20);
        contentPanel.add(edID);
        edID.setColumns(10);

        JLabel lblType = new JLabel("Type");
        lblType.setBounds(10, 43, 111, 14);
        contentPanel.add(lblType);

        edType = new JTextField();
        edType.setBounds(100, 40, 200, 20);
        contentPanel.add(edType);
        edType.setColumns(10);

        JLabel lblDigestMethod = new JLabel("Hash Algorithm");
        lblDigestMethod.setBounds(10, 74, 121, 14);
        contentPanel.add(lblDigestMethod);

        cmbHashAlgorithm = new JComboBox<String>();
        cmbHashAlgorithm.setModel(new DefaultComboBoxModel<String>(new String[]{"MD5", "SHA1", "SHA224", "SHA256", "SHA384", "SHA512", "RIPEMD160"}));
        cmbHashAlgorithm.setBounds(100, 70, 200, 22);
        contentPanel.add(cmbHashAlgorithm);

        JLabel lblTargetXMLElement = new JLabel("Target XML Element");
        lblTargetXMLElement.setBounds(10, 105, 121, 14);
        contentPanel.add(lblTargetXMLElement);

        edTargetXMLElement = new JTextField();
        edTargetXMLElement.setBounds(10, 125, 290, 20);
        contentPanel.add(edTargetXMLElement);
        edTargetXMLElement.setColumns(10);

        cbAutoGenerateElementId = new JCheckBox("Auto generate target element Id");
        cbAutoGenerateElementId.setSelected(false);
        cbAutoGenerateElementId.setBounds(10, 150, 250, 23);
        contentPanel.add(cbAutoGenerateElementId);

        JLabel lblCanonMethod = new JLabel("Canonicalization method");
        lblCanonMethod.setBounds(10, 180, 200, 14);
        contentPanel.add(lblCanonMethod);

        cmbCanonMethod = new JComboBox<String>();
        cmbCanonMethod.setModel(new DefaultComboBoxModel<String>(new String[]{"Canonical", "Canonical with comments", "Canonical v1.1", "Canonical with comments v1.1", "Exclusive canonical", "Exclusive canonical with comments", "Minimal canonical", "None"}));
        cmbCanonMethod.setBounds(10, 200, 290, 22);
        contentPanel.add(cmbCanonMethod);

        JLabel lblURIData = new JLabel("Target Data");
        lblURIData.setBounds(10, 240, 100, 14);
        contentPanel.add(lblURIData);

        JScrollPane scrollPane_1 = new JScrollPane();
        scrollPane_1.setBounds(10, 257, 300, 157);
        contentPanel.add(scrollPane_1);

        mmData = new JTextArea();
        scrollPane_1.setViewportView(mmData);


        JPanel buttonPane = new JPanel();
        buttonPane.setLayout(new FlowLayout(FlowLayout.RIGHT));
        getContentPane().add(buttonPane, BorderLayout.SOUTH);

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


        JButton cancelButton = new JButton("Cancel");
        cancelButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                setVisible(false);
            }
        });
        cancelButton.setActionCommand("Cancel");
        buttonPane.add(cancelButton);

    }

    public void Initialize(XMLReference reference)
    {
        if (reference != null)
        {
            edID.setText(reference.getID());
            edType.setText(reference.getReferenceType());

            for (int i = 0; i < cmbHashAlgorithm.getItemCount(); i++) {
                if (cmbHashAlgorithm.getItemAt(i).equalsIgnoreCase(reference.getHashAlgorithm()))
                {
                    cmbHashAlgorithm.setSelectedIndex(i);
                    break;
                }
            }      

            edTargetXMLElement.setText(reference.getTargetXMLElement());
            cbAutoGenerateElementId.setSelected(reference.getAutoGenerateElementId());

            switch (reference.getCanonicalizationMethod())
            {
                case XMLReference.cxcmCanon:
                	cmbCanonMethod.setSelectedIndex(0);
                    break;
                case XMLReference.cxcmCanonComment:
                	cmbCanonMethod.setSelectedIndex(1);
                    break;
                case XMLReference.cxcmCanon_v1_1:
                	cmbCanonMethod.setSelectedIndex(2);
                    break;
                case XMLReference.cxcmCanonComment_v1_1:
                	cmbCanonMethod.setSelectedIndex(3);
                    break;
                case XMLReference.cxcmExclCanon:
                	cmbCanonMethod.setSelectedIndex(4);
                    break;
                case XMLReference.cxcmExclCanonComment:
                	cmbCanonMethod.setSelectedIndex(5);
                    break;
                case XMLReference.cxcmMinCanon:
                	cmbCanonMethod.setSelectedIndex(6);
                    break;
                case XMLReference.cxcmNone:
                	cmbCanonMethod.setSelectedIndex(7);
                    break;
                default:
                	cmbCanonMethod.setSelectedIndex(0);
                    break;
            }

            mmData.setText(new String(reference.getTargetData(), StandardCharsets.US_ASCII));
        }
    }

    public void Update(XMLReference reference)
    {
        if (reference != null)
        {
            try {
                reference.setID(edID.getText());
                reference.setReferenceType(edType.getText());

                reference.setHashAlgorithm(cmbHashAlgorithm.getItemAt(cmbHashAlgorithm.getSelectedIndex()));

                reference.setTargetXMLElement(edTargetXMLElement.getText());
                reference.setAutoGenerateElementId(cbAutoGenerateElementId.isSelected());

                switch (cmbCanonMethod.getSelectedIndex())
                {
                    case 0:
                        reference.setCanonicalizationMethod(XMLReference.cxcmCanon);
                        break;
                    case 1:
                        reference.setCanonicalizationMethod(XMLReference.cxcmCanonComment);
                        break;
                    case 2:
                        reference.setCanonicalizationMethod(XMLReference.cxcmCanon_v1_1);
                        break;
                    case 3:
                        reference.setCanonicalizationMethod(XMLReference.cxcmCanonComment_v1_1);
                        break;
                    case 4:
                        reference.setCanonicalizationMethod(XMLReference.cxcmExclCanon);
                        break;
                    case 5:
                        reference.setCanonicalizationMethod(XMLReference.cxcmExclCanonComment);
                        break;
                    case 6:
                        reference.setCanonicalizationMethod(XMLReference.cxcmMinCanon);
                        break;
                    case 7:
                        reference.setCanonicalizationMethod(XMLReference.cxcmNone);
                        break;
                    default:
                        reference.setCanonicalizationMethod(XMLReference.cxcmCanon);
                        break;
                }

                reference.setTargetData(mmData.getText().getBytes());
            }
            catch (Exception e)
            {}
        }
    }

    public boolean isOK() {
        return isOk;
    }
}
