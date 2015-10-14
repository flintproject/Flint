/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.form;

import java.awt.Desktop;
import jp.oist.flint.garuda.GarudaClient;
import jp.oist.flint.k3.K3Loader;
import jp.oist.flint.plotter.IPlotter;
import jp.oist.flint.plotter.PlotterLoadException;
import jp.oist.flint.plotter.PlotterLoader;
import jp.sbi.garuda.platform.commons.exception.NetworkException;
import jp.sbi.garuda.platform.commons.net.GarudaConnectionNotInitializedException;
import org.apache.log4j.Logger;
import java.util.prefs.BackingStoreException;
import java.awt.event.ActionEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.security.GeneralSecurityException;
import java.util.prefs.Preferences;
import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.JComponent;
import javax.swing.JOptionPane;
import javax.swing.KeyStroke;
import jp.oist.flint.util.Utility;

/**
 * This is the class of the Preference dialog.
 */
public class PreferenceDialog extends javax.swing.JDialog {

    final private K3Loader mK3Loader = new K3Loader();

    final private PlotterLoader mPlotterLoader = new PlotterLoader();

    /**
     * Creates new form Preference
     */
    public PreferenceDialog(MainFrame parent, boolean modal) {
        super((java.awt.Frame)parent, modal);
        initComponents();

        if (GarudaClient.isRunning()) {
            radioButtonEnabled.setSelected(true);
            radioButtonDisabled.setSelected(false);
        }

        Action closeAction = new AbstractAction() {
                @Override
                public void actionPerformed(ActionEvent e) {
                    buttonCloseActionPerformed(null);
                }
        };

        label_FlintK3.addMouseListener(new MouseAdapter(){
            @Override
            public void mousePressed (MouseEvent evt) {
                String url = "https://flintk3.unit.oist.jp/user/";
                try {
                    Desktop desktop = Desktop.getDesktop();
                    desktop.browse(new URI(url));
                } catch (IOException ex) {
                    JOptionPane.showMessageDialog(PreferenceDialog.this, 
                            "Could not open the default browser\n " 
                                + "Please access to '" + url + "'", 
                            "Could not open the default browser.",
                            JOptionPane.ERROR_MESSAGE);
                } catch (URISyntaxException ex) {
                    // ignored.
                    Logger.getRootLogger().error(ex.getMessage());
                }
            }
        });

        addWindowListener(new WindowAdapter () {
            @Override
            public void windowOpened (WindowEvent evt) {
                Preferences prefs = Preferences.userRoot().node("/jp/oist/flint/session/k3");
                String encryptedUserId = prefs.get("encryptedUserId", null); 
                String encryptedPassword = prefs.get("encryptedPassword", null); 

                String userId = "";
                String password = "";
                String key = "";

                try {
                if (encryptedUserId != null && !encryptedUserId.isEmpty())
                    userId = Utility.decrypt(encryptedUserId);

                if (encryptedPassword != null && !encryptedPassword.isEmpty())
                    password = Utility.decrypt(encryptedPassword);
                } catch (GeneralSecurityException gse) {
                    // there is no way to recover encrypted information,
                    // so give up ...
                    Logger.getRootLogger().error(gse.getMessage());
                }

                txtfield_Username.setText(userId);
                txtfield_Password.setText(password);
            }
            @Override
            public void windowClosing(WindowEvent e) {
                buttonCloseActionPerformed(null);
            }
        });

        getRootPane().getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW).put(KeyStroke.getKeyStroke("ESCAPE"), "close");
        getRootPane().getActionMap().put("close", closeAction);

        try {
            mPlotterLoader.setComboBox(comboBoxPlotter);
        } catch (BackingStoreException bse) {
            // ignored
        }

        // K3
//        checkBoxK3Enabled.setSelected(mK3Loader.isEnabled());

        setLocationRelativeTo((java.awt.Frame)parent);
    }

    /**
     * This method is called from within the constructor to initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is always
     * regenerated by the Form Editor.
     */
    @SuppressWarnings("unchecked")
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        jTabbedPane1 = new javax.swing.JTabbedPane();
        jPanel1 = new javax.swing.JPanel();
        jLabel4 = new javax.swing.JLabel();
        comboBoxPlotter = new javax.swing.JComboBox();
        buttonDeletePlotter = new javax.swing.JButton();
        buttonConfigure = new javax.swing.JButton();
        buttonRegisterPlotter = new javax.swing.JButton();
        jPanel3 = new javax.swing.JPanel();
        radioButtonEnabled = new javax.swing.JRadioButton();
        radioButtonDisabled = new javax.swing.JRadioButton();
        jPanel4 = new javax.swing.JPanel();
        txtfield_Username = new javax.swing.JTextField();
        txtfield_Password = new javax.swing.JPasswordField();
        jLabel1 = new javax.swing.JLabel();
        jLabel2 = new javax.swing.JLabel();
        jLabel3 = new javax.swing.JLabel();
        label_FlintK3 = new javax.swing.JLabel();
        jPanel2 = new javax.swing.JPanel();
        buttonClose = new javax.swing.JButton();

        setDefaultCloseOperation(javax.swing.WindowConstants.DO_NOTHING_ON_CLOSE);
        setTitle("Flint Preference");
        setMinimumSize(new java.awt.Dimension(450, 250));

        jTabbedPane1.setBorder(javax.swing.BorderFactory.createEmptyBorder(0, 5, 0, 5));
        jTabbedPane1.setMinimumSize(new java.awt.Dimension(450, 220));
        jTabbedPane1.setPreferredSize(new java.awt.Dimension(450, 220));

        jPanel1.setMinimumSize(new java.awt.Dimension(450, 220));
        jPanel1.setPreferredSize(new java.awt.Dimension(450, 220));

        jLabel4.setText("Plotter:");

        comboBoxPlotter.addActionListener(new java.awt.event.ActionListener() {
            @Override
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                comboBoxPlotterActionPerformed(evt);
            }
        });

        buttonDeletePlotter.setText("Delete Plotter");
        buttonDeletePlotter.addActionListener(new java.awt.event.ActionListener() {
            @Override
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                buttonDeletePlotterActionPerformed(evt);
            }
        });

        buttonConfigure.setText("Configure");
        buttonConfigure.addActionListener(new java.awt.event.ActionListener() {
            @Override
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                buttonConfigureActionPerformed(evt);
            }
        });

        buttonRegisterPlotter.setText("Register Plotter");
        buttonRegisterPlotter.addActionListener(new java.awt.event.ActionListener() {
            @Override
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                buttonRegisterPlotterActionPerformed(evt);
            }
        });

        javax.swing.GroupLayout jPanel1Layout = new javax.swing.GroupLayout(jPanel1);
        jPanel1.setLayout(jPanel1Layout);
        jPanel1Layout.setHorizontalGroup(
            jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel1Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jLabel4)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(jPanel1Layout.createSequentialGroup()
                        .addComponent(buttonRegisterPlotter)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(buttonDeletePlotter))
                    .addGroup(jPanel1Layout.createSequentialGroup()
                        .addComponent(comboBoxPlotter, javax.swing.GroupLayout.PREFERRED_SIZE, 224, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(buttonConfigure)))
                .addContainerGap(50, Short.MAX_VALUE))
        );
        jPanel1Layout.setVerticalGroup(
            jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel1Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel4)
                    .addComponent(comboBoxPlotter, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(buttonConfigure))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(buttonDeletePlotter)
                    .addComponent(buttonRegisterPlotter))
                .addContainerGap(156, Short.MAX_VALUE))
        );

        jTabbedPane1.addTab("Plotter", jPanel1);

        radioButtonEnabled.setText("Online");
        radioButtonEnabled.addActionListener(new java.awt.event.ActionListener() {
            @Override
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                radioButtonEnabledActionPerformed(evt);
            }
        });

        radioButtonDisabled.setSelected(true);
        radioButtonDisabled.setText("Offline");
        radioButtonDisabled.addActionListener(new java.awt.event.ActionListener() {
            @Override
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                radioButtonDisabledActionPerformed(evt);
            }
        });

        javax.swing.GroupLayout jPanel3Layout = new javax.swing.GroupLayout(jPanel3);
        jPanel3.setLayout(jPanel3Layout);
        jPanel3Layout.setHorizontalGroup(
            jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel3Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(radioButtonEnabled)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(radioButtonDisabled)
                .addContainerGap(289, Short.MAX_VALUE))
        );
        jPanel3Layout.setVerticalGroup(
            jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel3Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(radioButtonEnabled)
                    .addComponent(radioButtonDisabled))
                .addContainerGap(166, Short.MAX_VALUE))
        );

        jTabbedPane1.addTab("Garuda", jPanel3);

        jLabel1.setText("User ID   :");

        jLabel2.setText("Password:");

        jLabel3.setText("If you do not have your account yet, please register at");

        label_FlintK3.setText("<html><body><a href='https://flintk3.unit.oist.jp/user/'>Flint K3</a></body></html>");
        label_FlintK3.setCursor(new java.awt.Cursor(java.awt.Cursor.DEFAULT_CURSOR));

        javax.swing.GroupLayout jPanel4Layout = new javax.swing.GroupLayout(jPanel4);
        jPanel4.setLayout(jPanel4Layout);
        jPanel4Layout.setHorizontalGroup(
            jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel4Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(jPanel4Layout.createSequentialGroup()
                        .addGroup(jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING, false)
                            .addComponent(jLabel1, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                            .addComponent(jLabel2, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addGroup(jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(txtfield_Password, javax.swing.GroupLayout.PREFERRED_SIZE, 204, javax.swing.GroupLayout.PREFERRED_SIZE)
                            .addComponent(txtfield_Username, javax.swing.GroupLayout.PREFERRED_SIZE, 204, javax.swing.GroupLayout.PREFERRED_SIZE)))
                    .addComponent(jLabel3)
                    .addComponent(label_FlintK3, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );
        jPanel4Layout.setVerticalGroup(
            jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel4Layout.createSequentialGroup()
                .addGap(25, 25, 25)
                .addGroup(jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(txtfield_Username, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jLabel1))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(txtfield_Password, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jLabel2))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jLabel3)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(label_FlintK3, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addContainerGap(31, Short.MAX_VALUE))
        );

        jTabbedPane1.addTab("K3", jPanel4);

        getContentPane().add(jTabbedPane1, java.awt.BorderLayout.CENTER);

        jPanel2.setMinimumSize(new java.awt.Dimension(450, 30));
        jPanel2.setPreferredSize(new java.awt.Dimension(450, 30));
        java.awt.FlowLayout flowLayout1 = new java.awt.FlowLayout(java.awt.FlowLayout.RIGHT, 5, 4);
        flowLayout1.setAlignOnBaseline(true);
        jPanel2.setLayout(flowLayout1);

        buttonClose.setText("Close");
        buttonClose.setMargin(new java.awt.Insets(2, 10, 2, 10));
        buttonClose.setMaximumSize(new java.awt.Dimension(80, 24));
        buttonClose.setMinimumSize(new java.awt.Dimension(80, 24));
        buttonClose.setPreferredSize(new java.awt.Dimension(80, 24));
        buttonClose.addActionListener(new java.awt.event.ActionListener() {
            @Override
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                buttonCloseActionPerformed(evt);
            }
        });
        jPanel2.add(buttonClose);

        getContentPane().add(jPanel2, java.awt.BorderLayout.SOUTH);

        pack();
    }// </editor-fold>//GEN-END:initComponents

    private void buttonCloseActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_buttonCloseActionPerformed
        Preferences prefs = Preferences.userRoot().node("/jp/oist/flint/session/k3");
        String userId = txtfield_Username.getText();
        String passwd = new String(txtfield_Password.getPassword());

        String encryptedUserId = "";
        String encryptedPassword = "";

        try {
        if (userId != null && !userId.isEmpty())
            encryptedUserId = Utility.encrypt(userId);

        if (passwd != null && !passwd.isEmpty())
            encryptedPassword = Utility.encrypt(passwd);
        } catch (GeneralSecurityException gse) {
            JOptionPane.showMessageDialog(this, gse.getMessage(), "Error on encrypting account information", JOptionPane.ERROR_MESSAGE);
            return;
        }

        prefs.put("encryptedUserId", encryptedUserId);
        prefs.put("encryptedPassword", encryptedPassword);

        dispose();
    }//GEN-LAST:event_buttonCloseActionPerformed

    private void radioButtonDisabledActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_radioButtonDisabledActionPerformed
        if (radioButtonDisabled.isSelected()) {
            radioButtonEnabled.setSelected(false);

            if (GarudaClient.isRunning()) {
                GarudaClient.stop();
            }
        }
    }//GEN-LAST:event_radioButtonDisabledActionPerformed

    private void radioButtonEnabledActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_radioButtonEnabledActionPerformed
        if (radioButtonEnabled.isSelected()) {
            radioButtonDisabled.setSelected(false);

            try {
                GarudaClient.start((MainFrame)getParent());
            } catch (GarudaConnectionNotInitializedException gcnie) {
                // recur the previous state
                radioButtonEnabled.setSelected(false);
                radioButtonDisabled.setSelected(true);

                JOptionPane.showMessageDialog(this, gcnie.getMessage(), "Error on Garuda API", JOptionPane.ERROR_MESSAGE);
            } catch (NetworkException ne) {
                // recur the previous state
                radioButtonEnabled.setSelected(false);
                radioButtonDisabled.setSelected(true);

                JOptionPane.showMessageDialog(this, ne.getMessage(), "Error on Garuda API", JOptionPane.ERROR_MESSAGE);
            }
        }
    }//GEN-LAST:event_radioButtonEnabledActionPerformed

    private void buttonRegisterPlotterActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_buttonRegisterPlotterActionPerformed
        PlotterRegistrationDialog prd = new PlotterRegistrationDialog((java.awt.Frame)getParent(), true);
        prd.setVisible(true);
    }//GEN-LAST:event_buttonRegisterPlotterActionPerformed

    private void buttonConfigureActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_buttonConfigureActionPerformed
        IPlotter plotter = (IPlotter)comboBoxPlotter.getSelectedItem();
        plotter.preference(this);
    }//GEN-LAST:event_buttonConfigureActionPerformed

    private void buttonDeletePlotterActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_buttonDeletePlotterActionPerformed
        String selectPlotterName = comboBoxPlotter.getSelectedItem().toString();
        String message = "Are you sure to delete " + selectPlotterName + "?";
        String title = "Deleting Plotter";
        int ans = JOptionPane.showConfirmDialog(getContentPane(), message, title, JOptionPane.YES_NO_OPTION);
        if (ans == JOptionPane.YES_OPTION) {
            // TODO delete plotter
        }
    }//GEN-LAST:event_buttonDeletePlotterActionPerformed

    private void comboBoxPlotterActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_comboBoxPlotterActionPerformed
        IPlotter plotter = (IPlotter)comboBoxPlotter.getSelectedItem();
        try {
            String key = mPlotterLoader.getKeyOf(plotter);
            mPlotterLoader.saveAsDefault(key);

            MainFrame m = (MainFrame)getOwner();
            m.setPlotterSettingTabEnabled(key);
        } catch (BackingStoreException bse) {
            // ignored
        } catch (PlotterLoadException ple) {
            // ignored
        }
    }//GEN-LAST:event_comboBoxPlotterActionPerformed

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton buttonClose;
    private javax.swing.JButton buttonConfigure;
    private javax.swing.JButton buttonDeletePlotter;
    private javax.swing.JButton buttonRegisterPlotter;
    private javax.swing.JComboBox comboBoxPlotter;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JLabel jLabel3;
    private javax.swing.JLabel jLabel4;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JPanel jPanel3;
    private javax.swing.JPanel jPanel4;
    private javax.swing.JTabbedPane jTabbedPane1;
    private javax.swing.JLabel label_FlintK3;
    private javax.swing.JRadioButton radioButtonDisabled;
    private javax.swing.JRadioButton radioButtonEnabled;
    private javax.swing.JPasswordField txtfield_Password;
    private javax.swing.JTextField txtfield_Username;
    // End of variables declaration//GEN-END:variables
}
