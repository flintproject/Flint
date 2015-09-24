/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.form.sub;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import jp.oist.flint.util.HashTableModel;
import jp.oist.flint.util.Utility;
import jp.physiome.Ipc;
import java.io.DataOutputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;
import javax.swing.RowFilter;
import javax.swing.table.TableColumn;
import javax.swing.table.TableModel;
import javax.swing.table.TableRowSorter;

public class VariableSelectionPane extends javax.swing.JPanel 
    implements PropertyChangeListener {

    private final HashTableModel mVariableModel;
    private TableRowSorter<HashTableModel> mEnabledSorter;

    public VariableSelectionPane(Ipc.ModelProbeResponse response) {
        initComponents();

        addPropertyChangeListener(this);

        Ipc.ModelVariableTable variableTable = response.getVariableTable();
        cmb_Filter_Column.setModel(new javax.swing.DefaultComboBoxModel(variableTable.getColumnList().toArray()));
        mVariableModel = Utility.convertVariableTableToTableModel(variableTable);

        if (mVariableModel.getRowCount() > 0) {
            tbl_Available_Variables.setModel(mVariableModel);
            TableColumn tc = tbl_Available_Variables.getColumn("key");
            tbl_Available_Variables.removeColumn(tc);
        } else {
            tbl_Available_Variables.setEnabled(false);
        }
        if (mVariableModel.getRowCount() > 0) {
            tbl_Enabled_Variables.setModel(mVariableModel);
            TableColumn tc = tbl_Enabled_Variables.getColumn("key");
            tbl_Enabled_Variables.removeColumn(tc);
            mEnabledSorter = new TableRowSorter<>(mVariableModel);
            tbl_Enabled_Variables.setRowSorter(mEnabledSorter);
            mEnabledSorter.setRowFilter(null);
        } else {
            tbl_Enabled_Variables.setEnabled(false);
        }
    }

    javax.swing.JTable getAvailableVariablesTable () {
        return tbl_Available_Variables;
    }

    javax.swing.JTable getEnabledVariablesTable () {
        return tbl_Enabled_Variables;
    }

    public ArrayList<String> getKeys () {
        ArrayList<String> keys = new ArrayList<>();
        for (int i = 0; i < tbl_Enabled_Variables.getRowCount(); i++) {
            int j = tbl_Enabled_Variables.convertRowIndexToModel(i);
            String key = mVariableModel.getValueAt(j, 2).toString();
            keys.add(key);
        }
        return keys;
    }

    public void setSelectedFilterSyntax (Object v) {
        cmb_Filter_Syntax.setSelectedItem(v);
    }

    public void setSelectedFilterSyntax (int i) {
        cmb_Filter_Syntax.setSelectedIndex(i);
    }

    public void setFilterPattern(String s) {
        txt_Filter_Pattern.setText(s);
    }

    public void setSelectedFilterColumn(int i) {
        cmb_Filter_Column.setSelectedIndex(i);
    }

    public void setSelectedFilterColumn(Object v) {
        cmb_Filter_Column.setSelectedItem(v);
    }

    public int getSelectedFilterSyntaxIndex () {
        return cmb_Filter_Syntax.getSelectedIndex();
    }

    public Object getSelectedFilterSyntax () {
        return cmb_Filter_Syntax.getSelectedItem();
    }

    public String getFilterPattern () {
        return txt_Filter_Pattern.getText();
    }

    public int getSelectedFilterColumnIndex () {
        return cmb_Filter_Column.getSelectedIndex();
    }

    public Object getSelectedFilterColumn () {
        return cmb_Filter_Column.getSelectedItem();
    }

    private void variableFilter () {
        int selectedFilter = cmb_Filter_Syntax.getSelectedIndex();
        int selectedColumn = cmb_Filter_Column.getSelectedIndex();

        String pattern = txt_Filter_Pattern.getText();

        if (pattern.isEmpty()) {
            mEnabledSorter.setRowFilter(null);
            return;
        }

        switch (selectedFilter) {
            case 0: // Regular expression
                try { 
                mEnabledSorter.setRowFilter(RowFilter.regexFilter(pattern, selectedColumn));
                } catch (PatternSyntaxException e) {
                    mEnabledSorter.setRowFilter(null);
                }
                break;
            case 1: // Wildcard
                pattern = pattern.replace("*", "");
                pattern =  Pattern.quote(pattern);
                mEnabledSorter.setRowFilter(RowFilter.regexFilter(pattern, selectedColumn));
                break;
            case 2: // Fixed String
                String q = Pattern.quote(pattern);
                RowFilter<TableModel,Integer> filter = RowFilter.regexFilter(q,selectedColumn);
                mEnabledSorter.setRowFilter(filter);
                break;
            default:
                mEnabledSorter.setRowFilter(null);
                break;
        }
    }

    public void writeEnabledTableTo (File f) throws FileNotFoundException, IOException {
        try (DataOutputStream specStream = new DataOutputStream(new FileOutputStream(f))) {
        for (int i = 0; i < tbl_Enabled_Variables.getRowCount(); i++) {
            int j = tbl_Enabled_Variables.convertRowIndexToModel(i);
            String key = mVariableModel.getValueAt(j, 2).toString();
            specStream.writeBytes(key);
            specStream.write('\n');
        }
        }
    }

    public HashTableModel getVariableModel () {
        return mVariableModel;
    }

    @Override
    public void propertyChange (PropertyChangeEvent evt) {
        String propertyName = evt.getPropertyName();
        Object newValue = evt.getNewValue();

        if ("enabled".equals(propertyName)) {
            Boolean enabled = (Boolean)newValue;
            cmb_Filter_Column.setEnabled(enabled);
            cmb_Filter_Syntax.setEnabled(enabled);
            lbl_FilterPattern.setEnabled(enabled);
            mFilterColumnLabel.setEnabled(enabled);
            tbl_Available_Variables.setEnabled(enabled);
            tbl_Enabled_Variables.setEnabled(enabled);
            txt_Filter_Pattern.setEnabled(enabled);
        }
    }

    /**
     * This method is called from within the constructor to initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is always * regenerated by the Form Editor.
     */
    @SuppressWarnings("unchecked")
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        pnl_Content = new javax.swing.JPanel();
        pnl_Left = new javax.swing.JPanel();
        scl_Available_Variables = new javax.swing.JScrollPane();
        tbl_Available_Variables = new javax.swing.JTable();
        pnl_Right = new javax.swing.JPanel();
        scl_Enabled_Variables = new javax.swing.JScrollPane();
        tbl_Enabled_Variables = new javax.swing.JTable();
        pnl_Filter = new javax.swing.JPanel();
        pnl_FileterRow1 = new javax.swing.JPanel();
        lbl_FilterPattern = new javax.swing.JLabel();
        cmb_Filter_Syntax = new javax.swing.JComboBox();
        pnl_FilterRow2 = new javax.swing.JPanel();
        txt_Filter_Pattern = new javax.swing.JTextField();
        pnl_FilterRow3 = new javax.swing.JPanel();
        mFilterColumnLabel = new javax.swing.JLabel();
        cmb_Filter_Column = new javax.swing.JComboBox();

        setMinimumSize(new java.awt.Dimension(480, 430));
        setPreferredSize(new java.awt.Dimension(480, 430));
        setLayout(new javax.swing.BoxLayout(this, javax.swing.BoxLayout.LINE_AXIS));

        pnl_Content.setLayout(new javax.swing.BoxLayout(pnl_Content, javax.swing.BoxLayout.X_AXIS));

        pnl_Left.setMinimumSize(new java.awt.Dimension(0, 0));
        pnl_Left.setPreferredSize(new java.awt.Dimension(480, 510));
        pnl_Left.setLayout(new java.awt.BorderLayout());

        scl_Available_Variables.setBackground(new java.awt.Color(255, 255, 255));
        scl_Available_Variables.setOpaque(false);

        tbl_Available_Variables.setAutoCreateRowSorter(true);
        tbl_Available_Variables.setModel(new javax.swing.table.DefaultTableModel(
            new Object [][] {
                {null, null}
            },
            new String [] {
                "Module Name", "Physical Quantity Name"
            }
        ) {
            Class[] types = new Class [] {
                java.lang.String.class, java.lang.String.class
            };
            boolean[] canEdit = new boolean [] {
                false, false
            };

            public Class getColumnClass(int columnIndex) {
                return types [columnIndex];
            }

            public boolean isCellEditable(int rowIndex, int columnIndex) {
                return canEdit [columnIndex];
            }
        });
        tbl_Available_Variables.setFillsViewportHeight(true);
        tbl_Available_Variables.setRequestFocusEnabled(false);
        tbl_Available_Variables.setSelectionMode(javax.swing.ListSelectionModel.SINGLE_SELECTION);
        tbl_Available_Variables.setShowVerticalLines(false);
        scl_Available_Variables.setViewportView(tbl_Available_Variables);

        pnl_Left.add(scl_Available_Variables, java.awt.BorderLayout.CENTER);

        pnl_Content.add(pnl_Left);

        pnl_Right.setMinimumSize(new java.awt.Dimension(0, 0));
        pnl_Right.setLayout(new java.awt.BorderLayout());

        scl_Enabled_Variables.setBorder(javax.swing.BorderFactory.createTitledBorder(javax.swing.BorderFactory.createEtchedBorder(), "Enabled Variables"));
        scl_Enabled_Variables.setOpaque(false);

        tbl_Enabled_Variables.setModel(new javax.swing.table.DefaultTableModel(
            new Object [][] {
                {null, null}
            },
            new String [] {
                "Module Name", "Physical Quantity Name"
            }
        ) {
            Class[] types = new Class [] {
                java.lang.String.class, java.lang.String.class
            };
            boolean[] canEdit = new boolean [] {
                false, false
            };

            public Class getColumnClass(int columnIndex) {
                return types [columnIndex];
            }

            public boolean isCellEditable(int rowIndex, int columnIndex) {
                return canEdit [columnIndex];
            }
        });
        tbl_Enabled_Variables.setFillsViewportHeight(true);
        tbl_Enabled_Variables.setMinimumSize(new java.awt.Dimension(240, 0));
        tbl_Enabled_Variables.setRequestFocusEnabled(false);
        tbl_Enabled_Variables.setSelectionMode(javax.swing.ListSelectionModel.SINGLE_SELECTION);
        tbl_Enabled_Variables.setShowVerticalLines(false);
        tbl_Enabled_Variables.getTableHeader().setReorderingAllowed(false);
        scl_Enabled_Variables.setViewportView(tbl_Enabled_Variables);
        tbl_Enabled_Variables.getColumnModel().getSelectionModel().setSelectionMode(javax.swing.ListSelectionModel.SINGLE_SELECTION);
        if (tbl_Enabled_Variables.getColumnModel().getColumnCount() > 0) {
            tbl_Enabled_Variables.getColumnModel().getColumn(0).setPreferredWidth(40);
        }

        pnl_Right.add(scl_Enabled_Variables, java.awt.BorderLayout.CENTER);

        pnl_Filter.setMaximumSize(new java.awt.Dimension(65536, 65536));
        pnl_Filter.setMinimumSize(new java.awt.Dimension(0, 0));
        pnl_Filter.setPreferredSize(new java.awt.Dimension(480, 90));
        pnl_Filter.setLayout(new javax.swing.BoxLayout(pnl_Filter, javax.swing.BoxLayout.Y_AXIS));

        pnl_FileterRow1.setBorder(javax.swing.BorderFactory.createEmptyBorder(3, 3, 3, 3));
        pnl_FileterRow1.setMinimumSize(new java.awt.Dimension(153, 27));
        pnl_FileterRow1.setPreferredSize(new java.awt.Dimension(153, 27));
        pnl_FileterRow1.setLayout(new java.awt.BorderLayout());

        lbl_FilterPattern.setText("Filter Pattern :");
        lbl_FilterPattern.setMaximumSize(new java.awt.Dimension(115, 15));
        lbl_FilterPattern.setMinimumSize(new java.awt.Dimension(115, 15));
        lbl_FilterPattern.setPreferredSize(new java.awt.Dimension(115, 15));
        pnl_FileterRow1.add(lbl_FilterPattern, java.awt.BorderLayout.WEST);

        cmb_Filter_Syntax.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "Regex expression", "Wildcard", "Fixed String" }));
        cmb_Filter_Syntax.setSelectedIndex(1);
        cmb_Filter_Syntax.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                cmb_Filter_SyntaxActionPerformed(evt);
            }
        });
        pnl_FileterRow1.add(cmb_Filter_Syntax, java.awt.BorderLayout.CENTER);

        pnl_Filter.add(pnl_FileterRow1);

        pnl_FilterRow2.setBorder(javax.swing.BorderFactory.createEmptyBorder(3, 3, 3, 3));
        pnl_FilterRow2.setMinimumSize(new java.awt.Dimension(14, 27));
        pnl_FilterRow2.setPreferredSize(new java.awt.Dimension(480, 27));
        pnl_FilterRow2.setLayout(new java.awt.BorderLayout());

        txt_Filter_Pattern.setText("*");
        txt_Filter_Pattern.addKeyListener(new java.awt.event.KeyAdapter() {
            public void keyPressed(java.awt.event.KeyEvent evt) {
                txt_Filter_PatternKeyPressed(evt);
            }
            public void keyReleased(java.awt.event.KeyEvent evt) {
                txt_Filter_PatternKeyReleased(evt);
            }
            public void keyTyped(java.awt.event.KeyEvent evt) {
                txt_Filter_PatternKeyTyped(evt);
            }
        });
        pnl_FilterRow2.add(txt_Filter_Pattern, java.awt.BorderLayout.CENTER);

        pnl_Filter.add(pnl_FilterRow2);

        pnl_FilterRow3.setBorder(javax.swing.BorderFactory.createEmptyBorder(3, 3, 3, 3));
        pnl_FilterRow3.setMinimumSize(new java.awt.Dimension(205, 27));
        pnl_FilterRow3.setPreferredSize(new java.awt.Dimension(480, 27));
        pnl_FilterRow3.setLayout(new java.awt.BorderLayout());

        mFilterColumnLabel.setText("Filter column :");
        mFilterColumnLabel.setMaximumSize(new java.awt.Dimension(115, 15));
        mFilterColumnLabel.setMinimumSize(new java.awt.Dimension(115, 15));
        mFilterColumnLabel.setPreferredSize(new java.awt.Dimension(115, 15));
        pnl_FilterRow3.add(mFilterColumnLabel, java.awt.BorderLayout.WEST);

        cmb_Filter_Column.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "Physical Quantity Name", "Module Name" }));
        cmb_Filter_Column.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                cmb_Filter_ColumnActionPerformed(evt);
            }
        });
        pnl_FilterRow3.add(cmb_Filter_Column, java.awt.BorderLayout.CENTER);

        pnl_Filter.add(pnl_FilterRow3);

        pnl_Right.add(pnl_Filter, java.awt.BorderLayout.SOUTH);

        pnl_Content.add(pnl_Right);

        add(pnl_Content);
    }// </editor-fold>//GEN-END:initComponents

    private void txt_Filter_PatternKeyPressed(java.awt.event.KeyEvent evt) {//GEN-FIRST:event_txt_Filter_PatternKeyPressed
        variableFilter();
    }//GEN-LAST:event_txt_Filter_PatternKeyPressed
    private void txt_Filter_PatternKeyReleased(java.awt.event.KeyEvent evt) {//GEN-FIRST:event_txt_Filter_PatternKeyReleased
        variableFilter();
    }//GEN-LAST:event_txt_Filter_PatternKeyReleased
    private void txt_Filter_PatternKeyTyped(java.awt.event.KeyEvent evt) {//GEN-FIRST:event_txt_Filter_PatternKeyTyped
        variableFilter();
    }//GEN-LAST:event_txt_Filter_PatternKeyTyped
    private void cmb_Filter_SyntaxActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_cmb_Filter_SyntaxActionPerformed
        variableFilter();
    }//GEN-LAST:event_cmb_Filter_SyntaxActionPerformed
    private void cmb_Filter_ColumnActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_cmb_Filter_ColumnActionPerformed
        variableFilter();
    }//GEN-LAST:event_cmb_Filter_ColumnActionPerformed

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JComboBox cmb_Filter_Column;
    private javax.swing.JComboBox cmb_Filter_Syntax;
    private javax.swing.JLabel lbl_FilterPattern;
    private javax.swing.JLabel mFilterColumnLabel;
    private javax.swing.JPanel pnl_Content;
    private javax.swing.JPanel pnl_FileterRow1;
    private javax.swing.JPanel pnl_Filter;
    private javax.swing.JPanel pnl_FilterRow2;
    private javax.swing.JPanel pnl_FilterRow3;
    private javax.swing.JPanel pnl_Left;
    private javax.swing.JPanel pnl_Right;
    private javax.swing.JScrollPane scl_Available_Variables;
    private javax.swing.JScrollPane scl_Enabled_Variables;
    private javax.swing.JTable tbl_Available_Variables;
    private javax.swing.JTable tbl_Enabled_Variables;
    private javax.swing.JTextField txt_Filter_Pattern;
    // End of variables declaration//GEN-END:variables
}
