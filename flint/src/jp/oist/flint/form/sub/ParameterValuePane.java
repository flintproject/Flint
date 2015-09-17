/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.form.sub;

import jp.oist.flint.desktop.Document;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Insets;
import java.awt.Point;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.KeyEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import javax.swing.AbstractAction;
import javax.swing.BoxLayout;
import javax.swing.DefaultCellEditor;
import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.KeyStroke;
import javax.swing.SwingUtilities;
import javax.swing.event.ChangeEvent;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableColumn;
import javax.swing.table.TableColumnModel;
import jp.oist.flint.form.util.FormulaAutoCompleter;
import jp.oist.flint.parameter.ParameterModel;
import jp.oist.flint.parameter.PhmlParameterModel;
import jp.oist.flint.parameter.SbmlParameterModel;
import jp.oist.flint.phsp.entity.Model;
import jp.oist.flint.phsp.entity.ParameterSet;
import jp.oist.flint.phsp.entity.TargetSet;
import jp.physiome.Ipc;

public class ParameterValuePane extends JPanel 
    implements PropertyChangeListener {

    public final static String ACTION_SEARCH = "parametervaluepane.action.search";

    /* Action Names */
    public final static String ACTION_DEFINE_VALUE_SET = "parametervaluepane.action.define_value_set";

    /* Components Names */
    public final static String DEFINE_VALUE_SET_BUTTON = "parametervaluepane.button.define_value_set";

    private final List<ActionListener> mActionListeners= new ArrayList<>();

    private final Ipc.ModelLanguage mLanguage;

    private ValueEditWindow mValueEditWindow;

    private ActionListener mActionListenerHandler;

    private DefaultCellEditor mCellEditor;

    private final FormulaAutoCompleter mAutoCompleter;

    public ParameterValuePane(Document document) throws IOException {
        super();
        Ipc.ModelProbeResponse response = document.getResponse();
        mLanguage = response.getLanguage();
        addPropertyChangeListener(this);

        initComponents();

        File paramFile = new File(document.getDirectory(), "param");
        File dataFile  = new File(document.getDirectory(), "init");

        ParameterModel model = ParameterModel.factory(mLanguage, paramFile, dataFile);
        tbl_Parameter = new JTable(model);
        tbl_Parameter.setRowHeight(25);

        int editorIndex = -1; 
        switch(mLanguage) {
        case ISML:
            TableColumnModel columnModel = tbl_Parameter.getColumnModel();
            TableColumn column;
            // hide pq-id moudle-id
            column = columnModel.getColumn(PhmlParameterModel.INDEX_PQ_ID);
            tbl_Parameter.removeColumn(column);
            column = columnModel.getColumn(PhmlParameterModel.INDEX_MODULE_ID);
            tbl_Parameter.removeColumn(column);
            editorIndex = PhmlParameterModel.INDEX_EXPRESSION;
            break;
        case SBML:
            editorIndex = SbmlParameterModel.INDEX_EXPRESSION;
            break;
        case UNKNOWN:
        default:
        }

        int columnCount = tbl_Parameter.getColumnCount();
        for (int i=0; i<columnCount; i++)
            tbl_Parameter.getColumnModel()
                    .getColumn(i).setCellRenderer(new StripeTableCellRenderer());

        JTextField editorText = new JTextField();
        mAutoCompleter = new FormulaAutoCompleter(editorText);
        mCellEditor = new DefaultCellEditor(editorText);

        editorText.addFocusListener(new FocusListener() {
            @Override
            public void focusGained(FocusEvent e) {
            }

            @Override
            public void focusLost(FocusEvent e) {
                mCellEditor.stopCellEditing();
            }
        });

        tbl_Parameter.getColumnModel().getColumn(editorIndex).setCellEditor(mCellEditor);
        tbl_Parameter.setAutoCreateRowSorter(true);

        pnl_Table.add(new JScrollPane(tbl_Parameter), BorderLayout.CENTER);

        initSubWindows();

        initEvents();
    }

    private void initSubWindows () {
        mValueEditWindow = new ValueEditWindow();
    }

    private void initEvents () {
        mValueEditWindow.addWindowListener(new WindowAdapter() {
            @Override
            public void windowDeactivated(WindowEvent evt) {
                fireUpdateAutoCompeleter();
            }
        });

        mActionListenerHandler = new ActionListener () {
            @Override
            public void actionPerformed(ActionEvent evt) {
                String actionCommand = evt.getActionCommand();
                if (ACTION_DEFINE_VALUE_SET.equals(actionCommand))
                    defineValueSetActionPerformed(evt);

                fileActionPerformed(evt);
            }
        };

        btn_DefineValueSet.addActionListener(mActionListenerHandler);

        KeyStroke enter = KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, 0);
        KeyStroke up    = KeyStroke.getKeyStroke(KeyEvent.VK_UP,    0);
        KeyStroke down  = KeyStroke.getKeyStroke(KeyEvent.VK_DOWN,  0);

        Object enterActionName = tbl_Parameter
                .getInputMap(JTable.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT).get(enter);
        Object upActionName = tbl_Parameter
                .getInputMap(JTable.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT).get(up);
        Object downActionName = tbl_Parameter
                .getInputMap(JTable.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT).get(down);

        AbstractAction enterAndDownAction = new AbstractAction () {
            @Override
            public void actionPerformed(ActionEvent evt) {
                if (mAutoCompleter.isAutoCompleterVisible())
                    return;
                JTable table = (JTable)evt.getSource();
                if (table.isEditing()) {
                    table.editingStopped(new ChangeEvent("update"));
                } else {
                    int selectedRow    = table.getSelectedRow();
                    int lastRow       = table.getRowCount()-1;
                    int nextRow = Math.min(lastRow, selectedRow+1);
                    table.getSelectionModel().setSelectionInterval(nextRow, nextRow);
                }
            }
        };
        AbstractAction upAction = new AbstractAction () {
            @Override
            public void actionPerformed(ActionEvent evt) {
                if (mAutoCompleter.isAutoCompleterVisible())
                    return;
                JTable table = (JTable)evt.getSource();
                if (table.isEditing()) {
                    table.editingStopped(new ChangeEvent("update"));
                } else {
                    int selectedRow    = table.getSelectedRow();
                    int nextRow = Math.max(0, selectedRow-1);
                    table.getSelectionModel().setSelectionInterval(nextRow, nextRow);
                }
            }
        };
        tbl_Parameter.getActionMap().put(enterActionName, enterAndDownAction);
        tbl_Parameter.getActionMap().put(downActionName,  enterAndDownAction);
        tbl_Parameter.getActionMap().put(upActionName,    upAction);
    }

    @SuppressWarnings("unchecked")
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        pnl_Search = new JPanel();
        pnl_DefineValueSet = new JPanel();
        pnl_DefineValueSetBox = new JPanel();
        btn_DefineValueSet = new JButton();
        pnl_Table = new JPanel();

        setPreferredSize(new Dimension(800, 600));
        setLayout(new BorderLayout());

        pnl_Search.setMaximumSize(new Dimension(32767, 40));
        pnl_Search.setMinimumSize(new Dimension(0, 40));
        pnl_Search.setPreferredSize(new Dimension(0, 40));
        pnl_Search.setLayout(new BoxLayout(pnl_Search, BoxLayout.PAGE_AXIS));

        pnl_DefineValueSet.setMaximumSize(new Dimension(32767, 40));
        pnl_DefineValueSet.setMinimumSize(new Dimension(155, 40));
        pnl_DefineValueSet.setPreferredSize(new Dimension(800, 40));
        pnl_DefineValueSet.setLayout(new FlowLayout(FlowLayout.RIGHT));

        pnl_DefineValueSetBox.setPreferredSize(new Dimension(145, 30));
        pnl_DefineValueSetBox.setLayout(new BoxLayout(pnl_DefineValueSetBox, BoxLayout.LINE_AXIS));

        btn_DefineValueSet.setText("Define value set");
        btn_DefineValueSet.setActionCommand("parametervaluepane.action.define_value_set");
        btn_DefineValueSet.setMargin(new Insets(0, 0, 0, 0));
        btn_DefineValueSet.setMaximumSize(new Dimension(145, 24));
        btn_DefineValueSet.setMinimumSize(new Dimension(145, 24));
        btn_DefineValueSet.setName("parametervaluepane.button.define_value_set"); // NOI18N
        btn_DefineValueSet.setPreferredSize(new Dimension(145, 24));
        pnl_DefineValueSetBox.add(btn_DefineValueSet);

        pnl_DefineValueSet.add(pnl_DefineValueSetBox);

        pnl_Search.add(pnl_DefineValueSet);

        add(pnl_Search, BorderLayout.NORTH);

        pnl_Table.setPreferredSize(new Dimension(800, 500));
        pnl_Table.setLayout(new BorderLayout());
        add(pnl_Table, BorderLayout.CENTER);
    }// </editor-fold>//GEN-END:initComponents

    private void defineValueSetActionPerformed (ActionEvent evt) {
        Window window = SwingUtilities.getWindowAncestor(this);
        Point location = window.getLocationOnScreen();


        mValueEditWindow.setLocation(new Point(location.x+20, location.y+20));
        mValueEditWindow.setVisible(true);
    }

    public ParameterSet getParameterSet () {
        return mValueEditWindow.getParameterSet();
    }

    public TargetSet getTargetSet () {
        TargetSet ts = new TargetSet();
            int rowCount = tbl_Parameter.getRowCount();
            switch (mLanguage) {
                case ISML:
                    PhmlParameterModel model = (PhmlParameterModel)tbl_Parameter.getModel();
                    for (int row=0; row<rowCount; row++) {
                        String moduleId   = model.getModuleIdAt(row);
                        String pqId     = model.getPhysicalQuantityIdAt(row);
                        String value    = model.getExpressionAt(row);

                        TargetSet.Target tt = new TargetSet.Target();
                        tt.setModuleId(moduleId);
                        tt.setPhysicalQuantityId(pqId);
                        tt.setValue(value);
                        ts.add(tt);
                    }
                    break;
                case SBML:
                    for (int row=0; row<rowCount; row++) {
                        String speciesId = (String)tbl_Parameter.getModel().getValueAt(row, 0);
                        String value  = (String)tbl_Parameter.getModel().getValueAt(row, 1);
                        if (speciesId.contains("sbml:")) {
                            int index = speciesId.indexOf("sbml:");
                            speciesId = speciesId.substring(index+"sbml:".length());
                        }
                        TargetSet.Target tt = new TargetSet.Target();
                        tt.setSpeciesId(speciesId);
                        tt.setValue(value);
                        ts.add(tt);
                    }
                    break;
            }
        return ts;
    }

    public void loadParameterAndTarget (Model model) {
        if (model == null || model.getParameterSet().isEmpty())
            return;
        mValueEditWindow.loadParameterSet(model.getParameterSet());
            TargetSet tset = model.getTargetSet();
            for (TargetSet.Target tt : tset.getTargets()) {
                String value = tt.getValue();
                int rowCount = tbl_Parameter.getRowCount();
                switch (model.getModelFormat()) {
                case PHML:
                    PhmlParameterModel phmlModel = (PhmlParameterModel)tbl_Parameter.getModel();
                    String moduleId1 = tt.getModuleId();
                    String pqId1     = tt.getPhysicalQuantityId();
                    for (int row=0; row<rowCount; row++) {
                        String moduleId2 = phmlModel.getModuleIdAt(row);
                        String pqId2 = phmlModel.getPhysicalQuantityIdAt(row);

                        if (moduleId1.equals(moduleId2) && pqId1.equals(pqId2)) {
                            phmlModel.setExpressionAt(row, value);
                            break;
                        }
                    }
                    break;
                case SBML:
                    String speciesId1 = "sbml:"+tt.getSpeciesId();
                    SbmlParameterModel sbmlModel = (SbmlParameterModel)tbl_Parameter.getModel();
                    for (int row=0; row<rowCount; row++) {
                        String speciesId2 = sbmlModel.getSpeciesIdAt(row);
                        if (speciesId1.equals(speciesId2))
                            sbmlModel.setExpressionAt(row, value);
                    }
                    break;
                } 
            }
            fireUpdateAutoCompeleter();
    }

    public void fireUpdateAutoCompeleter () {
        mAutoCompleter.clear();
        ParameterSet ps = mValueEditWindow.getParameterSet();
        for (ParameterSet.Parameter p : ps.getParameters()) {
            if (p.isEmpty())
                continue;

            String enumValue;

            switch (p.getType()) {
            case ENUM: 
                enumValue = p.getEnumValue(); break;
            case INTERVAL:
                enumValue = p.toString(); break;
            default:
                enumValue = "";
            }
            if (enumValue.length() > 30) {
                String s = enumValue.replaceAll("\n", "").substring(0, 27);
                int index = s.lastIndexOf(',');
                enumValue = s.substring(0, index);
                enumValue += "...";
            }

            mAutoCompleter.add(p.getName(), String.format("%s : %s", enumValue, "Value"));
        }
    }

    public void addActionListener (ActionListener l) {
        mActionListeners.add(l);
    }

    public void removeActionListener (ActionListener l) {
        mActionListeners.remove(l);
    }

    public void removeRow (int index) {
        DefaultTableModel model = (DefaultTableModel)tbl_Parameter.getModel();
        if (model == null)
            return;

        model.removeRow(index);
    }

    public void removeAllRows () {
        DefaultTableModel model = (DefaultTableModel)tbl_Parameter.getModel();
        if (model == null)
            return;

        int rowCount = tbl_Parameter.getRowCount();
        for (int i=0; i<rowCount; i++)
            removeRow(i);
    }

    protected void fileActionPerformed (ActionEvent evt) {
        for (ActionListener l : mActionListeners)
            l.actionPerformed(evt);
    }

    @Override
    public void propertyChange (PropertyChangeEvent evt) {
        String propertyName = evt.getPropertyName();
        Object newValue = evt.getNewValue();
        if ("enabled".equals(propertyName)) {
            Boolean enabled = (Boolean)newValue;

            btn_DefineValueSet.setEnabled(enabled);
            tbl_Parameter.setEnabled(enabled);
        }
    }

    private JTable tbl_Parameter;
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private JButton btn_DefineValueSet;
    private JPanel pnl_DefineValueSet;
    private JPanel pnl_DefineValueSetBox;
    private JPanel pnl_Search;
    private JPanel pnl_Table;
    // End of variables declaration//GEN-END:variables

    private static class StripeTableCellRenderer extends DefaultTableCellRenderer {
        @Override
        public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row, int column) {
            Component c = super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column);
            if (!isSelected) {
                boolean isEven = (row%2) == 0;
                Color bgcolor = (isEven)? Color.white : new Color(0xF5, 0xF5, 0xF5);
                c.setBackground(bgcolor);
            }

            return c;
        }
    }
}
