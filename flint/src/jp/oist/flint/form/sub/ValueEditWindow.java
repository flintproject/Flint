/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.form.sub;

import java.awt.BorderLayout;
import java.awt.CardLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.net.URL;
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.HashSet;
import javax.swing.AbstractButton;
import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTable;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.ListSelectionModel;
import javax.swing.WindowConstants;
import javax.swing.border.LineBorder;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.TableCellRenderer;
import javax.swing.text.JTextComponent;
import jp.oist.flint.phsp.entity.ParameterSet;
import jp.oist.flint.util.Randomizer;
import jp.oist.flint.util.Utility;

public class ValueEditWindow extends JFrame
    implements ItemListener, ListSelectionListener, ActionListener,
               WindowListener {

    /* CardLayout Names */
    public final static String CARD_INTERVAL   = "flint.valueeditwindow.card.interval";

    public final static String CARD_ENUM       = "flint.valueeditwindow.card.enum";

    /* ActionEvent Names*/
    public final static String ACTION_ADD_ROW    = "flint.valueeditwindow.action.add_row";

    public final static String ACTION_DELETE_ROW = "flint.valueeditwindow.action.delete_row";

    public final static String ACTION_GENERATE = "flint.valueeditwindow.action.generate";

    public final static String ACTION_CANCEL = "flint.valueeditwindow.action.cancel";

    public final static String ACTION_APPLY = "flint.valueeditwindow.action.apply";

    public final static String ACTION_OK = "flint.valueeditwindow.action.ok";

    /* Component Names */
    public final static String NAME_TEXT = "flint.valueeditwindow.text.name";

    public final static String ENUM_VALUE_TEXT = "flint.valueeditwindow.text.enum.value";

    public final static String RM_SEED_TEXT = "flint.valueeditwindow.text.enum.randomizer.seed";

    public final static String RM_COUNT_TEXT = "flint.valueeditwindow.text.enum.randomizer.count";

    public final static String RM_LOWER_LIMIT_TEXT = "flint.valueeditwindow.text.enum.randomizer.lowerlimit";

    public final static String RM_UPPER_LIMIT_TEXT = "flint.valueeditwindow.text.enum.randomizer.upperlimit";

    public final static String RANGE_LOWER_TEXT = "flint.valueeditwindow.text.interval.rangelower";

    public final static String RANGE_UPPER_TEXT = "flint.valueeditwindow.text.interval.rangeupper";

    public final static String RANGE_STEP_TEXT = "flint.valueeditwindow.text.interval.rangestep";

    public final static String ADD_ROW_BUTTON    = "flint.valueeditwindow.button.add_row";

    public final static String DELETE_ROW_BUTTON = "flint.valueeditwindow.button.delete_row";

    public final static String GENERATE_BUTTON = "flint.valueeditwindow.button.enum.randomizer.generate";

    public final static String OK_BUTTON = "flint.valueeditwindow.button.ok";

    public final static String CANCEL_BUTTON = "flint.valueeditwindow.button.cancel";

    public final static String APPLY_BUTTON = "flint.valueeditwindow.button.apply";

    public final static String RANDOMIZER_CHECKBOX = "flint.valueeditwindow.checkbox.randomizer";

    public final static String ENUM_RADIO_BUTTON = "flint.valueeditwindow.radio.enum";

    public final static String INTERVAL_RADIO_BUTTON = "flint.valueeditwindow.interval.enum";

    private final static Color EVEN_ROW_COLOR = new Color(0xF5, 0xF5, 0xF5);

    private final static Color ODD_ROW_COLOR = Color.WHITE;

    private final static int INDEX_NAME = 0;

    private final static int INDEX_EXPRESSION = 1;

    private final ArrayList<JTextComponent> mTextComponents;

    private final TableCellRenderer  mValueTableCellRenderer;

    private final Randomizer mRandomizer;

    private ParameterSet mOriginalParameterSet;

    private ParameterSet mParameterSet;

    public ValueEditWindow() {
        super();

        URL iconUrl = getClass().getResource("/jp/oist/flint/image/icon.png");
        setIconImage(new ImageIcon(iconUrl).getImage());
        setLocationRelativeTo(getParent());

        addWindowListener(this);

        mValueTableCellRenderer = new ValueTableCellRenderer();
        mParameterSet   = new ParameterSet();
        mOriginalParameterSet = mParameterSet.deepClone();

        mTextComponents = new ArrayList<>();
        mRandomizer = new Randomizer();

        initComponents();
        pack();

        initEvents();
    }

    private void initEvents () {
        btn_AddRow.addActionListener(this);
        btn_DeleteRow.addActionListener(this);
        btn_Generate.addActionListener(this);
        btn_Close.addActionListener(this);
        btn_OK.addActionListener(this);
        btn_Apply.addActionListener(this);

        tbl_Values.getSelectionModel().addListSelectionListener(this);
        Enumeration<AbstractButton> elements = group_ParameterType.getElements();
        while (elements.hasMoreElements()) {
            AbstractButton checkbox = elements.nextElement();
            checkbox.addItemListener(this);
        }

        chk_GenerateRandomValue.addItemListener(new ItemListener () {
            @Override
            public void itemStateChanged(ItemEvent evt) {
                int rowIndex = getSelectedRow();
                String name = ParameterSet.Parameter.USE_RANDOMIZER;

                JCheckBox checkbox = (JCheckBox)evt.getSource();
                boolean isSelected  = checkbox.isSelected();
                ParameterSetTableModel tableModel = (ParameterSetTableModel)tbl_Values.getModel();
                tableModel.setParameterValue(rowIndex, name, isSelected);
                pnl_GenerateRandomValueForm.setVisible(isSelected);
            }
        });

        pnl_GenerateRandomValueForm.setVisible(false);

        mTextComponents.add(txt_Name);
        mTextComponents.add(txt_RangeUpper);
        mTextComponents.add(txt_RangeLower);
        mTextComponents.add(txt_Step);
        mTextComponents.add(txtarea_Value);
        mTextComponents.add(txt_Seed);
        mTextComponents.add(txt_Count);
        mTextComponents.add(txt_LowerLimit);
        mTextComponents.add(txt_UpperLimit);

        for (JTextComponent textComponent : mTextComponents)
            textComponent.getDocument().addDocumentListener(new UpdateDocumentListener(textComponent));

        txt_LowerLimit.setActionCommand(ACTION_GENERATE);
        txt_LowerLimit.addActionListener(this);
        txt_UpperLimit.setActionCommand(ACTION_GENERATE);
        txt_UpperLimit.addActionListener(this);
        txt_Seed.setActionCommand(ACTION_GENERATE);
        txt_Seed.addActionListener(this);
        txt_Count.setActionCommand(ACTION_GENERATE);
        txt_Count.addActionListener(this);

        addRow(new ParameterSet.Parameter());
    }

    @Override
    public void actionPerformed (ActionEvent evt) {
        String actionCommand = evt.getActionCommand();
        if (actionCommand == null)
            return;
        switch (actionCommand) {
        case ACTION_ADD_ROW:
            addRowActionPerformed(evt);
            break;
        case ACTION_DELETE_ROW:
            deleteRowActionPerformed(evt);
            break;
        case ACTION_GENERATE:
            generateActionPerformed(evt);
            break;
        case ACTION_CANCEL:
            cancelActionPerformed(evt);
            break;
        case ACTION_OK:
            okActionPerformed(evt);
            break;
        case ACTION_APPLY:
            applyActionPerformed(evt);
            break;
        }
    }

    @Override
    public void itemStateChanged(ItemEvent evt) {
        if (ItemEvent.SELECTED != evt.getStateChange())
            return;
        JRadioButton radioBtn = (JRadioButton)evt.getSource();
        String cardName = null;

        ParameterSetTableModel tableModel = (ParameterSetTableModel)tbl_Values.getModel();
        int rowIndex = getSelectedRow();
        if (chk_Interval.equals(radioBtn)) {
            cardName = CARD_INTERVAL;
            tableModel.setParameterValue(rowIndex, 
                    ParameterSet.Parameter.TYPE, ParameterSet.ParameterType.INTERVAL);
        } else {
            cardName = CARD_ENUM;
            tableModel.setParameterValue(rowIndex, 
                    ParameterSet.Parameter.TYPE, ParameterSet.ParameterType.ENUM);
        }
        if (cardName == null)
            return;
        CardLayout layout =(CardLayout)pnl_EditorPane.getLayout();
        layout.show(pnl_EditorPane, cardName);
    }

    @Override
    public void valueChanged(ListSelectionEvent evt) {
        int selectedRow = getSelectedRow();
        if (evt.getValueIsAdjusting() || selectedRow == -1) 
            return;

        ParameterSetTableModel tableModel = (ParameterSetTableModel)tbl_Values.getModel();
        ParameterSet.Parameter p = tableModel.getRow(selectedRow);

        updateComponents(p);
    }

    private String getParameterKey(String name) {
        if (name == null)
            return null;
        switch (name) {
        case NAME_TEXT:
            return ParameterSet.Parameter.NAME;
        case ENUM_VALUE_TEXT:
            return ParameterSet.Parameter.ENUM_VALUE;
        case RM_COUNT_TEXT:
            return ParameterSet.Parameter.RANDOMIZER_COUNT;
        case RM_SEED_TEXT:
            return ParameterSet.Parameter.RANDOMIZER_SEED;
        case RM_LOWER_LIMIT_TEXT:
            return ParameterSet.Parameter.RANDOMIZER_LOWER_LIMIT;
        case RM_UPPER_LIMIT_TEXT:
            return ParameterSet.Parameter.RANDOMIZER_UPPER_LIMIT;
        case RANGE_LOWER_TEXT:
            return ParameterSet.Parameter.RANGE_LOWER;
        case RANGE_UPPER_TEXT:
            return ParameterSet.Parameter.RANGE_UPPER;
        case RANGE_STEP_TEXT:
            return ParameterSet.Parameter.RANGE_STEP;
        }
        return null;
    }

    private void updateComponents (ParameterSet.Parameter p) {

        for (JTextComponent textComponent : mTextComponents) {
            String key = getParameterKey(textComponent.getName());
            Object value = p.get(key);
            textComponent.setText((value == null)? "" : value.toString());
        }

        ParameterSet.ParameterType type = p.getType();
        Boolean useRandomizer = (Boolean)p.get(ParameterSet.Parameter.USE_RANDOMIZER);
        if (useRandomizer == null)
            useRandomizer = Boolean.FALSE;
        chk_GenerateRandomValue.setSelected(useRandomizer);

        split_EditorAndTable.validate();
        split_EditorAndTable.repaint();

        switch (type) {
        case ENUM: 
            chk_Enum.setSelected(true); 
            break;
        case INTERVAL: 
            chk_Interval.setSelected(true); 
            break;
        }
    }

    public void loadParameterSet (ParameterSet pset) {
        int length = pset.getParameters().length;
        tbl_Values.getSelectionModel().clearSelection();
        mParameterSet.removeAll();
        if (length > 0) {
            for (ParameterSet.Parameter pp : pset.getParameters())
                mParameterSet.add(pp);
        } else {
            mParameterSet.add(new ParameterSet.Parameter());
        }

        mOriginalParameterSet = mParameterSet.deepClone();
        tbl_Values.getSelectionModel().setSelectionInterval(0,0);
    }

    public void validateForms () throws Exception {
        cosmetic();

        int rowCount = tbl_Values.getRowCount();
        ParameterSetTableModel model = (ParameterSetTableModel)tbl_Values.getModel();
        String name;
        String expression;
        int row;

        HashSet<String> names = new HashSet<>();

        for (row=0; row<rowCount; row++) {
            name = (String)model.getValueAt(row, INDEX_NAME);
            expression = (String)model.getValueAt(row, INDEX_EXPRESSION);

            if ((name == null || name.trim().isEmpty())
                    && (expression == null || expression.trim().isEmpty()))
                continue;

            ParameterSet.Parameter p = model.getParameterByName(name);
            try {
                p.validateName();
            } catch (Exception ex) {
                String msg = String.format("Invalid parameter name '%s' in row number %d", name, row+1);
                throw new Exception(msg);
            }

            try {
                p.validateValue();
            } catch (Exception ex) {
                String msg = String.format("Invalid value in row number %d", row+1);
                throw new Exception(msg);
            }

            if (!names.add(name)) {
                String msg = String.format("Duplicate \"%s\" exists ", name);
                throw new Exception(msg);
            }
        }
    }

    public void close () {
        ParameterSetTableModel model = (ParameterSetTableModel)tbl_Values.getModel();

        mParameterSet = mOriginalParameterSet.deepClone();
        model.setPamraterSet(mParameterSet);

        setVisible(false);
        if (mParameterSet.isEmpty())
            mParameterSet.add(new ParameterSet.Parameter());

        int rowIndex = 0;
        tbl_Values.getSelectionModel().setSelectionInterval(rowIndex, rowIndex);

        ParameterSet.Parameter p = mParameterSet.get(rowIndex);

        updateComponents(p);
        btn_Apply.setEnabled(false);
    }

    @SuppressWarnings("unchecked")
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        group_ParameterType = new ButtonGroup();
        pnl_Content = new JPanel();
        split_EditorAndTable = new JSplitPane();
        pnl_ValuesPane = new JPanel();
        pnl_ControlRow = new JPanel();
        btn_AddRow = new JButton();
        btn_DeleteRow = new JButton();
        scl_Values = new JScrollPane();
        tbl_Values = new JTable();
        pnl_Wrapper = new JPanel();
        pnl_Name = new JPanel();
        lbl_Name = new JLabel();
        txt_Name = new JTextField();
        pnl_Type = new JPanel();
        lbl_Type = new JLabel();
        chk_Enum = new JRadioButton();
        chk_Interval = new JRadioButton();
        pnl_Editor = new JPanel();
        pnl_EditorPane = new JPanel();
        pnl_EnumEditor = new JPanel();
        pnl_EnumEditorContent = new JPanel();
        pnl_ValueRow = new JPanel();
        lbl_Value = new JLabel();
        pnl_ValueFormRow = new JPanel();
        scl_Value = new JScrollPane();
        txtarea_Value = new JTextArea();
        pnl_ChkGenerateRandomValueRow = new JPanel();
        chk_GenerateRandomValue = new JCheckBox();
        pnl_GenerateRandomValue = new JPanel();
        pnl_GenerateRandomValueForm = new JPanel();
        pnl_SeedAndCountRow = new JPanel();
        pnl_SeedRow = new JPanel();
        lbl_Seed = new JLabel();
        txt_Seed = new JTextField();
        pnl_CountRow = new JPanel();
        lbl_Count = new JLabel();
        txt_Count = new JTextField();
        pnl_LimitRow = new JPanel();
        pnl_LowerLimitRow = new JPanel();
        lbl_LowerLimit = new JLabel();
        txt_LowerLimit = new JTextField();
        pnl_UpperLimitRow = new JPanel();
        lbl_UpperLimit = new JLabel();
        txt_UpperLimit = new JTextField();
        pnl_Generate = new JPanel();
        btn_Generate = new JButton();
        pnl_IntervalEditor = new JPanel();
        pnl_IntervalEditorContent = new JPanel();
        pnl_RangeLowerRow = new JPanel();
        lbl_RangeLower = new JLabel();
        txt_RangeLower = new JTextField();
        pnl_RangeUpperRow = new JPanel();
        lbl_RangeUpper = new JLabel();
        txt_RangeUpper = new JTextField();
        pnl_StepRow = new JPanel();
        lbl_Step = new JLabel();
        txt_Step = new JTextField();
        pnl_Foot = new JPanel();
        btn_OK = new JButton();
        btn_Apply = new JButton();
        btn_Close = new JButton();

        setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);
        setTitle("Define value set");
        setPreferredSize(new Dimension(770, 600));

        pnl_Content.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
        pnl_Content.setAlignmentX(0.0F);
        pnl_Content.setAlignmentY(0.0F);
        pnl_Content.setLayout(new BoxLayout(pnl_Content, BoxLayout.PAGE_AXIS));

        split_EditorAndTable.setBorder(null);
        split_EditorAndTable.setDividerLocation(130);
        split_EditorAndTable.setOrientation(JSplitPane.VERTICAL_SPLIT);
        split_EditorAndTable.setMinimumSize(new Dimension(0, 0));
        split_EditorAndTable.setPreferredSize(new Dimension(0, 0));

        pnl_ValuesPane.setBorder(BorderFactory.createTitledBorder("Values"));
        pnl_ValuesPane.setMinimumSize(new Dimension(0, 150));
        pnl_ValuesPane.setOpaque(false);
        pnl_ValuesPane.setPreferredSize(new Dimension(570, 150));
        pnl_ValuesPane.setLayout(new BorderLayout());

        pnl_ControlRow.setMaximumSize(new Dimension(32767, 30));
        pnl_ControlRow.setMinimumSize(new Dimension(63, 30));
        pnl_ControlRow.setPreferredSize(new Dimension(100, 30));
        pnl_ControlRow.setLayout(new FlowLayout(FlowLayout.RIGHT, 5, 3));

        btn_AddRow.setText("+");
        btn_AddRow.setActionCommand("flint.valueeditwindow.action.add_row");
        btn_AddRow.setMaximumSize(new Dimension(24, 24));
        btn_AddRow.setMinimumSize(new Dimension(24, 24));
        btn_AddRow.setName("flint.valueeditwindow.button.add_row"); // NOI18N
        btn_AddRow.setPreferredSize(new Dimension(24, 24));
        pnl_ControlRow.add(btn_AddRow);

        btn_DeleteRow.setText("-");
        btn_DeleteRow.setActionCommand("flint.valueeditwindow.action.delete_row");
        btn_DeleteRow.setMaximumSize(new Dimension(24, 24));
        btn_DeleteRow.setMinimumSize(new Dimension(24, 24));
        btn_DeleteRow.setName("flint.valueeditwindow.button.delete_row"); // NOI18N
        btn_DeleteRow.setPreferredSize(new Dimension(24, 24));
        pnl_ControlRow.add(btn_DeleteRow);

        pnl_ValuesPane.add(pnl_ControlRow, BorderLayout.SOUTH);

        tbl_Values.setModel(new ParameterSetTableModel(mParameterSet));
        tbl_Values.setDoubleBuffered(true);
        tbl_Values.setEditingColumn(3);
        tbl_Values.setEditingRow(3);
        tbl_Values.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
        tbl_Values.getTableHeader().setReorderingAllowed(false);
        scl_Values.setViewportView(tbl_Values);
        tbl_Values.getColumnModel().getSelectionModel().setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
        if (tbl_Values.getColumnModel().getColumnCount() > 0) {
            tbl_Values.getColumnModel().getColumn(0).setCellRenderer(mValueTableCellRenderer);
            tbl_Values.getColumnModel().getColumn(1).setCellRenderer(mValueTableCellRenderer);
        }

        pnl_ValuesPane.add(scl_Values, BorderLayout.CENTER);

        split_EditorAndTable.setTopComponent(pnl_ValuesPane);

        pnl_Wrapper.setPreferredSize(new Dimension(200, 110));
        pnl_Wrapper.setLayout(new BoxLayout(pnl_Wrapper, BoxLayout.PAGE_AXIS));

        pnl_Name.setMaximumSize(new Dimension(2147483647, 20));
        pnl_Name.setMinimumSize(new Dimension(0, 0));
        pnl_Name.setPreferredSize(new Dimension(790, 30));
        pnl_Name.setLayout(new BoxLayout(pnl_Name, BoxLayout.LINE_AXIS));

        lbl_Name.setText("Name :");
        lbl_Name.setPreferredSize(new Dimension(60, 16));
        pnl_Name.add(lbl_Name);

        txt_Name.setMaximumSize(new Dimension(2147483647, 24));
        txt_Name.setMinimumSize(new Dimension(14, 24));
        txt_Name.setName(NAME_TEXT);
        txt_Name.setPreferredSize(new Dimension(0, 24));
        pnl_Name.add(txt_Name);

        pnl_Wrapper.add(pnl_Name);

        pnl_Type.setMaximumSize(new Dimension(32767, 30));
        pnl_Type.setMinimumSize(new Dimension(0, 0));
        pnl_Type.setPreferredSize(new Dimension(790, 30));
        FlowLayout flowLayout1 = new FlowLayout(FlowLayout.LEFT, 0, 0);
        flowLayout1.setAlignOnBaseline(true);
        pnl_Type.setLayout(flowLayout1);

        lbl_Type.setText("Type :");
        lbl_Type.setPreferredSize(new Dimension(60, 16));
        pnl_Type.add(lbl_Type);

        group_ParameterType.add(chk_Enum);
        chk_Enum.setSelected(true);
        chk_Enum.setText("Enum");
        chk_Enum.setName(ENUM_RADIO_BUTTON);
        pnl_Type.add(chk_Enum);

        group_ParameterType.add(chk_Interval);
        chk_Interval.setText("Interval");
        chk_Interval.setName(INTERVAL_RADIO_BUTTON);
        pnl_Type.add(chk_Interval);

        pnl_Wrapper.add(pnl_Type);

        pnl_Editor.setLayout(new BorderLayout());

        pnl_EditorPane.setAlignmentY(0.0F);
        pnl_EditorPane.setMinimumSize(new Dimension(0, 0));
        pnl_EditorPane.setPreferredSize(new Dimension(790, 50));
        pnl_EditorPane.setLayout(new CardLayout());

        pnl_EnumEditor.setBorder(BorderFactory.createTitledBorder("Enum Value"));
        pnl_EnumEditor.setMinimumSize(new Dimension(0, 0));
        pnl_EnumEditor.setPreferredSize(new Dimension(790, 0));
        pnl_EnumEditor.setLayout(new BorderLayout());

        pnl_EnumEditorContent.setBorder(BorderFactory.createEmptyBorder(0, 5, 5, 5));
        pnl_EnumEditorContent.setAlignmentY(0.0F);
        pnl_EnumEditorContent.setMinimumSize(new Dimension(0, 0));
        pnl_EnumEditorContent.setPreferredSize(new Dimension(800, 0));
        pnl_EnumEditorContent.setLayout(new BoxLayout(pnl_EnumEditorContent, BoxLayout.PAGE_AXIS));

        pnl_ValueRow.setMinimumSize(new Dimension(100, 20));
        pnl_ValueRow.setPreferredSize(new Dimension(100, 20));
        FlowLayout flowLayout2 = new FlowLayout(FlowLayout.LEFT, 0, 0);
        flowLayout2.setAlignOnBaseline(true);
        pnl_ValueRow.setLayout(flowLayout2);

        lbl_Value.setText("Value :");
        pnl_ValueRow.add(lbl_Value);

        pnl_EnumEditorContent.add(pnl_ValueRow);

        pnl_ValueFormRow.setBorder(BorderFactory.createEmptyBorder(0, 15, 0, 15));
        pnl_ValueFormRow.setMinimumSize(new Dimension(0, 70));
        pnl_ValueFormRow.setPreferredSize(new Dimension(0, 70));
        pnl_ValueFormRow.setLayout(new BoxLayout(pnl_ValueFormRow, BoxLayout.LINE_AXIS));

        scl_Value.setMinimumSize(new Dimension(0, 0));
        scl_Value.setPreferredSize(new Dimension(0, 0));

        txtarea_Value.setColumns(20);
        txtarea_Value.setRows(2);
        txtarea_Value.setTabSize(4);
        txtarea_Value.setMinimumSize(new Dimension(0, 0));
        txtarea_Value.setName(ENUM_VALUE_TEXT);
        scl_Value.setViewportView(txtarea_Value);

        pnl_ValueFormRow.add(scl_Value);

        pnl_EnumEditorContent.add(pnl_ValueFormRow);

        pnl_ChkGenerateRandomValueRow.setAlignmentY(0.0F);
        pnl_ChkGenerateRandomValueRow.setMaximumSize(new Dimension(32767, 25));
        pnl_ChkGenerateRandomValueRow.setMinimumSize(new Dimension(183, 25));
        pnl_ChkGenerateRandomValueRow.setPreferredSize(new Dimension(183, 25));
        pnl_ChkGenerateRandomValueRow.setLayout(new FlowLayout(FlowLayout.LEFT, 0, 0));

        chk_GenerateRandomValue.setText("Generate random values");
        chk_GenerateRandomValue.setName(RANDOMIZER_CHECKBOX);
        pnl_ChkGenerateRandomValueRow.add(chk_GenerateRandomValue);

        pnl_EnumEditorContent.add(pnl_ChkGenerateRandomValueRow);

        pnl_GenerateRandomValue.setMinimumSize(new Dimension(0, 0));
        pnl_GenerateRandomValue.setPreferredSize(new Dimension(790, 0));
        pnl_GenerateRandomValue.setLayout(new BorderLayout());

        pnl_GenerateRandomValueForm.setBorder(BorderFactory.createTitledBorder(new LineBorder(new Color(204, 204, 204), 1, true), "Generate Random Value"));
        pnl_GenerateRandomValueForm.setAlignmentX(0.0F);
        pnl_GenerateRandomValueForm.setAlignmentY(0.0F);
        pnl_GenerateRandomValueForm.setMinimumSize(new Dimension(0, 0));
        pnl_GenerateRandomValueForm.setPreferredSize(new Dimension(0, 0));
        pnl_GenerateRandomValueForm.setLayout(new BoxLayout(pnl_GenerateRandomValueForm, BoxLayout.PAGE_AXIS));

        pnl_SeedAndCountRow.setMaximumSize(new Dimension(2147483647, 36));
        pnl_SeedAndCountRow.setMinimumSize(new Dimension(0, 0));
        pnl_SeedAndCountRow.setPreferredSize(new Dimension(0, 36));
        pnl_SeedAndCountRow.setLayout(new BoxLayout(pnl_SeedAndCountRow, BoxLayout.LINE_AXIS));

        pnl_SeedRow.setBorder(BorderFactory.createEmptyBorder(5, 10, 5, 10));
        pnl_SeedRow.setMaximumSize(new Dimension(2147483647, 36));
        pnl_SeedRow.setMinimumSize(new Dimension(37, 36));
        pnl_SeedRow.setPreferredSize(new Dimension(100, 36));
        pnl_SeedRow.setLayout(new BorderLayout());

        lbl_Seed.setText("Seed :");
        lbl_Seed.setMaximumSize(new Dimension(95, 13));
        lbl_Seed.setMinimumSize(new Dimension(95, 13));
        lbl_Seed.setPreferredSize(new Dimension(95, 16));
        pnl_SeedRow.add(lbl_Seed, BorderLayout.WEST);

        txt_Seed.setMaximumSize(new Dimension(2147483647, 28));
        txt_Seed.setMinimumSize(new Dimension(4, 28));
        txt_Seed.setName(RM_SEED_TEXT);
        txt_Seed.setPreferredSize(new Dimension(4, 28));
        pnl_SeedRow.add(txt_Seed, BorderLayout.CENTER);

        pnl_SeedAndCountRow.add(pnl_SeedRow);

        pnl_CountRow.setBorder(BorderFactory.createEmptyBorder(5, 10, 5, 10));
        pnl_CountRow.setMaximumSize(new Dimension(2147483647, 36));
        pnl_CountRow.setMinimumSize(new Dimension(60, 36));
        pnl_CountRow.setPreferredSize(new Dimension(100, 36));
        pnl_CountRow.setLayout(new BorderLayout());

        lbl_Count.setText("Count :");
        lbl_Count.setMaximumSize(new Dimension(95, 13));
        lbl_Count.setMinimumSize(new Dimension(95, 13));
        lbl_Count.setPreferredSize(new Dimension(95, 16));
        pnl_CountRow.add(lbl_Count, BorderLayout.WEST);

        txt_Count.setMaximumSize(new Dimension(2147483647, 28));
        txt_Count.setMinimumSize(new Dimension(4, 28));
        txt_Count.setName(RM_COUNT_TEXT);
        txt_Count.setPreferredSize(new Dimension(4, 28));
        pnl_CountRow.add(txt_Count, BorderLayout.CENTER);

        pnl_SeedAndCountRow.add(pnl_CountRow);

        pnl_GenerateRandomValueForm.add(pnl_SeedAndCountRow);

        pnl_LimitRow.setMaximumSize(new Dimension(2147483647, 36));
        pnl_LimitRow.setMinimumSize(new Dimension(0, 36));
        pnl_LimitRow.setPreferredSize(new Dimension(0, 36));
        pnl_LimitRow.setLayout(new BoxLayout(pnl_LimitRow, BoxLayout.LINE_AXIS));

        pnl_LowerLimitRow.setBorder(BorderFactory.createEmptyBorder(5, 10, 5, 10));
        pnl_LowerLimitRow.setMaximumSize(new Dimension(2147483647, 36));
        pnl_LowerLimitRow.setMinimumSize(new Dimension(95, 36));
        pnl_LowerLimitRow.setPreferredSize(new Dimension(100, 36));
        pnl_LowerLimitRow.setLayout(new BorderLayout());

        lbl_LowerLimit.setText("Lower Limit :");
        lbl_LowerLimit.setMaximumSize(new Dimension(95, 13));
        lbl_LowerLimit.setMinimumSize(new Dimension(95, 13));
        lbl_LowerLimit.setPreferredSize(new Dimension(95, 16));
        pnl_LowerLimitRow.add(lbl_LowerLimit, BorderLayout.WEST);

        txt_LowerLimit.setMaximumSize(new Dimension(2147483647, 28));
        txt_LowerLimit.setMinimumSize(new Dimension(4, 28));
        txt_LowerLimit.setName(RM_LOWER_LIMIT_TEXT);
        txt_LowerLimit.setPreferredSize(new Dimension(4, 28));
        pnl_LowerLimitRow.add(txt_LowerLimit, BorderLayout.CENTER);

        pnl_LimitRow.add(pnl_LowerLimitRow);

        pnl_UpperLimitRow.setBorder(BorderFactory.createEmptyBorder(5, 10, 5, 10));
        pnl_UpperLimitRow.setMaximumSize(new Dimension(2147483647, 36));
        pnl_UpperLimitRow.setMinimumSize(new Dimension(95, 36));
        pnl_UpperLimitRow.setPreferredSize(new Dimension(100, 36));
        pnl_UpperLimitRow.setLayout(new BorderLayout());

        lbl_UpperLimit.setText("Upper Limit :");
        lbl_UpperLimit.setMaximumSize(new Dimension(95, 13));
        lbl_UpperLimit.setMinimumSize(new Dimension(95, 13));
        lbl_UpperLimit.setPreferredSize(new Dimension(95, 16));
        pnl_UpperLimitRow.add(lbl_UpperLimit, BorderLayout.WEST);

        txt_UpperLimit.setMaximumSize(new Dimension(2147483647, 28));
        txt_UpperLimit.setMinimumSize(new Dimension(4, 28));
        txt_UpperLimit.setName(RM_UPPER_LIMIT_TEXT);
        txt_UpperLimit.setPreferredSize(new Dimension(4, 28));
        pnl_UpperLimitRow.add(txt_UpperLimit, BorderLayout.CENTER);

        pnl_LimitRow.add(pnl_UpperLimitRow);

        pnl_GenerateRandomValueForm.add(pnl_LimitRow);

        pnl_Generate.setMaximumSize(new Dimension(32767, 36));
        pnl_Generate.setMinimumSize(new Dimension(10, 36));
        pnl_Generate.setPreferredSize(new Dimension(100, 36));
        pnl_Generate.setLayout(new FlowLayout(FlowLayout.RIGHT, 10, 5));

        btn_Generate.setText("Generate");
        btn_Generate.setActionCommand("flint.valueeditwindow.action.generate");
        btn_Generate.setName("flint.valueeditwindow.button.enum.randomizer.generate"); // NOI18N
        pnl_Generate.add(btn_Generate);

        pnl_GenerateRandomValueForm.add(pnl_Generate);

        pnl_GenerateRandomValue.add(pnl_GenerateRandomValueForm, BorderLayout.CENTER);

        pnl_EnumEditorContent.add(pnl_GenerateRandomValue);

        pnl_EnumEditor.add(pnl_EnumEditorContent, BorderLayout.CENTER);

        pnl_EditorPane.add(pnl_EnumEditor, "flint.valueeditwindow.card.enum");

        pnl_IntervalEditor.setBorder(BorderFactory.createTitledBorder("Interval Value"));
        pnl_IntervalEditor.setMinimumSize(new Dimension(0, 0));
        pnl_IntervalEditor.setPreferredSize(new Dimension(790, 0));
        pnl_IntervalEditor.setLayout(new BoxLayout(pnl_IntervalEditor, BoxLayout.LINE_AXIS));

        pnl_IntervalEditorContent.setAlignmentY(0.0F);
        pnl_IntervalEditorContent.setMaximumSize(new Dimension(2147483647, 2147483647));
        pnl_IntervalEditorContent.setMinimumSize(new Dimension(0, 0));
        pnl_IntervalEditorContent.setPreferredSize(new Dimension(0, 0));
        pnl_IntervalEditorContent.setLayout(new BoxLayout(pnl_IntervalEditorContent, BoxLayout.PAGE_AXIS));

        pnl_RangeLowerRow.setMaximumSize(new Dimension(2147483647, 36));
        pnl_RangeLowerRow.setMinimumSize(new Dimension(37, 36));
        pnl_RangeLowerRow.setPreferredSize(new Dimension(100, 36));
        pnl_RangeLowerRow.setLayout(new BoxLayout(pnl_RangeLowerRow, BoxLayout.LINE_AXIS));

        lbl_RangeLower.setText("Range Lower :");
        lbl_RangeLower.setMaximumSize(new Dimension(120, 13));
        lbl_RangeLower.setMinimumSize(new Dimension(120, 13));
        lbl_RangeLower.setPreferredSize(new Dimension(120, 16));
        pnl_RangeLowerRow.add(lbl_RangeLower);

        txt_RangeLower.setMaximumSize(new Dimension(2147483647, 28));
        txt_RangeLower.setMinimumSize(new Dimension(4, 28));
        txt_RangeLower.setName(RANGE_LOWER_TEXT);
        txt_RangeLower.setPreferredSize(new Dimension(4, 28));
        pnl_RangeLowerRow.add(txt_RangeLower);

        pnl_IntervalEditorContent.add(pnl_RangeLowerRow);

        pnl_RangeUpperRow.setMaximumSize(new Dimension(2147483647, 40));
        pnl_RangeUpperRow.setMinimumSize(new Dimension(60, 40));
        pnl_RangeUpperRow.setPreferredSize(new Dimension(100, 40));
        pnl_RangeUpperRow.setLayout(new BoxLayout(pnl_RangeUpperRow, BoxLayout.LINE_AXIS));

        lbl_RangeUpper.setText("Range Upper :");
        lbl_RangeUpper.setMaximumSize(new Dimension(120, 13));
        lbl_RangeUpper.setMinimumSize(new Dimension(120, 13));
        lbl_RangeUpper.setPreferredSize(new Dimension(120, 16));
        pnl_RangeUpperRow.add(lbl_RangeUpper);

        txt_RangeUpper.setMaximumSize(new Dimension(2147483647, 28));
        txt_RangeUpper.setMinimumSize(new Dimension(4, 28));
        txt_RangeUpper.setName(RANGE_UPPER_TEXT);
        txt_RangeUpper.setPreferredSize(new Dimension(4, 28));
        pnl_RangeUpperRow.add(txt_RangeUpper);

        pnl_IntervalEditorContent.add(pnl_RangeUpperRow);

        pnl_StepRow.setMaximumSize(new Dimension(2147483647, 36));
        pnl_StepRow.setMinimumSize(new Dimension(95, 36));
        pnl_StepRow.setPreferredSize(new Dimension(100, 36));
        pnl_StepRow.setLayout(new BoxLayout(pnl_StepRow, BoxLayout.LINE_AXIS));

        lbl_Step.setText("Step :");
        lbl_Step.setMaximumSize(new Dimension(120, 13));
        lbl_Step.setMinimumSize(new Dimension(120, 13));
        lbl_Step.setPreferredSize(new Dimension(120, 16));
        pnl_StepRow.add(lbl_Step);

        txt_Step.setMaximumSize(new Dimension(2147483647, 28));
        txt_Step.setMinimumSize(new Dimension(4, 28));
        txt_Step.setName(RANGE_STEP_TEXT);
        txt_Step.setPreferredSize(new Dimension(4, 28));
        pnl_StepRow.add(txt_Step);

        pnl_IntervalEditorContent.add(pnl_StepRow);

        pnl_IntervalEditor.add(pnl_IntervalEditorContent);

        pnl_EditorPane.add(pnl_IntervalEditor, "flint.valueeditwindow.card.interval");

        pnl_Editor.add(pnl_EditorPane, BorderLayout.CENTER);

        pnl_Wrapper.add(pnl_Editor);

        split_EditorAndTable.setBottomComponent(pnl_Wrapper);

        pnl_Content.add(split_EditorAndTable);

        getContentPane().add(pnl_Content, BorderLayout.CENTER);

        pnl_Foot.setLayout(new FlowLayout(FlowLayout.RIGHT));

        btn_OK.setText("OK");
        btn_OK.setActionCommand("flint.valueeditwindow.action.ok");
        btn_OK.setName("flint.valueeditwindow.button.ok"); // NOI18N
        pnl_Foot.add(btn_OK);

        btn_Apply.setText("Apply");
        btn_Apply.setActionCommand("flint.valueeditwindow.action.apply");
        btn_Apply.setName("flint.valueeditwindow.button.apply"); // NOI18N
        pnl_Foot.add(btn_Apply);

        btn_Close.setText("Cancel");
        btn_Close.setActionCommand("flint.valueeditwindow.action.cancel");
        btn_Close.setName("flint.valueeditwindow.button.cancel"); // NOI18N
        pnl_Foot.add(btn_Close);

        getContentPane().add(pnl_Foot, BorderLayout.SOUTH);
    }// </editor-fold>//GEN-END:initComponents

    private void addRowActionPerformed (ActionEvent evt) {
        addRow(new ParameterSet.Parameter());
    }

    private void deleteRowActionPerformed (ActionEvent evt) {
        int rowIndex = getSelectedRow();
        removeRow(rowIndex);
    }

    private void generateActionPerformed (ActionEvent evt) {
        String sSeed = txt_Seed.getText();
        String sMinimum = txt_LowerLimit.getText();
        String sMaximum = txt_UpperLimit.getText();
        String sCount   = txt_Count.getText();

        if (sSeed == null || sSeed.trim().isEmpty())
            sSeed = String.valueOf(System.currentTimeMillis());

        try {
            Long seed  = Long.parseLong(sSeed);
            Long count = Long.parseLong(sCount);
            Double minimum = Double.parseDouble(sMinimum);
            Double maximum = Double.parseDouble(sMaximum);

            NumberFormat numberFormat = NumberFormat.getIntegerInstance();
            numberFormat.setMinimumFractionDigits(
                    Math.max(Utility.getDecimalPlace(sMinimum), Utility.getDecimalPlace(sMaximum)));
            numberFormat.setGroupingUsed(false);

            mRandomizer.setSeed(seed);
            mRandomizer.setMinimum(minimum);
            mRandomizer.setMaximum(maximum);
            mRandomizer.setNumberFormat(numberFormat);
            String enumValue = mRandomizer.generateAsString(count, ",");

            txtarea_Value.setText(enumValue);
        } catch (NumberFormatException ex) { }
    }

    private void cancelActionPerformed (ActionEvent evt) {
        close();
    }

    private void okActionPerformed (ActionEvent evt) {
        try {
            applyFormValues();
            close();
        } catch (Exception ex) {
            String msg = ex.getMessage() + System.getProperty("line.separator");
            JOptionPane.showMessageDialog(this, msg, "ERROR", JOptionPane.ERROR_MESSAGE);
        }
    }

    private void applyActionPerformed (ActionEvent evt) {
        try {
            applyFormValues();
        } catch (Exception ex) {
            JOptionPane.showMessageDialog(this, ex.getMessage(), "ERROR", JOptionPane.ERROR_MESSAGE);
        }
    }

    private void applyFormValues () throws Exception {
        cosmetic();
        validateForms();
        mOriginalParameterSet = mParameterSet.deepClone();
        btn_Apply.setEnabled(false);
    }

    private void updateDocument (DocumentEvent evt) {
        JTextComponent source = (JTextComponent)evt.getDocument().getProperty("source");

        String key = getParameterKey(source.getName());
        String value = source.getText();

        if (key == null)
            return;

        int selectedRow = getSelectedRow();
        ParameterSetTableModel tableModel = (ParameterSetTableModel)tbl_Values.getModel();
        tableModel.setParameterValue(selectedRow, key, value);

        btn_Apply.setEnabled(true);
    }

    public void addRow (ParameterSet.Parameter parameter) {
        int lastIndex = mParameterSet.size();
        ParameterSetTableModel tableModel = (ParameterSetTableModel)tbl_Values.getModel();
        tableModel.addRow(parameter);

        tbl_Values.getSelectionModel().setSelectionInterval(lastIndex, lastIndex);
    }

    public void removeRow (int rowIndex) {
        ParameterSetTableModel tableModel = (ParameterSetTableModel)tbl_Values.getModel();
        tableModel.removeRow(rowIndex);

        if (tableModel.getRowCount() == 0)
            tableModel.addRow(new ParameterSet.Parameter("", ""));

        int selectedRow = Math.max(0, rowIndex-1);
        tbl_Values.getSelectionModel().setSelectionInterval(selectedRow, selectedRow);
    }

    public int getSelectedRow () {
        return tbl_Values.getSelectedRow();
    }

    public ParameterSet getParameterSet () {
        if (mOriginalParameterSet.isEmpty())
            return new ParameterSet.Dummy();
        return mOriginalParameterSet;
    }

    private void cosmetic () {
        int size = mParameterSet.size();
        int index = 0;
        for (int i=0; i<size; i++) {
            if (mParameterSet.get(index).isEmpty()) {
                removeRow(index);
            } else {
                index++;
            }
        }
    }

    /*
     * Implements WindowListener
     */
    @Override
    public void windowOpened(WindowEvent e) { }

    @Override
    public void windowClosing(WindowEvent e) {
        close();
    }

    @Override
    public void windowClosed(WindowEvent e) { }

    @Override
    public void windowIconified(WindowEvent e) { }

    @Override
    public void windowDeiconified(WindowEvent e) { }

    @Override
    public void windowActivated(WindowEvent e) { }

    @Override
    public void windowDeactivated(WindowEvent e) { }


    // Variables declaration - do not modify//GEN-BEGIN:variables
    private JButton btn_AddRow;
    private JButton btn_Apply;
    private JButton btn_Close;
    private JButton btn_DeleteRow;
    private JButton btn_Generate;
    private JButton btn_OK;
    private JRadioButton chk_Enum;
    private JCheckBox chk_GenerateRandomValue;
    private JRadioButton chk_Interval;
    private ButtonGroup group_ParameterType;
    private JLabel lbl_Count;
    private JLabel lbl_LowerLimit;
    private JLabel lbl_Name;
    private JLabel lbl_RangeLower;
    private JLabel lbl_RangeUpper;
    private JLabel lbl_Seed;
    private JLabel lbl_Step;
    private JLabel lbl_Type;
    private JLabel lbl_UpperLimit;
    private JLabel lbl_Value;
    private JPanel pnl_ChkGenerateRandomValueRow;
    private JPanel pnl_Content;
    private JPanel pnl_ControlRow;
    private JPanel pnl_CountRow;
    private JPanel pnl_Editor;
    private JPanel pnl_EditorPane;
    private JPanel pnl_EnumEditor;
    private JPanel pnl_EnumEditorContent;
    private JPanel pnl_Foot;
    private JPanel pnl_Generate;
    private JPanel pnl_GenerateRandomValue;
    private JPanel pnl_GenerateRandomValueForm;
    private JPanel pnl_IntervalEditor;
    private JPanel pnl_IntervalEditorContent;
    private JPanel pnl_LimitRow;
    private JPanel pnl_LowerLimitRow;
    private JPanel pnl_Name;
    private JPanel pnl_RangeLowerRow;
    private JPanel pnl_RangeUpperRow;
    private JPanel pnl_SeedAndCountRow;
    private JPanel pnl_SeedRow;
    private JPanel pnl_StepRow;
    private JPanel pnl_Type;
    private JPanel pnl_UpperLimitRow;
    private JPanel pnl_ValueFormRow;
    private JPanel pnl_ValueRow;
    private JPanel pnl_ValuesPane;
    private JPanel pnl_Wrapper;
    private JScrollPane scl_Value;
    private JScrollPane scl_Values;
    private JSplitPane split_EditorAndTable;
    private JTable tbl_Values;
    private JTextField txt_Count;
    private JTextField txt_LowerLimit;
    private JTextField txt_Name;
    private JTextField txt_RangeLower;
    private JTextField txt_RangeUpper;
    private JTextField txt_Seed;
    private JTextField txt_Step;
    private JTextField txt_UpperLimit;
    private JTextArea txtarea_Value;
    // End of variables declaration//GEN-END:variables

    private static class ParameterSetTableModel extends AbstractTableModel {
        private ParameterSet mParameterSet;

        public ParameterSetTableModel (ParameterSet parameterSet) {
            mParameterSet = parameterSet;
        }

        public void setPamraterSet (ParameterSet parameterSet) {
            mParameterSet = parameterSet;
            this.fireTableStructureChanged();
        }

        public ParameterSet.Parameter getRow (int rowIndex) {
            return mParameterSet.get(rowIndex);
        }

        public void setParameterValue(int rowIndex, String key, Object v) {
            mParameterSet.get(rowIndex).set(key, v);
            this.fireTableRowsUpdated(rowIndex, rowIndex);
        }

        public ParameterSet.Parameter getParameterByName (String name) {
            return mParameterSet.getParameterByName(name);
        }

        public Object getParameterProperty (int rowIndex, String key) {
            return mParameterSet.get(rowIndex).get(key);
        }

        private void addRow (ParameterSet.Parameter parameter) {
            int lastIndex  = getRowCount();
            mParameterSet.add(parameter);
            fireTableRowsInserted(lastIndex, lastIndex);
        }

        private void removeRow (int rowIndex) {
            mParameterSet.remove(rowIndex);
            fireTableRowsDeleted(rowIndex, rowIndex);
        }

        @Override
        public int getRowCount() {
            return mParameterSet.size();
        }

        @Override
        public int getColumnCount() {
            return 2;
        }

        @Override
        public Class<?> getColumnClass (int columnIndex) {
            return String.class;
        }

        @Override
        public String getColumnName (int columnIndex) {
            switch (columnIndex) {
            case 0: return "Name";
            case 1: return "Values";
            }
            return null;
        }

        @Override
        public boolean isCellEditable (int rowIndex, int columnIndex) {
            return false;
        }

        @Override
        public Object getValueAt(int rowIndex, int columnIndex) {
            switch (columnIndex) {
            case 0: return mParameterSet.get(rowIndex).getName();
            case 1:
                ParameterSet.Parameter p = mParameterSet.get(rowIndex);
                if (p != null)
                    return p.toString();
            }
            return null;
        }

        @Override
        public void setValueAt (Object value, int rowIndex, int columnIndex) {
            // do nothing...
        }
    }

    private static class ValueTableCellRenderer extends DefaultTableCellRenderer {
        @Override
        public Component getTableCellRendererComponent(JTable table, Object value,
                      boolean isSelected, boolean hasFocus, int row, int column) {
            Component c = super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column);

            boolean isEven = (row%2) == 0;

            if (isSelected || hasFocus)
                return c;

            Color bgcolor = (isEven)? EVEN_ROW_COLOR : ODD_ROW_COLOR;
            c.setBackground(bgcolor);

            return c;
        }
    }

    private class UpdateDocumentListener implements DocumentListener {
        private final JTextComponent mSource;

        public UpdateDocumentListener(JTextComponent source) {
            mSource = source;
        }

        @Override
        public void insertUpdate(DocumentEvent evt) {
            evt.getDocument().putProperty("source", mSource);
            updateDocument(evt);
        }

        @Override
        public void removeUpdate(DocumentEvent evt) {
            updateDocument(evt);
        }

        @Override
        public void changedUpdate(DocumentEvent evt) {
            updateDocument(evt);
        }
    }
}
