/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.form;

import java.awt.Component;
import java.text.NumberFormat;
import javax.swing.JLabel;
import javax.swing.JTable;
import javax.swing.SwingConstants;
import javax.swing.table.TableCellRenderer;

class DoubleRenderer implements TableCellRenderer {

    private final NumberFormat mNf;

    public DoubleRenderer() {
        mNf = NumberFormat.getInstance();
        mNf.setGroupingUsed(false);
    }

    @Override
    public Component getTableCellRendererComponent(JTable table, Object value,
                                                   boolean isSelected, boolean hasFocus,
                                                   int row, int column) {
        JLabel label = new JLabel();
        label.setHorizontalAlignment(SwingConstants.RIGHT);
        double d = ((Double)value).doubleValue();
        if ((long)d == d) {
            String text = mNf.format(d);
            label.setText(text);
        } else {
            String text = String.format("%g", d);
            label.setText(text);
        }
        return label;
    }

}
