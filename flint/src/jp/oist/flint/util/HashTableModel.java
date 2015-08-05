/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.util;

import java.util.List;
import java.util.Map;
import javax.swing.table.AbstractTableModel;

/**
 * This is a custom TableModel class.
 */
public class HashTableModel extends AbstractTableModel {

    /** The Map object which stored the index and column name of the column. */
    private Map<Integer, String> keyOrder;
    /** The list of Map objects which stored the value of the cell. */
    private List<Map> mList;

    public HashTableModel() {
    }

    public HashTableModel(List<Map> _data) {
        mList = _data;
    }

    public HashTableModel(List<Map> _data, Map<Integer, String> _keyOrder) {
        mList = _data;
        keyOrder = _keyOrder;
    }

    /**
     * Map passed by the argument is considered as a column list, and it is a set.
     *
     * @param _keyOrder
     */
    public void setColumnOrder(Map<Integer, String> _keyOrder) {
        keyOrder = _keyOrder;
    }

    /**
     * The number of columns is returned.
     *
     * @return int
     */
    @Override
    public int getColumnCount() {
        return mList.get(0).keySet().size();
    }

    /**
     * The number of lines is returned.
     *
     * @return int
     */
    @Override
    public int getRowCount() {
        return mList.size();
    }

    /**
     * The value of the column passed by the argument is returned.
     *
     * @return int
     */
    @Override
    public Object getValueAt(int rowIndex, int columnIndex) {
        return mList.get(rowIndex).get(keyOrder.get(columnIndex));

    }

    /**
     * The column name of the n-th column passed by the argument is returned.
     *
     * @return int
     */
    @Override
    public String getColumnName(int column) {
        return keyOrder.get(column);
    }
}
