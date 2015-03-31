/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.util;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.Vector;
import javax.swing.AbstractListModel;

/**
 * This is a class of a data model for VariableList.
 */
public class ListItemModel extends AbstractListModel {

    private final Vector<ListItem> mItems = new Vector<>();
    /** Multiple selection flag. */
    private boolean multipleFlg = true;

    public ListItemModel(LinkedHashMap<String, String> map) {
        // sort items by name
        SortedSet<ListItem> sortedSet = new TreeSet<>();
        for (Map.Entry<String, String> e : map.entrySet()) {
            sortedSet.add(new ListItem(e.getKey(), e.getValue()));
        }
        int s = sortedSet.size();
        if (s == 0) return;
        for (ListItem item : sortedSet) {
            mItems.add(item);
        }
    }

    /**
     * The length of a list is returned.
     *
     * @see AbstractListModel
     * @return int
     */
    @Override
    public int getSize() {
        return mItems.size();
    }

    /**
     * The value of the i-th argument is returned.
     *
     * @see AbstractListModel
     * @param i
     * @return Object
     */
    @Override
    public Object getElementAt(int i) {
        return mItems.get(i);
    }

    public Object firstElement() {
        return mItems.firstElement();
    }

    public void add(String key, String name) {
        int s = getSize();
        mItems.addElement(new ListItem(key, name));
        fireIntervalAdded(this, s, s);
    }

    public void remove(int i) {
        mItems.remove(i);
        fireIntervalRemoved(this, i, i);
    }

    public String keyAt(int i) {
        return mItems.get(i).getKey();
    }

    public String nameAt(int i) {
        return mItems.get(i).getName();
    }

    public String firstKey() {
        return ((ListItem)firstElement()).getKey();
    }

    public String firstName() {
        return ((ListItem)firstElement()).getName();
    }

    public Vector<String> keys() {
        Vector<String> v = new Vector<>();
        for (ListItem item : mItems) {
            v.add(item.getKey());
        }
        return v;
    }

    /**
     * A multiple selection flag is returned.
     *
     * @return boolean
     */
    public boolean getMultipleFlg() {
        return multipleFlg;
    }

    /**
     * A multiple selection flag is set.
     *
     * @param multipleFlg
     */
    public void setMultipleFlg(boolean multipleFlg) {
        this.multipleFlg = multipleFlg;
    }
}
