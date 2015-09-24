/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.util;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.SortedSet;
import java.util.TreeSet;
import javax.swing.AbstractListModel;

/**
 * This is a class of a data model for VariableList.
 */
public class ListItemModel extends AbstractListModel {

    private final ArrayList<ListItem> mItems = new ArrayList<>();
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
        return mItems.get(0);
    }

    public void add(String key, String name) {
        int s = getSize();
        mItems.add(new ListItem(key, name));
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

    public ArrayList<String> keys() {
        ArrayList<String> list = new ArrayList<>();
        for (ListItem item : mItems) {
            list.add(item.getKey());
        }
        return list;
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
