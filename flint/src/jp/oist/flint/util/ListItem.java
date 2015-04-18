/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.util;

/**
 * This is the class of a list item.
 */
public class ListItem implements Comparable<ListItem> {

    private final String mKey;
    private final String mName;

    public ListItem(String key, String name) {
        mKey = key;
        mName = name;
    }

    @Override
    public int compareTo(ListItem o) {
        int n = mName.compareTo(o.mName);
        return (n == 0) ? mKey.compareTo(o.mKey) : n;
    }

    @Override
    public String toString() {
        return mName;
    }

    public String getKey() {
        return mKey;
    }

    public String getName() {
        return mName;
    }
}
