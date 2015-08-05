/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.util;

import java.util.Objects;

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
    public boolean equals(Object object) {
        if (!(object instanceof ListItem))
            return false;
        ListItem o = (ListItem)object;
        return mKey.equals(o.mKey) && mName.equals(o.mName);
    }

    @Override
    public int hashCode() {
        return Objects.hash(mKey, mName);
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
