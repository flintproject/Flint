/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.util;

import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;

public class LegendCollection {

    private final LinkedHashMap<String, String> mTitleMap;
    private final HashMap<String, Integer> mIndexMap;
    private final HashMap<String, Integer> mCount;

    public LegendCollection() {
        mTitleMap = new LinkedHashMap<>();
        mIndexMap = new HashMap<>();
        mCount = new HashMap<>();
    }

    public void register(String key, String title, int index) {
        mTitleMap.put(key, title);
        mIndexMap.put(key, index);
        if (mCount.containsKey(title)) {
            int n = mCount.get(title);
            mCount.put(title, n+1);
        } else {
            mCount.put(title, 1);
        }
    }

    public HashMap<String, String> toHashMap() {
        HashMap<String, String> m = new HashMap<>();
        for (Map.Entry<String, String> e : mTitleMap.entrySet()) {
            String key = e.getKey();
            String title = e.getValue();
            if (mCount.get(title) > 1) {
                int index = mIndexMap.get(key);
                m.put(key, "(" + index + ") " + title);
            } else {
                m.put(key, title);  // it is better to keep legend the same as title if possible
            }
        }
        return m;
    }
}
