/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.preference;

import java.util.prefs.Preferences;

public class ExperimentalFeatures {

    static final String KEY = "experimentalFeatures";

    static final Preferences mPrefs;

    static {
        mPrefs = Preferences.userRoot().node("/jp/oist/flint");
    }

    public static boolean enabled() {
        return mPrefs.getBoolean(KEY, false);
    }

    public static void setEnabled(boolean b) {
        mPrefs.putBoolean(KEY, b);
    }
}
