/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.k3;

import java.util.prefs.BackingStoreException;
import java.util.prefs.Preferences;

public class K3Loader {

    final static String ROOT_DIRECTORY = "/jp/oist/flint";
    final static String K3_DIRECTORY = ROOT_DIRECTORY + "/k3";

    public boolean isEnabled() {
        Preferences prefs = Preferences.userRoot().node(K3_DIRECTORY);
        return prefs.getBoolean("enabled", false);
    }

    public void setEnabled(boolean enabled) throws BackingStoreException {
        Preferences prefs = Preferences.userRoot().node(K3_DIRECTORY);
        prefs.putBoolean("enabled", enabled);
        prefs.sync();
    }
}
