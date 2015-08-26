/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.command;

import java.io.File;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.prefs.BackingStoreException;
import java.util.prefs.Preferences;
import javax.swing.SwingUtilities;
import javax.swing.event.EventListenerList;

public class Session {

    private static final int MAX_RECENT_MODELS = 8;

    private final ArrayList<File> mRecentModels = new ArrayList<>();

    private final EventListenerList mListeners = new EventListenerList();

    public Session() {
        try {
            Preferences prefs = Preferences.userRoot().node("/jp/oist/flint/session/model");
            String[] keys = prefs.keys();

            Arrays.sort(keys, new Comparator<String> () {
                @Override
                public int compare(String s1, String s2) {
                    int i1 = Integer.parseInt(s1);
                    int i2 = Integer.parseInt(s2);
                    return i1 - i2;
                }
            });
            for (String key : keys) {
                String uri = prefs.get(key, null);
                if (uri == null) continue;
                mRecentModels.add(new File(new URI(uri)));
            }
        } catch (BackingStoreException | URISyntaxException e) {
            // ignore
        }
    }

    public void addListener(ISessionListener listener) {
        mListeners.add(ISessionListener.class, listener);
        notifyRecentModels();
    }

    public String getLastPath() {
        if (mRecentModels.isEmpty()) return "";
        return mRecentModels.get(0).getParent();
    }

    private void notifyRecentModels() {
        SwingUtilities.invokeLater(new Runnable() {
            @Override
            public void run() {
                for (ISessionListener listener : mListeners.getListeners(ISessionListener.class)) {
                    listener.recentModelsUpdated(mRecentModels);
                }
            }
        });
    }

    public void updateRecentModels(final File file) {
        if (mRecentModels.remove(file)) {
            mRecentModels.add(0, file);
        } else if (mRecentModels.size() < MAX_RECENT_MODELS) {
            mRecentModels.add(0, file);
        } else {
            // we have to drop the old one
            mRecentModels.remove(MAX_RECENT_MODELS - 1);
            mRecentModels.add(0, file);
        }
        notifyRecentModels();
    }

    public void saveRecentModels() throws BackingStoreException {
        int i = 0;
        Preferences prefs = Preferences.userRoot().node("/jp/oist/flint/session/model");
        for (File model : mRecentModels) {
            // We have to serialize File through URI for satisfying assumption of read-write invariance.
            // See http://docs.oracle.com/javase/6/docs/api/java/io/File.html#toURI()
            // and setion "identities" of http://docs.oracle.com/javase/6/docs/api/java/net/URI.html
            prefs.put(Integer.toString(i), model.toURI().toString());
            i += 1;
        }
        prefs.flush();
    }
}
