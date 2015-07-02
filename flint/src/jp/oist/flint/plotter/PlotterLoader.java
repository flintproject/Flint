/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.plotter;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.prefs.BackingStoreException;
import java.util.prefs.Preferences;
import javax.swing.JComboBox;

public class PlotterLoader {
    final static String ROOT_DIRECTORY = "/jp/oist/flint";
    final static String PLOTTER_DIRECTORY = ROOT_DIRECTORY + "/plotter";

    public void setComboBox(JComboBox comboBox)
        throws BackingStoreException {
        comboBox.removeAllItems();

        String defaultPlotter = null;
        try {
            defaultPlotter = getDefault();
        } catch (PlotterLoadException ple) {
            // ignored
        }
        Preferences prefs = Preferences.userRoot().node(PLOTTER_DIRECTORY);
        String[] keys = prefs.childrenNames();
        int c = 0;
        for (String key : keys) {
            try {
                IPlotter plotter = load(key);
                comboBox.addItem(plotter);
                if (key.equals(defaultPlotter)) {
                    comboBox.setSelectedIndex(c);
                }
                c += 1;
            } catch (ClassNotFoundException | IOException | IllegalAccessException | InstantiationException | PlotterLoadException e) {
                deregister(key);
            }
        }
    }

    public IPlotter loadDefault()
        throws ClassNotFoundException, IOException, IllegalAccessException, InstantiationException, PlotterLoadException {
        return load(getDefault());
    }

    public static IPlotter load(String key, String className, String path)
        throws ClassNotFoundException, IOException, IllegalAccessException, InstantiationException, PlotterLoadException {
        File file = new File(path);
        if (!file.isFile()) {
            throw new PlotterLoadException(key + "'s file not found");
        }
        URL url = file.toURI().toURL();
        URL[] urls = {url};
        // Do not forget to close a URLClassLoader. See
        // http://docs.oracle.com/javase/7/docs/api/java/net/URLClassLoader.html#close()
        try (URLClassLoader ucl = new URLClassLoader(urls)) {
        Class klass = ucl.loadClass(className);
        return (IPlotter)klass.newInstance();
        }
    }

    private IPlotter load(String key)
        throws ClassNotFoundException, IOException, IllegalAccessException, InstantiationException, PlotterLoadException {
        Preferences prefs = getPlotterPrefercences(key);
        String className = getClassName(key, prefs);
        String path = prefs.get("path", null);
        if (path == null) {
            ClassLoader cl = ClassLoader.getSystemClassLoader();
            Class klass = cl.loadClass(className);
            return (IPlotter)klass.newInstance();
        } else {
            return load(key, className, path);
        }
    }

    public void saveAsDefault(IPlotter plotter) throws BackingStoreException, PlotterLoadException {
        String key = getKeyOf(plotter);
        saveAsDefault(key);
    }

    public void saveAsDefault(String key) throws BackingStoreException {
        Preferences prefs = getRootPreferences();
        prefs.put("defaultPlotter", key);
        prefs.sync();
    }

    public String getKeyOf(IPlotter plotter) throws BackingStoreException, PlotterLoadException {
        String name = plotter.getClass().getName();
        Preferences prefs = Preferences.userRoot().node(PLOTTER_DIRECTORY);
        String[] keys = prefs.childrenNames();
        for (String key : keys) {
            String className = getClassName(key);
            if (name.equals(className)) {
                return key;
            }
        }
        throw new PlotterLoadException("could not find plotter's key");
    }

    public static boolean exists(String key) {
        try {
            getClassName(key);
            return true;
        } catch (PlotterLoadException ple) {
            return false;
        }
    }

    private String getDefault() throws PlotterLoadException {
        Preferences prefs = getRootPreferences();
        String defaultPlotter = prefs.get("defaultPlotter", null);
        if (defaultPlotter == null) {
            throw new PlotterLoadException("plotter not specified");
        }
        return defaultPlotter;
    }

    private static String getClassName(String key) throws PlotterLoadException {
        Preferences prefs = getPlotterPrefercences(key);
        return getClassName(key, prefs);
    }

    private static String getClassName(String key, Preferences prefs) throws PlotterLoadException {
        String className = prefs.get("className", null);
        if (className == null) {
            throw new PlotterLoadException(key + "'s className not specified");
        }
        return className;
    }

    public static void register(String key, String className, String path) throws BackingStoreException {
        Preferences prefs = getPlotterPrefercences(key);
        prefs.put("className", className);
        prefs.put("path", path);
        prefs.sync();
    }

    public static void register(String key, Class klass) throws BackingStoreException {
        Preferences prefs = getPlotterPrefercences(key);
        prefs.put("className", klass.getName());
        prefs.sync();
    }

    private static void deregister(String key) throws BackingStoreException {
        Preferences prefs = getPlotterPrefercences(key);
        prefs.removeNode();
    }

    private static Preferences getPlotterPrefercences(String key) {
        String dir = PLOTTER_DIRECTORY + "/" + key;
        return Preferences.userRoot().node(dir);
    }

    private static Preferences getRootPreferences() {
        return Preferences.userRoot().node(ROOT_DIRECTORY);
    }
}
