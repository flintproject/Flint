/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.plotter;

import java.io.IOException;
import java.net.URL;
import java.net.URLClassLoader;
import java.security.PrivilegedExceptionAction;

public class PlotterURLClassLoader implements PrivilegedExceptionAction<IPlotter> {

    private final URL mUrl;

    private final String mClassName;

    public PlotterURLClassLoader(URL url, String className) {
        mUrl = url;
        mClassName = className;
    }

    public IPlotter run() throws PlotterLoadException {
        URL[] urls = {mUrl};
        // Do not forget to close a URLClassLoader. See
        // http://docs.oracle.com/javase/7/docs/api/java/net/URLClassLoader.html#close()
        try (URLClassLoader ucl = new URLClassLoader(urls)) {
            Class klass = ucl.loadClass(mClassName);
            return (IPlotter)klass.newInstance();
        } catch (ClassNotFoundException | IOException |
                 IllegalAccessException | InstantiationException e) {
            throw new PlotterLoadException(e.getMessage(), e);
        }
    }
}
