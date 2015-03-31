/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.util;

import java.io.File;
import java.io.IOException;

public class FileUtility {

    public static String extension (File f) {
        return extension(f.getName());
    }

    public static String extension (String s) {
        int index = s.lastIndexOf(".")+1;
        if (index <= 0) return null;
        return s.substring(index, s.length());
    }

    public static String filename (File f) {
        return filename(f.getName());
    }

    public static String filename (String s) {
        int index = s.lastIndexOf(".");
        if (index <= 0) return s;
        return s.substring(0, index);
    }

    public static boolean recursiveDelete (File target) {
        if (target.isDirectory()) {
            for (File f : target.listFiles())
                recursiveDelete(f);
        }
        return target.delete();
    }
}
