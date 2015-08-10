/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.filesystem;

import java.io.File;

public class Filename {

    public static String getNameWithoutExtension(File file) {
        String fileName = file.getName();
        int postion = fileName.lastIndexOf('.');
        if (postion == -1) return fileName;
        return fileName.substring(0, postion);
    }

    public static File getVariant(File sourceFile, File targetDir, int index, int numDigits, String extension) {
        String name = getNameWithoutExtension(sourceFile);
        String format = "%s_%0" + numDigits + "d.%s";
        String basename = String.format(format, name, index, extension);
        return new File(targetDir, basename);
    }
}
