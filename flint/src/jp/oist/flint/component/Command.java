/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.component;

import java.io.File;
import java.util.ArrayList;

public class Command {

    final String mName;

    final ArrayList<String> mOptions = new ArrayList<>();

    final ArrayList<Object> mArguments = new ArrayList<>();

    File mInputFile = null;

    File mOutputFile = null;

    public Command(String name) {
        mName = name;
    }

    public String getName() {
        return mName;
    }

    public ArrayList<String> getOptions() {
        return mOptions;
    }

    public void addOption(String key, int value) {
        mOptions.add("--" + key + " " + value);
    }

    public ArrayList<Object> getArguments() {
        return mArguments;
    }

    public void addArgument(File file) {
        mArguments.add(file);
    }

    public void addArgument(int i) {
        mArguments.add(new Integer(i));
    }

    public void addArgument(String s) {
        mArguments.add(s);
    }

    public File getInputFile() {
        return mInputFile;
    }

    public void setInputFile(File file) {
        mInputFile = file;
    }

    public File getOutputFile() {
        return mOutputFile;
    }

    public void setOutputFile(File file) {
        mOutputFile = file;
    }
}
