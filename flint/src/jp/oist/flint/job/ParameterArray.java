/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.job;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Scanner;

/*
 * This class represents the associative array of parameters and their values
 * assigned to a job.
 */
public class ParameterArray {

    private final ArrayList<String> mList;

    public ParameterArray(File file) throws IOException {
        mList = new ArrayList<>();
        try (Scanner scanner = new Scanner(file, "UTF8")) {
            while (scanner.hasNextLine()) {
                mList.add(scanner.nextLine());
            }
        }
    }

    public ParameterArray(ArrayList<String> list) {
        mList = list;
    }

    @Override
    public String toString() {
        // TODO: use String.join() once Java 8 becomes our requirement.
        int size = mList.size();
        if (size == 0)
            return "";
        String first = mList.get(0);
        if (size == 1 && first.equals("defaultValue=0.0")) // dummy
            return "";
        StringBuilder sb = new StringBuilder(first);
        for (int i=1;i<mList.size();i++) {
            sb.append(',').append(mList.get(i));
        }
        return sb.toString();
    }

}
