/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.view;

public class Summary {

    private final String mName;
    private final double mMax;
    private final double mMin;

    public Summary(String name, double maxValue, double minValue) {
        mName = name;
        mMax = maxValue;
        mMin = minValue;
    }

    public String getName() {
        return mName;
    }

    public double getMax() {
        return mMax;
    }

    public double getMin() {
        return mMin;
    }
}
