/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.view;

import java.util.ArrayList;

public class ChartConfig {

    private int mXindex;
    private String mXlegend;
    private final ArrayList<Integer> mY1indices = new ArrayList<>();
    private final ArrayList<String> mY1legends = new ArrayList<>();
    private final ArrayList<Integer> mY2indices = new ArrayList<>();
    private final ArrayList<String> mY2legends = new ArrayList<>();

    private boolean mLegend = true;

    private boolean mXlog = false;
    private boolean mY1log = false;
    private boolean mY2log = false;

    public ArrayList<Integer> getIndices() {
        ArrayList<Integer> indices = new ArrayList<>();
        indices.add(mXindex);
        indices.addAll(mY1indices);
        indices.addAll(mY2indices);
        return indices;
    }

    public ArrayList<String> getLegends() {
        ArrayList<String> legends = new ArrayList<>();
        legends.add(mXlegend);
        legends.addAll(mY1legends);
        legends.addAll(mY2legends);
        return legends;
    }

    public int getXindex() {
        return mXindex;
    }

    public void setXindex(int index) {
        mXindex = index;
    }

    public void setXlegend(String legend) {
        mXlegend = legend;
    }

    public ArrayList<Integer> getY1indices() {
        return mY1indices;
    }

    public void addY1indices(int index) {
        mY1indices.add(index);
    }

    public String getY1legend(int i) {
        return mY1legends.get(i);
    }

    public void addY1legend(String legend) {
        mY1legends.add(legend);
    }

    public ArrayList<Integer> getY2indices() {
        return mY2indices;
    }

    public void addY2indices(int index) {
        mY2indices.add(index);
    }

    public String getY2legend(int i) {
        return mY2legends.get(i);
    }

    public void addY2legend(String legend) {
        mY2legends.add(legend);
    }

    public boolean getLegend() {
        return mLegend;
    }

    public void setLegend(boolean b) {
        mLegend = b;
    }

    public boolean getXlog() {
        return mXlog;
    }

    public void setXlog(boolean b) {
        mXlog = b;
    }

    public boolean getY1log() {
        return mY1log;
    }

    public void setY1log(boolean b) {
        mY1log = b;
    }

    public boolean getY2log() {
        return mY2log;
    }

    public void setY2log(boolean b) {
        mY2log = b;
    }

}
