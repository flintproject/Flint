/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.form.job;

public interface IParameterInfo {

    public String getTitle (int index);

    public Number getMaximum (int index);

    public Number getMinimum (int index);

    public String[] getTitles();

    public String[] getActiveTitles ();

    public Number[] getActiveMaximums ();

    public Number[] getActiveMinimums ();

    public Integer[] getIgnoreIndexes ();

    public int getParameterCount();

    public int getActiveParameterCount ();

    public int[] getActiveIndexes ();
}
