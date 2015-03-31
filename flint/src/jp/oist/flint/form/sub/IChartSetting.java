/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.form.sub;

import jp.oist.flint.util.ListItemModel;

public interface IChartSetting {

    String getTerminalType ();

    String getLegend(String key);

    int getTrackIndex(Object key);

    double getXEnd();

    String getXLegend();

    ListItemModel getXListItemModel();

    double getXStart();

    double getY2End();

    String getY2Legend();

    ListItemModel getY2ListItemModel();

    double getY2Start();

    double getYEnd();

    String getYLegend();

    ListItemModel getYListItemModel();

    double getYStart();

    boolean isLegendChecked();

    boolean isXLegendDefault();

    boolean isXRangeDefault();

    boolean isXlogChecked();

    boolean isY1logChecked();

    boolean isY2LegendDefault();

    boolean isY2RangeDefault();

    boolean isY2logChecked();

    boolean isYLegendDefault();

    boolean isYRangeDefault();

    boolean showsKey();

}
