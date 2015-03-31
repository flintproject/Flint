/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.form.sub;

import java.io.File;
import jp.oist.flint.view.Summary;
import org.jfree.chart.ChartPanel;

public interface IChartController {

    File getModelFile ();

    File getIsdFile ();

    File getCsvFile ();

    String getOutputFilePath ();

    void clearChart();

    void displayChart(ChartPanel panel);

    void displaySummary(Summary summary);

    IChartSetting getChartSetting();

    void renderPlot();
}
