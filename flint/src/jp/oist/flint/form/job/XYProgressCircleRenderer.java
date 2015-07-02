/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.form.job;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.Shape;
import java.awt.Stroke;
import java.awt.geom.AffineTransform;
import java.awt.geom.Arc2D;
import java.awt.geom.Area;
import java.awt.geom.Rectangle2D;
import org.jfree.chart.axis.ValueAxis;
import org.jfree.chart.entity.EntityCollection;
import org.jfree.chart.plot.CrosshairState;
import org.jfree.chart.plot.XYPlot;
import org.jfree.chart.renderer.xy.XYLineAndShapeRenderer;
import org.jfree.data.xy.XYDataset;
import org.jfree.data.xy.XYSeriesCollection;
import org.jfree.ui.RectangleEdge;

public class XYProgressCircleRenderer extends XYLineAndShapeRenderer {

    private final ScatterChart mDelegator;

    public XYProgressCircleRenderer(ScatterChart delegator) {
        super (false, true);
        mDelegator = delegator;
    }

    @Override
    protected void drawSecondaryPass(Graphics2D g2, XYPlot plot, XYDataset dataset,
                     int pass, int series, int item,
                     ValueAxis domainAxis, Rectangle2D dataArea, ValueAxis rangeAxis, 
                     CrosshairState crosshairState, EntityCollection entities) {
            Shape entityArea;

            double valueX = dataset.getXValue(series, item);
            double valueY = dataset.getYValue(series, item);
            if (Double.isNaN(valueX) || Double.isNaN(valueY))
                return;

            RectangleEdge xAxisLocation = plot.getDomainAxisEdge();
            RectangleEdge yAxisLocation = plot.getRangeAxisEdge();
            double transX1 = domainAxis.valueToJava2D(valueX, dataArea, xAxisLocation);
            double transY1 = rangeAxis.valueToJava2D(valueY, dataArea, yAxisLocation);

            if (getItemShapeVisible(series, item)) {
                try {
                    XYProgressSeries pseries = (XYProgressSeries) ((XYSeriesCollection)dataset)
                            .getSeries(series);
                    Shape shape = drawPiProgress(g2, pseries, item, transX1, transY1);
                    entityArea = shape;

                    if (entities != null)
                        addEntity(entities, entityArea, dataset, series, item, transX1, 
                             transY1);
                    return;
                } catch (ClassCastException ex) {
                }
            }
            super.drawSecondaryPass(g2, plot, dataset, pass, series, item, 
                    domainAxis, dataArea, rangeAxis, crosshairState, entities);
    }

    protected Shape drawPiProgress (final Graphics2D g2d, XYProgressSeries series,
            int item, double x, double y) {

        Stroke defStroke = g2d.getStroke();
        Color defColor = g2d.getColor();
        Color bgColor  = g2d.getBackground();

        AffineTransform transform = g2d.getTransform();
        g2d.setStroke(new BasicStroke(1));

        double radius = 5.0;
        double diametar = 10.0;

        int x1 = (int)(x - radius);
        int y1 = (int)(y - radius);

        double max   = (double)mDelegator.getMaximum();
        double min   = (double)mDelegator.getMinimum();
        double value = (double)series.getProgress(item);

        double percent = (value-min) / (max- min);
        int angle = (int)(360.0 * percent);
        AffineTransform af = new AffineTransform();
        af.rotate(Math.toRadians(angle-90), (int)x, (int)y);
        Area areaOval    = new Area(new Arc2D.Double(
                x1, y1, diametar, diametar,
                0, angle, Arc2D.PIE));
        areaOval.transform(af);

        g2d.setColor(new Color(0x7F, 0xBF, 0xFF, 0xF0)); 
        g2d.fill(areaOval);

        g2d.setColor(defColor);
        g2d.setBackground(bgColor);
        g2d.drawOval(x1, y1, (int)diametar, (int)diametar);

        g2d.setStroke(defStroke);
        g2d.setTransform(transform);

        return new Arc2D.Double((double)x1, (double)y1, diametar, diametar, 0.0, 360.0, Arc2D.PIE);
    }
}
