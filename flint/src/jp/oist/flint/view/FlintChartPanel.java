/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */

package jp.oist.flint.view;

import java.awt.AWTEvent;
import java.awt.Cursor;
import java.awt.Point;
import java.awt.event.MouseEvent;
import java.awt.event.MouseWheelEvent;
import java.awt.event.MouseWheelListener;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import org.jfree.chart.ChartPanel;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.axis.ValueAxis;
import org.jfree.chart.plot.PlotRenderingInfo;
import org.jfree.chart.plot.ValueAxisPlot;
import org.jfree.chart.plot.Zoomable;
import org.jfree.data.Range;
import org.jfree.ui.RectangleEdge;

public class FlintChartPanel extends ChartPanel 
    implements MouseWheelListener {

    private Point mMovedAnchorPoint = null;

    private volatile int mClickedButtonId = -1;

    public FlintChartPanel(JFreeChart chart) {
        super(chart);

        initComponents();
    }

    private void initComponents() {
        addMouseWheelListener(this);
    }

    private void dispatchEventToParent (AWTEvent evt) {
        if (getParent() != null)
            getParent().dispatchEvent(evt);
    }

    @Override
    public void mouseDragged (MouseEvent evt) {
        switch (mClickedButtonId) {
        case MouseEvent.BUTTON1:
            move(translateJava2DToScreen(evt.getPoint()));
            break;
        }
        dispatchEventToParent(evt);
    }

    @Override 
    public void mouseReleased (MouseEvent evt) {

        int buttonId = evt.getButton();

        switch (buttonId) {
        case MouseEvent.BUTTON1:
            mMovedAnchorPoint = null;
            setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
            break;
        }

        mClickedButtonId = -1;
        dispatchEventToParent(evt);
    }

    @Override
    public void mousePressed (MouseEvent evt) {
        if (evt.getClickCount() > 1)
            return;
        mClickedButtonId = evt.getButton();
        switch (mClickedButtonId) {
        case MouseEvent.BUTTON1:
            mMovedAnchorPoint = evt.getPoint();
            break;
        }
        dispatchEventToParent(evt);
    }

    @Override
    public void mouseMoved (MouseEvent evt) {
        super.mouseMoved(evt);
    }

    @Override
    public void mouseWheelMoved(MouseWheelEvent evt) {
        int wheelRotation = evt.getWheelRotation();
        Point2D point = translateJava2DToScreen(evt.getPoint());

        if (wheelRotation > 0) { // zoom out
            zoomOut(point);
        } else if (wheelRotation < 0) { // zoom in
            zoomIn(point);
        }
        dispatchEventToParent(evt);

    }

    private PlotRenderingInfo getPlotRenderingInfo (Point p) {
        int subplotIndex = getChartRenderingInfo().getPlotInfo().getSubplotIndex(p);
        return (subplotIndex < 0) ?
            getChartRenderingInfo().getPlotInfo() 
                : getChartRenderingInfo().getPlotInfo().getSubplotInfo(subplotIndex);
    }

    private void move (Point2D currentPnt) {
        if (mMovedAnchorPoint == null)
            return;

        Point anchorPnt = mMovedAnchorPoint;
        mMovedAnchorPoint = (Point)currentPnt;

        setCursor(new Cursor(Cursor.CROSSHAIR_CURSOR));

        Rectangle2D dataArea = getPlotRenderingInfo((Point)currentPnt).getDataArea();
        for (int i = 0; i < getChart().getXYPlot().getDomainAxisCount(); i++) {
            ValueAxis domainAxis = getChart().getXYPlot().getDomainAxis(i);
            if (domainAxis == null)
                continue;

            double currentX = domainAxis.java2DToValue(
                currentPnt.getX(), dataArea, RectangleEdge.TOP);
            double previousX  = domainAxis.java2DToValue(
                anchorPnt.getX(), dataArea, RectangleEdge.TOP);
            double movement = previousX - currentX;
            move(domainAxis, movement);
        }
        for (int i = 0; i < getChart().getXYPlot().getRangeAxisCount(); i++) {
            ValueAxis rangeAxis = getChart().getXYPlot().getRangeAxis(i);
            if (rangeAxis == null)
                continue;

            double curY = rangeAxis.java2DToValue(currentPnt.getY(), dataArea, RectangleEdge.TOP);
            double prevY = rangeAxis.java2DToValue(anchorPnt.getY(), dataArea, RectangleEdge.TOP);
            double movement = curY - prevY;
            move(rangeAxis, movement);
        }
    }

    private void move (ValueAxis axis, double movement) {
        if (axis == null || axis.getPlot() instanceof ValueAxisPlot == false)
            return;

        ValueAxisPlot plot = (ValueAxisPlot)axis.getPlot();
        Range defaultRange =  plot.getDataRange(axis);

        if (defaultRange == null)
            return;

        Range currentRange = axis.getRange();

        Range movementRange = new Range(currentRange.getLowerBound() + movement,
                                        currentRange.getUpperBound() + movement);
        Range permissibleRange = new Range(defaultRange.getLowerBound() - axis.getLowerMargin(),
                                           defaultRange.getUpperBound() + axis.getUpperMargin());

        if (permissibleRange.getLowerBound() <= movementRange.getLowerBound()
                && movementRange.getUpperBound() <= permissibleRange.getUpperBound())
            axis.setRange(movementRange);
    }

    private void zoomIn (Point2D point) {
        PlotRenderingInfo plotInfo = getPlotRenderingInfo((Point)point);
        Rectangle2D dataArea = plotInfo.getDataArea();

        double anchorX = dataArea.getX() + dataArea.getWidth() * 0.5;
        double anchorY = dataArea.getY() + dataArea.getHeight() * 0.5;

        zoomInBoth(anchorX, anchorY);
    }
        
    private void zoomOut (Point2D point) {
        for (int i = 0; i < getChart().getXYPlot().getDomainAxisCount(); i++) {
            ValueAxis domainAxis = getChart().getXYPlot().getDomainAxis(i);
            double anchor = domainAxis.getRange().getCentralValue();
            zoomOutAxis(domainAxis, anchor);
        }

        for (int i = 0; i < getChart().getXYPlot().getRangeAxisCount(); i++) {
            ValueAxis rangeAxis = getChart().getXYPlot().getRangeAxis(i);
            double anchor = rangeAxis.getRange().getCentralValue();
            zoomOutAxis(rangeAxis, anchor);
        }
    }

    private void zoomOutAxis (ValueAxis axis, double anchor) {
        if (axis == null || axis.getPlot() instanceof ValueAxisPlot == false
                || axis.getPlot() instanceof Zoomable == false)
            return;

        ValueAxisPlot plot = (ValueAxisPlot)axis.getPlot();
        Range defaultRange = plot.getDataRange(axis);
        if (defaultRange == null)
            return;
        double halfLength = axis.getRange().getLength() * getZoomOutFactor() * 0.5;
        Range zoomedRange = new Range (anchor - halfLength, anchor + halfLength);
        if (zoomedRange.getUpperBound() <= defaultRange.getUpperBound()) {
            axis.setRange(zoomedRange);
        } else {
            axis.setAutoRange(true);
        }
    }
}
