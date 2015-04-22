/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.form.job;

import java.awt.AWTEvent;
import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Component;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.RenderingHints;
import java.awt.Stroke;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.awt.geom.AffineTransform;
import java.awt.geom.Arc2D;
import java.awt.geom.Area;
import java.awt.geom.Line2D;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import javax.swing.JPanel;
import javax.swing.UIManager;

public class RadarChartProgress extends JPanel 
    implements PropertyChangeListener, MouseListener, MouseMotionListener {

    private RadarChart mRadarChart = null;

    private final int mNumberOfSides;

    private boolean mValueIsAdjusting = false;

    private int mMaximumProgress = 100;

    private int mMinimumProgress = 0;

    private int mProgress = 0;

    private boolean mIsCancelled = false;

    private int mSelectedIndex = -1;

    public RadarChartProgress (int numberOfSides) {
        super(null);

        mNumberOfSides = numberOfSides;

        initComponents();
    }

    private void initComponents () {
        addPropertyChangeListener(this);
        addMouseListener(this);
        addMouseMotionListener(this);

        setBackground(UIManager.getColor("List.background"));
        setForeground(UIManager.getColor("List.foreground"));
        setCursor(new Cursor(Cursor.HAND_CURSOR));

        mRadarChart = new RadarChart(mNumberOfSides);
        mRadarChart.setColor(getForeground());
        mRadarChart.setBackground(getBackground());
    }

    public void setAntialiasing (boolean b) {
        mRadarChart.setAntialiasing(b);
    }

    public boolean isAntialiasing() {
        return mRadarChart.isAntialiasing();
    }

    public int getNumberOfSides () {
        return mRadarChart.getNumberOfSides();
    }

    public void setValues (Number[] values) {
        mRadarChart.setValues(values);
    }

    public void setTitles (String[] titles) {
        mRadarChart.setTitles(titles);
    }

    public void setTitle (int side, String title) {
        mRadarChart.setTitle(side, title);
    }

    public String getTitle (int side) {
        return mRadarChart.getTitle(side);
    }

    public void setMinimumValues (Number[] minimums) {
        mRadarChart.setMinimums(minimums);
    }

    public void setMaximumValues (Number[] maximums) {
        mRadarChart.setMaximums(maximums);
    }

    public Number[] getValues () {
        return mRadarChart.getValues();
    }

    public Number getValue (int side) {
        return mRadarChart.getValue(side);
    }

    public Point getVertexPoint (int index) {
        return mRadarChart.getVertexPoint(index);
    }

    public Point getBackgroundVertexPoint (int index) {
        return mRadarChart.getBackgroundVertexPoint(index);
    }

    public void onCancelled () {
        mIsCancelled = true;
    }

    public boolean isCancelled () {
        return mIsCancelled;
    }

    public void setCancelled(boolean cancelled) {
        mIsCancelled = cancelled;
    }

    public int vertexAtPoint (Point p) {
        int radius = mRadarChart.getVertexRadius();
        for (int i=0; i< mRadarChart.getNumberOfSides(); i++) {
            Point vertexPoint = getVertexPoint(i);
            if (vertexPoint == null)
                continue;

            Line2D.Double linePoints = new Line2D.Double(
                    mRadarChart.getCenterPoint(), vertexPoint); 

            if (linePoints.intersects(p.x-5.0, p.y-5.0, 10.0, 10.0) ||
                    vertexPoint.x-radius <= p.x && p.x <= vertexPoint.x+radius
                    && vertexPoint.y-radius <= p.y && p.y <= vertexPoint.y+radius)
                return i;
        }
        return -1;
    }

    public int backgroundVertexAtPoint (Point p) {
        int radius = mRadarChart.getVertexRadius();
        for (int i=0; i< mRadarChart.getNumberOfSides(); i++) {
            Point vertexPoint = mRadarChart.getBackgroundVertexPoint(i);
            if (vertexPoint == null)
                continue;

            Line2D.Double linePoints = new Line2D.Double(
                    mRadarChart.getCenterPoint(), vertexPoint); 

            if (linePoints.intersects(p.x-5.0, p.y-5.0, 10.0, 10.0) ||
                   (vertexPoint.x-radius <= p.x && p.x <= vertexPoint.x+radius
                    && vertexPoint.y-radius <= p.y && p.y <= vertexPoint.y+radius))
                return i;
        }
        return -1;
    }

    public boolean getValueIsAdjusting () {
        return mValueIsAdjusting;
    }

    public void setValueIsAdjusting (boolean isAdjusting) {
        mValueIsAdjusting = isAdjusting;
    }

    public int getMaximumProgress () {
        return mMaximumProgress;
    }

    public void setMaximumProgress (int maximum) {
        mMaximumProgress = maximum;
    }

    public int getMinimumProgress () {
        return mMinimumProgress;
    }

    public void setMinimumProgress (int minimum) {
        mMinimumProgress = minimum;
    }

    public int getProgress () {
        return mProgress;
    }

    public void setProgress (int progress) {
        mProgress = progress;
    }

    @Override
    public void paint (Graphics g) {
        super.paint(g);

        Graphics2D g2d = (Graphics2D)g;

        Color defaultColor = g2d.getColor();
        Stroke defaultStroke = g2d.getStroke();

        mRadarChart.draw(g, getSize().width, getSize().height);

        g2d.setColor(getForeground());
        g2d.setBackground(getBackground());

        if (mRadarChart.isAntialiasing())
            g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, 
                    RenderingHints.VALUE_ANTIALIAS_ON);
        Dimension dim = getSize();

        Point center = new Point(dim.width/2, dim.height/2);

        double p     = (double)mProgress;
        double min   = (double)mMaximumProgress;
        double max   = (double)mMinimumProgress;


        double percent = (p-min)/(max-min);
        int angle = (int)(360 - 360 * percent);

        int radius = (Math.min(dim.width, dim.height) - mRadarChart.getVertexRadius()*2)/2;
        int x = center.x - radius;
        int y = center.y - radius;

        if (mSelectedIndex >= 0) {
            g2d.setStroke(new BasicStroke(5));
            g2d.setColor(new Color(0xFF, 0x7F, 0x7F, 0xA0));
            Point vertex = mRadarChart.getBackgroundVertexPoint(mSelectedIndex);
            g2d.drawLine(center.x, center.y, vertex.x, vertex.y);
            g2d.setStroke(defaultStroke);
        }

        Area areaOval    = new Area(new Arc2D.Double(
                x, y, radius*2, radius*2,
                0, angle, Arc2D.PIE));

        AffineTransform af = new AffineTransform();
        af.rotate(Math.toRadians(angle-90), center.x, center.y);
        areaOval.transform(af);

        /* Pastel blue / alpha: 37% */
        g2d.setColor(new Color(0x7F, 0xBF, 0xFF, 0x60)); 
        g2d.fill(areaOval);

        g2d.setColor(defaultColor);
    }

    private MouseEvent transformePoint(MouseEvent evt) {
        Component source = (getParent() == null)? 
                this : getParent();
        Point p = evt.getPoint();
        p.translate(getBounds().x, getBounds().y);
        return new MouseEvent (
                source, evt.getID(), evt.getWhen(), evt.getModifiers(),
                p.x, p.y, evt.getClickCount(), evt.isPopupTrigger(), 
                evt.getButton());
    }

    private void dispatchEventToParent (AWTEvent evt) {
        if (getParent() != null)
            getParent().dispatchEvent(evt);
    }

    /*
     * implements MouseMotionListener 
     */
    @Override
    public void mouseDragged(MouseEvent me) {
        dispatchEventToParent(transformePoint(me));
    }

    @Override
    public void mouseMoved(MouseEvent me) {
        Point point  = me.getPoint();
        mSelectedIndex = backgroundVertexAtPoint(point);
        repaint();

        dispatchEventToParent(transformePoint(me));
    }

    /*
     * implements MouseListener  
     */
    @Override
    public void mouseClicked(MouseEvent me) {
        dispatchEventToParent(transformePoint(me));
    }

    @Override
    public void mousePressed(MouseEvent me) {
        dispatchEventToParent(transformePoint(me));
    }

    @Override
    public void mouseReleased(MouseEvent me) {
        dispatchEventToParent(transformePoint(me));
    }

    @Override
    public void mouseEntered(MouseEvent me) {
        dispatchEventToParent(transformePoint(me));
    }

    @Override
    public void mouseExited(MouseEvent me) {
        mSelectedIndex = -1;
        repaint();

        dispatchEventToParent(transformePoint(me));
    }

    /*
     * implements PropertyChangeListener
     */
    @Override
    public void propertyChange(PropertyChangeEvent evt) {
        String propertyName = evt.getPropertyName();
        Object newValue = evt.getNewValue();

        if (mRadarChart == null)
            return;

        if ("foreground".contains(propertyName)) {
            mRadarChart.setColor((Color)newValue);
        } else if ("background".contains(propertyName)) {
            mRadarChart.setBackground((Color)newValue);
        }
    }
}
