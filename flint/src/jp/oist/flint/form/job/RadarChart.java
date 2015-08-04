/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.form.job;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.Polygon;
import java.awt.RenderingHints;
import java.awt.Stroke;
import java.awt.geom.AffineTransform;
import java.util.Arrays;

public class RadarChart {

    private final static double MINIMUM_RATIO = 0.01;

    public final static int BORDER_POLYGON = 0x01;

    public final static int BORDER_CIRCLE  = 0x02;

    private final int mNumberOfSides;

    private String[] mTitles;

    private Number[] mValues;

    private Number[] mMinimums;

    private Number[] mMaximums;

    private Point[] mVertexes = null;

    private Point[] mBackgroundVertexes = null;

    private Point mCenter = null;

    private int mVertexRadius = 3;

    private Color mColor = Color.BLACK;

    private Color mBackground = Color.WHITE;

    private boolean isAntialiasing = false;

    private int mBorderType = BORDER_CIRCLE;

    public RadarChart (int numberOfSides) {
        mNumberOfSides = numberOfSides;

        mValues  = new Number[numberOfSides];
        Arrays.fill(mValues, null);
        mMaximums = new Number[numberOfSides];
        Arrays.fill(mMaximums, null);
        mMinimums = new Number[numberOfSides];
        Arrays.fill(mMinimums, null);
        mTitles  = new String[numberOfSides];
        Arrays.fill(mTitles, "");
    }

    public void setColor (Color color) {
        mColor = color;
    }

    public Color getColor () {
        return mColor;
    }

    public void setBackground (Color background) {
        mBackground = background;
    }

    public Color getBackground () {
        return mBackground;
    }

    public void setAntialiasing (boolean b) {
        isAntialiasing = b;
    }

    public boolean isAntialiasing() {
        return isAntialiasing;
    }

    public int getNumberOfSides() {
        return mNumberOfSides;
    }

    public void setTitles (String[] headers) {
        mTitles = headers;
    }

    public void setTitle (int side, String title) {
        mTitles[side] = title;
    }

    public String getTitle (int side) {
        return mTitles[side];
    }

    public void setValues (Number[] values) {
        mValues = values;
    }

    public void setValue (int side, Double value) {
        mValues[side] = value;
    }

    public Number getValue(int side) {
        return mValues[side];
    }

    public void setMaximums (Number[] maximums) {
        mMaximums = maximums;
    }

    public void setMaximum (int side, Double maximum) {
        mMaximums[side] = maximum;
    }

    public void setMinimums (Number[] minimums) {
        mMinimums = minimums;
    }

    public void setMinimum (int side, Double minimum) { 
        mMinimums[side] = minimum;
    }

    public int getVertexRadius () {
        return mVertexRadius;
    }

    public void setVertexRadius (int radius) {
        mVertexRadius = radius;
    }

    public Point getVertexPoint (int index) {
        if (mVertexes == null)
            return null;
        return mVertexes[index];
    }

    public Point getBackgroundVertexPoint (int index) {
        if (mBackgroundVertexes == null)
            return null;
        return mBackgroundVertexes[index];
    }

    public void setBorderType (int borderType) {
        mBorderType = borderType;
    }

    public int getBorderType () {
        return mBorderType;
    }

    public Point getCenterPoint () { 
        return mCenter;
    }

    public void validate (int width, int height) {
        if (mVertexes != null && mBackgroundVertexes != null)
            return;

        mBackgroundVertexes = new Point[mNumberOfSides];
        mVertexes = new Point[mNumberOfSides];
        mCenter = new Point(width/2, height/2);

        AffineTransform af = new AffineTransform();
        af.rotate(Math.toRadians(-90), mCenter.x, mCenter.y);

        double radian = 2.0 * Math.PI / mNumberOfSides;

        int vertex = 0;
        for (int i=0; i<mNumberOfSides; i++) {

            double ratio;
            double dmax = mMaximums[i].doubleValue();
            double dmin = mMinimums[i].doubleValue();
            double diff = dmax - dmin;
            if (diff == 0) {
                ratio = 1.0;
            } else {
                ratio = (mValues[i].doubleValue() - dmin) / diff;
            }
            if (MINIMUM_RATIO > ratio) {
                ratio = MINIMUM_RATIO;
            }

            double theta = radian * vertex;

            double[] point;
            int x, y;

            int radius = Math.min(mCenter.x, mCenter.y);

            x = (int)((radius-mVertexRadius) * Math.cos(theta) + mCenter.x);
            y = (int)((radius-mVertexRadius) * Math.sin(theta) + mCenter.y);

            point = new double[] {x, y};
            af.transform(point, 0, point, 0, 1);
            mBackgroundVertexes[i] = new Point((int)point[0], (int)point[1]);

            x = (int)(ratio*(radius-(mVertexRadius)) * Math.cos(theta) + mCenter.x);
            y = (int)(ratio*(radius-(mVertexRadius)) * Math.sin(theta) + mCenter.y);
            point = new double[] {x, y};
            af.transform(point, 0, point, 0, 1);

            mVertexes[i] = new Point((int)point[0], (int)point[1]);

            vertex++;
        }
    }

    public void revalidate (int width, int height) {
        mVertexes = null;
        mBackgroundVertexes = null;
        validate(width, height);
    }


    public void draw (Graphics g, int width, int height) {
        revalidate (width, height);
        draw(g);
    }

    public void draw (Graphics g) {

        if (mVertexes == null || mBackgroundVertexes == null)
            throw new IllegalArgumentException("vertexes must be non null");

        Graphics2D g2d = (Graphics2D)g;

        g2d.setColor(mColor);
        g2d.setBackground(mBackground);
        if (isAntialiasing)
            g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, 
                    RenderingHints.VALUE_ANTIALIAS_ON);

        Polygon polygon = new Polygon();
        Polygon bgPolygon = new Polygon();

        Stroke defaultStroke = g2d.getStroke();

        for (int i=0; i<mNumberOfSides; i++) {

            bgPolygon.addPoint(mBackgroundVertexes[i].x, mBackgroundVertexes[i].y);
            polygon.addPoint(mVertexes[i].x, mVertexes[i].y);

            if (i == 0) {
                g2d.setColor(Color.lightGray);
                g2d.fillOval(mBackgroundVertexes[i].x - mVertexRadius,
                             mBackgroundVertexes[i].y - mVertexRadius,
                             mVertexRadius*2, mVertexRadius*2);
            } else {
                g2d.setColor(Color.LIGHT_GRAY);
                g2d.drawOval(mBackgroundVertexes[i].x - mVertexRadius,
                             mBackgroundVertexes[i].y - mVertexRadius,
                             mVertexRadius*2, mVertexRadius*2);
            }

            g2d.setColor(Color.lightGray);
            g2d.setStroke(new BasicStroke(1));
            g2d.drawLine(mCenter.x, mCenter.y,
                    mBackgroundVertexes[i].x, mBackgroundVertexes[i].y);

            g2d.setStroke(defaultStroke);
            g2d.setColor(mColor);
            g2d.drawLine(mCenter.x, mCenter.y,
                    mVertexes[i].x, mVertexes[i].y);
        }
        g2d.setStroke(defaultStroke);

        g2d.setColor(Color.lightGray);
        switch (mBorderType) {
        case BORDER_POLYGON:
            g2d.drawPolygon(bgPolygon);
            break;
        case BORDER_CIRCLE:
            int radius = (int)(Math.min(mCenter.x, mCenter.y)-mVertexRadius);
            int x = mCenter.x - radius;
            int y = mCenter.y - radius;
            g2d.drawOval(x, y, radius*2, radius*2);
            break;
        }

        g2d.setColor(mColor);
        g2d.drawPolygon(polygon);
    }
}
