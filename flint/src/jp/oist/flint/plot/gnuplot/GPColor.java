/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.plot.gnuplot;

import java.awt.Color;
import java.awt.color.ColorSpace;

/**
 * This is a class which manages the color used by gnuplotter.
 *
 * @version 1.0, 03 September, 2011
 * @author miyahira
 */
public class GPColor extends Color {

    /**
     * Constructor.
     *
     * @see Color
     * @param cspace
     * @param components
     * @param alpha
     */
    public GPColor(ColorSpace cspace, float[] components, float alpha) {
        super(cspace, components, alpha);
    }

    /**
     * Constructor.
     *
     * @see Color
     * @param r
     * @param g
     * @param b
     * @param a
     */
    public GPColor(float r, float g, float b, float a) {
        super(r, g, b, a);
    }

    /**
     * Constructor.
     *
     * @see Color
     * @param r
     * @param g
     * @param b
     */
    public GPColor(float r, float g, float b) {
        super(r, g, b);
    }

    /**
     * Constructor.
     *
     * @see Color
     * @param rgba
     * @param hasalpha
     */
    public GPColor(int rgba, boolean hasalpha) {
        super(rgba, hasalpha);
    }

    /**
     * Constructor.
     *
     * @see Color
     * @param r
     * @param g
     * @param b
     * @param a
     */
    public GPColor(int r, int g, int b, int a) {
        super(r, g, b, a);
    }

    /**
     * Constructor.
     *
     * @see Color
     * @param r
     * @param g
     * @param b
     */
    public GPColor(int r, int g, int b) {
        super(r, g, b);
    }

    /**
     * Constructor.
     *
     * @see Color
     * @param rgb
     */
    public GPColor(int rgb) {
        super(rgb);
    }

    /**
     * Constructor.
     *
     * @see Color
     * @param c
     */
    public GPColor(Color c) {
        super(c.getRed(), c.getGreen(), c.getBlue(), c.getAlpha());
    }

    /**
     * The Hex value of each color current value is returned.
     *
     * @return String
     */
    public String getHexString() {
        String s = "";
        String r, g, b;
        r = Integer.toHexString(this.getRed());
        g = Integer.toHexString(this.getGreen());
        b = Integer.toHexString(this.getBlue());
        if (r.length() == 1) {
            r = "0" + r;
        }
        if (g.length() == 1) {
            g = "0" + g;
        }
        if (b.length() == 1) {
            b = "0" + b;
        }
        s += r + "" + g + "" + b;
        return s;
    }

    /**
     * GPColor is returned based on a hex value.
     *
     * @param hex hex value.
     * @return GPColor
     */
    public static GPColor parseHexString(String hex) {
        int r, g, b;
        r = Integer.parseInt(hex.substring(0, 2), 16);
        g = Integer.parseInt(hex.substring(2, 4), 16);
        b = Integer.parseInt(hex.substring(4, 6), 16);

        GPColor c = new GPColor(r, g, b);
        return c;
    }
}
