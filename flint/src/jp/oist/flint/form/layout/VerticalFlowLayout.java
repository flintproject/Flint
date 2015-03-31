/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.form.layout;

import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Insets;

public class VerticalFlowLayout extends FlowLayout {

    public VerticalFlowLayout () {
        this(FlowLayout.LEFT, 0, 0);
    }

    public VerticalFlowLayout (int align) {
        this(align, 0, 0);
    }


    public VerticalFlowLayout (int aling, int hgpp, int vgpp) {
        super(aling, hgpp, vgpp);
    }

    @Override
    public Dimension preferredLayoutSize (Container target) {
        synchronized (target.getTreeLock()) {
            Dimension dim = new Dimension(0, 0);
            boolean firstVisibleComponent = true;

            int width = target.getSize().width;
            int totalWidth = 0;
            int maxHeight = -1;

            for (Component m : target.getComponents()) {
                if (!m.isVisible())
                    continue;
                Dimension d = m.getPreferredSize();
                dim.width = Math.max(dim.width, d.width);

                if (firstVisibleComponent) {
                    firstVisibleComponent = false;
                    maxHeight = d.height;
                } else {
                    dim.height += getVgap(); 
                }

                totalWidth += d.width;
                if (totalWidth > width) {
                    dim.height += maxHeight;
                    totalWidth = d.width;
                    maxHeight = d.height;
                }
            }

            if (width > totalWidth)
                dim.height += maxHeight;

            Insets insets = target.getInsets();
            dim.width += insets.left + insets.right + getHgap() * 2;
            dim.height += insets.top + insets.bottom + getVgap() * 2;

            return dim;
        }
    }



    @Override
    public Dimension minimumLayoutSize (Container target) {
        synchronized (target.getTreeLock()) {
            Dimension dim = new Dimension(0, 0);
            boolean firstVisibleComponent = true;

            for (Component m : target.getComponents()) {
                if (!m.isVisible())
                    continue;

                Dimension d = m.getPreferredSize();
                dim.width = Math.max(dim.width, d.width);
                if (firstVisibleComponent) {
                    firstVisibleComponent = false;
                } else { 
                    dim.height += getVgap();
                }
                dim.height += d.height;
            }
            Insets insets = target.getInsets();
            dim.width += insets.left + insets.right + getHgap() * 2;
            dim.height += insets.top + insets.bottom + getVgap() * 2;

            return dim;
        }
    }





    
}
