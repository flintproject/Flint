/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.theme;

import java.awt.Image;
import java.net.URL;
import javax.swing.ImageIcon;

public class Icon {

    static final ImageIcon mIcon;

    static {
        URL iconUrl = Icon.class.getResource("/jp/oist/flint/image/icon.png");
        mIcon = new ImageIcon(iconUrl);
    }

    public static ImageIcon getIcon() {
        return mIcon;
    }

    public static Image getImage() {
        return mIcon.getImage();
    }
}
