/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.util;

public class Uuid {

    public static String fromByteArray(byte[] bytes) {
        assert bytes.length == 16;
        StringBuilder sb = new StringBuilder();
        for (int i=0;i<16;i++) {
            sb.append(String.format("%02x", bytes[i]));
            if (i == 3 || i == 5 || i == 7 || i == 9)
                sb.append('-');
        }
        return sb.toString();
    }

}
