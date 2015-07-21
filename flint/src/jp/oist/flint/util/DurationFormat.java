/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.util;

import java.util.concurrent.TimeUnit;

public class DurationFormat {

    public static String fromMillis(long millis) {
        long h = TimeUnit.MILLISECONDS.toHours(millis);
        millis -= TimeUnit.HOURS.toMillis(h);
        long m = TimeUnit.MILLISECONDS.toMinutes(millis);
        millis -= TimeUnit.MINUTES.toMillis(m);
        long s = TimeUnit.MILLISECONDS.toSeconds(millis);
        StringBuffer sb = new StringBuffer();
        if (h > 0) {
            sb.append(h);
            sb.append('h');
            sb.append(m);
            sb.append('m');
            sb.append(s);
            sb.append('s');
        } else if (m > 0) {
            sb.append(m);
            sb.append('m');
            sb.append(s);
            sb.append('s');
        } else {
            sb.append(s);
            sb.append('s');
        }
        return sb.toString();
    }

}
