/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.util;

import java.text.SimpleDateFormat;
import java.util.Date;

public class PeriodFormat {

    public static String fromTo(Date from, Date to) {
		SimpleDateFormat ymd = new SimpleDateFormat("yyy-MM-dd");
		String fromDate = ymd.format(from);
		String toDate = ymd.format(to);
		SimpleDateFormat hms = new SimpleDateFormat("HH:mm:ss");
		String fromTime = hms.format(from);
		String toTime = hms.format(to);
        StringBuffer sb = new StringBuffer();
        if (fromDate.equals(toDate)) {
			sb.append(fromTime);
			sb.append(" - ");
			sb.append(toTime);
        } else {
            sb.append(fromDate);
			sb.append(' ');
			sb.append(fromTime);
            sb.append(" - ");
            sb.append(toDate);
			sb.append(' ');
			sb.append(toTime);
        }
        return sb.toString();
    }

}
