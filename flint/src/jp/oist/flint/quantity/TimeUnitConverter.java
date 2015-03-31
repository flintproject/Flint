/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.quantity;

import jp.physiome.Ipc;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.MathContext;

public class TimeUnitConverter {

    public static String convert(String v, Ipc.TimeUnit src, Ipc.TimeUnit dst) {
        BigInteger srcN = BigInteger.valueOf(src.getN());
        BigInteger srcD = BigInteger.valueOf(src.getD());
        BigInteger dstN = BigInteger.valueOf(dst.getN());
        BigInteger dstD = BigInteger.valueOf(dst.getD());
        BigInteger n = srcN.multiply(dstD);
        BigInteger d = srcD.multiply(dstN);
        if (n.equals(d)) return v;
        BigDecimal x = new BigDecimal(v).multiply(new BigDecimal(n));
        BigDecimal y = x.divide(new BigDecimal(d), MathContext.DECIMAL64);
        return y.toString();
    }
}
