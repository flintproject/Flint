/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.util;

import java.math.BigDecimal;
import java.math.MathContext;
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Random;

public class Randomizer {

    private Long mSeed;

    private BigDecimal mMaximum;

    private BigDecimal mMinimum;

    private NumberFormat mNumberFormat = NumberFormat.getInstance();

    public Randomizer () {
        mSeed = System.currentTimeMillis();
        mMaximum = new BigDecimal(Double.MAX_VALUE);
        mMinimum = new BigDecimal(Double.MIN_VALUE);
    }

    public Randomizer (Long seed, Double max, Double min) {
        mSeed   = seed;
        mMaximum  = new BigDecimal(max);
        mMinimum  = new BigDecimal(min);
    }

    public Randomizer (Long seed, BigDecimal max, BigDecimal min) {
        mSeed = seed;
        mMaximum = max;
        mMinimum = min;
    }

    public Long getSeed () {
        return mSeed;
    }

    public void setSeed(Long seed) {
        mSeed = seed;
    }

    public Double getMaximum () {
        return mMaximum.doubleValue();
    }

    public void setMaximum (BigDecimal max) {
        mMaximum = max;
    }

    public void setMaximum (Double max) {
        mMaximum = new BigDecimal(max);
    }

    public void setMaximum (String max) {
        mMaximum = new BigDecimal(max);
    }

    public Double getMinimum () {
        return mMinimum.doubleValue();
    }

    public void setMinimum (BigDecimal min) {
        mMinimum = min;
    }

    public void setMinimum (Double min) {
        mMinimum = new BigDecimal(min);
    }

    public void setMinimum (String min) {
        mMinimum = new BigDecimal(min);
    }

    public void setNumberFormat (NumberFormat nf) {
        mNumberFormat = nf;
    }

    public NumberFormat getNumberFormat () {
        return mNumberFormat;
    }

    public List<String> generate (long count) {
        HashSet<String> retval = new HashSet<>();

        Random random = new Random(mSeed);

        if (mMinimum.compareTo(mMaximum) >= 0)
            return new ArrayList(retval);

        BigDecimal range = mMaximum.subtract(mMinimum);

        while (retval.size() < count) {
            BigDecimal next = new BigDecimal(random.nextDouble(), MathContext.DECIMAL32);
            String value = mNumberFormat.format(next.multiply(range).add(mMinimum));
            retval.add(value);
        }

        return new ArrayList(retval);
    }

    public String generateAsString (long count) {
        return generateAsString(count, ",");
    }

    public String generateAsString (long count, String separator) {
        List<String> numList = generate(count);
        if (numList == null || numList.isEmpty()) 
            return "";

        StringBuilder sb = new StringBuilder();

        for (String sNum : numList)
            sb.append(sNum).append(separator);

        String retval = sb.toString();
        retval = retval.substring(0, retval.length()-separator.length());
        return retval;
    }
}
