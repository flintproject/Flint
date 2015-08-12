/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.util;

import java.math.BigDecimal;
import java.math.MathContext;
import java.text.NumberFormat;
import java.util.ArrayList;
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

    private List<BigDecimal> generate(int count) {
        assert mMinimum.compareTo(mMaximum) <= 0;

        ArrayList<BigDecimal> result = new ArrayList<>(count);
        Random random = new Random(mSeed);
        BigDecimal range = mMaximum.subtract(mMinimum);
        for (int i=0;i<count;i++) {
            BigDecimal next = new BigDecimal(random.nextDouble(), MathContext.DECIMAL32);
            result.add(next.multiply(range).add(mMinimum));
        }
        return result;
    }

    public String generateAsString(int count) {
        return generateAsString(count, ",");
    }

    public String generateAsString(int count, String separator) {
        List<BigDecimal> generated = generate(count);
        if (generated.isEmpty())
            return "";

        StringBuilder sb = new StringBuilder(mNumberFormat.format(generated.get(0)));
        int size = generated.size();
        for (int i=1; i<size; i++)
            sb.append(separator).append(mNumberFormat.format(generated.get(i)));
        return sb.toString();
    }
}
