/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */

package jp.oist.flint.phsp;

import org.apache.log4j.Logger;
import java.math.BigDecimal;
import java.math.MathContext;
import java.util.ArrayList;
import java.util.List;
import jp.oist.flint.phsp.entity.ParameterSet;

public class PhspUtility {

    public static int calculateCombinationCount (ParameterSet pset) {
        int combinationCount = -1;

        for (ParameterSet.Parameter p : pset.getParameters()) {
            int size = 1;
            switch(p.getType()) {
            case ENUM:
                size = getEnumValue(p.getEnumValue()).length;
                break;
            case INTERVAL:
                size = getIntervalValue(p.getRangeLower(), 
                                        p.getRangeUpper(), 
                                        p.getRangeStep()).length;
                break;
            }

            if (size == 0)
                continue;

            if (combinationCount == -1) {
                combinationCount = size;
            } else {
                combinationCount *= size;
            }
        }
        return combinationCount;
    }

    public static Number[] getIntervalValue (String sLower, String sUpper, String sStep) {
        return getIntervalValue(sLower, sUpper, sStep, -1);
    }

    public static Number[] getIntervalValue (String sLower, String sUpper, String sStep, int maxCount) {
        List<Number> nums = new ArrayList<>();
        BigDecimal step, upper, lower;
        int count=0;

        try { // As double.
            lower = new BigDecimal(sLower, MathContext.DECIMAL128);
            upper = new BigDecimal(sUpper, MathContext.DECIMAL128);
            step  = new BigDecimal(sStep,  MathContext.DECIMAL128);

            if (lower.compareTo(upper) > 0 || BigDecimal.ZERO.equals(step))
                return new Number[0];

            for (BigDecimal i=lower; i.compareTo(upper)<=0 && count!=maxCount; 
                    i=i.add(step, MathContext.DECIMAL128), count++)
                nums.add(i.doubleValue());

            return nums.toArray(new Number[nums.size()]);
        } catch (NumberFormatException ex) { }

        Logger.getRootLogger().error("Could not parse interval values.");
        return new Number[0];
    }

    public static Number[] getEnumValue (String s) {
        return getEnumValue(s, -1);
    }

    public static Number[] getEnumValue (String s, int maxCount) {
        String[] vars = s.split(",");
        List<Number> nums = new ArrayList<>();
        try {
            for (int i=0; i!=maxCount && i<vars.length; i++) {
                String v = vars[i].trim();
                Number n = Double.parseDouble(v);
                nums.add(n);
            }
        } catch (NumberFormatException ex) {
            return nums.toArray(new Number[nums.size()]);
        }

        return nums.toArray(new Number[nums.size()]);
    }

    // DO NOT create an instance.
    private PhspUtility () {} 
}
