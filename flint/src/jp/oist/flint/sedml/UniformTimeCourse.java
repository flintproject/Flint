/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.sedml;

import java.math.BigDecimal;

public class UniformTimeCourse {

    int mNumberOfPoints = 0;

    public UniformTimeCourse(ISimulationConfiguration config) throws SedmlException {
        try {
            BigDecimal nop = new BigDecimal(config.getLength()).divideToIntegralValue(new BigDecimal(config.getStep()));
            mNumberOfPoints = nop.intValueExact();
        } catch (ArithmeticException | NumberFormatException ex) {
            throw new SedmlException("invalid configuration: " + ex.getMessage(), ex);
        }
    }

    public int getNumberOfPoints() {
        return mNumberOfPoints;
    }
}
