/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.plot.gnuplot;

/**
 * This is a property class used by gnuplotter.
 */
public class GPGnuplotVariable extends GPVariable {

    /**
     * Property information is returned by a character string.
     *
     * @return String
     */
    public String getPlotString() {
        return getName() + "=" + getValue();
    }

    /**
     * Returns property data.
     *
     * @see GPVariable
     * @return Object[]
     */
    @Override
    public Object[] getData() {
        Object data[] = new Object[4];
        data[0] = GPVariable.Type.GNUPLOT;
        data[1] = getName();
        data[2] = getValue();
        data[3] = isActive();

        return data;
    }

    /**
     * The value of the data type given by the argument is set.
     *
     * @param i
     * @param value
     */
    @Override
    public void setData(int i, Object value) {

        if (i == 1) {
            setName((String) value);
        } else if (i == 2) {
            setValue((String) value);
        } else if (i == 3) {
            setActive((Boolean) value);
        }
    }

    /**
     * Returns the type of a property.
     *
     * @return Type
     */
    @Override
    public Type getType() {
        return GPVariable.Type.GNUPLOT;
    }
}
