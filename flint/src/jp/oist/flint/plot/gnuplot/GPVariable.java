/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.plot.gnuplot;

/**
 * This is a property class used by gnuplotter.
 */
public abstract class GPVariable {

    /** Property type. */
    public enum Type {

        GNUPLOT, STRING
    };
    /** Property name. */
    private String name;
    /** Property value. */
    private String value;
    /** The active flag of a property. */
    private boolean activeFlg = true;

    /**
     * A property name is returned.
     *
     * @return String
     */
    public String getName() {
        return name;
    }

    /**
     * The value of a property is set.
     *
     * @param name
     */
    public void setName(String name) {
        this.name = name;
    }

    /**
     * The value of a property is returned.
     *
     * @return String
     */
    public String getValue() {
        return value;
    }

    /**
     * The value of a property is set.
     *
     * @param value
     */
    public void setValue(String value) {
        this.value = value;
    }

    /**
     * It is returned whether a property is active.
     *
     * @return boolean
     */
    public boolean isActive() {
        return activeFlg;
    }

    /**
     *The active value of a property is set.
     *
     * @param active
     */
    public void setActive(boolean active) {
        this.activeFlg = active;
    }

    /**
     * The abstract method which returns the type of a property.
     *
     * @return Type
     */
    public abstract Type getType();

    /**
     * The abstract method which returns property data.
     *
     * @return Object[]
     */
    public abstract Object[] getData();

    /**
     * The value of the data type given by the argument is set.
     *
     * @param i
     * @param value
     */
    public abstract void setData(int i, Object value);
}
