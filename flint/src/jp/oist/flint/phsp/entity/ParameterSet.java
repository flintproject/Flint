/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.phsp.entity;

import jp.oist.flint.phsp.PhspException;
import java.math.BigDecimal;
import java.math.MathContext;
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import jp.oist.flint.phsp.PhspUtility;
import jp.oist.flint.util.Utility;

public class ParameterSet {

    public enum ParameterType {
        ENUM,
        INTERVAL
    }

    private final List<Parameter> mParameters;

    public ParameterSet () {
        mParameters = new ArrayList<>();
    }

    ParameterSet (List<Parameter> ps) {
        mParameters = ps;
    }

    public int size () {
        return mParameters.size();
    }

    public void removeEmptyParameters () {
        ArrayList<Parameter> targets = new ArrayList<>();
        for (Parameter p : mParameters) {
            if (p.isEmpty()) targets.add(p);
        }

        int length = targets.size();
        for (int i=0; i<length; i++)
            mParameters.remove(targets.get(i));
    }

    public Parameter[] getParameters () {
        return (Parameter[])mParameters
                .toArray(new Parameter[mParameters.size()]); 
    }

    public ParameterSet.Parameter getParameterByName (String name) {
        if (name == null)
            return null;

        for (Parameter p : mParameters) {
            if (name.equals(p.getName()))
                return p;
        }
        return null;
    }

    public ParameterSet filterByNames (List<String> filterNames) {
        ArrayList<Parameter> params = new ArrayList<>();
        for (String filterName : filterNames) {
            for (Parameter p : mParameters) {
                if (filterName.equals(p.getName())) {
                    params.add(p);
                    break;
                }
            }
        }

        return (params.size()>0)? new ParameterSet(params) : new Dummy();
    }

    public Parameter get (int index) {
        return mParameters.get(index);
    }

    public void remove (int index) {
        mParameters.remove(index);
    }

    public void removeAll () {
        mParameters.clear();
    }

    public void add (Parameter parameter) {
        mParameters.add(parameter);
    } 

    public Number parseNumber (String s) {
        Number retval;
        try {
            retval = Long.parseLong(s);
        } catch (NumberFormatException ex) {
            retval = Double.parseDouble(s);
        }

        return retval;
    }

    public boolean isEmpty () {
        return size() == 0;
    }

    public ParameterSet deepClone () {
        ParameterSet clone = new ParameterSet();
        for (Parameter p : mParameters) 
            clone.mParameters.add(p.deepClone());
        return clone;
    }

    @Override
    public boolean equals(Object object) {
        if (!(object instanceof ParameterSet))
            return false;
        ParameterSet other = (ParameterSet)object;
        if (size() != other.size())
            return false;

        for (int i=0; i<size(); i++) {
            Parameter p1 = get(i);
            Parameter p2 = other.get(i);

            if (p1.equals(p2) == false)
                return false;
        }
        return true;
    }

    public static class Dummy extends ParameterSet {
        public Dummy () {
            super.add(new ParameterSet.Parameter("defaultValue", "0"));
        }

        @Override
        public void remove (int index) { }

        @Override
        public void removeAll () { }

        @Override
        public void add (ParameterSet.Parameter parameter) {}

        @Override
        public ParameterSet clone () {
            return this;
        }
    }

    public static class Parameter {

        public final static String TYPE = "type";

        public final static String NAME = "name";

        public final static String ENUM_VALUE = "enum.value";

        public final static String RANGE_LOWER = "interval.lower";

        public final static String RANGE_UPPER = "interval.upper";

        public final static String RANGE_STEP = "interval.step";

        /* 
         * Optional Parameter Keys
         */
        public final static String RANDOMIZER_COUNT = "enum.randomizer.count";

        public final static String RANDOMIZER_SEED = "enum.randomizer.seed";

        public final static String RANDOMIZER_LOWER_LIMIT = "enum.randomizer.lowerlimit";

        public final static String RANDOMIZER_UPPER_LIMIT = "enum.randomizer.upperlimit";

        public final static String USE_RANDOMIZER = "interval.randomizer";

        private HashMap<String, Object> mData;

        public Parameter () {
            List<String> keys = Arrays.asList(new String[] {
                TYPE, NAME, ENUM_VALUE, RANGE_LOWER, RANGE_UPPER, RANGE_STEP,
            });
            mData = new HashMap<>();

            for (String key : keys) mData.put(key, "");

            mData.put(TYPE, ParameterType.ENUM);
            mData.put(ENUM_VALUE, "");
        }

        public Parameter (String name, String enumVar) {
            this();

            mData.put(NAME, name);
            mData.put(TYPE, ParameterType.ENUM);
            mData.put(ENUM_VALUE, enumVar);
        }

        public Parameter (String name, String lower, String upper, String step) {
            this();

            mData.put(NAME, name);
            mData.put(TYPE, ParameterType.INTERVAL);
            mData.put(RANGE_LOWER, lower);
            mData.put(RANGE_UPPER, upper);
            mData.put(RANGE_STEP, step);
        }

        public void validate() throws PhspException {
            validateName();
            validateValue();
        }

        public void validateName() throws PhspException {

            String name = getName();

            if ((name == null || name.trim().isEmpty()))
                throw new PhspException("name is empty");

            name = name.trim();

            String validationRegex = "([a-zA-Z_])+([a-zA-Z_]|[0-9])*";

            if (name.matches(validationRegex))
                return;

            throw new PhspException("invalid name");
        }

        public void validateValue() throws PhspException {
            switch (getType()) {
            case ENUM:
                validateEnum();
                break;
            case INTERVAL:
                validateInterval();
                break;
            }
        }

        private void validateEnum() throws PhspException {
            String enumValue = getEnumValue();

            if (enumValue == null || enumValue.trim().isEmpty())
                throw new PhspException("enum value is empty");


            enumValue = enumValue.trim();
            String validationRegex = "^([+-]?[0-9]*[.]?[0-9]+)([,][+-]?[0-9]*[.]?[0-9]+)*$";

            if (enumValue.matches(validationRegex))
                return;

            throw new PhspException("invalid enum value");
        }

        private void validateInterval() throws PhspException {
            String rangeLower = getRangeLower();
            String rangeUpper = getRangeUpper();
            String rangeStep  = getRangeStep();

            if (rangeLower == null || rangeLower.trim().isEmpty())
                throw new PhspException("range lower is empty");

            if (rangeUpper == null || rangeUpper.trim().isEmpty())
                throw new PhspException("range upper is empty");

            if (rangeStep == null || rangeStep.trim().isEmpty())
                throw new PhspException("range step is empty");

            try {
                Double.valueOf(rangeLower);
            } catch (NumberFormatException ex) {
                throw new PhspException("invalid number format for range's lower", ex);
            }
            try {
                Double.valueOf(rangeUpper);
            } catch (NumberFormatException ex) {
                throw new PhspException("invalid number format for range's upper", ex);
            }
            try {
                Double.valueOf(rangeStep);
            } catch (NumberFormatException ex) {
                throw new PhspException("invalid number format for range's step", ex);
            }
        }

        public String[] getKeys () {
            int length = mData.size();
            return mData.keySet().toArray(new String[length]);
        }

        public boolean isEmpty () {
            ParameterType type = getType();
            String values = toString();
            return ((values == null) || values.isEmpty())
                    && (getName() == null || getName().isEmpty());
        }

        public Object get (String key) {
            return mData.get(key);
        }

        public void set (String key, Object value) {
            mData.put(key, value);
        }

        public ParameterType getType() {
            return (ParameterType)get(TYPE);
        }

        public void setType(ParameterType type) {
            set(TYPE, type);
        }

        public String getName() {
            return (String)get(NAME);
        }

        public void setName(String name) {
            set(NAME, name);
        }

        public String getEnumValue() {
            return (String)get(ENUM_VALUE);
        }

        public void setEnumValue(String enumValue) {
            set(ENUM_VALUE, enumValue);
        }

        public String getRangeLower() {
            return (String)get(RANGE_LOWER);
        }

        public void setRangeLower(String lower) {
            set(RANGE_LOWER, lower);
        }

        public String getRangeUpper() {
            return (String)get(RANGE_UPPER);
        }

        public void setRangeUpper(String upper) {
            set(RANGE_UPPER, upper);
        }

        public String getRangeStep() {
            return (String)get(RANGE_STEP);
        }

        public void setRangeStep(String step) {
            set(RANGE_STEP, step);
        }

        @Override
        public String toString () {
            Double step, lower, upper;
            Number[] result;

            StringBuilder sb = new StringBuilder();
            String separator = ", ";
            String retval = "";

            if (ParameterType.INTERVAL.equals(getType())) {
                try {
                    lower = Double.parseDouble(getRangeLower());
                    upper = Double.parseDouble(getRangeUpper());
                    step  = Double.parseDouble(getRangeStep());
                } catch (NumberFormatException ex2) {
                    return "";
                }

                int period = 20;
                result = PhspUtility.getIntervalValue(getRangeLower(), 
                                                      getRangeUpper(), 
                                                      getRangeStep(), period);

                if (result.length == 0)
                    return "";

                int dpStep  = Utility.getDecimalPlace(step);
                int dpLower = Utility.getDecimalPlace(lower);
                int dpUpper = Utility.getDecimalPlace(upper);
                BigDecimal bdStep = new BigDecimal(getRangeStep(),  MathContext.DECIMAL64);
                BigDecimal bdLower = new BigDecimal(getRangeLower(), MathContext.DECIMAL64);
                BigDecimal bdUpper = new BigDecimal(getRangeUpper(), MathContext.DECIMAL64);

                if (BigDecimal.ZERO.compareTo(bdStep) == 0)
                    return "";

                BigDecimal bdTotal  = bdLower.add(
                        bdStep.multiply(new BigDecimal(period), MathContext.DECIMAL64), 
                        MathContext.DECIMAL64);

                int decimalPoint = Math.max(dpStep, Math.max(dpLower, dpUpper));
                NumberFormat nf = (NumberFormat)NumberFormat.getNumberInstance().clone();
                nf.setMinimumFractionDigits(decimalPoint);

                for (Number v : result)
                    sb.append(nf.format(v)).append(separator);

                retval = sb.substring(0, sb.length()-separator.length());

                if (bdUpper.compareTo(bdTotal) > 0)
                    retval += "...";

            } else if (ParameterType.ENUM.equals(getType())) {
                result = PhspUtility.getEnumValue(getEnumValue());
                if (result.length == 0)
                    return "";

                for (Number v : result)
                    sb.append(v.toString()).append(separator);

                retval = sb.substring(0, sb.length()-separator.length());
            }


            return retval;
        }

        public Parameter deepClone () {
            Parameter clone = new Parameter();
            for (String key : mData.keySet())
                clone.mData.put(key, mData.get(key));
            return clone;
        }

        @Override
        public boolean equals(Object object) {
            if (!(object instanceof Parameter))
                return false;
            Parameter other = (Parameter)object;
            for (String key : mData.keySet()) {
                if (get(key).equals(other.get(key)) == false)
                    return false;
            }

            return true;
        }
    }
}
