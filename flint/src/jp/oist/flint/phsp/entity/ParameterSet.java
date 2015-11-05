/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.phsp.entity;

import java.util.ArrayList;
import java.util.List;

public class ParameterSet {

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
        return mParameters.toArray(new Parameter[mParameters.size()]);
    }

    public Parameter getParameterByName (String name) {
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
            super.add(new Parameter("defaultValue", "0"));
        }

        @Override
        public void remove (int index) { }

        @Override
        public void removeAll () { }

        @Override
        public void add (Parameter parameter) {}

        @Override
        public ParameterSet clone () {
            return this;
        }
    }
}
