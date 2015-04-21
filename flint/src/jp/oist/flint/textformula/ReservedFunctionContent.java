/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.textformula;

public class ReservedFunctionContent {

    public static class FunctionInformation {

        public final String functionName;

        public final String mathmlName;

        public final int numberOfArguments;

        public FunctionInformation(String sn, String mn, int n) {
            this.functionName = sn;
            this.mathmlName = mn;
            this.numberOfArguments = n;
        }
    }

    public final static FunctionInformation[] functions;

    static {
        /**
         * text, mathml, number of arguments
         */
        functions = new FunctionInformation[] {
            new FunctionInformation("pow", "power", 2),

            new FunctionInformation("sin", "sin", 1),
            new FunctionInformation("cos", "cos", 1),
            new FunctionInformation("tan", "tan", 1),

            new FunctionInformation("sinh", "sinh", 1),
            new FunctionInformation("cosh", "cosh", 1),
            new FunctionInformation("tanh", "tanh", 1),

            new FunctionInformation("asin", "arcsin", 1),
            new FunctionInformation("acos", "arccos", 1),
            new FunctionInformation("atan",   "arctan", 1),

            new FunctionInformation("asinh", "arcsinh", 1),
            new FunctionInformation("acosh", "arccosh", 1),
            new FunctionInformation("atanh", "arctanh", 1),

            new FunctionInformation("sec", "sec", 1),
            new FunctionInformation("csc", "csc", 1),
            new FunctionInformation("cot", "cot", 1),

            new FunctionInformation("sech", "sech", 1),
            new FunctionInformation("csch", "csch", 1),
            new FunctionInformation("coth", "coth", 1),

            new FunctionInformation("asec", "arcsec", 1),
            new FunctionInformation("acsc", "arccsc", 1),
            new FunctionInformation("acot", "arccot", 1),

            new FunctionInformation("asech", "arcsech", 1),
            new FunctionInformation("acsch", "arccsch", 1),
            new FunctionInformation("acoth", "arccoth", 1),

            new FunctionInformation("exp",   "exp", 1),
            new FunctionInformation("ln",    "ln",  1),
            new FunctionInformation("log",   "ln",  1),
            new FunctionInformation("log",   "log", 2),
            new FunctionInformation("log10", "log", 1),

            new FunctionInformation("fabs",  "abs",   1),
            new FunctionInformation("sqrt",  "root",  1),
            new FunctionInformation("sqrt",  "root",  2),
            new FunctionInformation("floor", "floor", 1),

            new FunctionInformation("diff", "diff", 2),

            new FunctionInformation("inverse",   "inverse",   1),
            new FunctionInformation("transpose", "transpose", 1),

            new FunctionInformation("determinant", "determinant", 1),
            new FunctionInformation("det",         "determinant", 1),

            new FunctionInformation("outerproduct",  "outerproduct",  2),

            new FunctionInformation("vectorproduct", "vectorproduct", 2),

            new FunctionInformation("scalarproduct", "scalarproduct", 2),
            new FunctionInformation("innerproduct",  "scalarproduct", 2),

            new FunctionInformation("grad", "grad", 1),

            new FunctionInformation("divergence", "divergence", 1),
            new FunctionInformation("div",        "divergence", 1),

            new FunctionInformation("laplacian", "laplacian", 1),

            new FunctionInformation("curl", "curl", 1),
            new FunctionInformation("rot",  "curl", 1),
        };
    }

    public static int indexOfFunctionName (String sn) {
        return indexOfFunctionName(0, sn);
    }

    public static int indexOfFunctionName (int startIndex, String sn) {
        for (int i=startIndex; i<functions.length; i++)
            if (functions[i].functionName.equals(sn)) return i;
        return -1;
    }

    public static int indexOfMathmlName (String mn) {
        return indexOfMathmlName(0, mn);
    }

    public static int indexOfMathmlName (int startIndex, String mn) {
        for (int i=startIndex; i<functions.length; i++)
            if (functions[i].mathmlName.equals(mn)) return i;
        return -1;
    }

    public static java.util.List<FunctionInformation> getFunctionInformationFromFunctionName (String sn) {
            java.util.ArrayList<FunctionInformation> funcs =
                new java.util.ArrayList<>();
                int index = 0;
                while ((index=indexOfFunctionName(index, sn))>=0) {
                    funcs.add(functions[index]);
                    index++;
                }

            return funcs;
    }

    public static java.util.List<FunctionInformation> getFunctionInformationFromMathmlName (String mn) {
            java.util.ArrayList<FunctionInformation> funcs =
                new java.util.ArrayList<>();
                int index = 0;
                while ((index=indexOfMathmlName(index, mn))>=0) {
                    funcs.add(functions[index]);
                    index++;
                }
            return funcs;
    }
}
