/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.textformula.mathml;

import jp.oist.flint.textformula.ReservedFunctionContent;
import jp.oist.flint.textformula.ReservedFunctionContent.FunctionInformation;
import java.io.IOException;
import java.util.List;

public class FunctionFormulaNode extends FormulaNode {

    public FunctionFormulaNode(String name) {
        super(name);
    }

    @Override
    protected String formulaToString() throws IOException {
        int argc = children.size();
        List<FunctionInformation> funcInfos =
            ReservedFunctionContent.getFunctionInformationFromMathmlName(name);

        if (funcInfos == null || funcInfos.isEmpty())
            throw new IOException(String.format("%s is not supported yet.", name));

        FunctionInformation funcInfo = null;
        for (FunctionInformation f : funcInfos) {
            if (f.numberOfArguments == argc) {
                funcInfo = f; break;
            }
        }

        if (funcInfo == null) {
            String sNumOfArgs = String.valueOf(funcInfos.get(0).numberOfArguments);
            throw new IOException(
                String.format("Missing arguments %s for `%s` function.",
                sNumOfArgs,
                name));
        }

        String functionName = funcInfo.functionName;
        int startIndex = 0;
        // if logbase is 10, chnage functionName to `log10`
        if ("log".equals(functionName)
                && argc == 2 && children.get(0).toFormulaString().equals("10")) {
                functionName = "log10";
                startIndex = 1;
        }

        StringBuilder sb = new StringBuilder(functionName).append("(");

        for (int i=startIndex; i<children.size(); i++) {
            FormulaNode child = children.get(i);
            sb.append(child.toFormulaString());
            if (i != children.size()-1)
                sb.append(", ");
        }
        sb.append(")");

        return sb.toString();
    }
}
