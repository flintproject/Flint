/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.textformula.mathml;

import java.io.IOException;

public class CustomFunctionFormulaNode extends FormulaNode {

    public CustomFunctionFormulaNode(String name) {
        super(name);
    }

    @Override
    protected String formulaToString() throws IOException {
        String functionName = this.name;
        int argc = children.size();

        StringBuilder sb = new StringBuilder(functionName).append("(");
        for (int i=1; i<argc; i++) {
            FormulaNode child = children.get(i);
            sb.append(child.toFormulaString());
            if (i != children.size()-1)
                sb.append(", ");
        }
        sb.append(")");

        return sb.toString();

    }
}
