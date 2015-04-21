/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.textformula.mathml;

import java.io.IOException;

public class AssignmentFormulaNode extends FormulaNode {

    public AssignmentFormulaNode(String name) {
        super(name);
    }

    @Override
    protected String formulaToString() throws IOException {
        StringBuilder sb = new StringBuilder();
        if (children.size() == 2) {
            FormulaNode left = children.get(0);
            FormulaNode right = children.get(1);

            sb.append(left.toFormulaString())
                .append(" "+operator+" ")
                .append(right.toFormulaString());
            return sb.toString();
        }

        throw new IOException("Missing arguments is 2 for Assignment \"=\"");
    }

    @Override
    protected String getOperator(String name) {
        return "=";
    }
}
