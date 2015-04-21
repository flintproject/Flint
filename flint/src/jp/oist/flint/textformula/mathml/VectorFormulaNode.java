/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.textformula.mathml;

import java.io.IOException;

public class VectorFormulaNode extends FormulaNode {

    public VectorFormulaNode(String name) {
        super(name);
    }

    @Override
    protected String formulaToString() throws IOException {
        StringBuilder sb = new StringBuilder("{");
        for (FormulaNode node : children) {
            sb.append(node.toFormulaString())
                .append(", ");
        }
        String formula = sb.toString();
        formula = formula.substring(0, formula.length()-2);
        return formula + "}";
    }
}
