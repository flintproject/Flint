/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.textformula.mathml;

import java.io.IOException;

public class RootFormulaNode extends FormulaNode {

    public RootFormulaNode(String name) {
        super(name);
    }

    @Override
    protected String formulaToString() throws IOException {
        if (children.size() == 1)
            return children.get(0).toFormulaString();
        throw new IOException("math element does NOT have children.");
    }
}
