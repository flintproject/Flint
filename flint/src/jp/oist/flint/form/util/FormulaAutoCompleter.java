/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */

package jp.oist.flint.form.util;

import org.xml.sax.SAXException;
import java.io.IOException;
import java.io.StringReader;
import javax.swing.text.JTextComponent;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.TransformerException;
import jp.oist.flint.textformula.ReservedFunctionContent;
import jp.oist.flint.textformula.ReservedFunctionContent.FunctionInformation;
import jp.oist.flint.textformula.TextFormula2MathML;
import jp.oist.flint.textformula.analyzer.ParseException;
import jp.oist.flint.textformula.mathml.MathML2TextFormula;

public class FormulaAutoCompleter extends AutoCompleter {

    private final TextFormula2MathML formula2mml;

    private final MathML2TextFormula mml2formula;

    public FormulaAutoCompleter(JTextComponent textComponent) {
        super(textComponent);

        formula2mml = new TextFormula2MathML();
        mml2formula = new MathML2TextFormula();

        insertFunctionCompletions();
    }

    private void insertFunctionCompletions () {
        for (FunctionInformation funcInfo : ReservedFunctionContent.functions)
            addFunctionInfomation(funcInfo); 
    }

    private void addFunctionInfomation (FunctionInformation funcInfo) {
        String funcName = funcInfo.functionName;
        int argc = funcInfo.numberOfArguments;

        StringBuilder sb = new StringBuilder(funcName);
        sb.append("(");
        for (int i=0; i<argc; i++) 
            sb.append(String.format("v%s, ",i));

        String func = sb.toString().substring(0, sb.length()-2)+")";
        super.add(func, "Function");
    }

    public void setMathML(String t)
        throws IOException, ParserConfigurationException, SAXException {
        mml2formula.parse(new StringReader(t));
        setText(mml2formula.getTextFormula());
    }

    public String getTextAsMathML()
        throws ParseException, ParserConfigurationException, TransformerException {
        formula2mml.parse(new StringReader(getText()));

        return formula2mml.getMathML();
    }

    @Override
    public void clear() {
        super.clear();
        insertFunctionCompletions();
    }
}
