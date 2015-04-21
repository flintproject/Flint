/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.textformula.mathml;

import org.w3c.dom.Document;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import java.io.IOException;
import java.io.Reader;
import java.io.InputStream;

public class MathML2TextFormula {

    public final static String MATHML_NAMESPACE_URI = "http://www.w3.org/1998/Math/MathML";

    private final DocumentBuilderFactory mFactory;

    private final MathMLHandler mHandler;

    public MathML2TextFormula() {
        mFactory = DocumentBuilderFactory.newInstance();
        mFactory.setNamespaceAware(true);
        mFactory.setXIncludeAware(false);

        mHandler = new MathMLHandler();
    }

    public void parse(Reader reader)
        throws IOException, ParserConfigurationException, SAXException {
        parse(new InputSource(reader));
    }

    public void parse(InputStream stream)
        throws IOException, ParserConfigurationException, SAXException {
        parse(new InputSource(stream));
    }

    public void parse(InputSource source)
        throws IOException, ParserConfigurationException, SAXException {
            DocumentBuilder builder = mFactory.newDocumentBuilder();
            Document document = builder.parse(source);
            mHandler.parse(document);
    }

    public String getTextFormula () throws IOException {
        return mHandler.getFormulaString();
    }

}
