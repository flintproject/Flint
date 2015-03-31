/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.sedml;

import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.XMLReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;
import javax.xml.parsers.ParserConfigurationException;

public class SedmlReader {

    final File mFile;
    final SedmlHandler mHandler;

    public SedmlReader(final File file) {
        this.mFile = file;
        this.mHandler = new SedmlHandler2();
    }

    public SedmlHandler getHandler() {return mHandler;}

    public boolean parse() throws IOException, ParserConfigurationException, SAXException {
        SAXParserFactory spf = SAXParserFactory.newInstance();
        spf.setNamespaceAware(true);
        SAXParser sp = spf.newSAXParser();
        XMLReader xmlReader = sp.getXMLReader();
        xmlReader.setContentHandler(mHandler);
        InputSource is = new InputSource(new FileInputStream(mFile));
        xmlReader.parse(is);
        return true;
    }
}
