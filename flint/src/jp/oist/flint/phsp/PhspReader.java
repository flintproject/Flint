/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.phsp;

import org.apache.log4j.Logger;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.StringReader;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.concurrent.ExecutionException;
import javax.swing.SwingWorker;
import javax.xml.XMLConstants;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;
import javax.xml.validation.Schema;
import javax.xml.validation.SchemaFactory;
import javax.xml.validation.Validator;

import jp.oist.flint.phsp.entity.Model;
import jp.oist.flint.phsp.entity.Model.ModelFormat;
import jp.oist.flint.phsp.entity.ParameterSet;
import jp.oist.flint.phsp.entity.TargetSet;
import jp.oist.flint.textformula.mathml.MathML2TextFormula;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.xml.sax.SAXException;

public class PhspReader extends SwingWorker <IPhspConfiguration, Model> {

    private final File mPhspFile;

    private String mBasePath;

    private IPhspConfiguration mConfiguration;

    public PhspReader (File phspFile) throws IOException {
        mPhspFile = phspFile;

        if (!phspFile.exists())
            throw new IOException("file (%s) does not exist.");

        mBasePath = mPhspFile.getParent();

        mConfiguration = null;
    }

    @Override
    protected void done () {
        try {
            get();
        } catch (InterruptedException | ExecutionException ex) {
            Logger.getRootLogger().error(ex.getMessage());
        }
    }

    @Override
    protected IPhspConfiguration doInBackground()
        throws IOException, ParserConfigurationException, PhspException, SAXException, TransformerException {
        if (mConfiguration != null)
            return mConfiguration;

        if (!mPhspFile.exists()) {
            String msg = String.format("`%s` does not exist.", mPhspFile.getPath());
            throw new FileNotFoundException(msg);
        }

        DocumentBuilderFactory dbfactory = DocumentBuilderFactory.newInstance(); 
        dbfactory.setSchema(newSchema());
        dbfactory.setNamespaceAware(true);
        dbfactory.setValidating(false);

        DocumentBuilder builder = dbfactory.newDocumentBuilder();
        Document document = builder.parse(mPhspFile);

        Element phspElmt = document.getDocumentElement();
        List<Model> models = visitPhsp(phspElmt);
        mConfiguration = new PhspConfiguration(models);

        return mConfiguration;
    }

    public void validate () throws SAXException, IOException {
        Schema schema = newSchema();
        Validator validator = schema.newValidator();
        validator.validate(new StreamSource(new FileInputStream(mPhspFile)));
    }

    public Schema newSchema () throws SAXException {
        SchemaFactory sf = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI);
        Source source = new StreamSource(getClass().getResourceAsStream(PhspConstant.PHSP_SCHEMA_PATH));
        return sf.newSchema(source);
    }

    private List<Model> visitPhsp(Element element)
        throws IOException, ParserConfigurationException, PhspException, SAXException, TransformerException {
        List<Model> models = new ArrayList<>();
        int length = element.getChildNodes().getLength();
        for (int i=0; i<length; i++) {
            Node item = element.getChildNodes().item(i);
            if (item.getNodeType() != Node.ELEMENT_NODE)
                continue;
            Element child = (Element)item;
            if ("model".equals(child.getLocalName())) {
                Model model = visitModel(child);
                models.add(model);
                firePropertyChange("LOADED", null, model);
            }
        }
        return models;
    }

    private Model visitModel(Element element)
        throws IOException, ParserConfigurationException, PhspException, SAXException, TransformerException {
        String fmt    = element.getAttribute("format");
        String iref   = element.getAttribute("iref");

        if (fmt == null)
            throw new PhspException("model without format");
        ModelFormat format;
        try {
            format = ModelFormat.valueOf(fmt.trim().toUpperCase(Locale.ENGLISH));
        } catch (IllegalArgumentException iae) {
            throw new PhspException("unknown format of model: " + fmt, iae);
        }

        if (!new File(iref).isAbsolute())
            iref = mBasePath + File.separator + iref;
        File modelFile     = new File(iref);

        TargetSet targetSet = null;
        ParameterSet parameterSet = null;
        String description = "";
        Logger.getRootLogger().debug("Model : " + iref);

        int length = element.getChildNodes().getLength();
        for (int i=0; i<length; i++) {
            Node item = element.getChildNodes().item(i);
            if (item.getNodeType() != Node.ELEMENT_NODE) 
                continue;
            Element child = (Element)item;
            String localName = child.getLocalName().toLowerCase();
            if ("parameter-set".equals(localName))
                parameterSet = visitParameterSet(child);

            if ("target-set".equals(localName))
                targetSet = visitTargetSet(child, format);

            if ("description".equals(localName))
                description = child.getTextContent(); 
        }

        return new Model(format, modelFile, iref,
                    description, parameterSet, targetSet);
    }

    private ParameterSet visitParameterSet(Element element)
        throws PhspException {
        ParameterSet parameterSet = new ParameterSet();
        int length = element.getChildNodes().getLength();
        for (int i=0; i<length; i++) {
            Node item = element.getChildNodes().item(i);
            if (item.getNodeType() != Node.ELEMENT_NODE) 
                continue;
            Element child = (Element)item;
            String localName = child.getLocalName().toLowerCase();
            if ("parameter".equals(localName)) {
                ParameterSet.Parameter pp = visitParameter(child);
                pp.setName(child.getAttribute("name"));
                    parameterSet.add(pp);
            }
        }
        return parameterSet;
    }

    private ParameterSet.Parameter visitParameter(Element element)
        throws PhspException {
        int length = element.getChildNodes().getLength();
        ParameterSet.Parameter pp = new ParameterSet.Parameter();

        for (int i=0; i<length; i++) {
            Node item = element.getChildNodes().item(i);
            if (item.getNodeType() != Node.ELEMENT_NODE) 
                continue;
            Element child = (Element)item;
            String localName = child.getLocalName().toLowerCase();
            if ("range".equals(localName)) {
                String type = child.getAttribute("type");
                if (type == null)
                    throw new PhspException("range without type");
                ParameterSet.ParameterType eType;
                try {
                    eType = ParameterSet.ParameterType.valueOf(type.trim().toUpperCase(Locale.ENGLISH));
                } catch (IllegalArgumentException iae) {
                    throw new PhspException("unknown type of range: " + type, iae);
                }
                pp.setType(eType);
                pp.setRangeLower(child.getAttribute("lower"));
                pp.setRangeUpper(child.getAttribute("upper"));
                pp.setRangeStep(child.getAttribute("step"));
                pp.setEnumValue(child.getTextContent());
            }
        }
        return pp;
    }

    private TargetSet visitTargetSet (Element element, ModelFormat format) 
        throws IOException, ParserConfigurationException, SAXException, TransformerException {
        TargetSet targetSet = new TargetSet();
        int length = element.getChildNodes().getLength();
        for (int i=0; i<length; i++) {
            Node item = element.getChildNodes().item(i);
            if (item.getNodeType() != Node.ELEMENT_NODE) 
                continue;
            Element child = (Element)item;
            String localName = child.getLocalName().toLowerCase();
            if ("target".equals(localName)) {
                TargetSet.Target target = visitTarget(child, format);
                    targetSet.add(target);
            }
        }
        return targetSet;
    }

    private TargetSet.Target visitTarget(Element element, ModelFormat format) 
        throws IOException, ParserConfigurationException, SAXException, TransformerException {
        TargetSet.Target target = new TargetSet.Target();
        if (element.hasAttribute("module-id")) {
            target.setModuleId(element.getAttribute("module-id"));
            target.setPhysicalQuantityId(element.getAttribute("physical-quantity-id"));
        } else {
            target.setSpeciesId(element.getAttribute("species-id"));
        }
        int length = element.getChildNodes().getLength();
        for (int i=0; i<length; i++) {
            Node item = element.getChildNodes().item(i);
            if (item.getNodeType() != Node.ELEMENT_NODE) 
                continue;
            Element child = (Element)item;
            String localName = child.getLocalName().toLowerCase();
            if ("value".equals(localName)) {
                target.setValue(visitValue(child));
                break;
            }
        }

        return target;
    }

    private String visitValue(Element element)
        throws IOException, ParserConfigurationException, SAXException, TransformerException {
        int length = element.getChildNodes().getLength();
        Element child = null;
        for (int i=0; i<length; i++) {
            Node item = element.getChildNodes().item(i);
            if (item.getNodeType() != Node.ELEMENT_NODE)
                continue;
            child = (Element)item;
            String localName = child.getLocalName().toLowerCase();
            if ("math".equals(localName))
                break;

            child = null;
        }
        if (child == null || child.getChildNodes().getLength() == 0)
            return "";

        MathML2TextFormula mml2formula = new MathML2TextFormula();
        String xmlText = element2s(child);
        mml2formula.parse(new StringReader(xmlText));
        String result = mml2formula.getTextFormula();
        return result;
    }

    private String element2s(Element element) throws TransformerException {
            StringWriter sw = new StringWriter();
            TransformerFactory tf = TransformerFactory.newInstance();
            Transformer transformer = tf.newTransformer();
            transformer.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "yes");
            transformer.setOutputProperty(OutputKeys.METHOD, "xml");
            transformer.setOutputProperty(OutputKeys.INDENT, "yes");
            transformer.setOutputProperty(OutputKeys.ENCODING, "UTF-8");
            transformer.transform(new DOMSource(element), new StreamResult(sw));
            return sw.toString();
    }

    /**
     * Inner Class PhspConfiguration
     */
    private class PhspConfiguration implements IPhspConfiguration {

        private final List<Model> mModels;

        public PhspConfiguration (List<Model> models) {
            mModels = models;
        }

        @Override
        public Model[] getModels() {
            int size = mModels.size();
            return mModels.toArray(new Model[size]);
        }

        @Override
        public Model getModel(int index) {
            return mModels.get(index);
        }

        @Override
        public int getModelLength() {
            return mModels.size();
        }
    } 
}
