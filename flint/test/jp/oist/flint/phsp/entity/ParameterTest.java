/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.phsp.entity;

import jp.oist.flint.phsp.PhspException;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import static org.junit.Assert.*;

public class ParameterTest {

    public ParameterTest() {
    }

    @BeforeClass
    public static void setUpClass() { }

    @AfterClass
    public static void tearDownClass() {
    }

    @Before
    public void setUp() {
    }

    @After
    public void tearDown() {
    }

    @Test
    public void testValidateName() {
        String name = "value";
        Parameter p = new Parameter();

        boolean result = true;
        try {
            p.setName(name);
            p.validateName();
        } catch (PhspException ex) {
            result = false;
        }
        assertEquals(true, result);

        name = "val1";
        result = true;
        try {
            p.setName(name);
            p.validateName();
        } catch (PhspException ex) {
            result = false;
        }
        assertEquals(true, result);

        name = "_val1";
        result = true;
        try {
            p.setName(name);
            p.validateName();
        } catch (PhspException ex) {
            result = false;
        }
        assertEquals(true, result);

        name = "1";
        result = true;
        try {
            p.setName(name);
            p.validateName();
        } catch (PhspException ex) {
            result = false;
        }
        assertEquals(false, result);

        name = "1_value";
        result = true;
        try {
            p.setName(name);
            p.validateName();
        } catch (PhspException ex) {
            result = false;
        }
        assertEquals(false, result);
    }

    @Test
    public void testValidateValue() {
        Parameter p = new Parameter();


        String enumValue;
        boolean result;


        /* Test for Enum value */
        p.setType(Parameter.Type.ENUM);
        enumValue = "1";
        result = true;
        try {
            p.setEnumValue(enumValue);
            p.validateValue();
        } catch (PhspException ex) {
            result = false;
        }
        assertEquals(true, result);

        enumValue = "-1";
        result = true;
        try {
            p.setEnumValue(enumValue);
            p.validateValue();
        } catch (PhspException ex) {
            result = false;
        }
        assertEquals(true, result);

        enumValue = "1,-1,0";
        result = true;
        try {
            p.setEnumValue(enumValue);
            p.validateValue();
        } catch (PhspException ex) {
            result = false;
        }
        assertEquals(true, result);

        enumValue = "1,-1,1.0,-1.0,0.1,-0.1,12.34,-12.34";
        result = true;
        try {
            p.setEnumValue(enumValue);
            p.validateValue();
        } catch (PhspException ex) {
            result = false;
        }
        assertEquals(true, result);

        enumValue = "value1";
        result = true;
        try {
            p.setEnumValue(enumValue);
            p.validateValue();
        } catch (PhspException ex) {
            result = false;
        }
        assertEquals(false, result);

        enumValue = "1,2,3,value1";
        result = true;
        try {
            p.setEnumValue(enumValue);
            p.validateValue();
        } catch (PhspException ex) {
            result = false;
        }
        assertEquals(false, result);
    }
}
