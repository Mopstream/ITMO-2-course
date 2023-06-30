package com.senechka.lab3.tests;

import com.senechka.lab3.beans.Coordinates;
import com.senechka.lab3.utils.Bounds;
import com.senechka.lab3.utils.CoordinatesValidation;
import org.testng.annotations.Test;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class CoordinatesValidationTest {

    @Test
    public void testValidateTrue() {
        Coordinates coordinates = new Coordinates(0, 2, 2);
        assertTrue(CoordinatesValidation.validate(coordinates));
    }

    @Test
    public void testValidateFalse() {
        Coordinates coordinates = new Coordinates(4, 2, 2);
        assertFalse(CoordinatesValidation.validate(coordinates));
    }

    @Test
    public void testValidateVariableTrue() {
        assertTrue(CoordinatesValidation.validateVariable(0, Bounds.X_BOUNDS));
    }

    @Test
    public void testValidateVariableFalse() {
        assertFalse(CoordinatesValidation.validateVariable(4, Bounds.X_BOUNDS));
    }
}