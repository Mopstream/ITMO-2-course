package com.senechka.lab3.utils;

import com.senechka.lab3.beans.Coordinates;

/**
 * The CoordinatesValidation class provides methods to validate coordinates based on predefined bounds.
 */
public class CoordinatesValidation {
    /**
     * Validates the given coordinates based on predefined bounds.
     * @param coordinates The coordinates to validate.
     * @return true if the coordinates are valid, false otherwise.
     */
    public static boolean validate(Coordinates coordinates) {
        return validateVariable(coordinates.getX(), Bounds.X_BOUNDS) &&
                validateVariable(coordinates.getY(), Bounds.Y_BOUNDS) &&
                validateVariable(coordinates.getR(), Bounds.R_BOUNDS);
    }

    /**
     * Validates a single variable based on its bounds.
     * @param var The variable to validate.
     * @param bounds The bounds for the variable.
     * @return true if the variable is valid, false otherwise.
     */
    public static boolean validateVariable(double var, Bounds bounds) {
        if (var > bounds.getLeft() && var < bounds.getRight()) return true;
        return bounds.isInclusive() && (var == bounds.getLeft() || var == bounds.getRight());
    }
}
