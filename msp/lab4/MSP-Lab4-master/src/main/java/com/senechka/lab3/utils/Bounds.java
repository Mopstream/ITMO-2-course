package com.senechka.lab3.utils;

/**
 * The Bounds enum represents the bounds and inclusivity of different variables.
 */
public enum Bounds {
    X_BOUNDS(-3, 3, true),
    Y_BOUNDS(-3, 5, false),
    R_BOUNDS(1, 3, true);

    private final double left;
    private final double right;
    private final boolean inclusive;

    Bounds(double left, double right, boolean inclusive) {
        this.left = left;
        this.right = right;
        this.inclusive = inclusive;
    }

    /**
     * Returns the left bound of the variable.
     * @return The left bound.
     */
    public double getLeft() {
        return left;
    }

    /**
     * Returns the right bound of the variable.
     * @return The right bound.
     */
    public double getRight() {
        return right;
    }

    /**
     * Checks if the variable is inclusive within the bounds.
     * @return true if the variable is inclusive, false otherwise.
     */
    public boolean isInclusive() {
        return inclusive;
    }
}
