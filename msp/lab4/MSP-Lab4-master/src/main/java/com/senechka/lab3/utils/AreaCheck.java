package com.senechka.lab3.utils;

import com.senechka.lab3.beans.Coordinates;

/**
 * The AreaCheck class is responsible for checking if coordinates are within the hit area.
 */
public class AreaCheck {

    /**
     * Checks if the given coordinates are within the hit area.
     * @param coordinates The coordinates to check.
     * @return true if the coordinates are within the hit area, false otherwise.
     */
    public boolean isHit(Coordinates coordinates) {
        return coordinates != null && isHit(coordinates.getX(), coordinates.getY(), coordinates.getR());
    }

    /**
     * Checks if the given coordinates are within the hit area.
     * @param x The x-coordinate.
     * @param y The y-coordinate.
     * @param r The radius of the hit area.
     * @return true if the coordinates are within the hit area, false otherwise.
     */
    public boolean isHit(double x, double y, double r) {
        return isCircleHit(x, y, r) || isRectangleHit(x, y, r) || isTriangleHit(x, y, r);
    }

    private boolean isTriangleHit(double x, double y, double r) {
        return (x <= 0 && y >= 0) && (2 * y - x <= r);
    }

    private boolean isRectangleHit(double x, double y, double r) {
        return (x >= 0 && y >= 0) && (x <= r && y <= r);
    }

    private boolean isCircleHit(double x, double y, double r) {
        return (x >= 0 && y <= 0) && (x*x + y*y <= r*r);
    }
}
