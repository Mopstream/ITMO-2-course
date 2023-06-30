package com.senechka.lab3.beans;

import lombok.*;

import javax.faces.bean.ManagedBean;
import javax.inject.Named;
import java.io.Serializable;
import javax.enterprise.context.RequestScoped;

/**
 * The Coordinates class represents the coordinates used in the application.
 * It holds the x, y, and r values and provides methods for setting the r value to specific predefined values.
 */
@Getter
@Setter
@ToString
@NoArgsConstructor
@Named("coordinates")
@RequestScoped
public class Coordinates implements Serializable {
    private double x;
    private double y;
    private double r;
    @ToString.Exclude
    private final double[] rValues = {1, 1.5, 2, 2.5, 3};

    /**
     * Constructs a new Coordinates instance with the specified x and y values.
     * @param x The x value of the coordinates.
     * @param y The y value of the coordinates.
     */
    public Coordinates(double x, double y) {
        this.x = x;
        this.y = y;
    }

    /**
     * Constructs a new Coordinates instance with the specified x, y, and r values.
     * @param x The x value of the coordinates.
     * @param y The y value of the coordinates.
     * @param r The r value of the coordinates.
     */
    public Coordinates(double x, double y, double r) {
        this.x = x;
        this.y = y;
        this.r = r;
    }

    /**
     * Sets the r value to the first predefined value.
     */
    public void setFirstRValue() {
        System.out.println("Set first r");
        this.r = rValues[0];
    }

    /**
     * Sets the r value to the second predefined value.
     */
    public void setSecondRValue() {
        this.r = rValues[1];
    }

    /**
     * Sets the r value to the third predefined value.
     */
    public void setThirdRValue() {
        this.r = rValues[2];
    }

    /**
     * Sets the r value to the fourth predefined value.
     */
    public void setFourthRValue() {
        this.r = rValues[3];
    }

    /**
     * Sets the r value to the fifth predefined value.
     */
    public void setFifthRValue() {
        this.r = rValues[4];
    }
}
