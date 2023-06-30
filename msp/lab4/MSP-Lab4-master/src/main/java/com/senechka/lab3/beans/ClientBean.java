package com.senechka.lab3.beans;

import com.senechka.lab3.db.HitResult;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

import javax.faces.annotation.ManagedProperty;
import javax.enterprise.context.SessionScoped;
import javax.faces.context.FacesContext;
import javax.inject.Named;
import javax.management.InstanceAlreadyExistsException;
import javax.management.MBeanRegistrationException;
import javax.management.MalformedObjectNameException;
import javax.management.NotCompliantMBeanException;
import java.io.Serializable;
import java.util.LinkedList;
import java.util.function.Function;

/**
 * The ClientBean class represents a client in the application.
 * It handles user requests and interacts with the Service class.
 */
@Getter
@Setter
@ToString
@Named("client")
@SessionScoped
public class ClientBean implements Serializable {
    private final String sessionId;
    private final LinkedList<HitResult> currentHits;

    @ManagedProperty(value = "#{coordinates}")
    private Coordinates coordinates = new Coordinates();
    @ManagedProperty(value = "#{service}")
    private Service service = new Service();

    /**
     * Constructs a new ClientBean instance.
     * Initializes the sessionId and retrieves the current hits for the user.
     */
    public ClientBean() throws MalformedObjectNameException, NotCompliantMBeanException, InstanceAlreadyExistsException, MBeanRegistrationException {
        this.sessionId = FacesContext.getCurrentInstance().getExternalContext().getSessionId(true);
        this.currentHits = service.getUserHits(sessionId);
    }

    /**
     * Makes a user request using the provided coordinates.
     * Calls the makeRequest method with the coordinates and adds the resulting HitResult to the current hits list.
     */
    public void makeUserRequest() {
        makeRequest(this.coordinates);
    }

    /**
     * Makes a remote request using the coordinates obtained from the request parameters.
     * Retrieves the coordinates from the request parameters, creates a Coordinates instance, and calls the makeRequest method.
     */
    public void makeRemoteRequest() {
        Function<String, Double> getParam = (name) -> {
            return Double.parseDouble(FacesContext.getCurrentInstance().getExternalContext().getRequestParameterMap().get(name));
        };

        try {
            Coordinates coordinates = new Coordinates(getParam.apply("x"), getParam.apply("y"), getParam.apply("r"));
            makeRequest(coordinates);
        } catch (NullPointerException | NumberFormatException exception) {
            System.out.println("Can't parse values from request params");
        }
    }

    /**
     * Makes a request to the service using the provided coordinates.
     * Calls the service's processRequest method and adds the resulting HitResult to the current hits list if it is not null.
     * @param coordinates The coordinates for the request.
     */
    public void makeRequest(Coordinates coordinates) {
        System.out.println("Make request: " + coordinates.toString());
        HitResult result = service.processRequest(this.sessionId, coordinates);

        if (result != null)
            this.currentHits.addFirst(result);
    }

    /**
     * Clears the current hits list and marks the user's hits as removed in the service.
     */
    public void clearHits() {
        currentHits.clear();
        service.clearUserHits(this.sessionId);

        System.out.println("Current hits: " + currentHits);
    }
}
