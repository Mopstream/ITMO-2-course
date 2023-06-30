package com.senechka.lab3.db;

import org.hibernate.Session;
import org.hibernate.SessionFactory;
import org.hibernate.cfg.Configuration;

import javax.enterprise.context.ApplicationScoped;
import javax.inject.Named;
import java.util.List;

/**
 * The {@code DataBaseController} class is responsible for handling database operations.
 * It provides methods to retrieve user hits, add hit results, and mark user hits as removed.
 */
@Named
@ApplicationScoped
public class DataBaseController {
    private SessionFactory factory;
    private Session session;

    /**
     * Constructs a new instance of {@code DataBaseController}.
     * Initializes the session factory and creates a session.
     */
    public DataBaseController() {
        try {
            this.factory = new Configuration()
                    .configure("hibernate.cfg.xml")
                    .addAnnotatedClass(HitResult.class)
                    .buildSessionFactory();

            this.createSession();
        } catch (Exception e) {
            System.out.println("Exception during session factory init: " + e.getMessage());
        }
    }

    private void createSession() {
        this.session = factory.getCurrentSession();
    }

    /**
     * Retrieves a list of hit results for a given user session ID.
     *
     * @param sessionId the session ID of the user
     * @return a list of hit results for the user, or {@code null} if the session ID is null
     */
    public List<HitResult> getUserHits(String sessionId) {
        if (sessionId == null) return null;
        createSession();

        this.session.beginTransaction();

        String sqlRequest = "SELECT hit FROM HitResult hit WHERE hit.sessionId= :sessionId AND hit.removed=false";

        List<HitResult> results = this.session.createQuery(sqlRequest, HitResult.class)
                .setParameter("sessionId", sessionId)
                .getResultList();

        this.session.getTransaction().commit();

        System.out.println("Get hits from db: " + results.size());

        return results;
    }

    /**
     * Adds a hit result to the database.
     *
     * @param hitResult the hit result to be added
     */
    public void addHitResult(HitResult hitResult) {
        System.out.println("check");
        if (hitResult == null) return;
        createSession();

        System.out.println("Start saving hit");

        this.session.beginTransaction();
        this.session.save(hitResult);
        this.session.getTransaction().commit();

        System.out.println("Save hit to db");
    }

    /**
     * Marks the user hits as removed for a given session ID.
     *
     * @param sessionId the session ID of the user
     */
    public void markUserHitsRemoved(String sessionId) {
        if (sessionId == null) return;
        createSession();

        this.session.beginTransaction();

        this.session.createQuery("UPDATE HitResult SET removed=true WHERE sessionId= :sessionId")
                .setParameter("sessionId", sessionId)
                .executeUpdate();

        this.session.getTransaction().commit();
    }
}
