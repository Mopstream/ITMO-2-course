package com.senechka.lab3.beans;

import com.senechka.lab3.db.HitResult;
import com.senechka.lab3.mbeans.SummaryHitsMbean;
import com.senechka.lab3.mbeans.percentCountMBean;
import com.senechka.lab3.utils.AreaCheck;
import com.senechka.lab3.db.DataBaseController;
import com.senechka.lab3.utils.CoordinatesValidation;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

import javax.enterprise.context.SessionScoped;
import javax.inject.Named;
import javax.management.*;
import javax.persistence.EntityManagerFactory;
import java.io.Serializable;
import java.lang.management.ManagementFactory;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.LinkedList;
import java.util.List;

@Getter
@Setter
@ToString
@SessionScoped
@Named("service")
public class Service implements Serializable {
    private final AreaCheck areaCheck;
    private final DataBaseController dbController;
    private final SummaryHitsMbean summaryHitsMbean;
    private final percentCountMBean percentCountMBean;
    private final MBeanServer mBeanServer;

    public Service() {
        areaCheck = new AreaCheck();
        dbController = new DataBaseController();
        summaryHitsMbean = new SummaryHitsMbean();
        percentCountMBean = new percentCountMBean();
        mBeanServer = ManagementFactory.getPlatformMBeanServer();
        try{
        mBeanServer.registerMBean(summaryHitsMbean, new ObjectName("Zalupa:name=summary"));
        mBeanServer.registerMBean(percentCountMBean, new ObjectName("Zalupa:name=percent"));} catch (Exception e){
            System.out.println(e);
        }
    }

    public LinkedList<HitResult> getUserHits(String sessionId) {
        if (sessionId == null) return new LinkedList<>();

        List<HitResult> hits = dbController.getUserHits(sessionId);

        System.out.println("Return user hits: " + hits);

        return hits != null ? new LinkedList<>(hits) : new LinkedList<>();
    }

    public HitResult processRequest(String sessionId, Coordinates coordinates) {
        if (!CoordinatesValidation.validate(coordinates)) {
            System.out.println("Coordinates not valid");
            return null;
        }

        boolean isHit = areaCheck.isHit(coordinates);
        HitResult hitResult = new HitResult(sessionId, coordinates, getCurrentDate(), isHit);

        System.out.println("Get result:" + hitResult);

        dbController.addHitResult(hitResult);

        System.out.println("WTFFF");


        summaryHitsMbean.shot(isHit);
        percentCountMBean.shot(isHit);
        System.out.println(summaryHitsMbean.getResult() + " GOVNO "+ summaryHitsMbean.getNepopal());
        System.out.println(percentCountMBean.percent());

        return hitResult;
    }

    public void clearUserHits(String sessionId) {
        dbController.markUserHitsRemoved(sessionId);
    }

    private String getCurrentDate() {
        return LocalDateTime.now().format(DateTimeFormatter.ofPattern("dd/MM/yyyy HH:mm:ss"));
    }
}
