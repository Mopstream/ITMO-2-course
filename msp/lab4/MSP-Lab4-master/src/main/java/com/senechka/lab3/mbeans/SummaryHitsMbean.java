package com.senechka.lab3.mbeans;
import javax.management.Notification;
import javax.management.NotificationBroadcasterSupport;

public class SummaryHitsMbean extends NotificationBroadcasterSupport{
    int sum = 0;
    int nepopal = 0;

    int notificaionCounter = 0;
    public void shot (boolean hit){
        sum++;
        if (sum %15 ==0) {sendNotification(new Notification("Summary is divides by 15", this, notificaionCounter++));}
        if (!hit){
            nepopal++;

        }

    }

    public int getResult() {
        return sum;
    }

    public int getNepopal() {
        return nepopal;
    }
}
