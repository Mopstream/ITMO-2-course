package com.senechka.lab3.mbeans;

import javax.management.Notification;

public class percentCountMBean {
    double sum = 0;
    double nepopal = 0;


    public void shot (boolean hit){
        sum++;

        if (!hit){
            nepopal++;

        }

    }
    public double percent(){
        if (sum == 0) {
            return 0;
        }
        return ( (nepopal/sum) * 100);
    }
    public double getResult() {
        return sum;
    }

    public double getNepopal() {
        return nepopal;
    }
}
