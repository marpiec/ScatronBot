#!/bin/sh
/home/marpiec/Projects/hackaton/bot/package.sh \
&& cp /home/marpiec/Projects/hackaton/bot/target/scala-2.9.3/bot_2.9.3-1.0.jar  ~/Projects/Scalatron/bots/ScalatronBot/ScalatronBot.jar
java -jar ~/Projects/Scalatron/bin/Scalatron.jar -browser no  -x 100 -y 100 
