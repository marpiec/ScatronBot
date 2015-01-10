#!/bin/sh
cp  /home/marpiec/Projects/hackaton/bot/src/main/scala/Bot.scala /home/marpiec/Projects/hackaton/marpiec/Bot.scala
(cd /home/marpiec/Projects/hackaton/marpiec/; git add Bot.scala; git commit -a -m "change"; git push origin master;)