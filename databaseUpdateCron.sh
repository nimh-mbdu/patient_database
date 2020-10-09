#!/bin/bash

docker run --rm --user $(id -u sadeghin):$(id -g sadeghin) -v /etc/passwd:/etc/passwd -v /etc/group:/etc/group -v /home/sadeghin/cifs/jsbach/string-mbd:/string-mbd -v /home/sadeghin/cifs/jsbach/sdan1:/sdan1 database-scripts /string-mbd/Database/Database_Scripts_Github/runDatabaseRscript.sh > /home/sadeghin/databaseUpdateCron.out 2>&1
