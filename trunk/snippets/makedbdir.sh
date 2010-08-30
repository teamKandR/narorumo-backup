#!/bin/bash

mkdir -p db
./db.py
sudo chgrp -R www-data db
sudo chmod -R ug+rwx db
