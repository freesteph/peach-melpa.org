#!/bin/bash

code="puts \"blacklisting $1...\";"
code+="t = Theme.find_by(name: \"$1\");"
code+="if t; t.update_attributes!(blacklisted: true); puts 'success!'; "
code+="else; puts 'could not find such theme...';end"

bin/rails runner "${code}"
