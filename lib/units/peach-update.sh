#!/bin/bash
cd $HOME/peach-melpa
bundle exec rake themes:refresh
bundle exec rake themes:parse
