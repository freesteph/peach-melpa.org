#!/bin/sh

bundle exec rails db:migrate

bundle exec rails s
exec "$@"
