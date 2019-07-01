FROM ruby:2.6.3-alpine

RUN apk add build-base sqlite-dev yarn tzdata

RUN mkdir -p /var/peach/ && mkdir -p /var/peach/gemftw
WORKDIR /var/peach

COPY Gemfile* /var/peach/gemftw/
WORKDIR /var/peach/gemftw
RUN bundle install --without development test

WORKDIR /var/peach
COPY . ./

ENV RAILS_ENV="production"

RUN rails s
