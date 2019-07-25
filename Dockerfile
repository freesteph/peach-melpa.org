FROM ruby:2.6.3-alpine

ENV RAILS_ENV="production"
ENV RAILS_SERVE_STATIC_FILES="true"

RUN apk add build-base yarn tzdata postgresql-dev
RUN mkdir -p /var/peach/gemftw

COPY Gemfile* /var/peach/gemftw/
WORKDIR /var/peach/gemftw
RUN bundle install --without development test

WORKDIR /var/peach
COPY . ./
RUN rake assets:precompile

/CMD sh -c
