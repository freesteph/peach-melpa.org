FROM ruby:2.6.3-alpine

ENV RAILS_ENV="production"
ENV RAILS_SERVE_STATIC_FILES="true"

RUN apk add build-base sqlite-dev yarn tzdata
RUN mkdir -p /var/peach/gemftw

COPY Gemfile* /var/peach/gemftw/
WORKDIR /var/peach/gemftw
RUN bundle install --without development test

WORKDIR /var/peach
COPY . ./
RUN rake db:migrate && rake assets:precompile

EXPOSE 80 443 3000

CMD rails s
