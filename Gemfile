# frozen_string_literal: true

source 'https://rubygems.org'

# Bundle edge Rails instead: gem 'rails', github: 'rails/rails'
gem 'rails'
# Use Puma as the app server
gem 'puma'
# Use SCSS for stylesheets
gem 'sassc-rails'
# Use Uglifier as compressor for JavaScript assets
gem 'uglifier', '>= 1.3.0'

# Use jquery as the JavaScript library
gem 'jquery-rails'
# Turbolinks makes navigating your web application faster. Read more: https://github.com/turbolinks/turbolinks
gem 'turbolinks', '~> 5'
# Build JSON APIs with ease. Read more: https://github.com/rails/jbuilder
gem 'jbuilder', '~> 2.5'
# Use Redis adapter to run Action Cable in production
# gem 'redis', '~> 3.0'
# Use ActiveModel has_secure_password
# gem 'bcrypt', '~> 3.1.7'

# Use Capistrano for deployment
# gem 'capistrano-rails', group: :development

gem 'aws-sdk-s3', require: false
gem 'faker'
gem 'haml-rails'
gem 'image_processing'
gem 'sentry-raven'
gem 'therubyracer'

group :production do
  gem 'bcrypt_pbkdf'
  gem 'ed25519'
  gem 'pg'
  gem 'whenever'
end

group :development, :test do
  gem 'byebug', platform: :mri
  gem 'factory_bot_rails'
  gem 'guard-rspec'
  gem 'guard-rubocop'
  gem 'rspec-rails'
  gem 'rubocop', require: false
  gem 'rubocop-rails', require: false
  gem 'sqlite3'
end

group :development do
  gem 'bootsnap'
  gem 'capistrano', '~> 3.11.2'
  gem 'capistrano-rails'
  gem 'capistrano-rbenv'
  gem 'capistrano-systemd-multiservice'
  gem 'capistrano3-puma'
  gem 'guard-livereload', '~> 2.5', require: false
  gem 'listen', '~> 3.0.5'
  gem 'rack-livereload'
  gem 'spring'
  gem 'spring-watcher-listen', '~> 2.0.0'
  gem 'web-console'
end
