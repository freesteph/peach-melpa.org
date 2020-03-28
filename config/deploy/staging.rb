# frozen_string_literal: true

server 'peach-melpa.org', user: 'peach', roles: %w[app web]

set :branch, 'staging'
set :deploy_to, '/home/peach/staging/www'
set :tmp_dir, '/home/peach/staging/tmp'
set :keep_releases, 1
