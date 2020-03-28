# frozen_string_literal: true

server 'peach-melpa.org', user: 'peach', roles: %w[app db web], primary: true

set :deploy_to, '/home/peach/deploy/www/'
set :tmp_dir, '/home/peach/deploy/tmp'
