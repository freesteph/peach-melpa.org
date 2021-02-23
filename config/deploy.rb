# frozen_string_literal: true

# config valid for current version and patch releases of Capistrano
lock '~> 3.11.2'

set :application, 'peach-melpa'
set :repo_url, 'https://github.com/freesteph/peach-melpa.org.git'

set :ssh_options,
    keys: [
      '~/.ssh/peach-deploy-key',
      '~/.ssh/peach-melpa-dev.pem'
    ],
    forward_agent: true,
    auth_methods: %w[publickey]

# Default value for default_env is {}
set :default_env,
    DISPLAY: ':13',
    NODE_ENV: 'production'


# Default value for local_user is ENV['USER']
# set :local_user, -> { `git config user.name`.chomp }

# Default value for keep_releases is 5
# set :keep_releases, 5

# Uncomment the following to require manually verifying the host key before first deploy.
# set :ssh_options, verify_host_key: :secure

set :bundle_jobs, 1

set :keep_releases, 2

before 'deploy:assets:precompile', 'deploy:yarn_install'

after 'deploy:migrate', 'seed'
