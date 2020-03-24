# frozen_string_literal: true

# config valid for current version and patch releases of Capistrano
lock '~> 3.11.2'

set :application, 'peach-melpa'
set :repo_url, 'https://github.com/freesteph/peach-melpa.org.git'

# Default branch is :master
# ask :branch, `git rev-parse --abbrev-ref HEAD`.chomp
# set :branch, 'feature/capistrano'

# Default deploy_to directory is /var/www/my_app_name
set :deploy_to, '/home/peach/deploy/www/'

# Default value for :format is :airbrussh.
# set :format, :nairbrussh

# You can configure the Airbrussh format using :format_options.
# These are the defaults.
# set :format_options, command_output: true, log_file: "log/capistrano.log", color: :auto, truncate: :auto

# Default value for :pty is false
# set :pty, true

# Default value for :linked_files is []
# append :linked_files, "config/database.yml"

# Default value for linked_dirs is []
# append :linked_dirs, "log", "tmp/pids", "tmp/cache", "tmp/sockets", "public/system"

set :tmp_dir, '/home/peach/deploy/tmp'
# Default value for default_env is {}
set :default_env,
    'DISPLAY': ':13'

# Default value for local_user is ENV['USER']
# set :local_user, -> { `git config user.name`.chomp }

# Default value for keep_releases is 5
# set :keep_releases, 5

# Uncomment the following to require manually verifying the host key before first deploy.
# set :ssh_options, verify_host_key: :secure

set :bundle_jobs, 1

before 'deploy:migrate', 'aws:grab_db_credentials'
after 'deploy:migrate', 'db:seed'
