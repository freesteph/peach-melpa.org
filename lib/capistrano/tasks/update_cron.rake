# frozen_string_literal: true

namespace :deploy do
  desc 'sets the update process to run hourly with whenever'
  task :update_cron do # rubocop:disable Rails/RakeEnvironment
    on roles(:app) do
      within current_path do
        execute :bundle, :exec, "whenever --update-crontab #{fetch(:application)}"
      end
    end
  end
end
