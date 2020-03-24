# frozen_string_literal: true

task :seed do # rubocop:disable Rails/RakeEnvironment
  on primary :db do
    within current_path do
      with rails_env: fetch(:stage) do
        execute :rake, 'db:seed'
      end
    end
  end
end
