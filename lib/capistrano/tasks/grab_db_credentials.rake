require 'json'

RETRIEVE_CMD = "secretsmanager get-secret-value --secret-id='prod-db' | jq '.SecretString | fromjson'"

namespace :aws do
  desc 'Grab the credentials for the DB'
  task :grab_db_credentials do
    on roles(:app) do
      execute :aws, RETRIEVE_CMD, verbosity: :DEBUG do |data|
        credentials = JSON.parse data

        %w[username password host port].each do |key|
          ENV["PEACH_DB_#{key.upcase}"] = credentials.fetch(key).to_s
        end
      end
    end
  end
end
