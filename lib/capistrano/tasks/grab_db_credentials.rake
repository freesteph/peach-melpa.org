require 'json'

namespace :aws do
  desc 'Grab the credentials for the DB'
  task :grab_db_credentials do
    data = `aws secretsmanager get-secret-value --secret-id="prod-db" | jq '.SecretString | fromjson'`
    credentials = JSON.parse data

    %w[username password host port].each do |key|
      ENV["PEACH_DB_#{key.upcase}"] = credentials.fetch(key).to_s
    end
  end
end
