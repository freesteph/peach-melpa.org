# frozen_string_literal: true

require_relative '../logging'
require 'faker'
require 'factory_bot_rails'

namespace :seeds do
  desc 'creates fake themes'
  task :create, [:how_many] => :environment do |_task, args|
    include FactoryBot::Syntax::Methods

    n = args[:how_many].nil? ? 1 : args[:how_many].to_i

    PeachMelpa::Log.info('dev-seeder') { "going to generate #{n} themes" }

    create_list(:theme, n)
  end
end
