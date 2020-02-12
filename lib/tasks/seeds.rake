# frozen_string_literal: true

require_relative '../logging'
require 'faker'
require_relative '../../lib/dev_seeds.rb'

namespace :seeds do
  desc 'grabs MELPA archives.json and put it in tmp'
  task :create, [:how_many] => :environment do |_task, args|
    n = args[:how_many].nil? ? 1 : args[:how_many].to_i

    PeachMelpa::Log.info('dev-seeder') { "going to generate #{n} themes" }

    n.times do
      PeachMelpa::Seeds.generate_fake_theme Faker::Internet.slug(glue: '-')
    end
  end
end
