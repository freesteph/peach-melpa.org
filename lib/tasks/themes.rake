# frozen_string_literal: true

require 'timeout'
require 'open-uri'
require 'json'
require "#{Rails.root}/lib/critic"
require "#{Rails.root}/lib/retrieval"
require "#{Rails.root}/lib/parsing"

SCREENSHOT_FOLDER = "#{Rails.root}/tmp/screenshots/"

namespace :themes do
  desc 'grabs MELPA archives.json and put it in tmp'
  task refresh: :environment do
    PeachMelpa::Retrieval.refresh_melpa_archive
  end

  desc 'grabs tmp JSON file and store themes'
  task :parse, %i[theme_name force] => :environment do |_task, args|
    PeachMelpa::Parsing.pick_updated_themes only: args[:theme_name], force: args[:force]
  end

  desc 'refresh & update'
  task update: :environment do
    Rake::Task['themes:refresh'].invoke
    Rake::Task['themes:parse'].invoke
  end
end
