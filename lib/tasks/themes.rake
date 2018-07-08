require 'timeout'
require 'open-uri'
require 'json'
require "#{Rails.root}/lib/critic"
require "#{Rails.root}/lib/retrieval"
require "#{Rails.root}/lib/parsing"

namespace :themes do
  SCREENSHOT_FOLDER = "#{Rails.root}/tmp/screenshots/"

  desc "grabs MELPA archives.json and put it in tmp"
  task refresh: :environment do
    PeachMelpa::Retrieval.refresh_melpa_archive
  end

  desc "grabs tmp JSON file and store themes"
  task parse: :environment do
    PeachMelpa::Parsing.pick_updated_themes
  end
end
