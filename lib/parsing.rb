require 'json'
require_relative './retrieval'

module PeachMelpa
  module Parsing
    SCREENSHOT_FOLDER = "#{::Rails.root}/tmp/screenshots/"

    def self.looks_like_theme? name
      name.end_with? "-theme"
    end

    def self.select_themes data
      data.select { |name,val| self.looks_like_theme? name }
    end

    def self.parse_theme obj
      name = obj.first.partition("-theme").first

      meta = obj.last
      version = meta["ver"].join(".")
      description = meta["desc"]
      url = meta["props"]["url"] unless meta["props"].nil?

      theme = Theme.find_or_create_by(name: name)

      if theme.older_than? version and not theme.blacklisted?
        theme.update_screenshots!(version: version, description: description, url: url)
      end
    end

    def self.pick_updated_themes
      data = JSON.parse(IO.read(PeachMelpa::Retrieval::ARCHIVE_PATH))
      themes = self.select_themes data

      if not Dir.exists? SCREENSHOT_FOLDER
        Dir.mkdir SCREENSHOT_FOLDER
      end

      themes.each do |theme|
        self.parse_theme(theme)
      end
    end
  end
end
