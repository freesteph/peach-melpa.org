require 'json'
require './lib/retrieval'

module PeachMelpa
  module Parsing
    def self.looks_like_theme? name
      name.end_with? "-theme"
    end

    def self.select_themes data
      data.select { |name,val| self.looks_like_theme? name }
    end

    def self.parse_theme obj
      name = obj.first.split("-").first
      version = obj.last['ver'].join(".")

      theme = Theme.find_or_create_by(name: name)

      if theme.older_than? version
        theme.update_screenshots! version
      end
    end

    def self.pick_updated_themes
      data = JSON.parse(IO.read(PeachMelpa::Retrieval::ARCHIVE_PATH))

      themes = self.select_themes data

      themes.each do |theme|
        self.parse_theme(theme)
      end
    end
  end
end
