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
      name = extract_theme_name obj.first

      meta = obj.last
      version = meta["ver"].join(".")
      description = meta["desc"]

      theme = Theme.find_or_create_by(name: name)

      if theme.older_than? version and not theme.blacklisted?
        theme.update_screenshots!(version: version, description: description)
      end
    end

    def self.pick_updated_themes opts = {}
      data = JSON.parse(IO.read(PeachMelpa::Retrieval::ARCHIVE_PATH))

      # FIXME: harmonise interface so it can be something predicate-based like
      # filters = opts[:only] ? self.find_theme(opts[:only]) : self.looks_like_theme?
      # themes = data.select(filters)
      themes = if opts[:only] then
                 data.select { |entry,| self.extract_theme_name(entry) == opts[:only] }
               else
                 self.select_themes data
               end

      if not Dir.exists? SCREENSHOT_FOLDER
        Dir.mkdir SCREENSHOT_FOLDER
      end

      themes.each do |theme|
        self.parse_theme(theme)
      end
    end

    private
    def self.extract_theme_name data
      data.partition("-theme").first
    end
  end
end
