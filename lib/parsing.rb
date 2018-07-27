require 'json'
require_relative './retrieval'
require_relative './logger'

module PeachMelpa
  module Parsing
    SCREENSHOT_FOLDER = "#{::Rails.root}/tmp/screenshots/"

    def self.looks_like_theme? name
      name.end_with? "-theme"
    end

    def self.select_themes data
      data.select { |name,val| self.looks_like_theme? name }
    end

    def self.parse_theme obj, opts = {}
      name = extract_theme_name obj.first
      PeachMelpa::Log.info(name) { "trying to find theme" }

      meta = obj.last
      version = meta["ver"].join(".")
      description = meta["desc"]

      theme = Theme.find_or_create_by(name: name)

      if theme.older_than? version and not theme.blacklisted? or opts[:force] == true
        PeachMelpa::Log.info(name) { "theme eligible for update..." }
        theme.update_screenshots!(version: version, description: description)
      end
    end

    def self.pick_updated_themes opts = {}
      PeachMelpa::Log::info { "starting to parse themes" }
      data = JSON.parse(IO.read(PeachMelpa::Retrieval::ARCHIVE_PATH))

      # FIXME: harmonise interface so it can be something predicate-based like
      # filters = opts[:only] ? self.find_theme(opts[:only]) : self.looks_like_theme?
      # themes = data.select(filters)
      themes = if opts[:only] then
                 data.select { |entry,| self.extract_theme_name(entry) == opts[:only] }
               else
                 self.select_themes data
               end

      PeachMelpa::Log::info { "captured #{themes.length} themes to update " }

      if not Dir.exists? SCREENSHOT_FOLDER
        Dir.mkdir SCREENSHOT_FOLDER
      end

      args = nil
      args = {force: true } if not opts[:force].nil?

      themes.each do |theme|
        self.send :parse_theme, theme, *[args].reject(&:nil?)
      end
    end

    private
    def self.extract_theme_name data
      data.partition("-theme").first
    end
  end
end
