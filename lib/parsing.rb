require 'json'
require_relative './retrieval'
require_relative './logging'

module PeachMelpa
  module Parsing
    SCREENSHOT_FOLDER = "#{::Rails.root}/tmp/screenshots/"

    def self.looks_like_theme? name
      name.end_with? "-theme", "-themes"
    end

    def self.select_themes data
      data.select { |name,val| self.looks_like_theme? name }
    end

    def self.parse_theme name, meta, opts = {}
      PeachMelpa::Log.info(name) { "trying to find theme" }

      version = meta["ver"].join(".")
      description = meta["desc"]
      url = meta["props"]["url"]
      authors = [].concat(meta["props"]["authors"] || []).join(", ")
      kind = meta["type"]

      theme = Theme.find_or_create_by(name: name)

      if theme.older_than? version and not theme.blacklisted? or opts[:force] == true
        PeachMelpa::Log.info(name) { "theme eligible for update..." }
        theme.update_screenshots!(
          version: version,
          description: description,
          url: url,
          authors: authors,
          kind: kind)
      else
        PeachMelpa::Log.info(name) { "skipped because either up-to-date or blacklisted." }
      end
    end

    def self.pick_updated_themes opts = {}
      PeachMelpa::Log::info { "starting to parse themes" }
      data = JSON.parse(IO.read(PeachMelpa::Retrieval::ARCHIVE_PATH))

      # FIXME: harmonise interface so it can be something predicate-based like
      # filters = opts[:only] ? self.find_theme(opts[:only]) : self.looks_like_theme?
      # themes = data.select(filters)
      themes = if opts[:only] then
                 data.select { |entry| entry == opts[:only] }
               else
                 self.select_themes data
               end

      PeachMelpa::Log::info { "captured #{themes.length} themes to update " }

      if not Dir.exists? SCREENSHOT_FOLDER
        Dir.mkdir SCREENSHOT_FOLDER
      end

      args = nil
      args = {force: true } if not opts[:force].nil?

      themes.each do |name, props|p
        self.send :parse_theme, name, props, *[args].reject(&:nil?)
      end
    end
  end
end
