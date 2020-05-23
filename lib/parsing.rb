# frozen_string_literal: true

require 'json'
require_relative './retrieval'
require_relative './logging'
require_relative './download_count'

module PeachMelpa
  module Parsing
    SCREENSHOT_FOLDER = "#{::Rails.root}/tmp/screenshots/"

    AKIN = %w[color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized].freeze

    REJECTS = %w[svg-mode-line-themes select-themes rand-theme per-buffer-theme
                 display-theme cycle-themes color-theme-theme helm-themes
                 remember-last-theme color-theme-modern].freeze

    def self.looks_like_theme?(name)
      return false if REJECTS.include?(name)

      name.end_with?('-theme', '-themes') || AKIN.include?(name)
    end

    def self.select_themes(data)
      data.select { |name, _val| looks_like_theme? name }
    end

    def self.parse_theme(name, meta, download_count, opts = {})
      PeachMelpa::Log.info(name) { 'trying to find theme' }

      version = meta['ver'].join('.')
      description = meta['desc']
      url = meta['props']['url']
      authors = [].concat(meta['props']['authors'] || []).join(', ')
      kind = meta['type']

      theme = Theme.find_or_create_by(name: name)

      if theme.older_than?(version) || (opts[:force] == true)
        PeachMelpa::Log.info(name) { 'theme eligible for update...' }
        theme.update_screenshots!(
          version: version,
          description: description,
          download_count: download_count,
          url: url,
          authors: authors,
          kind: kind
        )
      else
        PeachMelpa::Log.info(name) { 'skipped because up to date.' }
      end
    end

    def self.start_daemon
      PeachMelpa::Log.info { 'start Emacs daemon...' }
      `killall -q -9 emacs`
      `killall -q -9 emacsclient`
      `emacs --daemon=peach -Q -l lib/take-screenshot.el`
    end

    def self.pick_updated_themes(opts = {})
      start_daemon

      PeachMelpa::Log.info { 'starting to parse themes' }
      data = JSON.parse(IO.read(PeachMelpa::Retrieval::ARCHIVE_PATH))

      # FIXME: harmonise interface so it can be something predicate-based like
      # filters = opts[:only] ? self.find_theme(opts[:only]) : self.looks_like_theme?
      # themes = data.select(filters)
      themes = if opts[:only]
                 data.select { |entry| entry == opts[:only] }
               else
                 select_themes data
               end

      PeachMelpa::Log.info { "captured #{themes.length} themes to update " }

      Dir.mkdir SCREENSHOT_FOLDER unless Dir.exist? SCREENSHOT_FOLDER

      args = nil
      args = { force: true } unless opts[:force].nil?

      download_count = PeachMelpa::DownloadCount.new

      themes.each do |name, props|
        parse_theme(name, props, download_count.count_for(name), *[args].reject(&:nil?))
      end
    end
  end
end
