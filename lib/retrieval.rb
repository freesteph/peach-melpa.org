# frozen_string_literal: true

require_relative './logging'
require 'net/http'
require 'rails'

module PeachMelpa
  module Retrieval
    MELPA_HOST = 'melpa.org'
    MELPA_ARCHIVE_NAME = '/archive.json'
    MELPA_STATS_NAME = '/download_counts.json'

    ARCHIVE_PATH = "#{Rails.root}/tmp/archive.json"
    STATS_PATH = "#{Rails.root}/tmp/downloads.json"

    def self.refresh_melpa_archive
      PeachMelpa::Log.info { 'fetching JSON archive from MELPA' }
      archive = Net::HTTP.get(MELPA_HOST, MELPA_ARCHIVE_NAME)
                         .encode('UTF-8', invalid: :replace, undef: :replace, replace: '?')
      File.write(ARCHIVE_PATH, archive)
      PeachMelpa::Log.info { "wrote #{MELPA_ARCHIVE_NAME} to #{ARCHIVE_PATH}" }
      PeachMelpa::Log.info { 'done fetching!' }
    end

    def self.refresh_melpa_stats
      PeachMelpa::Log.info { 'fetching JSON stats from MELPA' }
      archive = Net::HTTP.get(MELPA_HOST, MELPA_STATS_NAME)
                         .encode('UTF-8', invalid: :replace, undef: :replace, replace: '?')
      File.write(STATS_PATH, archive)
      PeachMelpa::Log.info { "wrote #{MELPA_STATS_NAME} to #{STATS_PATH}" }
      PeachMelpa::Log.info { 'done fetching!' }
    end
  end
end
