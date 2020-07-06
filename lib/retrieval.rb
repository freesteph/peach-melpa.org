# frozen_string_literal: true

require_relative './logging'
require 'net/http'
require 'rails'

module PeachMelpa
  module Retrieval
    MELPA_HOST = 'https://melpa.org'
    MELPA_ARCHIVE_NAME = '/archive.json'
    ARCHIVE_PATH = "#{Rails.root}/tmp/archive.json"

    def self.refresh_melpa_archive
      PeachMelpa::Log.info { 'fetching JSON archive from MELPA' }
      archive = Net::HTTP.get(archive_url)
                         .encode('UTF-8', invalid: :replace, undef: :replace, replace: '?')

      File.write(ARCHIVE_PATH, archive)
      PeachMelpa::Log.info { "wrote #{MELPA_ARCHIVE_NAME} to #{ARCHIVE_PATH}" }
      PeachMelpa::Log.info { 'done fetching!' }
    end

    def self.archive_url
      URI.join(MELPA_HOST, MELPA_ARCHIVE_NAME)
    end
  end
end
