require 'net/http'
require 'rails'

module PeachMelpa
  module Retrieval
    MELPA_HOST = "melpa.org"
    MELPA_ARCHIVE_NAME = "/archive.json"
    ARCHIVE_PATH = "#{Rails.root}/tmp/archive.json"

    def self.refresh_melpa_archive
      archive = Net::HTTP.get(MELPA_HOST, MELPA_ARCHIVE_NAME)
                  .encode('UTF-8', invalid: :replace, undef: :replace, replace: '?')
      File.write(ARCHIVE_PATH, archive)
    end
  end
end
