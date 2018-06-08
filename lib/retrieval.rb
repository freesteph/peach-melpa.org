require 'net/http'

module PeachMelpa
  module Retrieval
    MELPA_HOST = 'https://melpa.org/'
    MELPA_ARCHIVE_NAME = 'foo'
    ARCHIVE_PATH = "~/tmp/archive.json"

    def self.refresh_melpa_archive
      File.write("w+", ARCHIVE_PATH) do |tmp|
        tmp.write(Net::HTTP.get(MELPA_HOST, MELPA_ARCHIVE_NAME))
      end
    end
  end
end
