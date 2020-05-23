# frozen_string_literal: true

require 'json'

require_relative 'retrieval'
require_relative 'errors'

module PeachMelpa
  class DownloadCount
    def initialize
      @downloads = JSON.parse(File.read(PeachMelpa::Retrieval::STATS_PATH))
    end

    def count_for(theme)
      raise('need to pass a theme') if theme.nil?

      @downloads[theme] || raise(PeachMelpa::Errors::NoDownloadCount)
    end
  end
end
