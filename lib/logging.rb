# frozen_string_literal: true

require 'logger'

module PeachMelpa
  module Log
    @logger = ::Logger.new($stdout)

    def self.info(name = '', &block)
      return if ENV['RAILS_ENV'] == 'test'

      raise 'cannot call logger without block' unless block_given?

      @logger.info(name, &block)
    end
  end
end
