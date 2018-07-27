require 'logger'

module PeachMelpa
  module Log
    @logger = Logger.new(STDOUT)

    def self.info name = ""
      raise "cannot call logger without block" if not block_given?

      @logger.info(name) do
        yield
      end
    end
  end
end
