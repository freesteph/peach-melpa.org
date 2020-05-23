# frozen_string_literal: true

module PeachMelpa
  module Errors
    class EmacsError < StandardError
      def message
        'something bad happened within the Emacs process'
      end
    end

    class NoThemeScreenshotsFolder < StandardError
      def message
        'the screenshots folder (for this specific theme) does not exist'
      end
    end

    class NoDownloadCount < StandardError
      def message
        'no download count found for this specific theme'
      end
    end
  end
end
