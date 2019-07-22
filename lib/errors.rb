module PeachMelpa
  module Errors
    class EmacsError < StandardError
      def message
        "something bad happened within the Emacs process"
      end
    end

    class NoScreenshotsFolder < StandardError
      def message
        "the folder used to store the generated screenshots does not exist"
      end
    end
  end
end
