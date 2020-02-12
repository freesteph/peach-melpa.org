# frozen_string_literal: true

require 'image_processing'

module PeachHelpers
  # initially called the submodule ImageProcessing but had to avoid
  # the clash and couldn't come up with anything better
  module Critic
    def self.get_brightness_index(path)
      magick_string = ImageProcessing::MiniMagick
                      .source(path)
                      .colorspace('gray')
                      .resize('1x1')
                      .convert('txt')
                      .call
                      .read

      hex_code = /#[\dA-Z]{6}/.match(magick_string)

      if hex_code.nil?
        raise <<~MSG
          something went wrong and the brightness could not be parsed: tried to
          read image file at #{path} and resulting MiniMagick string
          was #{magick_string}.
        MSG
      end

      grey = hex_code[0].last(2).hex

      1 - grey / 255.0
    end
  end
end
