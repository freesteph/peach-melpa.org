# frozen_string_literal: true

require_relative './logging'
require 'tempfile'
require 'faker'

module PeachMelpa
  module Seeds
    def self.generate_fake_theme(name, variants_length = 1)
      PeachMelpa::Log.info('dev-seeder') { "generating theme '#{name}' with #{variants_length} variant(s)" }

      preview = Tempfile.new(['preview', '.png'])

      `convert -size 1600x1200 xc:#{Faker::Color.hex_color} #{preview.path}`

      Array.new(1).each_with_index do |_e, i|
        t = Theme.create!(name: name, version: Time.zone.now.to_s)
        v = Variant.create!(theme: t, name: "#{name}-variant-#{i + 1}")

        Mode.all.each do |mode|
          s = Screenshot.create!(variant: v, mode: mode)
          s.image.attach(
            io: File.open(preview.path),
            filename: 'screenshot'
          )
          s.save!
        end
      end
    end
  end
end
