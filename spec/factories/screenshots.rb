# frozen_string_literal: true

FactoryBot.define do
  factory :screenshot do
    mode
    variant

    after(:create) do |s|
      preview = Tempfile.new(['preview', '.png'])

      `convert -size 1600x1200 xc:#{Faker::Color.hex_color} #{preview.path}`

      s.image.attach(io: File.open(preview.path), filename: 'screenshot')
    end
  end
end
