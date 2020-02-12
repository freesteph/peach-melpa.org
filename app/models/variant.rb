# frozen_string_literal: true

require_relative '../../lib/parsing'

class Variant < ApplicationRecord
  belongs_to :theme
  has_many :screenshots, dependent: :destroy
  validates :name, presence: true, uniqueness: { scope: :theme }

  def to_param
    name
  end

  def parse!
    assets_path = File.join PeachMelpa::Parsing::SCREENSHOT_FOLDER, theme.name

    Dir.chdir(assets_path) do
      Dir.glob("#{name}_*").each do |name|
        mode = extract_mode name

        s = screenshots.create!(
          mode: mode
        )

        s.image.attach(
          io: File.open(File.join(assets_path, name)),
          filename: name
        )
      end
    end
  end

  def to_s
    name
  end

  def extract_mode(filename)
    ext = File.basename(filename, '.*').split('_').last

    Mode.find_by(extension: ext)
  end
end
