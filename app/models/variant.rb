require_relative '../../lib/parsing'

class Variant < ApplicationRecord
  belongs_to :theme
  has_many :screenshots, dependent: :destroy
  validates :name, presence: true, uniqueness: { scope: :theme }

  def to_param
    name
  end

  def parse!
    assets_path = File.join PeachMelpa::Parsing::SCREENSHOT_FOLDER, self.theme.name

    Dir.chdir(assets_path) do
      Dir.glob("#{self.name}_*").each do |name|
        mode = self.extract_mode name

        s = self.screenshots.create!(
          mode: mode,
        )

        s.image.attach(
          io: File.open(File.join(assets_path, name)),
          filename: name)
      end
    end
  end

  def mode_for filename, modes
    ext = File.basename(filename, ".*").split("_").last

    modes.find { |name, e| ext == e }
  end

  def screenshot_for lang, modes
    extension = modes[lang]

    self.screenshots.find do |s|
      ext = File.basename(s.filename.to_s, ".*").split("_").last

      ext == extension
    end
  end

  def to_s
    name
  end

  def extract_mode filename
    ext = File.basename(filename, ".*").split("_").last

    Mode.find_by(extension: ext)
  end
end
