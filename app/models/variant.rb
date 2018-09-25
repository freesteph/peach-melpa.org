require_relative '../../lib/parsing'

class Variant < ApplicationRecord
  belongs_to :theme
  has_many_attached :screenshots
  validates :name, presence: true, uniqueness: { scope: :theme }

  def parse!
    assets_path = PeachMelpa::Parsing::SCREENSHOT_FOLDER
    Dir.chdir(assets_path) do
      Dir.glob("#{self.name}_*").each do |name|
        self.screenshots.attach(
          io: File.open(assets_path + name),
          filename: name
        )
      end
    end
  end
end
