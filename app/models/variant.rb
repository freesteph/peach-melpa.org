class Variant < ApplicationRecord
  belongs_to :theme
  validates :name, presence: true, uniqueness: { scope: :theme }
end
