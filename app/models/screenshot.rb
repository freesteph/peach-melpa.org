class Screenshot < ApplicationRecord
  has_one_attached :image

  belongs_to :variant
  belongs_to :mode
end
