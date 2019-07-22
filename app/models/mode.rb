class Mode < ApplicationRecord
  validates :name, presence: true, uniqueness: true
end
