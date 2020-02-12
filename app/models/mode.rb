# frozen_string_literal: true

class Mode < ApplicationRecord
  validates :name, presence: true
  validates :extension, presence: true, uniqueness: true
end
