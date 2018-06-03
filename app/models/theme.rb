class Theme < ApplicationRecord
  has_one_attached :screenshot

  def to_param
    name
  end
end
