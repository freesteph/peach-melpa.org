class Theme < ApplicationRecord
  has_one_attached :screenshot

  def to_param
    name
  end

  def older_than? version
    if not self.version
      return true
    else
      return self.version < version
    end
  end
end
