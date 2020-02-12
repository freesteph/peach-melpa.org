# frozen_string_literal: true

class AddBlacklistedToThemes < ActiveRecord::Migration[5.2]
  def change
    add_column :themes, :blacklisted, :boolean
  end
end
