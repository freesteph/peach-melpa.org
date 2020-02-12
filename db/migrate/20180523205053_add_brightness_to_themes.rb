# frozen_string_literal: true

class AddBrightnessToThemes < ActiveRecord::Migration[5.2]
  def change
    add_column :themes, :brightness, :float
  end
end
