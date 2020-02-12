# frozen_string_literal: true

class AddAuthorsToThemes < ActiveRecord::Migration[5.2]
  def change
    add_column :themes, :authors, :string
  end
end
