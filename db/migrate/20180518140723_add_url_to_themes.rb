# frozen_string_literal: true

class AddUrlToThemes < ActiveRecord::Migration[5.2]
  def change
    add_column :themes, :url, :string
  end
end
