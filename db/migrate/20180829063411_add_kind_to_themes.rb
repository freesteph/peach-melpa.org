# frozen_string_literal: true

class AddKindToThemes < ActiveRecord::Migration[5.2]
  def change
    add_column :themes, :kind, :string
  end
end
