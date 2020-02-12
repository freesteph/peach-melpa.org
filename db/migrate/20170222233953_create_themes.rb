# frozen_string_literal: true

class CreateThemes < ActiveRecord::Migration[5.0]
  def change
    create_table :themes do |t|
      t.string :name
      t.string :version

      t.timestamps
    end
  end
end
