# frozen_string_literal: true

class CreateVariants < ActiveRecord::Migration[5.2]
  def change
    create_table :variants do |t|
      t.references :theme, foreign_key: true
      t.string :name

      t.timestamps
    end
  end
end
