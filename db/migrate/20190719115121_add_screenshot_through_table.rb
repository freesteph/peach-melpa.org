class AddScreenshotThroughTable < ActiveRecord::Migration[6.0]
  def change
    create_table :modes do |t|
      t.column :name, :string, { null: false }
      t.column :extension, :string, { null: false }
      t.index :extension, { unique: true }
    end

    create_table :screenshots do |t|
      t.references :mode, foreign_key: true
      t.references :variant, foreign_key: true
    end
  end
end
