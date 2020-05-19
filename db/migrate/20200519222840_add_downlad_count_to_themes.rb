class AddDownladCountToThemes < ActiveRecord::Migration[6.0]
  def change
    add_column :themes, :download_count, :bigint
  end
end
