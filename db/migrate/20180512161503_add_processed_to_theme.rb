class AddProcessedToTheme < ActiveRecord::Migration[5.1]
  def change
    add_column :themes, :processed, :boolean
  end
end
