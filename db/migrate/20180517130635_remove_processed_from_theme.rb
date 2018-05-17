class RemoveProcessedFromTheme < ActiveRecord::Migration[5.2]
  def change
    remove_column :themes, :processed, :boolean
  end
end
