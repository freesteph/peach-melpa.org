class AddDescriptionToTheme < ActiveRecord::Migration[5.2]
  def change
    add_column :themes, :description, :string
  end
end
