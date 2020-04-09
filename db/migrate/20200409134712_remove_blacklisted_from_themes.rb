class RemoveBlacklistedFromThemes < ActiveRecord::Migration[6.0]
  def change
    change_table :themes do |t|
      t.remove :blacklisted
      end
  end
end
