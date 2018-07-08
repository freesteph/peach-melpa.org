require 'rails_helper'

RSpec.describe Theme, type: :model do
  describe "older_than?" do
    it "is true if the theme has no version" do
      t = Theme.new
      expect(t.older_than? "any").to be(true)
    end

    it "is true if the theme's version is alphabetically superior" do
      t = Theme.new(version: "2018aaa")
      expect(t.older_than? "2019aaa").to be(true)
    end

    it "false if the theme's version is alphabetically inferior" do
      t = Theme.new(version: "2018bbb")
      expect(t.older_than? "2018aaa").to be(false)
    end
  end
end
