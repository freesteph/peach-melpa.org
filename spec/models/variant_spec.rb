require 'rails_helper'

RSpec.describe Variant, type: :model do
  before do
    @theme = Theme.create!(name: 'poet')
    @variant = Variant.create!(theme: @theme, name: 'test')
  end

  it "is valid with a theme and a name" do
    expect(@variant).to be_valid
  end

  it "belongs to a theme" do
    @variant.update_attributes(theme: nil)
    expect(@variant).not_to be_valid
  end

  it "needs a theme" do
    @variant.update_attributes(name: nil)
    expect(@variant).not_to be_valid
  end

  it "needs a name unique in the scope of the theme" do
    other = Variant.new(theme: @theme, name: "test")
    expect(other).not_to be_valid

    other.update_attributes(theme: Theme.create!)
    expect(other).to be_valid
  end
end
