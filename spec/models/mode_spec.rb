require 'rails_helper'

RSpec.describe Mode, type: :model do
  before do
    @mock_args = {
      name: "Lisp",
      extension: "el"
    }
  end

  it "has a valid factory" do
    m = Mode.new(@mock_args)

    expect(m).to be_valid
  end

  it "requires an extension" do
    m = Mode.new(name: "Foo")

    expect(m).to_not be_valid
  end

  it "requires a name" do
    m = Mode.new(extension: "el")

    expect(m).to_not be_valid
  end

  it "has a unique extension" do
    Mode.new(@mock_args).save

    m = Mode.new(@mock_args)

    expect(m).to_not be_valid
  end

  it "allows two modes with the same name" do
    Mode.new(@mock_args).save

    m = Mode.new(name: "Lisp", extension: "lp")

    expect(m).to be_valid
  end
end
