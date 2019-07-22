require 'rails_helper'

RSpec.describe Mode, type: :model do
  before do
    @mock_args = {
      name: "Lisp"
    }
  end

  it "has a valid factory" do
    m = Mode.new(@mock_args)

    expect(m).to be_valid
  end

  it "requires a name" do
    m = Mode.new(name: nil)

    expect(m).to_not be_valid
  end

  it "has a unique name" do
    Mode.new(@mock_args).save

    m = Mode.new(@mock_args)

    expect(m).to_not be_valid
  end
end
