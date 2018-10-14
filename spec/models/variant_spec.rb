require 'rails_helper'
require_relative '../../lib/parsing'

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

  describe "parse!" do
    before do
      allow(File).to receive(:open).and_return :file
      allow(Dir).to receive(:glob).and_return ["v1", "v2"]
      allow(Dir).to receive(:chdir).and_yield
      allow(@variant).to receive_message_chain("screenshots.attach")
    end

    it "changes to the screenshot folder directory" do
      @variant.parse!

      expect(Dir)
        .to have_received(:chdir)
              .with(PeachMelpa::Parsing::SCREENSHOT_FOLDER + @theme.name)
      end

    it "scans the screenshots directory for its own screenshots" do
      @variant.parse!

      expect(Dir).to have_received(:glob).with "#{@variant.name}_*"
    end

    it "attaches the screenshots found" do
      @variant.parse!

      ["v1", "v2"].each do |entry|
        expect(File)
          .to have_received(:open)
                .with(File.join(
                        PeachMelpa::Parsing::SCREENSHOT_FOLDER,
                        @theme.name,
                        entry))
        expect(@variant.screenshots).to have_received(:attach)
                                          .with(io: :file, filename: entry)
      end
    end
  end
end
