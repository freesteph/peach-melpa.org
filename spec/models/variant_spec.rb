require 'rails_helper'
require_relative '../../lib/parsing'

RSpec.describe Variant, type: :model do
  before do
    @theme = Theme.create!(name: 'poet')
    @variant = Variant.create!(theme: @theme, name: 'test')
    @mode = Mode.new
    @screenshot = Screenshot.new # create!(variant: @variant, mode: @mode)
  end

  it "is valid with a theme and a name" do
    expect(@variant).to be_valid
  end

  it "belongs to a theme" do
    @variant.update(theme: nil)
    expect(@variant).not_to be_valid
  end

  it "needs a theme" do
    @variant.update(name: nil)
    expect(@variant).not_to be_valid
  end

  it "needs a name unique in the scope of the theme" do
    other = Variant.new(theme: @theme, name: "test")
    expect(other).not_to be_valid

    other.update(theme: Theme.create!(name: "other theme"))
    expect(other).to be_valid
  end

  describe "parse!" do
    before do
      allow(File).to receive(:open).and_return :file
      allow(Dir).to receive(:glob).and_return ["v1", "v2"]
      allow(Dir).to receive(:chdir).and_yield
      allow(@variant).to receive_message_chain("screenshots.create!").and_return @screenshot
      allow(@screenshot).to receive_message_chain("image.attach")
      allow(@variant).to receive(:extract_mode).and_return @mode
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

    it "calls extract_mode with each name found" do
      @variant.parse!

      expect(@variant).to have_received(:extract_mode).twice
    end

    it "creates screenshots with the correct mode" do
      @variant.parse!
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
        expect(@variant.screenshots).to have_received(:create!).twice.with(mode: @mode)
        expect(@screenshot.image).to have_received(:attach).with(io: :file, filename: entry)
      end
    end
  end

  describe "extract_mode" do
    before do
      @mode = Mode.create!(name: "foo", extension: "bar");
    end

    it "finds the relevant mode with the file extension" do
      m = @variant.extract_mode "bar"

      expect(m).to be_eql @mode
    end
  end
end
