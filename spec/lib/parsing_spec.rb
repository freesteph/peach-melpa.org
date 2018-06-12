require 'json'
require './lib/parsing'
require './lib/retrieval'

RSpec.describe PeachMelpa::Parsing do
  include PeachMelpa::Parsing

  describe "pick_updated_themes" do
    before :each do
      allow(JSON).to receive(:parse)
      allow(IO).to receive(:read).and_return :res
      allow(PeachMelpa::Parsing).to receive(:select_themes).and_return []
    end

    it "parses the archive file" do
      PeachMelpa::Parsing.pick_updated_themes

      expect(IO)
        .to have_received(:read)
              .with(PeachMelpa::Retrieval::ARCHIVE_PATH)

      expect(JSON)
        .to have_received(:parse)
              .with(:res)
    end

    it "selects all themes ending with -name" do
      allow(JSON).to receive(:parse).and_return :res

      PeachMelpa::Parsing.pick_updated_themes

      expect(PeachMelpa::Parsing)
        .to have_received(:select_themes)
              .with(:res)
    end

    it "creates or finds a theme with the extracted name" do
      allow(PeachMelpa::Parsing).to receive(:select_themes).and_return(["foo"])
      allow(PeachMelpa::Parsing).to receive(:parse_theme)

      PeachMelpa::Parsing.pick_updated_themes

      expect(PeachMelpa::Parsing)
        .to have_received(:parse_theme)
              .with("foo")
    end
  end

  describe "parse_theme" do
    mock_theme = [
      "foo-theme", {
        "ver" => [0, 1],
        "deps" => "deps",
        "desc" => "some theme",
        "type" => "single",
        "props" => {
          "commit" => "commit hash",
          "keywords" => ["keyword"],
          "url" => "https://some.url/to/theme"
        }
      }
    ]

    theme = nil

    before :each do
      theme = double()
      allow(theme).to receive(:older_than?)
      allow(theme).to receive(:update_screenshots!)

      allow(Theme).to receive(:find_or_create_by).and_return theme
    end

    it "finds or creates a theme with the theme radical" do
      PeachMelpa::Parsing.parse_theme mock_theme

      expect(Theme)
        .to have_received(:find_or_create_by)
              .with(name: "foo")
    end

    it "checks if the theme needs updating" do
      PeachMelpa::Parsing.parse_theme mock_theme

      expect(theme).to have_received(:older_than?).with "0.1"
    end

    describe "if the theme needs updating" do
      before :each do
        allow(theme).to receive(:older_than?).and_return true
      end

      it "calls udpate_screenshots on it" do
        PeachMelpa::Parsing.parse_theme mock_theme

        expect(theme).to have_received(:update_screenshots!)
      end
    end
  end

  describe "select_themes" do
    # FIXME: could go to jail for writing code like that
    def make_melpa_struct name
      [name, :data]
    end

    it "selects members whose object key name ends with -theme " do
      foo_theme = make_melpa_struct "foo-theme"
      bar_theme = make_melpa_struct "bar-theme"
      foo = make_melpa_struct "foo"
      bar = make_melpa_struct "bar"

      [
        [[foo_theme, bar_theme], [foo_theme, bar_theme]],
        [[foo_theme, bar], [foo_theme]],
        [[foo_theme, bar, make_melpa_struct("bar-theme-foo")], [foo_theme]],
        [[], []],
        [[foo], []]
      ].map do |data, result|
        expect(PeachMelpa::Parsing.select_themes(data)).to eq(result)
      end
    end
  end
end
