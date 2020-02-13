# frozen_string_literal: true

require 'json'
require 'rails_helper'
require './lib/parsing'
require './lib/retrieval'

RSpec.describe PeachMelpa::Parsing do
  include PeachMelpa::Parsing

  before :each do
    @mock_theme = JSON.parse(
      <<~THEME
        {
          "foo-theme": {
            "ver": [
              0,
              1
            ],
            "deps": null,
            "desc": "some theme",
            "type": "single",
            "props": {
              "commit": "7f45ab9e23164d65538edb2beb9692ecdc24c31e",
              "authors": [
                "Stéphane Maniaci <steph@rspec.org>"
              ],
              "maintainer": "Stéphane Maniaci <steph@rspec.org>",
              "url": "http://github.com/freesteph/peach-melpa"
            }
          }
        }
      THEME
    )
    @name, @props = @mock_theme.to_a.first
  end

  describe 'pick_updated_themes' do
    before :each do
      allow(JSON).to receive(:parse).and_return @mock_theme
      allow(IO).to receive(:read).and_return :res
      allow(PeachMelpa::Parsing).to receive(:select_themes).and_return @mock_theme
      allow(PeachMelpa::Parsing).to receive(:start_daemon)
      allow(PeachMelpa::Parsing).to receive(:stop_daemon)
      allow(PeachMelpa::Parsing).to receive(:parse_theme)
    end

    it 'parses the archive file' do
      PeachMelpa::Parsing.pick_updated_themes

      expect(IO)
        .to have_received(:read)
        .with(PeachMelpa::Retrieval::ARCHIVE_PATH)

      expect(JSON)
        .to have_received(:parse)
        .with(:res)
    end

    context "if an `only' argument is given" do
      it 'runs solely for that theme' do
        PeachMelpa::Parsing.pick_updated_themes only: 'foo-theme'

        expect(PeachMelpa::Parsing).to have_received(:parse_theme).once
      end

      it 'does not run select_themes' do
        PeachMelpa::Parsing.pick_updated_themes only: 'foo-theme'

        expect(PeachMelpa::Parsing).to_not have_received(:select_themes)
      end

      it 'does not run if not matching theme found' do
        PeachMelpa::Parsing.pick_updated_themes only: 'bar-theme'

        expect(PeachMelpa::Parsing).not_to have_received(:parse_theme)
      end
    end

    context "if a `force' argument is given" do
      it 'forwards it to parse_them' do
        PeachMelpa::Parsing.pick_updated_themes force: 'yes'

        name, props = @mock_theme.to_a.first

        expect(PeachMelpa::Parsing)
          .to have_received(:parse_theme).with(name, props, force: true)
      end
    end

    context 'if no arguments are given' do
      it 'parses every returned theme' do
        allow(PeachMelpa::Parsing).to receive(:select_themes).and_return Array.new(10)

        PeachMelpa::Parsing.pick_updated_themes

        expect(PeachMelpa::Parsing).to have_received(:parse_theme).exactly(10).times
      end
    end

    it 'selects all themes ending with -theme' do
      allow(JSON).to receive(:parse).and_return :res

      PeachMelpa::Parsing.pick_updated_themes

      expect(PeachMelpa::Parsing)
        .to have_received(:select_themes)
        .with(:res)
    end

    it 'creates or finds a theme with the extracted name' do
      allow(PeachMelpa::Parsing).to receive(:select_themes).and_return(@mock_theme)
      allow(PeachMelpa::Parsing).to receive(:parse_theme)

      PeachMelpa::Parsing.pick_updated_themes

      expect(PeachMelpa::Parsing)
        .to have_received(:parse_theme)
        .with(@name, @props)
    end

    it "creates the screenshots folders if it doesn't exist" do
      allow(Dir).to receive(:exist?).and_return false
      allow(Dir).to receive(:mkdir)

      PeachMelpa::Parsing.pick_updated_themes

      expect(Dir).to have_received(:mkdir).with PeachMelpa::Parsing::SCREENSHOT_FOLDER
    end
  end

  describe 'parse_theme' do
    before :each do
      @theme = double
      allow(@theme).to receive(:older_than?)
      allow(@theme).to receive(:update_screenshots!)
      allow(@theme).to receive(:blacklisted?).and_return false

      allow(Theme).to receive(:find_or_create_by).and_return @theme
    end

    it 'finds or creates a theme' do
      PeachMelpa::Parsing.parse_theme @name, @props

      expect(Theme)
        .to have_received(:find_or_create_by)
        .with(name: 'foo-theme')
    end

    it 'checks if the theme needs updating' do
      PeachMelpa::Parsing.parse_theme @name, @props

      expect(@theme).to have_received(:older_than?).with '0.1'
    end

    context 'if the theme needs updating' do
      before :each do
        allow(@theme).to receive(:older_than?).and_return true
      end

      it 'calls update_screenshots on it with the formatted attributes' do
        PeachMelpa::Parsing.parse_theme @name, @props

        expect(@theme).to have_received(:update_screenshots!)
          .with(
            version: '0.1',
            description: 'some theme',
            url: 'http://github.com/freesteph/peach-melpa',
            authors: 'Stéphane Maniaci <steph@rspec.org>',
            kind: 'single'
          )
      end

      describe 'authors parsing' do
        context 'when there are multiple authors' do
          it 'formats them in a single string' do
            multi = @props.dup
            multi['props']['authors'] = %w[John Doe]

            PeachMelpa::Parsing.parse_theme @name, multi

            expect(@theme).to have_received(:update_screenshots!)
              .with(
                a_hash_including(authors: 'John, Doe')
              )
          end
        end

        context "when the field doesn't exist" do
          it 'sends an empty string' do
            none = @props.dup
            none['props'].delete 'authors'

            PeachMelpa::Parsing.parse_theme @name, none

            expect(@theme).to have_received(:update_screenshots!)
              .with(
                a_hash_including(authors: '')
              )
          end
        end
      end

      context 'but the theme is blacklisted' do
        before :each do
          allow(@theme).to receive(:blacklisted?).and_return true
        end

        it 'does not call update_screenshots!' do
          PeachMelpa::Parsing.parse_theme @name, @props

          expect(@theme).to_not have_received(:update_screenshots!)
        end
      end
    end

    context 'if the theme does not need updating' do
      before :each do
        allow(@theme).to receive(:older_than?).and_return false
      end

      context 'but the force option is given' do
        it 'updates the theme nevertheless' do
          PeachMelpa::Parsing.parse_theme @name, @props, force: true

          expect(@theme).to have_received(:update_screenshots!)
        end
      end
    end
  end

  describe 'looks_like_theme?' do
    it 'select names ending with theme[s]?' do
      %w[foo-theme bar-baz-theme some-themes].each do |theme|
        expect(PeachMelpa::Parsing.looks_like_theme?(theme)).to be_truthy
      end

      %w[foo_theme foo-theme-pkg bartheme].each do |pkg|
        expect(PeachMelpa::Parsing.looks_like_theme?(pkg)).to_not be_truthy
      end
    end
  end

  describe 'select_themes' do
    before :each do
      allow(PeachMelpa::Parsing).to receive(:looks_like_theme?).and_return false
    end

    it 'selects members who pass looks_like_theme?' do
      themes = PeachMelpa::Parsing.select_themes @mock_theme

      expect(PeachMelpa::Parsing)
        .to have_received(:looks_like_theme?).with @name

      expect(themes).to be_empty
    end
  end
end
