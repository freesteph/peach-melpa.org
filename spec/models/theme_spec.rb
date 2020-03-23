# frozen_string_literal: true

require 'timeout'
require 'rails_helper'
require_relative '../../lib/parsing'
require_relative '../../lib/errors'

RSpec.describe Theme, type: :model do
  let(:theme) { create(:theme, :with_variant, name: 'foo-theme') }
  let(:variant) { theme.variants.first }
  let(:args) do
    {
      version: '2',
      description: 'text',
      url: 'url',
      authors: 'John, Doe',
      kind: 'single'
    }
  end

  describe 'validation' do
    it 'has a valid factory' do
      expect(build(:theme)).to be_valid
    end

    it 'is not valid without a name' do
      expect(build(:theme, name: nil)).to_not be_valid
    end

    context 'when another theme already exists under the same name' do
      it 'is not valid' do
        create(:theme, name: 'great')

        expect(build(:theme, name: 'great')).to_not be_valid
      end
    end
  end

  describe 'update_screenshots!' do
    before :each do
      allow(Kernel).to receive(:spawn).and_return :pid
      allow(Process).to receive(:wait) { `(exit 0)` }
      allow(Process).to receive(:kill)

      allow(Timeout).to receive(:timeout).and_yield
      allow(theme).to receive(:capture_artifacts!)
    end

    it 'wraps the command between a Timeout block' do
      theme.update_screenshots! args

      expect(Timeout).to have_received(:timeout).with(90)
    end

    it 'calls the Kernel.spawn method with the name and new version' do
      theme.update_screenshots! args

      expect(Kernel)
        .to have_received(:spawn)
        .with "emacsclient -s peach -n -c -eval '(fetch-and-load-theme \"foo-theme\")'"
    end

    it 'waits for the process to finish' do
      theme.update_screenshots! args

      expect(Process).to have_received(:wait).with :pid
    end

    context 'when the Emacs subprocess fails' do
      before :each do
        allow(Process).to receive(:wait) { `(exit 1)` }
      end

      it 'does not raises the error' do
        expect { theme.update_screenshots! args }.to_not raise_error
      end

      it 'does not touch the theme version' do
        theme.update_screenshots! args

        expect(theme.version).to eq(nil)
      end

      it 'should collect some debug information'
    end

    context "when the theme's screenshot folder doesn't exist" do
      before :each do
        allow(Process).to receive(:wait).and_raise(PeachMelpa::Errors::NoThemeScreenshotsFolder)
      end

      # FIXME: can probably diagnose this
    end

    context 'when the command takes too long' do
      before :each do
        allow(Process).to receive(:wait).and_raise(Timeout::Error)
      end

      it 'kills the process' do
        theme.update_screenshots! args

        expect(Process).to have_received(:kill).with('TERM', :pid)
      end

      it 'does not touch the theme version' do
        theme.update_screenshots! args

        expect(theme.version).to eq(nil)
      end

      it 'should collect some debug information'
    end

    context 'when any other error arise' do
      before :each do
        allow(Kernel).to receive(:spawn).and_raise 'zut'
      end

      it 'raises the error' do
        expect { theme.update_screenshots! args }
          .to raise_error
      end
    end
  end

  describe 'devise_variants' do
    it 'infers the theme variants based on the list of screenshots' do
      test_data = %w[
        poet_c.png
        poet-dark-monochrome_c.png
        poet-dark-monochrome_el.png
        poet-dark-monochrome_js.png
        poet-dark-monochrome_org.png
        poet-dark-monochrome_rb.png
        poet_el.png
        poet_js.png
        poet-monochrome_c.png
        poet-monochrome_el.png
        poet-monochrome_js.png
        poet-monochrome_org.png
        poet-monochrome_rb.png
        poet_org.png
        poet_rb.png
      ]

      theme = build(:theme, name: 'poet-theme')

      expect(theme.devise_variants(test_data))
        .to match_array %w[poet poet-monochrome poet-dark-monochrome]
    end
  end

  describe '.radical' do
    it 'gets the theme name without the suffix' do
      [
        %w[foo-theme foo],
        %w[some-rad-theme some-rad],
        %w[combo-themes combo],
        %w[color-theme-sanityinc-tomorrow sanityinc-tomorrow]
      ].each do |name, radical|
        expect(build(:theme, name: name).radical).to eq radical
      end
    end
  end

  describe '.capture_artifacts!' do
    let(:folder_path) { PeachMelpa::Parsing::SCREENSHOT_FOLDER + 'foo-theme' }

    before do
      allow(variant).to receive(:parse!)
      allow(variant).to receive_message_chain('screenshots.attach')
      allow(variant).to receive_message_chain('screenshots.purge')
      allow(theme)
        .to receive_message_chain('variants.destroy_all')
      allow(theme)
        .to receive_message_chain('variants.find_or_create_by')
        .and_return variant
      allow(theme).to receive(:devise_variants).and_return [:variant]

      allow(Dir).to receive(:chdir).and_yield
      allow(Dir).to receive(:glob).and_return %w[one two]
      allow(Dir).to receive(:rmdir)
      allow(File).to receive(:delete)
      allow(Dir).to receive(:exist?).and_return true
      allow(theme).to receive(:radical).and_return :rad
    end

    context 'when the screenshots folder does not exist' do
      before do
        allow(Dir).to receive(:exist?).and_return false
      end

      it 'aborts the operation' do
        expect { theme.capture_artifacts! args }
          .to raise_error(PeachMelpa::Errors::NoThemeScreenshotsFolder)
      end
    end

    it 'stores the new version into the theme' do
      allow(theme).to receive(:update!)

      theme.capture_artifacts! args

      expect(theme).to have_received(:update!).once.with(args)
    end

    it 'changes to the screenshot/theme folder' do
      theme.capture_artifacts! args

      expect(Dir).to have_received(:chdir)
        .with(PeachMelpa::Parsing::SCREENSHOT_FOLDER).ordered
      expect(Dir).to have_received(:chdir)
        .with('foo-theme').ordered
    end

    it 'uses Dir.glob to capture all the screenshots' do
      theme.capture_artifacts! args

      expect(Dir).to have_received(:glob).with('*')
    end

    it 'calls devise_variants with the results' do
      theme.capture_artifacts! args

      expect(theme).to have_received(:devise_variants).with(%w[one two])
    end

    it 'deletes all variants beforehand' do
      theme.capture_artifacts! args

      expect(theme.variants)
        .to have_received(:destroy_all)
    end

    it 'finds or create a variant with the resulting names' do
      theme.capture_artifacts! args

      expect(theme.variants)
        .to have_received(:find_or_create_by)
        .with(name: :variant)
    end

    it 'calls parse! on each variant' do
      theme.capture_artifacts! args

      expect(variant).to have_received(:parse!)
    end

    it 'deletes all the files after' do
      theme.capture_artifacts! args

      expect(File).to have_received(:delete).with('one', 'two')
      expect(Dir).to have_received(:rmdir).with('foo-theme')
    end
  end
end
