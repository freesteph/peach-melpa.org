require 'timeout'
require 'rails_helper'
require_relative '../../lib/parsing'
require_relative '../../lib/errors'

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

  describe "update_screenshots!" do
    before :each do
      @mock_args = {
        version: "2",
        description: "text",
        url: "url",
        authors: "John, Doe",
        kind: "single"
      }

      @theme = Theme.create!(name: "foo-theme")
      @variant = @theme.variants.create!(name: "bar")

      allow(@variant).to receive(:parse!)
      allow(@theme).to receive_message_chain("screenshots.attach")
      allow(@theme).to receive_message_chain("screenshots.purge")
      allow(@theme)
        .to receive_message_chain("variants.destroy_all")
      allow(@theme)
        .to receive_message_chain("variants.find_or_create_by")
              .and_return @variant
      allow(@theme).to receive(:devise_variants).and_return [:variant]
      allow(@theme).to receive(:cleanup_old_screenshots!)

      allow(Kernel).to receive(:spawn).and_return :pid
      allow(Process).to receive(:wait) { `(exit 0)` }
      allow(Process).to receive(:kill)

      allow(Timeout).to receive(:timeout).and_yield
    end

    it "calls cleanup_old_screenshots! before starting" do
      @theme.update_screenshots! @mock_args

      expect(@theme).to have_received(:cleanup_old_screenshots!).once
    end

    it "wraps the command between a Timeout block" do
      @theme.update_screenshots! @mock_args

      expect(Timeout).to have_received(:timeout).with(30)
    end

    it "calls the Kernel.spawn method with the name and new version" do
      pending "need to update with the daemon arch"
      @theme.update_screenshots! @mock_args

      expect(Kernel).to have_received(:spawn)
                          .with "emacs -Q -l lib/take-screenshot.el -eval '(fetch-and-load-theme \"foo-theme\" \"2\" \"single\")'"
    end

    it "waits for the process to finish" do
      @theme.update_screenshots! @mock_args

      expect(Process).to have_received(:wait).with :pid
    end

    context "when the cmd exits properly" do
      before do
        allow(Dir).to receive(:chdir).and_yield
        allow(Dir).to receive(:glob).and_return ["one", "two"]
        allow(File).to receive(:delete)
        allow(@theme).to receive(:radical).and_return :rad
      end

      it "deletes the old screenshots" do
        allow(@theme).to receive(:update_attributes!)

        @theme.update_screenshots! @mock_args

        expect(@theme.screenshots).to have_received(:purge).once
      end

      it "stores the new version into the theme" do
        allow(@theme).to receive(:update_attributes!)

        @theme.update_screenshots! @mock_args

        expect(@theme).to have_received(:update_attributes!).once.with(@mock_args)
      end

      it "changes to the screenshot folder directory" do
        @theme.update_screenshots! @mock_args

        expect(Dir).to have_received(:chdir).with(PeachMelpa::Parsing::SCREENSHOT_FOLDER)
      end

      it "uses Dir.glob and the theme radical to capture all the screenshots" do
        @theme.update_screenshots! @mock_args

        expect(Dir).to have_received(:glob).with("rad*")
      end

      it "calls devise_variants with the results" do
        @theme.update_screenshots! @mock_args

        expect(@theme).to have_received(:devise_variants).with(["one", "two"])
      end

      it "deletes all variants beforehand" do
        @theme.update_screenshots! @mock_args

        expect(@theme.variants)
          .to have_received(:destroy_all)
      end

      it "finds or create a variant with the resulting names" do
        @theme.update_screenshots! @mock_args

        expect(@theme.variants)
          .to have_received(:find_or_create_by)
                .with(name: :variant)
      end

      it "calls parse! on each variant" do
        @theme.update_screenshots! @mock_args

        expect(@variant).to have_received(:parse!)
      end

      it "deletes all the files after" do
        @theme.update_screenshots! @mock_args

        expect(File).to have_received(:delete).with("one", "two")
      end
    end

    context "when the Emacs subprocess fails" do
      before :each do
        allow(Process).to receive(:wait) { `(exit 1)` }
      end

      it "does not raises the error" do
        expect{ @theme.update_screenshots! @mock_args }.to_not raise_error
      end

      it "does not touch the theme version" do
        @theme.update_screenshots! @mock_args

        expect(@theme.version).to eq(nil)
      end

      it "should collect some debug information"
    end

    context "when the command takes too long" do
      before :each do
        allow(Process).to receive(:wait).and_raise(Timeout::Error)
      end

      it "kills the process" do
        @theme.update_screenshots! @mock_args

        expect(Process).to have_received(:kill).with("TERM", :pid)
      end


      it "does not touch the theme version" do
        @theme.update_screenshots! @mock_args

        expect(@theme.version).to eq(nil)
      end

      it "should collect some debug information"
    end

    context "when any other error arise" do
      before :each do
        allow(Kernel).to receive(:spawn).and_raise "zut"
      end

      it "raises the error" do
        expect{ @theme.update_screenshots! @mock_args }
          .to raise_error
      end
    end
  end

  describe "devise_variants" do
    it "infers the theme variants based on the list of screenshots" do
      test_data = %w(
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
      )

      theme = Theme.create!(name: 'poet-theme')
      puts theme.inspect

      expect(theme.devise_variants(test_data))
        .to match_array ["poet", "poet-monochrome", "poet-dark-monochrome"]
    end
  end

  describe ".radical" do
    it "gets the theme name without the suffix" do
      [
        ["foo-theme", "foo"],
        ["some-rad-theme", "some-rad"],
        ["combo-themes", "combo"]
      ].each do |name, radical|
        expect(Theme.new(name: name).radical).to eq radical
      end
    end
  end
end
