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
      }

      @theme = Theme.new(name: "foo")
      allow(@theme).to receive_message_chain("screenshots.attach")

      allow(Kernel).to receive(:spawn).and_return :pid
      allow(Process).to receive(:wait) { `(exit 0)` }
      allow(Process).to receive(:kill)

      allow(Timeout).to receive(:timeout).and_yield
      allow(File).to receive(:open).and_return :file
    end

    it "wraps the command between a Timeout block" do
      @theme.update_screenshots! @mock_args

      expect(Timeout).to have_received(:timeout).with(10)
    end

    it "calls the Kernel.spawn method with the name and new version" do
      @theme.update_screenshots! @mock_args

      expect(Kernel).to have_received(:spawn)
                          .with(Theme::CMD % [@theme.name, @mock_args[:version]])
    end

    it "waits for the process to finish" do
      @theme.update_screenshots! @mock_args

      expect(Process).to have_received(:wait).with :pid
    end

    context "when the cmd exits properly" do
      before do
        allow(Dir).to receive(:chdir)
        allow(Dir).to receive(:glob).and_return ["1", "2"]
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

      it "uses Dir.glob to capture all the screenshots created" do
        @theme.update_screenshots! @mock_args

        expect(Dir).to have_received(:glob).with("foo_*")
      end

      %w(1 2).each do |entry|
        it "infers the screenshot path and attach it to the screenshot field" do
          @theme.update_screenshots! @mock_args

          expect(File).to have_received(:open)
                            .with(PeachMelpa::Parsing::SCREENSHOT_FOLDER + entry)
          expect(@theme.screenshots).to have_received(:attach)
                                         .with(io: :file, filename: entry)
        end
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
end
