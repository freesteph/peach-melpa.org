require './lib/retrieval'

require 'open-uri'

RSpec.describe PeachMelpa::Retrieval do
  include PeachMelpa::Retrieval

  describe "refresh_melpa_archive" do
    it "should call open-uri's open" do
      refresh_melpa_archive
      allow(OpenURI).to receive(:open)

      expect(OpenURI).to have_received(:open).with("https://melpa.org/archive.json")
    end
  end
end
\
