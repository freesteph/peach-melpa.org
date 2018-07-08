require 'net/http'

require './lib/retrieval'

RSpec.describe PeachMelpa::Retrieval do
  include PeachMelpa::Retrieval

  describe "refresh_melpa_archive" do
    before :each do
      allow(Net::HTTP).to receive(:get).and_return :response
      allow(File).to receive(:write)
    end

    before :each do
      PeachMelpa::Retrieval.refresh_melpa_archive
    end

    it "should call net open with the MELPA archive" do
      expect(Net::HTTP)
        .to have_received(:get)
              .with(
                PeachMelpa::Retrieval::MELPA_HOST,
                PeachMelpa::Retrieval::MELPA_ARCHIVE_NAME
              )
    end

    it "should write the result into the tmp/archive.json file" do
      expect(File)
        .to have_received(:write)
              .with(PeachMelpa::Retrieval::ARCHIVE_PATH, :response)
    end
  end
end
