require 'net/http'

require './lib/retrieval'

RSpec.describe PeachMelpa::Retrieval do
  include PeachMelpa::Retrieval

  describe "refresh_melpa_archive" do
    file = nil

    before :each do
      file = double
      allow(file).to receive(:write)
      allow(Net::HTTP).to receive(:get).and_return :response
      allow(File).to receive(:write).and_yield(file)
    end

    before :each do
      PeachMelpa::Retrieval.refresh_melpa_archive
    end

    it "should call net open with the MELPA archive" do
      expect(Net::HTTP).to have_received(:get)
                             .with(
                               PeachMelpa::Retrieval::MELPA_HOST,
                               PeachMelpa::Retrieval::MELPA_ARCHIVE_NAME)
    end

    it "should write the result into the tmp/archive.json file" do
      expect(File).to have_received(:write)
                        .with("w+", PeachMelpa::Retrieval::ARCHIVE_PATH)
      expect(file).to have_received(:write).with(:response)
    end
  end
end
