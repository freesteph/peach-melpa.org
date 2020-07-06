# frozen_string_literal: true

require 'net/http'

require './lib/retrieval'

RSpec.describe PeachMelpa::Retrieval do
  include PeachMelpa::Retrieval

  describe 'archive_url' do
    before do
      allow(URI).to receive(:join).and_return :url
    end

    it 'uses URI to concatenate an HTTPS URI' do
      expect(PeachMelpa::Retrieval.archive_url).to eq :url
    end
  end

  describe 'refresh_melpa_archive' do
    before :each do
      @resp = double
      allow(@resp).to receive(:encode).and_return :response
      allow(PeachMelpa::Retrieval).to receive(:archive_url).and_return :archive_url
      allow(Net::HTTP).to receive(:get).and_return @resp
      allow(File).to receive(:write)
    end

    before :each do
      PeachMelpa::Retrieval.refresh_melpa_archive
    end

    it 'should call net open with the MELPA archive' do
      expect(Net::HTTP)
        .to have_received(:get)
        .with(:archive_url)
    end

    it 'should write the result into the tmp/archive.json file' do
      expect(File)
        .to have_received(:write)
        .with(PeachMelpa::Retrieval::ARCHIVE_PATH, :response)
    end
  end
end
