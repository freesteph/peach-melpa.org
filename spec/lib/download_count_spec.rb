# frozen_string_literal: true

require './lib/download_count'

RSpec.describe PeachMelpa::DownloadCount do
  before do
    allow(File).to receive(:read).and_return '{"foo": 234}'
  end

  describe '#count_for' do
    it 'needs a theme' do
      expect { subject.count_for(nil) }.to raise_error 'need to pass a theme'
    end

    it 'returns the matching download count' do
      expect(subject.count_for('foo')).to eq 234
    end

    it 'raises if the theme is not found' do
      expect { subject.count_for('bar') }.to raise_error PeachMelpa::Errors::NoDownloadCount
    end
  end
end
