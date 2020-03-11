# frozen_string_literal: true

require 'rails_helper'

RSpec.describe Mode, type: :model do
  let(:mode) { build(:mode) }

  it 'has a valid factory' do
    expect(mode).to be_valid
  end

  it 'requires an extension' do
    expect(build(:mode, extension: nil)).to_not be_valid
  end

  it 'requires a name' do
    expect(build(:mode, name: nil)).to_not be_valid
  end

  it 'has a unique extension' do
    mode.save!
    expect(build(:mode, extension: mode.extension)).to_not be_valid
  end

  it 'allows two modes with the same name' do
    mode.save!

    expect(build(:mode, name: mode.name)).to be_valid
  end
end
