# frozen_string_literal: true

require 'rails_helper'

RSpec.describe Screenshot, type: :model do
  describe 'validation' do
    it 'has a valid factory' do
      expect(build(:screenshot)).to be_valid
    end

    it 'is not valid without a variant' do
      expect(build(:screenshot, variant: nil)).not_to be_valid
    end

    it 'is not valid without a mode' do
      expect(build(:screenshot, mode: nil)).not_to be_valid
    end
  end
end
