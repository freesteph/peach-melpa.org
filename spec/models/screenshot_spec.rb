# frozen_string_literal: true

require 'rails_helper'

RSpec.describe Screenshot, type: :model do
  before do
    @theme = Theme.create!(name: 'theme')
    @variant = @theme.variants.create!(name: 'variant')
    @mode = Mode.create!(name: 'Lisp', extension: 'el')
  end

  it 'is valid with the default attributes' do
    s = Screenshot.create(variant: @variant, mode: @mode)

    expect(s).to be_valid
  end

  it 'is not valid without a variant' do
    s = Screenshot.create(variant: nil, mode: @mode)

    expect(s).not_to be_valid
  end

  it 'is not valid without a mode' do
    s = Screenshot.create(variant: @variant, mode: nil)

    expect(s).not_to be_valid
  end
end
