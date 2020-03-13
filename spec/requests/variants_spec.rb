# frozen_string_literal: true

require 'rails_helper'

RSpec.describe 'Variants', type: :request do
  describe 'GET /variants/:id' do
    let!(:variant) { create(:variant) }

    it 'redirects to the theme and forwards params' do
      get theme_variant_path(
        variant.theme,
        variant,
        lang: Mode.last.extension
      )

      expect(response).to redirect_to theme_path(
        variant.theme,
        variant: variant,
        lang: Mode.last.extension
      )
    end
  end
end
