# frozen_string_literal: true

require 'rails_helper'

RSpec.describe 'Themes', type: :request do
  describe 'GET /themes' do
    it 'works! (now write some real specs)' do
      get themes_path
      expect(response).to have_http_status(200)
    end
  end

  describe 'GET /theme/:id' do
    let!(:theme) { create(:theme) }

    it 'has a valid show route' do
      get theme_path(theme)
      expect(response).to have_http_status(200)
    end
  end
end
