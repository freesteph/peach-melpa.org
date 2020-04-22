# frozen_string_literal: true

require 'rails_helper'

RSpec.describe 'Themes', type: :request do
  before do
    create(:mode, name: 'Lisp')
  end

  describe 'GET /themes' do
    it 'works' do
      get themes_path
      expect(response).to have_http_status(200)
    end
  end

  describe 'GET /theme/:id' do
    let!(:theme) { create(:theme, :with_full_variant) }

    it 'has a valid show route' do
      get theme_path(theme)
      expect(response).to have_http_status(200)
    end

    context 'when the theme does not exist' do
      let(:response) { get theme_path(build(:theme)) }

      it 'redirects to the home page if the theme is not found' do
        expect(response).to redirect_to root_path
      end

      it 'sets the notice flash' do
        skip 'not sure how to test for flash presence'
      end
    end

    context 'when the theme has no variants' do
      let(:response) { get theme_path(create(:theme)) }

      it 'redirects to the home page if the theme has no variants' do
        expect(response).to redirect_to root_path
      end

      it 'sets the notice flash' do
        skip 'not sure how to test for flash presence'
      end
    end
  end
end
