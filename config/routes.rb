# frozen_string_literal: true

Rails.application.routes.draw do
  root to: 'themes#index'

  resources :themes, only: %i[index show], param: :name do
    resources :variants, param: :variant, only: :show
  end
end
