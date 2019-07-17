Rails.application.routes.draw do
  root to: "themes#index"

  resources :themes, only: [:index, :show], param: :name do
    resources :variants, param: :name
  end
  # For details on the DSL available within this file, see http://guides.rubyonrails.org/routing.html
end
