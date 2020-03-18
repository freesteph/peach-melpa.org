# frozen_string_literal: true

class VariantsController < ApplicationController
  def show
    theme = Theme.find_by(name: params[:theme_name])
    variant = theme.variants.find_by(name: params[:variant])
    mode = Mode.find_by(extension: params[:lang]) || Mode.first

    redirect_to theme_path(theme, variant: variant, lang: mode.extension)
  end
end
