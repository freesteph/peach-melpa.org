# frozen_string_literal: true

class ThemesController < ApplicationController
  before_action :set_theme, only: %i[show]

  @page_size = 12
  @lisp = Mode.find_by(name: 'Lisp')

  # GET /themes
  # GET /themes.json
  def index
    set_page_title_for 'home'
    @page = (request.params[:page] || 1).to_i

    offset = (@page - 1) * @@page_size

    @count = Theme.perfect.count

    @previous = if @page == 2
                  themes_path
                else
                  themes_path(page: @page - 1)
                end

    @next = if offset + @@page_size >= @count
              nil
            else
              themes_path(page: @page + 1)
            end

    @themes = Theme
              .perfect
              .offset(offset)
              .limit(@@page_size)
              .order(version: :desc)
  end

  # GET /themes/1
  # GET /themes/1.json
  def show
    # FIXME: redirect this to variants or do some transparent hoisting
  end

  private

  # Use callbacks to share common setup or constraints between actions.
  def set_theme
    @theme = Theme.find_by(name: params[:name])
  end

  def theme_params
    params.require(:theme).permit(:name, :version)
  end
end
