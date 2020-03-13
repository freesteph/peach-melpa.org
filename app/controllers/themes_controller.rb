# frozen_string_literal: true

class ThemesController < ApplicationController
  before_action :set_theme,
                :set_variant,
                :set_mode,
                :set_pagination,
                only: %i[show]

  PAGE_SIZE = 12
  @lisp = Mode.find_by(name: 'Lisp')

  # GET /themes
  # GET /themes.json
  def index
    set_page_title_for 'home'
    @page = (request.params[:page] || 1).to_i

    offset = (@page - 1) * PAGE_SIZE

    @count = Theme.perfect.count

    @previous = if @page == 2
                  themes_path
                else
                  themes_path(page: @page - 1)
                end

    @next = if offset + PAGE_SIZE >= @count
              nil
            else
              themes_path(page: @page + 1)
            end

    @themes = Theme
              .perfect
              .offset(offset)
              .limit(PAGE_SIZE)
              .order(version: :desc)
  end

  # GET /themes/1
  # GET /themes/1.json
  def show
    set_page_title_for 'show',
                       name: @theme.name,
                       description: @theme.description

    @multi = @theme.variants.length > 1
    @screenshot = @variant.screenshots.find_by(mode: @mode)
    @url = @theme.url.nil? ? "https://melpa.org/#/#{@theme.name}" : @theme.url
    @title = @theme.name
  end

  private

  def set_theme
    @theme = Theme.find_by(name: params[:name])
  end

  def set_variant
    @variants = @theme.variants
    @variant =
      @variants.find_by(name: request.params[:variant]) ||
      @variants.first
  end

  def set_mode
    lang = request.params[:lang] || 'lisp'
    @mode = @modes.find_by(extension: lang) || @modes.find_by(name: 'Lisp')
  end

  def set_pagination
    mode_index = @modes.find_index @mode

    @previous_mode = @modes[mode_index - 1] unless mode_index.zero?
    @next_mode = @modes[mode_index + 1]

    index = @variants.find_index @variant
    @previous = @variants[index - 1] unless index.zero?
    @next = @variants[index + 1]
  end

  def theme_params
    params.require(:theme).permit(:name, :version)
  end
end
