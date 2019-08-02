class ThemesController < ApplicationController
  before_action :set_theme, only: [:show, :edit, :update, :destroy]

  @@page_size = 12

  @lisp = Mode.find_by(name: "Lisp")

  # GET /themes
  # GET /themes.json
  def index
    @title = "Browse Emacs themes from MELPA"
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
    @title = @theme.name

    @variant = @theme.variants.first
    @multi = @theme.variants.length > 1
    @mode = @variant.mode_for @variant.screenshots.first.filename.to_s, @modes
  end

  private
    # Use callbacks to share common setup or constraints between actions.
    def set_theme
      @theme = Theme.find_by(name: params[:name])
    end

    # Never trust parameters from the scary internet, only allow the white list through.
    def theme_params
      params.require(:theme).permit(:name, :version)
    end

    def page

    end
end
