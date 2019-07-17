class ThemesController < ApplicationController
  before_action :set_theme, only: [:show, :edit, :update, :destroy]
  before_action :set_modes

  # GET /themes
  # GET /themes.json
  def index
    @title = "Browse Emacs themes from MELPA"

    all = Theme.order(version: :desc).includes(:variants)

    @themes = all.reject { |t| t.variants.empty? }
  end
  # GET /themes/1
  # GET /themes/1.json
  def show
    @title = @theme.name

    @variant = @theme.variants.first
    @multi = @them.variants.length > 1
    @mode = @variant.mode_for @variant.screenshots.first.filename.to_s, @modes
  end

  private
    # Use callbacks to share common setup or constraints between actions.
    def set_theme
      @theme = Theme.find_by(name: params[:name])
    end

    def set_modes
      @modes = MODES
    end

    # Never trust parameters from the scary internet, only allow the white list through.
    def theme_params
      params.require(:theme).permit(:name, :version)
    end
end
