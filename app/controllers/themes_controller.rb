class ThemesController < ApplicationController
  before_action :set_theme, only: [:show, :edit, :update, :destroy]

  # GET /themes
  # GET /themes.json
  def index
    @themes = Theme.order(version: :desc).with_attached_screenshot.select { |t| t.screenshot.attached? }

    brightness_range = @themes.map(&:brightness)

    @lowest = brightness_range.min
    @highest = brightness_range.max
  end
  # GET /themes/1
  # GET /themes/1.json
  def show
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
end
