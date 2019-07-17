class VariantsController < ApplicationController
  before_action :set_variant

  def show
    p request.params
  end

  private

  def set_variant
    @theme = Theme.find_by(name: request.params[:theme_name])
    variants = @theme.variants
    @variant = variants.find { |v| v.name == request.params[:name] }
    @index = variants.find_index @variant
    @previous = variants[@index-1] unless @index == 0
    @next = variants[@index+1]
    @mode = @variant.mode_for @variant.screenshots.first.filename.to_s, MODES
  end
end
