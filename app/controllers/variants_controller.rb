class VariantsController < ApplicationController
  before_action :set_variant, :set_mode_info, :set_other_variants_info

  def show
    @multi = @theme.variants.length > 1
    @screenshot = @variant.screenshots.find_by(mode: @mode)
    @url = @theme.url.nil? ? "https://melpa.org/#/#{@theme.name}" : @theme.url
  end

  private

  def set_other_variants_info
    @index = @variants.find_index @variant
    @previous = @variants[@index-1] unless @index == 0
    @next = @variants[@index+1]
  end

  def set_mode_info
    lang = request.params[:lang] || "lisp"

    @mode = @modes.find_by(extension: lang ) || @modes.find_by(name: "Lisp")
    @mode_index = @modes.find_index @mode
    @previous_mode = @modes[@mode_index - 1] unless @mode_index == 0
    @next_mode = @modes[mode_next = @mode_index + 1]
  end

  def set_variant
    @theme = Theme.find_by(name: request.params[:theme_name])
    @title = @theme.name
    @variants = @theme.variants
    @variant = @variants.find { |v| v.name == request.params[:name] }
  end
end
