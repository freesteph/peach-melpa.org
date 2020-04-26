# frozen_string_literal: true

class ApplicationController < ActionController::Base
  protect_from_forgery with: :exception

  before_action :set_modes

  private

  def set_modes
    @modes = Mode.all
  end

  def set_page_title_for(descriptor, args = {})
    %w[title description header].each do |s|
      instance_variable_set "@#{s}", t("pages.#{descriptor}.#{s}", args)
    end
  end
end
