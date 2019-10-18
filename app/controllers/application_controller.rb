class ApplicationController < ActionController::Base
  protect_from_forgery with: :exception

  before_action :set_modes

  private

  def set_modes
    @modes = Mode.all
  end


  def set_page_title_for(descriptor, args = {})
    @title = t("pages.#{descriptor}.title", args)
    @description = t("pages.#{descriptor}.description", args)
  end

end
