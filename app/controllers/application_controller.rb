class ApplicationController < ActionController::Base
  protect_from_forgery with: :exception

  before_action :set_modes

  private

  def set_modes
    @modes = Mode.all
  end
end
