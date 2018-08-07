class ApplicationController < ActionController::Base
  protect_from_forgery with: :exception

  MODES = {
    "Lisp" => "el",
    "Javascript" => "js",
    "C" => "c",
    "Ruby" => "rb"
  }
end
