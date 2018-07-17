require_relative '../../lib/errors'

class Theme < ApplicationRecord
  has_many_attached :screenshots

  CMD = "emacs -Q -l lib/take-screenshot.el -eval '(fetch-and-load-theme \"%s\" \"%s\")'"

  def to_param
    name
  end

  def older_than? version
    if not self.version
      return true
    else
      return self.version < version
    end
  end

  def update_screenshots! new_attrs
    pid = nil

    begin
      Timeout::timeout(10) do
        pid = Kernel.spawn CMD % [self.name, new_attrs[:version]]
        Process.wait pid

        if not $?.success?
          raise PeachMelpa::Errors::EmacsError
        end

        Dir.chdir PeachMelpa::Parsing::SCREENSHOT_FOLDER
        Dir.glob("#{self.name}_*").each do |entry|
          asset_path = PeachMelpa::Parsing::SCREENSHOT_FOLDER + entry

          self.screenshots.attach(
            io: File.open(asset_path),
            filename: entry
          )
        end

        self.update_attributes!(new_attrs)
      end
    rescue Timeout::Error
      # the process hung
      Process.kill "TERM", pid
    rescue PeachMelpa::Errors::EmacsError
      # something bad happened in Emacs
    end
  end
end
