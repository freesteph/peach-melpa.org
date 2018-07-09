require_relative '../../lib/errors'

class Theme < ApplicationRecord
  has_one_attached :screenshot

  CMD = "emacs --batch -Q -l lib/take-screenshot.el -eval '(fetch-and-load-theme \"%s\" \"%s\")'"

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
      Timeout::timeout(5) do
        pid = Kernel.spawn CMD % [self.name, new_attrs[:version]]
        process = Process.wait pid

        if not process.success?
          raise PeachMelpa::Errors::EmacsError
        end

        asset_path = PeachMelpa::Parsing::SCREENSHOT_FOLDER + self.name + ".png"

        self.screenshot.attach(
          io: File.open(asset_path),
          filename: "#{self.name}.png"
        )

        self.update_attributes!(new_attrs)
      end
    rescue Timeout::Error
      # the process hung
      Process.kill "TERM", pid
      # FIXME: handle this properly
      # rescue PeachMelpa::Errors::EmacsError
    rescue => e
      # something else happened
    end
  end
end
