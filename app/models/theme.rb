require_relative '../../lib/errors'
require_relative '../../lib/logging'

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
        cmd = CMD % [self.name, new_attrs[:version]]
        PeachMelpa::Log.info(self.name) { "going to launch #{cmd}" }
        pid = Kernel.spawn cmd
        Process.wait pid

        if not $?.success?
          raise PeachMelpa::Errors::EmacsError
        end

        PeachMelpa::Log.info(self.name) { "success! picking up screenshots..." }

        Dir.chdir PeachMelpa::Parsing::SCREENSHOT_FOLDER
        Dir.glob("#{self.name}_*").each do |entry|
          asset_path = PeachMelpa::Parsing::SCREENSHOT_FOLDER + entry

          self.screenshots.attach(
            io: File.open(asset_path),
            filename: entry
          )
        end

        PeachMelpa::Log.info(self.name) { "updating attributes..." }
        self.update_attributes!(new_attrs)

        PeachMelpa::Log.info(self.name) { "done..." }
      end
    rescue Timeout::Error
     # the process hung
      PeachMelpa::Log.info(self.name) {
        "the Emacs process is taking too much time, killing it now."
      }
      Process.kill "TERM", pid
    rescue PeachMelpa::Errors::EmacsError => e
      PeachMelpa::Log.info(self.name) {
        "the Emacs process exited with an error:going to launch #{e}"
      }
      # something bad happened in Emacs
    end
  end
end
