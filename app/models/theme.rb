require_relative '../../lib/errors'
require_relative '../../lib/logging'

class Theme < ApplicationRecord
  has_many_attached :screenshots
  has_many :variants, dependent: :destroy

  CMD = "emacsclient -s peach -n -c -eval '(fetch-and-load-theme \"%s\")'"

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
      Timeout::timeout(30) do
        cmd = CMD % [self.name]
        PeachMelpa::Log.info(self.name) { "going to launch #{cmd}" }
        pid = Kernel.spawn cmd
        Process.wait pid

        if not $?.success?
          raise PeachMelpa::Errors::EmacsError
        end

        PeachMelpa::Log.info(self.name) { "success! picking up screenshots..." }
        self.screenshots.purge

        Dir.chdir PeachMelpa::Parsing::SCREENSHOT_FOLDER do
          files = Dir.glob("#{self.radical}*")

          PeachMelpa::Log.info(self.name) { "deleting all variants" }
          self.variants.destroy_all

          variant_names = self.devise_variants(files)
          PeachMelpa::Log.info(self.name) { "found variants: #{variant_names}"}

          variant_names.each do |name|
            PeachMelpa::Log.info(self.name) { "capturing: #{name}"}
            variant = self.variants.find_or_create_by(name: name)
            variant.parse!
          end

          PeachMelpa::Log.info(self.name) { "updating attributes..." }
          self.update_attributes!(new_attrs)

          PeachMelpa::Log.info(self.name) { "cleaning up screenshots..." }
          File.delete(*files)

          PeachMelpa::Log.info(self.name) { "done." }
        end
      end
    rescue Timeout::Error
     # the process hung
      PeachMelpa::Log.info(self.name) {
        "the Emacs process is taking too much time, killing it now."
      }
      Process.kill "TERM", pid
    rescue PeachMelpa::Errors::EmacsError => e
      PeachMelpa::Log.info(self.name) {
        "the Emacs process exited with an error:going to launch #{e.inspect}"
      }
      # something bad happened in Emacs
    end
  end

  def devise_variants screenshots
    endings = Regexp.new("_(#{PeachMelpa::EXTENSIONS.values.join('|')}).png$")

    screenshots.map { |s| s.gsub(endings, '') }.uniq
  end

  def radical
    self.name.partition("-theme").first
  end

  private
end
