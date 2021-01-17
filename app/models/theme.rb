# frozen_string_literal: true

require 'English'
require_relative '../../lib/errors'
require_relative '../../lib/logging'

class Theme < ApplicationRecord
  has_many :variants, dependent: :destroy

  validates :name, presence: true, uniqueness: true # rubocop:disable Rails/UniqueValidationWithoutIndex

  scope :perfect, -> { joins(:variants).distinct }

  CMD = "emacsclient -s peach -n -c -eval '(fetch-and-load-theme \"%s\")'"

  def to_param
    name
  end

  def older_than?(version)
    if self.version
      d1 = make_datetime(self.version)
      d2 = make_datetime(version)

      d1 < d2
    else
      true
    end
  end

  def update_screenshots!(new_attrs)
    pid = nil

    begin
      Timeout.timeout(90) do
        cmd = format(CMD, name)
        PeachMelpa::Log.info(name) { "going to launch #{cmd}" }
        pid = Kernel.spawn cmd
        Process.wait pid

        # rubocop:disable Style/NegatedIf, Style/Not
        raise PeachMelpa::Errors::EmacsError if not $CHILD_STATUS.success?

        # rubocop:enable Style/NegatedIf, Style/Not

        PeachMelpa::Log.info(name) { 'success! picking up screenshots...' }
      end

      capture_artifacts! new_attrs
    rescue Timeout::Error
      # the process hung
      PeachMelpa::Log.info(name) do
        'the Emacs process is taking too much time, killing it now.'
      end
      Process.kill 'TERM', pid
    rescue PeachMelpa::Errors::NoThemeScreenshotsFolder => e
      PeachMelpa::Log.info(name) do
        "#{e.message}: moving on..."
      end
    rescue PeachMelpa::Errors::EmacsError => e
      PeachMelpa::Log.info(name) do
        "the Emacs process exited with an unknown error: #{e.inspect}"
      end
      # something bad happened in Emacs
    end
  end

  def devise_variants(screenshots)
    endings = Regexp.new("_(#{PeachMelpa::EXTENSIONS.values.join('|')}).png$")

    screenshots.map { |s| s.gsub(endings, '') }.uniq
  end

  def radical
    extras = %w[color theme themes]

    name
      .split('-')
      .reject { |p| extras.include?(p) }
      .join('-')
  end

  def capture_artifacts!(new_attrs)
    Dir.chdir PeachMelpa::Parsing::SCREENSHOT_FOLDER do
      raise PeachMelpa::Errors::NoThemeScreenshotsFolder unless Dir.exist? name

      Dir.chdir(name) do
        files = Dir.glob('*')

        PeachMelpa::Log.info(name) { 'deleting all variants' }
        variants.destroy_all

        variant_names = devise_variants(files)
        PeachMelpa::Log.info(name) { "found variants: #{variant_names}" }

        variant_names.each do |name|
          PeachMelpa::Log.info(self.name) { "capturing: #{name}" }
          variant = variants.find_or_create_by(name: name)
          variant.parse!
        end

        PeachMelpa::Log.info(self.name) { 'updating attributes...' }
        update!(new_attrs)

        PeachMelpa::Log.info(self.name) { 'cleaning up screenshots...' }
        File.delete(*files)

        PeachMelpa::Log.info(self.name) { 'done.' }
      end

      Dir.rmdir(self.name)
    end
  end

  def multi?
    variants.count > 1
  end

  def thumbnail
    @lisp = Mode.find_by(name: 'Lisp')
    variants.first.screenshots.find_by(mode: @lisp)
  end

  def preview
    factor = 2.49
    dimensions = "#{1600 / factor}x#{1200 / factor}"

    # FIXME: figure out if this operation is cached or not
    thumbnail.image.variant(combine_options: {
                              gravity: 'SouthWest',
                              crop: "#{dimensions}+0+0",
                              resize: '300x300^'
                            })
  end

  def make_datetime(version)
    d, t = version.split('.')
    t.prepend '00' if t.length <= 2 # midnight minutes

    date = Date.parse d
    time = t.insert(-3, ':')

    DateTime.parse("#{date} #{time}")
  end

  def version_to_datetime
    make_datetime(version)
  end
end
