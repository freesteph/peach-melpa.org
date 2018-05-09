require 'open-uri'
require 'json'

namespace :themes do
  MELPA_ARCHIVE = 'https://melpa.org/archive.json'
  ARCHIVE_PATH = "#{Rails.root}/tmp/archive.json"

  desc "grabs MELPA archives.json and put it in tmp"
  task refresh: :environment do
    open(MELPA_ARCHIVE) do |fresh|
      File.open(ARCHIVE_PATH, "r+") do |tmp|
        tmp.write(fresh.read)
      end
    end
  end

  desc "grabs tmp JSON file and store themes"
  task parse: :environment do
    data = JSON.parse(IO.read(ARCHIVE_PATH))

    themes = data.select { |name,| name.end_with? '-theme' }

    # FIXME: 5 is just for testing purposes
    themes.first(5).each do |theme|
      name = theme.first.sub('-theme', '')
      info = theme.last
      version = info['ver'].join('.')

      puts "parsing #{name}"
      Theme.find_or_create_by(name: name) do |theme|
        theme.version = version
      end
    end
  end

  desc "update screenshot for THEME_NAME"
  task :screenshot, [:name] => :environment do |t, args|
    theme = Theme.find_by(name: args[:name])

    Kernel.raise "could not find #{args[:name]}" if theme.nil?

    cmd = %x[emacs -Q -l lib/take-screenshot.el -eval '(fetch-and-load-theme "#{theme[:name]}" "#{theme[:version]}")']
    puts "done!"
  end
end
