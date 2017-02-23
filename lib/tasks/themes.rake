require 'json'

namespace :themes do
  MELPA_ARCHIVE = 'https://melpa.org/archive.json'
  ARCHIVE_PATH = "#{Rails.root}/tmp/archive.json"

  desc "grabs MELPA archives.json and put it in tmp"
  task refresh: :environment do
  end

  desc "grabs tmp JSON file and store themes"
  task parse: :environment do
    data = JSON.parse(IO.read(ARCHIVE_PATH))

    themes = data.select { |name,| name.end_with? '-theme' }

    themes.first(5).each do |theme|
      name = theme.first.sub('-theme', '')
      info = theme.last

      puts "parsing #{name}"
      Theme.find_or_create_by(name: name)
    end
  end

  desc "update screenshot for THEME_NAME"
  task :screenshot, [:name] => :environment do |t, args|
    theme = args[:name]

    cmd = %x[emacs -Q -l lib/take-screenshot.el -eval '(fetch-and-load-theme "zerodark")']
    puts "done!"
  end
end
