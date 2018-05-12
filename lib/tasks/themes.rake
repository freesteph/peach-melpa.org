require 'open-uri'
require 'json'

namespace :themes do
  MELPA_ARCHIVE = 'https://melpa.org/archive.json'
  ARCHIVE_PATH = "#{Rails.root}/tmp/archive.json"

  desc "grabs MELPA archives.json and put it in tmp"
  task refresh: :environment do
    puts "grabbing archive file from MELPA"
    open(MELPA_ARCHIVE) do |fresh|
      puts "writing MELPA file down."
      File.open(ARCHIVE_PATH, "w+") do |tmp|
        tmp.write(fresh.read)
      end
    end
  end

  desc "grabs tmp JSON file and store themes"
  task parse: :environment do
    data = JSON.parse(IO.read(ARCHIVE_PATH))

    themes = data.select { |name,| name.end_with? '-theme' }

    themes.first(50).each do |theme|
      name = theme.first.sub('-theme', '')
      info = theme.last
      version = info['ver'].join('.')

      puts "parsing #{name}"
      t = Theme.find_or_create_by(name: name)

      if t.version == version
        puts "not updating as version (#{t.version}) is similar"
      else
        t.version = version
        t.save

        puts "creating screenshot for #{name}"
        Rake::Task["themes:screenshot"].reenable
        Rake::Task["themes:screenshot"].invoke t.name
        puts "done with the screenshot"
      end
    end
  end

  desc "update screenshot for THEME_NAME"
  task :screenshot, [:name] => :environment do |t, args|
    theme = Theme.find_by(name: args[:name])

    Kernel.raise "could not find #{args[:name]}" if theme.nil?

    cmd = %x[DISPLAY=:13 emacs -Q -l lib/take-screenshot.el -eval '(fetch-and-load-theme "#{theme[:name]}" "#{theme[:version]}")']
    puts "done!"
  end
end
