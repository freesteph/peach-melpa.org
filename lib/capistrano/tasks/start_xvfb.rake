# frozen_string_literal: true

XVFB_ARGS = ':13 -nocursor -screen 0 "1600x1200x24+32" &'

namespace :deploy do
  desc 'starts an Xvfb server if necessary'
  task :start_xvfb do # rubocop:disable Rails/RakeEnvironment
    on roles(:app) do
      within current_path do
        running = capture :pidof, 'Xvfb'

        execute(:Xvfb, XVFB_ARGS) unless running
      end
    end
  end

  after :finishing, 'deploy:start_xvfb'
end
