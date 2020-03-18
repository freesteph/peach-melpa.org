# frozen_string_literal: true

FactoryBot.define do
  factory :mode do
    name { Faker::ProgrammingLanguage.name }
    extension { Faker::File.extension }
  end
end
