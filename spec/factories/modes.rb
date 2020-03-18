# frozen_string_literal: true

FactoryBot.define do
  factory :mode do
    name { Faker::ProgrammingLanguage.name }
    extension { 'some random extension' }
  end
end
