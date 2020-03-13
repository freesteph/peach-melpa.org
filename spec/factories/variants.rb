# frozen_string_literal: true

FactoryBot.define do
  factory :variant do
    name { Faker::Internet.slug(glue: '-') }
    theme

    after(:create) do |variant|
      create_list(:screenshot, 3, variant: variant)
    end
  end
end
