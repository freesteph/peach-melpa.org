# frozen_string_literal: true

FactoryBot.define do
  factory :theme do
    name { Faker::Internet.slug(glue: '-') }

    after(:create) do |theme|
      create_list(:variant, 3, theme: theme)
    end
  end
end
