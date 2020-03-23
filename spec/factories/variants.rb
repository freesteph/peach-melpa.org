# frozen_string_literal: true

FactoryBot.define do
  factory :variant do
    theme
    name { Faker::Internet.slug(glue: '-') }

    trait :full do
      after(:create) do |variant|
        Mode.all.each do |mode|
          create(:screenshot, :full, variant: variant, mode: mode)
        end
      end
    end
  end
end
