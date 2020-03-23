# frozen_string_literal: true

FactoryBot.define do
  factory :theme do
    name { Faker::Internet.slug(glue: '-') }

    trait :with_variant do
      after :create do |t|
        create(:variant, theme: t)
      end
    end

    trait :with_full_variant do
      after :create do |t|
        create(:variant, :full, theme: t)
      end
    end
  end
end
