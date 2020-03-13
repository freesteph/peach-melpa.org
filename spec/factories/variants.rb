# frozen_string_literal: true

FactoryBot.define do
  factory :variant do
    name { Faker::Internet.slug(glue: '-') }
    theme

    after(:create) do |variant|
      Mode.all.each do |mode|
        create(:screenshot, variant: variant, mode: mode)
      end
    end
  end
end
