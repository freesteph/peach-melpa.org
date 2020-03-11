# frozen_string_literal: true

FactoryBot.define do
  factory :screenshot do
    mode { Mode.first }
    variant
  end
end
