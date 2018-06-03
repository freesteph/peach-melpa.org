module ApplicationHelper
  def feature_enabled? name
    peach_features.include? name
  end

  private

  def peach_features
    ENV
      .select { |key| key.starts_with? "PEACH_" }
      .keys
      .map { |key| key.split("PEACH_").last.downcase }
  end
end
