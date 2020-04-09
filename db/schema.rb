# This file is auto-generated from the current state of the database. Instead
# of editing this file, please use the migrations feature of Active Record to
# incrementally modify your database, and then regenerate this schema definition.
#
# This file is the source Rails uses to define your schema when running `rails
# db:schema:load`. When creating a new database, `rails db:schema:load` tends to
# be faster and is potentially less error prone than running all of your
# migrations from scratch. Old migrations may fail to apply correctly if those
# migrations use external dependencies or application code.
#
# It's strongly recommended that you check this file into your version control system.

ActiveRecord::Schema.define(version: 2020_04_09_134712) do

  create_table "active_storage_attachments", force: :cascade do |t|
    t.string "name", null: false
    t.string "record_type", null: false
    t.integer "record_id", null: false
    t.integer "blob_id", null: false
    t.datetime "created_at", null: false
    t.index ["blob_id"], name: "index_active_storage_attachments_on_blob_id"
    t.index ["record_type", "record_id", "name", "blob_id"], name: "index_active_storage_attachments_uniqueness", unique: true
  end

  create_table "active_storage_blobs", force: :cascade do |t|
    t.string "key", null: false
    t.string "filename", null: false
    t.string "content_type"
    t.text "metadata"
    t.bigint "byte_size", null: false
    t.string "checksum", null: false
    t.datetime "created_at", null: false
    t.index ["key"], name: "index_active_storage_blobs_on_key", unique: true
  end

  create_table "modes", force: :cascade do |t|
    t.string "name", null: false
    t.string "extension", null: false
    t.index ["extension"], name: "index_modes_on_extension", unique: true
  end

  create_table "screenshots", force: :cascade do |t|
    t.integer "mode_id"
    t.integer "variant_id"
    t.index ["mode_id"], name: "index_screenshots_on_mode_id"
    t.index ["variant_id"], name: "index_screenshots_on_variant_id"
  end

  create_table "themes", force: :cascade do |t|
    t.string "name"
    t.string "version"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.string "description"
    t.string "url"
    t.float "brightness"
    t.string "authors"
    t.string "kind"
  end

  create_table "variants", force: :cascade do |t|
    t.integer "theme_id"
    t.string "name"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["theme_id"], name: "index_variants_on_theme_id"
  end

  add_foreign_key "screenshots", "modes"
  add_foreign_key "screenshots", "variants"
  add_foreign_key "variants", "themes"
end
