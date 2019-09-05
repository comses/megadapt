describe('a weather config', {
  config <- climate_config_create()

  it('can be used to create a climate model', {
    climate_fnss <- climate_deserialize(config)
    expect_s3_class(climate_fnss, 'climate_fnss')
  })
})
