    let defs = ./defaults.dhall 

in    defs
    ⫽ ./default-extensions.dhall 
    ⫽ { name =
          "flight-zone"
      , synopsis =
          "Control zones to fly."
      , description =
          "Control zones for hang gliding and paragliding competitons."
      , category =
          "Flight"
      , github =
          "blockscope/flare-timing/zone"
      , ghc-options =
          [ "-Wall" ]
      , dependencies = defs.dependencies
      , library =
          { source-dirs =
              "library"
          , exposed-modules =
              [ "Flight.Zone" ]
          }
      }
