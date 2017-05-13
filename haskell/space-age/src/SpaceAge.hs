module SpaceAge (Planet(..), ageOn) where

data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune

planetYearsPerEarthYear Mercury = 0.2408467
planetYearsPerEarthYear Venus   = 0.61519726
planetYearsPerEarthYear Earth   = 1.0
planetYearsPerEarthYear Mars    = 1.8808158
planetYearsPerEarthYear Jupiter = 11.862615
planetYearsPerEarthYear Saturn  = 29.447498
planetYearsPerEarthYear Uranus  = 84.016846
planetYearsPerEarthYear Neptune = 164.79132
planetYearsPerEarthYear other   = error "Input is not a planet."

earthSecondsPerYear = 31557600

ageOn :: Planet -> Float -> Float
ageOn planet seconds = seconds / earthSecondsPerYear /
                       planetYearsPerEarthYear planet
