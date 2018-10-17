Transforms
Define transforms. We get IO, a carried state, an initialise and finalise.
---

data &Transform& = forall a . &Transform&
  { _init :: IO *a*
  , _trns :: *a* -> String -> [Id] -> SrcLoc
          -> IO (*a*, Initializer)
  , _fin  :: *a* -> IO ()
  }

- we can do IO
- we can pass around some state
