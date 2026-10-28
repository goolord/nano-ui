# nano-ui-form: Validated Forms

`nano-ui-form` provides applicative form composition and declarative validation based on `ditto`.

## Composing Validated Forms

Forms combine field inputs, error messages, and validation rules applicatively:

```haskell
data UserRegistration = UserRegistration
  { username :: Text
  , age      :: Int
  , email    :: Text
  }

registrationForm :: Form NanoUI UserRegistration
registrationForm = UserRegistration
  <$> field "Username" (nonEmpty "Username required" textInput)
  <*> field "Age" (check (> 13) "Must be at least 13" (numericInput 0 120))
  <*> field "Email" (check containsAt "Invalid email" textInput)
  where
    containsAt t = "@" `T.isInfixOf` t
```

When submitted, validation errors are rendered inline beside invalid fields. Successful submissions produce the parsed, strongly typed `UserRegistration` record.
