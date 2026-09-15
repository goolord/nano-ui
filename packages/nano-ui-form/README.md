# nano-ui-form

Forms for [nano-ui](https://github.com/goolord/nano-ui), built on
[ditto](https://hackage.haskell.org/package/ditto).

A form is an applicative value. Each input draws a nano-ui widget and parses
its value; validators attach errors that appear under the field.

```haskell
data Signup = Signup Text Text

signup :: Form Text Signup
signup =
  Signup
    <$> withFieldErrors (inputText "Name" "" `prove` notEmpty "Name is required")
    <*> withFieldErrors (inputText "Email" "" `prove` validEmail (const "Not an email address"))
```

`runNanoForm` runs a form inside a view and returns its view along with the
parsed result. `nanoFormSubmit` adds a submit button and yields the value on
the frame it is pressed.
