# Purescript-Halogen-Hooks-Extra

Provides useful Halogen Hooks and other utilities commonly used in hooks (e.g. actions).

## Hooks

- `useDebouncer` - run an action only when a specified amount of time has passed without receiving any new updats/values (e.g. search the database using user's query after 300 ms of no input).
- `useThrottle` - run an action with the next value after ignoring new values for a specified amount of time (e.g. handle 1 scroll event every 300 ms).
- `useEvent` - run a callback whenever a new value is pushed (e.g. allow a library to publish events within it to end-users of the library).
- `useStateFn` - use the `state /\ modifyState <- useState` behavior that was implemented in `halogen-hook` `v0.3.0` and then reverted in `v0.4.0`.
- `useGet` - ensure you aren't getting/using "stale" values (e.g. running finalizers in `Effect`ful code or running asynchronous code).

## Actions

- Reduce boilerplate when calling `preventDefault` on mouse events and key events.

## Examples

```bash
# Compile the examples
spago -x examples.dhall bundle-app --main Examples.Main --to dist/app.js

# Then open the `dist/index.html` file in your favorite browser
# firefox dist/index.html
# google-chrome dist/index.html
```
