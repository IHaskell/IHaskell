# IHaskell-Widgets

This package implements the [ipython widgets](https://github.com/ipython/ipywidgets) in
IHaskell. The frontend (javascript) is provided by the jupyter/ipython notebook environment, whereas
the backend is implemented in haskell.

To know more about the widget messaging protocol, see [MsgSpec.md](MsgSpec.md).

## Contributing examples
If you want to contribute with more Notebook examples, please do so on the `Examples/`
folder. Before commiting, please make sure they can be executed sequentialy and
then remove the output from the Nootebooks with:

```bash
jupyter nbconvert *.ipynb --to notebook --inplace --clear-output
```

## Things to do
- [ ] Validate the JSON implementation of widgets against the MsgSpec schema
- [ ] Create integration tests for the widgets
- [ ] Make the `output` widget work
- [ ] Processing of widget messages concurrently
- [ ] Make the layout widget values more 'Haskelian': Instead of checking if the string is valid at runtime, make some types so it's checked at compile-time
- [ ] Create a serializable color data type instead of using `Maybe String`
- [ ] Overload setField so it can be used with `Maybes` without having to put `Just` every time
- [ ] Add some "utils" work:
    - [ ] Create media widget from file
    - [ ] Get the selected label from a selection value