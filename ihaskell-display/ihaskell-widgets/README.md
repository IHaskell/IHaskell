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