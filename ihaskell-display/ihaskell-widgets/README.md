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
- [ ] Automatic validation of the JSON implementation of widgets against the MsgSpec schema
- [ ] Create integration tests for the widgets
- [ ] Make the output widget capture output (problem: you have to get the message id of where the output is displayed)
- [ ] Make the layout widget values more 'Haskelian': Instead of checking if the string is valid at runtime, make some types so it's checked at compile-time
- [ ] Create a serializable color data type instead of using `Maybe String`
- [ ] Overload setField so it can be used with `Maybes` or other wrapper types without having to put `Just` every time.
- [ ] Add some "utils" work:
    - [ ] Create media widget from file
    - [ ] Get the selected label from a selection value

## How to...
This is a mini-guide for developers that want to update to a more recent widgets specification, but without
dwelling into the deeps of the project

### Add a new attribute
If you want to add a new attribute you'll have to:
1. Create a new singleton in [Singletons.hs](./Singletons.hs) inside the type `data Field`.
2. Write the serialization key of the field as specified in the model (see [MsgSpec.md](./MsgSpec.md)) inside the `toKey` function at [Singletons.hs](./Singletons.hs)
3. Because we use the `singletons-th` library, you have to define an alias for the attribute at [Common.hs](./Common.hs) to be able to use it at run-time more easily.
4. Now you have to specify the type of the field. Edit the type family `Fieldtype` at [Types.hs](./Types.hs)

### Add an attribute to a widget
First you have to check if the attribute is only for one widget, or is from a common class. You can check it at [ipywidget's repo](https://github.com/jupyter-widgets/ipywidgets/tree/master/ipywidgets/widgets).

- If it's only for one widget:
    1. Edit the `type instance WidgetFields <WidgetNameType> = ...` at [Types.hs](./Types.hs), adding the new field to the field array.
    2. Modify the `mk<WidgetName>` at `Module/WidgetName.hs`, adding the default value of the attribute. If the widget doesn't have any attributes yet, you can check how to do it on other widgets.
- If it's for a common class:
    1. Edit the `type <ClassName> = ...` at [Types.hs](./Types.hs)
    2. Edit the `default<ClassName>Widget` function from the same file, adding the default value for that attribute.

> Some widgets receive messages from the frontend when a value is modified (such as sliders, text areas, buttons...). You'll have to modify the `comm` function instantiated from the class `IHaskellWidget`. You can find an example at [IntSlider.hs](./Int/BoundedInt/IntSlider.hs)

## FAQ
When using widgets in ihaskell, you'll encounter a lot of compilation errors. If you are not very familiar with Haskell, they can be a bit hard to decipher, this is a mini guide that will (hopefully) appear when you paste the error in Google.

### setField: No instance for...
You probably got this error when trying to use setField like this:

```
<interactive>:1:1: error:
    • No instance for (Data.Vinyl.Lens.RecElem
                         Data.Vinyl.Core.Rec
                         'ihaskell-widgets-0.3.0.0:IHaskell.Display.Widgets.Singletons.Index
                         'ihaskell-widgets-0.3.0.0:IHaskell.Display.Widgets.Singletons.Index
                         '[]
                         '[]
                         (Data.Vinyl.TypeLevel.RIndex 'ihaskell-widgets-0.3.0.0:IHaskell.Display.Widgets.Singletons.Index '[]))
        arising from a use of ‘setField’
    • In the expression: setField select Index 0
      In an equation for ‘it’: it = setField select Index 0
```

What this error means is that there is no field called `Index` for this particular widget. You can display on screen all
the fields available for a widget using `properties widget`.

### setField: Couldn't match expected type SField f with actual type

If you get an error like this, you probably forgot to put the field name in the second argument of `setField`.
```
<interactive>:1:25: error:
    • Couldn't match expected type ‘ihaskell-widgets-0.3.0.0:IHaskell.Display.Widgets.Singletons.SField f’ with actual type ‘[a0]’
    • In the second argument of ‘setField’, namely ‘["Apples", "Oranges", "Pears"]’
      In the expression: setField selectMultiple ["Apples", "Oranges", "Pears"]
      In an equation for ‘it’: it = setField selectMultiple ["Apples", "Oranges", "Pears"]
    • Relevant bindings include it :: ihaskell-widgets-0.3.0.0:IHaskell.Display.Widgets.Types.FieldType f -> IO () (bound at <interactive>:1:1)
```