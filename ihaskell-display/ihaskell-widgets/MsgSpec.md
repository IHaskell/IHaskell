# IPython widget messaging specification version 2

The model implemented is the Model State v8, for ipywidgets 7.4., @jupyter-widgets/base 1.1., and @jupyter-widgets/controls 1.4.*.

> Largely based on: https://github.com/jupyter-widgets/ipywidgets/blob/master/packages/schema/messages.md

> The messaging specification as detailed is riddled with assumptions the IHaskell widget
> implementation makes. It works for us, so it should work for everyone.

## Creating widgets

Let's say the user types in some code, and the only effect of that code is the creation of a widget.
The kernel will open a comm for the widget, and store a reference to that comm. The comm_open message
also holds the initial state of the widget in it, which is used by the frontend to create a model for
the widget.

> The comm should be opened with a `target_name` of `"ipython.widget"`.

> The comm_open message's metadata gives the version of the widget messaging protocol, i.e., `{'version': '2.0.0'}`

Any *numeric* property initialized with the empty string is provided the default value by the
frontend. Some numbers need to be sent as actual numbers (when non-null), whereas the ones representing
lengths in CSS units need to be sent as strings specifying the size unit (px,em,cm,etc.).

The initial state must *at least* have the following fields in the `data.state` value of the message:

  - `_model_module`
  - `_model_module_version`
  - `_model_name`
  - `_view_module`
  - `_view_module_version`
  - `_view_name`

You can see more info on the model state of widgets [here](https://github.com/jupyter-widgets/ipywidgets/blob/master/packages/schema/jupyterwidgetmodels.v8.md), or as a json definition [here](https://github.com/jupyter-widgets/ipywidgets/blob/79312fb164e058c3a2fddd9f3ef35493515ed64b/packages/schema/jupyterwidgetmodels.latest.json)

> Warning!: By default there are two widgets modules: `@jupyter-widgets/controls` and `@jupyter-widgets/base`.

This state is also used with fragments of the overall state to sync changes between the frontend and
the kernel.

### Buffer paths
To display some widgets, we need to use the `buffer_paths`. It's only an array with arrays of keys on how to get to the fields that are to considered a
byte stream. For example, in an image widget, `buffer_paths` would be the array `[ ["value"] ]`, which means that `state.value` is a buffer path. The buffers are sent in the header of the message, just before the data, so the n-th buffer corresponds to the n-th buffer path in the array.

```json
"data": {
  "state": {
    "value": ...,
    ...
  },
  "buffer_paths": ["value"]
}
```

## Displaying widgets

The creation of a widget does not display it. To display a widget, the kernel sends a display
message to the frontend on the widget's iopub, with a custom mimetype instead of text/plain. Since 5.0, all custom json metadata should be encoded as a json object, instead of as a serialized string.

The `version_major` and `version_minor` fields are the version number of the schema of this specific message
(currently in sync with the WMP version). However, only the `model_id` field is required to display the widget.

[Source](https://github.com/jupyter-widgets/ipywidgets/issues/3220)

```json
method = "display_data",
content = {
    "data": {
      "application/vnd.jupyter.widget-view+json": {
      "model_id": "u-u-i-d",
      "version_major": 2,
      "version_minor": 0,
    }
}
```

## Clear output messages
A simple message that indicates that the output of the header message id's should be cleaned.

- `wait=true` indicates that it should clean the output in the next append, while `wait=false` cleans the output inmediately.

```json
method = "clear_output",
content = {
  "wait": bool
}
```

## Custom messages

* Widgets can also send a custom message, having the form:

```json
{
    "method": "custom",
    "content": { "<message content>" }
}
```

This message is used by widgets for ad-hoc syncronization, event handling and other stuff. An example
is mentioned in the next section.

## Handling changes to widget in the frontend

Changes to widgets in the frontend lead to messages being sent to the backend. These messages have
two possible formats:

1. Backbone.js initiated sync:

  ```json
  {
      "method": "backbone",
      "sync_data": { "<changes to sync with the backend>" }
  }
  ```

  These messages are sent by the Backbone.js library when some change is made to a widget. For
  example, whenever a change is made to the text inside a `TextWidget`, the complete contents are sent
  to the kernel so that the kernel stays up-to-date about the widget's contents.

2. Custom message:

  ```json
  {
      "method": "custom",
      "content": { "<custom message data>" }
  }
  ```

  This form is generally used to notify the kernel about events. For example, the `TextWidget` sends a
  custom message when the text is submitted by hitting the 'Enter' key.

## The issue with console input

Whenever the kernel needs to fetch input from the stdin, an `input_request` message is sent to the
frontend. The format for this message requires that this message be sent in response to an
`execute_request`, which is sent by the frontend whenever a cell is executed.

If this were not so, the frontend would not be able to determine under which cell to place the text
input widget, when an `input_request` is received.

Now, widgets cannot send `execute_request` messages. They can only send `comm_data` messages, which
means that it's not possible to fetch input inside widget event handlers.

---

*NOTE*: It's important that the messages sent on the comm are in response to an execution message
 from the front-end or another widget's comm message. This is required so the widget framework knows
 what cell triggered the message and can display the widget in the correct location.

---
