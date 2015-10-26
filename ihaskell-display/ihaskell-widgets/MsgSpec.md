# IPython widget messaging specification

> Largely based on: https://github.com/ipython/ipython/wiki/IPEP-23:-Backbone.js-Widgets

> The messaging specification as detailed is riddled with assumptions the IHaskell widget
> implementation makes. It works for us, so it should work for everyone.

## Creating widgets

Let's say the user types in some code, and the only effect of that code is the creation of a widget.
The kernel will open a comm for the widget, and store a reference to that comm. The comm_open message
also holds the initial state of the widget in it, which is used by the frontend to create a model for
the widget.

> The comm should be opened with a `target_name` of `"ipython.widget"`.

Any *numeric* property initialized with the empty string is provided the default value by the
frontend. Some numbers need to be sent as actual numbers (when non-null), whereas the ones representing
lengths in CSS units need to be sent as strings.

The initial state must *at least* have the following fields:

  - `msg_throttle` (default 3): To prevent the kernel from flooding with messages, the messages from
    the widget to the kernel are throttled. If `msg_throttle` messages were sent, and all are still
    processing, the widget will not send anymore state messages.

  - `_view_name` (depends on the widget): The frontend uses a generic model to represent
    widgets. This field determines how a set of widget properties gets rendered into a
    widget. Has the form `IPython.<widgetname>`, e.g `IPython.Button`.

  - `_css` (default value = empty list): A list of 3-tuples, (selector, key, value).

  - `visible` (default = True): Whether the widget is visible or not.

  - Rest of the properties as required initially.

This state is also used with fragments of the overall state to sync changes between the frontend and
the kernel.

## Displaying widgets

The creation of a widget does not display it. To display a widget, the kernel sends a display
message to the frontend on the widget's comm.

```json
{
    "method": "display"
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
