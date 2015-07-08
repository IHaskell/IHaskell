# IPython widget messaging specification

> Largely based on: https://github.com/ipython/ipython/wiki/IPEP-23:-Backbone.js-Widgets

> The messaging specification as detailed is riddled with the assumptions IHaskell's widget
> implementation makes. It works for us, so it should work for everyone.

## Creating widgets

Let's say the user types in some code, and the only effect of that code is the creation of a widget.
The kernel will open a comm for the widget, and store a reference to that comm inside it. Then, to
notify the frontend about the creation of a widget, an initial state update is sent on the widget's comm.

> The comm should be opened with a `target_name` of `"ipython.widget"`.

The initial state update message looks like this:

```json
{
    "method": "update",
    "state": { "<some/all widget properties>" }
}
```

Any *numeric* property initialized with the empty string is provided the default value by the frontend.

The initial state update must *at least* have the following fields:

  - `msg_throttle` (default 3): To prevent the kernel from flooding with messages, the messages from
    the widget to the kernel are throttled. If `msg_throttle` messages were sent, and all are still
    processing, the widget will not send anymore state messages.

  - `_view_name` (depends on the widget): The frontend uses a generic model to represent
    widgets. This field determines how a set of widget properties gets rendered into a
    widget. Has the form `IPython.<widgetname>`, e.g `IPython.Button`.

  - `_css` (default value = empty list): A list of 3-tuples, (selector, key, value).

  - `visible` (default = True): Whether the widget is visible or not.

  - Rest of the properties as required initially.

This state update is also used with fragments of the overall state to sync changes between the
frontend and the kernel.

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

---

*NOTE*: It's important that the messages sent on the comm are in response to an execution message
 from the front-end or another widget's comm message. This is required so the widget framework knows
 what cell triggered the message and can display the widget in the correct location.

---
