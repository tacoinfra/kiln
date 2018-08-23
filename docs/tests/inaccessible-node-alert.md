# Inaccessible Node Alert

## Intent

This alert should appear whenever the monitor cannot connect to a node that it is trying to monitor. This may happen for multiple reasons:

  1. The node isn't running at all.
  1. The node is running but without an RPC address configured.
  1. The node is running but the network situation prevents the monitor from connecting to it.
  1. The node RPC API is different from what the monitor expects (the URLs could be different, or the messages could be in a different format).

Some of these are harder to test than others, but they will all lead to the same alert.

## Tests

  1. Run the monitor. Note that the monitor is configured to track a specific chain.
  1. Configure the monitor to send alerts via email or some other medium. Test this to verify it works.
  1. Start a node with an RPC address configured. It should be running on the same chain as the chain being tracked by the monitor.
  1. Add this node to be monitored.
  1. Verify that the node is showing up in the UI with new blocks updating regularly.
  1. Kill the node.
  1. Verify that the monitor UI immediately shows an error alert for the correct node.
  1. Start the node again.
  1. Verify that the alert becomes resolved and the node begins updating in the UI again.

