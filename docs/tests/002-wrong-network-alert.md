# Wrong Network Alert

## Intent

This alert should appear whenever the monitor is asked to monitor a node that is not running on the same network (chain) as the monitor. In this situation, either the monitor or the node needs to be reconfigured such that they both run on the same network.

## Tests

  1. Run the monitor. Configure it to monitor a specific network.
  1. Configure the monitor to send alerts via email or some other medium. Test this to verify it works.
  1. Run a node on a *different* network. Make sure the node has an RPC address configured.
  1. Add this node to be monitored.
  1. Verify that the monitor immediately shows an alert that the node is on the wrong network.
  1. Verify that a notification was sent to the proper medium.
  1. Kill the node.
  1. Start a node on the *same* network as the monitor. Be sure to use the same RPC address as before.
  1. Verify that the alert becomes resolved and the node is monitored as normal.
