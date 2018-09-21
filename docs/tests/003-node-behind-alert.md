# Node is Behind Alert

## Intent

This alert should appear whenever a monitored node's head is falling behind the latest head that the monitor has seen. This means that the alert can only happen when monitoring *more than 1 node* (public nodes count). The monitor considers the "latest head" to be the block with the highest fitness from any node being monitored.

## Tests

  1. Run the monitor with a fresh database. Configure it to monitor a specific network.
  1. Configure the monitor to send alerts via email or some other medium. Test this to verify it works.
  1. Add two nodes running on the same network. One of them can be a public node. One of them must be a node you control. Ideally it should be "caught up" to the other node.
  1. Notice that both nodes show the same head block level (or are within 1 level of each other).
  1. Now kill one of the nodes.
  1. (You will get the "Inaccessible node" alert. But this is not relevant here.)
  1. Wait for a few minutes until the head level of the other node has increased by at least 3 levels.
  1. Turn your node back on.
  1. (The "Inaccessible node" alert should become resolved.)
  1. Note that you should see a new alert that your node is behind.
  1. Wait for it to catch up.
  1. Note that the behind alert becomes resolved.
