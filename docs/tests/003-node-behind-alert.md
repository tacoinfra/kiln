# Node is Behind Alert

## Intent

This alert should appear whenever a monitored node's head is falling behind the latest head that the monitor has seen. This means that the alert can only happen when monitoring *more than 1 node* (public nodes count). The monitor considers the "latest head" to be the block with the highest fitness from any node being monitored.

## Tests

**You need to run the following steps multiple times.** Step 5 asks you to add a public node. You will need to repeat this test using *each* public node by itself.

  1. Run the monitor with a fresh database. Configure it to monitor a specific network.
  2. Configure the monitor to send alerts via email or some other medium. Test this to verify it works.
  3. Add one local node to monitor. It should be on the same network as the monitor and it should not be behind the latest head.
  4. Ensure there are no alerts for the node.
  5. Add *only* the public node for this iteration of the test. (See **bold** introduction above.)
  6. Ensure that both nodes show the same head block level (or are within 1 level of each other).
  7. Now kill the local node.
  8. (You will get the "Inaccessible node" alert. But this is not relevant here.)
  9. Wait for a few minutes until the head level of the other node has increased by at least 3 levels.
  10. Turn your node back on.
  11. (The "Inaccessible node" alert should become resolved.)
  12. Note that you should see a new alert that your node is behind.
  13. Wait for it to catch up.
  14. Note that the behind alert becomes resolved.
