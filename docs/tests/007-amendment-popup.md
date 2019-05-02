# Amendment / governance info popup

## Intent

A popup (accessible by the top bar) should show the data from the current
amendment period. Data should be available for the current period (and any
preceding periods, if we are in a period after proposal).

## Tests

  1. Build the version of kiln to be tested
  2. Run kiln as such:
    ./backend --nodes=http://127.0.0.1:20000
  3. Open the monitor in the browser
  4. From another terminal window, navigate to tezos-bake-monitor and run:
    ob thunk unpack dep/tezos-baking-platform
    ob thunk unpack dep/tezos-baking-platform/tezos/master
    nix-shell -A tezos.master.sandbox dep/tezos-baking-platform --run 'flextesa voting dep/tezos-baking-platform/tezos/master/src/bin_client/test/proto_test_injection --base-port=20000 --interactive=true --pause-on-error=true'
  5. Enter `q` in the interactive shell from the previous step. The node should be updated in kiln and show the first block.
  6. Enter `q` again. The top bar should be updated with the block and amendment period data. Verify that we are in the proposal period.
  7. After some time, flextesa will run up to block 41. You should see the popup updated as the proposal period resets, ending on the last proposal period in cycle 5-6.
  8. Enter `q` again. Verify that three proposals have been added to the popup.
  9. Enter `q` again, flextesa will quickly cycle through exploration and testing periods. Ensure that the popup stays up to date throughout and you can go back to previous periods and the data is still there.
  10. Enter `q` twice, verify we are in the promotion period and the popup is populated with data.
  11. Enter `q` again, flextesa will run through the remainder of the promotion period.
  12. Enter `q` repeatedly until flextesa exits. If you need to rerun this test, you should stop kiln and delete the database.

