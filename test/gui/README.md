# GUI regression test

Requires Lazarus, LCL and LazControls, like `gulpux`. This test is separate from
the plain-FPC regression suite because it creates the real application form
and runs its event handlers and worker threads.

From the repository root:

```sh
lazbuild test/gui/gui_regression.lpi
build/bin/gui-regression
```

On a headless Linux machine, install Xvfb and run:

```sh
timeout 60s xvfb-run -a build/bin/gui-regression
```

The program creates a new temporary directory, prints its path and leaves its
fixtures there for inspection. It uses the test executable's own application
configuration file, not gulpux's settings. Exit status zero means success.

Coverage:

- synchronization followed by automatic listing;
- close veto and disabled navigation while a worker is running;
- completion restores navigation and hides progress;
- historical file previews and live search event wiring;
- deletion-only revisions, including an empty final snapshot;
- restore of a selected revision into an existing destination;
- non-deleting restore preserves unrelated destination files;
- restore completion and failure return to the original working directory;
- worker exceptions appear in the log and leave usable navigation.

Native validation and confirmation dialogs are not automated by this test.
Manually verify these additional cases with disposable files/directories:

1. Select a sync root, then remove it before confirming: an error is displayed,
   no worker starts, and no archive is created or changed.
2. Leave the restore destination empty or select a missing directory: no restore
   starts, and the configuration screen remains available.
3. Choose the deleting restore mode: confirmation defaults to No. Canceling
   leaves the destination untouched; confirming applies the selected snapshot
   and removes unrelated files. Never test this on valuable data.

Some GTK2/LCL installations emit GObject/pixbuf diagnostics while rendering
list-view icons under Xvfb. These diagnostics should be investigated separately
from the test's functional assertions; they are not suppressed by the test.
