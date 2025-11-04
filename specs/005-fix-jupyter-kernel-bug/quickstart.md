# Quickstart: Testing the Jupyter Kernel Fix

This guide provides the steps to test the fix for the Jupyter kernel integration.

## Prerequisites

1.  **Haskell Environment**: Ensure you have GHC and Cabal installed.
2.  **Jupyter**: A working Jupyter environment (Notebook or Lab) must be installed.
3.  **Dependencies**: Install the project dependencies by running `cabal update` and `cabal build`.

## Steps

1.  **Install the Kernel**:
    -   From the root of the repository, run the installation script:
        ```bash
        ./scripts/install-kernelspec.sh
        ```
    -   This script will create a `kernelspec` for the Haskell kernel that Jupyter can discover.

2.  **Verify the Installation**:
    -   Check that Jupyter recognizes the new kernel:
        ```bash
        jupyter kernelspec list
        ```
    -   You should see `hs-jupyter` in the list of available kernels.

3.  **Launch Jupyter Notebook**:
    -   Start Jupyter Notebook from your terminal:
        ```bash
        jupyter notebook
        ```

4.  **Create a New Haskell Notebook**:
    -   In the Jupyter interface, click the "New" button and select "Haskell" from the dropdown menu.
    -   A new notebook should open, and the kernel status indicator should show "Kernel Ready."

5.  **Execute Code**:
    -   In the first cell, type a simple Haskell expression:
        ```haskell
        let message = "Hello, Jupyter!"
        putStrLn message
        ```
    -   Run the cell by pressing `Shift + Enter`.
    -   The output `Hello, Jupyter!` should be displayed below the cell.

6.  **Test Interrupt and Restart**:
    -   In a new cell, create an infinite loop:
        ```haskell
        let loop = loop
        loop
        ```
    -   Run the cell. The kernel should become busy.
    -   From the "Kernel" menu, select "Interrupt." The execution should stop.
    -   From the "Kernel" menu, select "Restart." The kernel should restart and be ready for new commands.

## Expected Outcome

If all steps are completed successfully, the bug is fixed. You should be able to use the Haskell kernel in Jupyter just like any other kernel.
