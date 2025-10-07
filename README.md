# FLRToolkit: Forensic Likelihood Ratio Evaluation Toolkit

**Updated:** October 7, 2025\
**Author:** Guangmou Deng\
**Contact:** [guangmou01\@outlook.com](mailto:guangmou01@outlook.com)

------------------------------------------------------------------------

## 🔎 Overview

**FLRToolkit** is a Shiny-based application suite that provides essential tools for **Forensic Likelihood-Ratio (LR)-based** and **Biometric** systems. It focuses on **source-level interpretation**, offering functionalities for **LR computation, calibration, and validation**.

The toolkit provides an end-to-end workflow for forensic LR evaluation, including:

-   **LR Computation** - computation of LR scores by different methods.

-   **Calibration** – fusion and calibration of LR scores from multiple systems.

    1.  LogRegCalibrator.R
    2.  Bi-GaussCalibrator.R

-   **Validation** – performance visualization of a single LR system.

    1.  ECEPlotter.R
    2.  TippettPlotter.R
