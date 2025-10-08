# FLRToolkit
### - Forensic Likelihood Ratio Evaluation Toolkit

📦 BETA Version\
📅 October 8, 2025\
👤 Deng, Guangmou\
📧 [guangmou01\@outlook.com](mailto:guangmou01@outlook.com)

------------------------------------------------------------------------

#### 🔎 Overview

**FLRToolkit** is a Shiny-based application suite that provides essential tools for **Forensic Likelihood-Ratio (LR)-based** and **Biometric** systems. It focuses on **source-level interpretation**, offering functionalities for end-to-end workflow for forensic LR evaluation, including:

-   **LR Computation** - computation of LR scores by different methods.

-   **Calibration** – fusion and calibration of LR scores from multiple systems.

    1.  Linear Logistic-Regression Calibrator
    2.  Bi-Gaussianized Calibrator

-   **Validation** – performance evaluation of LR-based systems.

    1.  Tippett Plot Generator
    2.  Empirical Cross-Entropy Plot Generator

#### 📚 References

**Articles**

-   Aitken, C. G. G., & Lucy, D. (2004). Evaluation of trace evidence in the form of multivariate data. *Journal of the Royal Statistical Society: Series C (Applied Statistics)*, 53(1), 109–122. <https://doi.org/10.1046/j.0035-9254.2003.05271.x>

-   Morrison, G. S. (2013). Tutorial on logistic-regression calibration and fusion: Converting a score to a likelihood ratio. *Australian Journal of Forensic Sciences*, 45(2), 173–197. <https://doi.org/10.1080/00450618.2012.733025>

-   Morrison, G. S., & Poh, N. (2018). Avoiding overstating the strength of forensic evidence: Shrunk likelihood ratios/Bayes factors. *Science & Justice*, 58(3), 200–218. <https://doi.org/10.1016/j.scijus.2017.12.005>

-   Morrison, G. S., et al. (2021). Consensus on validation of forensic voice comparison. *Science & Justice*, 61(3), 299–309. <https://doi.org/10.1016/j.scijus.2021.02.002>

-   Morrison, G. S. (2024). Bi-Gaussianized calibration of likelihood ratios. *Law, Probability and Risk*, 23(1), 1–34. <https://doi.org/10.1093/lpr/mgae004>

**Scripts**

-   Brümmer, N. (2005). *FoCal Toolbox* [MATLAB script]. <http://www.dsp.sun.ac.za/nbrummer/focal>

-   Morrison, G. S. (2007). *Matlab implementation of Aitken & Lucy’s (2004) forensic likelihood-ratio software using multivariate-kernel-density estimation* [MATLAB script]. <http://geoff-morrison.net/#MVKD>

-   Morrison, G. S. (2009). *Robust version of train_llr_fusion.m from Niko Brümmer’s FoCal Toolbox* [MATLAB script]. <https://geoff-morrison.net/#TrainFus>

-   Morrison, G. S. (2017). *Regularized version of train_llr_fusion.m from Niko Brümmer’s FoCal Toolbox* [MATLAB script]. <https://geoff-morrison.net>
