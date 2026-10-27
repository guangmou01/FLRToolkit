# FLRToolkit

### Forensic Likelihood Ratio Evaluation Toolkit (BETA 2026.10)

Oct 28, 2026\
Deng, Guangmou\
[guangmou01\@outlook.com](mailto:guangmou01@outlook.com)

------------------------------------------------------------------------

### Overview

FLRToolkit is a Shiny-based application suite that provides graphical user interface tools for forensic likelihood-ratio (LR)-based or biometric systems. It focuses on source-level interpretation, offering functions of end-to-end workflow for forensic LR evaluation, including:

1.  Computation of LR scores by different methods;
2.  Calibration and fusion of LR scores from multiple systems;
3.  Performance evaluation of LR-based systems.

``` text
# MVKDLR.R                MVKD-based likelihood ratio calculation
# LogRegCalibrator.R      Logistic-regression calibration
# Bi-GaussCalibrator.R    Bi-Gaussianized calibration
# TippettPlotter.R        Tippett plot
# ECEPlotter.R            Empirical cross-entropy plot
```

------------------------------------------------------------------------

### References

-   Aitken, C. G. G., & Lucy, D. (2004). Evaluation of trace evidence in the form of multivariate data. *Journal of the Royal Statistical Society: Series C (Applied Statistics)*, 53(1), 109–122. <https://doi.org/10.1046/j.0035-9254.2003.05271.x>
-   Morrison, G. S., et al. (2010). Estimating the precision of the likelihood-ratio output of a forensic-voice-comparison system. *The Speaker and Language Recognition Workshop*, 63-70.
-   Morrison, G. S. (2013). Tutorial on logistic-regression calibration and fusion: Converting a score to a likelihood ratio. *Australian Journal of Forensic Sciences*, 45(2), 173–197. <https://doi.org/10.1080/00450618.2012.733025>
-   European Network of Forensic Science Institutes. (2015). Methodological guidelines for best practice in forensic semiautomatic and automatic speaker recognition (Version 01). <https://enfsi.eu/about-enfsi/structure/working-groups/documents-page/documents/best-practice-manuals/>
-   Meuwly, D., et al. (2017). A guideline for the validation of likelihood ratio methods used for forensic evidence evaluation. *Forensic Science International*, 276, 142–153. <https://doi.org/10.1016/j.forsciint.2016.03.048>
-   Morrison, G. S., & Poh, N. (2018). Avoiding overstating the strength of forensic evidence: Shrunk likelihood ratios/Bayes factors. *Science & Justice*, 58(3), 200–218. <https://doi.org/10.1016/j.scijus.2017.12.005>
-   Morrison, G. S., et al. (2021). Consensus on validation of forensic voice comparison. *Science & Justice*, 61(3), 299–309. <https://doi.org/10.1016/j.scijus.2021.02.002>
-   Morrison, G. S. (2024). Bi-Gaussianized calibration of likelihood ratios. *Law, Probability and Risk*, 23(1), 1–34. <https://doi.org/10.1093/lpr/mgae004>
-   Brümmer, N. (2005). *FoCal Toolbox* [MATLAB script]. <http://www.dsp.sun.ac.za/nbrummer/focal>
-   Morrison, G. S. (2007). *Matlab implementation of Aitken & Lucy’s (2004) forensic likelihood-ratio software using multivariate-kernel-density estimation* [MATLAB script]. <http://geoff-morrison.net/#MVKD>
-   Morrison, G. S. (2009). *Robust version of train_llr_fusion.m from Niko Brümmer’s FoCal Toolbox* [MATLAB script]. <https://geoff-morrison.net/#TrainFus>
-   Morrison, G. S. (2017). *Regularized version of train_llr_fusion.m from Niko Brümmer’s FoCal Toolbox* [MATLAB script]. <https://geoff-morrison.net>
