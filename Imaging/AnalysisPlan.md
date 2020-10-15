# Analysis Plan: Imaging Data

This described the analyses for the imaging data specifically. The core
hypotheses for the project can be found in
[the main analysis plan](../AnalysisPlan.md).

### Dataset overview

Available modalities:
 + Structural: T1/T2/QSM
 + Functional: rfMRI
 + Diffusion
 + ASL

### Analysis overview

 + GLM to assess which imaging measures correlate with patients developing
   apathy. Basically contrasts between the following groups:
    + Apathetic at baseline.
    + Develop apathy.
    + No apathy during study.

 + Potential imaging measures:
    + VBM-style analysis of T1/T2/QSM.
    + BIANCA for white-matter hyperintensities.
    + FA etc from DTI (if enough data).
    + Seed-based functional/structural connectivity from key ROIs
      [LeHeron-Husain-2018] (MIST for segmentation?).
    + Perfusion from ASL (BASIL?).
    + PROFUMO?
    + PALM NPC over modalities?
