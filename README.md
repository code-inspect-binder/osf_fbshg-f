# Executable Environment for OSF Project [fbshg](https://osf.io/fbshg/)

This repository was automatically generated as part of a project to test the reproducibility of open science projects hosted on the Open Science Framework (OSF).

**Project Title:** The True Role that Suppressor Effects Play in Condition-Based Regression Analysis: None. A Reply to Fiedler (2021)

**Project Description:**
> . 

Here, we provide readers with additional materials for the article 

Humberg, S., Dufner, M., Schönbrodt, F. D., Geukes, K., Hutteman, R., van Zalk, M. H. W., Denissen, J. J. A., Nestler, S., &amp; Back, M. D. (2022). The true role that suppressor effects play in condition-based regression analysis: None. A reply to Fiedler (2021). Journal of Personality and Social Psychology, 123(4), 884–888. https://doi.org/10.1037/pspp0000428

The following files are provided: 

ComF_helpers.R is a background file that defines functions used in the other R code files (download the file, but you do not need to open it).

ComF_ReproduceResults.R is the R code that reproduces all fake data, plots and results reported in the article. 

ComF_SOM.pdf is a Supplemental Online Material with the following contents:
- 1. Mathematical Proof of the Suppressor Effect
--- 1.1 Proof 
--- 1.2 Computational Examples 
--- 1.3 Simulated Examples (Including Code to Reproduce Fiedler’s Simulation Study) 
- 2. How to Inspect Example Data With Arbitrary PPZ-Correlation-Structures
- 3. Suppressor Effects are Unconnected to SE Effect Patterns: Empirical Illustration
- 4. Summary of Prior Discussions of Suppressor Effects in the Context of CRA

ComF_SOM_Rcode.R is the R code that reproduces the contents of ComF_SOM.pdf.

.

**Original OSF Page:** [https://osf.io/fbshg/](https://osf.io/fbshg/)

---

**Important Note:** The contents of the `fbshg_src` folder were cloned from the OSF project on **12-03-2025**. Any changes made to the original OSF project after this date will not be reflected in this repository.

The `DESCRIPTION` file was automatically added to make this project Binder-ready. For more information on how R-based OSF projects are containerized, please refer to the `osf-to-binder` GitHub repository: [https://github.com/Code-Inspect/osf-to-binder](https://github.com/Code-Inspect/osf-to-binder)

## flowR Integration

This version of the repository has the **[flowR Addin](https://github.com/flowr-analysis/rstudio-addin-flowr)** preinstalled. flowR allows visual design and execution of data analysis workflows within RStudio, supporting better reproducibility and modular analysis pipelines.

To use flowR, open the project in RStudio and go to `Addins` > `flowR`.

## How to Launch:

**Launch in your Browser:**

🚀 **MyBinder:** [![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/code-inspect-binder/osf_fbshg-f/HEAD?urlpath=rstudio)

   * This will launch the project in an interactive RStudio environment in your web browser.
   * Please note that Binder may take a few minutes to build the environment.

🚀 **NFDI JupyterHub:** [![NFDI](https://nfdi-jupyter.de/images/nfdi_badge.svg)](https://hub.nfdi-jupyter.de/r2d/gh/code-inspect-binder/osf_fbshg-f/HEAD?urlpath=rstudio)

   * This will launch the project in an interactive RStudio environment on the NFDI JupyterHub platform.

**Access Downloaded Data:**
The downloaded data from the OSF project is located in the `fbshg_src` folder.

## Run via Docker for Long-Term Reproducibility

In addition to launching this project using Binder or NFDI JupyterHub, you can reproduce the environment locally using Docker. This is especially useful for long-term access, offline use, or high-performance computing environments.

### Pull the Docker Image

```bash
docker pull meet261/repo2docker-fbshg-f:latest
```

### Launch RStudio Server

Run the container (with a name, e.g. `rstudio-dev`):
```bash
docker run -it --name rstudio-dev --platform linux/amd64 -p 8888:8787 --user root meet261/repo2docker-fbshg-f bash
```

Inside the container, start RStudio Server with no authentication:
```bash
/usr/lib/rstudio-server/bin/rserver --www-port 8787 --auth-none=1
```

Then, open your browser and go to: [http://localhost:8888](http://localhost:8888)

> **Note:** If you're running the container on a remote server (e.g., via SSH), replace `localhost` with your server's IP address.
> For example: `http://<your-server-ip>:8888`

## Looking for the Base Version?

For the original Binder-ready repository **without flowR**, visit:
[osf_fbshg](https://github.com/code-inspect-binder/osf_fbshg)

