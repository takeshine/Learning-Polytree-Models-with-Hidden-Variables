# Implementation of an Algorithm to Recover Hidden Nodes

This repository contains the implementation and simulation of the algorithm described in the core paper of my thesis, focusing on recovering hidden nodes in polytrees under a Gaussian Structural Equation Modeling (SEM) assumption.

## Repository Structure

The repository is organized into four main folders, each corresponding to a specific aspect or section of the thesis work:

### 1. `algorithm_itself`

- **Description**: This folder contains the core implementation of the algorithm. 
- **Key Files**: 
  - `graph_on_paper.R`: Generates synthetic data for polytrees as described in the paper.
  - `final_para.html`: Contains the version of the results used in the final thesis document.

### 2. `sample_size`

- **Description**: Corresponding to Section 4.3.1 of the thesis, this folder contains simulations testing how changes in sample size affect the performance of the algorithm.
- **Key Instructions**: 
  - Use `data.R` to generate 20 datasets with up to 100,000 samples each, and save them in the specified path in `simu_run_1.ipynb`.
  - Ensure to create an output folder to collect the outputs of these simulations.

### 3. `new_layer`

- **Description**: For Section 4.3.2 of the thesis, this folder includes simulations for a new version of the algorithm.
- **Key Instructions**: 
  - Execute the `pipe_line.ipynb` file. 
  - An output folder should be created to collect the results, along with a `data_set` folder to store the current datasets necessary for the pipeline to run properly.

### 4. `node_number`

- **Description**: This folder corresponds to Section 4.3.3 of the thesis, where each subfolder represents the node number of the tree being analyzed.
- **Key Instructions**: 
  - Run `node_size_change.ipynb` for each node number. 
  - Ensure that code is available in each folder and that an output folder is created parallel to the nodes folder to collect results. Adjust the output path in the notebook accordingly if needed.

## How to Use

Each folder contains specific instructions and necessary scripts for running the simulations and generating results. Before running simulations, ensure all dependencies are installed and output directories are created as specified.

## Dependencies

- **R**: Required for `graph_on_paper.R` and `data.R`.
- **Python with Jupyter Notebook**: Required for `.ipynb` files.
