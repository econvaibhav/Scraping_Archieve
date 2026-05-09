# IEA Data Scraping and Visualization

**Author: Vaibhav Agarwal and Manikanta Radhakrishna**


**Last Updated: 26th March 2026**

# Overview
This repository contains a pipeline for scraping energy data from the International Energy Agency (IEA) and generating analytical visualizations.  

Project Structure
- ### main.py: The Python scraper that extracts country-specific data from IEA web pages.  

- ### Final_VisualCode.R: The R script used for all project graphs.  


Organized into sections separated by #.  

Each section maps to a specific visual (e.g., the second section produces v2 graphs).  

- ### visuals/: A directory containing the exported graphs (labeled v1, v2, etc.) matching the R script sections.  

- ### pyproject.toml: Overview of the Python version and package dependencies.  

- ### uv.lock: A detailed environment snapshot for exact replication.  

# Setup and Reproducibility
This project uses uv for dependency management. To sync your local environment and match the exact versions used in this project, run the command: **uv sync**
