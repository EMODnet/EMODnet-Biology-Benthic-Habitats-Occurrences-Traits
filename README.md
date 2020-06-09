# EMODnet Biology Project Template

This repository contains a template for developing data products in the framework of [EMODnet Biology](https://www.emodnet-biology.eu/).

## Directory structure

```
{{cookiecutter.directory_name}}/
├── analysis
├── data/
│   ├── derived_data/
│   └── raw_data/
│   	└── data.csv
├── docs/
├── product/
└── scripts/
```

* **analysis** - R Markdown or Jupyter Notebooks
* **data** - Raw and derived data. The original raw data must not be modified.
* **docs** - Rendered reports
* **product** - Output product files
* **scripts** - Reusable code

## Getting started

To start a project using this template, you need to install [cookiecutter](https://github.com/cookiecutter/cookiecutter) first. Run the following statement in the command line to create a new project.

```bash
$ cookiecutter https://github.com/EMODnet/EMODnet-Biology-Project-Template
```

Alternatively, you can download this repository and modify the files yourself.

## Parameters

This template will ask for the following input:

* **directory_name:** The name of the new directory containing the project. It must not have spaces nor special characters. For example "zooplankton_abundance"
* **project_name:** The name of the project in plain text. This will be added to the README.md file. For example "Ecological niche model of zooplankton abundance"
* **project_description:** A short description about the project. It will be added to the README.md file.
* **data_wfs_request:** This is the query of the data that will be extracted from EMODnet-Biology. You can use the [download toolbox](https://www.emodnet-biology.eu/toolbox/en/download/occurrence/explore) to select the desired data. Once your query is ready, click on "Get webservice url" and copy the this link here. The data will be downloaded as .csv on ~/data/raw_data/. ![toolbox](https://github.com/salvafern/cookiecutter-emodnetbio-dataproduct/blob/master/toolbox_screenshot.png)
* **citation:** Include here the preferred citation for the product. This can be edited afterwards.

## Contact
Please feel free to write some lines to [bio@emodnet.eu](mailto:bio@emodnet.eu) if you have any questions.
