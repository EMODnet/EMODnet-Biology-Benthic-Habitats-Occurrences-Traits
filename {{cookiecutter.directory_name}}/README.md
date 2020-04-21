# {{cookiecutter.project_name}}

{{cookiecutter.project_description}}

## Directory structure

* **analysis** - Markdown or Jupyter notebooks
* **data** - Raw and derived data
* **docs** - Rendered reports
* **product** - Output product files
* **scripts** - Reusable code

## Data

Raw data can be downloaded from EMODnet Biology using the following WFS request:

```
{{cookiecutter.data_wfs_request}}
```

## Analysis

...

## Citation

This product should be cited as:
*{{cookiecutter.citation}}*