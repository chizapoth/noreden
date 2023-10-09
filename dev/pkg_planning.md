# Package planning documentation

This is the package planning documentation for `noreden`. 


## Purpose

The package shall provide user-friendly functions and tools for nutrition researcher to explore diet design, under various nutritional and environmental constraints.

## Scope and requirements

The package is based on existing information on **food groups**: average intake, nutrition information and environmental impact per unit intake.

### Functions (must have)

Selectors 

* food group(s) selection 
* nutrient(s) selection
* environmental metric(s) selection

Computation

* computation for constraints with given food groups and per unit contribution of outcome (of nutrient, env impact)
* standardizer for constraints across different outcomes
* wrapper function for the optimization program

Result presentation

* summary table of result diet
* summary table of comparison of outcomes


### Functions (good to have)

* `gt` table summary 







