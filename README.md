## Causal Inference with R 

Across public health, medicine, and social and behavioral sciences, researchers aim to answer causal questions. However, inferences about causation are challenging and require several assumptions to draw unbiased causal effects from observational data. This workshop discusses the difference between association and causation and introduces various methods to infer causal effects with observational data. We will then study R packages that implement propensity score techniques to estimate an average treatment effect – such as matching, subclassification, and inverse probability weighting. The workshop also provides several implementation options depending on the data type and the target causal effects of interest.

## Usage

- `Code/lalonde_data.R`: implements matching, subclassification, and inverse probability weighting to estimate the average treatment effect of a job training program on the real earnings. 


- `Code/rhc_data.R`: uses the propensity score methods to evaluate the average treatment effect of a right heart cathetization on death. You can download the data from the `Data/` folder. 

```{r}
rhc = read.csv("data/rhc.csv", header = TRUE, sep = ",")
```
