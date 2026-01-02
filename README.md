# Silvopasture and Bird Diversity in Colombia

This repository contains the data wrangling code associated with my PhD dissertation, examining the impact of silvopasture on bird taxonomic and functional diversity in fragmented landscapes of Colombia. The project aims to explore how local landcover and land-use impact bird diversity, and how these relationships are moderated by landscape and environmental factors.

## Project Overview

As part of the Sustainable Cattle Ranching (SCR) project, this research uses data from 500+ unique point count sites collected from 2013 - 2025 to model multi-species occupancy and abundance. The code in this repository prepares the data for future analyses and links these data with important covariates  (environmental, functional traits, etc.) that aid in future analyses:

-Taxonomy: Standradizing taxonomy to South American Classification Committee (SACC) 2018 taxonomy, and matching with BirdLife and BirdTree. 
-Functional traits: Predominantly from Avonet
-Land-use land-cover: Classification of manually digitized landcover in 300m buffers around point count centroids
-Phylogeny: Using Hackett (2008) backbone

Code also compares bird observations to their known distributional and elevational ranges, and removes observations that may have been erroneous or were not made within the 50-m fixed point count radius.

## Methodology

- **Data**: 500+ point count sites collected over a decade.
- **Possible Analyses**: Bayesian multi-species occupancy / abundance models, alpha and beta diversity, community composition, functional / phylogenetic diversity

## Usage

If you are interested in collaborating on this project please send me an email at skinnerayayron93 [at] gmail [dot] com. The data is not available publicly at present. 

## License

Feel free to use, modify, and distribute this code however you wish. No restrictions apply.

## Acknowledgments

Thank you to the Sustainable Cattle Ranching project and to my advisors for providing data and helping me understand cattle ranching in Colombia. Thank you to the NGO SELVA for sponsoring a year long Fulbright grant in Colombia, where they provided logistical and conceptual guidance, as well as providing important training towards identification and manipulation of the Colombian avifauna. A special thanks to my advisors at UBC and TNC for providing funding, professional guidance, and both academic and personal support.

## Contact

For more information or questions, please contact skinnerayayron93 [at] gmail [dot] com
