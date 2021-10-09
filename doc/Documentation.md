## Objectives of PheCode classification

EHR-based studies offer several advantages: they are cost efficient and allow large scale longitudinal analyses. EHR provides the potential to analyze hundreds of human diseases, drug responses, and many observable clinical traits. In particular, it allows phenome-wide association studies (PheWAS). The objective of PheCodes is to facilitate these large scale studies.


To define a patient's phenotype, EHR leverages billing codes. However, those are not always organized meaningfully for the purpose of high-throughput phenotypic analyses. PheCode regroups ICD-10-cm and ICD-9 billing codes in order to facilitate clinical research using ICD codes and represent clinically relevant phenotypes.



## Rolling up

The PheCode classification is hierarchical. It includes broader disease codes that are not necessarily present in the ICD-9 or ICD-10-cm classification. Rolling up ICD-9 and ICD-10-cm codes to parent codes should be done thoroughly.



## Using the app

The same PheCode hierarchy is displayed in two ways: as a sunburst plot and as a tree. Select a code or a phenotype to display the relevant classification and hierarchical structure. For clarity and to allow comparison, ICD-9 and ICD-10 codes are shown in separate colors. By default, the plots are centered around the top code, that has no parent code. One can click on the child code of interest to collapse/expand the subcodes.



## References

Wei WQ, Bastarache LA, Carroll RJ, Marlo JE, Osterman TJ, Gamazon ER, Cox NJ, Roden DM, Denny JC. Evaluating phecodes, clinical classification software, and ICD-9-CM codes for phenome-wide association studies in the electronic health record. PLoS One. 2017 Jul 7;12(7):e0175508. doi: 10.1371/journal.pone.0175508. PMID: 28686612; PMCID: PMC5501393.


Wu, P., Gifford, A., Meng, X., Li, X., Campbell, H., Varley, T., Zhao, J., Carroll, R., Bastarache, L., Denny, J. C., Theodoratou, E., & Wei, W. Q. (2019). Mapping ICD-10 and ICD-10-CM Codes to Phecodes: Workflow Development and Initial Evaluation. JMIR medical informatics, 7(4), e14325. https://doi.org/10.2196/14325