# extractInterEDAMLinks

R script to extract the following information from the bio.tools content and the EDAM ontology

- Links between data and format from tool annotations in bio.tools, given by EDAM data+format combinations in input/output functions
- Already given links in EDAM given by the ```is_format_of``` attribute
- Parent terms of EDAM formats

There at least two aims:

- Annotate corresponding data term(s) to each EDAM format
   - Identify and remove data without format
- Widen concept to include ```has_topic``` and other relationships between EDAM
